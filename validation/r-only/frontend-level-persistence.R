source(file.path("helpers", "native-guard.R"))
# Fresh-process public context checks; no compiler, Stan execution or MCMC.
resource<-Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
if(nzchar(resource)&&file.exists(resource)) {
 source_mode<-nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))
 script<-tempfile(fileext=".R");saved<-tempfile(fileext=".rds");logs<-character()
 prefix<-c("args<-commandArgs(TRUE)",
   if(source_mode)paste0("for(f in list.files(file.path(",dQuote(Sys.getenv("MEASUREMENTPKG_SOURCE"),q=FALSE),",'R'),full.names=TRUE))source(f)")else c(paste0(".libPaths(c(",paste(vapply(.libPaths(),function(x)dQuote(x,q=FALSE),character(1)),collapse=","),"))"),"library(bmeas)",".mp_hash<-getFromNamespace('.mp_hash','bmeas')"))
 body<-c("path<-args[3]",
"if(args[2]=='write') {",
" f<-expand.grid(person=1:2,route=1:3,content=1:3,stringsAsFactors=FALSE);f$task<-paste0('display_',f$route,'_',f$content);f$obs<-paste0('o',seq_len(nrow(f)))",
" f$counts<-I(matrix(NA_integer_,nrow(f),10));f$options<-I(matrix(0L,nrow(f),10));active<-list(c(1:4,10),c(5:7,9:10),5:10)",
" for(i in seq_len(nrow(f)))f$options[i,active[[f$route[i]]]]<-1L",
" full<-observe(m3_model(Sys.getenv('MEASUREMENTPKG_M3_RESOURCE')),f,'person','task',fields=list(counts='counts',options='options',route='route',content='content'),units='count',observation_id='obs',parameter_bindings=m3_reference_bindings(f,'obs'),parameters=list(a=list(fixed=.5),c=list(fixed=.5),ra=list(fixed=.4),rc=list(fixed=.6)))",
" seen<-observe(gaussian_model(),data.frame(person=1:2,task='t',y=c(.2,.7)),'person','task','y',units='points',parameters=list(mu='task',sigma=list(fixed=1)))",
" spec<-bhmm(bhmf(),data=list(seen=seen,full=full),subjects='shared',missing='observed_only',fit=FALSE)",
"}else spec<-readRDS(path)",
"hash<-.mp_hash(spec$models$full$model$validate_bindings);fingerprint<-spec$fingerprint",
"first_data<-first_code<-NULL",
"for(i in 1:5) {",
" data<-standata(spec,threads=2L);code<-as.character(stancode(spec,threads=2L,log_lik=TRUE));table<-model_table(spec);check_model(spec)",
" if(is.null(first_data)){first_data<-data;first_code<-code}else stopifnot(identical(first_data,data),identical(first_code,code))",
" stopifnot(identical(hash,.mp_hash(spec$models$full$model$validate_bindings)),identical(fingerprint,spec$fingerprint),nrow(parameter_binding_table(spec))==0L)",
" spec<-unserialize(serialize(spec,NULL,version=2))",
"}",
"if(args[2]=='write')saveRDS(spec,path,version=2)",
"cat(args[2],'five public standata/stancode/check/RDS cycles with JIT3 passed; no native calls\\n')")

 writeLines(c(prefix,"compiler::enableJIT(3)",body),script)
 for(mode in c("write","read")) {
  log<-tempfile(fileext=".log");logs<-c(logs,log)
  status<-system2(file.path(R.home("bin"),"Rscript"),c(shQuote(script),"unused",mode,shQuote(saved)),stdout=log,stderr=log)
  if(status!=0L)stop(paste(readLines(log,warn=FALSE),collapse="\n"))
 }
 stopifnot(file.exists(saved))
 cat("10 fresh-process empty-routed callback/codegen/RDS cycles passed; no native calls\n")
 unlink(c(script,saved,logs))
}
