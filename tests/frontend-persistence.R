source(file.path("helpers", "native-guard.R"))
# Fresh-process persistence for known reference callbacks; no Stan execution.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
m3<-Sys.getenv('MEASUREMENTPKG_M3_RESOURCE');sdm<-Sys.getenv('MEASUREMENTPKG_SDM_RESOURCE')
if(nzchar(m3)&&nzchar(sdm)&&file.exists(m3)&&file.exists(sdm)) {
  original<-demo_cognitive_wiring_spec(m3,sdm)
  f<-bhmf(residual=list(targets=~memory$a['CS']+memory$c['CS']+memory$ra['CS']+memory$rc['CS']+report$log_c['CR']+report$log_kappa['CR'],cor='free'))
  # Explicit generic engineering priors for a persistence test, not research defaults.
  p<-c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(.3)','parameter_sd'))
  spec<-bhmm(f,original$models,subjects='shared',prior=p,fit=FALSE)
  full<-m3_model(m3);hash<-private('.mp_hash')
  bundle<-list(spec=spec,full=full,full_hash=hash(full),data=standata(spec),code=as.character(stancode(spec)))
  path<-tempfile(fileext='.rds');saveRDS(bundle,path,version=2)
  restored<-readRDS(path)
  ok(identical(standata(restored$spec),bundle$data),'same-process serialization preserves public data/context validation')
  ok(identical(as.character(stancode(restored$spec)),bundle$code),'same-process serialization preserves generated scientific code')
  ok(identical(hash(restored$full),bundle$full_hash),'full M3 callback also avoids constructor-frame capture')
  script<-tempfile(fileext='.R');result<-tempfile(fileext='.rds')
  writeLines(c("args<-commandArgs(TRUE)",
    "if(nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))) {for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f);hash<-.mp_hash}else {library(bmeas);hash<-getFromNamespace('.mp_hash','bmeas')}",
    "x<-readRDS(args[1]);stopifnot(identical(standata(x$spec),x$data),identical(as.character(stancode(x$spec)),x$code),identical(hash(x$full),x$full_hash))",
    "saveRDS(list(data=standata(x$spec),code=as.character(stancode(x$spec)),full_hash=hash(x$full)),args[2])"),script)
  output<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(script),shQuote(path),shQuote(result)),stdout=TRUE,stderr=TRUE)
  ok((is.null(attr(output,'status'))||attr(output,'status')==0L)&&file.exists(result),'fresh R process accepts saved spec and full M3 callback hash')
  child<-readRDS(result)
  ok(identical(child$data,bundle$data)&&identical(child$code,bundle$code),'fresh-process data and code match exactly')
  mutate<-function(which){x<-restored$spec
    if(which=='counts')x$data_map$blocks$memory$counts[1,1]<-x$data_map$blocks$memory$counts[1,1]+1L
    if(which=='options')x$data_map$blocks$memory$options[1,1]<-x$data_map$blocks$memory$options[1,1]+1L
    if(which=='resource')x$models$memory$model$adapter$resources[[1]]$sha256<-paste(rep('0',64),collapse='')
    if(which=='parameter')x$models$memory$model$parameters$a$receives<-'theta'
    if(which=='callback')x$models$memory$model$informative_rows<-function(d)rep(TRUE,nrow(d))
    e<-tryCatch(standata(x),error=identity)
    ok(inherits(e,'measurement_error')&&e$code=='E_SPEC_CHANGED',paste('persistence keeps mutation guard for',which))
  }
  for(field in c('counts','options','resource','parameter','callback'))mutate(field)
  for(factory in list(m3_cs_model,m3_model)) {
    model<-factory(m3);before<-hash(model)
    counts<-matrix(0,4,10);counts[1,1]<-1;counts[2,5]<-1;counts[4,1]<-NA;counts[4,2]<-2
    data<-data.frame(row=1:4);data$counts<-counts
    for(i in 1:20)model$informative_rows(data)
    ok(identical(hash(model),before)&&identical(hash(unserialize(serialize(model,NULL,version=2))),before),'fresh known M3 callback is stable through JIT/calls/serialization')
  }
  model<-m3_cs_model(m3);before<-hash(model);valid<-original$models$memory$data
  for(i in 1:20)model$validate_data(valid)
  ok(identical(hash(model),before),'fresh reference validator is also bytecode-stable before first validation')
  unlink(c(path,script,result))
}else cat('SKIP external-reference persistence: provide the two original resource paths\n')
cat('Frontend persistence checks:',passed,'passed; no native compilation, Stan or posterior sampling.\n')

ok(identical(private(".mp_stanc_options")(),list(O0=TRUE)),"explicit stanc O0 prevents reuse of known faulty O1 inlining")
cat("Persistence and compiler-policy checks:",passed,"passed. No native invocation.\n")
