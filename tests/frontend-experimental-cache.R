source(file.path("helpers", "native-guard.R"))
# A compiler/sampler double tests the actual cache lifecycle; no native call.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
tasks<-c('t1','t2','t3');d<-expand.grid(person=1:4,task=tasks,stringsAsFactors=FALSE);d$y<-sin(seq_len(nrow(d))*.6)+.1*seq_len(nrow(d))
a<-observe(gaussian_model(),d,'person','task','y',units='points')
b<-observe(gaussian_model(),transform(d,y=.7*y+.2),'person','task','y',units='points')
s<-bhmm(bhmf(G=~a$mu[tasks]+b$mu[tasks]),list(a=a,b=b),subjects='shared',fit=FALSE)
local({
 old<-private('.bh_compile_scalar');calls<-new.env();calls$compile<-calls$sample<-0L
 replace<-function(f)if(source_mode)assign('.bh_compile_scalar',f,.GlobalEnv)else assignInNamespace('.bh_compile_scalar',f,'bmeas')
 on.exit(replace(old))
 # The size receipt sees a real metadata file, never synthetic posterior CSV evidence.
 mock_output<-tempfile('backend-metadata-',fileext='.txt')
 writeLines('metadata-only backend double; not posterior data',mock_output)
 on.exit(unlink(mock_output),add=TRUE)
 backend<-list(return_codes=function()rep(0L,calls$args$chains),output_files=function()mock_output,output=function()character(),
  diagnostic_summary=function(quiet)list(num_divergent=1L,num_max_treedepth=2L,ebfmi=.2),
  summary=function(variables,...)data.frame(variable=c('alpha[1]','loading[1]','obs_sd[1]','R_component[1,1]','z_raw[1,1]'),mean=0,median=0,sd=1,q5=-1,q95=1,rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.5),
  draws=function(variables,format)matrix(1,1,1,dimnames=list(NULL,'L_component[1,1]')))
 replace(function(spec,cache_dir,save_log_lik){calls$compile<-calls$compile+1L;list(model=list(sample=function(...){calls$sample<-calls$sample+1L;calls$args<-list(...);backend}),
  identity=list(code=as.character(stancode(spec,threads=2L,log_lik=save_log_lik)),toolchain=list(stanc_options=list(O0=TRUE))),cache_key='explicit_cache_double',cache_hit=TRUE,executable_sha256='mock_not_a_native_file')})
 run<-function(x,...)bhmm(x,chains=1,iter=20,warmup=10,output_dir=tempdir(),...,resources='development')
 err(run(s),'E_EXECUTION_PROFILE');ok(calls$compile==0L&&calls$sample==0L,'default blocks before even a cache hit can be consulted')
 warnings<-character();fit<-withCallingHandlers(run(s,execution='experimental'),warning=function(w){warnings<<-c(warnings,w$code);invokeRestart('muffleWarning')})
 ok(calls$compile==1L&&calls$sample==1L&&fit$provenance$cache_hit,'explicit profile reaches actual lifecycle through verified cache double')
 ok(identical(warnings,'W_EXPERIMENTAL_EXECUTION'),'profile warning occurs before lifecycle dispatch')
 ok(fit$cache$execution_profile$used=='experimental_typed_composition'&&fit$cache$backend_qualification$qualification$status=='exact_composition_unqualified','actual cache retains support separately from absent exact evidence')
 ok(fit$cache$backend_qualification$support$policy_version=='typed_experimental_execution_v1','cached policy is versioned')
 ok(fit$cache$inference$status=='not_assessed'&&fit$cache$computation$status=='warning'&&all(fit$cache$parameter_summary$rhat==2),'poor mock diagnostics cannot become inference validity')
 fit$backend<-NULL;copy<-unserialize(serialize(fit,NULL));before<-c(calls$compile,calls$sample)
 result<-capture.output(print(summary(copy)));posterior_summary(copy)
 ok(identical(before,c(calls$compile,calls$sample))&&any(grepl('experimental_typed_composition',result,fixed=TRUE)),'saved backend-free summary neither compiles nor samples and preserves the actual profile')
 err(run(s),'E_EXECUTION_PROFILE');ok(calls$compile==1L&&calls$sample==1L,'a prior cache hit does not remove subsequent opt-in')
 changed<-s;changed$models$a$data$y[1]<-99
 err(run(changed,execution='experimental'),'E_SPEC_CHANGED');ok(calls$compile==1L&&calls$sample==1L,'tampered data stop before cache dispatch')
})
cat(passed,'generic cache assertions passed with doubles only\n')
