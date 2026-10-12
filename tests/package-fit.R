source(file.path("helpers", "native-guard.R"))
# Lifecycle forwarding and cache tests use an explicit backend double, never MCMC.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
local({
  old<-private('.bh_compile_scalar')
  replace<-function(f)if(source_mode)assign('.bh_compile_scalar',f,envir=.GlobalEnv)else assignInNamespace('.bh_compile_scalar',f,ns='bmeas')
  on.exit(replace(old))
  calls<-new.env();calls$compile<-calls$sample<-0L
  # The size receipt sees a real metadata file, never synthetic posterior CSV evidence.
  mock_output<-tempfile('backend-metadata-',fileext='.txt')
  writeLines('metadata-only backend double; not posterior data',mock_output)
  on.exit(unlink(mock_output),add=TRUE)
  backend<-list(return_codes=function()rep(0L,calls$args$chains),output_files=function()mock_output,output=function()character(),
    diagnostic_summary=function(quiet)list(num_divergent=0L,num_max_treedepth=0L,ebfmi=1),
    summary=function(variables,...)data.frame(variable=c('alpha[1]','loading[1]','obs_sd[1]','R_component[1,1]','z_raw[1,1]'),
      mean=0,median=0,sd=1,q5=-1,q95=1,rhat=1.5,ess_bulk=5,ess_tail=5,mcse_mean=.5),
    draws=function(variables,format)matrix(rep(1,10),10,1,dimnames=list(NULL,'L_component[1,1]')))
  compiled<-list(model=list(sample=function(...){calls$sample<-calls$sample+1L;calls$args<-list(...);backend}),
    identity=list(code='explicit_unit_test_double',toolchain=list()),cache_key='mock',cache_hit=TRUE,executable_sha256='mock')
  replace(function(spec,cache_dir,save_log_lik){calls$compile<-calls$compile+1L;compiled})
  d<-data.frame(person=1:4,x1=1:4,x2=c(2,1,4,3),x3=c(3,1,2,4))
  s<-bhmm(bhmf(G=~x1+x2+x3),d,subject='person',fit=FALSE)
  f<-bhmm(s,chains=1,iter=20,warmup=10,cores=1,threads=1,seed=63,output_dir=tempdir(),control=list(max_treedepth=6),resources='development')
  ok(inherits(f,'bhmm_fit') && calls$compile==1L && calls$sample==1L,'public fit dispatch invokes one compile-loader and one sampler')
  ok(identical(class(f),c('bmeas_fit','bhmm_fit'))&&inherits(summary(f),'summary.bmeas_fit'),'new fit and summary have distinct bmeas dispatch with retained legacy classes')
  ok(startsWith(capture.output(print(f))[1L],'bmeas component fit'),'new fit uses current package branding')
  ok(calls$args$iter_warmup==10L && calls$args$iter_sampling==10L,'iter includes warmup exactly')
  ok(calls$args$parallel_chains==1L && calls$args$threads_per_chain==1L,'serial runtime forwards one actual thread explicitly')
  ok(calls$args$max_treedepth==6L && calls$args$seed==63L,'explicit sampler controls forwarded')
  ok(calls$args$data$mp_use_reduce_sum==0L && f$provenance$cache_hit,'serial branch and reuse provenance retained')
  ok(identical(f$cache$initialization,f$provenance$initialization)&&f$cache$initialization$recipe_version=='role_scale_aware_v1','execution-time initialization recipe and primitive table persist in the detached cache')
  ok(identical(f$provenance$initialization$constrained_init_hash,private('.mp_hash')(calls$args$init())),'provenance hash records exactly the init passed to the backend')
  ok(f$cache$inference$status=='not_assessed'&&f$cache$predictive_fit$status=='not_run','mock numerical completion never becomes inference or PPC')
  ok(f$cache$admissibility$status=='screened_not_proven','matrix screening does not promise global numeric proof')
  saved<-capture.output(print(summary(f)));f$backend<-NULL
  ok(identical(saved,capture.output(print(summary(f)))),'summary is cache-only after backend removal')
  ok(nrow(posterior_summary(f))==5L,'posterior_summary is cache-only')
  ok(posterior_summary(f)$parameter_key[3]=='observation_sd:scores::sigma::x1','noise summary uses sigma target key')
  ok(any(grepl('DEVELOPMENT POLICY',saved))&&any(grepl('No claim of usable posterior inference',saved)),'prototype limitations prominent in printed summary')
  before<-calls$sample
  err<-tryCatch(bhmm(s),error=identity)
  ok(inherits(err,'measurement_error')&&err$code=='E_RUN_CONFIG'&&calls$sample==before,'no implicit tiny sampling default')
  backend$output<-function()'Exception: component SD cannot be represented'
  warning_cache<-private('.bh_cache_scalar_fit')(backend,s,list())
  ok(warning_cache$numerical_evaluations$observed_failure_messages==1L&&warning_cache$computation$status=='warning','recognized numerical message preserves warning instead of clean success')
})
cat('PASS representative public fit routing to terminal doubles and passive cache; no fitting\n')
