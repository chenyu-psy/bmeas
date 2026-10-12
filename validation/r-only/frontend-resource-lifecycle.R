source(file.path("helpers", "native-guard.R"))
# Public resource-policy lifecycle with explicit R backend doubles only.
# These temporary metadata files and return codes are not posterior evidence.
source_root<-Sys.getenv('MEASUREMENTPKG_SOURCE')
source_mode<-nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,'bmeas')
passed<-0L
ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
local({
  env<-if(source_mode).GlobalEnv else asNamespace('bmeas')
  originals<-list()
  replace<-function(name,value) {
    if(is.null(originals[[name]]))originals[[name]]<<-list(value=get(name,env,inherits=FALSE),locked=bindingIsLocked(name,env))
    if(bindingIsLocked(name,env))unlockBinding(name,env)
    assign(name,value,env)
    if(originals[[name]]$locked)lockBinding(name,env)
  }
  on.exit(for(name in names(originals)) {
    if(bindingIsLocked(name,env))unlockBinding(name,env)
    assign(name,originals[[name]]$value,env)
    if(originals[[name]]$locked)lockBinding(name,env)
  },add=TRUE)
  calls<-new.env(parent=emptyenv());calls$compile<-calls$sample<-0L;calls$forbidden<-character()
  forbidden<-function(name)local({name<-name;function(...){calls$forbidden<-c(calls$forbidden,name);stop(paste('FORBIDDEN_RESOURCE_ROUTE',name))}})
  fixtures<-vapply(seq_len(4L),function(i)tempfile('resource-metadata-',fileext='.txt'),character(1));fixture<-fixtures[1L];saved<-tempfile(fileext='.rds')
  on.exit(unlink(c(fixtures,saved)),add=TRUE)
  for(path in fixtures)writeLines('Backend-double metadata file; not posterior draws.',path)
  backend<-list(return_codes=function()rep(0L,calls$args$chains),output_files=function()fixtures[seq_len(calls$args$chains)],
    summary=forbidden('backend_summary'),diagnostic_summary=forbidden('backend_hmc'),
    output=forbidden('backend_output'),draws=forbidden('backend_draws'))
  compiled<-list(model=list(sample=function(...){calls$sample<-calls$sample+1L;calls$args<-list(...);backend}),
    identity=list(code='R_ONLY_RESOURCE_LIFECYCLE_DOUBLE',toolchain=list()),
    cache_key='double',cache_hit=TRUE,executable_sha256='double')
  replace('.bh_compile_scalar',function(...){calls$compile<-calls$compile+1L;compiled})
  replace('.bh_provider_runtime_info',function()list(available=TRUE,cmdstanr_version='0.9.0',cmdstan_version='2.40.0',os_type='unix',wsl=FALSE))
  for(name in c('.bh_cache_scalar_fit','.bh_numeric_cache','.bh_capture_numerical_messages',
    '.bh_report_availability_cache','.bh_reference_report_cache','.bh_affine_matrix_cache',
    '.bh_affine_summary_map','.bh_marker_summary_map','.bh_reference_summary_map',
    '.bh_compile_identity','.mp_toolchain_fingerprint'))replace(name,forbidden(name))
  d<-data.frame(person=1:4,x1=c(.2,.8,.3,1),x2=c(.7,.6,-.3,.1),x3=c(-.2,.6,.9,.4))
  spec<-bmeas(bmf(G=~x1+x2+x3),d,subject='person',fit=FALSE)
  policy<-list(max_output_bytes=256*1024^2,cache='deferred')
  run<-function()bmeas(spec,chains=4,iter=2000,warmup=1000,cores=4,
    resources=policy,cache_dir=tempdir(),output_dir=tempdir())
  fit<-run()
  ok(inherits(fit,'bmeas_fit')&&calls$compile==1L&&calls$sample==1L,'deferred public dispatch returns one genuine fit from one explicit backend double')
  ok(!length(calls$forbidden),'no posterior summary, HMC, output-capture, draw, cache or native compile helper is called')
  ok('diagnostics'%in%names(calls$args)&&is.null(calls$args$diagnostics),
    'deferred calls explicitly disable supported provider sampler-diagnostic materialization before sample returns')
  ok(identical(fit$spec,spec)&&identical(fit$code,compiled$identity$code),'retained scientific context and code identity are unchanged')
  ok(fit$execution_receipt$sampling$status=='completed'&&identical(fit$execution_receipt$sampling$return_codes,rep(0L,4L)),
    'completion receipt retains one actual double return code per configured chain')
  ok(is.na(fit$execution_receipt$sampling$observed_csv_rows)&&fit$execution_receipt$output$original_file_hashes$status=='not_collected',
    'configured states never become observed rows or original-file authentication')
  ok(fit$cache$posterior_reporting$status=='deferred_by_policy'&&is.null(fit$cache$numerical_evaluations)&&fit$cache$inference$status=='not_assessed',
    'policy deferral is separate from the numerical producer schema and inference evidence')
  ok(identical(fit$cache$initialization,fit$provenance$initialization)&&
    identical(fit$cache$initialization$constrained_init_hash,private('.mp_hash')(calls$args$init())),
    'retained initialization records describe exactly the unchanged values sent to the backend')
  read<-function(x)list(checks=fit_checks(x),summary=summary(x),parameters=parameter_summary(x),posterior=posterior_summary(x),
    text=capture.output(print(x)),checks_text=capture.output(print(fit_checks(x))))
  warm<-read(fit)
  ok(identical(names(warm$checks),c('version','context','sampling','domains','predictive')),'five top-level report views are preserved')
  ok(is.null(warm$parameters)&&is.null(warm$posterior)&&warm$checks$sampling$parameters_availability=='deferred_by_policy',
    'posterior access is passive and explicitly unavailable by policy')
  ok(all(vapply(warm$checks$sampling$extrema,function(x)is.na(x$value)&&is.na(x$coverage$rows),logical(1)))&&
    all(vapply(warm$checks$sampling$chains$metrics,function(x)is.na(x$observed_total)&&is.na(x$coverage$usable),logical(1))),
    'deferred diagnostic quantities and coverage remain unknown, never artificial zeros')
  ok(any(grepl('CALLER RESOURCE POLICY',warm$text,fixed=TRUE))&&
    any(grepl('Sampling completion: completed | posterior diagnostics: deferred_by_policy',warm$text,fixed=TRUE)),
    'printing distinguishes caller resources, sampler completion and uncomputed diagnostics')
  fit$backend<-NULL;saveRDS(fit,saved,version=2)
  ok(identical(warm,read(readRDS(saved))),'detached cold reports match warm passive reports')

  # A 64 KiB metadata fixture is sufficient to exercise a real postfit stat
  # exceedance for a tiny configured one-state double, without large output.
  small_policy<-list(max_output_bytes=64*1024,cache='deferred')
  small_run<-function()bmeas(spec,chains=1,iter=2,warmup=1,resources=small_policy,
    cache_dir=tempdir(),output_dir=tempdir())
  writeBin(raw(64*1024+1L),fixture)
  too_large<-tryCatch(small_run(),error=identity)
  ok(inherits(too_large,'bmeas_postfit_resource_error')&&too_large$code=='E_OUTPUT_BUDGET',
    'postfit actual size exceedance returns a typed resource outcome')
  ok(inherits(too_large$fit,'bmeas_fit')&&too_large$execution_receipt$sampling$status=='completed'&&
    identical(too_large$output_files,fixture)&&file.exists(fixture),
    'output exceedance preserves the genuine fit, successful sampler receipt and output paths')
  ok(read(too_large$fit)$checks$sampling$posterior_reporting$status=='uncomputed_output_resource',
    'resource failure remains distinct from policy deferral and is passively readable')
  backend$output_files<-function()paste0(fixture,'-missing')
  missing_file<-tryCatch(small_run(),error=identity)
  ok(inherits(missing_file,'bmeas_postfit_resource_error')&&missing_file$code=='E_OUTPUT_SIZE_UNAVAILABLE'&&
    is.na(missing_file$resource_outcome$actual_output_bytes)&&missing_file$execution_receipt$sampling$status=='completed',
    'missing output size is unknown and retains successful sampling evidence')
  backend$output_files<-function()fixtures;backend$return_codes<-function()0L
  incomplete<-tryCatch(run(),error=identity)
  ok(inherits(incomplete,'measurement_error')&&incomplete$code=='E_SAMPLING_FAILED'&&
    incomplete$execution_receipt$sampling$status=='completion_unavailable'&&identical(incomplete$output_files,fixtures),
    'one return code for four chains cannot establish complete sampling and still retains paths')
  backend$return_codes<-function()rep(0L,4L);backend$output_files<-function()fixture
  incomplete_paths<-tryCatch(run(),error=identity)
  ok(inherits(incomplete_paths,'bmeas_postfit_resource_error')&&incomplete_paths$execution_receipt$sampling$status=='completed'&&
    incomplete_paths$resource_outcome$status=='measurement_unavailable'&&identical(incomplete_paths$output_files,fixture),
    'four successful return codes with one output path cannot establish complete aggregate bytes')
  provider_cause<-structure(list(message='metadata-constructor double failed',call=NULL),class=c('provider_double_error','error','condition'))
  compiled$model$sample<-function(...){calls$sample<-calls$sample+1L;calls$args<-list(...);stop(provider_cause)}
  before_failure<-calls$sample
  provider_failed<-tryCatch(run(),error=identity)
  ok(inherits(provider_failed,'bmeas_provider_error')&&identical(provider_failed$cause,provider_cause)&&
    provider_failed$provider_operation$status=='failed'&&provider_failed$sampling$status=='unknown'&&
    is.null(provider_failed$fit)&&is.null(provider_failed$sampling$return_codes)&&!length(provider_failed$output_files)&&
    identical(provider_failed$output_dir,normalizePath(tempdir(),winslash='/'))&&calls$sample==before_failure+1L&&file.exists(fixture),
    'public provider failure preserves cause and directory with completion unknown, no fake fit, no retry and no deletion')
  eager_failure<-tryCatch(bmeas(spec,chains=1,iter=2,warmup=1,resources='development',
    cache_dir=tempdir(),output_dir=tempdir()),error=identity)
  ok(inherits(eager_failure,'bmeas_provider_error')&&identical(eager_failure$cause,provider_cause)&&
    !any(c('diagnostics','output_basename')%in%names(calls$args)),
    'eager development forwards unchanged provider defaults without deferred diagnostic or basename arguments')

  # Synthetic cached metrics test optional-marker isolation, never posterior
  # validity. The original scientific spec and on-disk fixtures stay untouched.
  retained<-fit;retained$cache$posterior_reporting<-NULL
  retained$cache$parameter_summary<-data.frame(variable='alpha[1]',parameter_key='test',parameter_role='test',
    rhat=1.2,ess_bulk=4,ess_tail=3)
  expected<-read(retained)
  variants<-list(callback=forbidden('metadata_callback'),malformed=list(version='unknown'),
    contradictory=fit$cache$posterior_reporting)
  for(name in names(variants)) {
    x<-retained;x$cache$posterior_reporting<-variants[[name]];out<-read(x)
    ok(out$checks$domains$posterior_reporting$availability=='unavailable_invalid_metadata'&&
      identical(out$checks$sampling,expected$checks$sampling),paste('optional',name,'marker preserves retained metrics without callbacks'))
  }
  x<-retained;x$cache<-c(x$cache,list(posterior_reporting=fit$cache$posterior_reporting,posterior_reporting=fit$cache$posterior_reporting))
  out<-read(x)
  ok(out$checks$domains$posterior_reporting$availability=='unavailable_invalid_metadata'&&
    identical(out$checks$sampling,expected$checks$sampling),'duplicate optional markers do not hide retained metrics')
  x<-fit;x$execution_receipt$sampling$reason<-forbidden('receipt_callback');out<-read(x)
  ok(out$checks$context$run$execution_receipt$availability=='unavailable_invalid_metadata'&&
    any(grepl('Sampling completion: unavailable',out$text,fixed=TRUE)),'malformed receipt is isolated before formatting')
  ok(!length(calls$forbidden),'all lifecycle, passive and malformed-metadata paths leave forbidden routes untouched')
})
cat('Frontend resource lifecycle checks:',passed,'passed. R doubles only; no compiler, real sampler, posterior CSV reader or extraction.\n')
