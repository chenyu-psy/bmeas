source(file.path("helpers", "native-guard.R"))
# Synthetic saved-object fixtures only: no model preparation or posterior work.
source_root<-Sys.getenv('MEASUREMENTPKG_SOURCE');source_mode<-nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,'bmeas')
passed<-0L;calls<-character()
ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
deny<-function(name)local({name<-name;function(...){calls<<-c(calls,name);stop(paste('FORBIDDEN_PASSIVE_ROUTE',name))}})
`$.passive_probe`<-deny('optional node S3 extraction')
format.passive_probe<-deny('optional node S3 formatting')
guarded<-function(expr) {
  originals<-list()
  install<-function(env,name) {
    if(!exists(name,env,inherits=FALSE))return(invisible(NULL))
    originals[[length(originals)+1L]]<<-list(env=env,name=name,value=get(name,env,inherits=FALSE),locked=bindingIsLocked(name,env))
    if(bindingIsLocked(name,env))unlockBinding(name,env)
    assign(name,deny(name),env);if(originals[[length(originals)]]$locked)lockBinding(name,env)
  }
  on.exit(for(x in rev(originals)) {
    if(bindingIsLocked(x$name,x$env))unlockBinding(x$name,x$env)
    assign(x$name,x$value,x$env);if(x$locked)lockBinding(x$name,x$env)
  },add=TRUE)
  env<-if(source_mode).GlobalEnv else asNamespace('bmeas')
  for(n in c('bmeas','bhmm','check_model','get_prior','standata','stancode','execution_plan','.bh_run_plan',
    '.bh_prepare','.bh_validate_spec','.mp_validate_spec','.mp_hash','.mp_sha256','.bh_fit_scalar','.bh_compile_scalar',
    '.bh_cache_scalar_fit','.bh_numeric_cache','.bh_capture_numerical_messages','.bh_affine_matrix_cache',
    '.bh_reference_report_cache','.bh_report_availability_cache','.bh_affine_summary_map','.bh_marker_summary_map',
    '.bh_reference_summary_map','.bh_provider_runtime_info','.bh_provider_metadata_plan','.bh_provider_preflight',
    '.bh_provider_sample','.bh_initialization','.bh_materialize_init','pp_check','individual_parameters'))install(env,n)
  for(n in c('readRDS','readLines','readBin','readChar','scan','load','saveRDS','save','file.info','file',
    'gzfile','bzfile','xzfile','url','writeLines','writeBin','system','system2','dyn.load','loadNamespace'))install(baseenv(),n)
  for(n in c('read.table','read.csv','read.csv2','write.table','write.csv'))install(asNamespace('utils'),n)
  force(expr)
}
read_report<-function(x)list(checks=fit_checks(x),summary=summary(x),parameters=parameter_summary(x),
  posterior=posterior_summary(x),text=capture.output(print(x)),checks_text=capture.output(print(fit_checks(x))))
strip_optional<-function(x) {
  for(n in c('execution_receipt','resource_outcome','posterior_reporting'))x$run[[n]]<-NULL
  x
}
local({
  # An intentionally minimal, unvalidated saved context. No constructor or model
  # method creates it, and its numbers are test evidence, never a posterior.
  spec<-list(schema_version='synthetic_saved_context',
    tables=list(components=data.frame(component='G'),component_groups=data.frame(group='G'),amplitudes=data.frame(),
      parameter_residuals=data.frame(target=character()),targets=data.frame(key='y',model='block',level='subject_task'),
      person_law=list(distribution='iid'),augmentation=list(type='common_plus_parameter_residual'),
      correlation_coordinate_system=list(type='marginal')),
    models=list(block=list(model=list(name='saved_fixture',version='1'),units='test_units')),
    data_map=list(subject_keys=c('p1','p2'),blocks=list(block=data.frame(.person_key=c('p1','p2'),.observation_id=c('o1','o2')))),
    checks=list(identification=list(status='retained_fixture')))
  conditioning<-list(status='screened_not_proven',reason='synthetic stored screen',draws=data.frame(draw=1L,indicator=.01))
  optional<-list(status='available',reason='synthetic stored availability',unavailable_values=0L,
    flags=matrix(1,1,1),log_magnitudes=matrix(0,1,1),entries=data.frame(variable='Sigma[1,1]',unavailable_draws=0L,total_draws=1L))
  expanded<-list(status='available',entries=data.frame(variable='R[1,1]',mean=1,sd=NA_real_))
  cache<-list(parameter_summary=data.frame(variable='alpha[1]',parameter_key='test',parameter_role='test',rhat=1.2,ess_bulk=10,ess_tail=5),
    hmc=list(num_divergent=1L,num_max_treedepth=0L,ebfmi=.9),admissibility=conditioning,optional_reports=optional,
    expanded_correlation_matrices=expanded,identification=spec$checks$identification,
    posterior_propriety=list(status='retained_fixture'),inference=list(status='not_assessed'),
    computation=list(status='computed'),execution_profile=list(used='synthetic_fixture'))
  fit<-structure(list(spec=spec,run=list(chains=1L,iter_warmup=10L,iter_sampling=20L,reduction='serial'),
    cache=cache,provenance=list(kind='smoke'),backend=list(summary=deny('backend summary'),draws=deny('backend draws'),
      output=deny('backend output'),diagnostic_summary=deny('backend hmc'),output_files=deny('backend files'))),
    class=c('bmeas_fit','bhmm_fit'))
  baseline<-guarded(read_report(fit))
  ok(nrow(baseline$checks$context$blocks)==1L&&baseline$checks$context$run$configured$availability=='retained',
    'synthetic saved block and configured run are available without model preparation')
  plan<-list(resource_policy=list(kind='caller_resources',cache='deferred'),identification=cache$identification,
    posterior_propriety=cache$posterior_propriety,execution_profile=cache$execution_profile)
  deferred<-fit;deferred$cache<-private('.bh_uncomputed_fit_cache')(spec,plan);marker<-deferred$cache$posterior_reporting
  good<-guarded(read_report(deferred))
  ok(good$checks$domains$posterior_reporting$availability=='retained'&&is.na(good$checks$sampling$extrema$rhat$value),
    'valid uncomputed placeholders keep the deferred marker and unknown diagnostics')
  partial<-fit;partial$cache[c('parameter_summary','hmc','numerical_evaluations')]<-NULL;partial$cache$posterior_reporting<-marker
  result<-guarded(read_report(partial))
  ok(result$checks$domains$posterior_reporting$availability=='unavailable_invalid_metadata'&&
    identical(result$checks$domains$admissibility$record,conditioning)&&identical(result$checks$domains$optional_reports$record,optional)&&
    !any(grepl('covariance quantities remain unavailable',result$text,fixed=TRUE))&&
    any(grepl('Optional covariance/score reports: available',result$text,fixed=TRUE)),
    'partial retained conditioning and availability are never relabelled as deferred')
  variants<-list(conditioning=list(field='admissibility',value=list(status='not_assessed',draws=conditioning$draws)),
    flags=list(field='optional_reports',value=list(status='not_assessed',flags=optional$flags)),
    logs=list(field='optional_reports',value=list(status='not_assessed',log_magnitudes=optional$log_magnitudes)),
    counts=list(field='optional_reports',value=list(status='not_assessed',entries=optional$entries)),
    expanded=list(field='expanded_correlation_matrices',value=list(status='not_assessed',entries=expanded$entries)))
  bundle<-list(baseline=fit,deferred=deferred,partial=partial)
  for(n in names(variants)) {
    x<-deferred;x$cache[c('admissibility','optional_reports','expanded_correlation_matrices')]<-NULL
    v<-variants[[n]];x$cache[[v$field]]<-v$value;out<-guarded(read_report(x))
    ok(out$checks$domains$posterior_reporting$availability=='unavailable_invalid_metadata'&&
      identical(out$summary$cache[[v$field]],v$value),paste('retained',n,'alone invalidates only the marker'))
    bundle[[n]]<-x
  }
  static<-deferred
  static$cache$admissibility<-list(status='not_applicable',draws=data.frame(),threshold=.01)
  static$cache$optional_reports<-list(status='not_applicable',unavailable_values=0L,flags=matrix(numeric(),0,0),
    log_magnitudes=list(status='not_assessed',reason='uncomputed'),entries=data.frame(variable=character(),total_draws=numeric()))
  static$cache$expanded_correlation_matrices<-list(status='not_applicable',entries=data.frame())
  static$cache$correlation_constraints<-list(primitives=data.frame(parameter='static'),fixed=matrix(1,1,1))
  out<-guarded(read_report(static))
  ok(out$checks$domains$posterior_reporting$availability=='retained'&&
    identical(out$summary$cache$correlation_constraints,static$cache$correlation_constraints)&&
    identical(out$checks$domains$identification,good$checks$domains$identification),
    'empty placeholders and static scientific records are not posterior contradictions')
  bundle$static<-static
  classed<-deferred;classed$cache$optional_reports<-structure(optional,class='passive_probe')
  out<-guarded(read_report(classed))
  ok(out$checks$domains$posterior_reporting$availability=='unavailable_invalid_metadata'&&
    identical(out$summary$cache$optional_reports,classed$cache$optional_reports),
    'classed retained payload invalidates the marker without invoking extraction or formatting methods')
  bundle$classed_payload<-classed
  mixed<-deferred;mixed$cache$admissibility<-conditioning
  out<-guarded(read_report(mixed))
  ok(out$checks$domains$posterior_reporting$availability=='unavailable_invalid_metadata'&&
    !any(grepl('all draws retained',out$text,fixed=TRUE))&&
    any(grepl('Optional covariance/score reports: deferred_by_policy',out$text,fixed=TRUE)),
    'a still-uncomputed optional placeholder stays explicit when another retained domain invalidates the global marker')
  bundle$mixed_placeholder<-mixed
  for(n in c('execution_receipt','resource_outcome','posterior_reporting','combined')) {
    x<-fit
    if(n%in%c('execution_receipt','combined'))x<-c(x,list(execution_receipt=list(),execution_receipt=list()))
    if(n%in%c('resource_outcome','combined'))x<-c(x,list(resource_outcome=list(),resource_outcome=list()))
    if(n%in%c('posterior_reporting','combined'))x$cache<-c(x$cache,list(posterior_reporting=marker,posterior_reporting=marker))
    class(x)<-class(fit);out<-guarded(read_report(x))
    ok(identical(strip_optional(out$checks$context),strip_optional(baseline$checks$context))&&
      identical(out$checks$sampling,baseline$checks$sampling),paste('duplicate optional',n,'preserves all unrelated context and cached metrics'))
    if(n%in%c('execution_receipt','combined'))ok(out$checks$context$run$execution_receipt$availability=='unavailable_invalid_metadata','duplicate receipt is locally unavailable')
    if(n%in%c('resource_outcome','combined'))ok(out$checks$context$run$resource_outcome$availability=='unavailable_invalid_metadata','duplicate outcome is locally unavailable')
    bundle[[paste0('duplicate_',n)]]<-x
  }
  for(n in c('spec','run','cache')) {
    x<-c(fit,setNames(list(fit[[n]]),n));class(x)<-class(fit);out<-guarded(read_report(x))
    ok(nrow(out$checks$context$blocks)==0L&&out$checks$context$run$configured$availability=='unavailable_in_cache',
      paste('genuinely duplicated',n,'retains existing strict context handling'))
    bundle[[paste0('strict_',n)]]<-x
  }
  bundle<-lapply(bundle,function(x){x$backend<-NULL;x})
  before<-serialize(bundle,NULL,version=2);warm<-guarded(lapply(bundle,read_report))
  path<-tempfile(fileext='.rds');on.exit(unlink(path),add=TRUE)
  saveRDS(bundle,path,version=2);cold<-readRDS(path)
  ok(identical(warm,guarded(lapply(cold,read_report))),'detached cold roundtrip preserves every partial/duplicate/static report exactly')
  ok(identical(before,serialize(bundle,NULL,version=2)),'passive projection leaves saved fixture bytes unchanged')
  ok(!length(calls),'all model/backend/cache/provider/file/native terminal traps remain untouched during reporting')
})
cat('Passive resource locality checks:',passed,'passed; synthetic saved records only, no model/provider/posterior operation.\n')
