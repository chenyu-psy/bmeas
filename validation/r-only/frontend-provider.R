source(file.path("helpers", "native-guard.R"))
# Reviewed provider/path boundary with definitions and tiny R doubles only.
# No installed provider model, metadata/CSV reader, compiler or sampler is run.
source_root<-Sys.getenv('MEASUREMENTPKG_SOURCE');source_mode<-nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,'bmeas')
passed<-0L
ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
local({
  env<-if(source_mode).GlobalEnv else asNamespace('bmeas');originals<-list()
  replace<-function(name,value) {
    if(is.null(originals[[name]]))originals[[name]]<<-list(value=get(name,env,inherits=FALSE),locked=bindingIsLocked(name,env))
    if(bindingIsLocked(name,env))unlockBinding(name,env)
    assign(name,value,env);if(originals[[name]]$locked)lockBinding(name,env)
  }
  on.exit(for(name in names(originals)) {
    if(bindingIsLocked(name,env))unlockBinding(name,env)
    assign(name,originals[[name]]$value,env);if(originals[[name]]$locked)lockBinding(name,env)
  },add=TRUE)
  root<-tempfile('provider-guard-');dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  root<-normalizePath(root,winslash='/',mustWork=TRUE)
  runtime<-list(available=TRUE,cmdstanr_version='0.9.0',cmdstan_version='2.40.0',os_type='unix',wsl=FALSE)
  replace('.bh_provider_runtime_info',function()runtime)
  runtime_definition<-originals[['.bh_provider_runtime_info']]$value
  discovery_calls<-0L
  for(platform in list(list(os_type='windows',wsl=FALSE),list(os_type='unix',wsl=TRUE))) {
    platform_env<-new.env(parent=environment(runtime_definition))
    platform_env$.bh_provider_platform_info<-local({value<-platform;function()value})
    platform_env$requireNamespace<-function(...){discovery_calls<<-discovery_calls+1L;stop('FORBIDDEN_PROVIDER_DISCOVERY')}
    inspect<-runtime_definition;environment(inspect)<-platform_env
    inspected<-inspect()
    ok(inspected$discovery=='skipped_unsupported_platform'&&discovery_calls==0L,
      'unsupported platform returns before provider namespace/version discovery')
  }
  plan<-list(resource_policy=list(cache='deferred'))
  preflight<-private('.bh_provider_preflight');resolve<-private('.bh_provider_output_dir')
  error<-function(expr,code,label){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'measurement_error')&&e$code==code,label);invisible(e)}
  if(.Platform$OS.type=='unix') {
    safe<-file.path(root,'ordinary spaces','new child')
    set.seed(716);seed_before<-.Random.seed
    first<-preflight(plan,safe);second<-preflight(plan,safe)
    ok(identical(first$output_dir,safe)&&identical(first$requested_output_dir,safe)&&!dir.exists(safe),'ordinary spaces accepted without creating or renaming the requested directory')
    ok(!identical(first$output_basename,second$output_basename)&&grepl('^[A-Za-z0-9_-]+$',first$output_basename)&&identical(seed_before,.Random.seed),
      'generated basenames are distinct safe text and do not consume caller RNG')
    existing_name<-file.path(root,paste0(first$output_basename,'-1.csv'));writeLines('metadata only',existing_name)
    ok(!private('.bh_provider_basename_available')(root,first$output_basename)&&file.exists(existing_name),
      'existing output names are detected and preserved before provider invocation')
    original_name<-private('.bh_provider_new_basename');name_calls<-0L
    replace('.bh_provider_new_basename',function(){name_calls<<-name_calls+1L;if(name_calls==1L)first$output_basename else second$output_basename})
    selected<-preflight(plan,root)
    ok(selected$output_basename==second$output_basename&&name_calls==2L&&file.exists(existing_name)&&identical(seed_before,.Random.seed),
      'a name collision triggers a bounded name-only replacement without touching prior output or caller RNG')
    name_calls<-0L;replace('.bh_provider_new_basename',function(){name_calls<<-name_calls+1L;first$output_basename})
    error(preflight(plan,root),'E_PROVIDER_OUTPUT_EXISTS','repeated name collisions refuse after a bounded selection loop')
    ok(name_calls==16L&&file.exists(existing_name),'name retries are bounded and preserve existing files')
    replace('.bh_provider_new_basename',original_name)
    old<-getwd();setwd(root);relative<-preflight(plan,'relative child');setwd(old)
    ok(identical(relative$requested_output_dir,'relative child')&&identical(relative$output_dir,file.path(root,'relative child')),'requested and canonical relative directories are recorded separately')
    tilde<-preflight(plan,'~')
    ok(identical(tilde$output_dir,normalizePath(path.expand('~'),winslash='/'))&&tilde$requested_output_dir=='~','tilde expansion is checked and recorded rather than passed literally')
    unsafe<-file.path(root,"bad'parent");dir.create(unsafe)
    error(preflight(plan,unsafe),'E_PROVIDER_PATH','literal single quote is refused without sanitization')
    error(preflight(plan,paste0(root,'/bad\nchild')),'E_PROVIDER_PATH','control character is refused')
    bad<-rawToChar(as.raw(255));Encoding(bad)<-'UTF-8';warnings<-options(warn=2)
    error(preflight(plan,bad),'E_PROVIDER_PATH','invalid UTF-8 is refused before formatting or expansion')
    options(warnings)
    old<-getwd();setwd(unsafe);error(preflight(plan,'child'),'E_PROVIDER_PATH','unsafe expanded current directory is refused');setwd(old)
    alias<-file.path(root,'safe_alias');stopifnot(file.symlink(unsafe,alias))
    error(preflight(plan,file.path(alias,'new child')),'E_PROVIDER_PATH','safe spelling cannot hide an unsafe resolved symlink parent')
    safe_parent<-file.path(root,'safe parent');dir.create(safe_parent)
    safe_alias<-file.path(root,'safe_parent_alias');stopifnot(file.symlink(safe_parent,safe_alias))
    ok(identical(preflight(plan,file.path(safe_alias,'new child'))$output_dir,file.path(safe_parent,'new child')),'safe existing symlink parent resolves before a new suffix')
    # Expansion double avoids changing HOME while exercising an unsafe home.
    expand_env<-new.env(parent=environment(resolve));expand_env$path.expand<-function(x)file.path(unsafe,'new child')
    expand_double<-resolve;environment(expand_double)<-expand_env
    error(expand_double('~/new child'),'E_PROVIDER_PATH','unsafe tilde-expanded directory is checked')
  }
  for(variant in c('missing','cmdstanr','cmdstanr_development','cmdstan','windows','wsl')) {
    saved_runtime<-runtime
    if(variant=='missing')runtime$available<-FALSE
    if(variant=='cmdstanr')runtime$cmdstanr_version<-'0.9.1'
    if(variant=='cmdstanr_development')runtime$cmdstanr_version<-'0.9.0.9000'
    if(variant=='cmdstan')runtime$cmdstan_version<-'2.41.0'
    if(variant=='windows')runtime$os_type<-'windows'
    if(variant=='wsl')runtime$wsl<-TRUE
    error(preflight(plan,root),if(variant=='missing')'E_PROVIDER_UNAVAILABLE'else if(variant%in%c('cmdstanr','cmdstanr_development','cmdstan'))'E_PROVIDER_VERSION'else'E_PROVIDER_PLATFORM',paste('unreviewed provider',variant,'is refused'))
    runtime<-saved_runtime
  }
  replace('.bh_provider_runtime_info',function()stop('runtime discovery forbidden for eager scope'))
  eager<-preflight(list(resource_policy=list(cache='eager')),root)
  ok(eager$status=='eager_legacy_scope'&&is.null(eager$output_basename),'eager scope preserves provider defaults without deferred discovery or arguments')
  replace('.bh_provider_runtime_info',function()runtime)

  # Check pure count arithmetic independently using only a tiny explicit header.
  sampler<-c('lp__','accept_stat__','stepsize__','treedepth__','n_leapfrog__','divergent__','energy__')
  inventory<-data.frame(variable=c('theta','z',sampler),elements=c(6,10,rep(1,7)),stringsAsFactors=FALSE)
  inventory$dimensions<-I(c(list(c(2,3),10),rep(list(numeric()),7)))
  output<-list(status='estimated',inventory=inventory,estimated_csv_bytes=1234)
  dims<-list(total_sampled_parameter_dimension=16)
  metadata<-private('.bh_provider_metadata_plan')(dims,list(chains=2L),output,list(cache='deferred'))
  names_expanded<-c('theta.1.1','theta.2.1','theta.1.2','theta.2.2','theta.1.3','theta.2.3',paste0('z.',1:10),sampler)
  header_bytes<-nchar(paste(names_expanded,collapse=','),type='bytes')+1L
  ok(metadata$headers$encoded_text_bytes_per_chain==header_bytes&&metadata$headers$name_entries_all_chains==2*length(names_expanded),
    'count-only decimal-index sizing matches an independent tiny header including a two-digit index')
  ok(metadata$inverse_metric$scalar_slots==32&&metadata$inverse_metric$numeric_payload_bytes==256&&
    is.na(metadata$full_file_scan$actual_bytes)&&is.na(metadata$scalar_metadata$adaptation_and_other_numeric_slots),
    'default diagonal C by unconstrained-P cost is separate from unknown actual scan and other metadata costs')
  eager_metadata<-private('.bh_provider_metadata_plan')(dims,list(chains=2L),output,list(cache='eager'))
  missing_metadata<-private('.bh_provider_metadata_plan')(dims,list(chains=2L),output)
  ok(metadata$deferred_posterior_draw_scalar_slots==0&&metadata$provider$runtime_compatibility=='unverified'&&
    is.na(eager_metadata$deferred_posterior_draw_scalar_slots)&&eager_metadata$deferred_materialization_status=='inapplicable_eager'&&
    is.na(missing_metadata$deferred_posterior_draw_scalar_slots)&&missing_metadata$deferred_materialization_status=='uncomputed_policy_missing'&&
    eager_metadata$provider$runtime_compatibility=='not_applied_to_requested_mode',
    'zero posterior slots and runtime scope apply only to an actual deferred request')
  unavailable<-private('.bh_provider_metadata_plan')(list(total_sampled_parameter_dimension=2^53-1),list(chains=2L),
    list(status='unavailable_lowering',inventory=NULL,estimated_csv_bytes=NA_real_))
  ok(unavailable$status=='unavailable_lowering'&&is.na(unavailable$inverse_metric$scalar_slots),
    'absent lowering does not multiply hypothetical provider allocations or invent zero cost')
  error(private('.bh_provider_metadata_plan')(list(total_sampled_parameter_dimension=2^53-1),list(chains=2L),output),
    'E_RESOURCE_ESTIMATE','supported metadata overflow is refused before any array allocation')

  original<-structure(list(message='simulated provider metadata construction failure',call=NULL,marker=17L),
    class=c('test_provider_failure','error','condition'))
  calls<-0L
  model<-list(sample=function(...){calls<<-calls+1L;stop(original)})
  context<-list(output_dir=root,known_artifact_paths=character())
  failure<-tryCatch(private('.bh_provider_sample')(model,list(),context),error=identity)
  ok(inherits(failure,'bmeas_provider_error')&&identical(failure$cause,original)&&failure$provider_operation$status=='failed'&&
    failure$sampling$status=='unknown'&&is.null(failure$sampling$return_codes)&&is.null(failure$fit)&&identical(failure$output_dir,root)&&!length(failure$output_files)&&calls==1L,
    'failed provider call preserves original cause and known directory without retry, success claims or fabricated fit')
  interruption<-structure(list(message='test interrupt',call=NULL),class=c('interrupt','error','condition'))
  interrupt_model<-list(sample=function(...)stop(interruption))
  got<-tryCatch(private('.bh_provider_sample')(interrupt_model,list(),context),interrupt=identity,error=identity)
  ok(identical(got,interruption)&&!inherits(got,'bmeas_provider_error'),'interrupts propagate unchanged through the provider wrapper')

  # Public fit preflight must run before dense init or compilation.
  d<-data.frame(person=1:4,x1=c(.2,.8,.3,1),x2=c(.7,.6,-.3,.1),x3=c(-.2,.6,.9,.4))
  spec<-bmeas(bmf(G=~x1+x2+x3),d,subject='person',fit=FALSE)
  terminals<-character()
  for(name in c('.bh_materialize_init','.bh_compile_scalar'))replace(name,local({n<-name;function(...){terminals<<-c(terminals,n);stop('FORBIDDEN_PROVIDER_PREFLIGHT_ROUTE')}}))
  runtime$cmdstanr_version<-'unreviewed'
  error(bmeas(spec,chains=1,iter=2,warmup=1,resources=list(max_output_bytes=1024^2,cache='deferred'),output_dir=root),
    'E_PROVIDER_VERSION','public fitting refuses unreviewed provider before dense initialization or compile-cache access')
  ok(!length(terminals),'preflight failure leaves all dense/native boundary traps untouched')
})
cat('Frontend provider checks:',passed,'passed; definitions and R doubles only, with no provider CSV/model/native operation.\n')
