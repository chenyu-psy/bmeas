.bh_compile_identity <- function(spec, save_log_lik) {
  code<-as.character(stancode(spec,threads=2L,log_lik=save_log_lik))
  fingerprint<-.mp_toolchain_fingerprint(list(stan_threads=TRUE,O=0),.mp_stanc_options())
  if(.bh_reference_mode(spec)||.bh_affine_engine(spec))fingerprint$environment[c("CPPFLAGS","CXXFLAGS")]<-.bh_reference_compiler_environment()
  list(code=code,toolchain=fingerprint,version=if(.bh_affine_engine(spec))"bmeas-affine-correlation-labels-v1"else if(.bh_reference_mode(spec))"bhmm-centered-task-raw-coordinates-v1"else"bhmm-marker-scale-expression-mgs-v7",
    resources=lapply(.mp_adapter_resources(spec),function(r)r[c("sha256","exports","cache_name")]),
    compiler_support="one STAN_THREADS binary; runtime serial or reduce_sum_static")
}
.bh_compile_scalar <- function(spec,cache_dir,save_log_lik=FALSE) {
  .bh_affine_runtime_gate(spec)
  if(!requireNamespace("cmdstanr",quietly=TRUE)).mp_abort("E_BACKEND_MISSING","CmdStanR and CmdStan are required for explicit fitting")
  .mp_scalar_string(cache_dir,"cache_dir")
  if(.bh_reference_mode(spec)||.bh_affine_engine(spec)) {
    original_environment<-Sys.getenv(c("CPPFLAGS","CXXFLAGS"),unset=NA_character_)
    on.exit({for(name in names(original_environment))if(is.na(original_environment[[name]]))Sys.unsetenv(name)else do.call(Sys.setenv,stats::setNames(list(original_environment[[name]]),name))},add=TRUE)
    do.call(Sys.setenv,as.list(.bh_reference_compiler_environment()))
  }
  identity<-.bh_compile_identity(spec,save_log_lik);key<-.mp_hash(identity)
  directory<-file.path(cache_dir,key);dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  if(!dir.exists(directory)).mp_abort("E_OUTPUT","could not create compilation cache")
  stan_file<-file.path(directory,"scalar-components.stan")
  changed<-.mp_sync_stan_source(identity$code,stan_file)
  for(resource in .mp_adapter_resources(spec)) {
    destination<-file.path(directory,resource$cache_name)
    if(!file.exists(destination)||.mp_sha256(destination)!=resource$sha256) {
      if(!file.copy(resource$file,destination,overwrite=TRUE)||.mp_sha256(destination)!=resource$sha256).mp_abort("E_RESOURCE_HASH","failed to copy a pinned external resource")
      changed<-TRUE
    }
  }
  manifest_file<-file.path(directory,"compile-manifest.rds")
  manifest<-if(file.exists(manifest_file))tryCatch(readRDS(manifest_file),error=function(e)NULL)else NULL
  cached<-!changed && !is.null(manifest) && identical(manifest$identity,identity) &&
    file.exists(manifest$executable) && identical(.mp_sha256(manifest$executable),manifest$executable_sha256)
  model<-if(cached)cmdstanr::cmdstan_model(stan_file,exe_file=manifest$executable,compile=FALSE,
    cpp_options=list(stan_threads=TRUE,O=0),stanc_options=.mp_stanc_options())else
    cmdstanr::cmdstan_model(stan_file,cpp_options=list(stan_threads=TRUE,O=0),stanc_options=.mp_stanc_options(),force_recompile=TRUE)
  if(cached) {
    # CmdStanR's compile=FALSE constructor keeps options in precompile storage.
    # The public up-to-date compile path registers them without invoking make.
    executable_time<-file.info(manifest$executable)$mtime
    if(file.info(stan_file)$mtime>executable_time && !Sys.setFileTime(stan_file,executable_time))
      .mp_abort("E_CACHE","could not align verified cache-source time for the no-build loader")
    if(file.info(stan_file)$mtime>file.info(manifest$executable)$mtime)
      .mp_abort("E_CACHE","cached source is newer than executable; refusing an ambiguous reuse")
    model$compile(cpp_options=list(stan_threads=TRUE,O=0),stanc_options=.mp_stanc_options(),force_recompile=FALSE)
    if(!identical(file.info(manifest$executable)$mtime,executable_time) ||
       !identical(.mp_sha256(manifest$executable),manifest$executable_sha256))
      .mp_abort("E_CACHE","verified executable changed during its no-build load")
  }
  if(!isTRUE(model$cpp_options()$stan_threads)) .mp_abort("E_THREADING_UNSUPPORTED","loaded CmdStanR model did not register verified threading support")
  record<-list(identity=identity,executable=model$exe_file(),executable_sha256=.mp_sha256(model$exe_file()))
  saveRDS(record,manifest_file)
  list(model=model,identity=identity,cache_key=key,cache_hit=cached,executable_sha256=record$executable_sha256)
}
.bh_numeric_cache <- function(backend,spec) {
  tryCatch({
    dimensions<-c(component=nrow(spec$tables$components),parameter=nrow(spec$tables$parameter_residuals))
    if(!any(dimensions>0L))return(list(status="not_applicable",reason="there are no random component or parameter-residual correlation matrices to screen; task/fixed means do not define person-level correlations",draws=data.frame()))
    blocks<-lapply(names(dimensions)[dimensions>0L],function(layer) {
      K<-dimensions[[layer]];square_factor<-layer=="component"&&.bh_raw_cor(spec)&&.bh_task_augmentation(spec)
      symbol<-if(square_factor)"F_component"else paste0("L_",layer)
      draws<-as.matrix(backend$draws(variables=symbol,format="draws_matrix"));class(draws)<-c("matrix","array")
      labels<-paste0(symbol,"[",rep(seq_len(K),K),",",rep(seq_len(K),each=K),"]")
      stopifnot(all(labels%in%colnames(draws)))
      stored<-NULL
      stored_matrix<-square_factor||.bh_is_affine_cor_table(if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations)
      if(stored_matrix) {
        stored<-as.matrix(backend$draws(variables=paste0("R_",layer),format="draws_matrix"))
        keys<-paste0("R_",layer,"[",rep(seq_len(K),K),",",rep(seq_len(K),each=K),"]")
        stopifnot(all(keys%in%colnames(stored)),nrow(stored)==nrow(draws));stored<-stored[,keys,drop=FALSE]
      }
      do.call(rbind,lapply(seq_len(nrow(draws)),function(i){
        L<-matrix(as.numeric(draws[i,labels]),K,K);R<-tcrossprod(L)
        if(stored_matrix)R<-matrix(as.numeric(stored[i,]),K,K)
        singular<-svd(L,nu=0,nv=0)$d;indicator<-(max(singular)/min(singular))^2*.Machine$double.eps
        data.frame(draw=i,layer=layer,condition_roundoff_indicator=indicator,
          min_stored_eigenvalue=min(eigen(R,symmetric=TRUE,only.values=TRUE)$values),
          matrix_export_warning=!is.finite(indicator)||indicator>1e-6||(stored_matrix&&min(eigen(R,symmetric=TRUE,only.values=TRUE)$values)<=0),stringsAsFactors=FALSE)
      }))
    })
    table<-do.call(rbind,blocks)
    list(status=if(any(table$matrix_export_warning))"warning"else"screened_not_proven",
      reason=if(.bh_centered_final_marginal_mode(spec))"Squared correlation-factor condition number times machine epsilon: ordinary MGS layers use the factor product, while affine residual layers inspect their stored matrix. This is a numerical screen, not a target truncation or proof of HMC reliability"else if(.bh_reference_mode(spec))"Squared covariance-factor condition number times machine epsilon, with the rounded stored component R checked separately; reporting warnings do not truncate the task target or establish HMC reliability"else"kappa2(L)^2 * machine epsilon is a conservative matrix-export conditioning screen, not a target truncation or proof of HMC reliability",
      draws=table,threshold=1e-6,
      total_predictor_scope="Sigma_predictor combines common and parameter-residual covariance; it can be positive semidefinite by design and is not relabeled as a correlation block")
  },error=function(e)list(status="not_assessed",reason=paste("conditioning cache unavailable:",conditionMessage(e))))
}
.bh_cache_scalar_fit <- function(backend,spec,plan) {
  .bh_final_correlation_reporting_gate(spec)
  if(.bh_reference_mode(spec)&&!.bh_reference_runtime_supported(spec)).mp_abort("E_REFERENCE_CACHE_UNAVAILABLE","this separate person/correlation/augmentation combination has no runtime cache profile; inspect its R/IR and source evidence")
  if(.bh_has_affine_correlations(spec)&&!.bh_affine_runtime_supported(spec)).mp_abort("E_AFFINE_CACHE_UNAVAILABLE","this affine correlation and person/augmentation combination has no runtime cache profile; source inspection does not imply execution qualification")
  K<-nrow(spec$tables$components);U<-nrow(spec$tables$parameter_residuals)
  affine_map<-.bh_affine_summary_map(spec)
  derived_marker<-.bh_marker_summary_map(spec)
  task<-.bh_task_augmentation(spec);reference_map<-.bh_reference_summary_map(spec)
  if(task)derived_marker<-derived_marker[!derived_marker$parameter_role%in%c("standardized_component_person_score","scaled_component_person_score"),,drop=FALSE]
  monitored<-c(if(nrow(.bh_free_component_sd(spec)))"component_sd",derived_marker$variable,if(nrow(spec$tables$targets))"alpha",if(nrow(.bh_amplitudes(spec)))"amplitude",if(nrow(.bh_free_loadings(spec)))"loading",if(nrow(spec$tables$observation_parameters))"obs_sd",
    if(.bh_has_shares(spec))c("total_sd",if(nrow(spec$tables$share_coefficients))"share_beta","share_weight"),
    if(nrow(.bh_free_parameter_sd(spec)))"parameter_sd",
    if(.bh_final_correlation_reporting(spec))c(affine_map$variable,if(K&&!task)"z_raw",if(U)c("parameter_row_sd",if(!task)"e_raw"))else
      c(if(K)c("R_component",if(!task)"z_raw"),if(U)c("parameter_row_sd","R_parameter",if(!task)"e_raw")),reference_map$variable)
  monitored<-unique(monitored)
  summaries<-tryCatch(as.data.frame(backend$summary(variables=monitored,
    "mean","median","sd","quantile2","rhat","ess_bulk","ess_tail","mcse_mean")),error=identity)
  hmc<-tryCatch(backend$diagnostic_summary(quiet=TRUE),error=identity)
  numerical_messages<-.bh_capture_numerical_messages(backend)
  numeric_cache<-.bh_numeric_cache(backend,spec)
  report_cache<-if(task).bh_reference_report_cache(backend,spec)else .bh_report_availability_cache(backend,spec)
  failed<-inherits(summaries,"error")||inherits(hmc,"error")
  if(inherits(summaries,"error"))summaries<-NULL
  if(inherits(hmc,"error"))hmc<-NULL
  if(!is.null(summaries)) {
    keys<-c(.bh_free_component_sd(spec)$parameter,.bh_amplitudes(spec)$parameter,if(nrow(spec$tables$targets))paste0("Intercept:",spec$tables$targets$key),.bh_free_loadings(spec)$parameter,
      if(nrow(spec$tables$observation_parameters))paste0("observation_sd:",spec$tables$observation_parameters$key),.bh_free_parameter_sd(spec)$parameter,
      spec$tables$share_targets$parameter,spec$tables$share_coefficients$parameter,
      if(.bh_has_shares(spec))vapply(seq_len(nrow(spec$tables$share_parts)),function(i).bh_public_id("variance_share",spec$tables$share_parts$owner[i],spec$tables$share_parts$target[i],spec$tables$share_parts$part[i]),character(1)))
    symbols<-c(if(nrow(.bh_free_component_sd(spec)))paste0("component_sd[",seq_len(nrow(.bh_free_component_sd(spec))),"]"),if(nrow(.bh_amplitudes(spec)))paste0("amplitude[",seq_len(nrow(.bh_amplitudes(spec))),"]"),if(nrow(spec$tables$targets))paste0("alpha[",seq_len(nrow(spec$tables$targets)),"]"),if(nrow(.bh_free_loadings(spec)))paste0("loading[",seq_len(nrow(.bh_free_loadings(spec))),"]"),
      if(nrow(spec$tables$observation_parameters))paste0("obs_sd[",seq_len(nrow(spec$tables$observation_parameters)),"]"),
      if(nrow(.bh_free_parameter_sd(spec)))paste0("parameter_sd[",seq_len(nrow(.bh_free_parameter_sd(spec))),"]"),
      if(.bh_has_shares(spec))c(paste0("total_sd[",seq_len(nrow(spec$tables$share_targets)),"]"),if(nrow(spec$tables$share_coefficients))paste0("share_beta[",seq_len(nrow(spec$tables$share_coefficients)),"]"),paste0("share_weight[",seq_len(nrow(spec$tables$share_parts)),"]")))
    roles<-c(rep("component_sd",nrow(.bh_free_component_sd(spec))),rep("amplitude",nrow(.bh_amplitudes(spec))),rep("Intercept",nrow(spec$tables$targets)),rep("loading",nrow(.bh_free_loadings(spec))),
      rep("observation_sd",nrow(spec$tables$observation_parameters)),rep("parameter_sd",nrow(.bh_free_parameter_sd(spec))),
      rep("total_sd",length(spec$tables$share_targets$parameter)),rep("share",length(spec$tables$share_coefficients$parameter)),rep("derived_variance_share",length(spec$tables$share_parts$part)))
    summaries$parameter_key<-keys[match(summaries$variable,symbols)]
    summaries$parameter_role<-roles[match(summaries$variable,symbols)]
    at<-match(summaries$variable,derived_marker$variable);derived<-!is.na(at)
    summaries$parameter_key[derived]<-derived_marker$parameter_key[at[derived]]
    summaries$parameter_role[derived]<-derived_marker$parameter_role[at[derived]]
    summaries$loading_aliases<-spec$tables$loading_primitives$members[match(summaries$parameter_key,spec$tables$loading_primitives$parameter)]
    if(!is.null(spec$tables$parameter_roles)) {
      target_keys<-spec$tables$priors$target[match(summaries$parameter_key,spec$tables$priors$parameter)]
      summaries$target_variation_level<-spec$tables$parameter_roles$level[match(target_keys,spec$tables$parameter_roles$key)]
    }
    K<-nrow(spec$tables$components)
    for(i in seq_len(K))for(j in seq_len(K))summaries$parameter_key[summaries$variable==paste0("R_component[",i,",",j,"]")]<-
      .bh_public_id("component_correlation",spec$tables$components$component[i],spec$tables$components$component[j])
    for(i in seq_len(U))for(j in seq_len(U))summaries$parameter_key[summaries$variable==paste0("R_parameter[",i,",",j,"]")]<-
      .bh_public_id("parameter_residual_correlation",spec$tables$parameter_residuals$target[i],spec$tables$parameter_residuals$target[j])
    for(i in seq_len(U))summaries$parameter_key[summaries$variable==paste0("parameter_row_sd[",i,"]")]<-.bh_public_id("derived_parameter_sd",spec$tables$parameter_residuals$target[i])
    summaries$parameter_role[grepl("^R_component\\[|^R_parameter\\[",summaries$variable)]<-"marginal_correlation_matrix_entry"
    summaries$parameter_role[grepl("^parameter_row_sd\\[",summaries$variable)]<-"derived_parameter_sd"
    summaries$parameter_role[grepl("^z_raw\\[|^e_raw\\[",summaries$variable)]<-"latent_person_coordinate"
    if(.bh_reference_mode(spec)) {
      at<-match(summaries$variable,reference_map$variable);selected<-!is.na(at)
      summaries$parameter_key[selected]<-reference_map$parameter_key[at[selected]]
      summaries$parameter_role[selected]<-reference_map$parameter_role[at[selected]]
      for(field in c("target","person_id","basis_index","raw_block","previous_whitened_coordinate"))summaries[[paste0("reference_",field)]]<-reference_map[[field]][at]
      if(.bh_raw_cor(spec))summaries$parameter_role[grepl("^R_component\\[",summaries$variable)]<-"derived_marginal_component_correlation_entry"
    }
    if(.bh_final_correlation_reporting(spec)) {
      at<-match(summaries$variable,affine_map$variable);selected<-!is.na(at)
      summaries$parameter_key[selected]<-affine_map$parameter_key[at[selected]]
      summaries$parameter_role[selected]<-affine_map$parameter_role[at[selected]]
      summaries$correlation_layer<-affine_map$layer[at]
      summaries$correlation_alias_cells<-I(vector("list",nrow(summaries)))
      summaries$correlation_alias_cells[selected]<-affine_map$alias_cells[at[selected]]
    }
    summaries$parameter_role[is.na(summaries$parameter_role)]<-"computational"
    unknown<-is.na(summaries$parameter_key);summaries$parameter_key[unknown]<-paste0("computational:",summaries$variable[unknown])
  }
  capture_available<-identical(numerical_messages$output_capture$status,"succeeded")
  numerical_warning<-!capture_available||numerical_messages$observed_failure_messages>0L
  warning<-numerical_warning||identical(report_cache$status,"unavailable_values")||identical(numeric_cache$status,"warning")||(!is.null(hmc)&&any(hmc$num_divergent>0|hmc$num_max_treedepth>0))
  computation_reason<-"bounded engineering run; numerical completion is not usable posterior inference"
  if(!failed&&!capture_available)computation_reason<-paste("numerical-message capture unavailable;",computation_reason)
  else if(!failed&&numerical_messages$observed_failure_messages>0L)computation_reason<-paste("recognized numerical-message lines were captured;",computation_reason)
  result<-list(parameter_summary=summaries,fixed_parameters=spec$tables$fixed_parameters,parameter_roles=spec$tables$parameter_roles,
    loading_cells=spec$tables$loading_cells,loading_primitives=spec$tables$loading_primitives,loading_orientation=spec$tables$orientation,
    component_scales=spec$tables$component_scales,components=spec$tables$components,derived_marker_estimands=derived_marker,
    optional_reports=report_cache,
    hmc=hmc,computation=list(status=if(failed)"failed"else if(warning)"warning"else"computed",
    reason=computation_reason),
    numerical_evaluations=numerical_messages,
    admissibility=numeric_cache,posterior_propriety=plan$posterior_propriety,gaussian_diagnostics=plan$gaussian_diagnostics,execution_profile=plan$execution_profile,backend_qualification=plan$backend_qualification,identification=if(is.null(plan$identification))spec$checks$identification else plan$identification,
    inference=list(status="not_assessed",reason=.bh_resource_inference_reason(plan$resource_policy)),
    predictive_fit=.bh_ppc_initial_status(spec))
  if(.bh_final_correlation_reporting(spec)) {
    result$correlation_constraints<-.bh_affine_cache_metadata(spec,affine_map)
    result$expanded_correlation_matrices<-.bh_affine_matrix_cache(backend,spec)
  }
  if(.bh_reference_mode(spec)) {
    result$reference_law<-.bh_reference_cache_metadata(spec,reference_map)
    result$posterior_normalizability<-plan$posterior_normalizability
  }
  result
}
# Pure construction for an already resolved plan. This does not invoke the
# provider, initialize a fit, read output, or discover the native runtime.
.bh_fit_sample_args <- function(plan,data,init,output_dir,provider) {
  run<-plan$run
  sample_args<-list(data=data,chains=run$chains,parallel_chains=run$parallel_chains,
    threads_per_chain=run$threads_per_chain,iter_warmup=run$iter_warmup,iter_sampling=run$iter_sampling,
    seed=run$seed,init=function()init,output_dir=output_dir,refresh=0,
    max_treedepth=run$max_treedepth,adapt_delta=plan$adapt_delta,save_warmup=FALSE,sig_figs=run$csv_sig_figs)
  # CmdStanR 0.9.0 otherwise reads sampler-diagnostic draws in its constructor,
  # before sample() returns. NULL suppresses those posterior reads, but that
  # provider still scans CSV metadata internally; it is not a file-read-free call.
  if(identical(plan$resource_policy$cache,"deferred")) {
    sample_args["diagnostics"]<-list(NULL)
    sample_args$output_basename<-provider$output_basename
  }
  sample_args
}
.bh_fit_scalar <- function(spec,plan,cache_dir,output_dir,save_log_lik=FALSE) {
  .bh_affine_correlation_lowering_gate(spec)
  .bh_final_correlation_reporting_gate(spec)
  if(!sum(spec$tables$priors$parameter_count)).mp_abort("E_NO_FREE_PARAMETERS","this prepared model has no unknown parameters to sample; inspect fixed operands and generated likelihoods")
  plan<-.bh_revalidate_plan(spec,plan,save_log_lik)
  .bh_resource_fit_gate(plan)
  .bh_affine_runtime_gate(spec)
  if(!is.logical(save_log_lik)||length(save_log_lik)!=1L||is.na(save_log_lik)).mp_abort("E_ARGUMENT","save_log_lik must be explicit logical")
  .mp_scalar_string(output_dir,"output_dir")
  provider<-.bh_provider_preflight(plan,output_dir)
  output_dir<-provider$output_dir
  estimate<-plan$output_estimate$estimated_csv_bytes
  initial<-.bh_initialization(spec);.bh_initialization_gate(initial)
  init<-.bh_materialize_init(spec,initial)
  initialization_record<-initial[intersect(c("status","recipe_version","primitives","primitive_prior_totals","reasons","scope",
    "correlation_initialization","correlation_scientific","joint_correlation_prior_kernel","correlation_chart_log_jacobian","aggregate_kernel_with_correlation_charts"),names(initial))]
  initialization_record$constrained_init_hash<-.mp_hash(init)
  initialization_record$materialization_version<-"Stan_declared_parameter_ranks_v1"
  initialization_record$hash_convention<-"MD5 of saveRDS(version=2) constrained init including zero score matrices; singleton parameter vectors retain array rank1 and empty parameter fields are omitted"
  compiled<-.bh_compile_scalar(spec,cache_dir,save_log_lik)
  run<-plan$run;data<-standata(spec,threads=2,reduction=run$reduction,grainsize=run$grainsize)
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  if(!dir.exists(output_dir)).mp_abort("E_OUTPUT","could not create sample output directory")
  if(identical(provider$status,"reviewed_deferred_scope")&&
    !identical(.bh_provider_output_dir(output_dir),output_dir))
    .mp_abort("E_PROVIDER_PATH","the resolved output directory changed after provider preflight; sampling was not started")
  if(identical(provider$status,"reviewed_deferred_scope")&&
    !.bh_provider_basename_available(output_dir,provider$output_basename))
    .mp_abort("E_PROVIDER_OUTPUT_EXISTS","files already use the generated output basename; refusing to overwrite them before sampling")
  started<-Sys.time()
  sample_args<-.bh_fit_sample_args(plan,data,init,output_dir,provider)
  backend<-.bh_provider_sample(compiled$model,sample_args,provider)
  receipt<-.bh_execution_receipt(backend,run,started)
  if(!identical(receipt$sampling$status,"completed"))
    stop(structure(list(message="E_SAMPLING_FAILED: backend sampling completion was not established; inspect retained output paths",
      call=NULL,code="E_SAMPLING_FAILED",execution_receipt=receipt,output_files=receipt$output$paths),
      class=c("measurement_error","error","condition")))
  outputs<-receipt$output$paths
  outcome<-.bh_output_resource_outcome(receipt,plan$resource_policy$limits$max_output_bytes)
  # Policy/resource branches precede every posterior summary, HMC diagnostic,
  # captured-output call, draw reader and draw-dependent cache helper.
  cache<-if(!identical(outcome$status,"within_limit"))
    .bh_uncomputed_fit_cache(spec,plan,"uncomputed_output_resource",paste("Posterior diagnostics are uncomputed:",outcome$reason))else
    if(identical(plan$resource_policy$cache,"deferred")) .bh_uncomputed_fit_cache(spec,plan)else
      .bh_cache_scalar_fit(backend,spec,plan)
  cache$initialization<-initialization_record
  fit<-structure(list(spec=spec,run=run,backend=backend,cache=cache,code=compiled$identity$code,
    execution_receipt=receipt,resource_outcome=outcome,
    provenance=list(kind=plan$resource_policy$kind,started=started,finished=Sys.time(),compile_key=compiled$cache_key,
      cache_hit=compiled$cache_hit,executable_sha256=compiled$executable_sha256,toolchain=compiled$identity$toolchain,
      initialization=initialization_record,model_fingerprint=spec$fingerprint,iter_total=run$iter_warmup+run$iter_sampling,
      execution_profile=plan$execution_profile,exploratory_requested=isTRUE(plan$exploratory),exploratory_rank_override=isTRUE(plan$exploratory_rank_override),
      save_log_lik=save_log_lik,csv_sig_figs=run$csv_sig_figs,estimated_csv_bytes=estimate,output_files=outputs,
      resource_policy=plan$resource_policy,resource_admission=plan$resource_admission,
      output_estimate=plan$output_estimate,cache_estimate=plan$cache_estimate,
      requested_run=plan$request,adapt_delta=plan$adapt_delta,plan_identity=plan$identity,
      scientific_checks=plan$scientific_checks,postprocessing_support=plan$postprocessing_support,
      provider=provider,provider_metadata=plan$provider_metadata)),class=c("bmeas_fit","bhmm_fit"))
  if(!identical(outcome$status,"within_limit")) .bh_postfit_resource_error(fit)
  fit
}
posterior_summary <- function(object) {
  if(inherits(object,"bmeas_fit_archive"))return(.bh_archive_report(object,"posterior_summary"))
  .bh_pf_public_input(object,"posterior_summary")
  if(.bh_pf_is_portable(object))return(.bh_pf_get(.bh_pf_envelope(object)$cache,"parameter_summary"))
  if(inherits(object,"measurement_fit"))return(parameter_summary(object))
  if(!inherits(object,"bhmm_fit")).mp_abort("E_FIT","expected a bmeas fit")
  object$cache$parameter_summary
}
summary.bhmm_fit <- function(object,...) {
  .bh_pf_legacy_report_input(object,"summary",allow_private=TRUE)
  portable<-.bh_pf_is_view(object)
  checks<-.bh_fit_checks(object)
  if(portable)object<-.bh_pf_summary_metadata(object)
  structure(list(cache=object$cache,run=object$run,components=object$spec$tables$components,
    component_groups=object$spec$tables$component_groups,amplitudes=.bh_amplitudes(object$spec),
    parameter_roles=object$cache$parameter_roles,fixed_parameters=object$cache$fixed_parameters,task_target_domain=object$spec$tables$task_target_domain,
    loading_cells=object$cache$loading_cells,loading_primitives=object$cache$loading_primitives,component_scales=object$cache$component_scales,correlation_constraints=object$cache$correlation_constraints,
    share_targets=object$spec$tables$share_targets,share_coefficients=object$spec$tables$share_coefficients,
    parameter_residuals=if(is.null(object$spec$tables$parameter_residuals))data.frame()else object$spec$tables$parameter_residuals,targets=object$spec$tables$targets,subjects=if(portable).bh_pf_summary_count(checks,c("context","subjects","record","joint_subject_count"))else length(object$spec$data_map$subject_keys),
    observations=if(portable).bh_pf_summary_count(checks,c("context","consumed_rows","record"))else sum(vapply(object$spec$data_map$blocks,nrow,integer(1))),
    person_law=object$spec$tables$person_law,augmentation=object$spec$tables$augmentation,correlation_coordinates=object$spec$tables$correlation_coordinate_system,
    execution_profile=object$provenance$execution_profile,exploratory_rank_override=isTRUE(object$provenance$exploratory_rank_override),
    checks=checks,context=checks$context,blocks=checks$context$blocks,overlap=checks$context$overlap$record,
    excluded_responses=checks$context$exclusions$count),class=c("summary.bmeas_fit","summary.bhmm_fit"))
}
print.summary.bhmm_fit <- function(x,...) {
  if(inherits(x,"summary.bmeas_portable_fit")) .bh_pf_fail("print","use the portable summary method for portable report tags","E_PORTABLE_UNSUPPORTED")
  historical<-is.null(x$checks)
  report<-if(historical).bh_project_fit_checks(list(cache=x$cache,run=x$run))else x$checks
  context<-report$context
  retained<-function(node)if(is.list(node)&&identical(node$availability,"retained"))node$record else NULL
  count<-function(tab)if(is.data.frame(tab))nrow(tab)else NA_integer_
  plain_column<-function(tab,name,type) is.data.frame(tab)&&name%in%names(tab)&&typeof(tab[[name]])==type&&is.null(dim(tab[[name]]))&&!is.object(tab[[name]])&&!anyNA(tab[[name]])
  subjects<-if(historical)x$subjects else context$subjects$record$joint_subject_count
  observations<-if(historical)x$observations else retained(context$consumed_rows)
  person_record<-if(historical)x$person_law else retained(context$person_law)
  person<-if(is.list(person_record)&&is.character(person_record$distribution)&&length(person_record$distribution)==1L&&!is.na(person_record$distribution))person_record$distribution else"unavailable in cache"
  augmentation<-if(historical)x$augmentation else retained(context$augmentation)
  if(!is.list(augmentation))augmentation<-NULL
  task<-!is.null(augmentation)&&identical(augmentation$type,"task_cholesky")
  provenance<-retained(context$provenance)
  resource_policy<-if(is.list(provenance)).bh_report_slot(provenance,"resource_policy")else NULL
  if(historical)cat("bmeas historical summary | SMOKE ONLY | stored subject-domain count:",.bh_report_display(subjects),"| stored likelihood-row count:",.bh_report_display(observations),"\n")else
    cat("bmeas component fit |",.bh_resource_run_label(resource_policy),"|",.bh_report_display(subjects),"subject-domain IDs |",.bh_report_display(observations),"likelihood rows\n")
  components<-if(historical)x$components else retained(context$estimands$record$components)
  residuals<-if(historical)x$parameter_residuals else retained(context$scales$record$parameter_residuals)
  targets<-if(historical)x$targets else retained(context$targets)
  groups<-x$component_groups
  amplitudes<-if(is.data.frame(groups))x$amplitudes else NULL
  cat(.bh_report_display(count(groups)),"common component groups,",.bh_report_display(count(components)),"scalar coordinates,",.bh_report_display(count(amplitudes)),"shared amplitudes;",.bh_report_display(count(residuals)),"parameter residuals;",.bh_report_display(count(targets)),"target parameters; person law:",person,"; separate observation noise\n")
  if(task)cat(augmentation$task_white_coordinates,"sampled task-white basis coordinates and",augmentation$task_constrained_scores,"centered roster-indexed white scores; task deviations are distinct, and common/residual generative scores are unsampled.\n")
  if(plain_column(x$parameter_roles,"role","character")&&plain_column(x$parameter_roles,"level","character")) {
    cat(sum(x$parameter_roles$role=="predictor"&x$parameter_roles$level=="subject_task"),"declared subject-task targets;")
    active<-if(plain_column(x$task_target_domain,"level","character")&&plain_column(x$task_target_domain,"has_symbolic_support","logical"))sum(x$task_target_domain$level=="subject_task"&x$task_target_domain$has_symbolic_support)else NA_integer_
    cat("",.bh_report_display(active),"symbolically active person-effect targets;")
    cat("",sum(x$parameter_roles$role=="predictor"&x$parameter_roles$level=="task"),"free task means,",.bh_report_display(count(x$fixed_parameters)),"fixed parameter/task values. Task-mean uncertainty is not person-level variation.\n")
  }
  if(plain_column(x$loading_cells,"state","character"))cat("Ordinary loading map:",sum(x$loading_cells$state=="free"),"free cells backed by",.bh_report_display(count(x$loading_primitives)),"unique coefficients;",sum(x$loading_cells$state=="fixed"),"fixed cells. Aliases share one posterior coefficient and fixed declared beta cells have no loading-primitive uncertainty row; marker-scaled effective loadings have separate derived rows.\n")
  if(plain_column(x$component_scales,"state","character")&&any(x$component_scales$state=="free"))cat(sum(x$component_scales$state=="free"),"population component SDs; Lambda is declared beta and effective loadings are beta times SD.",if(is.null(augmentation))"Person-score augmentation is unavailable in this saved context.\n"else if(task)"Common scores are not sampled by task augmentation.\n"else"Standardized z differs from scaled f.\n")
  if(is.list(x$correlation_constraints)&&is.data.frame(x$correlation_constraints$primitives))cat(nrow(x$correlation_constraints$primitives),"unique free marginal-correlation labels; repeated matrix cells are aliases and fixed cells have no correlation uncertainty/prior row.\n")
  if(is.data.frame(x$share_targets))cat(nrow(x$share_targets),"target total SDs and",.bh_report_display(count(x$share_coefficients)),"unique logit-contrast coefficients; variance shares are derived. Correlation prior coordinates:",if(!is.list(x$correlation_coordinates)||is.null(x$correlation_coordinates$type))"unavailable in cache"else x$correlation_coordinates$type,".\n")
  run<-if(historical)x$run else retained(context$run$configured)
  if(!is.list(run))run<-NULL
  cat("Configured run:",.bh_report_display(run$chains),"chain(s),",.bh_report_display(run$iter_warmup),"warmup +",.bh_report_display(run$iter_sampling),"sampling iterations per chain;",.bh_report_display(run$reduction),"likelihood\n")
  profile<-if(historical)x$execution_profile else retained(context$execution_profile)
  if(!is.list(profile))profile<-NULL
  cat("Execution profile:",.bh_report_display(profile$used),"| evidence remains bounded\n")
  callback_blocks<-if(is.list(profile$support))profile$support$blocks else NULL
  if(is.list(callback_blocks)&&any(vapply(callback_blocks,function(b)is.list(b)&&is.character(b$callback_dependencies)&&length(b$callback_dependencies)==1L&&!is.na(b$callback_dependencies)&&startsWith(b$callback_dependencies,"caller_managed"),logical(1))))
    cat("Custom callback dependencies remain caller-managed; serialized context does not freeze mutable global/namespace state.\n")
  if(isTRUE(x$exploratory_rank_override))cat("Explicit exploratory override: unresolved local rank deficiency remains; this is not identified inference.\n")
  if(historical)cat("Historical summary: additional model/data context was not retained; cached diagnostics follow.\n")
  .bh_print_fit_checks(report,context=!historical,header=FALSE)
  optional_reports<-.bh_report_slot(x$cache,"optional_reports")
  if(is.list(optional_reports)) {
    reporting<-.bh_posterior_reporting_record(x$cache)
    optional_status<-.bh_saved_field(optional_reports,"status")
    if(!is.null(reporting)&&identical(reporting$availability,"retained")&&
      identical(.bh_saved_field(reporting$record,"version"),"bmeas_posterior_reporting_v1"))cat("Optional covariance/score reports:",reporting$record$status,"|",reporting$record$reason,"\n")else
      if(.bh_saved_string(optional_status)&&optional_status%in%c("deferred_by_policy","uncomputed_output_resource")&&
        .bh_saved_string(.bh_saved_field(optional_reports,"reason"))&&
        !.bh_retained_optional_posterior_evidence(list(optional_reports=optional_reports)))
        cat("Optional covariance/score reports:",.bh_saved_field(optional_reports,"status"),"|",.bh_saved_field(optional_reports,"reason"),"\n")else
        cat("Optional covariance/score reports:",.bh_optional_report_scalar(.bh_saved_field(optional_reports,"status")),
          "| unavailable values:",.bh_optional_report_scalar(.bh_saved_field(optional_reports,"unavailable_values")),"| all draws retained\n")
  }
  cat("No claim of usable posterior inference, global identification or full boundary/HMC qualification.\n")
  invisible(x)
}
print.bhmm_fit <- function(x,...) {.bh_pf_legacy_report_input(x,"print");print(summary(x));invisible(x)}

summary.bmeas_fit <- summary.bhmm_fit
print.bmeas_fit <- print.bhmm_fit
print.summary.bmeas_fit <- print.summary.bhmm_fit
