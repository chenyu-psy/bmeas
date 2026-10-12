# Explicit predictive computation is separate from passive fit reports.
.bh_ppc_plain_integer <- function(x,name,lower=1L) {
  if(!typeof(x)%in%c("integer","double")||is.object(x)||!is.null(dim(x)))
    .mp_abort("E_ARGUMENT",paste(name,"must be a plain numeric scalar"))
  .mp_integer(x,name,lower)
}
.bh_ppc_budget_defaults <- function() list(max_draws=20L,max_observations=1000L,
  max_simulated_values=20000L,max_source_bytes=10485760L,max_posterior_rows=100L,
  max_draw_values=50000L,max_latent_values=10000L,max_statistics=200L,max_pair_terms=50000L,max_work_values=1000000L)

.bh_ppc_streaming_budget <- function(budget) {
  if(typeof(budget)!="list"||isS4(budget))return(FALSE)
  names_<-attr(unclass(budget),"names",exact=TRUE)
  if(is.null(names_))return(FALSE)
  if(typeof(names_)!="character"||!is.null(attributes(names_))||anyNA(names_)||
    any(!validEnc(names_))||anyDuplicated(names_))
    .mp_abort("E_DIAGNOSTIC_BUDGET","budget has ambiguous field names")
  "posterior"%in%names_
}

.bh_ppc_budget_resolve <- function(budget) {
  if(!inherits(budget,"measurement_diagnostic_budget")||typeof(budget)!="list"||isS4(budget))
    .mp_abort("E_DIAGNOSTIC_BUDGET","an explicit diagnostic_budget is required")
  values<-unclass(budget);defaults<-.bh_ppc_budget_defaults()
  streaming<-.bh_ppc_streaming_budget(budget)
  if(streaming) {
    values<-.bh_resource_named_list(values,"diagnostic budget",c(names(defaults),"posterior"),"E_DIAGNOSTIC_BUDGET")
    acquisition<-c("max_source_bytes","max_posterior_rows","max_draw_values")
    if(any(acquisition%in%names(values)))
      .mp_abort("E_DIAGNOSTIC_BUDGET","streaming posterior policy cannot be mixed with legacy acquisition fields")
    if(!all(names(defaults)[1:3]%in%names(values)))
      .mp_abort("E_DIAGNOSTIC_BUDGET","the three predictive-work budget fields remain required")
    .bh_posterior_consumer_policy(values$posterior,character())
    old<-structure(values[setdiff(names(values),"posterior")],class="measurement_diagnostic_budget")
    resolved<-.bh_ppc_budget_resolve(old)
    resolved$values<-resolved$values[setdiff(names(resolved$values),acquisition)]
    resolved$origin<-resolved$origin[setdiff(names(resolved$origin),acquisition)]
    resolved$values$posterior<-values$posterior
    resolved$origin<-c(resolved$origin,posterior="explicit_streaming_policy")
    return(resolved)
  }
  if(is.null(names(values))||anyNA(names(values))||anyDuplicated(names(values))||
      any(!names(values)%in%names(defaults))||!all(names(defaults)[1:3]%in%names(values)))
    .mp_abort("E_DIAGNOSTIC_BUDGET","budget fields are missing, duplicated or unsupported")
  resolved<-defaults;origin<-rep("published_frontend_default",length(defaults));names(origin)<-names(defaults)
  for(n in names(values)) {
    x<-values[[n]]
    lower<-if(n=="max_draws")2L else if(n%in%c("max_draw_values","max_latent_values","max_statistics","max_pair_terms"))0L else 1L
    if(!typeof(x)%in%c("integer","double")||is.object(x)||!is.null(dim(x))||
        length(x)!=1L||!is.finite(x)||x!=floor(x)||x<lower||x>.Machine$integer.max)
      .mp_abort("E_DIAGNOSTIC_BUDGET",paste("invalid predictive budget",n))
    if(n%in%c("max_source_bytes","max_posterior_rows","max_draw_values")&&x>defaults[[n]])
      .mp_abort("E_DIAGNOSTIC_BUDGET",paste(n,"exceeds legacy acquisition; supply posterior=list(reader=\"streaming\",...) with explicit finite source/read/materialization allowances"))
    resolved[[n]]<-as.integer(x);origin[[n]]<-"explicit_budget"
  }
  list(values=resolved,origin=origin)
}

.bh_ppc_plain_list <- function(x,label,allow_class=FALSE,allow_null=FALSE) {
  if(is.null(x)&&allow_null)return(NULL)
  if(typeof(x)!="list"||isS4(x)||(!allow_class&&is.object(x))||
      !is.null(attr(x,"dim",exact=TRUE)))
    .mp_abort("E_PPC_LAYOUT",paste(label,"must be a saved plain list container"))
  value<-unclass(x);keys<-attr(value,"names",exact=TRUE)
  if(length(value)&&(typeof(keys)!="character"||is.object(keys)||
      !is.null(dim(keys))||length(keys)!=length(value)||anyNA(keys)||
      any(!nzchar(keys))||anyDuplicated(keys)))
    .mp_abort("E_PPC_LAYOUT",paste(label,"has missing, duplicate or ambiguous field names"))
  value
}

.bh_ppc_plain_value <- function(x,label,type,scalar=FALSE) {
  if(!typeof(x)%in%type||is.object(x)||!is.null(dim(x))||
      (scalar&&length(x)!=1L))
    .mp_abort("E_PPC_LAYOUT",paste(label,"must have its saved plain scalar/vector representation"))
  if(anyNA(x)||(typeof(x)%in%c("integer","double")&&any(!is.finite(x))))
    .mp_abort("E_PPC_LAYOUT",paste(label,"has a missing or nonfinite value"))
  x
}

.bh_ppc_validate_saved_spec <- function(spec) {
  original<-.bh_ppc_plain_list(spec,"specification",allow_class=TRUE)
  saved<-original$fingerprint;original$fingerprint<-NULL
  # Restore the original class only for serialization. Access and assignment
  # above used the local plain container, never a saved S3 accessor/replacer.
  original_attributes<-attributes(spec);original_attributes$names<-names(original)
  attributes(original)<-original_attributes
  if(!identical(saved,.mp_hash(original)))
    .mp_abort("E_SPEC_CHANGED","prepared context changed or was serialized under an incompatible package build; rebuild from the saved declaration and original data without resetting its fingerprint")
  invisible(TRUE)
}

.bh_ppc_context <- function(fit,level) {
  portable<-.bh_pf_is_view(fit)
  if((!inherits(fit,"bhmm_fit")&&!portable)||typeof(fit)!="list") .mp_abort("E_FIT","expected a main-API bmeas fit")
  fit<-.bh_ppc_plain_list(fit,"fit",allow_class=TRUE)
  if(!is.character(level)||is.object(level)||!is.null(dim(level))||length(level)!=1L||
      is.na(level)||!level%in%c("population","conditional"))
    .mp_abort("E_CHECK_UNSUPPORTED","select an explicit population or conditional predictive level")
  spec<-fit$spec;cache<-.bh_ppc_plain_list(fit$cache,"fit cache",allow_null=TRUE)
  if(!inherits(spec,"bhmm_spec")&&!(portable&&identical(.bh_pf_class(spec),"bmeas_portable_context")))
    .mp_abort("E_PPC_LAYOUT","predictive computation requires a retained structured specification; cached summaries cannot reconstruct it")
  spec<-.bh_ppc_plain_list(spec,"specification",allow_class=TRUE)
  spec$tables<-.bh_ppc_plain_list(spec$tables,"specification tables")
  spec$tables$person_law<-.bh_ppc_plain_list(spec$tables$person_law,"saved person law")
  spec$tables$augmentation<-.bh_ppc_plain_list(spec$tables$augmentation,"saved augmentation")
  person<-spec$tables$person_law;augmentation<-spec$tables$augmentation
  .bh_ppc_plain_value(person$distribution,"person distribution","character",TRUE)
  .bh_ppc_plain_value(person$roster_size,"roster size",c("integer","double"),TRUE)
  .bh_ppc_plain_value(augmentation$type,"augmentation type","character",TRUE)
  .bh_ppc_plain_value(augmentation$target_index,"task target indices",c("integer","double"))
  .bh_ppc_plain_value(augmentation$target,"task target keys","character")
  if(identical(person$distribution,"sample_centered")) {
    .bh_ppc_plain_value(augmentation$isometric_basis,"centered basis","character",TRUE)
    .bh_ppc_plain_value(augmentation$raw_coordinate_variance,"centered raw-coordinate variance",c("integer","double"),TRUE)
  }
  spec$data_map<-.bh_ppc_plain_list(spec$data_map,"consumed data map")
  spec$data_map$blocks<-.bh_ppc_plain_list(spec$data_map$blocks,"consumed blocks")
  spec$models<-.bh_ppc_plain_list(spec$models,"observation blocks")
  for(b in names(spec$models)) {
    block<-.bh_ppc_plain_list(spec$models[[b]],paste(b,"observation block"),allow_class=TRUE)
    .bh_ppc_plain_value(block$units,paste(b,"response units"),"character",TRUE)
    block$model<-.bh_ppc_plain_list(block$model,paste(b,"observation model"),allow_class=TRUE)
    block$data<-.bh_ppc_forward_frame(block$data,".task",paste(b,"declared data"))
    if(!is.null(block$parameter_bindings)) {
      block$parameter_bindings<-.bh_ppc_plain_list(block$parameter_bindings,paste(b,"parameter bindings"))
      block$parameter_bindings$domains<-.bh_ppc_forward_frame(block$parameter_bindings$domains,
        c("parameter","task"),paste(b,"parameter domain"))
    }
    spec$models[[b]]<-block
  }
  if(!is.character(spec$fingerprint)||is.object(spec$fingerprint)||!is.null(dim(spec$fingerprint))||
      length(spec$fingerprint)!=1L||is.na(spec$fingerprint))
    .mp_abort("E_PPC_LAYOUT","the saved specification identity is unavailable; rebuild explicitly without resealing")
  provenance<-.bh_ppc_plain_list(fit$provenance,"fit provenance",allow_class=TRUE)
  if(!identical(.bh_report_slot(provenance,"model_fingerprint"),spec$fingerprint))
    .mp_abort("E_PPC_LAYOUT","the saved fit and specification identities do not agree")
  code<-.bh_report_slot(fit,"code")
  if(!is.character(code)||is.object(code)||!is.null(dim(code))||length(code)!=1L||is.na(code)||!nzchar(code))
    .mp_abort("E_PPC_LAYOUT","the fitted source was not retained; predictive source binding is unavailable")
  for(n in c("posterior_propriety","posterior_normalizability")) {
    record<-.bh_report_slot(cache,n)
    if(is.null(record))next
    record<-.bh_ppc_plain_list(record,paste("saved",n,"record"),allow_class=TRUE)
    status<-.bh_report_slot(record,"status")
    if(!is.null(status)&&(!is.character(status)||is.object(status)||!is.null(dim(status))||length(status)!=1L||is.na(status)))
      .mp_abort("E_PPC_LAYOUT",paste("saved",n,"status is ambiguous"))
    if(length(status)&&status%in%c("proven_improper","proven_non_normalizable"))
      .mp_abort("E_PPC_POSTERIOR","a proved improper or non-normalizable target is not a posterior predictive source")
  }
  if(portable)attr(spec,"class")<-"bmeas_portable_context"
  spec
}

.bh_ppc_plan <- function(fit,draws,budget,level) {
  spec<-.bh_ppc_context(fit,level);draws<-.bh_ppc_plain_integer(draws,"draws",2L)
  limits<-.bh_ppc_budget_resolve(budget);cap<-limits$values
  if(!"posterior"%in%names(cap)&&cap$max_draws>20L)
    .mp_abort("E_DIAGNOSTIC_BUDGET","legacy acquisition retains at most20 replications; supply an explicit posterior streaming policy for larger requests")
  if(draws>cap$max_draws) .mp_abort("E_DIAGNOSTIC_BUDGET","requested replications exceed the explicit predictive budget")
  .bh_ppc_preflight_work(unclass(spec),draws,cap,level)
  forward<-.bh_ppc_forward_plan(spec,level,cap,saved_joint=.bh_pf_joint_view(fit))
  O<-.bh_count_sum(vapply(forward$blocks,function(x)nrow(x$rows),integer(1)))
  normals<-.bh_count_product(draws,forward$latent_values_per_draw)
  if(O>cap$max_observations||.bh_count_product(draws,O,forward$response_cells_per_row)>cap$max_simulated_values||normals>cap$max_latent_values)
    .mp_abort("E_DIAGNOSTIC_BUDGET","full-design response or latent simulation exceeds the predictive budget")
  columns<-length(forward$required_variables)
  streaming<-"posterior"%in%names(cap)
  posterior_policy<-if(streaming).bh_posterior_policy(cap$posterior,forward$required_variables)else NULL
  source_row_ceiling<-if(streaming)posterior_policy$limits$max_source_rows else
    if(columns)min(cap$max_posterior_rows,floor(cap$max_draw_values/columns))else cap$max_posterior_rows
  if(draws>source_row_ceiling)
    .mp_abort("E_DIAGNOSTIC_BUDGET",if(streaming)
      "the source-row allowance cannot contain the requested distinct posterior states"else
      "the legacy all-row extraction budget cannot contain the requested distinct posterior states; supply posterior=list(reader=\"streaming\",...) with explicit finite allowances")
  stat_cap<-cap
  stat_cap$max_work_values<-cap$max_work_values-.bh_count_sum(unlist(forward$allocation_accounting))
  statistics<-.bh_ppc_statistics_plan(spec,draws,stat_cap,level)
  forward$allocation_accounting$statistic_slots<-statistics$allocation_slots
  .bh_ppc_work_admit(unlist(forward$allocation_accounting),cap)
  provenance<-.bh_report_slot(fit,"provenance")
  paths<-.bh_report_slot(provenance,"output_files")
  path_shape<-is.character(paths)&&!is.object(paths)&&is.null(dim(paths))&&length(paths)>0L&&!anyNA(paths)&&!anyDuplicated(paths)&&all(nzchar(paths))
  expected<-if(streaming).bh_posterior_expected_chains(fit)else NULL
  buffers<-if(streaming).bh_ppc_stream_buffers(forward,statistics,draws)else NULL
  if(streaming) {
    if(path_shape&&length(paths)>posterior_policy$limits$max_chains)
      .mp_abort("E_DIAGNOSTIC_BUDGET","every recorded source chain must fit the explicit streaming chain allowance")
    if(path_shape&&!is.null(expected$value)&&length(paths)!=expected$value)
      .mp_abort("E_PPC_SOURCE_IDENTITY","recorded file count differs from the retained expected chain count")
    chain_count<-if(path_shape)length(paths)else expected$value
    if(!is.null(chain_count)&&draws<chain_count)
      .mp_abort("E_PPC_SOURCE_SELECTION","balanced PPC requires at least one selected state from every recorded chain")
    known_slots<-.bh_count_sum(c(.bh_count_product(draws,columns),.bh_count_product(2,draws),
      unname(buffers$fixed),.bh_count_product(draws,.bh_count_sum(buffers$per_selected_row))),
      "known predictive numeric slots")
    if(known_slots>posterior_policy$limits$max_materialized_values)
      .mp_abort("E_DIAGNOSTIC_BUDGET","known predictive/projection numeric buffers exceed the streaming allowance")
  }
  source_record<-if(streaming)list(
    availability=if(path_shape)"recorded_paths_uninspected"else"recorded_paths_unavailable",
    required_reader="package-owned streaming CmdStan2.40 parser after bounded seven-pass acquisition",
    observed_rows=NA_real_,maximum_source_rows=source_row_ceiling,
    recorded_path_count=if(path_shape)length(paths)else NA_real_,expected_chains=expected,
    policy=posterior_policy$limits,policy_origin=posterior_policy$origins,
    numeric_buffer_known_lower_bound=known_slots,numeric_buffer_scope=
      "Declared projected keys/values and consumer buffers; source header/chain/parser buffers are added before acquisition allocation, with unknown R overhead/RSS",
    consumer_buffers=buffers,selection=list(method="balanced",draws=draws),
    validation_scope="Every retained row is validated; only selected joint states are materialized",
    original_fit_authentication="not inferred from a future read-time receipt")else list(
    availability=if(path_shape)"recorded_paths_uninspected"else"recorded_paths_unavailable",
    required_reader="fresh CmdStanR CSV reader after explicit bounded acquisition",
    observed_rows=NA_integer_,maximum_admissible_source_rows=as.integer(source_row_ceiling),
    maximum_materialized_values=source_row_ceiling*columns,
    original_fit_authentication="not inferred from a future read-time receipt")
  if(identical(.bh_pf_class(spec),"bmeas_portable_context")&&.bh_m3cs_predictive_spec(spec))
    source_record$reader_execution<-.bh_m3cs_saved_capability(spec)
  if(.bh_pf_product_view(fit))source_record$reader_execution<-.bh_pf_product_reader_capability()
  structure(list(version=if(.bh_m3cs_predictive_spec(spec))"bmeas_m3cs_ppc_plan_v3"else"bmeas_basic_ppc_plan_v3",level=level,law=forward$law,
    replications=draws,observations_per_rep=O,simulated_values=.bh_count_product(draws,O,forward$response_cells_per_row),
    latent_values=normals,predictor_values_per_rep=forward$predictor_values_per_draw,
    scatter_values=.bh_count_product(draws,forward$predictor_values_per_draw),
    geometry=forward$geometry,roster=forward$roster,active_target_index=forward$active_target_index,
    required_variables=forward$required_variables,required_columns=columns,
    budget=cap,budget_origin=limits$origin,
    allocation_accounting=forward$allocation_accounting,
    source=source_record,
    statistics=statistics$statistics,statistic_counts=statistics$counts,
    model_identity=list(specification_fingerprint=.bh_pf_get(spec,"fingerprint"),
      saved_fit_model_fingerprint=.bh_report_slot(provenance,"model_fingerprint"),
      data_identity=.bh_report_slot(spec,"provenance")),
    scientific_scope=if(level=="conditional")"Conditional response replication; no population measurement-structure assessment"else
      "Population response and paired-person discrepancy screening under the declared iid or same-N group law",
    cost_scope=if(streaming)paste("No posterior files or backend metadata inspected. Explicit source/read/temp/record and numeric-slot quotas are separate from predictive work;",
      "actual rows, header work and seven-pass costs remain unobserved, and no RSS/wall-time guarantee is made.")else
      "No posterior files or backend metadata inspected. Bounds cover the complete source materialization and simulation; no fixed wall-time or OS-level memory guarantee.",
    assessment_status="not_run",screening_only=TRUE),class="bmeas_ppc_plan")
}

print.bmeas_ppc_plan <- function(x,...) {
  original<-x
  x<-.bh_ppc_plain_list(x,"saved predictive plan",allow_class=TRUE)
  for(name in c("version","level","law"))
    .bh_ppc_plain_value(x[[name]],paste("printed predictive",name),"character",TRUE)
  if(!x$version%in%c("bmeas_basic_ppc_plan_v3","bmeas_m3cs_ppc_plan_v3","bmeas_basic_ppc_plan_v1","bmeas_basic_ppc_plan_v2","bmeas_m3cs_ppc_plan_v1","bmeas_m3cs_ppc_plan_v2")||
    !x$level%in%c("population","conditional")||
    !x$law%in%c("iid_common_plus_parameter_residual","same_N_sample_centered_task_cholesky","conditional_response"))
    .mp_abort("E_PPC_LAYOUT","unsupported saved predictive-plan display schema")
  for(name in c("replications","observations_per_rep","required_columns"))
    .bh_count(x[[name]],paste("printed predictive",name),code="E_PPC_LAYOUT")
  x$source<-.bh_ppc_plain_list(x$source,"saved predictive source plan")
  x$statistic_counts<-.bh_ppc_plain_list(x$statistic_counts,"saved predictive statistic counts")
  .bh_count(x$statistic_counts$statistics,"printed statistic count",code="E_PPC_LAYOUT")
  row_field<-if(("posterior"%in%names(x$budget)))"maximum_source_rows"else"maximum_admissible_source_rows"
  .bh_count(x$source[[row_field]],"printed source-row allowance",code="E_PPC_LAYOUT")
  cat("bmeas predictive plan (not run)\n")
  cat("  Level:",x$level,"; law:",x$law,"\n")
  cat("  Full-design replications:",x$replications,"; response rows each:",x$observations_per_rep,"\n")
  if(("posterior"%in%names(x$budget))) {
    cat("  Required joint-draw columns:",x$required_columns,"; streaming source-row allowance:",x$source$maximum_source_rows,"\n")
    cat("  Whole-source validation; balanced selected states only. Read/temp/numeric allowances are distinct; actual source remains uninspected.\n")
  }else cat("  Required joint-draw columns:",x$required_columns,"; admissible total source rows:",x$source$maximum_admissible_source_rows,"\n")
  cat("  Discrepancy statistics:",x$statistic_counts$statistics,"; source files remain uninspected\n")
  cat("  Descriptive screening only; assess_measurement_fit() is the explicit computation step\n")
  invisible(original)
}

.bh_ppc_initial_status <- function(spec) {
  supported<-tryCatch({.bh_ppc_forward_plan(spec,"population");TRUE},error=function(e)e)
  if(isTRUE(supported)||(inherits(supported,"measurement_error")&&supported$code=="E_DIAGNOSTIC_BUDGET"))list(status="not_run",reason="Predictive checking requires explicit level, draws, seed and diagnostic_budget; M3-CS additionally requires an explicitly compiled simulator; no simulation ran")else
    list(status="unsupported",reason=conditionMessage(supported))
}

.bh_ppc_rng_preflight <- function() {
  has_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(has_seed&&(bindingIsActive(".Random.seed",.GlobalEnv)||bindingIsLocked(".Random.seed",.GlobalEnv)))
    .mp_abort("E_RNG_STATE","active or locked .Random.seed bindings cannot be safely preserved")
  if(!has_seed&&environmentIsLocked(.GlobalEnv))
    .mp_abort("E_RNG_STATE","a locked global environment cannot receive and remove a temporary RNG state")
  if(has_seed) {
    state<-get(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
    if(typeof(state)!="integer"||is.object(state)||!is.null(attributes(state))||length(state)<2L||anyNA(state))
      .mp_abort("E_RNG_STATE","the caller RNG state is not a plain recorded integer state")
    # Querying RNGkind() can replace a malformed .Random.seed. Reject its
    # documented kind/normal/sample header and state shape before that query.
    code<-as.double(state[1L]);rng<-code%%100;normal<-(code%/%100)%%100;sample<-code%/%10000
    lengths<-c(`0`=4L,`1`=3L,`2`=3L,`3`=626L,`4`=102L,`6`=102L,`7`=7L)
    if(code<0||!rng%in%c(0:4,6:7)||!normal%in%0:5||!sample%in%0:1||
        length(state)!=lengths[[as.character(rng)]])
      .mp_abort("E_RNG_STATE","the caller RNG kind header or state length is unsupported or malformed")
    on.exit(suspendInterrupts(assign(".Random.seed",state,envir=.GlobalEnv)),add=TRUE)
  }
  tryCatch(withCallingHandlers(.mp_prediction_rng_preflight(),warning=function(w)
    .mp_abort("E_RNG_STATE","the caller RNG state cannot be queried without a repair warning")),
    error=function(e){if(inherits(e,"measurement_error"))stop(e);.mp_abort("E_RNG_STATE",conditionMessage(e))})
  invisible(TRUE)
}

.bh_ppc_with_rng <- function(seed,action) {
  .bh_ppc_rng_preflight();seed<-.bh_ppc_plain_integer(seed,"seed")
  had_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  old_seed<-if(had_seed)get(".Random.seed",envir=.GlobalEnv,inherits=FALSE)else NULL
  old_kind<-RNGkind()
  on.exit(suspendInterrupts({suppressWarnings(do.call(RNGkind,as.list(old_kind)));if(had_seed)assign(".Random.seed",old_seed,envir=.GlobalEnv)else
    if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)}),add=TRUE)
  RNGkind("Mersenne-Twister","Inversion","Rejection");set.seed(seed)
  action()
}

.bh_ppc_current_status <- function(result,level) {
  cover<-result$coverage
  predictive<-if(cover$planned==0L||cover$evaluable==0L)"not_assessed"else
    if(cover$status=="complete")"computed"else"warning"
  population<-result$measurement_structure
  measurement<-if(level=="conditional"||population$planned==0L||population$evaluable==0L)"not_assessed"else
    if(population$status=="complete")"computed"else"warning"
  list(predictive_fit=list(status=predictive,level=level,
      reason="Descriptive response discrepancies only; computation and interval departures are not a good-fit verdict",coverage=cover),
    measurement_structure_fit=list(status=measurement,level=level,
      reason=if(level=="conditional")"Conditional replication does not assess population measurement structure"else
        "Paired-person response covariance discrepancies under the declared population law; not latent correlations or a global fit verdict",
      coverage=population))
}

.bh_ppc_assess <- function(fit,checks,level,draws,seed,budget,simulator=NULL,context_validation=NULL,acquisition_paths=NULL,archive_hashes=NULL) {
  portable<-.bh_pf_active_route(fit,context_validation,"assess_measurement_fit")
  cognitive<-.bh_m3cs_predictive_spec(.bh_report_slot(fit,"spec"))
  if(!identical(checks,"ppc")||(!cognitive&&!is.null(simulator)))
    .mp_abort("E_CHECK_UNSUPPORTED","main-API basic-family assessment uses checks='ppc' and canonical R kernels; no external simulator or LOO is invoked")
  plan<-.bh_ppc_plan(fit,draws,budget,level)
  retained_plan<-plan
  if(portable)plan<-unclass(plan)
  fit_attributes<-attributes(fit);fit_class<-attr(fit,"class",exact=TRUE)
  fit<-.bh_ppc_plain_list(fit,"fit",allow_class=TRUE)
  spec<-.bh_ppc_context(structure(fit,class=fit_class),level)
  seed<-.bh_ppc_plain_integer(seed,"seed");.bh_ppc_rng_preflight()
  # Integrity checking is explicit computation here, never part of ppc_plan or
  # cached report reads. This does not migrate/reseal a historical context.
  if(!portable) .bh_ppc_validate_saved_spec(fit$spec)
  forward<-.bh_ppc_forward_plan(spec,level,plan$budget,saved_joint=portable&&identical(context_validation$context_profile,"bmeas_joint_m3cs_context_v1"))
  statistic_plan<-.bh_ppc_statistics_plan(spec,plan$replications,plan$budget,level)
  if(cognitive) {
    if(portable).bh_m3cs_simulator_check(fit$spec,simulator,plan$budget,context_validation)
    else .bh_m3cs_simulator_check(fit$spec,simulator,plan$budget)
  }
  completed<-0L;phase<-"source_acquisition";receipt<-NULL
  metadata<-list(version=if(cognitive)"bmeas_m3cs_ppc_result_v2"else"bmeas_basic_ppc_result_v2",level=level,law=plan$law,
    seed=seed,rng_kind=c("Mersenne-Twister","Inversion","Rejection"),R_version=R.version.string,
    requested_replications=plan$replications,completed_replications=0L,
    plan=retained_plan,source_qualification=.bh_report_slot(.bh_report_slot(fit,"provenance"),"kind"),
    upstream=list(computation=.bh_report_slot(fit$cache,"computation"),
      inference=.bh_report_slot(fit$cache,"inference"),identification=.bh_report_slot(fit$cache,"identification"),
      posterior_propriety=.bh_report_slot(fit$cache,"posterior_propriety"),
      posterior_normalizability=.bh_report_slot(fit$cache,"posterior_normalizability")),
    screening_only=TRUE,source_origin_scope="Current-input receipt; no original fit-time posterior hash is inferred",
    interpretation="Tiny potentially dependent posterior-state selections give coarse descriptive replication intervals, not calibrated tests")
  if(portable&&.bh_pf_product_view(structure(fit,class=fit_class)))
    metadata$reader_execution<-.bh_pf_product_reader_capability()
  if(cognitive) {
    if(portable)metadata$reader_execution<-.bh_m3cs_saved_capability(fit$spec)
    metadata$simulator<-unclass(simulator)[c("version","code_hash","resource_hash","executable_hash","toolchain")]
    if(portable) {
      current_resource<-.bh_pf_get(simulator,"resource_file")
      if(is.null(current_resource))current_resource<-fit$spec$models[[1L]]$model$adapter$resources[[1L]]$file
      metadata$simulator$current_resource_binding<-list(file=current_resource,
        sha256=.bh_pf_get(simulator,"resource_hash"),scope="current_execution_only_historical_paths_unchanged")
    }
    metadata$simulator$execution_contract<-list(algorithm="fixed_param",warmup=0L,draws_per_replication=1L,parameters=0L,threads=1L)
    metadata$stan_seeds<-integer(plan$replications)
  }
  answer<-tryCatch(.bh_ppc_with_rng(seed,function() {
    paths<-if(is.null(acquisition_paths)).bh_report_slot(.bh_report_slot(fit,"provenance"),"output_files")else acquisition_paths
    if(!plan$observations_per_rep) .mp_abort("E_PPC_NO_OBSERVATIONS","no consumed responses are available for an empirical predictive assessment")
    streaming<-"posterior"%in%names(plan$budget)
    acquired<-if(streaming).bh_posterior_ingest_csv(paths,forward$required_variables,
      plan$budget$posterior,states=list(method="balanced",draws=plan$replications),
      expected_model="scalar_components_model",expected_chains=plan$source$expected_chains$value,
      consumer_buffers=plan$source$consumer_buffers,archive_hashes=archive_hashes)else
      .bh_ppc_ingest_csv(paths,forward$required_variables,plan$budget,plan$replications,
        expected_model="scalar_components_model")
    if(streaming)acquired$receipt$expected_chain_count_context<-plan$source$expected_chains
    receipt<<-.bh_pf_context_attach(acquired$receipt,context_validation)
    metadata$draw_keys<-acquired$draw_keys
    metadata$source_receipt<-receipt
    metadata$source_receipt$bound_model_identity<-plan$model_identity
    metadata$source_receipt$identity_scope<-"Association with supplied saved identities; executable/data origin is not independently authenticated"
    if(portable) {
      metadata$source_receipt$source_qualification<-metadata$source_qualification
      metadata$source_receipt$portable_bound_identity<-context_validation$identity
    } else {
      metadata$source_receipt$saved_source_code_hash<-.mp_hash(fit$code)
      metadata$source_receipt$source_qualification<-metadata$source_qualification
      metadata$source_receipt$predictive_map_hash<-.mp_hash(list(roster=forward$roster,
        targets=spec$tables$targets,operands=spec$tables$observation_operands,
        statistics=statistic_plan$statistics))
    }
    phase<<-"observed_statistics"
    observed<-.bh_ppc_statistics(spec,statistic_plan)
    reps<-matrix(NA_real_,plan$replications,nrow(observed),
      dimnames=list(paste0("replication_",seq_len(plan$replications)),observed$statistic_id))
    # Reader/package initialization cannot consume the predictive replay stream.
    RNGkind("Mersenne-Twister","Inversion","Rejection");set.seed(seed)
    phase<<-"forward_simulation"
    for(r in seq_len(plan$replications)) {
      draw<-as.numeric(acquired$draws[r,,drop=FALSE]);names(draw)<-forward$required_variables
      if(cognitive)metadata$stan_seeds[r]<-sample.int(.Machine$integer.max,1L)
      responses<-if(cognitive) {
        if(portable).bh_m3cs_simulate(fit$spec,forward,draw,simulator,metadata$stan_seeds[r],plan$budget,context_validation)
        else .bh_m3cs_simulate(fit$spec,forward,draw,simulator,metadata$stan_seeds[r],plan$budget)
      }else .bh_ppc_simulate_responses(forward,draw)
      replicated<-.bh_ppc_statistics(spec,statistic_plan,responses)
      if(!identical(replicated$statistic_id,observed$statistic_id))
        .mp_abort("E_PREDICTION_SUPPORT","replicate statistics do not match the planned identities")
      reps[r,]<-replicated$estimate;completed<<-r
    }
    phase<<-"aggregate_statistics"
    result<-.bh_ppc_aggregate(observed,reps)
    metadata$completed_replications<-completed
    metadata$replicate_statistics_status<-"retained"
    metadata$replicate_statistics_layout<-"rows=replications; columns=table statistic_id"
    metadata$quantile_type<-7L
    metadata$actual_work<-if(streaming)list(source_rows=receipt$source_rows,
      selected_rows=receipt$selected_rows,selected_required_values=receipt$selected_required_values,
      required_numeric_conversions=receipt$required_numeric_conversions,
      lexical_fields=receipt$lexical_fields,alias_comparisons=receipt$alias_comparisons,
      numeric_buffer_schedule=receipt$buffer_schedule,
      read_bytes_measured=receipt$read_bytes_measured,hash_bytes_reserved=receipt$hash_bytes_reserved,
      read_bytes_charged=receipt$read_bytes_charged,
      simulated_values=completed*plan$observations_per_rep*forward$response_cells_per_row,latent_values=completed*forward$latent_values_per_draw,
      statistics=nrow(observed),aggregate_values=length(reps))else
      list(source_rows=receipt$actual_posterior_rows,all_row_shape=receipt$all_row_shape,
        materialized_required_values=receipt$materialized_required_values,
        simulated_values=completed*plan$observations_per_rep*forward$response_cells_per_row,latent_values=completed*forward$latent_values_per_draw,
        statistics=nrow(observed),aggregate_values=length(reps))
    if(streaming)metadata$posterior_selection<-receipt$selection
    result$metadata<-metadata;result
  }),error=function(e)e)
  out<-fit
  if(inherits(answer,"error")) {
    partial<-if(!is.null(answer$receipt))answer$receipt else receipt
    metadata$source_receipt<-.bh_pf_context_attach(partial,context_validation);metadata$completed_replications<-completed
    metadata$failure<-list(phase=phase,code=if(is.null(answer$code))"E_PPC_ATTEMPT"else answer$code,
      reason=conditionMessage(answer))
    metadata$replicate_statistics_status<-"unavailable_failed_attempt"
    out$cache$ppc<-list(table=NULL,replicate_statistics=NULL,metadata=metadata)
    out$cache$predictive_fit<-list(status="failed",level=level,law=plan$law,
      reason=conditionMessage(answer),phase=phase)
    out$cache$measurement_structure_fit<-list(status="not_assessed",level=level,
      reason="This predictive attempt failed; no current measurement-structure comparison is retained")
  }else {
    statuses<-.bh_ppc_current_status(answer,level)
    out$cache$ppc<-answer
    out$cache$predictive_fit<-statuses$predictive_fit;out$cache$predictive_fit$law<-plan$law
    out$cache$measurement_structure_fit<-statuses$measurement_structure_fit
  }
  fit_attributes$names<-names(out);attributes(out)<-fit_attributes
  out
}
