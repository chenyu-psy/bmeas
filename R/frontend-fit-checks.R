# Deterministic projections of saved fit evidence. No model or backend access.
.bh_report_slot <- function(x, name) {
  if(is.null(x))return(NULL)
  if(!is.list(x)) .mp_abort("E_FIT_REPORT", "saved report container is not a plain list")
  .subset2(unclass(x),name)
}
.bh_report_record <- function(x,name) {
  value<-.bh_report_slot(x,name)
  if(is.null(value))list(availability="unavailable_in_cache",record=NULL,
    reason=paste(name,"was not retained; whether it was requested or computed is unknown"))else
    list(availability="retained",record=value,reason="Saved evidence is returned without reassessment")
}
.bh_report_table <- function(x,name) {
  if(is.null(x))return(NULL)
  if(!identical(class(x),"data.frame")||anyDuplicated(names(x))) .mp_abort("E_FIT_REPORT",paste(name,"is not an unambiguous cached data frame"))
  x
}
.bh_report_column <- function(x,name) if(is.null(x))NULL else .subset2(unclass(x),name)
.bh_report_named <- function(x,names) {names(x)<-names;x}
.bh_report_row_keys <- function(tab,at) {
  get<-function(name) {
    x<-.bh_report_column(tab,name)
    if(is.null(x))return(rep(NA_character_,length(at)))
    if(!is.character(x)||is.object(x)||!is.null(dim(x))||length(x)!=.row_names_info(tab,2L))return(rep(NA_character_,length(at)))
    x[at]
  }
  data.frame(row_index=as.integer(at),variable=get("variable"),parameter_key=get("parameter_key"),
    parameter_role=get("parameter_role"),stringsAsFactors=FALSE)
}
.bh_report_coverage <- function(x,valid,rows=length(x)) {
  list(rows=as.integer(rows),finite=sum(is.finite(x)),missing=sum(is.na(x)&!is.nan(x)),nan=sum(is.nan(x)),
    positive_infinity=sum(x==Inf,na.rm=TRUE),negative_infinity=sum(x== -Inf,na.rm=TRUE),
    invalid=sum(!is.na(x)&!valid),usable=sum(valid),scope="cached rows; not independent scientific primitives")
}
.bh_report_extreme <- function(tab,field) {
  n<-if(is.null(tab))0L else .row_names_info(tab,2L)
  raw<-.bh_report_column(tab,field);finite_only<-field!="rhat"
  statistic<-if(finite_only)paste0("minimum_finite_",field)else"maximum_recorded_rhat"
  base<-list(availability=if(is.null(raw))"unavailable_in_cache"else"retained",statistic=statistic,
    finite_only=finite_only,value=NA_real_,status="unavailable",rows=.bh_report_row_keys(tab,integer()),
    threshold=list(status="not_recorded",reason="No new diagnostic threshold is applied"),
    row_key_availability=.bh_report_named(lapply(c("variable","parameter_key","parameter_role"),function(n){x<-.bh_report_column(tab,n);if(is.null(x))"unavailable_in_cache"else if(is.character(x)&&!is.object(x)&&is.null(dim(x))&&length(x)==.row_names_info(tab,2L))"retained"else"unavailable_invalid_metadata"}),c("variable","parameter_key","parameter_role")))
  if(is.null(raw)) {
    base$coverage<-list(rows=as.integer(n),finite=NA_integer_,missing=NA_integer_,nan=NA_integer_,
      positive_infinity=NA_integer_,negative_infinity=NA_integer_,invalid=NA_integer_,usable=0L,
      scope="metric column absent; cached row count is not a primitive count")
    base$reason<-paste(field,"was not retained")
    return(base)
  }
  if(!typeof(raw)%in%c("integer","double")||is.object(raw)||!is.null(dim(raw))||length(raw)!=n) .mp_abort("E_FIT_REPORT",paste(field,"must be one numeric cached value per row"))
  valid<-if(field=="rhat")!is.na(raw)&raw>0 else is.finite(raw)&raw>=0
  base$coverage<-.bh_report_coverage(raw,valid,n)
  if(any(valid)) {
    base$value<-if(field=="rhat")max(raw[valid])else min(raw[valid])
    base$rows<-.bh_report_row_keys(tab,which(valid&raw==base$value))
  }
  base$status<-if(!any(valid))"no_usable_values"else if(any(is.infinite(raw)))"nonfinite_recorded"else if(any(!is.na(raw)&!valid))"invalid_recorded"else if(any(is.na(raw)))"partial_values"else"available"
  base$reason<-if(!n)"The cached table has no monitored rows"else if(!any(valid))"No usable recorded metric; this is not a favorable diagnostic"else
    if(finite_only)"Finite nonnegative cached values determine this minimum; other entries remain visible in coverage and the unchanged parameter table"else
      "Positive cached Rhat values, including +Inf, determine this maximum; no value is rewritten from configured chain counts"
  base
}
.bh_report_chain_metric <- function(hmc,field,identity=NULL) {
  x<-.bh_report_slot(hmc,field)
  entries<-data.frame(stored_row_index=integer(),chain_id=character(),value=numeric(),value_status=character(),stringsAsFactors=FALSE)
  result<-list(availability=if(is.null(x))"unavailable_in_cache"else"retained",entries=entries,
    observed_total=NA_real_,minimum=list(value=NA_real_,entries=entries),complete_for_retained_entries=FALSE,
    chain_identity="unavailable_in_cache",threshold=list(status="not_recorded"))
  if(is.null(x)) {
    result$coverage<-list(rows=0L,usable=0L,scope="metric absent; no complete-chain count inferred")
    result$reason<-paste(field,"was not retained")
    return(result)
  }
  if(!typeof(x)%in%c("integer","double")||is.object(x)||!is.null(dim(x))) .mp_abort("E_FIT_REPORT",paste("cached HMC",field,"must be a numeric vector"))
  valid<-is.finite(x)&x>=0
  if(field!="ebfmi")valid<-valid&x==floor(x)
  ids<-rep(NA_character_,length(x))
  if(!is.null(identity)&&length(identity)==length(x)&&!anyNA(identity)&&!anyDuplicated(identity)) {
    ids<-base::as.character.default(identity);result$chain_identity<-"retained_explicit_ids"
  }
  state<-ifelse(is.nan(x),"NaN",ifelse(is.na(x),"missing",ifelse(is.infinite(x),"nonfinite",ifelse(valid,"recorded","invalid"))))
  entries<-data.frame(stored_row_index=seq_along(x),chain_id=ids,value=x,value_status=state,stringsAsFactors=FALSE)
  result$entries<-entries;result$coverage<-.bh_report_coverage(x,valid)
  result$coverage$scope<-"entries of this metric only; unequal vectors are not recycled or treated as complete configured chains"
  result$complete_for_retained_entries<-length(x)>0L&&all(valid)
  if(any(valid)) {
    if(field=="ebfmi") {
      value<-min(x[valid]);result$minimum<-list(value=value,entries=entries[which(valid&x==value),,drop=FALSE])
    }else result$observed_total<-sum(x[valid])
  }
  result$reason<-"Only valid retained entries contribute; missing, nonfinite and invalid entries are explicit. Stored row indices are not invented chain IDs."
  result
}
.bh_report_chains <- function(hmc) {
  if(!is.null(hmc)&&!is.list(hmc)) .mp_abort("E_FIT_REPORT","cached HMC diagnostics are not a list or data frame")
  identity<-.bh_report_slot(hmc,"chain_id")
  if(is.null(identity))identity<-.bh_report_slot(hmc,"chain")
  if(!is.null(identity)&&(!is.atomic(identity)||!is.null(dim(identity))||!is.null(attr(identity,"class",exact=TRUE))))identity<-NULL
  list(availability=if(is.null(hmc))"unavailable_in_cache"else"retained",record=hmc,
    metrics=.bh_report_named(lapply(c("num_divergent","num_max_treedepth","ebfmi"),function(field).bh_report_chain_metric(hmc,field,identity)),c("num_divergent","num_max_treedepth","ebfmi")),
    scope="Per-metric retained vectors; no recycling or inference of observed counts from configured runs")
}
.bh_fit_checks <- function(fit) {
  if(!inherits(fit,"bhmm_fit")&&!.bh_pf_is_view(fit)) .mp_abort("E_FIT",if(inherits(fit,"bhmm_spec"))"a prepared specification is not a fit; use check_model()"else"expected a bmeas fit")
  .bh_project_fit_checks(fit)
}
.bh_project_fit_checks <- function(fit) {
  cache<-.bh_report_slot(fit,"cache")
  if(!is.null(cache)&&!is.list(cache)) .mp_abort("E_FIT_REPORT","saved fit cache is not a plain list")
  parameters<-.bh_report_table(.bh_report_slot(cache,"parameter_summary"),"parameter_summary")
  fields<-c("computation","inference","predictive_fit","identification","posterior_normalizability","posterior_propriety",
    "admissibility","optional_reports","prior_sensitivity","measurement_structure_fit","conventional_fit","numerical_evaluations","initialization","gaussian_diagnostics","execution_profile","backend_qualification")
  domains<-.bh_report_named(lapply(fields,function(n).bh_report_record(cache,n)),fields)
  cache_names<-names(unclass(cache))
  if(!is.null(cache_names)&&(!is.character(cache_names)||is.object(cache_names)||
    !is.null(dim(cache_names))||anyNA(cache_names)||!all(validEnc(cache_names))||sum(cache_names=="numerical_evaluations")>1L))
    domains$numerical_evaluations<-list(availability="unavailable_invalid_metadata",record=NULL,
      reason="Optional numerical-evidence cache field has ambiguous names; no record was selected")
  domains$numerical_evaluations$interpretation<-.bh_report_numerical(domains$numerical_evaluations)
  extrema<-.bh_report_named(lapply(c("rhat","ess_bulk","ess_tail"),function(n).bh_report_extreme(parameters,n)),c("rhat","ess_bulk","ess_tail"))
  chains<-.bh_report_chains(.bh_report_slot(cache,"hmc"))
  thresholds<-.bh_report_slot(cache,"diagnostic_thresholds")
  if(is.list(thresholds)) {
    for(n in names(extrema))if(!is.null(.bh_report_slot(thresholds,n)))extrema[[n]]$threshold<-list(status="recorded_not_reapplied",record=.bh_report_slot(thresholds,n))
    for(n in names(chains$metrics))if(!is.null(.bh_report_slot(thresholds,n)))chains$metrics[[n]]$threshold<-list(status="recorded_not_reapplied",record=.bh_report_slot(thresholds,n))
  }
  columns<-if(is.null(parameters))character()else names(parameters)
  interval_columns<-columns[grepl("^(q[0-9]+([.][0-9]+)?|[0-9]+([.][0-9]+)?%)$",columns)]
  metadata<-.bh_report_slot(cache,"report_metadata")
  metadata_valid<-is.null(metadata)||(typeof(metadata)=="list"&&!is.object(metadata)&&is.null(dim(metadata))&&!anyDuplicated(names(metadata)))
  invalid_metadata<-function(reason)list(availability="unavailable_invalid_metadata",record=NULL,reason=reason)
  summary_provenance<-if(metadata_valid).bh_report_record(metadata,"summary_method")else
    invalid_metadata("Optional report_metadata is not an unambiguous plain list; cached metrics remain available")
  interval_provenance<-if(metadata_valid).bh_report_record(metadata,"interval_columns")else
    invalid_metadata("Optional report_metadata is not an unambiguous plain list; only actual cached quantile-column names are exposed")
  saved_intervals<-if(metadata_valid).bh_report_slot(metadata,"interval_columns")else NULL
  if(!is.null(saved_intervals)) {
    if(!is.character(saved_intervals)||is.object(saved_intervals)||!is.null(dim(saved_intervals))||anyNA(saved_intervals)||anyDuplicated(saved_intervals)||any(!saved_intervals%in%columns))
      interval_provenance<-invalid_metadata("Optional interval-column metadata does not match the cached table; only actual cached quantile-column names are exposed")else
      interval_columns<-saved_intervals
  }
  predictive<-list(assessment=domains$predictive_fit,results=.bh_report_record(cache,"ppc"),
    scope="Saved predictive evidence only; this report runs no simulation or empirical fit assessment")
  if(!is.null(.bh_report_slot(cache,"ppc")))
    predictive$cached<-.bh_report_ppc(.bh_report_slot(cache,"ppc"),domains$predictive_fit)
  report<-structure(list(version="bmeas_cached_fit_checks_v1",context=.bh_fit_report_context(.bh_optional_reporting_context(fit)),
    sampling=list(parameters=parameters,parameters_availability=if(is.null(parameters))"unavailable_in_cache"else"retained",
      extrema=extrema,
      chains=chains,
      summary_provenance=summary_provenance,
      thresholds=.bh_report_record(cache,"diagnostic_thresholds"),
      interval_columns=interval_columns,
      interval_provenance=interval_provenance,
      mcse_column=if(!is.null(parameters)&&"mcse_mean"%in%names(parameters))"mcse_mean"else NA_character_,
      interval_scope="Cached named quantiles only; historical estimator details are unavailable unless separately recorded",
      mcse_scope="mcse_mean is the error of the posterior mean, not of quantiles or interval endpoints"),
    domains=domains,predictive=predictive),class="bmeas_fit_checks")
  .bh_project_uncomputed_reporting(report,cache,fit)
}
.bh_report_display <- function(value) if(typeof(value)%in%c("integer","double","character","logical")&&!is.object(value)&&length(value)==1L&&!is.na(value))format(value,digits=5,trim=TRUE)else"unavailable"
.bh_report_domain_status <- function(x) {
  if(!identical(x$availability,"retained"))return("unavailable in cache")
  if(!is.list(x$record))return("retained; record status unavailable")
  status<-.bh_report_slot(x$record,"status")
  if(is.character(status)&&length(status)==1L&&!is.na(status))status else"retained; status unavailable"
}
.bh_print_fit_checks <- function(x,context=TRUE,header=TRUE) {
  if(!inherits(x,"bmeas_fit_checks")||!identical(.bh_report_slot(x,"version"),"bmeas_cached_fit_checks_v1")) .mp_abort("E_FIT_REPORT","unsupported cached report schema; use the original fit to project its saved cache")
  if(inherits(x,"bmeas_portable_fit_checks"))x<-unclass(x)
  if(header)cat("bmeas cached fit checks | saved evidence only\n")
  reporting_node<-.bh_optional_saved_record(x$sampling,"posterior_reporting",.bh_valid_posterior_reporting)
  reporting<-if(!is.null(reporting_node)&&identical(reporting_node$availability,"retained"))reporting_node$record else NULL
  reporting_domain<-.bh_saved_field(x$domains,"posterior_reporting")
  receipt_node<-.bh_saved_field(.bh_saved_field(x$context,"run"),"execution_receipt")
  for(node in list(posterior_reporting=reporting_domain,execution_receipt=receipt_node,
    resource_outcome=.bh_saved_field(.bh_saved_field(x$context,"run"),"resource_outcome"))) {
    if(identical(.bh_saved_field(node,"availability"),"unavailable_invalid_metadata")) {
      reason<-.bh_saved_field(node,"reason")
      cat("Optional execution/reporting metadata: unavailable_invalid_metadata |",
        if(.bh_saved_string(reason))reason else"Malformed optional evidence; retained metrics remain available","\n")
    }
  }
  if(!is.null(reporting)) {
    receipt<-.bh_optional_saved_record(receipt_node,"record",.bh_valid_execution_receipt)
    sampling_status<-if(!is.null(receipt)&&identical(receipt$availability,"retained"))receipt$record$sampling$status else NULL
    cat("Sampling completion:",.bh_report_display(sampling_status),"| posterior diagnostics:",reporting$status,"\n")
    cat(reporting$reason,"; numerical-message, parameter, HMC and covariance quantities remain unavailable.\n")
  }
  refreshed<-.bh_print_diagnostic_reporting(x)
  if(context) {
    b<-x$context$blocks
    if(is.data.frame(b)&&nrow(b))for(i in seq_len(nrow(b)))cat("  ",b$block[i],":",.bh_report_display(b$model_name[i]),"v",.bh_report_display(b$model_version[i]),"|",.bh_report_display(b$observed_people[i]),"observed people,",.bh_report_display(b$consumed_rows[i]),"likelihood rows | units:",.bh_report_display(b$response_units[i]),"\n")
    subjects<-x$context$subjects$record
    cat("Subject domain:",.bh_report_display(subjects$joint_subject_count),"IDs | observed people:",.bh_report_display(subjects$observed_people),"| roster:",.bh_report_display(subjects$roster_count),"| unobserved roster members:",.bh_report_display(subjects$unobserved_roster_people),"\n")
    cat("Excluded likelihood records:",.bh_report_display(x$context$exclusions$count),"| saved overlap:",x$context$overlap$availability,"| observed run counts:",x$context$run$observed$availability,"\n")
    cat("Data context: fit_checks(fit)$context retains exclusions, overlap, roster and configured/observed run distinctions\n")
  }
  for(field in names(x$sampling$extrema)) {
    item<-x$sampling$extrema[[field]];keys<-item$rows
    label<-switch(field,rhat="Maximum recorded Rhat",ess_bulk="Minimum finite bulk ESS",ess_tail="Minimum finite tail ESS")
    if(isTRUE(refreshed))label<-paste("Selected-monitor",label)
    cat("  ",label,":",.bh_report_display(item$value),"[",item$status,"]")
    if(nrow(keys)) {
      key<-keys$parameter_key[1L];if(is.na(key))key<-paste0("variable ",keys$variable[1L]," (semantic key unavailable)")
      cat(" |",key,"| role:",keys$parameter_role[1L],if(nrow(keys)>1L)paste0(" | ",nrow(keys)," tied rows")else"")
    }
    c<-item$coverage;cat(" | cached rows:",c$rows,"finite:",c$finite,"missing:",c$missing,"NaN:",c$nan,"+Inf:",c$positive_infinity,"-Inf:",c$negative_infinity,"\n")
  }
  h<-x$sampling$chains$metrics
  for(field in c("num_divergent","num_max_treedepth")) {
    item<-h[[field]];cat("  ",if(field=="num_divergent")"Divergences"else"Treedepth hits","observed total:",.bh_report_display(item$observed_total),"| usable retained entries:",item$coverage$usable,"of",item$coverage$rows,"| complete retained vector:",item$complete_for_retained_entries,"\n")
  }
  item<-h$ebfmi;cat("  minimum finite E-BFMI:",.bh_report_display(item$minimum$value),"| usable retained entries:",item$coverage$usable,"of",item$coverage$rows,"\n")
  if(nrow(item$minimum$entries)) {
    row<-item$minimum$entries[1L,,drop=FALSE]
    cat("    ",if(is.na(row$chain_id))paste("stored row",row$stored_row_index,"(chain ID unavailable)")else paste("chain",row$chain_id),"\n")
  }
  cat("Cached MCSE/interval table: posterior_summary(fit) or parameter_summary(fit);",x$sampling$mcse_scope,"\n")
  cat("  Recorded interval columns:",if(length(x$sampling$interval_columns))paste(x$sampling$interval_columns,collapse=", ")else"unavailable", "| interval metadata:",x$sampling$interval_provenance$availability,"| estimator provenance:",x$sampling$summary_provenance$availability,"| threshold metadata:",x$sampling$thresholds$availability,"\n")
  labels<-c(computation="Computation",inference="Sampling/inference",predictive_fit="Predictive fit",identification="Likelihood identification",posterior_normalizability="Posterior normalizability",posterior_propriety="Posterior propriety",admissibility="Matrix-export conditioning",prior_sensitivity="Prior sensitivity",conventional_fit="Conventional fit indices")
  domains<-.bh_report_slot(x,"domains")
  for(n in names(labels))cat("  ",labels[[n]],":",.bh_report_domain_status(.bh_report_slot(domains,n)),"\n")
  if(!is.null(x$predictive$cached)).bh_print_report_ppc(x$predictive$cached)
  domain_names<-names(unclass(domains))
  ambiguous_numerical<-!is.null(domain_names)&&(!is.character(domain_names)||is.object(domain_names)||
    !is.null(dim(domain_names))||anyNA(domain_names)||!all(validEnc(domain_names))||
    sum(domain_names=="numerical_evaluations")>1L)
  numerical_domain<-if(ambiguous_numerical)list(availability="unavailable_invalid_metadata",record=NULL,
    reason="Optional numerical-evidence report domain has ambiguous names; no record was selected")else
    .bh_report_slot(domains,"numerical_evaluations")
  if(!is.null(reporting))cat("Numerical evaluation evidence:",reporting$status,"| recognized numerical-message lines: unavailable | total failed evaluations: unknown | output capture: uncomputed\n")else
    .bh_print_numerical_report(.bh_report_numerical(numerical_domain))
  if(!is.null(x$predictive$cached))
    cat("Reading this report runs no new predictive simulation or threshold assessment and gives no global model-fit verdict. Missing evidence is not a passed check.\n")else
    cat("No new convergence threshold, predictive check or global model-fit verdict is applied. Missing evidence is not a passed check.\n")
  invisible(x)
}
print.bmeas_fit_checks <- function(x,...) {
  if(inherits(x,"bmeas_portable_fit_checks")) .bh_pf_fail("print","use the portable fit-check method for portable report tags","E_PORTABLE_UNSUPPORTED")
  .bh_print_fit_checks(x)
}
