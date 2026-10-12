# Execution receipts and explicitly uncomputed posterior reporting are separate
# from the numerical-message producer schema. These helpers never read CSV data.
.bh_resource_run_label <- function(policy) {
  kind<-if(is.list(policy)).subset2(unclass(policy),"kind")else NULL
  if(identical(kind,"caller_resources"))return("CALLER RESOURCE POLICY")
  if(identical(kind,"development"))return("DEVELOPMENT POLICY")
  "SMOKE ONLY"
}
.bh_resource_inference_reason <- function(policy) paste0(.bh_resource_run_label(policy),
  "; convergence, recovery and full numerical prior support are not established")

.bh_execution_receipt <- function(backend,run,started) {
  codes<-tryCatch(backend$return_codes(),error=function(e)NULL)
  valid_codes<-typeof(codes)%in%c("integer","double")&&!is.object(codes)&&
    is.null(dim(codes))&&length(codes)>0L&&!anyNA(codes)&&all(is.finite(codes))&&all(codes==floor(codes))
  paths<-tryCatch(backend$output_files(),error=function(e)NULL)
  valid_paths<-is.character(paths)&&!is.object(paths)&&is.null(dim(paths))&&
    !anyNA(paths)&&all(validEnc(paths))&&all(nzchar(paths))
  complete_codes<-valid_codes&&length(codes)==run$chains
  complete_paths<-valid_paths&&length(paths)==run$chains&&!anyDuplicated(paths)
  successful<-complete_codes&&all(codes==0)
  list(version="bmeas_execution_receipt_v1",sampling=list(
    status=if(successful)"completed"else if(valid_codes&&any(codes!=0))"failed"else"completion_unavailable",
    reason=if(successful)"The backend sample call returned with one zero return code per configured chain; posterior contents were not inspected"else
      if(valid_codes&&any(codes!=0))"The backend reported a nonzero return code"else
        if(valid_codes)"The reported return-code count does not match the configured chain count"else"The backend returned without usable return-code evidence",
    return_codes=if(valid_codes)codes else NULL,
    return_codes_availability=if(valid_codes)"retained"else"unavailable",
    return_code_coverage=if(complete_codes)"complete_for_configured_chains"else"incomplete_or_unavailable",
    configured_chains=run$chains,observed_csv_rows=NA_real_,
    observed_csv_rows_availability="not_observed",started=started,finished=Sys.time()),
    output=list(paths=if(valid_paths)paths else character(),
      paths_availability=if(valid_paths&&length(paths))"retained"else"unavailable",
      path_coverage=if(complete_paths)"complete_for_configured_chains"else"incomplete_or_unavailable",
      configured_path_count=run$chains,reported_path_count=if(valid_paths)as.double(length(paths))else NA_real_,
      original_file_hashes=list(status="not_collected",reason="The package collects file metadata without posterior-value reads or content hashes; provider-constructor metadata scans are disclosed separately")))
}

.bh_output_resource_outcome <- function(receipt,limit) {
  paths<-receipt$output$paths
  out<-list(version="bmeas_postfit_output_resource_v1",status="measurement_unavailable",
    max_output_bytes=limit,actual_output_bytes=NA_real_,output_files=paths,path_coverage=receipt$output$path_coverage,
    files=data.frame(path=paths,bytes=rep(NA_real_,length(paths)),status=rep("unavailable",length(paths)),stringsAsFactors=FALSE),
    reason="Sampler output size is unavailable; retained paths and the execution receipt are preserved",
    enforcement="Estimate admission plus a post-run file-size check; this does not cap temporary sampler writes",
    original_file_hashes="not_collected")
  if(!identical(receipt$output$path_coverage,"complete_for_configured_chains")||
    length(paths)!=receipt$sampling$configured_chains||anyDuplicated(paths)) {
    out$reason<-"A complete unique output path per configured chain was not retained; aggregate size is unknown and all reported paths and the sampling receipt are preserved"
    return(out)
  }
  if(!identical(receipt$output$paths_availability,"retained")||!length(paths))return(out)
  info<-tryCatch(file.info(paths),error=function(e)NULL)
  if(is.null(info)||nrow(info)!=length(paths)||!is.numeric(info$size)||is.object(info$size)||
    length(info$size)!=length(paths)||!is.logical(info$isdir)||length(info$isdir)!=length(paths))return(out)
  sizes<-info$size
  valid<-!is.na(sizes)&is.finite(sizes)&sizes>=0&sizes==floor(sizes)&sizes<=2^53-1&!is.na(info$isdir)&!info$isdir
  out$files$bytes<-sizes
  out$files$status<-ifelse(valid,"measured","unavailable")
  # Missing and unstatable members invalidate the aggregate; never remove them.
  if(!all(valid))return(out)
  total<-tryCatch(.bh_count_sum(sizes,"actual sampler output bytes"),error=function(e)NULL)
  if(is.null(total)) {
    out$status<-"measurement_overflow"
    out$reason<-"The output-size total exceeds the supported exact-integer range; paths and the execution receipt are preserved"
    return(out)
  }
  out$actual_output_bytes<-total
  out$status<-if(total>limit)"exceeded"else"within_limit"
  out$reason<-if(total>limit)"Actual sampler output exceeds the caller's output allowance; files and the successful-sampler receipt are preserved"else
    "One unique output path per configured chain was statted and measured sizes are within the output allowance; the package performed no additional posterior-value read"
  out
}

.bh_uncomputed_fit_cache <- function(spec,plan,status="deferred_by_policy",reason=NULL) {
  if(is.null(reason))reason<-"Posterior diagnostics were deferred/uncomputed by the explicit cache policy"
  uncomputed<-list(status=status,reason=reason)
  reporting<-list(version="bmeas_posterior_reporting_v1",status=status,
    requested_cache=plan$resource_policy$cache,posterior_diagnostics="uncomputed",reason=reason,
    phases=stats::setNames(rep("uncomputed",6L),c("parameter_summary","hmc","conditioning",
      "availability_and_covariance","expanded_summary","numerical_messages")),
    observed_csv_rows=NA_real_,observed_csv_rows_availability="not_observed")
  list(parameter_summary=NULL,fixed_parameters=spec$tables$fixed_parameters,
    parameter_roles=spec$tables$parameter_roles,loading_cells=spec$tables$loading_cells,
    loading_primitives=spec$tables$loading_primitives,loading_orientation=spec$tables$orientation,
    component_scales=spec$tables$component_scales,components=spec$tables$components,
    hmc=NULL,optional_reports=c(uncomputed,list(unavailable_values=NA_real_)),
    admissibility=uncomputed,computation=list(status="sampling_completed",reason=
      "The backend returned successfully; posterior diagnostic completion is recorded separately"),
    posterior_reporting=reporting,
    posterior_propriety=plan$posterior_propriety,posterior_normalizability=plan$posterior_normalizability,
    gaussian_diagnostics=plan$gaussian_diagnostics,execution_profile=plan$execution_profile,
    backend_qualification=plan$backend_qualification,
    identification=if(is.null(plan$identification))spec$checks$identification else plan$identification,
    inference=list(status="not_assessed",reason=.bh_resource_inference_reason(plan$resource_policy)),
    predictive_fit=list(status="not_run",reason="Predictive checking was not requested; sampling completion does not establish posterior-source or consumer support"))
}

.bh_postfit_resource_error <- function(fit) {
  outcome<-fit$resource_outcome
  code<-if(identical(outcome$status,"exceeded"))"E_OUTPUT_BUDGET"else"E_OUTPUT_SIZE_UNAVAILABLE"
  stop(structure(list(message=paste0(code,": ",outcome$reason),call=NULL,code=code,
    fit=fit,execution_receipt=fit$execution_receipt,resource_outcome=outcome,
    output_files=outcome$output_files),class=c("bmeas_postfit_resource_error","measurement_error","error","condition")))
}

# Optional new metadata is isolated from otherwise usable historical evidence.
.bh_saved_named_list <- function(x) {
  if(typeof(x)!="list"||is.object(x)||!is.null(dim(x)))return(FALSE)
  n<-names(x)
  !length(x)||(is.character(n)&&!is.object(n)&&is.null(dim(n))&&length(n)==length(x)&&
    !anyNA(n)&&all(validEnc(n))&&all(nzchar(n))&&!base::anyDuplicated.default(n))
}
.bh_saved_string <- function(x) typeof(x)=="character"&&!is.object(x)&&is.null(dim(x))&&
  length(x)==1L&&!is.na(x)&&validEnc(x)&&nzchar(x)
.bh_saved_field <- function(x,name) {
  if(typeof(x)!="list"||isS4(x))return(NULL)
  x<-unclass(x);n<-names(x)
  if(is.null(n)||!is.character(n)||is.object(n)||!is.null(dim(n))||anyNA(n)||
    !all(validEnc(n))||sum(n==name)!=1L)return(NULL)
  if(identical(unname(x[which(n==name)]),unname(alist(value=))))return(NULL)
  .subset2(x,name)
}
.bh_optional_saved_record <- function(container,name,validate) {
  invalid<-function(reason)list(availability="unavailable_invalid_metadata",record=NULL,reason=reason)
  if(is.null(container))return(NULL)
  if(typeof(container)!="list"||isS4(container))return(invalid(paste(name,"container is not a saved list")))
  container<-unclass(container);n<-names(container)
  if(!is.null(n)&&(!is.character(n)||is.object(n)||!is.null(dim(n))||anyNA(n)||!all(validEnc(n))))
    return(invalid(paste(name,"container has malformed names")))
  count<-sum(n==name)
  if(count>1L)return(invalid(paste(name,"has ambiguous saved fields")))
  if(!count)return(NULL)
  if(identical(unname(container[which(n==name)]),unname(alist(value=))))
    return(invalid(paste(name,"has a missing-argument saved field")))
  record<-.subset2(container,name)
  if(is.null(record))return(NULL)
  valid<-tryCatch({.bh_fit_report_context_plain(record,name);isTRUE(validate(record))},error=function(e)FALSE)
  if(!valid)return(invalid(paste(name,"is malformed; unrelated retained evidence remains available")))
  list(availability="retained",record=record,reason="Saved evidence; no recalculation performed")
}
.bh_valid_posterior_reporting <- function(record) {
  if(!.bh_saved_named_list(record))return(FALSE)
  strings<-c("version","status","requested_cache","posterior_diagnostics","reason","observed_csv_rows_availability")
  if(!all(vapply(strings,function(n).bh_saved_string(.subset2(record,n)),logical(1))))return(FALSE)
  if(!identical(record$version,"bmeas_posterior_reporting_v1")||
    !record$status%in%c("deferred_by_policy","uncomputed_output_resource")||
    !record$requested_cache%in%c("eager","deferred")||
    (record$status=="deferred_by_policy"&&record$requested_cache!="deferred")||
    !identical(record$posterior_diagnostics,"uncomputed")||
    !identical(record$observed_csv_rows_availability,"not_observed"))return(FALSE)
  value<-record$observed_csv_rows
  if(!typeof(value)%in%c("double","integer")||is.object(value)||!is.null(dim(value))||
    length(value)!=1L||!is.na(value)||is.nan(value))return(FALSE)
  phases<-record$phases
  typeof(phases)=="character"&&!is.object(phases)&&is.null(dim(phases))&&
    length(phases)==6L&&!anyNA(phases)&&all(phases=="uncomputed")&&
    !anyDuplicated(names(phases))&&setequal(names(phases),c("parameter_summary","hmc","conditioning",
      "availability_and_covariance","expanded_summary","numerical_messages"))
}
.bh_posterior_reporting_record <- function(cache) {
  # V2 is projected section by section. A bad optional monitor/count/phase
  # record cannot make the historical parameter table or context disappear.
  saved<-.bh_saved_field(cache,"posterior_reporting")
  if(identical(.bh_saved_field(saved,"version"),"bmeas_posterior_reporting_v2"))
    return(.bh_diagnostic_reporting_record(saved,cache))
  out<-.bh_optional_saved_record(cache,"posterior_reporting",.bh_valid_posterior_reporting)
  if(!is.null(out)&&identical(out$availability,"retained")) {
    diagnostic_at<-which(names(unclass(cache))%in%c("parameter_summary","hmc","numerical_evaluations"))
    if(any(vapply(diagnostic_at,function(i)!is.null(.subset2(unclass(cache),i)),logical(1)))||
      .bh_retained_optional_posterior_evidence(cache))
      out<-list(availability="unavailable_invalid_metadata",record=NULL,
        reason="Uncomputed posterior-reporting state contradicts retained posterior diagnostics; retained diagnostics are preserved")
  }
  out
}
# Only the draw-dependent cache domains are considered here. Scientific maps,
# priors, identification/propriety checks and explicit empty/uncomputed records
# do not become contradictions merely because their nodes are present.
.bh_retained_optional_posterior_evidence <- function(cache) {
  payload<-function(x) {
    if(is.null(x))return(FALSE)
    if(isS4(x))return(TRUE)
    if(identical(attr(x,"class",exact=TRUE),"data.frame"))
      return(if(typeof(x)=="list")tryCatch(.row_names_info(x,2L)>0L,error=function(e)TRUE)else TRUE)
    dims<-attr(x,"dim",exact=TRUE)
    if(typeof(dims)%in%c("integer","double")&&!is.object(dims)&&length(dims)>1L&&
      !anyNA(dims)&&dims[1L]>0)return(TRUE)
    if(!typeof(x)%in%c("logical","integer","double","complex","character","raw","list"))return(TRUE)
    if(is.object(x))x<-unclass(x)
    length(x)>0L
  }
  values<-unclass(cache);keys<-names(values)
  for(i in which(keys%in%c("admissibility","optional_reports","expanded_correlation_matrices"))) {
    name<-keys[i];node<-.subset2(values,i)
    if(is.null(node))next
    if(identical(attr(node,"class",exact=TRUE),"data.frame")&&payload(node))return(TRUE)
    if(typeof(node)!="list"||isS4(node)) {if(payload(node))return(TRUE);next}
    node<-unclass(node);n<-names(node)
    if(is.null(n)||!is.character(n)||is.object(n)||anyNA(n)||!all(validEnc(n))) {
      if(length(node))return(TRUE)
      next
    }
    status<-.bh_saved_field(node,"status")
    computed<-switch(name,admissibility=c("screened_not_proven","warning"),
      optional_reports=c("available","unavailable_values"),expanded_correlation_matrices="available")
    if(.bh_saved_string(status)&&status%in%computed)return(TRUE)
    fields<-switch(name,admissibility="draws",optional_reports=c("flags","log_magnitudes"),
      expanded_correlation_matrices=character())
    for(at in which(n%in%fields)) {
      value<-.subset2(node,at)
      # A failed log acquisition can be retained as an explicit empty status.
      if(n[at]=="log_magnitudes"&&.bh_saved_named_list(value)&&
        identical(value$status,"not_assessed")&&!any(names(value)%in%c("draws","values","entries")))next
      if(payload(value))return(TRUE)
    }
    if(name%in%c("optional_reports","expanded_correlation_matrices"))for(at in which(n=="entries")) {
      entries<-.subset2(node,at)
      if(!identical(attr(entries,"class",exact=TRUE),"data.frame")||!payload(entries))next
      columns<-names(unclass(entries))
      if(is.null(columns)||!is.character(columns)||is.object(columns)||anyNA(columns)||!all(validEnc(columns)))next
      metrics<-if(name=="optional_reports")c("unavailable_draws","total_draws")else
        c("mean","median","sd","rhat","ess_bulk","ess_tail","mcse_mean")
      if(any(columns%in%metrics)||(name=="expanded_correlation_matrices"&&any(grepl("^(q[0-9]+([.][0-9]+)?|[0-9]+([.][0-9]+)?%)$",columns))))return(TRUE)
    }
  }
  FALSE
}

.bh_duplicate_optional_field <- function(x,name) {
  if(typeof(x)!="list"||isS4(x))return(FALSE)
  n<-names(unclass(x))
  !is.null(n)&&is.character(n)&&!is.object(n)&&is.null(dim(n))&&!anyNA(n)&&
    all(validEnc(n))&&sum(n==name)>1L
}
.bh_optional_report_scalar <- function(x) {
  if(is.null(x))return(NULL)
  if(!typeof(x)%in%c("logical","integer","double","character")||is.object(x)||
    !is.null(dim(x))||length(x)!=1L)return("unavailable")
  if(is.character(x)&&!is.na(x)&&!validEnc(x))return("unavailable")
  x
}
.bh_optional_reporting_context <- function(fit) {
  # This is a projection-only view. The saved fit/cache and all uniquely named
  # historical fields stay untouched; ambiguous spec/run/cache names are never
  # removed and therefore still encounter the original strict context getter.
  drop_new_duplicates<-function(x,fields) {
    if(typeof(x)!="list"||isS4(x)||!is.null(dim(x)))return(x)
    duplicates<-fields[vapply(fields,function(n).bh_duplicate_optional_field(x,n),logical(1))]
    if(!length(duplicates))return(x)
    values<-unclass(x);values[!names(values)%in%duplicates]
  }
  view<-drop_new_duplicates(fit,c("execution_receipt","resource_outcome"))
  if(typeof(view)!="list"||isS4(view))return(view)
  n<-names(unclass(view))
  if(!is.null(n)&&is.character(n)&&!is.object(n)&&!anyNA(n)&&all(validEnc(n))&&sum(n=="cache")==1L) {
    view<-unclass(view);at<-which(n=="cache")
    view[[at]]<-drop_new_duplicates(.subset2(view,at),"posterior_reporting")
  }
  view
}
.bh_valid_execution_receipt <- function(record) {
  if(!.bh_saved_named_list(record)||!identical(record$version,"bmeas_execution_receipt_v1")||
    !.bh_saved_named_list(record$sampling)||!.bh_saved_named_list(record$output))return(FALSE)
  sampling<-record$sampling;output<-record$output
  if(!all(vapply(c("status","reason","return_codes_availability","return_code_coverage","observed_csv_rows_availability"),
    function(n).bh_saved_string(.subset2(sampling,n)),logical(1))))return(FALSE)
  if(!sampling$status%in%c("completed","failed","completion_unavailable")||
    !sampling$return_codes_availability%in%c("retained","unavailable")||
    !sampling$return_code_coverage%in%c("complete_for_configured_chains","incomplete_or_unavailable")||
    !identical(sampling$observed_csv_rows_availability,"not_observed"))return(FALSE)
  chains<-sampling$configured_chains;rows<-sampling$observed_csv_rows;codes<-sampling$return_codes
  if(!typeof(chains)%in%c("integer","double")||is.object(chains)||!is.null(dim(chains))||length(chains)!=1L||
    is.na(chains)||!is.finite(chains)||chains<1||chains!=floor(chains)||chains>.Machine$integer.max)return(FALSE)
  if(!typeof(rows)%in%c("integer","double")||is.object(rows)||!is.null(dim(rows))||length(rows)!=1L||!is.na(rows)||is.nan(rows))return(FALSE)
  valid_codes<-typeof(codes)%in%c("integer","double")&&!is.object(codes)&&is.null(dim(codes))&&
    length(codes)>0L&&!anyNA(codes)&&all(is.finite(codes))&&all(codes==floor(codes))
  if(sampling$return_codes_availability=="retained"&&!valid_codes)return(FALSE)
  if(sampling$return_codes_availability=="unavailable"&&!is.null(codes))return(FALSE)
  complete_codes<-valid_codes&&length(codes)==chains
  if((sampling$return_code_coverage=="complete_for_configured_chains")!=complete_codes)return(FALSE)
  if(sampling$status=="completed"&&(!valid_codes||length(codes)!=chains||any(codes!=0)||
    sampling$return_code_coverage!="complete_for_configured_chains"))return(FALSE)
  if(sampling$status=="failed"&&(!valid_codes||!any(codes!=0)))return(FALSE)
  if(sampling$status=="completion_unavailable"&&(complete_codes||(valid_codes&&any(codes!=0))))return(FALSE)
  paths<-output$paths
  valid_paths<-is.character(paths)&&!is.object(paths)&&is.null(dim(paths))&&!anyNA(paths)&&all(validEnc(paths))&&all(nzchar(paths))
  if(!valid_paths)return(FALSE)
  expected_paths<-output$configured_path_count;reported_paths<-output$reported_path_count
  if(!typeof(expected_paths)%in%c("integer","double")||is.object(expected_paths)||!is.null(dim(expected_paths))||
    length(expected_paths)!=1L||is.na(expected_paths)||!identical(as.double(expected_paths),as.double(chains)))return(FALSE)
  if(!typeof(reported_paths)%in%c("integer","double")||is.object(reported_paths)||!is.null(dim(reported_paths))||
    length(reported_paths)!=1L||is.nan(reported_paths))return(FALSE)
  if(is.na(reported_paths)) {
    if(length(paths)||!identical(output$paths_availability,"unavailable"))return(FALSE)
  }else if(!is.finite(reported_paths)||reported_paths!=length(paths))return(FALSE)
  if(!.bh_saved_string(output$path_coverage)||!output$path_coverage%in%c("complete_for_configured_chains","incomplete_or_unavailable")||
    ((output$path_coverage=="complete_for_configured_chains")!=(length(paths)==chains&&!anyDuplicated(paths))))return(FALSE)
  valid_paths&&
    .bh_saved_string(output$paths_availability)&&output$paths_availability%in%c("retained","unavailable")&&
    ((output$paths_availability=="retained")==(length(paths)>0L))&&
    .bh_saved_named_list(output$original_file_hashes)&&identical(output$original_file_hashes$status,"not_collected")&&
    .bh_saved_string(output$original_file_hashes$reason)
}
.bh_project_uncomputed_reporting <- function(report,cache,fit=NULL) {
  receipt<-.bh_optional_saved_record(fit,"execution_receipt",.bh_valid_execution_receipt)
  if(!is.null(receipt))report$context$run$execution_receipt<-receipt
  if(.bh_duplicate_optional_field(fit,"resource_outcome"))
    report$context$run$resource_outcome<-.bh_optional_saved_record(fit,"resource_outcome",function(x)FALSE)
  reporting<-.bh_posterior_reporting_record(cache)
  if(is.null(reporting))return(report)
  report$context$run$posterior_reporting<-reporting
  report$domains$posterior_reporting<-reporting
  if(!identical(reporting$availability,"retained"))return(report)
  record<-reporting$record
  if(identical(.bh_saved_field(record,"version"),"bmeas_posterior_reporting_v2"))
    return(.bh_project_diagnostic_reporting(report,reporting))
  unavailable<-list(availability=record$status,record=NULL,reason=record$reason)
  report$sampling$posterior_reporting<-record
  report$sampling$parameters_availability<-record$status
  for(n in names(report$sampling$extrema)) {
    x<-report$sampling$extrema[[n]];x$availability<-x$status<-record$status
    x$reason<-record$reason;x$value<-NA_real_
    for(field in setdiff(names(x$coverage),"scope"))x$coverage[[field]]<-NA_real_
    x$coverage$scope<-record$reason;report$sampling$extrema[[n]]<-x
  }
  report$sampling$chains$availability<-record$status
  report$sampling$chains$reason<-record$reason
  for(n in names(report$sampling$chains$metrics)) {
    x<-report$sampling$chains$metrics[[n]];x$availability<-record$status;x$reason<-record$reason
    x$observed_total<-NA_real_;x$minimum$value<-NA_real_;x$complete_for_retained_entries<-NA
    x$coverage<-list(rows=NA_real_,usable=NA_real_,scope=record$reason)
    report$sampling$chains$metrics[[n]]<-x
  }
  for(n in c("summary_provenance","thresholds","interval_provenance"))report$sampling[[n]]<-unavailable
  report$domains$numerical_evaluations<-unavailable
  report$domains$numerical_evaluations$interpretation<-list(
    version="bmeas_uncomputed_numerical_report_v1",status=record$status,reason=record$reason,
    recognized_message_lines=NA_real_,total_failed_evaluations=NA_real_,output_capture="uncomputed")
  report
}
