# Optional PPC presentation is a projection of retained records only. Keep every
# malformed optional field local; never invoke a method on a saved object.
.bh_report_ppc_plain <- function(x) {
  typeof(x)=="list"&&!is.object(x)&&is.null(dim(x))&&
    (length(x)==0L||(typeof(names(x))=="character"&&!is.object(names(x))&&is.null(dim(names(x)))&&
      all(validEnc(names(x)))&&!anyNA(names(x))&&
      !anyDuplicated(names(x))&&all(nzchar(names(x)))))
}
.bh_report_ppc_atomic <- function(x,type) {
  typeof(x)%in%type&&!is.object(x)&&is.null(dim(x))
}
.bh_report_ppc_count <- function(x) {
  .bh_report_ppc_atomic(x,c("integer","double"))&&length(x)==1L&&
    is.finite(x)&&x>=0&&x==floor(x)
}
.bh_report_ppc_field <- function(x,name,valid) {
  unavailable<-function(state,reason)list(availability=state,record=NULL,reason=reason)
  if(!is.null(x)&&!.bh_report_ppc_plain(x))return(unavailable("unavailable_invalid_metadata",
    "The optional saved container is not an unambiguous plain list"))
  if(!is.null(x)&&name%in%names(x)&&identical(.subset2(x,name),quote(expr=)))
    return(unavailable("unavailable_invalid_metadata",paste(name,"has no explicit saved value")))
  value<-.bh_report_slot(x,name)
  if(is.null(value))return(unavailable("unavailable_in_cache",paste(name,"was not retained")))
  if(!tryCatch(isTRUE(valid(value)),error=function(e)FALSE))
    return(unavailable("unavailable_invalid_metadata",paste(name,"has invalid saved metadata")))
  list(availability="retained",record=value,reason="Saved evidence is returned without reassessment")
}
.bh_report_ppc_limits <- function(meta) {
  historical<-list(scalable=FALSE,draws=20L,statistics=200L)
  if(!.bh_report_ppc_plain(meta))return(historical)
  version<-.bh_saved_field(meta,"version")
  if(!.bh_report_ppc_atomic(version,"character")||length(version)!=1L||is.na(version)||
    !version%in%c("bmeas_basic_ppc_result_v2","bmeas_m3cs_ppc_result_v2"))
    return(historical)
  tryCatch({
    plan<-.bh_saved_field(meta,"plan")
    .bh_pf_ppc_plan(plan,"cache.ppc.metadata.plan")
    plan<-unclass(plan)
    expected<-if(identical(version,"bmeas_m3cs_ppc_result_v2"))"bmeas_m3cs_ppc_plan_v3"else"bmeas_basic_ppc_plan_v3"
    if(!identical(plan$version,expected)||!identical(.bh_saved_field(meta,"level"),plan$level)||
      !identical(.bh_saved_field(meta,"law"),plan$law))return(NULL)
    requested<-.bh_saved_field(meta,"requested_replications")
    completed<-.bh_saved_field(meta,"completed_replications")
    if(!.bh_report_ppc_count(requested)||requested!=plan$replications||
      !.bh_report_ppc_count(completed)||completed>requested)return(NULL)
    list(scalable=TRUE,draws=plan$replications,statistics=plan$statistic_counts$statistics)
  },error=function(e)NULL)
}
.bh_report_ppc_table <- function(x,max_draws=20L,max_statistics=200L) {
  if(!identical(class(x),"data.frame")||anyDuplicated(names(x))||
      .row_names_info(x,2L)>max_statistics)return(FALSE)
  columns<-unclass(x);n<-.row_names_info(x,2L)
  if(!all(vapply(columns,function(z).bh_report_ppc_atomic(z,c("integer","double","character","logical"))&&length(z)==n,logical(1))))return(FALSE)
  character_fields<-c("statistic_id","domain","level","unit")
  numeric_fields<-c("observed","replicated_median","lower90","upper90","finite_replications","requested_replications")
  if(!all(c(character_fields,numeric_fields,"outside_interval")%in%names(columns)))return(FALSE)
  if(!all(vapply(columns[character_fields],function(z)is.character(z)&&!anyNA(z),logical(1)))||
      !all(vapply(columns[numeric_fields],is.numeric,logical(1)))||
      !is.logical(columns$outside_interval)||anyDuplicated(columns$statistic_id))return(FALSE)
  if(length(unique(columns$level))>1L||any(!columns$level%in%c("population","conditional"))||
      any(!columns$domain%in%c("marginal","measurement_structure","conditional_association")))return(FALSE)
  counts<-c(columns$finite_replications,columns$requested_replications)
  all(is.finite(counts)&counts>=0&counts<=max_draws&counts==floor(counts))&&
    all(columns$finite_replications<=columns$requested_replications)
}
.bh_report_ppc_coverage <- function(x,max_statistics=200L) {
  if(!.bh_report_ppc_plain(x)||!all(c("planned","evaluable","complete","unavailable","status")%in%names(x)))return(FALSE)
  if(!all(vapply(x[c("planned","evaluable","complete","unavailable")],.bh_report_ppc_count,logical(1))))return(FALSE)
  .bh_report_ppc_atomic(x$status,"character")&&length(x$status)==1L&&!is.na(x$status)&&
    x$status%in%c("complete","partial","unavailable","unassessed","no_statistics")&&
    x$planned<=max_statistics&&x$complete<=x$evaluable&&x$evaluable<=x$planned&&
    x$unavailable==x$planned-x$evaluable
}
.bh_report_ppc_streaming_source <- function(x,max_selected_rows=20L) {
  if(!.bh_saved_named_list(x)||!all(c("version","status","reader","source_rows",
    "selected_rows","selected_required_values","selection")%in%names(x))||
    !identical(x$version,1L)||!identical(x$status,"acquired")||
    !.bh_saved_named_list(x$reader)||!all(c("package","function_name")%in%names(x$reader))||
    !identical(x$reader$package,"bmeas")||
    !identical(x$reader$function_name,"private_streaming_csv"))return(FALSE)
  if(!all(vapply(x[c("source_rows","selected_rows","selected_required_values")],
    .bh_report_ppc_count,logical(1)))||!all(c("source_rows","selected_rows","selected_required_values")%in%names(x)))return(FALSE)
  if(any(unlist(x[c("source_rows","selected_rows","selected_required_values")],use.names=FALSE)>2^53-1))return(FALSE)
  if(x$source_rows<x$selected_rows||x$selected_rows<2||x$selected_rows>max_selected_rows)return(FALSE)
  if(x$selected_required_values%%x$selected_rows!=0||
    x$selected_required_values/x$selected_rows>.Machine$integer.max)return(FALSE)
  z<-x$selection
  if(!.bh_saved_named_list(z)||!all(c("method","allocations")%in%names(z))||
    !identical(z$method,"balanced"))return(FALSE)
  a<-z$allocations
  if(!identical(class(a),"data.frame")||.row_names_info(a,2L)<1L||.row_names_info(a,2L)>max_selected_rows||
    !.bh_saved_named_list(unclass(a)))return(FALSE)
  columns<-c("chain_id","source_rows","selected_rows","omitted_rows")
  if(!setequal(names(a),columns))return(FALSE)
  n<-.row_names_info(a,2L);a<-unclass(a)
  if(!all(vapply(a,function(v).bh_report_ppc_atomic(v,c("integer","double"))&&length(v)==n&&
    all(is.finite(v))&&all(v==floor(v)),logical(1))))return(FALSE)
  if(any(a$chain_id<1)||anyDuplicated(a$chain_id)||is.unsorted(a$chain_id,strictly=TRUE)||
    any(a$chain_id>.Machine$integer.max)||any(a$source_rows<1)||any(a$source_rows>.Machine$integer.max)||
    length(unique(a$source_rows))!=1L||any(a$selected_rows<1)||
    any(a$selected_rows>a$source_rows)||any(a$omitted_rows!=a$source_rows-a$selected_rows))return(FALSE)
  allocated<-x$selected_rows%/%n+as.double(seq_len(n)<=x$selected_rows%%n)
  if(any(a$selected_rows!=allocated))return(FALSE)
  identical(as.double(sum(a$source_rows)),as.double(x$source_rows))&&
    identical(as.double(sum(a$selected_rows)),as.double(x$selected_rows))
}
.bh_report_ppc_streaming_consistent <- function(meta,receipt,counts,table) {
  if(!identical(.subset2(meta,"posterior_selection"),receipt$selection)||
    !.bh_report_ppc_streaming_keys(.subset2(meta,"draw_keys"),receipt))return(FALSE)
  D<-receipt$selected_rows
  if(!all(vapply(counts,function(x)identical(x$availability,"retained")&&
    identical(as.double(x$record),as.double(D)),logical(1))))return(FALSE)
  if(identical(table$availability,"retained")&&
    any(.bh_report_column(table$record,"requested_replications")!=D))return(FALSE)
  plan<-.subset2(meta,"plan")
  if(!is.null(plan)) {
    if(typeof(plan)!="list"||isS4(plan))return(FALSE)
    plan<-unclass(plan)
    if(!.bh_saved_named_list(plan)||!"required_columns"%in%names(plan))return(FALSE)
    V<-.subset2(plan,"required_columns")
    if(!.bh_report_ppc_count(V)||V>.Machine$integer.max||
      receipt$selected_required_values!=D*V)return(FALSE)
  }
  TRUE
}
.bh_report_ppc_streaming_keys <- function(keys,receipt) {
  if(!identical(class(keys),"data.frame")||!.bh_saved_named_list(unclass(keys))||
    !setequal(names(keys),c("chain_id","saved_row"))||.row_names_info(keys,2L)!=receipt$selected_rows)return(FALSE)
  keys<-unclass(keys)
  if(!all(vapply(keys,function(v).bh_report_ppc_atomic(v,c("integer","double"))&&
    all(is.finite(v))&&all(v==floor(v)),logical(1))))return(FALSE)
  a<-receipt$selection$allocations
  expected<-.bh_ppc_source_selection(as.double(a$chain_id),as.double(a$source_rows[1L]),
    as.integer(receipt$selected_rows))
  identical(as.double(keys$chain_id),as.double(expected$chain_id))&&
    identical(as.double(keys$saved_row),as.double(expected$saved_row))
}
.bh_report_ppc <- function(ppc,assessment) {
  enum<-function(values)function(x).bh_report_ppc_atomic(x,"character")&&
    length(x)==1L&&!is.na(x)&&x%in%values
  scalar_text<-function(x).bh_report_ppc_atomic(x,"character")&&length(x)==1L&&!is.na(x)
  metadata<-.bh_report_ppc_field(ppc,"metadata",.bh_report_ppc_plain)
  # Pass a malformed sentinel through field projection so its unavailable status
  # is preserved rather than disguising invalid metadata as absent metadata.
  meta<-if(metadata$availability=="unavailable_invalid_metadata")FALSE else metadata$record
  limits<-.bh_report_ppc_limits(meta)
  level<-.bh_report_ppc_field(meta,"level",enum(c("population","conditional")))
  law<-.bh_report_ppc_field(meta,"law",enum(c("iid_common_plus_parameter_residual",
    "same_N_sample_centered_task_cholesky","conditional_response")))
  table<-.bh_report_ppc_field(ppc,"table",function(x)!is.null(limits)&&
    .bh_report_ppc_table(x,limits$draws,limits$statistics)&&
    (!limits$scalable||(.row_names_info(x,2L)==limits$statistics&&
      all(.bh_report_column(x,"requested_replications")==limits$draws))))
  counts<-lapply(c("requested_replications","completed_replications"),function(name)
    .bh_report_ppc_field(meta,name,function(x)!is.null(limits)&&.bh_report_ppc_count(x)&&x<=limits$draws))
  names(counts)<-c("requested","completed")
  failure<-.bh_report_ppc_field(meta,"failure",.bh_report_ppc_plain)
  assessment_status<-if(.bh_report_ppc_plain(assessment$record))
    .bh_report_slot(assessment$record,"status")else NULL
  status_valid<-enum(c("computed","warning","not_assessed","failed","not_run",
    "not_computed","not_requested"))(assessment_status)
  status<-if(!.bh_report_ppc_plain(ppc))"unavailable_invalid_metadata"else
    if(status_valid)if(assessment_status%in%c("not_run","not_computed","not_requested"))"uncomputed"else assessment_status else
    if(failure$availability=="retained")"failed"else
    if(table$availability=="retained")"retained"else"unavailable_in_cache"
  assessment_level<-if(.bh_report_ppc_plain(assessment$record))
    .bh_report_slot(assessment$record,"level")else NULL
  table_conditional<-table$availability=="retained"&&.row_names_info(table$record,2L)>0L&&
    all(.bh_report_column(table$record,"level")=="conditional")
  conditional<-identical(level$record,"conditional")||identical(law$record,"conditional_response")||
    identical(assessment_level,"conditional")||table_conditional
  out<-list(availability=if(.bh_report_ppc_plain(ppc))"retained"else"unavailable_invalid_metadata",
    status=status,metadata=metadata,level=level,law=law,replications=counts,table=table,
    source_qualification=.bh_report_ppc_field(meta,"source_qualification",scalar_text),
    coverage=.bh_report_ppc_field(ppc,"coverage",function(x)!is.null(limits)&&.bh_report_ppc_coverage(x,limits$statistics)),
    measurement_structure=.bh_report_ppc_field(ppc,"measurement_structure",function(x)!is.null(limits)&&.bh_report_ppc_coverage(x,limits$statistics)),
    conditional_association=.bh_report_ppc_field(ppc,"conditional_association",function(x)!is.null(limits)&&.bh_report_ppc_coverage(x,limits$statistics)),
    quantile_type=.bh_report_ppc_field(ppc,"quantile_type",function(x).bh_report_ppc_count(x)&&x>=1&&x<=9),
    interpretation=.bh_report_ppc_field(ppc,"interpretation",scalar_text),
    population_measurement_scope=if(conditional)"unassessed: conditional response checks only"else
      if(identical(level$record,"population"))"paired-person response covariance; not latent R or Sigma"else
        "unavailable: predictive level was not retained as valid metadata",
    scope="Coarse descriptive finite-replication intervals; potentially dependent posterior states, no calibrated p-value or good-fit verdict")
  if(.bh_report_ppc_plain(meta)&&"posterior_selection"%in%names(meta)) {
    out$posterior_source<-.bh_report_ppc_field(meta,"source_receipt",function(x)!is.null(limits)&&
      .bh_report_ppc_streaming_source(x,limits$draws))
    if(identical(out$posterior_source$availability,"retained")&&
      !isTRUE(tryCatch(.bh_report_ppc_streaming_consistent(meta,out$posterior_source$record,counts,table),
        error=function(e)FALSE)))
      out$posterior_source<-list(availability="unavailable_invalid_metadata",record=NULL,
        reason="Saved source and selected-state metadata contradict one another; the predictive table remains unchanged")
  }
  out
}
.bh_report_ppc_streaming_display <- function(x) {
  invalid<-function()list(availability="unavailable_invalid_metadata",record=NULL,
    reason="The optional saved source display is malformed or inconsistent; other saved evidence remains unchanged")
  tryCatch({
    source<-.subset2(x,"posterior_source")
    if(!.bh_report_ppc_plain(source)||!"availability"%in%names(source))return(invalid())
    availability<-.subset2(source,"availability")
    if(!.bh_report_ppc_atomic(availability,"character")||length(availability)!=1L||
      is.na(availability)||!validEnc(availability)||!availability%in%
      c("retained","unavailable_invalid_metadata","unavailable_in_cache"))return(invalid())
    if(!identical(availability,"retained"))return(list(availability=availability,record=NULL))
    metadata<-.bh_report_ppc_field(.subset2(x,"metadata"),"record",.bh_report_ppc_plain)
    if(!identical(metadata$availability,"retained"))return(invalid())
    meta<-metadata$record
    limits<-.bh_report_ppc_limits(meta)
    if(is.null(limits))return(invalid())
    projected<-.bh_report_ppc_field(meta,"source_receipt",function(value)
      .bh_report_ppc_streaming_source(value,limits$draws))
    if(!identical(projected$availability,"retained")||
      !identical(.subset2(source,"record"),projected$record))return(invalid())
    counts<-lapply(c("requested_replications","completed_replications"),function(name)
      .bh_report_ppc_field(meta,name,function(value).bh_report_ppc_count(value)&&value<=limits$draws))
    displayed<-.subset2(x,"replications")
    if(!.bh_report_ppc_plain(displayed)||
      !all(c("requested","completed")%in%names(displayed)))return(invalid())
    for(name in c("requested","completed")) {
      field<-.subset2(displayed,name)
      if(!.bh_report_ppc_plain(field)||!all(c("availability","record")%in%names(field))||
        !identical(.subset2(field,"availability"),"retained")||
        !.bh_report_ppc_count(.subset2(field,"record"))||
        .subset2(field,"record")!=projected$record$selected_rows)return(invalid())
    }
    table<-.bh_report_ppc_field(.subset2(x,"table"),"record",function(value)
      .bh_report_ppc_table(value,limits$draws,limits$statistics))
    if(!.bh_report_ppc_streaming_consistent(meta,projected$record,counts,table))return(invalid())
    source
  },error=function(e)invalid())
}
.bh_print_report_ppc <- function(x) {
  display<-function(field)if(identical(field$availability,"retained"))
    .bh_report_display(field$record)else field$availability
  cat("Cached PPC:",x$status,"| level:",display(x$level),"| law:",display(x$law),"\n")
  cat("  Replications requested:",display(x$replications$requested),"| completed:",display(x$replications$completed),"\n")
  cat("  Saved source qualification:",display(x$source_qualification),"\n")
  if("posterior_source"%in%names(x)) {
    source<-.bh_report_ppc_streaming_display(x)
    if(identical(source$availability,"retained"))cat("  Streaming source:",source$record$source_rows,
      "validated retained states;",source$record$selected_rows,"balanced selected states for this check\n")else
      cat("  Streaming source selection:",source$availability,"\n")
  }
  for(name in c("coverage","measurement_structure","conditional_association")) {
    field<-x[[name]]
    if(name=="measurement_structure"&&grepl("^unassessed:",x$population_measurement_scope)) {
      cat("  Population measurement structure: unassessed; conditional response checks only\n")
    }else if(field$availability=="retained") {
      r<-field$record
      cat("  ",name,":",r$status,"| evaluable:",r$evaluable,"of",r$planned,
        "| complete:",r$complete,"| unavailable:",r$unavailable,"\n")
    }else cat("  ",name,":",field$availability,"\n")
  }
  if(x$status%in%c("computed","warning","not_assessed","retained")&&x$table$availability=="retained") {
    tab<-x$table$record;n<-.row_names_info(tab,2L)
    cat("  Cached discrepancies:",n,"rows | empirical 5%/95% limits | R quantile type:",display(x$quantile_type),"\n")
    for(i in seq_len(min(n,4L))) {
      cat("    ",tab$statistic_id[i],"| observed:",.bh_report_display(tab$observed[i]),
        "| replicated median:",.bh_report_display(tab$replicated_median[i]),
        "| [",.bh_report_display(tab$lower90[i]),",",.bh_report_display(tab$upper90[i]),"]",
        "| units:",.bh_report_display(tab$unit[i]),"| finite/requested:",tab$finite_replications[i],"/",tab$requested_replications[i],
        "| outside interval:",.bh_report_display(tab$outside_interval[i]),"\n")
    }
    cat("  Full saved discrepancy table: fit_checks(fit)$predictive$cached$table$record (saved order; no ranking across units)\n")
  }else cat("  Current discrepancy table:",x$table$availability,"\n")
  cat("  Population measurement scope:",x$population_measurement_scope,"\n")
  cat("  ",x$scope,"\n")
  invisible(x)
}
