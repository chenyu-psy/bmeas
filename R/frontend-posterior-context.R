# Consumer admission catches R's missing-argument list entries without forcing
# them. The independently qualified parser/resolver bodies remain unchanged.
.bh_posterior_consumer_policy <- function(posterior,variables) {
  fields<-c("reader","max_source_bytes","max_read_bytes","max_materialized_values",
    "max_record_bytes","max_columns","max_source_rows","max_chains",
    "max_temp_bytes","max_scan_values")
  posterior<-.bh_resource_named_list(posterior,"posterior",fields,"E_PPC_SOURCE_ARGUMENT")
  .bh_posterior_policy(posterior,variables)
}

# Saved expected chain counts are context, not proof of posterior origin.
# This helper performs no file/provider access and never invents chain IDs.
.bh_posterior_expected_chains <- function(fit) {
  fit<-.bh_ppc_plain_list(fit,"fit",allow_class=TRUE)
  exact<-function(x,name) {
    if(is.null(x)||!name%in%names(x))return(NULL)
    if(identical(.subset2(x,name),quote(expr=)))
      .mp_abort("E_PPC_SOURCE_IDENTITY",paste("saved",name,"has no explicit value"))
    .subset2(x,name)
  }
  counts<-numeric();sources<-character()
  add<-function(value,source) {
    if(is.null(value))return(invisible(NULL))
    count<-.bh_count(value,source,1,.Machine$integer.max,"E_PPC_SOURCE_IDENTITY")
    counts<<-c(counts,count);sources<<-c(sources,source)
    invisible(NULL)
  }
  run<-.bh_ppc_plain_list(exact(fit,"run"),"saved run",allow_class=TRUE,allow_null=TRUE)
  if(!is.null(run))add(exact(run,"chains"),"saved run configured chains")
  receipt<-.bh_ppc_plain_list(exact(fit,"execution_receipt"),"saved execution receipt",allow_null=TRUE)
  if(!is.null(receipt)&&identical(exact(receipt,"version"),"bmeas_execution_receipt_v1")) {
    sampling<-.bh_ppc_plain_list(exact(receipt,"sampling"),"saved sampling receipt",allow_null=TRUE)
    if(!is.null(sampling))add(exact(sampling,"configured_chains"),"saved execution receipt configured chains")
  }
  if(length(unique(counts))>1L)
    .mp_abort("E_PPC_SOURCE_IDENTITY","retained expected chain counts contradict one another")
  if(!length(counts))return(list(value=NULL,availability="unavailable_in_saved_context",
    source=character(),reason="No usable saved expected chain count; actual IDs and rows must come from every recorded source file"))
  list(value=unname(counts[1L]),availability="retained",source=sources,
    reason="Saved configured chain count; no chain IDs or original sampling-time authentication are inferred")
}

# Count represented numeric/logical payload only, never dispatching a saved
# method. Text, object headers, original fit storage and internal RSS are not
# certified by this conservative explicit-buffer accounting.
.bh_posterior_numeric_payload <- function(x) {
  if(isS4(x))return(0)
  if(typeof(x)%in%c("integer","double","logical"))return(as.double(length(unclass(x))))
  if(typeof(x)!="list")return(0)
  x<-unclass(x)
  .bh_count_sum(vapply(seq_along(x),function(i).bh_posterior_numeric_payload(.subset2(x,i)),numeric(1)),
    "represented numeric metadata slots")
}

.bh_ppc_stream_buffers <- function(forward,statistics,draws) {
  O<-.bh_count_sum(vapply(forward$blocks,function(x)as.double(nrow(x$rows)),numeric(1)))
  O<-O*if(is.null(forward$response_cells_per_row))1L else forward$response_cells_per_row
  S<-as.double(nrow(statistics$statistics));V<-as.double(length(forward$required_variables))
  # Sum complete named buffers conservatively, including serial phases. This
  # allows no liveness discount or claim about hidden R/native allocations.
  fixed<-c(forward_metadata=.bh_posterior_numeric_payload(forward),
    statistic_metadata=.bh_posterior_numeric_payload(statistics),
    forward_work=as.double(forward$work_values_per_draw),
    joint_state_extract=.bh_count_product(2,V),
    received_means=O,received_scales=O,response_values=O,
    response_probabilities_or_rates=O,log_response_series=O,
    response_support_and_finite_masks=.bh_count_product(4,O),
    response_order_statistic_vectors=.bh_count_product(2,O),
    paired_person_vectors=.bh_count_product(2,statistics$counts$pair_overlap),
    observed_replicated_and_result_tables=.bh_count_sum(c(
      .bh_count_product(4,.bh_posterior_numeric_payload(statistics$statistics)),
      .bh_count_product(9,S))),scalar_bookkeeping=64)
  per_selected_row<-c(replicate_statistics=S,aggregate_transpose=S,
    retained_replicate_copy=S,replication_order_statistic_vectors=4)
  list(fixed=fixed,per_selected_row=per_selected_row)
}
