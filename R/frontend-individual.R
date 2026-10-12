# Explicit bounded extraction of person-level quantities from aligned saved rows.
# Passive fit reports never call this operation or acquire posterior sources.
.bh_individual_parameter_summary <- function(fit, scales, budget, states = "all", context_validation=NULL, acquisition_paths=NULL, archive_hashes=NULL) {
  portable<-.bh_pf_active_route(fit,context_validation,"individual_parameter_summary")
  plan <- .bh_individual_plan(fit, scales, budget, states)
  streaming <- !is.null(plan$budget$posterior)
  # This explicit operation checks the original sealed context. It does not
  # migrate an old object or attach the live specification to the result.
  if(!portable) .bh_ppc_validate_saved_spec(plan$original_spec)
  cells <- nrow(plan$cells)
  if (!cells) {
    acquired <- NULL
    receipt <- list(status = "not_needed",
      reason = "No declared predictor-role parameters; no posterior source was acquired")
    draw_keys <- data.frame(chain_id = integer(), saved_row = integer())
    retained_rows <- required_values <- 0L
  } else {
    if (streaming) {
      expected_chains <- .bh_posterior_expected_chains(fit)
      acquired <- .bh_posterior_ingest_csv(if(is.null(acquisition_paths))plan$output_files else acquisition_paths, plan$required_variables,
        plan$budget$posterior, states = plan$states, alias_groups = plan$alias_groups,
        expected_model = "scalar_components_model", expected_chains = expected_chains$value,
        consumer_buffers = plan$consumer_buffers, consumer_work = plan$consumer_work, archive_hashes = archive_hashes)
      acquired$receipt$expected_chain_context <- expected_chains
    } else acquired <- .bh_individual_ingest_csv(plan$output_files,
      plan$required_variables, plan$budget, expected_model = "scalar_components_model")
    receipt <- acquired$receipt
    draw_keys <- acquired$draw_keys
    retained_rows <- nrow(acquired$draws)
    required_values <- if (streaming) .bh_individual_product(retained_rows, ncol(acquired$draws),
      label = "selected required values") else retained_rows * ncol(acquired$draws)
    receipt$bound_model_identity <- plan$identity
    if(portable) receipt$portable_bound_identity<-context_validation$identity else {
      receipt$saved_source_code_hash <- .mp_hash(plan$saved_source_code)
      receipt$parameter_map_hash <- .mp_hash(list(cells = plan$cells,
        scales = plan$scales, roster = plan$context$person_roster))
    }
    receipt$source_qualification <- plan$source_qualification
    receipt$identity_scope <- paste(
      "Association with supplied saved identities and bytes read now;",
      "original fit-time CSV/executable/data origin is not independently authenticated")
  }
  receipt<-.bh_pf_context_attach(receipt,context_validation)
  transform_terms <- if (streaming) .bh_individual_product(cells, retained_rows, length(plan$scales),
    label = "selected transformation terms") else cells * retained_rows * length(plan$scales)
  output_limit <- if (streaming) plan$budget$max_summary_rows else 3000L
  transform_limit <- if (streaming) plan$budget$max_transformation_terms else 300000L
  if (transform_terms > transform_limit || cells * length(plan$scales) > output_limit)
    .mp_abort("E_INDIVIDUAL_BUDGET", "full scalar transformation or output work exceeds the published limit")
  result <- .bh_individual_summarize(plan, acquired)
  unavailable <- if (nrow(result)) sum(result$status != "available") else 0L
  status <- if (!cells) "not_applicable_no_predictor_parameters" else
    if (unavailable) "computed_with_unavailable_summaries" else "computed"
  metadata <- list(version = "bmeas_individual_parameter_summary_v1",
    status = status, requested_scales = plan$scales,
    source_qualification = plan$source_qualification,
    source_origin_scope = "Current-input receipt; no original fit-time posterior authentication is inferred",
    row_order = "Saved logical-role order, roster person_index, requested scale order",
    state_order = "Saved-row rank then ascending numeric chain ID; saved rows are not original sampler iterations",
    summary_method = list(mean = "arithmetic mean of represented states",
      median = "empirical median", sd = "sample standard deviation",
      quantile_probabilities = c(.05, .95), quantile_type = 7L),
    diagnostics_scope = "No new Rhat, ESS or MCSE; unavailable diagnostics do not imply adequate Monte Carlo precision",
    interpretation = paste(
      "Empirical pushforward summaries of the retained joint states, with fixed inputs and shared aliases labelled.",
      "These are uncertain model quantities, not error-free observations or an inference-quality verdict."),
    upstream = plan$upstream)
  if (streaming) {
    selection <- .bh_individual_streaming_selection(plan, acquired, draw_keys)
    metadata$version <- "bmeas_individual_parameter_summary_v2"
    metadata$selection <- selection
    metadata$state_order <- selection$state_order
    metadata$interpretation <- paste("Empirical pushforward summaries of", if (identical(selection$method,"all"))
      "every validated retained joint state." else "the explicitly selected retained joint states.",
      "Fixed inputs and shared aliases are labelled; these are uncertain model quantities, not error-free observations or an inference-quality verdict.")
    metadata$required_raw_validation_scope <- "All retained source states, including unselected states and every required shared-alias column"
    metadata$transformation_and_statistic_scope <- "Selected empirical states; any invalid selected transform invalidates that complete cell/scale summary"
    metadata$materialized_values_scope <- paste("Declared explicit numeric/logical buffer slots, including exact-summary vectors and numeric output columns;",
      "not all R objects, strings, internal or native copies, sorting overhead, original fit storage or operating-system RSS")
    receipt$non_csv_work <- if(portable) "Bounded immutable canonical validation is reported separately in context_validation; no legacy serialization seal is recomputed" else "Explicit saved scientific/spec validation and retained source/parameter-map hashing are separate from posterior CSV byte and temporary-storage accounting"
  }
  attr(result, "metadata") <- metadata
  attr(result, "source_receipt") <- receipt
  attr(result, "draw_keys") <- draw_keys
  attr(result, "context") <- plan$context
  attr(result, "budget") <- plan$budget
  attr(result, "work") <- list(logical_parameter_cells = cells,
    requested_scales = length(plan$scales), summary_rows = nrow(result),
    retained_source_rows = retained_rows,
    required_columns = length(plan$required_variables),
    required_values = required_values, transformation_terms = transform_terms,
    output_row_limit = output_limit, transformation_term_limit = transform_limit)
  if (streaming) {
    work <- attr(result, "work")
    work$retained_source_rows <- selection$full_source_rows
    work$selected_source_rows <- selection$selected_rows
    work$omitted_source_rows <- selection$omitted_rows
    work$context_terms <- plan$context$planned_work$context_terms
    work$context_terms_by_operation <- plan$context$planned_work$context_terms_by_operation
    work$consumer_buffers <- plan$consumer_buffers
    work$consumer_work <- plan$consumer_work
    work$selected_alias_comparison_terms <- .bh_individual_product(retained_rows,
      as.double(sum(pmax(0L,lengths(plan$alias_groups)-1L))), label="selected alias verification terms")
    attr(result, "work") <- work
  }
  class(result) <- c("bmeas_individual_parameter_summary", "data.frame")
  result
}

.bh_individual_streaming_selection <- function(plan, acquired, keys) {
  empty <- is.null(acquired)
  method <- if(identical(plan$states,"all")) "all" else "balanced"
  # Acquisition owns all source counts and per-chain allocation. Never infer the
  # full-source denominator from a selected matrix or from saved sampler iterations.
  full <- if(empty)0 else acquired$receipt$source_rows
  selected <- if(empty)0 else acquired$receipt$selected_rows
  .bh_individual_count(full,"validated full source rows",TRUE)
  .bh_individual_count(selected,"selected empirical rows",TRUE)
  if(selected>full||selected!=nrow(keys)||(!empty&&selected!=nrow(acquired$draws))||
      (method=="all"&&selected!=full))
    .bh_individual_fail("streaming source/selection counts disagree","E_INDIVIDUAL_SOURCE")
  if(!empty) {
    selected_values<-.bh_individual_count(acquired$receipt$selected_required_values,"selected required values")
    if(selected_values!=.bh_individual_product(selected,ncol(acquired$draws)))
      .bh_individual_fail("source selected numeric-value count disagrees with its matrix","E_INDIVIDUAL_SOURCE")
  }
  parser_selection<-if(empty)list(method=method)else .bh_individual_list(acquired$receipt$selection,"source selection")
  if(!identical(parser_selection$method,method)||
      (!empty&&!identical(plan$states,"all")&&selected!=plan$states$draws))
    .bh_individual_fail("source selection method or selected denominator differs from request","E_INDIVIDUAL_SOURCE")
  selection<-list(version="bmeas_individual_selection_v1",method=method,requested_states=plan$states,full_source_rows=full,selected_rows=selected,
    omitted_rows=full-selected,
    per_chain=if(empty)data.frame(chain_id=integer(),source_rows=integer(),selected_rows=integer(),omitted_rows=integer()) else
      acquired$receipt$selection$allocations,
    state_order=if(method=="all")
      "Saved-row rank then ascending numeric chain ID; saved rows are not original sampler iterations" else
      "Balanced selection rank then ascending numeric chain ID; exact selected keys are retained separately",
    limitations=if(method=="all")
      "Exact empirical summaries of all validated retained states; no convergence or inference-quality guarantee" else
      "Exact empirical summaries and type-7 quantiles of selected states only; no representativeness, full-source quantile, endpoint-error or inference guarantee")
  .bh_individual_selection_check(selection,keys,function(message)
    .bh_individual_fail(message,"E_INDIVIDUAL_SOURCE"))
  selection
}

.bh_individual_saved_selection_rank <- function(rank,allocation,L,fail) {
  if(allocation==1)return(floor((L+1)/2))
  if(rank-1>floor((2^53-1)/(L-1)))fail("saved selection index arithmetic is not exactly representable")
  product<-(rank-1)*(L-1)
  if(product>2^53-1)fail("saved selection index arithmetic is not exactly representable")
  denominator<-allocation-1
  quotient<-floor(product/denominator)
  if(quotient>floor((2^53-1)/denominator))fail("saved selection quotient arithmetic is not exactly representable")
  check<-quotient*denominator
  if(check>2^53-1)fail("saved selection quotient arithmetic is not exactly representable")
  # Match the parser's exact quotient check, including a division rounded up to
  # an integer. This local arithmetic does not access the parser or any source.
  if(check>product)quotient<-quotient-1
  1+quotient
}

# A local saved-record check: no reader, policy resolver, provider or source path.
.bh_individual_selection_check <- function(selection,keys,fail) {
  plain_list<-function(x)typeof(x)=="list"&&!is.object(x)&&!isS4(x)&&
    all(names(attributes(x))%in%"names")&&typeof(names(x))=="character"&&
    !is.object(names(x))&&is.null(attributes(names(x)))&&!anyNA(names(x))&&
    !anyDuplicated(names(x))&&all(nzchar(names(x)))
  number<-function(x)typeof(x)%in%c("integer","double")&&!is.object(x)&&
    is.null(attributes(x))&&length(x)==1L&&is.finite(x)&&x>=0&&
    x==floor(x)&&x<=.Machine$integer.max
  required<-c("version","method","requested_states","full_source_rows","selected_rows","omitted_rows","per_chain")
  if(!plain_list(selection)||!all(required%in%names(selection)))
    fail("saved selection requires its exact version, method, request, counts and allocation fields")
  for(name in required)if(identical(.subset2(selection,name),quote(expr=)))
    fail(paste("saved selection field must contain an explicit value:",name))
  if(!identical(selection[["version",exact=TRUE]],"bmeas_individual_selection_v1")||
      !identical(selection[["method",exact=TRUE]],"all")&&!identical(selection[["method",exact=TRUE]],"balanced"))
    fail("saved selection version or method is invalid")
  for(name in c("full_source_rows","selected_rows","omitted_rows"))
    if(!number(selection[[name,exact=TRUE]]))fail(paste("saved selection count is invalid:",name))
  if(selection[["selected_rows",exact=TRUE]]==1)
    fail("nonempty individual summaries require at least two selected states")
  request<-selection[["requested_states",exact=TRUE]]
  if(identical(selection[["method",exact=TRUE]],"all")) {
    if(!identical(request,"all"))fail("saved all-state selection requires the exact all-state request")
  } else {
    if(!plain_list(request)||length(request)!=2L||!setequal(names(request),c("method","draws")))
      fail("saved balanced request requires a plain exact method/draws record")
    for(name in c("method","draws"))if(identical(.subset2(request,name),quote(expr=)))
      fail(paste("saved request field must contain an explicit value:",name))
    if(!identical(request[["method",exact=TRUE]],"balanced")||
        !number(request[["draws",exact=TRUE]])||request[["draws",exact=TRUE]]<2)
      fail("saved balanced request needs its method and a supported integer draw count of at least two")
    if(selection[["selected_rows",exact=TRUE]]>0&&request[["draws",exact=TRUE]]!=selection[["selected_rows",exact=TRUE]])
      fail("saved balanced request count differs from the selected empirical count")
    if(selection[["selected_rows",exact=TRUE]]==0&&
        (selection[["full_source_rows",exact=TRUE]]!=0||selection[["omitted_rows",exact=TRUE]]!=0))
      fail("an unexecuted balanced request requires an empty source record")
  }
  if(selection[["selected_rows",exact=TRUE]]>selection[["full_source_rows",exact=TRUE]]||
      selection[["omitted_rows",exact=TRUE]]!=selection[["full_source_rows",exact=TRUE]]-selection[["selected_rows",exact=TRUE]]||
      (selection[["method",exact=TRUE]]=="all"&&selection[["omitted_rows",exact=TRUE]]!=0))
    fail("saved full-source and selected-state counts disagree")
  frame<-function(x,fields,label) {
    if(typeof(x)!="list"||isS4(x)||!identical(attr(x,"class",exact=TRUE),"data.frame")||
        any(!names(attributes(x))%in%c("names","class","row.names")))fail(paste(label,"must be plain"))
    columns<-attr(x,"names",exact=TRUE)
    if(typeof(columns)!="character"||is.object(columns)||!is.null(attributes(columns))||
        anyNA(columns)||anyDuplicated(columns)||!identical(columns,fields))fail(paste(label,"columns disagree"))
    row_names<-attr(x,"row.names",exact=TRUE)
    if(!typeof(row_names)%in%c("integer","character")||is.object(row_names)||!is.null(attributes(row_names)))
      fail(paste(label,"row names are malformed"))
    n<-.row_names_info(x,2L)
    if(!number(n))fail(paste(label,"row count is invalid"))
    for(value in unclass(x))if(!typeof(value)%in%c("integer","double")||is.object(value)||
        !is.null(attributes(value))||length(value)!=n||anyNA(value)||any(!is.finite(value))||
        any(value<0|value!=floor(value)|value>.Machine$integer.max))fail(paste(label,"has invalid numeric columns"))
    x
  }
  keys<-frame(keys,c("chain_id","saved_row"),"selected keys")
  allocation<-frame(selection[["per_chain",exact=TRUE]],c("chain_id","source_rows","selected_rows","omitted_rows"),"per-chain allocation")
  if(nrow(keys)!=selection[["selected_rows",exact=TRUE]]||any(keys$chain_id<1|keys$saved_row<1)||anyDuplicated(keys)||
      anyDuplicated(allocation$chain_id)||any(allocation$chain_id<1)||
      is.unsorted(allocation$chain_id,strictly=TRUE)||
      any(allocation$source_rows<1|allocation$selected_rows<1)||
      any(allocation$selected_rows>allocation$source_rows)||
      any(allocation$omitted_rows!=allocation$source_rows-allocation$selected_rows)||
      sum(as.double(allocation$source_rows))!=selection[["full_source_rows",exact=TRUE]]||
      sum(as.double(allocation$selected_rows))!=selection[["selected_rows",exact=TRUE]])
    fail("saved selected keys and per-chain source counts disagree")
  if(nrow(keys)) {
    chain<-match(keys$chain_id,allocation$chain_id)
    if(anyNA(chain)||any(keys$saved_row>allocation$source_rows[chain])||
        !identical(as.double(tabulate(chain,nbins=nrow(allocation))),as.double(allocation$selected_rows)))
      fail("saved selected keys do not match per-chain allocations")
    C<-nrow(allocation);L<-allocation$source_rows[1L];D<-selection[["selected_rows",exact=TRUE]]
    if(any(allocation$source_rows!=L))fail("saved selection requires equal complete chain layouts")
    expected_allocation<-rep(floor(D/C),C)
    if(D%%C)expected_allocation[seq_len(D%%C)]<-expected_allocation[seq_len(D%%C)]+1
    if(any(allocation$selected_rows!=expected_allocation))fail("saved per-chain allocation is not balanced")
    rank<-1;position<-1
    while(rank<=max(expected_allocation)) {
      c<-1
      while(c<=C) {
        a<-expected_allocation[c]
        if(rank<=a) {
          saved<-if(selection[["method",exact=TRUE]]=="all")rank else .bh_individual_saved_selection_rank(rank,a,L,fail)
          if(keys$chain_id[position]!=allocation$chain_id[c]||keys$saved_row[position]!=saved)
            fail("saved keys disagree with the declared deterministic selection order")
          position<-position+1
        }
        c<-c+1
      }
      rank<-rank+1
    }
  }
  # A structurally empty balanced request is unexecuted. The passive result
  # view must additionally establish its no-predictor/no-acquisition context.
  invisible(!(identical(selection[["method",exact=TRUE]],"balanced")&&selection[["selected_rows",exact=TRUE]]==0))
}

.bh_individual_result_view <- function(x) {
  fail <- function(message) .mp_abort("E_INDIVIDUAL_RESULT", message)
  plain_list <- function(value, label) {
    if (typeof(value) != "list" || is.object(value) ||
        any(!names(attributes(value)) %in% "names"))
      fail(paste(label, "must be plain saved metadata"))
    keys <- attr(value, "names", exact = TRUE)
    if (typeof(keys) != "character" || is.object(keys) || !is.null(attributes(keys)) ||
        length(keys) != length(value) || anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys))
      fail(paste(label, "has ambiguous fields"))
    value
  }
  text <- function(value, label, scalar = TRUE) {
    if (typeof(value) != "character" || is.object(value) || !is.null(attributes(value)) ||
        (scalar && length(value) != 1L) || anyNA(value))
      fail(paste(label, "requires plain saved text"))
    value
  }
  if (typeof(x) != "list" || isS4(x) ||
      !identical(attr(x, "class", exact = TRUE), c("bmeas_individual_parameter_summary", "data.frame")) ||
      !is.null(attr(x, "dim", exact = TRUE)))
    fail("expected a plain saved individual-parameter result table")
  keys <- attr(x, "names", exact = TRUE)
  text(keys, "result columns", FALSE)
  if (length(keys) != length(unclass(x)) || any(!nzchar(keys)) || anyDuplicated(keys))
    fail("result columns are ambiguous")
  rows <- attr(x, "row.names", exact = TRUE)
  if (!typeof(rows) %in% c("integer", "character") || is.object(rows) || !is.null(attributes(rows)))
    fail("result row names must be plain saved indices")
  n <- .row_names_info(x, 2L)
  if (length(n) != 1L || is.na(n) || n < 0L || n > .Machine$integer.max)
    fail("result row count exceeds the saved table shape")
  visible <- c("person_key", "parameter_key", "scale", "mean", "median", "sd", "lower90", "upper90", "status")
  if (!all(visible %in% keys)) fail("result lacks required display columns")
  values <- unclass(x)
  for (key in visible) {
    value <- values[[key, exact = TRUE]]
    type <- if (key %in% c("mean", "median", "sd", "lower90", "upper90")) c("integer", "double") else "character"
    if (!typeof(value) %in% type || is.object(value) || !is.null(attributes(value)) || length(value) != n)
      fail(paste("result display column is malformed:", key))
  }
  metadata <- plain_list(attr(x, "metadata", exact = TRUE), "result metadata")
  work <- plain_list(attr(x, "work", exact = TRUE), "result work")
  required_fields <- function(value, fields, label) {
    if(!all(fields%in%names(value)))fail(paste(label,"lacks exact required fields"))
    for(field in fields)if(identical(.subset2(value,field),quote(expr=)))
      fail(paste(label,"field must contain an explicit value:",field))
    invisible(NULL)
  }
  required_fields(metadata,c("version","status","requested_scales"),"result metadata")
  version <- text(metadata[["version",exact=TRUE]],"result version")
  if(!version%in%c("bmeas_individual_parameter_summary_v1","bmeas_individual_parameter_summary_v2"))
    fail("unsupported saved result version")
  streaming <- identical(version,"bmeas_individual_parameter_summary_v2")
  required_fields(work,c("logical_parameter_cells","retained_source_rows",
    if(streaming)"selected_source_rows"),"result work")
  if(streaming)required_fields(metadata,"selection","result metadata")
  request_executed <- TRUE
  if (!streaming && n > 3000L) fail("result row count exceeds the saved table shape")
  text(metadata[["status",exact=TRUE]], "result status")
  if (!metadata[["status",exact=TRUE]] %in% c("computed", "computed_with_unavailable_summaries", "not_applicable_no_predictor_parameters"))
    fail("unsupported saved result status")
  scales <- text(metadata[["requested_scales",exact=TRUE]], "requested scales", FALSE)
  if (!length(scales) || length(scales) > 3L || anyDuplicated(scales) ||
      any(!scales %in% c("predictor", "natural", "received")))
    fail("saved scale views are invalid")
  if (!is.null(metadata[["source_qualification",exact=TRUE]])) text(metadata[["source_qualification",exact=TRUE]], "source qualification")
  for (key in c("logical_parameter_cells", "retained_source_rows")) {
    value <- work[[key, exact = TRUE]]
    ceiling <- if (streaming) .Machine$integer.max else if (key == "logical_parameter_cells") 1000L else 100L
    if (!typeof(value) %in% c("integer", "double") || is.object(value) || !is.null(attributes(value)) ||
        length(value) != 1L || !is.finite(value) || value != floor(value) || value < 0L || value > ceiling)
      fail(paste("saved work field is invalid:", key))
  }
  if (streaming) {
    request_executed <- .bh_individual_selection_check(metadata[["selection",exact=TRUE]],attr(x,"draw_keys",exact=TRUE),fail)
    selection <- metadata[["selection",exact=TRUE]]
    if(selection[["selected_rows",exact=TRUE]]==0) {
      no_source <- plain_list(attr(x,"source_receipt",exact=TRUE),"empty-result source receipt")
      if(n!=0L||work[["logical_parameter_cells",exact=TRUE]]!=0||
          !identical(metadata[["status",exact=TRUE]],"not_applicable_no_predictor_parameters")||
          !identical(.subset2(no_source,"status"),"not_needed")||
          .row_names_info(attr(x,"draw_keys",exact=TRUE),2L)!=0L||
          .row_names_info(selection[["per_chain",exact=TRUE]],2L)!=0L)
        fail("an empty unexecuted selection requires no predictor rows or cells, not-applicable status and a no-acquisition receipt")
    }
    if (work[["retained_source_rows",exact=TRUE]] != selection[["full_source_rows",exact=TRUE]] ||
        !typeof(work[["selected_source_rows",exact=TRUE]])%in%c("integer","double") || is.object(work[["selected_source_rows",exact=TRUE]]) ||
        !is.null(attributes(work[["selected_source_rows",exact=TRUE]])) || length(work[["selected_source_rows",exact=TRUE]])!=1L ||
        is.na(work[["selected_source_rows",exact=TRUE]]) || work[["selected_source_rows",exact=TRUE]] != selection[["selected_rows",exact=TRUE]] ||
        as.double(work[["logical_parameter_cells",exact=TRUE]])*length(scales)!=n)
      fail("saved work, complete output and selected-state counts disagree")
    denominator <- values[["n_draws",exact=TRUE]]
    if (!typeof(denominator)%in%c("integer","double") || is.object(denominator) ||
        !is.null(attributes(denominator)) || length(denominator)!=n || anyNA(denominator) ||
        any(denominator!=selection[["selected_rows",exact=TRUE]]))fail("saved summary denominator differs from selected states")
  }
  plain <- structure(values[visible], class = "data.frame", row.names = .set_row_names(n))
  list(metadata = metadata, work = work, table = plain, selection_request_executed = request_executed)
}

print.bmeas_individual_parameter_summary <- function(x, ...) {
  view <- .bh_individual_result_view(x)
  metadata <- view$metadata
  work <- view$work
  display_rows <- .row_names_info(view$table, 2L)
  cat("bmeas individual parameter summaries\n")
  cat("  Status:", metadata[["status",exact=TRUE]], "\n")
  cat("  Logical person-parameter cells:", work[["logical_parameter_cells",exact=TRUE]],
      "; scale views:", paste(metadata[["requested_scales",exact=TRUE]], collapse = ", "), "\n")
  cat("  Retained source states:", work[["retained_source_rows",exact=TRUE]],
      "; summary rows:", display_rows, "\n")
  if (identical(metadata[["version",exact=TRUE]],"bmeas_individual_parameter_summary_v2")) {
    cat("  Selected empirical states:", metadata[["selection",exact=TRUE]][["selected_rows",exact=TRUE]],
      "; omitted states:", metadata[["selection",exact=TRUE]][["omitted_rows",exact=TRUE]],
      "; selection:", metadata[["selection",exact=TRUE]][["method",exact=TRUE]], "\n")
    if(!view$selection_request_executed)
      cat("  Requested balanced states:", metadata[["selection",exact=TRUE]][["requested_states",exact=TRUE]][["draws",exact=TRUE]],
        "; request unexecuted because there are no predictor parameters.\n")
  }
  if (!is.null(metadata[["source_qualification",exact=TRUE]])) {
    qualification <- metadata[["source_qualification",exact=TRUE]]
    if (is.character(qualification) && !is.object(qualification) &&
        is.null(dim(qualification)) && length(qualification) == 1L && !is.na(qualification))
      cat("  Saved source qualification:", qualification, "\n")
  }
  if (nrow(view$table)) {
    plain <- view$table
    print(plain[seq_len(min(12L, nrow(plain))), , drop = FALSE], row.names = FALSE)
    if (display_rows > 12L) cat("  First 12 rows shown; the returned table contains all rows.\n")
  }
  cat("  Fixed values and shared aliases are labelled in the full table.\n")
  cat("  No new convergence diagnostics or inference-quality verdict. Reading this result runs no extraction or simulation.\n")
  invisible(x)
}
