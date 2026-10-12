# Explicit all-retained-state diagnostic refresh. Passive reports never enter
# this source-acquisition transaction. No fitting or predictive simulation.
.bh_diagnostic_refresh_budget <- function(budget) {
  budget <- .bh_diag_list(budget, "diagnostic refresh budget")
  required <- c("posterior", "max_monitors", "max_metric_rows", "max_fft_length")
  optional <- c("max_catalogue_rows", "max_context_rows")
  fields <- names(budget)
  if (!all(required %in% fields) || any(!fields %in% c(required, optional)))
    .bh_diag_error("refresh budget requires posterior, max_monitors, max_metric_rows and max_fft_length; only catalogue/context limits are optional", "E_DIAGNOSTIC_BUDGET")
  for (name in c("max_monitors", "max_fft_length"))
    budget[[name]] <- .bh_diag_count(.subset2(budget, name), name, 0, .Machine$integer.max)
  budget$max_metric_rows <- .bh_diag_count(.subset2(budget, "max_metric_rows"), "max_metric_rows", 1, .Machine$integer.max)
  if (!"max_catalogue_rows" %in% fields) budget$max_catalogue_rows <- 10000
  if (!"max_context_rows" %in% fields) budget$max_context_rows <- 100000
  budget$max_catalogue_rows <- .bh_diag_count(budget$max_catalogue_rows, "max_catalogue_rows", 0, .Machine$integer.max)
  budget$max_context_rows <- .bh_diag_count(budget$max_context_rows, "max_context_rows", 1, .Machine$integer.max)
  posterior <- .bh_diag_list(.subset2(budget, "posterior"), "posterior budget")
  .bh_posterior_policy(posterior, character())
  budget$posterior <- posterior
  budget
}
.bh_diagnostic_refresh_string <- function(value, label) {
  if (!.bh_diag_plain(value, "character") || length(value) != 1L ||
      is.na(value) || !validEnc(value) || !nzchar(value))
    .bh_diag_error(paste(label, "must be one retained plain string"))
  value
}
.bh_diagnostic_refresh_expected_chains <- function(fit) {
  view <- .bh_diag_list(fit, "fit", if(.bh_pf_is_view(fit))"bmeas_portable_consumer_view"else c("bmeas_fit", "bhmm_fit"))
  counts <- numeric(); sources <- character()
  add <- function(value, label) {
    if (is.null(value)) return(invisible(NULL))
    counts <<- c(counts, .bh_diag_count(value,label,1,.Machine$integer.max))
    sources <<- c(sources,label)
    invisible(NULL)
  }
  run <- .bh_diag_field(view,"run",FALSE)
  if (!is.null(run)) {
    run <- .bh_diag_list(run,"saved run","measurement_run_config")
    add(.bh_diag_field(run,"chains",FALSE),"saved run configured chains")
  }
  receipt <- .bh_diag_field(view,"execution_receipt",FALSE)
  if (!is.null(receipt)) {
    receipt <- .bh_diag_list(receipt,"saved execution receipt")
    if (identical(.bh_diag_field(receipt,"version",FALSE),"bmeas_execution_receipt_v1")) {
      sampling <- .bh_diag_field(receipt,"sampling",FALSE)
      if (!is.null(sampling)) {
        sampling <- .bh_diag_list(sampling,"saved sampling receipt")
        add(.bh_diag_field(sampling,"configured_chains",FALSE),"saved execution receipt configured chains")
      }
    }
  }
  if (length(base::unique.default(counts))>1L)
    .bh_diag_error("retained expected chain counts contradict one another","E_DIAGNOSTIC_IDENTITY")
  if (!length(counts)) return(list(value=NULL,availability="unavailable_in_saved_context",source=character(),
    reason="No usable saved expected chain count; every recorded source path remains required"))
  list(value=unname(counts[1L]),availability="retained",source=sources,
    reason="Saved configured chain count; actual IDs and rows still require full source validation")
}
.bh_diagnostic_refresh_validate_spec <- function(spec) {
  original <- .bh_diag_list(spec,"specification",c("bmeas_spec","bhmm_spec"))
  saved <- .bh_diag_field(original,"fingerprint")
  original$fingerprint <- NULL
  saved_attributes <- attributes(spec); saved_attributes$names <- names(original)
  attributes(original) <- saved_attributes
  if (!identical(saved,.mp_hash(original)))
    .bh_diag_error("prepared context changed or uses an incompatible serialization; historical inputs are not resealed","E_SPEC_CHANGED")
  invisible(NULL)
}
.bh_diagnostic_catalogue_buffers <- function(catalogue, selected) {
  K <- as.double(.row_names_info(catalogue,2L))
  M <- as.double(.row_names_info(selected,2L))
  multiply <- .bh_diagnostic_metric_mul
  # Metadata has its own prior row admission. These explicit newly created
  # buffers are then charged with the posterior phase before any source read.
  # Shared catalogue metadata is counted once, not once per referring object.
  c(complete_catalogue_logical_columns=multiply(3,K,"catalogue logical columns"),
    catalogue_row_record_construction=multiply(3,K,"catalogue construction flags"),
    shared_catalogue_numeric_metadata=.bh_posterior_numeric_payload(attr(catalogue,"diagnostic_catalogue",exact=TRUE)),
    catalogue_stored_row_names=if(K)2 else 0,
    catalogue_row_name_expansion=K,
    selected_row_name_storage=M,
    selection_index_overlap=M,
    selected_row_record_construction=multiply(3,M,"selection construction flags"))
}
.bh_diagnostic_refresh_plan <- function(fit, parameters, budget) {
  cap <- .bh_diagnostic_refresh_budget(budget)
  catalogue <- .bh_diagnostic_catalogue(fit, list(max_rows=cap$max_catalogue_rows,
    max_context_rows=cap$max_context_rows))
  selected <- .bh_diagnostic_select(catalogue, parameters)
  M <- .row_names_info(selected, 2L)
  if (M > cap$max_monitors)
    .bh_diag_error("requested population monitors exceed max_monitors", "E_DIAGNOSTIC_MONITORS")
  view <- .bh_diag_list(fit, "fit", if(.bh_pf_is_view(fit))"bmeas_portable_consumer_view"else c("bmeas_fit", "bhmm_fit"))
  original_spec <- .bh_diag_field(view, "spec")
  spec <- .bh_diag_list(original_spec, "specification", if(.bh_pf_is_view(fit))"bmeas_portable_context"else c("bmeas_spec", "bhmm_spec"))
  provenance <- .bh_diag_list(.bh_diag_field(view, "provenance"), "fit provenance")
  fingerprint <- .bh_diagnostic_refresh_string(.bh_diag_field(spec, "fingerprint"), "specification fingerprint")
  bound <- .bh_diagnostic_refresh_string(.bh_diag_field(provenance, "model_fingerprint"), "fit model fingerprint")
  if (!identical(fingerprint, bound))
    .bh_diag_error("saved fit and specification identities disagree; historical context is not resealed", "E_DIAGNOSTIC_IDENTITY")
  code <- .bh_diagnostic_refresh_string(.bh_diag_field(view, "code"), "retained fitted source")
  paths <- .bh_diag_field(provenance, "output_files")
  if (!.bh_diag_plain(paths, "character") || !length(paths) || anyNA(paths) ||
      any(!validEnc(paths)) || any(!nzchar(paths)) || base::anyDuplicated.default(paths))
    .bh_diag_error("refresh requires every distinct retained output path", "E_DIAGNOSTIC_SOURCE")
  cache <- .bh_diag_field(view, "cache", FALSE)
  if (is.null(cache)) cache <- list() else cache <- .bh_diag_list(cache, "fit cache")
  for (name in c("posterior_propriety", "posterior_normalizability")) {
    node <- .bh_diag_field(cache, name, FALSE)
    if (!is.null(node)) {
      node <- .bh_diag_list(node, paste("saved", name))
      status <- .bh_diag_field(node, "status", FALSE)
      if (!is.null(status)) {
        status <- .bh_diagnostic_refresh_string(status, paste(name, "status"))
        if (status %in% c("proven_improper", "proven_non_normalizable"))
          .bh_diag_error("a proved improper or non-normalizable target cannot supply posterior diagnostic summaries", "E_DIAGNOSTIC_POSTERIOR")
      }
    }
  }
  aliases <- .subset2(unclass(selected), "source_aliases")
  aliases <- unclass(aliases)
  variables <- base::unique.default(unlist(aliases, use.names=FALSE))
  if (!length(variables)) variables <- character()
  groups <- list()
  for (group in aliases) if (length(group) > 1L)
    groups[[length(groups)+1L]] <- as.integer(match(group, variables))
  expected <- .bh_diagnostic_refresh_expected_chains(fit)
  if (!is.null(expected$value) && length(paths) != expected$value)
    .bh_diag_error("recorded file count differs from retained expected chain count", "E_DIAGNOSTIC_SOURCE")
  qualification <- .bh_diag_field(provenance, "kind", FALSE)
  if (is.null(qualification)) qualification <- "unavailable_in_saved_provenance"
  else qualification <- .bh_diagnostic_refresh_string(qualification, "source qualification")
  list(budget=cap, catalogue=catalogue, selected=selected, monitors=M,
    context_buffers=.bh_diagnostic_catalogue_buffers(catalogue,selected),
    variables=variables, alias_groups=groups, output_files=paths,
    expected_chains=expected, original_spec=original_spec, saved_code=code,
    old_cache=cache, source_qualification=qualification,
    identity=list(specification_fingerprint=fingerprint, saved_fit_model_fingerprint=bound,
      scope="Association with saved identities and currently read bytes; original output/executable/data origin is not authenticated"))
}
.bh_diagnostic_summary_table <- function(selected, records) {
  M <- .row_names_info(selected, 2L)
  columns <- unclass(selected)
  out <- list(variable=.subset2(columns,"source_variable"), parameter_key=.subset2(columns,"key"),
    parameter_role=.subset2(columns,"role"), label=.subset2(columns,"label"),
    layer=.subset2(columns,"layer"), units=.subset2(columns,"units"),
    source_kind=.subset2(columns,"source_kind"), prior_coordinate=.subset2(columns,"prior_coordinate"))
  for (name in c("mean","median","sd","q5","q95","rhat","ess_bulk","ess_tail","mcse_mean"))
    out[[name]] <- unname(vapply(records, function(record) .subset2(record$values,name), numeric(1)))
  structure(out, class="data.frame", row.names=.set_row_names(as.integer(M)))
}
.bh_diagnostic_phase_status <- function(records, parameter=FALSE) {
  if (parameter && !length(records)) return("not_requested")
  statuses <- unlist(lapply(records, function(record)
    vapply(record$metrics, function(metric) metric$status, character(1))), use.names=FALSE)
  if (!length(statuses)) return(if (parameter) "not_requested" else "unavailable")
  available <- statuses == "available"
  if (parameter) return(if (all(available)) "computed" else "computed_partial")
  if (all(statuses == "not_applicable")) return("not_applicable")
  if (all(available | statuses == "not_applicable")) return("computed")
  if (any(available)) "partial" else "unavailable"
}
.bh_diagnostic_refresh_compute <- function(fit, plan, context_validation=NULL, acquisition_paths=NULL, archive_hashes=NULL) {
  portable<-.bh_pf_active_route(fit,context_validation,"refresh_diagnostics")
  # Metadata budgets precede retained-context serialization. It is separate
  # from posterior CSV bytes; no new declaration/source generation is invoked.
  if(!portable) .bh_diagnostic_refresh_validate_spec(plan$original_spec)
  profile <- .bh_diagnostic_metric_profile(plan$monitors > 0)
  acquired <- .bh_diagnostic_ingest_csv(if(is.null(acquisition_paths))plan$output_files else acquisition_paths, plan$variables,
    plan$budget$posterior, monitors=plan$monitors,
    max_metric_rows=plan$budget$max_metric_rows, max_monitors=plan$budget$max_monitors,
    max_fft_length=plan$budget$max_fft_length, alias_groups=plan$alias_groups,
    expected_model="scalar_components_model", expected_chains=plan$expected_chains$value,
    context_buffers=plan$context_buffers, archive_hashes=archive_hashes)
  receipt <- .bh_pf_context_attach(acquired$receipt,context_validation)
  tryCatch({
    dimensions <- acquired$diagnostic$dimensions
    C <- dimensions$chains; L <- dimensions$rows_per_chain; D <- dimensions$rows
    M <- plan$monitors
    receipt$expected_chain_context <- plan$expected_chains
    receipt$bound_model_identity <- plan$identity
    if(portable) receipt$portable_bound_identity<-context_validation$identity else {
      receipt$saved_source_code_hash <- .mp_hash(plan$saved_code)
      receipt$selected_monitor_map_hash <- .mp_hash(plan$selected)
    }
    receipt$source_qualification <- plan$source_qualification
    receipt$non_csv_work <- if(portable) "Bounded immutable canonical validation is reported separately in context_validation; no legacy serialization seal is recomputed" else "Saved specification validation, retained source and selected-map hashing/serialization are outside CSV byte counters"
    selected_columns <- unclass(plan$selected)
    selected_keys <- .subset2(selected_columns,"key")
    at <- match(.subset2(selected_columns,"source_variable"), colnames(acquired$draws))
    if (anyNA(at)) .bh_diag_error("acquired source lacks a resolved canonical population column", "E_DIAGNOSTIC_SOURCE")
    parameter_metrics <- vector("list", M)
    if (M) names(parameter_metrics) <- selected_keys
    for (i in seq_len(M)) {
      x <- matrix(acquired$draws[,at[[i]]], nrow=L, ncol=C, byrow=TRUE)
      parameter_metrics[[i]] <- .bh_diagnostic_parameter_metrics(x,profile)
    }
    parameters <- .bh_diagnostic_summary_table(plan$selected, parameter_metrics)
    chain_metadata <- acquired$diagnostic$chain_metadata
    ids <- unname(vapply(chain_metadata, function(meta) meta$id, numeric(1)))
    hmc_metrics <- vector("list",C); names(hmc_metrics) <- base::as.character.default(ids)
    sampler <- match(c("divergent__","treedepth__","energy__"),colnames(acquired$draws))
    for (j in seq_len(C)) {
      rows <- seq.int(j,D,by=C)
      column <- function(k) if (is.na(sampler[[k]])) NULL else acquired$draws[rows,sampler[[k]]]
      meta <- chain_metadata[[j]]
      hmc_metrics[[j]] <- .bh_diagnostic_hmc_metrics(column(1L),column(2L),column(3L),
        L=L,thin=meta$thin,engine=meta$engine,max_depth=meta$max_depth)
    }
    hmc <- list(chain_id=ids)
    for (name in c("num_divergent","fraction_divergent","num_max_treedepth","fraction_max_treedepth","ebfmi"))
      hmc[[name]] <- unname(vapply(hmc_metrics,function(record).subset2(record$values,name),numeric(1)))
    parameter_status <- .bh_diagnostic_phase_status(parameter_metrics,TRUE)
    hmc_status <- .bh_diagnostic_phase_status(hmc_metrics,FALSE)
    phases <- c(list(parameter_summary=list(status=parameter_status,
      reason=if(M) "Selected population monitors only; undefined metrics remain locally unavailable" else "No parameter monitors requested; the prior table is not retained"),
      hmc=list(status=hmc_status,reason="All retained rows per original chain; optional absence and undefined metrics are explicit")),
      .bh_diagnostic_retained_phases(plan$old_cache))
    catalogue_info <- attr(plan$catalogue,"diagnostic_catalogue",exact=TRUE)
    estimator <- list(id=profile$id,required=profile$required,versions=profile$versions,
      bindings=if(M) .bh_diagnostic_metric_binding_provenance() else NULL)
    reporting <- list(version="bmeas_posterior_reporting_v2",
      reason="Explicit all-retained-state diagnostic refresh; no inference or all-variable convergence verdict",
      phases=phases, selection=list(selectable_keys=.subset2(unclass(plan$catalogue),"key"),
        selected_keys=selected_keys, excluded=catalogue_info$exclusions,
        scope="Selected supported population monitors; equality aliases are not extra independent monitors"),
      observed=list(chains=C,rows_per_chain=L,rows=D,chain_ids=ids,
        split_chains=dimensions$split_chains,split_rows_per_chain=dimensions$half_rows,
        split_rows=dimensions$split_rows,split_omitted_rows=dimensions$omitted_rows),
      parameter_metrics=parameter_metrics,hmc_metrics=hmc_metrics,
      provenance=list(source=receipt,estimator=estimator,
        budget=list(requested=plan$budget,metric_admission=dimensions,source_admission=receipt$budget)))
    # Assign only after all source/metric/report construction has succeeded.
    # R copy-on-modify keeps the caller's object and backend handle untouched.
    cache <- plan$old_cache
    cache$parameter_summary <- parameters; cache$hmc <- hmc
    cache$posterior_reporting <- reporting
    old_metadata <- .bh_diag_field(cache,"report_metadata",FALSE)
    metadata <- if (is.null(old_metadata)) list() else tryCatch(
      .bh_diag_list(old_metadata,"old report metadata"),error=function(e)list(previous_unavailable="Malformed old report metadata is not used for new summaries"))
    metadata$summary_method <- list(status="computed_selected_monitors",method=if(M) "posterior 1.7.0 concrete estimators; empirical type-7 q5/q95" else "No parameter estimators requested or called",scope="all observed states of selected semantic keys",parameter_keys=selected_keys)
    metadata$interval_columns <- c("q5","q95")
    cache$report_metadata <- metadata
    answer <- unclass(fit); answer$cache <- cache
    attrs <- attributes(fit); attrs$names <- names(answer); attributes(answer) <- attrs
    answer
  }, error=function(e) {
    if (inherits(e,"measurement_error")) stop(.bh_diagnostic_source_condition(e,receipt))
    message <- if (typeof(e)=="list") .subset2(unclass(e),"message") else NULL
    if (!.bh_diag_plain(message,"character") || length(message)!=1L || is.na(message))
      message <- "diagnostic refresh failed during off-object computation"
    .bh_posterior_abort("E_DIAGNOSTIC_REFRESH",message,receipt)
  })
}
refresh_diagnostics <- function(fit, parameters, budget) {
  if (missing(fit) || missing(parameters) || missing(budget))
    .bh_diag_error("fit, explicit parameter keys (or character()) and a finite refresh budget are required", "E_DIAGNOSTIC_ARGUMENT")
  if(inherits(fit,"bmeas_fit_archive")) return(.bh_archive_refresh(fit,parameters,budget))
  .bh_pf_public_input(fit,"refresh_diagnostics")
  if(.bh_pf_is_portable(fit)) {
    validated<-.bh_pf_active(fit,"refresh_diagnostics")
    return(.bh_pf_after_validation(validated,function() {
      plan<-.bh_diagnostic_refresh_plan(validated$view,parameters,budget)
      .bh_pf_commit_cache(fit,.bh_diagnostic_refresh_compute(validated$view,plan,validated$receipt))
    }))
  }
  plan <- .bh_diagnostic_refresh_plan(fit,parameters,budget)
  .bh_diagnostic_refresh_compute(fit,plan)
}
