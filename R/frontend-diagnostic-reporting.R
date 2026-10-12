# Passive projection of the optional explicit-refresh record. Every accessor is
# exact; validation visits saved values only and never calls a provider, a saved
# function, a model helper, a namespace loader or a posterior source reader.
.bh_diagnostic_report_numeric <- function(x, n = 1L) {
  typeof(x) %in% c("integer", "double") && !is.object(x) &&
    is.null(dim(x)) && length(x) == n
}
.bh_diagnostic_report_count <- function(x, minimum = 0, maximum = 2^53 - 1) {
  .bh_diagnostic_report_numeric(x) && !is.na(x) && is.finite(x) &&
    x == floor(x) && x >= minimum && x <= maximum
}
.bh_diagnostic_report_keys <- function(x, unique = TRUE) {
  typeof(x) == "character" && !is.object(x) && is.null(dim(x)) &&
    !anyNA(x) && all(validEnc(x)) && all(nzchar(x)) &&
    (!unique || !base::anyDuplicated.default(x))
}
.bh_diagnostic_report_logical <- function(x, n = 1L, missing = FALSE) {
  typeof(x) == "logical" && !is.object(x) && is.null(dim(x)) &&
    length(x) == n && (missing || !anyNA(x))
}
.bh_diagnostic_report_invalid <- function(label) list(
  availability = "unavailable_invalid_metadata", record = NULL,
  reason = paste(label, "is malformed or inconsistent; unrelated saved evidence is preserved"))
.bh_diagnostic_report_node <- function(x, validate, label) {
  if (is.null(x)) return(list(availability = "unavailable_in_cache", record = NULL,
    reason = paste(label, "was not retained")))
  valid <- tryCatch({
    .bh_fit_report_context_plain(x, label)
    isTRUE(validate(x))
  }, error = function(e) FALSE)
  if (!valid) return(.bh_diagnostic_report_invalid(label))
  list(availability = "retained", record = x,
    reason = "Saved explicit-refresh evidence; no recalculation performed")
}
.bh_diagnostic_report_retained <- function(node) identical(
  .bh_saved_field(node, "availability"), "retained")
.bh_diagnostic_report_value <- function(node) {
  if (.bh_diagnostic_report_retained(node)) .bh_saved_field(node, "record") else NULL
}
.bh_diagnostic_reporting_v2_header <- function(x) .bh_saved_named_list(x) &&
  identical(.bh_saved_field(x, "version"), "bmeas_posterior_reporting_v2")

.bh_diagnostic_report_selection <- function(x, parameters) {
  if (!.bh_saved_named_list(x)) return(FALSE)
  selectable <- .bh_saved_field(x, "selectable_keys")
  selected <- .bh_saved_field(x, "selected_keys")
  excluded <- .bh_saved_field(x, "excluded")
  if (!.bh_diagnostic_report_keys(selectable) || !.bh_diagnostic_report_keys(selected) ||
      !all(selected %in% selectable) || !.bh_saved_string(.bh_saved_field(x, "scope")) ||
      !.bh_saved_named_list(excluded) ||
      length(excluded) != 4L ||
      !all(c("fixed", "share_weights", "person_and_basis", "other_derived") %in% names(excluded)))
    return(FALSE)
  for (name in names(excluded)) {
    item <- .bh_saved_field(excluded, name)
    count <- .bh_saved_field(item, "known_rows")
    if (!.bh_saved_named_list(item) || !.bh_saved_string(.bh_saved_field(item, "status")) ||
        !.bh_diagnostic_report_numeric(count) || is.nan(count) ||
        (!is.na(count) && !.bh_diagnostic_report_count(count))) return(FALSE)
  }
  if (!identical(attr(parameters, "class", exact = TRUE), "data.frame")) return(FALSE)
  keys <- .bh_saved_field(parameters, "parameter_key")
  .bh_diagnostic_report_keys(keys) && length(keys) == .row_names_info(parameters, 2L) &&
    identical(unname(keys), unname(selected))
}
.bh_diagnostic_report_observed <- function(x) {
  if (!.bh_saved_named_list(x)) return(FALSE)
  counts <- c("chains", "rows_per_chain", "rows", "split_chains",
    "split_rows_per_chain", "split_rows", "split_omitted_rows")
  if (!all(vapply(counts, function(n) .bh_diagnostic_report_count(
      .bh_saved_field(x, n), if (n %in% c("chains", "rows_per_chain", "rows")) 1 else 0,
      if (n == "split_chains") 2 * .Machine$integer.max else .Machine$integer.max), logical(1)))) return(FALSE)
  C <- .bh_saved_field(x, "chains"); L <- .bh_saved_field(x, "rows_per_chain")
  D <- .bh_saved_field(x, "rows"); ids <- .bh_saved_field(x, "chain_ids")
  if (C > .Machine$integer.max / L ||
      !.bh_diagnostic_report_numeric(ids, C) || anyNA(ids) ||
      any(!is.finite(ids)) || any(ids < 1 | ids != floor(ids) | ids > .Machine$integer.max) ||
      base::anyDuplicated.default(ids) || (length(ids) > 1L && any(base::diff.default(ids) <= 0))) return(FALSE)
  h <- floor(L / 2)
  D == C * L && .bh_saved_field(x, "split_chains") == 2 * C &&
    .bh_saved_field(x, "split_rows_per_chain") == h &&
    .bh_saved_field(x, "split_rows") == 2 * C * h &&
    .bh_saved_field(x, "split_omitted_rows") == D - 2 * C * h
}
.bh_diagnostic_report_phase <- function(x, name) {
  if (!.bh_saved_named_list(x) || !.bh_saved_string(.bh_saved_field(x, "reason"))) return(FALSE)
  status <- .bh_saved_field(x, "status")
  permitted <- switch(name,
    parameter_summary = c("computed", "computed_partial", "not_requested"),
    hmc = c("computed", "partial", "unavailable", "not_applicable"),
    c("retained_evidence", "deferred_by_policy", "uncomputed_output_resource",
      "uncomputed", "unavailable"))
  .bh_saved_string(status) && status %in% permitted
}
.bh_diagnostic_report_phase_state <- function(value, name, history = NULL) {
  valid <- !is.null(value) && isTRUE(tryCatch({
    .bh_fit_report_context_plain(value, name); TRUE }, error = function(e) FALSE))
  if (!valid) {
    historical <- is.null(value) && isTRUE(tryCatch({
      .bh_fit_report_context_plain(history, "historical phase provenance")
      .bh_valid_posterior_reporting(history) }, error = function(e) FALSE))
    return(if (historical) .bh_saved_field(history, "status") else "unavailable")
  }
  status <- .bh_saved_field(value, "status")
  if (.bh_saved_string(status) && status %in%
      c("deferred_by_policy", "uncomputed_output_resource", "uncomputed")) return(status)
  if (name == "numerical_messages") {
    numerical <- .bh_report_numerical(list(availability = "retained", record = value))
    return(if (.bh_diagnostic_report_retained(.bh_saved_field(numerical, "recognized_lines")))
      "retained_evidence" else "unavailable")
  }
  computed <- switch(name, conditioning = c("screened_not_proven", "warning"),
    availability_and_covariance = c("available", "unavailable_values"), expanded_summary = "available")
  if (.bh_saved_string(status) && status %in% computed) "retained_evidence" else "unavailable"
}
.bh_diagnostic_report_saved_phase <- function(x, name, cache) {
  if (!.bh_diagnostic_report_phase(x, name)) return(FALSE)
  fields <- c(conditioning = "admissibility", availability_and_covariance = "optional_reports",
    expanded_summary = "expanded_correlation_matrices", numerical_messages = "numerical_evaluations")
  if (!name %in% names(fields)) return(TRUE)
  if (!identical(.bh_saved_field(x, "source"), paste0("cache$", fields[[name]])) ||
      !.bh_saved_named_list(.bh_saved_field(x, "provenance"))) return(FALSE)
  value <- .bh_saved_field(x, "record")
  if (identical(.bh_saved_field(x, "status"), "retained_evidence") && is.null(value)) return(FALSE)
  current <- .bh_saved_field(cache, fields[[name]])
  if (!is.null(value) && !identical(value, current)) return(FALSE)
  history <- .bh_diagnostic_report_value(.bh_saved_field(.bh_saved_field(x, "provenance"), "posterior_reporting"))
  expected <- .bh_diagnostic_report_phase_state(current, name, history)
  identical(.bh_saved_field(x, "status"), expected)
}
.bh_diagnostic_report_metric <- function(x, name, denominator) {
  if (!.bh_saved_named_list(x)) return(FALSE)
  raw <- .bh_saved_field(x, "raw"); value <- .bh_saved_field(x, "value")
  status <- .bh_saved_field(x, "status"); evaluated <- .bh_saved_field(x, "evaluated")
  reason <- .bh_saved_field(x, "reason"); warnings <- .bh_saved_field(x, "warnings")
  if (!.bh_diagnostic_report_numeric(raw) || !.bh_diagnostic_report_numeric(value) ||
      !.bh_diagnostic_report_logical(evaluated) ||
      !.bh_saved_string(status) || !status %in% c("available", "unavailable", "not_applicable") ||
      !.bh_diagnostic_report_count(.bh_saved_field(x, "denominator"), 1) ||
      .bh_saved_field(x, "denominator") != denominator ||
      !(typeof(reason) == "character" && !is.object(reason) && is.null(dim(reason)) &&
        length(reason) == 1L && (is.na(reason) || (validEnc(reason) && nzchar(reason)))) ||
      !.bh_diagnostic_report_keys(warnings, unique = FALSE)) return(FALSE)
  outcome <- if (!evaluated) "not_evaluated" else if (is.nan(raw)) "NaN" else
    if (is.na(raw)) "NA" else if (raw == Inf) "positive_infinity" else
      if (raw == -Inf) "negative_infinity" else "finite"
  if (!identical(.bh_saved_field(x, "outcome"), outcome)) return(FALSE)
  if (!evaluated && (!is.na(raw) || is.nan(raw))) return(FALSE)
  if (status != "available") return(is.na(value) && !is.nan(value) && !is.na(reason))
  if (!evaluated || !is.na(reason) || !identical(as.double(value), as.double(raw))) return(FALSE)
  valid <- if (name == "rhat") !is.na(value) && value > 0 else is.finite(value)
  if (name %in% c("sd", "ess_bulk", "ess_tail", "mcse_mean", "num_divergent",
      "num_max_treedepth", "fraction_divergent", "fraction_max_treedepth", "ebfmi"))
    valid <- valid && value >= 0
  if (name %in% c("num_divergent", "num_max_treedepth"))
    valid <- valid && value == floor(value) && value <= denominator
  if (name %in% c("fraction_divergent", "fraction_max_treedepth")) valid <- valid && value <= 1
  if (name == "ebfmi") valid <- valid && value > 0
  valid
}
.bh_diagnostic_report_metric_bundle <- function(x, fields, denominator) {
  if (!.bh_saved_named_list(x)) return(FALSE)
  values <- .bh_saved_field(x, "values"); metrics <- .bh_saved_field(x, "metrics")
  if (!.bh_diagnostic_report_numeric(values, length(fields)) || !identical(names(values), fields) ||
      !.bh_saved_named_list(metrics) || !identical(names(metrics), fields)) return(FALSE)
  for (field in fields) {
    item <- .bh_saved_field(metrics, field)
    if (!.bh_diagnostic_report_metric(item, field, denominator) ||
        !identical(as.double(.subset2(values, field)), as.double(.bh_saved_field(item, "value"))))
      return(FALSE)
  }
  TRUE
}
.bh_diagnostic_report_parameter <- function(x, observed, parameters, i) {
  counts <- .bh_saved_field(x, "counts")
  if (!.bh_saved_named_list(counts)) return(FALSE)
  map <- c(chains = "chains", rows_per_chain = "rows_per_chain", rows = "rows",
    half_rows = "split_rows_per_chain", split_chains = "split_chains",
    split_rows = "split_rows", omitted_rows = "split_omitted_rows")
  for (field in names(map)) {
    value <- .bh_saved_field(counts, field)
    if (!.bh_diagnostic_report_count(value) || value != .bh_saved_field(observed, map[[field]])) return(FALSE)
  }
  fields <- c("mean", "median", "sd", "q5", "q95", "rhat", "ess_bulk", "ess_tail", "mcse_mean")
  if (!.bh_diagnostic_report_metric_bundle(x, fields, .bh_saved_field(observed, "rows")) ||
      !.bh_diagnostic_report_logical(.bh_saved_field(x, "global_constant")) ||
      !.bh_diagnostic_report_logical(.bh_saved_field(x, "chain_constant"), .bh_saved_field(observed, "chains")))
    return(FALSE)
  global_constant <- .bh_saved_field(x, "global_constant")
  if (global_constant && !all(.bh_saved_field(x, "chain_constant"))) return(FALSE)
  metrics <- .bh_saved_field(x, "metrics")
  for (field in c("sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean")) {
    metric <- .bh_saved_field(metrics, field)
    if (!identical(.bh_saved_field(metric, "status"), "available")) next
    if (global_constant && field != "sd") return(FALSE)
    if (!global_constant && field %in% c("sd", "mcse_mean") &&
        .bh_saved_field(metric, "value") == 0) return(FALSE)
    if (field == "sd" && .bh_saved_field(observed, "rows") < 2) return(FALSE)
    if (field == "rhat" && (.bh_saved_field(observed, "chains") < 2 ||
        .bh_saved_field(observed, "rows_per_chain") < 4)) return(FALSE)
    if (field %in% c("ess_bulk", "ess_tail", "mcse_mean") &&
        .bh_saved_field(observed, "rows_per_chain") < 6) return(FALSE)
  }
  if (identical(.bh_saved_field(.bh_saved_field(metrics, "mcse_mean"), "status"), "available") &&
      !identical(.bh_saved_field(.bh_saved_field(metrics, "sd"), "status"), "available")) return(FALSE)
  values <- .bh_saved_field(x, "values")
  for (field in fields) {
    column <- .bh_saved_field(parameters, field)
    if (!.bh_diagnostic_report_numeric(column, .row_names_info(parameters, 2L)) ||
        !identical(as.double(column[i]), as.double(.subset2(values, field)))) return(FALSE)
  }
  .bh_saved_named_list(.bh_saved_field(x, "provenance"))
}
.bh_diagnostic_report_hmc <- function(x, observed, hmc, i) {
  fields <- c("num_divergent", "fraction_divergent", "num_max_treedepth", "fraction_max_treedepth", "ebfmi")
  L <- .bh_saved_field(observed, "rows_per_chain"); C <- .bh_saved_field(observed, "chains")
  counts <- .bh_saved_field(x, "counts"); engine <- .bh_saved_field(x, "engine")
  if (!.bh_diagnostic_report_metric_bundle(x, fields, L) || !.bh_saved_named_list(counts) ||
      !.bh_diagnostic_report_count(.bh_saved_field(counts, "rows"), 1) ||
      .bh_saved_field(counts, "rows") != L ||
      !.bh_diagnostic_report_count(.bh_saved_field(counts, "thin"), 1) ||
      !.bh_diagnostic_report_count(.bh_saved_field(counts, "adjacent_pairs")) ||
      .bh_saved_field(counts, "adjacent_pairs") != L - 1 ||
      !.bh_saved_string(engine) || !engine %in% c("nuts", "static")) return(FALSE)
  ids <- .bh_saved_field(hmc, "chain_id")
  if (!.bh_diagnostic_report_numeric(ids, C) ||
      !identical(as.double(ids), as.double(.bh_saved_field(observed, "chain_ids")))) return(FALSE)
  values <- .bh_saved_field(x, "values"); metrics <- .bh_saved_field(x, "metrics")
  for (field in fields) {
    column <- .bh_saved_field(hmc, field)
    if (!.bh_diagnostic_report_numeric(column, C) ||
        !identical(as.double(column[i]), as.double(.subset2(values, field)))) return(FALSE)
  }
  for (pair in list(c("num_divergent", "fraction_divergent"), c("num_max_treedepth", "fraction_max_treedepth"))) {
    count <- .bh_saved_field(metrics, pair[1L]); fraction <- .bh_saved_field(metrics, pair[2L])
    if (!identical(.bh_saved_field(count, "status"), .bh_saved_field(fraction, "status"))) return(FALSE)
    if (identical(.bh_saved_field(count, "status"), "available") &&
        .bh_saved_field(fraction, "value") != .bh_saved_field(count, "value") / L) return(FALSE)
  }
  statuses <- vapply(fields, function(n) .bh_saved_field(.bh_saved_field(metrics, n), "status"), character(1))
  expected <- if (all(statuses %in% c("available", "not_applicable"))) "computed" else
    if (any(statuses == "available")) "partial" else "unavailable"
  coverage <- .bh_saved_field(x, "coverage"); max_depth <- .bh_saved_field(x, "max_depth")
  if (!identical(.bh_saved_field(x, "status"), expected) ||
      !.bh_saved_named_list(coverage) || !identical(names(coverage), c("divergent", "depth", "energy")) ||
      !.bh_saved_named_list(max_depth) || !.bh_saved_named_list(.bh_saved_field(x, "provenance"))) return(FALSE)
  for (field in names(coverage)) {
    item <- .bh_saved_field(coverage, field); present <- .bh_saved_field(item, "present")
    if (!.bh_saved_named_list(item) || !.bh_diagnostic_report_logical(present) ||
        !all(vapply(c("retained", "finite", "missing"), function(n)
          .bh_diagnostic_report_count(.bh_saved_field(item, n)), logical(1))) ||
        .bh_saved_field(item, "retained") != L || .bh_saved_field(item, "finite") != (if (present) L else 0) ||
        .bh_saved_field(item, "missing") != (if (present) 0 else L)) return(FALSE)
  }
  depth_status <- .bh_saved_field(max_depth, "status"); depth_value <- .bh_saved_field(max_depth, "value")
  depth_raw <- .bh_saved_field(max_depth, "raw")
  if (!.bh_saved_string(depth_status) || !depth_status %in% c("available", "unavailable") ||
      !.bh_diagnostic_report_numeric(depth_value) || typeof(depth_raw) != "character" ||
      is.object(depth_raw) || !is.null(dim(depth_raw)) || anyNA(depth_raw) ||
      any(!validEnc(depth_raw))) return(FALSE)
  if (depth_status == "available") {
    if (!.bh_diagnostic_report_count(depth_value, 1, .Machine$integer.max) ||
        !identical(.bh_saved_field(max_depth, "reason"), NA_character_)) return(FALSE)
  } else if (!is.na(depth_value) || is.nan(depth_value) ||
      !.bh_saved_string(.bh_saved_field(max_depth, "reason")) ||
      !.bh_saved_field(max_depth, "reason") %in% c("missing_max_depth", "duplicate_max_depth",
        "malformed_max_depth", "contradictory_max_depth")) return(FALSE)
  depth_metric <- .bh_saved_field(metrics, "num_max_treedepth")
  if (engine == "static" && !identical(.bh_saved_field(depth_metric, "status"), "not_applicable")) return(FALSE)
  if (engine == "nuts" && identical(.bh_saved_field(depth_metric, "status"), "not_applicable")) return(FALSE)
  if (engine == "nuts" && depth_status != "available" &&
      identical(.bh_saved_field(depth_metric, "status"), "available")) return(FALSE)
  for (pair in list(c("divergent", "num_divergent"), c("depth", "num_max_treedepth"), c("energy", "ebfmi")))
    if (!.bh_saved_field(.bh_saved_field(coverage, pair[1L]), "present") &&
        identical(.bh_saved_field(.bh_saved_field(metrics, pair[2L]), "status"), "available")) return(FALSE)
  if ((L < 3 || .bh_saved_field(counts, "thin") != 1) &&
      identical(.bh_saved_field(.bh_saved_field(metrics, "ebfmi"), "status"), "available")) return(FALSE)
  constant <- .bh_saved_field(x, "energy_constant")
  energy_present <- .bh_saved_field(.bh_saved_field(coverage, "energy"), "present")
  arithmetic <- .bh_saved_field(x, "arithmetic")
  arithmetic_fields <- c("sum_squared_differences", "numerator", "variance", "raw_ratio")
  if (!.bh_diagnostic_report_logical(constant, missing = TRUE) ||
      (energy_present && is.na(constant)) || (!energy_present && !is.na(constant)) ||
      !.bh_saved_named_list(arithmetic) || !identical(names(arithmetic), arithmetic_fields) ||
      !all(vapply(arithmetic_fields, function(n) .bh_diagnostic_report_numeric(
        .bh_saved_field(arithmetic, n)), logical(1)))) return(FALSE)
  bfmi <- .bh_saved_field(metrics, "ebfmi")
  if (!.bh_saved_field(bfmi, "evaluated")) {
    if (!all(vapply(arithmetic_fields, function(n) {
      value <- .bh_saved_field(arithmetic, n); is.na(value) && !is.nan(value)
    }, logical(1)))) return(FALSE)
  } else {
    if (!energy_present || L < 3 || .bh_saved_field(counts, "thin") != 1) return(FALSE)
    squares <- .bh_saved_field(arithmetic, "sum_squared_differences")
    numerator <- .bh_saved_field(arithmetic, "numerator")
    variance <- .bh_saved_field(arithmetic, "variance")
    ratio <- .bh_saved_field(arithmetic, "raw_ratio")
    if (!identical(as.double(numerator), as.double(squares / L)) ||
        !identical(as.double(ratio), as.double(numerator / variance)) ||
        !identical(as.double(ratio), as.double(.bh_saved_field(bfmi, "raw")))) return(FALSE)
    if (identical(.bh_saved_field(bfmi, "status"), "available") &&
        (constant || !all(is.finite(c(squares, numerator, variance, ratio))) ||
          any(c(squares, numerator, variance, ratio) <= 0))) return(FALSE)
  }
  TRUE
}

# Used only by explicit refresh construction. Unchanged cache nodes and their
# original provenance are retained; presence alone never promotes computation.
.bh_diagnostic_retained_phases <- function(cache) {
  fields <- c(conditioning = "admissibility", availability_and_covariance = "optional_reports",
    expanded_summary = "expanded_correlation_matrices", numerical_messages = "numerical_evaluations")
  previous <- .bh_saved_field(cache, "posterior_reporting")
  previous_phases <- .bh_saved_field(previous, "phases")
  metadata <- .bh_saved_field(cache, "report_metadata")
  safe_history <- function(value, label) .bh_diagnostic_report_node(value,
    function(x) TRUE, label)
  prior_provenance <- list(posterior_reporting = safe_history(previous, "historical reporting provenance"),
    report_metadata = safe_history(metadata, "historical report metadata"))
  out <- list()
  for (phase in names(fields)) {
    field <- fields[[phase]]; value <- .bh_saved_field(cache, field)
    previous_phase <- .bh_saved_field(previous_phases, phase)
    if (.bh_diagnostic_reporting_v2_header(previous) &&
        isTRUE(tryCatch({ .bh_fit_report_context_plain(previous_phase, phase)
          .bh_diagnostic_report_saved_phase(previous_phase, phase, cache) }, error = function(e) FALSE))) {
      out[[phase]] <- previous_phase
      next
    }
    status <- .bh_saved_field(value, "status")
    reason <- .bh_saved_field(value, "reason")
    if (is.null(value) && identical(.bh_saved_field(previous, "version"), "bmeas_posterior_reporting_v1")) {
      status <- .bh_saved_field(previous, "status"); reason <- .bh_saved_field(previous, "reason")
    }
    valid <- !is.null(value) && isTRUE(tryCatch({
      .bh_fit_report_context_plain(value, field); TRUE }, error = function(e) FALSE))
    # Existing producer statuses, not the new refresh result, establish retained evidence.
    phase_status <- .bh_diagnostic_report_phase_state(value, phase, previous)
    evidence <- identical(phase_status, "retained_evidence")
    if (phase == "numerical_messages" && !evidence && !.bh_saved_string(reason))
      reason <- .bh_saved_field(.bh_saved_field(value, "output_capture"), "reason")
    out[[phase]] <- list(status = phase_status,
      reason = if (.bh_saved_string(reason)) reason else if (evidence)
        "Original saved evidence retained without reassessment" else "No valid evidence was retained for this phase",
      source = paste0("cache$", field), record = if (valid) value else NULL,
      provenance = prior_provenance)
  }
  out
}

.bh_diagnostic_reporting_record <- function(record, cache) {
  if (!.bh_diagnostic_reporting_v2_header(record)) return(.bh_diagnostic_report_invalid("posterior_reporting v2"))
  parameters <- .bh_saved_field(cache, "parameter_summary"); hmc <- .bh_saved_field(cache, "hmc")
  selection <- .bh_diagnostic_report_node(.bh_saved_field(record, "selection"),
    function(x) .bh_diagnostic_report_selection(x, parameters), "selected-monitor coverage")
  observed <- .bh_diagnostic_report_node(.bh_saved_field(record, "observed"),
    .bh_diagnostic_report_observed, "observed original/split counts")
  phases <- .bh_saved_field(record, "phases")
  phase_names <- c("parameter_summary", "hmc", "conditioning", "availability_and_covariance", "expanded_summary", "numerical_messages")
  projected_phases <- .bh_report_named(lapply(phase_names, function(name)
    .bh_diagnostic_report_node(.bh_saved_field(phases, name), function(x)
      .bh_diagnostic_report_saved_phase(x, name, cache), paste(name, "phase"))), phase_names)
  selected <- .bh_saved_field(.bh_diagnostic_report_value(selection), "selected_keys")
  counts <- .bh_diagnostic_report_value(observed)
  project_metrics <- function(name, keys, validate) {
    source <- .bh_saved_field(record, name)
    if (is.null(keys) || is.null(counts)) return(.bh_diagnostic_report_invalid(paste(name, "key/count context")))
    if (!.bh_saved_named_list(source) || length(source) != length(keys) ||
        (length(keys) && !identical(names(source), keys)))
      return(.bh_diagnostic_report_invalid(paste(name, "ordered keys")))
    entries <- .bh_report_named(lapply(seq_along(source), function(i) .bh_diagnostic_report_node(
      .bh_saved_field(source, keys[i]), function(x) validate(x, i), paste(name, keys[i]))), keys)
    list(availability = if (all(vapply(entries, .bh_diagnostic_report_retained, logical(1)))) "retained" else "partial",
      record = entries, reason = "Per-monitor/per-chain saved metric evidence; invalid entries are isolated")
  }
  parameter_metrics <- project_metrics("parameter_metrics", selected,
    function(x, i) .bh_diagnostic_report_parameter(x, counts, parameters, i))
  hmc_metrics <- project_metrics("hmc_metrics", if (is.null(counts)) NULL else
    base::as.character.default(.bh_saved_field(counts, "chain_ids")),
    function(x, i) .bh_diagnostic_report_hmc(x, counts, hmc, i))
  check_phase <- function(name, expected) {
    node <- projected_phases[[name]]
    if (.bh_diagnostic_report_retained(node) &&
        !identical(.bh_saved_field(.bh_diagnostic_report_value(node), "status"), expected))
      projected_phases[[name]] <<- .bh_diagnostic_report_invalid(paste(name, "phase status"))
  }
  if (.bh_diagnostic_report_retained(parameter_metrics)) {
    entries <- .bh_saved_field(parameter_metrics, "record")
    statuses <- unlist(lapply(entries, function(node) vapply(.bh_saved_field(
      .bh_diagnostic_report_value(node), "metrics"), function(x) .bh_saved_field(x, "status"), character(1))), use.names = FALSE)
    check_phase("parameter_summary", if (!length(entries)) "not_requested" else
      if (all(statuses == "available")) "computed" else "computed_partial")
  } else projected_phases$parameter_summary <- .bh_diagnostic_report_invalid("parameter_summary phase evidence")
  if (.bh_diagnostic_report_retained(hmc_metrics)) {
    entries <- .bh_saved_field(hmc_metrics, "record")
    statuses <- unlist(lapply(entries, function(node) vapply(.bh_saved_field(
      .bh_diagnostic_report_value(node), "metrics"), function(x) .bh_saved_field(x, "status"), character(1))), use.names = FALSE)
    check_phase("hmc", if (all(statuses == "not_applicable")) "not_applicable" else
      if (all(statuses %in% c("available", "not_applicable"))) "computed" else
        if (any(statuses == "available")) "partial" else "unavailable")
  } else projected_phases$hmc <- .bh_diagnostic_report_invalid("hmc phase evidence")
  provenance <- .bh_diagnostic_report_node(.bh_saved_field(record, "provenance"),
    function(x) .bh_saved_named_list(x) && all(c("source", "estimator", "budget") %in% names(x)),
    "refresh provenance")
  reason <- .bh_saved_field(record, "reason")
  list(availability = "retained", record = list(version = "bmeas_posterior_reporting_v2",
    reason = if (.bh_saved_string(reason)) reason else "Explicit refresh; phase-local saved evidence follows",
    phases = projected_phases, selection = selection, observed = observed,
    parameter_metrics = parameter_metrics, hmc_metrics = hmc_metrics, provenance = provenance),
    reason = "Optional v2 sections are validated independently; historical cache evidence remains unchanged")
}

.bh_project_diagnostic_reporting <- function(report, reporting) {
  record <- .bh_saved_field(reporting, "record")
  report$sampling$diagnostic_refresh <- record
  selection <- .bh_saved_field(record, "selection")
  report$sampling$monitor_coverage <- selection
  for (name in names(report$sampling$extrema)) {
    report$sampling$extrema[[name]]$coverage$scope <-
      "Selected monitor rows from this refresh; fixed and equality aliases are not independent monitors"
    report$sampling$extrema[[name]]$monitor_coverage <- selection
  }
  report$context$run$diagnostic_refresh_observed <- .bh_saved_field(record, "observed")
  report$sampling$chains$refresh_evidence <- .bh_saved_field(record, "hmc_metrics")
  report$sampling$phase_status <- .bh_saved_field(record, "phases")
  report$sampling$refresh_provenance <- .bh_saved_field(record, "provenance")
  if (.bh_diagnostic_report_retained(.bh_saved_field(record, "parameter_metrics")))
    report$sampling$interval_scope <- "Type-7 q5/q95 over all acquired states of selected monitors; no endpoint MCSE is claimed"
  report
}
.bh_diagnostic_report_display_record <- function(x, record) {
  if (!.bh_diagnostic_reporting_v2_header(record))
    return(.bh_diagnostic_report_invalid("saved projected refresh record"))
  # A saved report can be edited after projection. Availability flags alone
  # cannot authorize display: rebuild the optional view from plain envelopes
  # and run the same local schema/cache checks before length(), paste() or cat().
  unwrap <- function(node, collection = FALSE) {
    invalid <- function() list(invalid_projected_metadata = TRUE)
    if (is.null(node)) return(NULL)
    if (!.bh_saved_named_list(node)) return(invalid())
    availability <- .bh_saved_field(node, "availability")
    if (identical(availability, "unavailable_in_cache")) return(NULL)
    if (!collection) return(if (identical(availability, "retained")) .bh_saved_field(node, "record") else invalid())
    if (!.bh_saved_string(availability) || !availability %in% c("retained", "partial")) return(invalid())
    entries <- .bh_saved_field(node, "record")
    if (!.bh_saved_named_list(entries)) return(invalid())
    .bh_report_named(lapply(names(entries), function(key) {
      entry <- .bh_saved_field(entries, key)
      if (.bh_saved_named_list(entry) && .bh_diagnostic_report_retained(entry))
        .bh_saved_field(entry, "record") else invalid()
    }), names(entries))
  }
  sampling <- .bh_saved_field(x, "sampling"); domains <- .bh_saved_field(x, "domains")
  cache <- list(parameter_summary = .bh_saved_field(sampling, "parameters"),
    hmc = .bh_saved_field(.bh_saved_field(sampling, "chains"), "record"))
  for (name in c("admissibility", "optional_reports", "expanded_correlation_matrices", "numerical_evaluations"))
    cache[name] <- list(unwrap(.bh_saved_field(domains, name)))
  phases <- .bh_saved_field(record, "phases")
  # Historical fit-check views did not expose this cache domain separately.
  # Its preserved phase record is the available saved evidence in that view.
  if (is.null(.bh_saved_field(domains, "expanded_correlation_matrices")))
    cache["expanded_correlation_matrices"] <- list(.bh_saved_field(
      unwrap(.bh_saved_field(phases, "expanded_summary")), "record"))
  phase_names <- c("parameter_summary", "hmc", "conditioning", "availability_and_covariance", "expanded_summary", "numerical_messages")
  raw <- list(version = "bmeas_posterior_reporting_v2", reason = .bh_saved_field(record, "reason"),
    phases = .bh_report_named(lapply(phase_names, function(name) unwrap(.bh_saved_field(phases, name))), phase_names),
    selection = unwrap(.bh_saved_field(record, "selection")), observed = unwrap(.bh_saved_field(record, "observed")),
    parameter_metrics = unwrap(.bh_saved_field(record, "parameter_metrics"), TRUE),
    hmc_metrics = unwrap(.bh_saved_field(record, "hmc_metrics"), TRUE),
    provenance = unwrap(.bh_saved_field(record, "provenance")))
  .bh_diagnostic_reporting_record(raw, cache)
}
.bh_print_diagnostic_reporting <- function(x) {
  record <- .bh_saved_field(.bh_saved_field(x, "sampling"), "diagnostic_refresh")
  if (is.null(record)) return(invisible(FALSE))
  display <- .bh_diagnostic_report_display_record(x, record)
  if (!.bh_diagnostic_report_retained(display)) {
    cat("Optional refresh display metadata: unavailable_invalid_metadata | unrelated cached evidence follows\n")
    return(invisible(FALSE))
  }
  record <- .bh_diagnostic_report_value(display)
  phases <- .bh_saved_field(record, "phases")
  status <- function(name) {
    node <- .bh_saved_field(phases, name)
    if (.bh_diagnostic_report_retained(node)) .bh_saved_field(.bh_diagnostic_report_value(node), "status") else
      .bh_saved_field(node, "availability")
  }
  cat("Explicit diagnostic refresh | parameter phase:", status("parameter_summary"),
    "| HMC phase:", status("hmc"), "\n")
  node <- .bh_saved_field(record, "selection"); selected <- .bh_diagnostic_report_value(node)
  if (!is.null(selected)) cat("Selected monitor coverage:", length(.bh_saved_field(selected, "selected_keys")),
    "of", length(.bh_saved_field(selected, "selectable_keys")),
    "selectable monitors; fixed and equality aliases add no independent monitor.\n") else
      cat("Selected monitor coverage:", .bh_saved_field(node, "availability"), "\n")
  node <- .bh_saved_field(record, "observed"); observed <- .bh_diagnostic_report_value(node)
  if (!is.null(observed)) cat("Observed retained states:", .bh_saved_field(observed, "chains"),
    "original chains x", .bh_saved_field(observed, "rows_per_chain"), "rows =", .bh_saved_field(observed, "rows"),
    "| estimator split:", .bh_saved_field(observed, "split_chains"), "chains x",
    .bh_saved_field(observed, "split_rows_per_chain"), "rows | split-only omissions:",
    .bh_saved_field(observed, "split_omitted_rows"), "\n") else
      cat("Observed retained/split counts:", .bh_saved_field(node, "availability"), "\n")
  cat("Other saved phases: conditioning", status("conditioning"), "| covariance", status("availability_and_covariance"),
    "| expanded summaries", status("expanded_summary"), "| numerical messages", status("numerical_messages"), "\n")
  parameter_nodes <- .bh_saved_field(.bh_saved_field(record, "parameter_metrics"), "record")
  unavailable <- list()
  if (is.list(parameter_nodes)) for (key in names(parameter_nodes)) {
    node <- .bh_saved_field(parameter_nodes, key); evidence <- .bh_diagnostic_report_value(node)
    if (is.null(evidence)) {
      cat("  Selected monitor", key, "metric detail:", .bh_saved_field(node, "availability"), "\n")
      next
    }
    metrics <- .bh_saved_field(evidence, "metrics")
    for (field in names(metrics)) {
      item <- .bh_saved_field(metrics, field)
      if (identical(.bh_saved_field(item, "status"), "available")) next
      label <- paste(field, .bh_saved_field(item, "reason"), sep = ": ")
      previous <- .bh_saved_field(unavailable, label)
      unavailable[[label]] <- if (is.null(previous)) 1L else previous + 1L
    }
  }
  for (label in names(unavailable)) cat("  Selected metric unavailable:", label,
    "| monitors:", .bh_saved_field(unavailable, label), "\n")
  metrics <- .bh_saved_field(record, "hmc_metrics")
  entries <- .bh_saved_field(metrics, "record")
  if (is.list(entries)) for (id in names(entries)) {
    node <- .bh_saved_field(entries, id); evidence <- .bh_diagnostic_report_value(node)
    if (is.null(evidence)) {
      cat("  Chain", id, "HMC detail:", .bh_saved_field(node, "availability"), "\n")
      next
    }
    metric <- .bh_saved_field(evidence, "metrics")
    cat("  Chain", id, "retained rows:", .bh_saved_field(.bh_saved_field(evidence, "counts"), "rows"),
      "| thinning:", .bh_saved_field(.bh_saved_field(evidence, "counts"), "thin"),
      "| HMC availability:", .bh_saved_field(evidence, "status"), "\n")
    for (field in c("num_divergent", "num_max_treedepth", "ebfmi")) {
      item <- .bh_saved_field(metric, field)
      if (!identical(.bh_saved_field(item, "status"), "available")) cat("  Chain", id,
        field, ":", .bh_saved_field(item, "status"), "|", .bh_saved_field(item, "reason"),
        "| retained rows:", .bh_saved_field(item, "denominator"), "\n")
    }
  }
  invisible(TRUE)
}
