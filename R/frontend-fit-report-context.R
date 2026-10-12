# Read-only context for the cached public fit report. No specification helper is
# called here: even a historical specification is read as saved, without model
# validation, callback execution, migration, fingerprinting or backend access.
#
# blocks contains likelihood-row (not response-cell) counts and observed-person
# counts. block_availability documents every projected field. Other evidence
# uses availability/record/source/reason envelopes; retained records are unchanged.
# subjects keeps the joint domain, consumed-person union and saved roster separate.
# run$configured is never multiplied or relabelled as an observed draw count.
# consumed_rows is available only when every declared block count is retained.
# issues names optional metadata that could not safely support its projection;
# those fields are unavailable while unrelated saved evidence remains usable.
.bh_fit_report_context_error <- function(message) .mp_abort("E_FIT_REPORT", message)

.bh_fit_report_context_get <- function(x, name, path = name) {
  if (is.null(x)) return(NULL)
  if (typeof(x) != "list" || isS4(x))
    .bh_fit_report_context_error(paste(path, "must be a saved list"))
  x <- unclass(x)
  n <- names(x)
  if (length(x) && (is.null(n) || anyNA(n) || any(!nzchar(n)) || anyDuplicated(n)))
    .bh_fit_report_context_error(paste(path, "has ambiguous saved field names"))
  x[[name, exact = TRUE]]
}

# Reject executable/reference objects in selected evidence. Unclassing before
# traversal also prevents user-defined extraction methods from running.
.bh_fit_report_context_plain <- function(x, path) {
  if (isS4(x) || !typeof(x) %in% c("NULL", "logical", "integer", "double",
      "complex", "character", "raw", "list"))
    .bh_fit_report_context_error(paste(path, "is not plain saved report evidence"))
  classes <- attr(x, "class", exact = TRUE)
  if (length(classes) && any(!classes %in% c("data.frame", "AsIs", "factor", "ordered",
      "Date", "POSIXct", "POSIXt", "difftime", "numeric_version", "package_version",
      "measurement_run_config", "measurement_model_budget")))
    .bh_fit_report_context_error(paste(path, "contains a non-plain saved class"))
  if (any(classes %in% c("measurement_run_config", "measurement_model_budget")) && typeof(x) != "list")
    .bh_fit_report_context_error(paste(path, "must store its run configuration/budget as a plain list"))
  if (typeof(x) == "list") {
    values <- unclass(x)
    for (i in seq_along(values)) .bh_fit_report_context_plain(values[[i]], path)
  }
  attrs <- attributes(x)
  if (length(attrs)) for (i in seq_along(attrs))
    .bh_fit_report_context_plain(attrs[[i]], path)
  invisible(TRUE)
}

.bh_fit_report_context_record <- function(x, source,
    reason = "not retained in the saved fit/specification; no recalculation performed") {
  if (is.null(x)) return(list(availability = "unavailable_in_cache", record = NULL,
    source = source, reason = reason))
  .bh_fit_report_context_plain(x, source)
  list(availability = "retained", record = x, source = source, reason = NULL)
}

.bh_fit_report_context_table <- function(x, path) {
  if (is.null(x)) return(NULL)
  if (!identical(class(x), "data.frame"))
    .bh_fit_report_context_error(paste(path, "must be a plain saved data.frame"))
  .bh_fit_report_context_plain(x, path)
  if (anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))
    .bh_fit_report_context_error(paste(path, "has ambiguous column names"))
  x
}

.bh_fit_report_context_string <- function(x, path) {
  if (is.null(x)) return(NA_character_)
  if (typeof(x) != "character" || is.object(x) || !is.null(dim(x)) || length(x) != 1L)
    .bh_fit_report_context_error(paste(path, "must be a saved scalar string"))
  x
}

.bh_fit_report_context_keys <- function(x, path, distinct = FALSE) {
  if (is.null(x)) return(NULL)
  if (typeof(x) != "character" || is.object(x) || !is.null(dim(x)) || anyNA(x) || any(!nzchar(x)) ||
      (distinct && anyDuplicated(x)))
    .bh_fit_report_context_error(paste(path, "must contain plain saved keys"))
  x
}

.bh_fit_report_context <- function(fit) {
  # Context is optional evidence. A malformed context field must not discard
  # otherwise usable cached sampling diagnostics. Invalid fields are never
  # coerced; the affected projection is unavailable and its issue is retained.
  issues <- list()
  source_name <- function(x) sub("^fit\\$", "", x)
  note_issue <- function(source, reason) {
    issues[[length(issues) + 1L]] <<- data.frame(source = source_name(source),
      availability = "unavailable_in_cache", reason = paste0("invalid saved metadata: ", reason),
      stringsAsFactors = FALSE)
    invisible(NULL)
  }
  optional <- function(thunk, source, unavailable = NULL) {
    tryCatch(thunk(), error = function(e) {
      note_issue(source, conditionMessage(e))
      unavailable
    })
  }
  issue_reason <- function(source) {
    if (!length(issues)) return(NULL)
    source <- source_name(source)
    found <- vapply(issues, function(x) startsWith(source, x$source), logical(1))
    if (!any(found)) return(NULL)
    paste(unique(vapply(issues[found], function(x) x$reason, character(1))), collapse = "; ")
  }
  get <- function(x, name, path = name) optional(function()
    .bh_fit_report_context_get(x, name, path), path)
  record <- function(x, source,
      reason = "not retained in the saved fit/specification; no recalculation performed") {
    result <- optional(function() .bh_fit_report_context_record(x, source, reason), source)
    invalid <- issue_reason(source)
    if (is.null(result)) return(.bh_fit_report_context_record(NULL, source, invalid))
    if (identical(result$availability, "unavailable_in_cache") && !is.null(invalid)) result$reason <- invalid
    result
  }
  table <- function(x, path) optional(function() .bh_fit_report_context_table(x, path), path)
  spec <- get(fit, "spec", "fit")
  cache <- get(fit, "cache", "fit")
  tables <- get(spec, "tables", "fit$spec")
  mapped <- get(spec, "data_map", "fit$spec")
  models <- get(spec, "models", "fit$spec")
  data_blocks <- get(mapped, "blocks", "fit$spec$data_map")
  tab <- function(name) table(get(tables, name, "fit$spec$tables"),
    paste0("fit$spec$tables$", name))
  saved <- function(name, cache_name = name) {
    x <- get(cache, cache_name, "fit$cache")
    source <- paste0("fit$cache$", cache_name)
    if (is.null(x)) {
      x <- get(tables, name, "fit$spec$tables")
      source <- paste0("fit$spec$tables$", name)
    }
    record(x, source)
  }
  named_blocks <- function(x, path) {
    if (is.null(x)) return(character())
    if (typeof(x) != "list" || is.object(x))
      .bh_fit_report_context_error(paste(path, "must be a named saved list"))
    n <- names(x)
    if (length(x) && (is.null(n) || anyNA(n) || any(!nzchar(n)) || anyDuplicated(n)))
      .bh_fit_report_context_error(paste(path, "has ambiguous block names"))
    n
  }
  block_names <- unique(c(optional(function() named_blocks(models, "fit$spec$models"), "spec$models", character()),
    optional(function() named_blocks(data_blocks, "fit$spec$data_map$blocks"), "spec$data_map$blocks", character())))
  information <- tab("information")
  targets <- tab("targets")
  target_domain <- tab("task_target_domain")
  exclusions <- tab("exclusions")
  exclusions_source <- "fit$spec$tables$exclusions"
  if (is.null(exclusions)) {
    exclusions <- table(get(mapped, "excluded", "fit$spec$data_map"),
      "fit$spec$data_map$excluded")
    exclusions_source <- "fit$spec$data_map$excluded"
  }
  roster <- saved("person_roster", "person_roster")
  if (is.null(roster$record)) {
    reference <- get(cache, "reference_law", "fit$cache")
    roster <- record(get(reference, "person_roster", "fit$cache$reference_law"),
      "fit$cache$reference_law$person_roster")
  }
  roster_table <- table(roster$record, roster$source)
  n_blocks <- length(block_names)
  blocks <- data.frame(block = block_names, model_name = rep(NA_character_, n_blocks),
    model_version = rep(NA_character_, n_blocks), response_units = rep(NA_character_, n_blocks),
    consumed_rows = rep(NA_integer_, n_blocks), observed_people = rep(NA_integer_, n_blocks),
    informative_rows = rep(NA_integer_, n_blocks), excluded_rows = rep(NA_integer_, n_blocks),
    declared_subject_task_targets = rep(NA_integer_, n_blocks), active_person_effect_targets = rep(NA_integer_, n_blocks),
    stringsAsFactors = FALSE)
  availability <- list()
  add_availability <- function(block, field, value, source, reason = NULL) {
    present <- length(value) == 1L && !is.na(value)
    if (!present && is.null(reason)) reason <- issue_reason(source)
    availability[[length(availability) + 1L]] <<- data.frame(block = block,
      field = field, availability = if (present) "retained" else "unavailable_in_cache",
      source = source, reason = if (present) NA_character_ else if (is.null(reason))
        "not retained in the saved fit/specification" else reason, stringsAsFactors = FALSE)
  }
  observed_keys <- list()
  observed_complete <- !is.null(data_blocks) && all(block_names %in% names(data_blocks))
  for (i in seq_along(block_names)) {
    b <- block_names[i]
    binding <- get(models, b, "fit$spec$models")
    model <- get(binding, "model", paste0("fit$spec$models$", b))
    block_source <- paste0("spec$models$", b)
    blocks$model_name[i] <- optional(function() .bh_fit_report_context_string(get(model, "name"), "model$name"),
      paste0(block_source, "$model$name"), NA_character_)
    blocks$model_version[i] <- optional(function() .bh_fit_report_context_string(get(model, "version"), "model$version"),
      paste0(block_source, "$model$version"), NA_character_)
    blocks$response_units[i] <- optional(function() .bh_fit_report_context_string(get(binding, "units"), "block$units"),
      paste0(block_source, "$units"), NA_character_)
    d <- table(get(data_blocks, b, "fit$spec$data_map$blocks"), paste0("data_map$blocks$", b))
    if (!is.null(d)) {
      blocks$consumed_rows[i] <- nrow(d)
      keys <- optional(function() .bh_fit_report_context_keys(d$.person_key,
        paste0("data_map$blocks$", b, "$.person_key")), paste0("spec$data_map$blocks$", b, "$.person_key"))
      if (!is.null(keys)) {
        blocks$observed_people[i] <- length(unique(keys))
        observed_keys[[b]] <- unique(keys)
      } else observed_complete <- FALSE
    } else observed_complete <- FALSE

    # Information is counted only after an exact block/observation-key join.
    if (!is.null(d) && !is.null(information) &&
        all(c("block", "observation_id", "informative") %in% names(information)) &&
        ".observation_id" %in% names(d)) {
      blocks$informative_rows[i] <- optional(function() {
        .bh_fit_report_context_keys(information$block, "information$block")
        .bh_fit_report_context_keys(information$observation_id, "information$observation_id")
        .bh_fit_report_context_keys(d$.observation_id, "consumed observation keys", distinct = TRUE)
        info <- information[information$block == b, , drop = FALSE]
        if (anyDuplicated(info$observation_id))
          .bh_fit_report_context_error("information contains duplicate block/observation keys")
        if (typeof(info$informative) != "logical" || is.object(info$informative) ||
            !is.null(dim(info$informative)) || length(info$informative) != nrow(info))
          .bh_fit_report_context_error("information$informative must be one plain logical value per saved row")
        at <- match(d$.observation_id, info$observation_id)
        if (!anyNA(at) && !anyNA(info$informative[at])) sum(info$informative[at]) else NA_integer_
      }, "spec$tables$information", NA_integer_)
    }
    if (!is.null(exclusions)) {
      field <- if (".block" %in% names(exclusions)) ".block" else if ("block" %in% names(exclusions)) "block" else NULL
      if (!is.null(field)) {
        blocks$excluded_rows[i] <- optional(function() {
          .bh_fit_report_context_keys(exclusions[[field]], "exclusions block keys")
          sum(exclusions[[field]] == b)
        }, exclusions_source, NA_integer_)
      }
    }
    if (!is.null(targets) && all(c("key", "model", "level") %in% names(targets))) {
      selected <- optional(function() {
        .bh_fit_report_context_keys(targets$key, "targets$key", distinct = TRUE)
        .bh_fit_report_context_keys(targets$model, "targets$model")
        .bh_fit_report_context_keys(targets$level, "targets$level")
        targets$model == b
      }, "spec$tables$targets")
      if (!is.null(selected)) blocks$declared_subject_task_targets[i] <- sum(targets$level[selected] == "subject_task")
      if (!is.null(selected) && !is.null(target_domain) && all(c("target", "level", "has_symbolic_support") %in% names(target_domain))) {
        blocks$active_person_effect_targets[i] <- optional(function() {
          .bh_fit_report_context_keys(target_domain$target, "task_target_domain$target", distinct = TRUE)
          .bh_fit_report_context_keys(target_domain$level, "task_target_domain$level")
          support <- target_domain$has_symbolic_support
          if (typeof(support) != "logical" || is.object(support) ||
              !is.null(dim(support)) || length(support) != nrow(target_domain))
            .bh_fit_report_context_error("task_target_domain$has_symbolic_support must be one plain logical value per saved row")
          at <- match(targets$key[selected], target_domain$target)
          active <- support[at]
          level <- target_domain$level[at]
          if (!anyNA(at) && !anyNA(active) && !anyNA(level)) sum(level == "subject_task" & active) else NA_integer_
        }, "spec$tables$task_target_domain", NA_integer_)
      }
    }
    sources <- c(model_name = "spec$models[[block]]$model$name",
      model_version = "spec$models[[block]]$model$version", response_units = "spec$models[[block]]$units",
      consumed_rows = "spec$data_map$blocks[[block]]", observed_people = "spec$data_map$blocks[[block]]$.person_key",
      informative_rows = "spec$tables$information joined by block and observation_id",
      excluded_rows = exclusions_source, declared_subject_task_targets = "spec$tables$targets",
      active_person_effect_targets = "spec$tables$task_target_domain joined to targets by target/key")
    for (field in names(sources)) {
      field_issue <- switch(field, model_name = paste0(block_source, "$model$name"),
        model_version = paste0(block_source, "$model$version"), response_units = paste0(block_source, "$units"),
        observed_people = paste0("spec$data_map$blocks$", b, "$.person_key"), NULL)
      add_availability(b, field, blocks[[field]][i], sources[[field]],
        if (is.null(field_issue)) NULL else issue_reason(field_issue))
    }
  }
  block_availability <- if (length(availability)) do.call(rbind, availability) else
    data.frame(block = character(), field = character(), availability = character(),
      source = character(), reason = character(), stringsAsFactors = FALSE)

  joint <- optional(function() .bh_fit_report_context_keys(get(mapped, "subject_keys"),
    "data_map$subject_keys", distinct = TRUE), "spec$data_map$subject_keys")
  observed <- if (observed_complete) {
    if (length(observed_keys)) unique(unlist(observed_keys, use.names = FALSE)) else character()
  } else NULL
  if (!is.null(observed) && !is.null(joint) && any(!observed %in% joint)) {
    note_issue("spec$data_map$subject_keys", "consumed person keys fall outside the saved joint subject domain")
    joint <- NULL
  }
  roster_observed <- if (!is.null(roster_table)) roster_table$observed else NULL
  if (!is.null(roster_observed) && (typeof(roster_observed) != "logical" || is.object(roster_observed) ||
      !is.null(dim(roster_observed)) || length(roster_observed) != nrow(roster_table))) {
    note_issue(paste0(roster$source, "$observed"), "person_roster$observed must be one plain logical value per saved row")
    roster_observed <- NULL
  }
  subject_values <- list(joint_subject_keys = joint, observed_subject_keys = observed,
    joint_subject_count = if (is.null(joint)) NA_integer_ else length(joint),
    observed_people = if (is.null(observed)) NA_integer_ else length(observed),
    roster_count = if (is.null(roster_table)) NA_integer_ else nrow(roster_table),
    observed_roster_people = if (is.null(roster_observed) || anyNA(roster_observed)) NA_integer_ else sum(roster_observed),
    unobserved_roster_people = if (is.null(roster_observed) || anyNA(roster_observed)) NA_integer_ else sum(!roster_observed),
    person_roster = roster_table)
  subject_sources <- c(joint_subject_keys = "spec$data_map$subject_keys", observed_subject_keys = "consumed block person-key union",
    joint_subject_count = "spec$data_map$subject_keys", observed_people = "consumed block person-key union",
    roster_count = roster$source, observed_roster_people = roster$source,
    unobserved_roster_people = roster$source, person_roster = roster$source)
  subject_availability <- lapply(names(subject_values), function(n) {
    value <- subject_values[[n]]
    if (length(value) == 1L && is.atomic(value) && is.na(value)) value <- NULL
    result <- record(value, subject_sources[[n]])
    if (n %in% c("observed_roster_people", "unobserved_roster_people") && is.null(value)) {
      invalid <- issue_reason(paste0(roster$source, "$observed"))
      if (!is.null(invalid)) result$reason <- invalid
    }
    result
  })
  names(subject_availability) <- names(subject_values)
  subjects <- record(if (any(vapply(subject_availability, function(x)
    identical(x$availability, "retained"), logical(1)))) subject_values else NULL,
    "saved joint domain, consumed person-key union and person roster")
  subjects$fields <- subject_availability
  subjects$scope <- "Observed means contributing consumed likelihood rows; it does not imply informative observations. The roster may include unobserved people."

  missingness <- list(policy = record(get(spec, "missing"), "spec$missing"),
    assumption = record(get(mapped, "missing_assumption"), "spec$data_map$missing_assumption"),
    response_semantics = record(get(spec, "response_missing_semantics"), "spec$response_missing_semantics"))
  exclusion_reasons <- if (is.null(exclusions)) NULL else {
    fields <- intersect(c("reason", "exclusion_reason"), names(exclusions))
    if (length(fields)) exclusions[, unique(c(intersect(c(".block", "block", ".observation_id", "observation_id"), names(exclusions)), fields)), drop = FALSE] else NULL
  }
  exclusions_record <- record(exclusions, exclusions_source)
  exclusions_record$count <- if (is.null(exclusions)) NA_integer_ else nrow(exclusions)
  exclusions_record$count_scope <- "excluded likelihood records, not response cells"
  gather <- function(fields, cached = character()) {
    out <- lapply(fields, saved)
    names(out) <- fields
    for (n in cached) out[[n]] <- record(get(cache, n), paste0("cache$", n))
    out
  }
  group_record <- function(fields, source) {
    if (!any(vapply(fields, function(x) identical(x$availability, "retained"), logical(1)))) {
      out <- record(NULL, source)
      out$fields <- fields
      return(out)
    }
    record(fields, source)
  }
  reference_saved <- function(name, reference_name = name, cache_name = name) {
    out <- saved(name, cache_name)
    if (identical(out$availability, "retained")) return(out)
    reference <- get(cache, "reference_law", "fit$cache")
    record(get(reference, reference_name), paste0("cache$reference_law$", reference_name))
  }
  metadata <- get(cache, "report_metadata", "fit$cache")
  provenance <- get(fit, "provenance", "fit")
  execution_profile <- record(get(cache, "execution_profile"), "cache$execution_profile")
  if (!identical(execution_profile$availability, "retained"))
    execution_profile <- record(get(provenance, "execution_profile"), "fit$provenance$execution_profile")
  predictor_keys <- table(get(spec, "parameter_keys"), "spec$parameter_keys")
  predictor_scales <- if (is.null(predictor_keys)) NULL else predictor_keys[, intersect(
    c("model", "key", "parameter", "task", "predictor", "receives", "support", "units",
      "predictor_units", "observation_units"), names(predictor_keys)), drop = FALSE]
  summary_table <- table(get(cache, "parameter_summary", "fit$cache"), "fit$cache$parameter_summary")
  monitored_rows <- if (is.null(summary_table)) NULL else summary_table[, intersect(
    c("variable", "parameter_key", "parameter_role", "target_variation_level", "reference_target"), names(summary_table)), drop = FALSE]
  run <- list(configured = record(get(fit, "run"), "fit$run"),
    observed = record(get(metadata, "observed_run"), "cache$report_metadata$observed_run",
      "observed chain/draw counts were not retained; configured chains and iterations are not observed counts"),
    monitored = record(get(metadata, "monitored"), "cache$report_metadata$monitored"),
    cached_summary_rows = record(monitored_rows, "cache$parameter_summary"),
    summary_method = record(get(metadata, "summary_method"), "cache$report_metadata$summary_method"),
    limitations = list(computation = record(get(cache, "computation"), "cache$computation"),
      inference = record(get(cache, "inference"), "cache$inference")),
    count_scope = "fit$run contains configured run controls. Only separately retained observed_run metadata establishes observed chain/draw counts; cached summary rows are monitored quantities, not draws.")

  membership <- optional(function() .bh_fit_report_context_membership(summary_table, spec, tables, cache),
    "structured parameter/target membership")
  if (is.null(membership)) membership <- record(NULL, "structured parameter/target membership")
  empty_consumed <- typeof(data_blocks) == "list" && !is.object(data_blocks) && !length(data_blocks) &&
    (is.null(models) || (typeof(models) == "list" && !is.object(models) && !length(models)))
  total_consumed <- if (length(block_names) && !anyNA(blocks$consumed_rows)) sum(blocks$consumed_rows) else
    if (empty_consumed) 0L else NULL
  consumed_rows <- record(total_consumed, "spec$data_map$blocks",
    "the complete set of saved consumed likelihood rows was not retained; an absent context is not zero observations")
  consumed_rows$count_scope <- "total consumed likelihood rows across all saved declared blocks; not response cells or informative observations"
  result <- list(blocks = blocks, block_availability = block_availability,
    consumed_rows = consumed_rows,
    blocks_source = record(if (is.null(models) && is.null(data_blocks)) NULL else block_names,
      "spec$models and spec$data_map$blocks"), subjects = subjects,
    exclusions = exclusions_record,
    exclusion_reasons = record(exclusion_reasons, paste0(exclusions_source, "$reason"),
      "row-specific exclusion reasons were not retained; saved missingness policy is reported separately"),
    partial_response_exclusions = saved("partial_response_exclusions"),
    missingness = group_record(missingness, "saved missingness metadata"),
    overlap = record(get(mapped, "overlap"), "spec$data_map$overlap"),
    overlap_scope = "Saved consumed/observed-person coverage; not informative overlap or an identification certificate. Centered person laws can couple different people.",
    coverage = record(get(mapped, "coverage"), "spec$data_map$coverage"),
    task_coverage = record(get(mapped, "task_coverage"), "spec$data_map$task_coverage"),
    information = record(information, "spec$tables$information"),
    predictor_scales = record(predictor_scales, "spec$parameter_keys"),
    targets = record(targets, "spec$tables$targets"),
    parameter_roles = saved("parameter_roles"), fixed_parameters = saved("fixed_parameters"),
    task_target_domain = record(target_domain, "spec$tables$task_target_domain"),
    person_law = reference_saved("person_law"), augmentation = reference_saved("augmentation"),
    correlation_coordinates = reference_saved("correlation_coordinate_system", "correlation_coordinates", "correlation_coordinates"),
    correlation_constraints = record(get(cache, "correlation_constraints"), "cache$correlation_constraints"),
    scales = group_record(gather(c("component_scales", "loading_cells", "loading_primitives", "loading_coefficients",
      "coordinates", "parameter_residuals", "share_targets", "share_parts", "share_coefficients", "decompositions")), "saved scale and coefficient tables"),
    estimands = group_record(gather(c("components", "individual_targets", "task_latent_coordinates", "latent_coordinates",
      "parameter_latent_coordinates", "correlations", "parameter_correlations", "correlation_primitives",
      "correlation_affine_cells", "correlation_blocks", "raw_correlation_coordinates", "correlation_prior_geometry", "priors"),
      c("derived_marker_estimands", "reference_law", "expanded_correlation_matrices")), "saved estimand, coordinate and prior evidence"),
    membership = membership,
    execution_profile = execution_profile,
    provenance = record(provenance, "fit$provenance"),
    specification_version = record(get(spec, "schema_version"), "spec$schema_version"),
    run = run)
  result$issues <- if (length(issues)) unique(do.call(rbind, issues)) else
    data.frame(source = character(), availability = character(), reason = character(), stringsAsFactors = FALSE)
  rownames(result$issues) <- NULL
  result
}

# Membership is deliberately partial: exact parameter/target joins may assign a
# cached row to several blocks. No parameter key or alias display text is split.
.bh_fit_report_context_membership <- function(summary, spec, tables, cache) {
  get <- .bh_fit_report_context_get
  record <- .bh_fit_report_context_record
  table <- .bh_fit_report_context_table
  keys <- table(get(spec, "parameter_keys"), "spec$parameter_keys")
  if (is.null(keys)) keys <- table(get(tables, "targets"), "spec$tables$targets")
  source <- "structured parameter/target joins to spec$parameter_keys"
  if (is.null(summary) || !"parameter_key" %in% names(summary) || is.null(keys) ||
      !all(c("key", "model") %in% names(keys)))
    return(record(NULL, source, "structured cached-parameter or target-to-block membership was not retained"))
  summary_keys <- summary$parameter_key
  if (typeof(summary_keys) != "character" || is.object(summary_keys) || !is.null(dim(summary_keys)) ||
      length(summary_keys) != nrow(summary))
    .bh_fit_report_context_error("cached parameter_key membership requires one plain character key per cached row, without class or dimensions")
  .bh_fit_report_context_keys(keys$key, "parameter_keys$key", distinct = TRUE)
  .bh_fit_report_context_keys(keys$model, "parameter_keys$model")
  maps <- list()
  add_map <- function(x, parameter, target, from) {
    if (is.null(x) || !all(c(parameter, target) %in% names(x))) return(invisible(NULL))
    for (field in c(parameter, target)) if (typeof(x[[field]]) != "character" || is.object(x[[field]]) ||
        !is.null(dim(x[[field]])) || length(x[[field]]) != nrow(x))
      .bh_fit_report_context_error(paste(from, field, "requires one plain character value per saved row"))
    keep <- !is.na(x[[parameter]]) & !is.na(x[[target]])
    at <- match(x[[target]][keep], keys$key)
    valid <- !is.na(at)
    maps[[length(maps) + 1L]] <<- data.frame(parameter_key = x[[parameter]][keep][valid],
      target = x[[target]][keep][valid], block = keys$model[at[valid]],
      source = rep(from, sum(valid)), stringsAsFactors = FALSE)
    invisible(NULL)
  }
  for (n in c("priors", "loading_cells", "parameter_residuals", "share_targets", "share_coefficients"))
    add_map(table(get(tables, n), paste0("spec$tables$", n)), "parameter", "target", paste0("spec$tables$", n))
  add_map(table(get(cache, "derived_marker_estimands"), "cache$derived_marker_estimands"),
    "parameter_key", "target", "cache$derived_marker_estimands")
  reference <- get(cache, "reference_law")
  add_map(table(get(reference, "summary_map"), "cache$reference_law$summary_map"),
    "parameter_key", "target", "cache$reference_law$summary_map")
  if ("reference_target" %in% names(summary))
    add_map(summary, "parameter_key", "reference_target", "cache$parameter_summary$reference_target")
  mapping <- if (length(maps)) unique(do.call(rbind, maps)) else
    data.frame(parameter_key = character(), target = character(), block = character(), source = character())
  rows <- lapply(seq_len(nrow(summary)), function(i) {
    key <- summary_keys[i]
    hits <- mapping[!is.na(key) & mapping$parameter_key == key, , drop = FALSE]
    if (!nrow(hits)) return(data.frame(cached_row = i, parameter_key = key,
      target = NA_character_, block = NA_character_, source = NA_character_,
      membership = "unavailable_in_cache", stringsAsFactors = FALSE))
    data.frame(cached_row = i, hits, membership = if (length(unique(hits$block)) > 1L)
      "shared" else "assigned", stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(cached_row = integer(),
    parameter_key = character(), target = character(), block = character(), source = character(), membership = character())
  result <- record(out, source)
  result$scope <- "Partial structured membership only; unavailable rows remain unassigned, and shared rows are not allocated to one arbitrary block. No block-specific diagnostic extrema are calculated."
  result
}
