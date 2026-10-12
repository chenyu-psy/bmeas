# Deterministic summaries of the acquired joint eta states. This layer performs
# no source acquisition, posterior reconstruction, backend calls or RNG work.
# Only one cell/scale series is transformed at a time; raw series never escape.
.bh_individual_stats_plain_list <- function(x) {
  typeof(x) == "list" && !is.object(x) &&
    all(names(attributes(x)) %in% "names") &&
    typeof(names(x)) == "character" && !anyNA(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x))
}

.bh_individual_stats_cells <- function(cells) {
  required <- c("person_key", "person_id", "person_index", "parameter_key",
    "model", "parameter", "task", "declared_level", "effective_role",
    "shared_alias_key", "source_variable", "predictor", "receives", "support",
    "units", "predictor_units", "received_units", "fixed_received",
    "fixed_predictor", "fixed_natural", "predictor_representation",
    "natural_representation", "n_likelihood_rows", "n_informative_rows")
  if (typeof(cells) != "list" ||
      !identical(attr(cells, "class", exact = TRUE), "data.frame") ||
      any(!names(attributes(cells)) %in% c("names", "row.names", "class")) ||
      typeof(names(cells)) != "character" || anyNA(names(cells)) ||
      any(!nzchar(names(cells))) || anyDuplicated(names(cells)) ||
      !all(required %in% names(cells)))
    .mp_abort("E_INDIVIDUAL_PLAN", "individual summaries require a plain, complete cell table")
  for (column in cells) if (!typeof(column) %in% c("character", "integer", "double", "logical") ||
      is.object(column) || !is.null(attributes(column)))
    .mp_abort("E_INDIVIDUAL_PLAN", "cell fields must be plain scalar-vector columns")
  n <- nrow(cells)
  if (any(lengths(cells) != n))
    .mp_abort("E_INDIVIDUAL_PLAN", "cell fields must have equal lengths")
  character_fields <- setdiff(required, c("person_id", "person_index",
    "fixed_received", "fixed_predictor", "fixed_natural", "n_likelihood_rows",
    "n_informative_rows"))
  if (any(vapply(cells[character_fields], typeof, character(1)) != "character"))
    .mp_abort("E_INDIVIDUAL_PLAN", "cell identity and scale fields must be character vectors")
  for (name in c("person_index", "fixed_received", "fixed_predictor", "fixed_natural",
                 "n_likelihood_rows", "n_informative_rows"))
    if (!typeof(cells[[name]]) %in% c("integer", "double"))
      .mp_abort("E_INDIVIDUAL_PLAN", "cell indices, fixed values and coverage counts must be numeric")
  no_missing <- c("person_key", "parameter_key", "model", "parameter", "task",
                 "declared_level", "effective_role", "predictor", "receives", "support")
  if (any(vapply(cells[no_missing], anyNA, logical(1))) ||
      any(vapply(cells[no_missing], function(x) any(!nzchar(x)), logical(1))) ||
      any(!is.finite(cells$person_index) | cells$person_index < 1 |
          cells$person_index != floor(cells$person_index)) ||
      any(!cells$predictor %in% c("identity", "log", "logit")) ||
      any(!cells$receives %in% c("eta", "theta")))
    .mp_abort("E_INDIVIDUAL_PLAN", "cell identities or supported scale declarations are invalid")
  if (anyDuplicated(cells[c("person_key", "parameter_key")]))
    .mp_abort("E_INDIVIDUAL_PLAN", "person and logical parameter cells must be unique")
  fixed <- cells$declared_level == "fixed"
  if (any(fixed != (cells$effective_role == "fixed_by_declaration")) ||
      any(fixed != is.na(cells$source_variable)) ||
      any(!is.na(cells$source_variable) & !nzchar(cells$source_variable)) ||
      any(fixed & !is.na(cells$shared_alias_key)))
    .mp_abort("E_INDIVIDUAL_PLAN", "fixed roles, sampled sources and alias declarations disagree")
  shared <- cells$effective_role %in% c("shared_task_predictor", "shared_mean_only_predictor")
  if (any(shared != !is.na(cells$shared_alias_key)) ||
      any(!is.na(cells$shared_alias_key) & !nzchar(cells$shared_alias_key)))
    .mp_abort("E_INDIVIDUAL_PLAN", "shared roles require an explicit saved alias key")
  invisible(TRUE)
}

.bh_individual_stats_template <- function(cells, scales) {
  out <- cells[rep(seq_len(nrow(cells)), each = length(scales)), , drop = FALSE]
  rownames(out) <- NULL
  n <- nrow(out)
  out$scale <- rep(scales, times = nrow(cells))
  out$summary_units <- ifelse(out$scale == "predictor", out$predictor_units,
    ifelse(out$scale == "received", out$received_units, out$units))
  # Ensure the zero-cell result has the same declared column types.
  out$summary_units <- as.character(out$summary_units)
  for (name in c("mean", "median", "sd", "lower90", "upper90", "known_value",
                 "ess_bulk", "ess_tail", "rhat", "mcse_mean")) out[[name]] <- rep(NA_real_, n)
  for (name in c("status", "reason", "scale_status", "scale_reason", "estimate_kind",
                 "uncertainty_status", "diagnostics_status", "diagnostics_reason"))
    out[[name]] <- rep(NA_character_, n)
  for (stat in c("mean", "median", "sd", "lower90", "upper90")) {
    out[[paste0(stat, "_status")]] <- rep(NA_character_, n)
    out[[paste0(stat, "_reason")]] <- rep(NA_character_, n)
  }
  for (name in c("n_draws", "n_sampled_values", "n_invalid_states",
                 "n_invalid_transformed_states", "n_invalid_fixed_values")) out[[name]] <- integer(n)
  out$quantile_type <- rep(7L, n)
  out
}

.bh_individual_stats_transform <- function(eta, predictor, scale, receives) {
  natural <- scale == "natural" || (scale == "received" && receives == "theta")
  if (!natural || predictor == "identity")
    return(list(values = eta, n_invalid = 0L, reason = NA_character_))
  if (predictor == "log") {
    value <- exp(eta)
    invalid <- !is.finite(value) | value <= 0
    reasons <- c(if (any(!is.finite(value))) "exp_overflow",
                 if (any(value == 0)) "exp_underflow")
  } else {
    value <- stats::plogis(eta)
    invalid <- !is.finite(value) | value <= 0 | value >= 1
    reasons <- if (any(invalid)) "inverse_logit_saturation" else character()
  }
  list(values = value, n_invalid = as.integer(sum(invalid)),
       reason = if (length(reasons)) paste(reasons, collapse = ";") else NA_character_)
}

.bh_individual_stats_fixed <- function(cell, scale) {
  representation <- if (scale == "natural" || (scale == "received" && cell$receives == "theta"))
    cell$natural_representation else cell$predictor_representation
  value <- cell[[paste0("fixed_", scale)]]
  available <- !is.na(representation) && representation %in% c("supplied_exact", "available") &&
    is.finite(value)
  reason <- if (available) NA_character_ else if (is.na(representation))
    "fixed_representation_unavailable" else if (!representation %in% c("supplied_exact", "available"))
      paste0("fixed_", representation) else "fixed_value_not_finite"
  list(value = if (available) value else NA_real_, available = available, reason = reason)
}

.bh_individual_summarize <- function(plan, acquired = NULL) {
  if (!.bh_individual_stats_plain_list(plan) || !all(c("cells", "scales", "budget") %in% names(plan)))
    .mp_abort("E_INDIVIDUAL_PLAN", "a plain individual cell, scale and budget plan is required")
  cells <- plan$cells
  .bh_individual_stats_cells(cells)
  scales <- plan$scales
  if (typeof(scales) != "character" || is.object(scales) || !is.null(attributes(scales)) ||
      !length(scales) || length(scales) > 3L || anyNA(scales) || anyDuplicated(scales) ||
      any(!scales %in% c("predictor", "natural", "received")))
    .mp_abort("E_INDIVIDUAL_PLAN", "one to three distinct explicit scales are required")
  budget <- plan$budget
  streaming <- .bh_individual_stats_plain_list(budget) && "posterior" %in% names(budget)
  if (streaming) {
    budget <- .bh_individual_budget(budget)
    output_rows <- .bh_individual_product(nrow(cells), length(scales), label = "summary output rows")
    .bh_individual_count(output_rows, "summary output rows", dimension = TRUE)
    if (nrow(cells) > budget$max_parameter_cells || output_rows > budget$max_summary_rows)
      .mp_abort("E_INDIVIDUAL_BUDGET", "the complete cell and scale domain exceeds the summary budget")
  } else {
  caps <- c(max_source_bytes = 10485760, max_posterior_rows = 100,
            max_draw_values = 50000, max_parameter_cells = 1000)
  minima <- c(max_source_bytes = 1, max_posterior_rows = 2,
              max_draw_values = 0, max_parameter_cells = 0)
  if (!.bh_individual_stats_plain_list(budget) || !setequal(names(budget), names(caps)))
    .mp_abort("E_INDIVIDUAL_BUDGET", "the four exact individual budget fields are required")
  for (name in names(caps)) {
    value <- budget[[name]]
    if (!typeof(value) %in% c("integer", "double") || is.object(value) ||
        !is.null(attributes(value)) || length(value) != 1L || !is.finite(value) ||
        value != floor(value) || value < minima[[name]] || value > caps[[name]])
      .mp_abort("E_INDIVIDUAL_BUDGET", paste("invalid bounded individual budget field", name))
  }
  if (nrow(cells) > budget$max_parameter_cells || nrow(cells) * length(scales) > 3000L)
    .mp_abort("E_INDIVIDUAL_BUDGET", "the complete cell and scale domain exceeds the summary budget")
  }
  if (!nrow(cells)) return(.bh_individual_stats_template(cells, scales))
  if (!.bh_individual_stats_plain_list(acquired) || !"draws" %in% names(acquired))
    .mp_abort("E_INDIVIDUAL_SOURCE", "nonempty individual summaries require acquired source rows")
  draws <- acquired$draws
  if (!typeof(draws) %in% c("integer", "double") || is.object(draws) ||
      any(!names(attributes(draws)) %in% c("dim", "dimnames")) ||
      typeof(attr(draws, "dim", exact = TRUE)) != "integer" ||
      length(attr(draws, "dim", exact = TRUE)) != 2L)
    .mp_abort("E_INDIVIDUAL_SOURCE", "acquired draws must be a plain numeric matrix")
  dimensions <- attr(draws, "dimnames", exact = TRUE)
  if (!is.null(dimensions) && (typeof(dimensions) != "list" || is.object(dimensions) ||
      !is.null(attributes(dimensions)) || length(dimensions) != 2L ||
      any(vapply(dimensions, function(x) !is.null(x) && (typeof(x) != "character" ||
        is.object(x) || !is.null(attributes(x))), logical(1)))))
    .mp_abort("E_INDIVIDUAL_SOURCE", "acquired draw names must be plain dimension names")
  required <- unique(cells$source_variable[!is.na(cells$source_variable)])
  columns <- colnames(draws)
  if ((length(required) && is.null(columns)) || anyNA(columns) || anyDuplicated(columns) ||
      !setequal(required, columns) || ncol(draws) != length(required))
    .mp_abort("E_INDIVIDUAL_SOURCE", "acquired columns must match every required source scalar exactly")
  n_draws <- nrow(draws)
  if (n_draws < 2L)
    .mp_abort("E_INDIVIDUAL_SOURCE", "individual summaries require at least two retained source rows")
  excessive <- if (streaming) .bh_individual_product(nrow(cells), n_draws, length(scales),
    label = "selected-state transformation work") > budget$max_transformation_terms else
      n_draws > budget$max_posterior_rows || n_draws * ncol(draws) > budget$max_draw_values ||
        nrow(cells) * n_draws * length(scales) > 300000L
  if (excessive)
    .mp_abort("E_INDIVIDUAL_BUDGET", "complete retained rows, required values or transformation work exceed the budget")
  if (streaming) {
    # Recheck the known consumer payload before output/series allocation. The
    # acquisition envelope additionally covers its own parser/header buffers.
    buffers <- .bh_individual_consumer_buffers(plan)
    fixed_slots <- do.call(.bh_individual_add, as.list(unname(buffers$fixed)))
    per_row <- do.call(.bh_individual_add, as.list(unname(buffers$per_selected_row)))
    payload <- .bh_individual_add(fixed_slots,
      .bh_individual_product(n_draws, .bh_individual_add(per_row, as.double(ncol(draws)), 2)),
      label = "known consumer and acquired numeric payload")
    policy <- .bh_posterior_policy(budget$posterior, required)
    if (payload > policy$limits$max_materialized_values)
      .mp_abort("E_INDIVIDUAL_BUDGET", "known acquired matrix, keys and consumer numeric buffers exceed max_materialized_values")
  }
  for (j in seq_len(ncol(draws))) if (any(!is.finite(draws[, j])))
    .mp_abort("E_INDIVIDUAL_SOURCE", "a required eta source scalar contains a nonfinite value")

  # Compare every alias column against its saved group within every retained
  # joint state before any summaries. A mismatch is never averaged away.
  aliases <- unique(cells$shared_alias_key[!is.na(cells$shared_alias_key)])
  for (alias in aliases) {
    rows <- which(!is.na(cells$shared_alias_key) & cells$shared_alias_key == alias)
    source_columns <- match(unique(cells$source_variable[rows]), columns)
    reference <- source_columns[1L]
    if (streaming) source_columns <- source_columns[-1L]
    for (column in source_columns) for (draw in seq_len(n_draws))
      if (draws[draw, column] != draws[draw, reference])
        .mp_abort("E_INDIVIDUAL_SOURCE", "required eta columns disagree within a saved shared-alias group")
  }

  out <- .bh_individual_stats_template(cells, scales)
  stats <- c("mean", "median", "sd", "lower90", "upper90")
  out$n_draws[] <- as.integer(n_draws)
  out$diagnostics_status[] <- "unavailable"
  out$diagnostics_reason[] <- "no_retained_diagnostics_keyed_to_this_cell_and_scale"
  for (i in seq_len(nrow(cells))) {
    cell <- cells[i, , drop = FALSE]
    fixed <- cell$declared_level == "fixed"
    for (s in seq_along(scales)) {
      row <- (i - 1L) * length(scales) + s
      out$estimate_kind[row] <- if (fixed) "known_input" else "retained_state_summary"
      out$uncertainty_status[row] <- if (fixed) "fixed_by_declaration_no_estimation" else "retained_state_variation"
      out$n_sampled_values[row] <- if (fixed) 0L else as.integer(n_draws)
      sd_underflow <- FALSE
      if (fixed) {
        represented <- .bh_individual_stats_fixed(cell, scales[s])
        out$diagnostics_reason[row] <- "fixed_input_has_no_sampled_parameter_diagnostics"
        available <- represented$available
        reason <- represented$reason
        out$n_invalid_fixed_values[row] <- as.integer(!available)
        values <- if (available) c(mean = represented$value, median = represented$value,
          sd = 0, lower90 = represented$value, upper90 = represented$value) else rep(NA_real_, 5L)
        out$known_value[row] <- represented$value
      } else {
        series <- .bh_individual_stats_transform(draws[, match(cell$source_variable, columns)],
          cell$predictor, scales[s], cell$receives)
        available <- series$n_invalid == 0L
        reason <- series$reason
        out$n_invalid_states[row] <- out$n_invalid_transformed_states[row] <- series$n_invalid
        if (available) {
          # Transform first, then calculate each statistic on the full series.
          # Numerical failures of one statistic do not erase other statistics.
          limits <- stats::quantile(series$values, c(.05, .95), type = 7, names = FALSE)
          values <- c(mean = mean(series$values), median = stats::median(series$values),
            sd = stats::sd(series$values), lower90 = limits[1L], upper90 = limits[2L])
          # Base SD can lose nonzero variation when its squared deviations
          # underflow. A nonconstant represented series cannot have exact SD 0.
          sd_underflow <- isTRUE(values[["sd"]] == 0) &&
            any(series$values != series$values[1L])
          if (sd_underflow) values[["sd"]] <- NA_real_
        } else values <- rep(NA_real_, 5L)
        rm(series)
      }
      out$scale_status[row] <- if (available) "available" else "unavailable"
      out$scale_reason[row] <- reason
      for (j in seq_along(stats)) {
        stat <- stats[j]
        good <- available && is.finite(values[j])
        out[[stat]][row] <- if (good) unname(values[j]) else NA_real_
        out[[paste0(stat, "_status")]][row] <- if (good) "available" else "unavailable"
        out[[paste0(stat, "_reason")]][row] <- if (!available) reason else
          if (stat == "sd" && sd_underflow) "sd_arithmetic_underflow" else
          if (!good) paste0(stat, "_arithmetic_not_finite") else NA_character_
      }
      out$status[row] <- if (!available) "unavailable" else if (any(!is.finite(values)))
        "partially_available" else "available"
      out$reason[row] <- if (!available) reason else if (sd_underflow)
        "one_or_more_summary_statistics_unavailable" else if (any(!is.finite(values)))
        "one_or_more_summary_statistics_not_finite" else NA_character_
    }
  }
  out
}
