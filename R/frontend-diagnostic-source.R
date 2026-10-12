# Private opt-in diagnostic adapter for the established seven-pass CSV reader.
# All allocations below are numeric-slot reservations, not a total RAM bound.
# The source reader calls only these fixed package-owned functions; no callback
# or arbitrary sampler column can enter this profile.
.bh_diagnostic_source_profile <- function(x, variables) {
  fields <- c("monitors", "max_metric_rows", "max_monitors", "max_fft_length")
  if (!.bh_diagnostic_metric_fields(x, fields))
    .bh_posterior_abort("E_DIAGNOSTIC_ARGUMENT", "diagnostic source profile must use the package-owned plain fields")
  out <- list()
  for (key in fields) out[[key]] <- .bh_diagnostic_metric_int(x[[key]], key,
    if (key == "max_metric_rows") 1 else 0)
  if (out$monitors > out$max_monitors)
    .bh_posterior_abort("E_DIAGNOSTIC_MONITORS", "requested scalar monitors exceed max_monitors")
  if (out$monitors > length(variables) || (out$monitors == 0 && length(variables)))
    .bh_posterior_abort("E_DIAGNOSTIC_ARGUMENT", "population columns must cover the requested scalar monitors and aliases")
  out
}
.bh_diagnostic_source_allowlist <- function() {
  c("divergent__", "treedepth__", "energy__")
}

# Called at minimum admission, then for each declared header layout, and again
# with observed complete rows before projection. All-state acquisition makes
# L=D/C exact; there is no balancing, second thinning or second monitor cube.
.bh_diagnostic_source_slots <- function(tx, D) {
  profile <- tx$diagnostic
  if (D %% tx$chains != 0)
    .bh_posterior_abort("E_DIAGNOSTIC_LAYOUT", "diagnostic acquisition requires equal complete original chains")
  L <- D / tx$chains
  dimensions <- .bh_diagnostic_metric_dimensions(tx$chains, L, profile$monitors,
    profile$max_metric_rows, profile$max_monitors, profile$max_fft_length)
  C <- tx$chains; M <- profile$monitors
  integration <- c(
    current_chain_row_indices = L,
    canonical_column_indices = M,
    optional_sampler_column_indices = 3,
    selected_catalogue_logical_flags = .bh_posterior_mul(3, M),
    copied_chain_reporting_metadata = .bh_posterior_mul(8, C),
    controller_reporting_assembly = 256,
    optional_depth_manifest_overlap = .bh_posterior_mul(4, C),
    source_profile_and_scan_scalars = 64)
  tx$diagnostic_dimensions <- dimensions
  tx$receipt$diagnostic <- list(profile = "population_hmc_all_states_v1",
    dimensions = dimensions,
    dimension_basis = if (isTRUE(tx$diagnostic_observed_layout))
      "observed equal complete-chain row counts before projection" else
      if (is.null(tx$diagnostic_columns)) "minimum unavoidable layout before header observation" else
        "declared chain layout pending complete source validation",
    selected_population_columns = length(tx$population_variables),
    selected_scalar_columns = length(tx$variables),
    optional_sampler_allowlist = .bh_diagnostic_source_allowlist(),
    optional_sampler_columns = tx$diagnostic_sampler_columns,
    integration_numeric_slots = base::as.list.default(integration),
    integration_scope = paste("current-chain row indices, canonical/sampler indices,",
      "selected catalogue logical flags, copied chain metadata, reporting assembly,",
      "optional depth manifests and fixed source-profile scalar workspace"),
    all_state_conversion_formula = "2 * observed_rows * selected_scalar_columns",
    scan_limit_basis = if (!isTRUE(tx$diagnostic_scan_default)) "explicit caller ceiling" else
      if (is.null(tx$diagnostic_columns)) "finite preheader planning ceiling for population columns plus three optional sampler fields" else
        "finite default: 2 * max_source_rows * actual selected scalar columns",
    unknown_costs = dimensions$costs_outside_numeric_slot_quota)
  c(setNames(dimensions$known_package_owned_slots,
      paste0("diagnostic_metric:", names(dimensions$known_package_owned_slots))),
    setNames(integration, paste0("diagnostic_integration:", names(integration))))
}

# Full headers stay identical across chains, including optional sampler fields.
# The population prefix (and therefore every alias index) never changes.
.bh_diagnostic_source_columns <- function(tx, columns) {
  if (is.null(tx$diagnostic_columns)) {
    tx$diagnostic_columns <- columns
    allowlist <- .bh_diagnostic_source_allowlist()
    tx$diagnostic_sampler_columns <- allowlist[allowlist %in% columns]
    tx$variables <- c(tx$population_variables, tx$diagnostic_sampler_columns)
    tx$diagnostic_sampler_positions <- match(allowlist, tx$variables)
    tx$receipt$variables <- tx$variables
    if (isTRUE(tx$diagnostic_scan_default)) {
      tx$limits$max_scan_values <- .bh_posterior_mul(.bh_posterior_mul(2,
        tx$limits$max_source_rows), length(tx$variables), "observed diagnostic default scan values")
      tx$receipt$budget$max_scan_values <- tx$limits$max_scan_values
    }
  } else if (!identical(columns, tx$diagnostic_columns)) {
    .bh_posterior_abort("E_PPC_SOURCE_CHAINS", "chain headers describe inconsistent sampling layouts")
  }
  invisible(NULL)
}
.bh_diagnostic_source_validate_row <- function(tx, values) {
  at <- tx$diagnostic_sampler_positions
  if (!is.na(at[[1L]])) {
    value <- values[[at[[1L]]]]
    if (value != 0 && value != 1)
      .bh_posterior_abort("E_DIAGNOSTIC_VALUES", "divergent__ must contain only zero or one in every retained row")
  }
  if (!is.na(at[[2L]])) {
    value <- values[[at[[2L]]]]
    if (value < 0 || value != floor(value))
      .bh_posterior_abort("E_DIAGNOSTIC_VALUES", "treedepth__ must contain nonnegative finite integers in every retained row")
  }
  # The common reader's strict finite check covers energy and both flags first.
  invisible(NULL)
}

# These frames contain package-owned scalar columns. Avoid data.frame()'s
# as.data.frame dispatch on implicit character/numeric classes, and avoid
# caller methods for the classed frame during acquisition and receipt assembly.
.bh_diagnostic_source_frame <- function(columns, rows) {
  structure(columns, class = "data.frame",
    row.names = if (rows) c(NA_integer_, -as.integer(rows)) else integer())
}
.bh_diagnostic_source_frame_rows <- function(x, rows) {
  columns <- unclass(x)
  for (name in names(columns)) columns[[name]] <- .subset(columns[[name]], rows)
  .bh_diagnostic_source_frame(columns, length(rows))
}
.bh_diagnostic_source_frame_add <- function(x, name, value) {
  columns <- unclass(x)
  columns[[name]] <- value
  .bh_diagnostic_source_frame(columns, length(value))
}
.bh_diagnostic_source_file_test <- function(paths) {
  # Match the established utils::file_test("-f") predicate exactly, without
  # dispatching `$` on the returned data.frame. This remains a separate probe.
  info <- unclass(base::file.info(paths, extra_cols = FALSE))
  isdir <- info[["isdir"]]
  !is.na(isdir) & !isdir
}
.bh_diagnostic_source_condition <- function(condition, receipt) {
  if (typeof(condition) != "list") return(condition)
  fields <- unclass(condition)
  fields[["receipt"]] <- receipt
  saved <- attributes(condition)
  saved[["names"]] <- names(fields)
  attributes(fields) <- saved
  fields
}

# Preserve exact comment lines. Repeated agreeing values are duplicates;
# differing valid values are contradictory; malformed or misplaced evidence
# is unavailable locally. No observed depth or saved default supplies a value.
.bh_diagnostic_source_max_depth <- function(raw, misplaced) {
  if (!length(raw)) return(list(status = "unavailable", value = NA_real_,
    reason = "missing_max_depth", raw = raw))
  malformed <- misplaced; contradictory <- FALSE; first <- NA_real_
  for (line in raw) {
    text <- trimws(sub("^#", "", line))
    if (!grepl("^max_depth[[:space:]]*=", text)) {
      malformed <- TRUE
      next
    }
    token <- trimws(sub("^max_depth[[:space:]]*=", "", text))
    token <- trimws(sub("[[:space:]]*\\(Default\\)[[:space:]]*$", "", token))
    if (!grepl("^[0-9]+$", token)) {
      malformed <- TRUE
      next
    }
    value <- suppressWarnings(as.double(token))
    if (!is.finite(value) || value < 1 || value > .Machine$integer.max) {
      malformed <- TRUE
      next
    }
    if (is.na(first)) first <- value else if (value != first) contradictory <- TRUE
  }
  reason <- if (malformed) "malformed_max_depth" else
    if (contradictory) "contradictory_max_depth" else
      if (length(raw) > 1L) "duplicate_max_depth" else NA_character_
  list(status = if (is.na(reason)) "available" else "unavailable",
    value = if (is.na(reason)) first else NA_real_, reason = reason, raw = raw)
}

# The public controller resolves semantic selectors and provider availability
# before calling this adapter. Numeric integration costs are the fixed reviewed
# inventory above, not caller-provided functions or guessed provider memory.
.bh_diagnostic_ingest_csv <- function(files, variables, posterior, monitors,
    max_metric_rows, max_monitors, max_fft_length, alias_groups = list(),
    expected_model = NULL, expected_chains = NULL, original_hashes = NULL,
    context_buffers = numeric(), chunk_bytes = 65536L, archive_hashes = NULL) {
  .bh_posterior_ingest_csv(files = files, variables = variables,
    posterior = posterior, states = "all", alias_groups = alias_groups,
    expected_model = expected_model, expected_chains = expected_chains,
    original_hashes = original_hashes, chunk_bytes = chunk_bytes, archive_hashes = archive_hashes,
    consumer_buffers = list(fixed = context_buffers, per_selected_row = numeric()),
    diagnostic = list(monitors = monitors, max_metric_rows = max_metric_rows,
      max_monitors = max_monitors, max_fft_length = max_fft_length))
}
