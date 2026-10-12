# Explicit diagnostic calculations, contract v1.3. No source/backend access.
# Stage 1: caller supplies intact validated chain matrices. Provider-managed
# allocations are UNKNOWN; the numeric-slot inventory is not a RAM guarantee.

.bh_diagnostic_metric_plain <- function(x, type) {
  typeof(x) %in% type && !is.object(x) && is.null(attributes(x))
}
.bh_diagnostic_metric_int <- function(x, name, lower = 0, upper = 2^31 - 1) {
  if (!.bh_diagnostic_metric_plain(x, c("integer", "double")) ||
      length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < lower || x > upper)
    .mp_abort("E_DIAGNOSTIC_ARGUMENT", paste(name, "must be a finite exact supported integer"))
  as.double(x)
}
.bh_diagnostic_metric_mul <- function(a, b, name, limit = 2^53 - 1) {
  if (a != 0 && b > floor(limit / a))
    .mp_abort("E_DIAGNOSTIC_CAPABILITY", paste(name, "exceeds checked count representation"))
  ans <- a * b
  if (!is.finite(ans) || ans > limit)
    .mp_abort("E_DIAGNOSTIC_CAPABILITY", paste(name, "exceeds checked count representation"))
  ans
}
.bh_diagnostic_metric_add <- function(a, b, name, limit = 2^53 - 1) {
  if (a > limit - b)
    .mp_abort("E_DIAGNOSTIC_CAPABILITY", paste(name, "exceeds checked count representation"))
  a + b
}
.bh_diagnostic_metric_fields <- function(x, fields) {
  if (typeof(x) != "list" || is.object(x) ||
      !identical(names(attributes(x)), "names")) return(FALSE)
  n <- attr(x, "names", exact = TRUE)
  if (!.bh_diagnostic_metric_plain(n, "character") || length(n) != length(fields) ||
      anyNA(n) || any(!validEnc(n)) || base::anyDuplicated.default(n) || !all(fields %in% n)) return(FALSE)
  # Inspect list slices, never force an empty alist element through [[ or $.
  missing_element <- unname(alist(value = ))
  for (i in seq_along(x)) if (identical(unname(x[i]), missing_element)) return(FALSE)
  TRUE
}

# Exact-body/formals closure copies. Only lexical bindings change. The source
# namespace remains their overlay's parent; no installed binding is modified.
.bh_diagnostic_metric_build_bindings <- function() {
  posterior_names <- c(rhat = "rhat.default", ess_bulk = "ess_bulk.default",
    ess_tail = "ess_tail.default", mcse_mean = "mcse_mean.default",
    ess_mean = "ess_mean.default", ess_quantile = "ess_quantile.default",
    .ess_quantile = ".ess_quantile", .split_chains = ".split_chains",
    z_scale = "z_scale", fold_draws = "fold_draws", .rhat = ".rhat",
    .ess = ".ess", autocovariance = "autocovariance")
  stats_names <- c(median = "median.default", quantile = "quantile.default")
  # Private formatting helpers have varied with R. Resolve this auxiliary only
  # when the installed quantile body actually calls it; otherwise the concrete
  # format binding still covers direct formatting on plain probabilities.
  stats_namespace <- base::asNamespace("stats")
  quantile_original <- base::get("quantile.default", envir = stats_namespace, inherits = FALSE)
  if ("format_perc" %in% base::all.names(body(quantile_original))) {
    if (!base::exists("format_perc", envir = stats_namespace, inherits = FALSE))
      .mp_abort("E_DIAGNOSTIC_PROVIDER", "the installed quantile method requires an unavailable formatting helper")
    stats_names <- c(stats_names, format_perc = "format_perc")
  }
  base_names <- c(apply = "apply")
  maps <- list(posterior = posterior_names, stats = stats_names, base = base_names)
  overlays <- list()
  clone_sources <- character()
  bindings <- list(mean = base::mean.default, sort = base::sort.default,
    unique = base::unique.default, as.matrix = base::as.matrix.default,
    as.array = base::as.array.default, aperm = base::aperm.default,
    format = base::format.default)
  for (package in names(maps)) {
    namespace <- base::asNamespace(package)
    overlay <- base::new.env(parent = namespace)
    overlays[[package]] <- overlay
    map <- maps[[package]]
    for (alias in names(map)) {
      if (!base::exists(map[[alias]], envir = namespace, inherits = FALSE))
        .mp_abort("E_DIAGNOSTIC_PROVIDER", "a reviewed concrete estimator function is unavailable")
      original <- base::get(map[[alias]], envir = namespace, inherits = FALSE)
      if (typeof(original) != "closure" || is.object(original))
        .mp_abort("E_DIAGNOSTIC_PROVIDER", "reviewed estimator binding is not an ordinary closure")
      clone <- original
      environment(clone) <- overlay
      if (!identical(body(clone), body(original)) ||
          !identical(formals(clone), formals(original)))
        .mp_abort("E_DIAGNOSTIC_PROVIDER", "concrete binding changed a reviewed body or formals")
      bindings[[alias]] <- clone
      clone_sources[[alias]] <- paste0(package, ":::", map[[alias]])
    }
  }
  # Every clone sees the same finite map, while retaining access to its own
  # namespace's nongeneric functions and private constants through the parent.
  for (package in names(overlays)) {
    overlay <- overlays[[package]]
    for (alias in names(bindings))
      base::assign(alias, bindings[[alias]], envir = overlay)
    base::lockEnvironment(overlay, bindings = TRUE)
  }
  list(functions = bindings, provenance = list(
    profile = "reviewed_concrete_defaults_v1",
    source_versions = c(posterior = "1.7.0",
      R = paste(R.version$major, R.version$minor, sep = ".")),
    cloned_functions = clone_sources,
    direct_functions = c(mean = "base::mean.default", sort = "base::sort.default",
      unique = "base::unique.default", as.matrix = "base::as.matrix.default",
      as.array = "base::as.array.default", aperm = "base::aperm.default",
      format = "base::format.default"),
    body_formals_equal_at_construction = TRUE,
    overlays_locked_at_construction = TRUE))
}

# Initialize once, on an explicit required profile only. The immutable canonical
# graph permits identity validation without inspecting caller closure bindings
# or forcing a caller-created delayed binding. This cache contains no draws.
.bh_diagnostic_metric_binding_set <- local({
  cache <- base::new.env(parent = emptyenv())
  factory <- function() {
    if (!base::exists("bindings", envir = cache, inherits = FALSE)) {
      value <- .bh_diagnostic_metric_build_bindings()
      base::assign("bindings", value, envir = cache)
      base::lockEnvironment(cache, bindings = TRUE)
    }
    base::get("bindings", envir = cache, inherits = FALSE)
  }
  base::lockEnvironment(environment(factory), bindings = TRUE)
  factory
})
.bh_diagnostic_metric_bindings <- function() {
  .bh_diagnostic_metric_binding_set()$functions
}
.bh_diagnostic_metric_binding_provenance <- function() {
  .bh_diagnostic_metric_binding_set()$provenance
}

# cli may install this process option during posterior's first dependency load.
# Remove only its recognized default when this operation began with neither the
# option nor cli loaded. A distinct intervening caller value remains untouched.
.bh_diagnostic_metric_restore_cli_option <- function(owned) {
  if (!isTRUE(owned) || !base::isNamespaceLoaded("cli")) return(invisible(NULL))
  namespace <- base::getNamespace("cli")
  name <- "cli__default_handler"
  if (base::exists(name, envir = namespace, inherits = FALSE) &&
      !base::bindingIsActive(name, namespace)) {
    handler <- base::get(name, envir = namespace, inherits = FALSE)
    if (is.function(handler) &&
        identical(base::getOption("callr.condition_handler_cli_message"), handler))
      base::options(callr.condition_handler_cli_message = NULL)
  }
  invisible(NULL)
}

# FALSE neither loads nor inspects posterior. Do not save function objects,
# overlays or the private cache in a fit; save only versions/provenance.
.bh_diagnostic_metric_profile <- function(required = TRUE) {
  if (!.bh_diagnostic_metric_plain(required, "logical") ||
      length(required) != 1L || is.na(required))
    .mp_abort("E_DIAGNOSTIC_ARGUMENT", "required must be one plain logical")
  required <- isTRUE(required)
  versions <- list(R = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform, architecture = R.version$arch,
    pointer_bytes = .Machine$sizeof.pointer,
    posterior = NA_character_, matrixStats = NA_character_,
    checkmate = NA_character_, stats = paste(R.version$major, R.version$minor, sep = "."),
    binding_profile = "reviewed_concrete_defaults_v1",
    bfmi_reference = "cmdstanr 0.9.0 formula; package not required or invoked")
  funs <- NULL
  if (required) {
    cli_option_owned <- is.null(base::getOption("callr.condition_handler_cli_message")) &&
      !base::isNamespaceLoaded("cli")
    # Preserve the original return/error/interrupt if cleanup itself cannot be
    # completed. Only the recognized dependency-owned value is ever removed.
    on.exit(base::suspendInterrupts(tryCatch(
      .bh_diagnostic_metric_restore_cli_option(cli_option_owned),
      error = function(e) NULL)), add = TRUE)
    if (!base::requireNamespace("posterior", quietly = TRUE))
      .mp_abort("E_DIAGNOSTIC_PROVIDER", "selected parameter diagnostics require posterior 1.7.0")
    version <- unname(base::getNamespaceVersion("posterior"))
    if (!identical(version, "1.7.0"))
      .mp_abort("E_DIAGNOSTIC_PROVIDER", "selected parameter diagnostics require exactly posterior 1.7.0")
    funs <- .bh_diagnostic_metric_bindings()[c("rhat", "ess_bulk", "ess_tail", "mcse_mean")]
    versions$posterior <- version
    versions$matrixStats <- unname(base::getNamespaceVersion("matrixStats"))
    versions$checkmate <- unname(base::getNamespaceVersion("checkmate"))
  }
  list(id = "posterior_1.7.0_diagnostic_v1", required = required,
    functions = funs, versions = versions)
}
.bh_diagnostic_metric_validate_profile <- function(profile) {
  if (!.bh_diagnostic_metric_fields(profile, c("id", "required", "functions", "versions")) ||
      !identical(profile$required, TRUE) ||
      !.bh_diagnostic_metric_fields(profile$functions,
        c("rhat", "ess_bulk", "ess_tail", "mcse_mean")))
    .mp_abort("E_DIAGNOSTIC_PROVIDER", "use the package-owned required metric profile")
  trusted <- .bh_diagnostic_metric_profile(TRUE)
  if (!identical(profile, trusted))
    .mp_abort("E_DIAGNOSTIC_PROVIDER", "metric profile must contain the canonical concrete bindings and version record")
  invisible(NULL)
}

.bh_diagnostic_metric_dimensions <- function(C, L, M, max_metric_rows,
    max_monitors, max_fft_length) {
  C <- .bh_diagnostic_metric_int(C, "C", 1)
  L <- .bh_diagnostic_metric_int(L, "L", 1)
  M <- .bh_diagnostic_metric_int(M, "M")
  max_metric_rows <- .bh_diagnostic_metric_int(max_metric_rows, "max_metric_rows", 1)
  max_monitors <- .bh_diagnostic_metric_int(max_monitors, "max_monitors")
  max_fft_length <- .bh_diagnostic_metric_int(max_fft_length, "max_fft_length")
  D <- .bh_diagnostic_metric_mul(C, L, "observed rows")
  if (D > max_metric_rows)
    .mp_abort("E_DIAGNOSTIC_ROWS", "observed complete-chain rows exceed max_metric_rows")
  if (M > max_monitors)
    .mp_abort("E_DIAGNOSTIC_MONITORS", "requested scalar monitors exceed max_monitors")
  h <- floor(L / 2)
  split_chains <- .bh_diagnostic_metric_mul(2, C, "split chains")
  A <- .bh_diagnostic_metric_mul(split_chains, h, "split rows")
  rhat_eligible <- M > 0 && C >= 2 && L >= 4
  fft_eligible <- M > 0 && L >= 6
  if ((rhat_eligible || fft_eligible) && split_chains > 2^31 - 1)
    .mp_abort("E_DIAGNOSTIC_CAPABILITY", "split chain count exceeds provider dimensions")
  F <- 0
  if (fft_eligible) {
    # Guard before nextn and before converting any transform dimensions.
    if (h > floor((2^31 - 1) / 4))
      .mp_abort("E_DIAGNOSTIC_CAPABILITY", "twice the padded FFT length cannot be represented")
    padded_half <- stats::nextn(h)
    if (length(padded_half) != 1L || !is.finite(padded_half) ||
        padded_half < h || padded_half != floor(padded_half))
      .mp_abort("E_DIAGNOSTIC_CAPABILITY", "nextn did not yield a supported padded half length")
    F <- .bh_diagnostic_metric_mul(2, padded_half, "FFT length", 2^31 - 1)
    .bh_diagnostic_metric_mul(2, F, "complex FFT real components", 2^31 - 1)
    if (F > max_fft_length)
      .mp_abort("E_DIAGNOSTIC_FFT", "padded transform length exceeds max_fft_length")
  }
  fft_calls <- if (fft_eligible) .bh_diagnostic_metric_mul(16,
    .bh_diagnostic_metric_mul(C, M, "chain-monitor pairs"), "FFT calls") else 0
  ess_calls <- if (fft_eligible) .bh_diagnostic_metric_mul(4, M, "ESS evaluations") else 0
  # These are individually counted buffers. Charge every component together;
  # do not infer provider or native scratch from them. Reader-owned projection,
  # keys, column/alias maps are separate and must remain in its own envelope.
  parameter_active <- as.double(M > 0)
  known_slots <- c(
    monitor_extraction = D * parameter_active,
    monitor_reshape_copy = D * parameter_active,
    monitor_finite_masks = .bh_diagnostic_metric_mul(2, D) * parameter_active,
    global_constant_mask = D * parameter_active,
    chain_extraction_and_constant_mask = .bh_diagnostic_metric_mul(2, L) * parameter_active,
    monitor_chain_loop_indices = C * parameter_active,
    per_chain_constant_flags = .bh_diagnostic_metric_mul(C, M),
    parameter_records = .bh_diagnostic_metric_mul(56, M),
    parameter_numeric_table = .bh_diagnostic_metric_mul(9, M),
    parameter_return_assembly = 36 * parameter_active,
    hmc_extracted_vectors = .bh_diagnostic_metric_mul(3, L),
    hmc_integer_energy_conversion = L,
    hmc_validation_masks = .bh_diagnostic_metric_mul(3, L),
    hmc_constant_mask = L,
    hmc_count_masks = .bh_diagnostic_metric_mul(2, L),
    hmc_difference_indices_and_operands = .bh_diagnostic_metric_mul(4, max(0, L - 1)),
    hmc_differences_and_squares = .bh_diagnostic_metric_mul(2, max(0, L - 1)),
    hmc_records = .bh_diagnostic_metric_mul(46, C),
    hmc_numeric_table = .bh_diagnostic_metric_mul(5, C),
    hmc_return_assembly = 32,
    binding_validation_scalars = 64 * parameter_active,
    metric_accounting_scalars = 256)
  known_total <- 0
  for (slots in known_slots)
    known_total <- .bh_diagnostic_metric_add(known_total, slots, "metric numeric-slot inventory")
  list(chains = C, rows_per_chain = L, rows = D, monitors = M,
    half_rows = h, split_chains = split_chains, split_rows = A,
    omitted_rows = D - A, omitted_per_chain = L - 2 * h,
    rhat_eligible = rhat_eligible, fft_eligible = fft_eligible,
    fft_length = F, fft_real_components = 2 * F,
    ess_evaluations_upper = ess_calls, fft_calls_upper = fft_calls,
    limits = list(max_metric_rows = max_metric_rows, max_monitors = max_monitors,
      max_fft_length = max_fft_length),
    known_package_owned_slots = known_slots,
    known_package_owned_slots_total = known_total,
    costs_outside_numeric_slot_quota = list(provider_temporaries = "unknown",
      R_sort_fft_temporaries = "unknown", native_workspace = "unknown",
      strings_and_object_overhead = "unknown", existing_fit_backend = "unknown",
      peak_RSS = "unknown"))
}

.bh_diagnostic_metric_outcome <- function(raw, evaluated = TRUE) {
  if (!evaluated) "not_evaluated" else if (is.nan(raw)) "NaN" else
    if (is.na(raw)) "NA" else if (raw == Inf) "positive_infinity" else
      if (raw == -Inf) "negative_infinity" else "finite"
}
.bh_diagnostic_metric_record <- function(raw = NA_real_, denominator,
    status = "available", reason = NA_character_, evaluated = TRUE,
    warnings = character()) {
  list(raw = raw, value = if (status == "available") raw else NA_real_,
    outcome = .bh_diagnostic_metric_outcome(raw, evaluated), status = status,
    reason = reason, evaluated = evaluated, denominator = denominator,
    warnings = warnings)
}
.bh_diagnostic_metric_unavailable <- function(reason, denominator,
    status = "unavailable") {
  .bh_diagnostic_metric_record(denominator = denominator, status = status,
    reason = reason, evaluated = FALSE)
}
.bh_diagnostic_metric_classify <- function(raw, metric, denominator,
    global_constant = FALSE, warnings = character()) {
  if (!.bh_diagnostic_metric_plain(raw, c("integer", "double")) || length(raw) != 1L)
    .mp_abort("E_DIAGNOSTIC_ESTIMATOR", paste(metric, "did not return one plain numeric value"))
  reason <- NA_character_
  if (is.nan(raw)) reason <- "undefined_NaN" else if (is.na(raw)) reason <- "undefined_NA" else
    if (!is.finite(raw) && !(metric == "rhat" && raw == Inf))
      reason <- "nonfinite_statistic"
  if (is.finite(raw)) {
    if (metric == "rhat" && raw <= 0) reason <- "nonpositive_rhat"
    if (metric %in% c("sd", "ess_bulk", "ess_tail", "mcse_mean") && raw < 0)
      reason <- "negative_statistic"
    if (metric == "sd" && !global_constant && raw == 0)
      reason <- "nonconstant_sd_underflow"
    if (metric == "mcse_mean" && !global_constant && raw == 0)
      reason <- "nonconstant_mcse_underflow_or_invalid_prerequisite"
  }
  if (global_constant && metric %in% c("rhat", "ess_bulk", "ess_tail", "mcse_mean"))
    reason <- "constant_monitor"
  .bh_diagnostic_metric_record(raw, denominator,
    status = if (is.na(reason)) "available" else "unavailable",
    reason = reason, warnings = warnings)
}
.bh_diagnostic_metric_call <- function(fun, x, name) {
  warnings <- character()
  raw <- tryCatch(withCallingHandlers(fun(x), warning = function(w) {
    warnings <<- c(warnings, .bh_diagnostic_metric_condition_text(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    .mp_abort("E_DIAGNOSTIC_ESTIMATOR", paste(name, "failed:", .bh_diagnostic_metric_condition_text(e)))
  })
  list(raw = raw, warnings = warnings)
}
.bh_diagnostic_metric_condition_text <- function(condition) {
  # Ordinary provider conditions are lists. Bypass conditionMessage and `$`
  # dispatch, preserving plain stored text without invoking a caller method.
  value <- if (typeof(condition) == "list") .subset2(unclass(condition), "message") else NULL
  if (!.bh_diagnostic_metric_plain(value, "character") ||
      length(value) != 1L || is.na(value)) "condition without a plain message" else value
}
.bh_diagnostic_parameter_metrics <- function(x, profile) {
  if (!typeof(x) %in% c("integer", "double") || is.object(x) ||
      !is.matrix(x) || any(!names(attributes(x)) %in% c("dim", "dimnames")))
    .mp_abort("E_DIAGNOSTIC_VALUES", "parameter input must be one plain numeric L-by-C matrix")
  d <- attr(x, "dim", exact = TRUE)
  if (!.bh_diagnostic_metric_plain(d, "integer") || length(d) != 2L)
    .mp_abort("E_DIAGNOSTIC_VALUES", "matrix dimensions must be plain integers")
  L <- .bh_diagnostic_metric_int(d[[1L]], "matrix L", 1)
  C <- .bh_diagnostic_metric_int(d[[2L]], "matrix C", 1)
  D <- .bh_diagnostic_metric_mul(C, L, "matrix observed rows", 2^31 - 1)
  dn <- attr(x, "dimnames", exact = TRUE)
  if (!is.null(dn)) {
    if (typeof(dn) != "list" || is.object(dn) || length(dn) != 2L ||
        any(!names(attributes(dn)) %in% "names"))
      .mp_abort("E_DIAGNOSTIC_VALUES", "matrix dimnames must be a plain list")
    dn_names <- attr(dn, "names", exact = TRUE)
    if (!is.null(dn_names) && (!.bh_diagnostic_metric_plain(dn_names, "character") ||
        length(dn_names) != 2L || anyNA(dn_names)))
      .mp_abort("E_DIAGNOSTIC_VALUES", "matrix dimension names must be plain strings")
    missing_element <- unname(alist(value = ))
    for (j in seq_len(2L)) if (identical(unname(dn[j]), missing_element))
      .mp_abort("E_DIAGNOSTIC_VALUES", "matrix dimnames cannot contain missing arguments")
    for (j in seq_len(2L)) if (!is.null(dn[[j]]) &&
        (!.bh_diagnostic_metric_plain(dn[[j]], "character") || length(dn[[j]]) != d[[j]]))
      .mp_abort("E_DIAGNOSTIC_VALUES", "matrix dimnames must be plain character vectors")
  }
  if (any(!is.finite(x)))
    .mp_abort("E_DIAGNOSTIC_VALUES", "every selected parameter state must be finite")
  dimensions <- .bh_diagnostic_metric_dimensions(C, L, 1,
    max_metric_rows = 2^31 - 1, max_monitors = 1, max_fft_length = 2^31 - 1)
  .bh_diagnostic_metric_validate_profile(profile)
  # Exact source flags are distinct from the provider's numerical tolerance.
  global_constant <- all(x == x[[1L]])
  chain_constant <- logical(C)
  for (j in seq_len(C)) chain_constant[[j]] <- all(x[, j] == x[1L, j])
  metrics <- list()
  concrete <- .bh_diagnostic_metric_bindings()
  empirical <- c(mean = concrete$mean(x), median = concrete$median(x),
    sd = if (D >= 2) stats::sd(x) else NA_real_,
    q5 = concrete$quantile(x, probs = 0.05, names = FALSE, type = 7),
    q95 = concrete$quantile(x, probs = 0.95, names = FALSE, type = 7))
  for (name in names(empirical)) {
    metrics[[name]] <- if (name == "sd" && D < 2)
      .bh_diagnostic_metric_unavailable("insufficient_pooled_states_sd", D) else
      .bh_diagnostic_metric_classify(empirical[[name]], name, D, global_constant)
  }
  for (name in c("rhat", "ess_bulk", "ess_tail", "mcse_mean")) {
    reason <- if (name == "rhat" && C < 2) "requires_two_original_chains" else
      if (name == "rhat" && L < 4) "requires_four_rows_per_original_chain" else
        if (name != "rhat" && L < 6) "requires_six_rows_per_original_chain" else NA_character_
    if (!is.na(reason)) {
      metrics[[name]] <- .bh_diagnostic_metric_unavailable(reason, D)
    } else {
      ans <- .bh_diagnostic_metric_call(profile$functions[[name]], x, name)
      metrics[[name]] <- .bh_diagnostic_metric_classify(ans$raw, name, D,
        global_constant, ans$warnings)
    }
  }
  # Preserve the actual pinned return even when its SD prerequisite is invalid.
  if (metrics$mcse_mean$evaluated && !global_constant &&
      metrics$sd$status != "available") {
    metrics$mcse_mean$value <- NA_real_
    metrics$mcse_mean$status <- "unavailable"
    metrics$mcse_mean$reason <- if (identical(metrics$sd$reason, "nonconstant_sd_underflow"))
      "nonconstant_sd_underflow_prerequisite" else "invalid_sd_prerequisite"
  }
  list(values = vapply(metrics, function(z) z$value, numeric(1)),
    metrics = metrics, global_constant = global_constant,
    chain_constant = chain_constant,
    counts = dimensions[c("chains", "rows_per_chain", "rows", "half_rows",
      "split_chains", "split_rows", "omitted_rows")],
    provenance = list(profile = profile$id, versions = profile$versions,
      method_bindings = .bh_diagnostic_metric_binding_provenance(),
      empirical = "all observed states; sample SD; type-7 q5/q95",
      rhat = "posterior 1.7.0 rhat.default; concrete transitive bindings; intact matrix",
      ess_bulk = "posterior 1.7.0 ess_bulk.default; concrete transitive bindings; intact matrix",
      ess_tail = "posterior 1.7.0 ess_tail.default; minimum 5%/95% ESS; concrete bindings",
      mcse_mean = "posterior 1.7.0 mcse_mean.default; raw-mean ESS; concrete bindings",
      split = "provider private split drops odd middle row; empirical summaries retain it",
      source_constant_flags = "exact equality; independent of provider tolerance",
      scope = "selected sampled monitor; no convergence or inference certification"))
}

# Optional metadata has already been interpreted by the diagnostic reader.
# NULL is absent. Unavailable records retain the exact header evidence; a depth
# value is never inferred from the sample, another chain, or a saved default.
.bh_diagnostic_max_depth <- function(max_depth) {
  if (is.null(max_depth)) return(list(status = "unavailable", value = NA_real_,
    reason = "missing_max_depth", raw = character()))
  fields <- c("status", "value", "reason", "raw")
  if (!.bh_diagnostic_metric_fields(max_depth, fields) ||
      !.bh_diagnostic_metric_plain(max_depth$status, "character") ||
      length(max_depth$status) != 1L || is.na(max_depth$status) || !validEnc(max_depth$status) ||
      !max_depth$status %in% c("available", "unavailable") ||
      !.bh_diagnostic_metric_plain(max_depth$raw, "character") || anyNA(max_depth$raw) || any(!validEnc(max_depth$raw)))
    .mp_abort("E_DIAGNOSTIC_METADATA", "max_depth must be a validated plain optional metadata record")
  if (max_depth$status == "available") {
    value <- .bh_diagnostic_metric_int(max_depth$value, "recorded max_depth", 1)
    if (!identical(max_depth$reason, NA_character_))
      .mp_abort("E_DIAGNOSTIC_METADATA", "available max_depth cannot carry an unavailable reason")
    return(list(status = "available", value = value, reason = NA_character_, raw = max_depth$raw))
  }
  reasons <- c("missing_max_depth", "duplicate_max_depth", "malformed_max_depth", "contradictory_max_depth")
  if (!.bh_diagnostic_metric_plain(max_depth$reason, "character") ||
      length(max_depth$reason) != 1L || is.na(max_depth$reason) || !validEnc(max_depth$reason) ||
      !max_depth$reason %in% reasons ||
      !.bh_diagnostic_metric_plain(max_depth$value, c("integer", "double")) ||
      length(max_depth$value) != 1L || !is.na(max_depth$value) || is.nan(max_depth$value))
    .mp_abort("E_DIAGNOSTIC_METADATA", "unavailable max_depth needs a known reason and an NA value")
  list(status = "unavailable", value = NA_real_, reason = max_depth$reason, raw = max_depth$raw)
}
.bh_diagnostic_hmc_field <- function(x, name, L) {
  if (is.null(x)) return(invisible(NULL))
  if (!.bh_diagnostic_metric_plain(x, c("integer", "double")) ||
      length(x) != L || any(!is.finite(x)))
    .mp_abort("E_DIAGNOSTIC_VALUES", paste(name, "must contain every finite retained row of this chain"))
  if (name == "divergent__" && any(x != 0 & x != 1))
    .mp_abort("E_DIAGNOSTIC_VALUES", "divergent__ must contain only zero or one")
  if (name == "treedepth__" && (any(x < 0) || any(x != floor(x))))
    .mp_abort("E_DIAGNOSTIC_VALUES", "treedepth__ must contain nonnegative finite integers")
  invisible(NULL)
}
.bh_diagnostic_hmc_metrics <- function(divergent, depth, energy, L, thin,
    engine, max_depth) {
  L <- .bh_diagnostic_metric_int(L, "HMC L", 1)
  thin <- .bh_diagnostic_metric_int(thin, "retained thinning", 1)
  if (!.bh_diagnostic_metric_plain(engine, "character") || length(engine) != 1L ||
      is.na(engine) || !validEnc(engine) || !engine %in% c("nuts", "static"))
    .mp_abort("E_DIAGNOSTIC_METADATA", "engine must be the validated nuts or static engine")
  .bh_diagnostic_hmc_field(divergent, "divergent__", L)
  .bh_diagnostic_hmc_field(depth, "treedepth__", L)
  .bh_diagnostic_hmc_field(energy, "energy__", L)
  max_depth <- .bh_diagnostic_max_depth(max_depth)
  metrics <- list()
  if (is.null(divergent)) {
    metrics$num_divergent <- .bh_diagnostic_metric_unavailable("missing_divergent", L)
    metrics$fraction_divergent <- .bh_diagnostic_metric_unavailable("missing_divergent", L)
  } else {
    n <- sum(divergent)
    metrics$num_divergent <- .bh_diagnostic_metric_record(n, L)
    metrics$fraction_divergent <- .bh_diagnostic_metric_record(n / L, L)
  }
  depth_reason <- if (engine == "static") "static_hmc" else
    if (is.null(depth)) "missing_treedepth" else
      if (max_depth$status != "available") max_depth$reason else NA_character_
  if (is.na(depth_reason)) {
    n <- sum(depth >= max_depth$value)
    metrics$num_max_treedepth <- .bh_diagnostic_metric_record(n, L)
    metrics$fraction_max_treedepth <- .bh_diagnostic_metric_record(n / L, L)
  } else {
    status <- if (engine == "static") "not_applicable" else "unavailable"
    metrics$num_max_treedepth <- .bh_diagnostic_metric_unavailable(depth_reason, L, status)
    metrics$fraction_max_treedepth <- .bh_diagnostic_metric_unavailable(depth_reason, L, status)
  }
  energy_constant <- if (is.null(energy)) NA else all(energy == energy[[1L]])
  arithmetic <- list(sum_squared_differences = NA_real_, numerator = NA_real_,
    variance = NA_real_, raw_ratio = NA_real_)
  reason <- if (is.null(energy)) "missing_energy" else
    if (L < 3) "requires_three_retained_rows" else
      if (thin != 1) "thinned_energy_not_adjacent_transitions" else NA_character_
  if (!is.na(reason)) {
    metrics$ebfmi <- .bh_diagnostic_metric_unavailable(reason, L)
  } else {
    if (typeof(energy) == "integer") energy <- as.double(energy)
    delta <- base::diff.default(energy)
    squares <- delta^2
    arithmetic$sum_squared_differences <- sum(squares)
    arithmetic$numerator <- arithmetic$sum_squared_differences / L
    arithmetic$variance <- stats::var(energy)
    arithmetic$raw_ratio <- arithmetic$numerator / arithmetic$variance
    reason <- if (energy_constant) "constant_energy" else
      if (!is.finite(arithmetic$sum_squared_differences) || !is.finite(arithmetic$numerator))
        "energy_numerator_overflow" else
      if (!is.finite(arithmetic$variance)) "energy_variance_overflow" else
      if (arithmetic$numerator == 0) "energy_numerator_underflow" else
      if (arithmetic$variance <= 0) "energy_variance_underflow_or_invalid_denominator" else
      if (!is.finite(arithmetic$raw_ratio)) "nonfinite_ebfmi" else
      if (arithmetic$raw_ratio == 0) "ebfmi_underflow" else
      if (arithmetic$raw_ratio < 0) "negative_ebfmi" else NA_character_
    metrics$ebfmi <- .bh_diagnostic_metric_record(arithmetic$raw_ratio, L,
      status = if (is.na(reason)) "available" else "unavailable", reason = reason)
  }
  coverage <- lapply(list(divergent = divergent, depth = depth, energy = energy),
    function(x) list(present = !is.null(x), retained = L,
      finite = if (is.null(x)) 0 else L, missing = if (is.null(x)) L else 0))
  statuses <- vapply(metrics, function(z) z$status, character(1))
  status <- if (all(statuses %in% c("available", "not_applicable"))) "computed" else
    if (any(statuses == "available")) "partial" else "unavailable"
  list(values = vapply(metrics, function(z) z$value, numeric(1)), metrics = metrics,
    status = status, coverage = coverage, energy_constant = energy_constant,
    counts = list(rows = L, thin = thin, adjacent_pairs = L - 1),
    engine = engine, max_depth = max_depth, arithmetic = arithmetic,
    provenance = list(bfmi = "sum(diff(energy)^2)/L divided by sample variance; CmdStanR 0.9.0 formula",
      adjacency = "requires source-validated complete saved ranks and thin=1",
      scope = "one original chain's retained states; no all-transition or numerical-success claim"))
}
