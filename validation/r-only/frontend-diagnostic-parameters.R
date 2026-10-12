source(file.path("helpers", "native-guard.R"))
# Portable engineering controls against the installed namespace. Prepared
# declarations and invented matrices are not fitted-posterior evidence.
# No source acquisition, generated Stan, compiler, backend or native call.
library(bmeas)

local({
  private <- function(name) getFromNamespace(name, "bmeas")
  passed <- 0L
  ok <- function(value, label) {
    if (!isTRUE(value)) stop(label, call. = FALSE)
    passed <<- passed + 1L
    cat("PASS", label, "\n")
  }
  err <- function(expr, code, label) {
    value <- tryCatch(force(expr), error = identity)
    ok(inherits(value, "measurement_error") && identical(value$code, code), label)
    invisible(value)
  }
  same <- function(x, y) isTRUE(all.equal(x, y, tolerance = 1e-12,
    check.attributes = FALSE))
  state <- function() list(
    rng = if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
      get(".Random.seed", .GlobalEnv, inherits = FALSE) else NULL,
    cwd = getwd(), options = options())
  before <- state()
  # Restore only the bindings installed by a fixture, including existing caller
  # methods. Caller options are observed, never blanket-reset by the harness.
  with_methods <- function(names, fun, expr) {
    present <- vapply(names, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)
    old <- lapply(names[present], get, envir = .GlobalEnv, inherits = FALSE)
    names(old) <- names[present]
    on.exit({
      for (name in names) if (exists(name, .GlobalEnv, inherits = FALSE))
        rm(list = name, envir = .GlobalEnv)
      for (name in names(old)) assign(name, old[[name]], .GlobalEnv)
    }, add = TRUE)
    for (name in names) assign(name, fun, .GlobalEnv)
    force(expr)
  }
  guard_hits <- 0L
  with_guards <- function(names, expr) {
    ns <- asNamespace("bmeas")
    names <- names[vapply(names, exists, logical(1), envir = ns, inherits = FALSE)]
    old <- lapply(names, get, envir = ns, inherits = FALSE)
    locked <- vapply(names, bindingIsLocked, logical(1), env = ns)
    on.exit(for (i in seq_along(names)) {
      if (bindingIsLocked(names[i], ns)) unlockBinding(names[i], ns)
      assign(names[i], old[[i]], ns)
      if (locked[i]) lockBinding(names[i], ns)
    }, add = TRUE)
    for (i in seq_along(names)) {
      if (locked[i]) unlockBinding(names[i], ns)
      assign(names[i], function(...) { guard_hits <<- guard_hits + 1L
        stop("Forbidden diagnostic test route") }, ns)
      if (locked[i]) lockBinding(names[i], ns)
    }
    force(expr)
  }
  forbidden <- c(".bh_compile_scalar", ".bh_fit_scalar", ".bh_cache_scalar_fit",
    ".mp_toolchain_fingerprint", "stancode", "standata", "make_stan_code",
    "compile_measurement", "fit_measurement", "assess_measurement_fit",
    ".bh_posterior_ingest_csv", ".bh_ppc_ingest_csv", ".bh_individual_ingest_csv")
  with_guards(forbidden, {
    # Model constructors remain in their stable installed namespace. In
    # particular, do not source them into the evolving test environment.
    d <- data.frame(person = rep(as.character(1:4), 2),
      x1 = c(.2, .8, .3, 1, .4, .7, .1, .9),
      x2 = c(.7, .6, -.3, .1, .5, .4, -.1, .2),
      x3 = c(-.2, .6, .9, .4, -.1, .8, .7, .5),
      x4 = c(.1, -.6, .3, .5, .2, -.4, .5, .8))
    specs <- list(
      basic = bmeas(bmf(G = ~x1 + x2 + x3), d, subject = "person", fit = FALSE),
      equal = bmeas(bmf(A = ~x1, B = ~x2, C = ~x3, D = ~x4,
        cor = list(default = "zero", equal = list(shared = list(~A:B, ~C:D)))),
        d, subject = "person", fit = FALSE))
    fixed_data <- data.frame(person = as.character(1:4), task = "t",
      y = c(.2, .4, .1, .3), obs = paste0("o", 1:4))
    fixed_block <- observe(gaussian_model(), fixed_data, "person", "task", "y",
      units = "points", observation_id = "obs",
      parameters = list(mu = list(fixed = .2), sigma = list(fixed = 1)))
    specs$fixed <- bmeas(bmf(), list(score = fixed_block), fit = FALSE)
    fits <- lapply(specs, function(spec) structure(list(spec = spec,
      run = structure(list(chains = 2L, iter_warmup = 0L, iter_sampling = 7L,
        reduction = "serial"), class = "measurement_run_config"), cache = list(),
      backend = structure(list(), class = "engineering_backend_double")),
      class = c("bmeas_fit", "bhmm_fit")))
    original <- lapply(fits, serialize, connection = NULL, version = 2L)
    namespaces <- sort(loadedNamespaces())
    cats <- lapply(fits, diagnostic_parameters)
    ok(identical(vapply(cats, nrow, integer(1)), c(basic = 9L, equal = 13L, fixed = 0L)),
      "current Gaussian, equality and fixed-only discovery row counts")
    ok(all(cats$basic$eligible) && !any(cats$basic$fixed) &&
        all(c("Intercept", "loading", "observation_sd") %in% cats$basic$role),
      "exact public role spellings and nonfixed selectable rows")
    rho <- which(cats$equal$role == "unique_marginal_correlation")
    ok(length(rho) == 1L && length(cats$equal$source_aliases[[rho]]) == 2L &&
        cats$equal$prior_coordinate[rho], "one equality primitive retains both source aliases")
    info <- attr(cats$equal, "diagnostic_catalogue", exact = TRUE)
    ok(info$context$admitted_rows == sum(info$context$table_rows) +
        info$context$expanded_alias_references + info$context$grouped_notices &&
        info$catalogue_rows == sum(info$group_counts), "metadata rows and aliases are fully charged")
    ok(is.na(info$exclusions$person_and_basis$known_rows) &&
        info$exclusions$person_and_basis$status == "excluded_not_expanded",
      "unexpanded person counts stay unavailable")
    ok(identical(names(cats$fixed), names(cats$basic)) &&
        attr(cats$fixed, "diagnostic_catalogue")$catalogue_rows == 0,
      "fixed-only context has a typed empty catalogue")
    selection <- rev(cats$basic$key)
    clean_selected <- private(".bh_diagnostic_select")(cats$basic, selection)
    ok(identical(clean_selected$key, selection), "explicit selection keeps request order")
    exact <- list(max_rows = nrow(cats$equal), max_context_rows = info$context$admitted_rows)
    ok(identical(diagnostic_parameters(fits$equal, exact)$key, cats$equal$key),
      "exact metadata allowances admit the full catalogue")
    err(diagnostic_parameters(fits$equal, list(max_rows = exact$max_rows - 1,
      max_context_rows = exact$max_context_rows)), "E_DIAGNOSTIC_BUDGET", "catalogue is never truncated")
    err(diagnostic_parameters(fits$equal, list(max_rows = exact$max_rows,
      max_context_rows = exact$max_context_rows - 1)), "E_DIAGNOSTIC_BUDGET", "expanded context one under refuses")
    err(diagnostic_parameters(specs$basic), "E_FIT", "prepared specification is not a fit")
    err(private(".bh_diagnostic_select")(cats$basic, rep(selection[1], 2)),
      "E_DIAGNOSTIC_SELECTION", "duplicate selection refuses")
    err(private(".bh_diagnostic_select")(cats$basic, "unknown:key"),
      "E_DIAGNOSTIC_SELECTION", "unknown selection refuses")
    err(private(".bh_diagnostic_select")(cats$basic, structure(selection, class = "trap")),
      "E_DIAGNOSTIC_SELECTION", "classed selection refuses")
    err(diagnostic_parameters(fits$basic, list(max_rows = Inf, max_context_rows = 10000)),
      "E_DIAGNOSTIC_BUDGET", "nonfinite metadata allowance refuses")
    bad <- fits$basic
    bad$spec$tables$free_loadings$parameter[1L] <- "changed"
    err(diagnostic_parameters(bad), "E_DIAGNOSTIC_CONTEXT", "conflicting loading maps refuse")
    discovery_methods <- c("anyDuplicated.character", "split.integer", "split.numeric",
      "as.data.frame.character", "as.data.frame.logical", "as.data.frame.AsIs",
      "rbind.data.frame", "[.data.frame", "[.AsIs", "names.data.frame", "dim.data.frame", "$.data.frame")
    method_hits <- 0L
    hostile <- function(...) { method_hits <<- method_hits + 1L; stop("S3 fixture invoked") }
    protected <- with_methods(discovery_methods, hostile, {
      z <- lapply(fits, diagnostic_parameters)
      list(catalogues = z, selected = private(".bh_diagnostic_select")(z$basic, selection))
    })
    ok(method_hits == 0L && identical(protected$catalogues, cats) &&
        identical(protected$selected, clean_selected), "discovery bypasses hostile implicit-class methods")
    ok(identical(namespaces, sort(loadedNamespaces())), "discovery loads no provider namespace")
    ok(identical(original, lapply(fits, serialize, connection = NULL, version = 2L)),
      "discovery preserves every input fit byte")

    dimensions <- private(".bh_diagnostic_metric_dimensions")
    dims <- dimensions(2, 7, 1, 14, 1, 6)
    ok(dims$rows == 14 && dims$split_rows == 12 && dims$omitted_rows == 2 &&
        dims$fft_length == 6 && dims$fft_calls_upper == 32,
      "odd-length full and split counts use the pinned padded FFT schedule")
    ok(all(unlist(dims$costs_outside_numeric_slot_quota) == "unknown") &&
        dims$known_package_owned_slots_total == sum(dims$known_package_owned_slots),
      "finite package slots do not claim a total-memory bound")
    err(dimensions(2, 7, 1, 13, 1, 6), "E_DIAGNOSTIC_ROWS", "complete-row ceiling refuses")
    err(dimensions(2, 7, 1, 14, 0, 6), "E_DIAGNOSTIC_MONITORS", "monitor ceiling refuses")
    err(dimensions(2, 7, 1, 14, 1, 5), "E_DIAGNOSTIC_FFT", "FFT ceiling refuses")
    p0 <- private(".bh_diagnostic_metric_profile")(FALSE)
    ok(is.null(p0$functions) && is.na(p0$versions$posterior) &&
        identical(namespaces, sort(loadedNamespaces())), "HMC-only profile neither loads nor requires posterior")
    hmc <- private(".bh_diagnostic_hmc_metrics")
    energy <- c(1, 2, 4, 3, 6, 2)
    flags <- c(0, 1, 0, 0, 1, 0)
    depth <- c(1, 4, 2, 4, 3, 1)
    limit <- list(status = "available", value = 4L, reason = NA_character_, raw = "# max_depth = 4")
    h <- hmc(flags, depth, energy, 6L, 1L, "nuts", limit)
    ok(h$values[["num_divergent"]] == 2 && h$values[["fraction_divergent"]] == 2/6 &&
        h$values[["num_max_treedepth"]] == 2 && h$values[["fraction_max_treedepth"]] == 2/6,
      "HMC counts use exact retained-state denominators")
    ok(same(h$values[["ebfmi"]], sum(diff(energy)^2)/6/stats::var(energy)),
      "BFMI matches the independent pinned equation without CmdStanR")
    ht <- hmc(flags, depth, energy, 6L, 2L, "nuts", limit)
    ok(ht$metrics$ebfmi$reason == "thinned_energy_not_adjacent_transitions" &&
        ht$values[["num_divergent"]] == 2, "thinned chains keep flags but have no adjacent BFMI")
    hm <- hmc(NULL, NULL, NULL, 6L, 1L, "nuts", NULL)
    ok(hm$status == "unavailable" && all(is.na(hm$values)), "missing HMC fields never become favorable zeros")
    hs <- hmc(flags, NULL, energy, 6L, 1L, "static", NULL)
    ok(hs$metrics$num_max_treedepth$status == "not_applicable", "static HMC has no NUTS depth hits")
    hu <- hmc(flags, depth, rep(c(0, 1e-200), 3), 6L, 1L, "nuts", limit)
    ok(hu$metrics$ebfmi$status == "unavailable" && hu$arithmetic$numerator == 0,
      "BFMI underflow preserves raw arithmetic and refuses a zero estimate")
    err(hmc(c(0, 2), c(1, 2), c(1, 2), 2L, 1L, "nuts", limit),
      "E_DIAGNOSTIC_VALUES", "invalid present divergence flag refuses")

    # Admission does not load posterior. The first required profile is the cold
    # load when it was initially absent, and its option delta is checked directly.
    version <- tryCatch(as.character(utils::packageVersion("posterior")), error = function(e) NA_character_)
    if (identical(version, "1.7.0")) {
      profile_before <- state()
      was_loaded <- isNamespaceLoaded("posterior")
      profile <- private(".bh_diagnostic_metric_profile")(TRUE)
      ok(identical(profile_before, state()), "required profile preserves caller state including first dependency load")
      cat("PROFILE", if (was_loaded) "already loaded" else "cold dependency load", "\n")
      matrices <- list(
        odd = matrix(sin(seq_len(14)*.71) + seq_len(14)/100, 7, 2),
        short = matrix(c(.2, .7, -.1), 3, 1),
        one_chain = matrix(c(.1, .8, .2, -.3, .5, .7), 6, 1),
        constant = matrix(2, 6, 2),
        one_constant = cbind(rep(1, 6), c(.2, .8, .4, -.1, .7, .3)),
        underflow = matrix(rep(c(0, 1e-200, 2e-200), 4), 6, 2))
      reference <- function(x) suppressWarnings(c(mean = mean(x), median = stats::median(x),
        sd = stats::sd(x), q5 = unname(stats::quantile(x, .05, type = 7)),
        q95 = unname(stats::quantile(x, .95, type = 7)),
        rhat = if (nrow(x) >= 4 && ncol(x) >= 2) posterior::rhat(x) else NA_real_,
        ess_bulk = if (nrow(x) >= 6) posterior::ess_bulk(x) else NA_real_,
        ess_tail = if (nrow(x) >= 6) posterior::ess_tail(x) else NA_real_,
        mcse_mean = if (nrow(x) >= 6) posterior::mcse_mean(x) else NA_real_))
      # Direct pinned references and their comparisons precede hostile methods.
      refs <- lapply(matrices, reference)
      results <- lapply(matrices, private(".bh_diagnostic_parameter_metrics"), profile = profile)
      for (name in names(matrices)) {
        raw <- vapply(results[[name]]$metrics, function(x) x$raw, numeric(1))
        ok(same(raw, refs[[name]]), paste(name, "raw metrics equal direct posterior 1.7.0 references"))
      }
      ok(results$odd$counts$rows == 14 && results$odd$counts$split_rows == 12,
        "empirical odd rows remain acquired despite private split omissions")
      ok(results$short$metrics$rhat$reason == "requires_two_original_chains" &&
          results$short$metrics$ess_bulk$reason == "requires_six_rows_per_original_chain" &&
          results$one_chain$metrics$rhat$reason == "requires_two_original_chains",
        "public original-chain and row minima are metric-specific")
      ok(all(vapply(results$constant$metrics[c("rhat", "ess_bulk", "ess_tail", "mcse_mean")],
          function(x) x$status == "unavailable", logical(1))) && results$constant$values[["sd"]] == 0,
        "sampled constant monitor retains empirical spread and undefined convergence evidence")
      ok(identical(results$one_constant$chain_constant, c(TRUE, FALSE)),
        "per-chain constants are distinct from global constancy")
      ok(results$underflow$metrics$sd$reason == "nonconstant_sd_underflow" &&
          results$underflow$metrics$mcse_mean$reason == "nonconstant_sd_underflow_prerequisite",
        "SD underflow invalidates dependent MCSE with raw outputs retained")
      entries <- c("rhat.default", "ess_bulk.default", "ess_tail.default", "mcse_mean.default")
      provider_before <- lapply(entries, get, envir = asNamespace("posterior"), inherits = FALSE)
      methods <- as.vector(outer(c("rhat", "ess_bulk", "ess_tail", "mcse_mean", "ess_mean",
        "ess_quantile", "mean", "median", "quantile", "sort", "as.matrix", "as.array",
        "aperm", "unique", "format", "diff"), c("matrix", "array", "numeric", "integer", "logical"),
        paste, sep = "."))
      protected <- with_methods(methods, hostile, list(
        parameter = private(".bh_diagnostic_parameter_metrics")(matrices$odd, profile),
        hmc = hmc(flags, depth, energy, 6L, 1L, "nuts", limit)))
      ok(method_hits == 0L && identical(protected$parameter, results$odd) && identical(protected$hmc, h),
        "concrete estimator graph and BFMI ignore hostile implicit-class methods")
      ok(identical(provider_before, lapply(entries, get, envir = asNamespace("posterior"), inherits = FALSE)),
        "installed posterior function bodies and environments remain unchanged")
      binding <- private(".bh_diagnostic_metric_bindings")()
      provenance <- private(".bh_diagnostic_metric_binding_provenance")()
      checks <- vapply(names(provenance$cloned_functions), function(alias) {
        address <- strsplit(provenance$cloned_functions[[alias]], ":::", fixed = TRUE)[[1L]]
        original <- get(address[2L], asNamespace(address[1L]), inherits = FALSE)
        clone <- binding[[alias]]
        identical(body(clone), body(original)) && identical(formals(clone), formals(original)) &&
          environmentIsLocked(environment(clone)) && bindingIsLocked(alias, environment(clone))
      }, logical(1))
      ok(all(checks), "all copied concrete bodies and formals match and overlays are locked")
    } else {
      cat("SKIP parameter estimator and concrete-method controls: exact posterior 1.7.0 is unavailable\n")
    }
  })
  ok(guard_hits == 0L && identical(before, state()),
    "whole suite preserves RNG, caller options and cwd with zero guarded calls")
  cat("SUCCESS", passed, "diagnostic parameter assertions; zero source acquisitions\n")
})
