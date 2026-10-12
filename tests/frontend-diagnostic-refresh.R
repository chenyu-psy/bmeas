source(file.path("helpers", "native-guard.R"))
# Portable engineering CSV/controller controls, using current installed model
# constructors. At most four reader attempts, two chains x seven rows x eight
# columns each; no fitting, generated Stan, compiler or backend provider calls.
library(bmeas)

local({
  ns <- asNamespace("bmeas")
  private <- function(name) get(name, ns, inherits = FALSE)
  passed <- 0L
  ok <- function(value, label) {
    if (!isTRUE(value)) stop(label, call. = FALSE)
    passed <<- passed + 1L
    cat("PASS", label, "\n")
  }
  same <- function(x, y) isTRUE(all.equal(x, y, tolerance = 1e-12, check.attributes = FALSE))
  state <- function() list(
    rng = if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
      get(".Random.seed", .GlobalEnv, inherits = FALSE) else NULL,
    cwd = getwd(), options = options())
  before <- state()
  owned_directories <- function() sort(list.files(tempdir(), pattern = "^bmeas-streaming-", full.names = TRUE))
  owned_before <- owned_directories()
  with_bindings <- function(replacements, expr, env = ns) {
    fields <- names(replacements)
    original <- lapply(fields, get, envir = env, inherits = FALSE)
    locked <- vapply(fields, bindingIsLocked, logical(1), env = env)
    on.exit(for (i in seq_along(fields)) {
      if (bindingIsLocked(fields[i], env)) unlockBinding(fields[i], env)
      assign(fields[i], original[[i]], env)
      if (locked[i]) lockBinding(fields[i], env)
    }, add = TRUE)
    for (i in seq_along(fields)) {
      if (locked[i]) unlockBinding(fields[i], env)
      assign(fields[i], replacements[[i]], env)
      if (locked[i]) lockBinding(fields[i], env)
    }
    force(expr)
  }
  guard_hits <- 0L
  forbidden <- function(...) { guard_hits <<- guard_hits + 1L; stop("Forbidden diagnostic test route") }
  terminal_names <- c(".bh_compile_scalar", ".bh_fit_scalar", ".bh_cache_scalar_fit",
    ".mp_toolchain_fingerprint", "stancode", "standata", "make_stan_code",
    "compile_measurement", "fit_measurement", "assess_measurement_fit",
    ".bh_ppc_ingest_csv", ".bh_individual_ingest_csv")
  terminal_names <- terminal_names[vapply(terminal_names, exists, logical(1), envir = ns, inherits = FALSE)]
  guards <- setNames(rep(list(forbidden), length(terminal_names)), terminal_names)
  fixture_dir <- tempfile("bmeas-diagnostic-engineering-")
  stopifnot(dir.create(fixture_dir))
  on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)
  attempts <- character()
  acquired <- list()
  active <- NULL
  real_reader <- private(".bh_posterior_ingest_csv")
  real_diagnostic_reader <- private(".bh_diagnostic_ingest_csv")
  reader_counter <- function(...) {
    if (is.null(active) || length(attempts) >= 4L || active %in% attempts)
      stop("Unexpected or repeated source acquisition")
    attempts <<- c(attempts, active)
    real_reader(...)
  }
  capture_reader <- function(...) {
    value <- real_diagnostic_reader(...)
    acquired[[active]] <<- value
    value
  }
  guards$.bh_posterior_ingest_csv <- reader_counter
  guards$.bh_diagnostic_ingest_csv <- capture_reader
  with_bindings(guards, {
    d <- data.frame(person = rep(as.character(1:4), 2),
      x1 = c(.2, .8, .3, 1, .4, .7, .1, .9), x2 = c(.7, .6, -.3, .1, .5, .4, -.1, .2),
      x3 = c(-.2, .6, .9, .4, -.1, .8, .7, .5), x4 = c(.1, -.6, .3, .5, .2, -.4, .5, .8))
    spec <- bmeas(bmf(A = ~x1, B = ~x2, C = ~x3, D = ~x4,
      cor = list(default = "zero", equal = list(shared = list(~A:B, ~C:D)))),
      d, subject = "person", fit = FALSE)
    columns <- c("lp__", "alpha.1", "obs_sd.1", "R_component.1.2", "R_component.3.4",
      "divergent__", "treedepth__", "energy__")
    values <- function(id) {
      row <- seq_len(7L)
      rho <- .35*sin(row*.61 + id*.07)
      cbind(-1 - row/10, sin(row*.71 + id*.11) + row/100,
        .8 + .05*cos(row*.4 + id), rho, rho,
        as.double(row == 7 & id == 3), (row + id) %% 6,
        2 + .3*sin(row*.37 + id) + row*.09)
    }
    fixture <- function(id, bad = FALSE) {
      rows <- values(id)
      if (bad) rows[7L, 6L] <- 2
      depth <- if (id == 11) rep("# max_depth = 4", 2L) else "# max_depth = 4"
      lines <- c("# ENGINEERING FIXTURE: invented deterministic values, no posterior inference",
        "# stan_version_major = 2", "# stan_version_minor = 40", "# stan_version_patch = 0",
        "# model = scalar_components_model", paste0("# id = ", id),
        "# method = sample (Default)", "# num_samples = 7", "# num_warmup = 0",
        "# save_warmup = false (Default)", "# thin = 1", "# algorithm = hmc (Default)",
        "# engine = nuts (Default)", "# metric = diag_e (Default)", "# sig_figs = 18", depth,
        paste(columns, collapse = ","),
        apply(rows, 1L, function(row) paste(sprintf("%.17g", row), collapse = ",")),
        "# Elapsed Time: 0 seconds (Warm-up)", "# 0.01 seconds (Sampling)", "# 0.01 seconds (Total)")
      path <- file.path(fixture_dir, paste0("chain-", id, if (bad) "-invalid" else "", ".csv"))
      writeLines(lines, path, useBytes = TRUE)
      path
    }
    paths <- c(fixture(11), fixture(3))
    invalid <- fixture(3, TRUE)
    all_files <- c(paths, invalid)
    input_bytes <- tools::md5sum(all_files)
    B <- sum(file.info(paths)$size)
    ok(B <= 131072 && sum(file.info(c(paths[1L], invalid))$size) <= 131072,
      "each actual source set fits the 128 KiB envelope")
    policy <- list(reader = "streaming", max_source_bytes = 131072,
      max_read_bytes = 896*1024, max_materialized_values = 1000000,
      max_record_bytes = 16384, max_columns = 20, max_source_rows = 32,
      max_chains = 4, max_temp_bytes = 2*131072)
    budget <- list(posterior = policy, max_monitors = 3, max_metric_rows = 32,
      max_fft_length = 8, max_catalogue_rows = 100, max_context_rows = 10000)
    hmc_budget <- budget
    hmc_budget$max_monitors <- 0
    hmc_budget$max_fft_length <- 0
    shell <- function(files = paths) structure(list(spec = spec,
      run = structure(list(chains = 2L, iter_warmup = 0L, iter_sampling = 7L,
        reduction = "serial"), class = "measurement_run_config"),
      code = "ENGINEERING SOURCE IDENTITY MARKER; NO GENERATED OR EXECUTED STAN",
      provenance = list(kind = "engineering_synthetic_fixture_no_posterior_qualification",
        model_fingerprint = spec$fingerprint, output_files = files, execution_profile = "engineering_only"),
      cache = list(computation = list(status = "engineering_only", reason = "Invented values"),
        inference = list(status = "not_assessed", reason = "No inference qualification"),
        initialization = list(status = "engineering_marker"),
        ppc = list(engineering_marker = "preserve unrelated cache"),
        posterior_reporting = list(version = "bmeas_posterior_reporting_v1",
          status = "deferred_by_policy", requested_cache = "deferred",
          posterior_diagnostics = "uncomputed", observed_csv_rows = NA_real_,
          observed_csv_rows_availability = "not_observed", reason = "Engineering deferred cache",
          phases = setNames(rep("uncomputed", 6L), c("parameter_summary", "hmc", "conditioning",
            "availability_and_covariance", "expanded_summary", "numerical_messages")))),
      backend = structure(list(), class = "engineering_backend_double")),
      class = c("bmeas_fit", "bhmm_fit"))
    fit <- shell()
    catalogue <- diagnostic_parameters(fit)
    roles <- c("unique_marginal_correlation", "observation_sd", "Intercept")
    keys <- vapply(roles, function(role) catalogue$key[which(catalogue$role == role)[1L]], character(1))
    keys <- unname(keys)
    ok(length(keys) == 3L && !anyNA(keys) && !anyDuplicated(keys),
      "three exact public semantic keys selected in deliberate order")
    selection <- private(".bh_diagnostic_select")(catalogue, keys)
    variables <- unique(unlist(selection$source_aliases, use.names = FALSE))
    # One attempt is one call into the real package-owned streaming reader.
    run_refresh <- function(label, input, selected, cap) {
      bytes <- serialize(input, NULL, version = 2L)
      saved_state <- state()
      owned <- owned_directories()
      active <<- label
      on.exit(active <<- NULL, add = TRUE)
      result <- tryCatch(refresh_diagnostics(input, selected, cap), error = identity, interrupt = identity)
      ok(identical(bytes, serialize(input, NULL, version = 2L)), paste(label, "input fit is atomic"))
      ok(identical(saved_state, state()) && identical(owned, owned_directories()),
        paste(label, "RNG options cwd and owned temporary directories unchanged"))
      result
    }
    namespaces <- sort(loadedNamespaces())
    hmc_fit <- run_refresh("hmc_only", fit, character(), hmc_budget)
    ok(inherits(hmc_fit, "bmeas_fit") && identical(namespaces, sort(loadedNamespaces())),
      "HMC-only refresh succeeds without loading a provider")
    ok(nrow(parameter_summary(hmc_fit)) == 0L &&
        hmc_fit$cache$posterior_reporting$phases$parameter_summary$status == "not_requested",
      "HMC-only refresh saves an empty parameter table and explicit phase")
    hmc_records <- hmc_fit$cache$posterior_reporting$hmc_metrics
    ok(hmc_records[["11"]]$max_depth$reason == "duplicate_max_depth" &&
        hmc_records[["3"]]$metrics$num_max_treedepth$status == "available" &&
        hmc_fit$cache$posterior_reporting$phases$hmc$status == "partial",
      "optional duplicate depth metadata is chain-local")
    for (id in c("3", "11")) {
      expected <- values(as.integer(id))
      record <- hmc_records[[id]]
      ok(record$values[["num_divergent"]] == sum(expected[, 6L]) &&
          same(record$values[["ebfmi"]], sum(diff(expected[, 8L])^2)/7/stats::var(expected[, 8L])),
        paste("chain", id, "HMC equals independent flag and BFMI references"))
    }
    provider_version <- tryCatch(as.character(utils::packageVersion("posterior")), error = function(e) NA_character_)
    selected_fit <- NULL
    if (identical(provider_version, "1.7.0")) {
      selected_fit <- run_refresh("selected", fit, keys, budget)
      ok(inherits(selected_fit, "bmeas_fit") &&
          identical(parameter_summary(selected_fit)$parameter_key, keys),
        "selected refresh preserves semantic request order")
      ok(identical(selected_fit$cache$ppc, fit$cache$ppc) &&
          identical(selected_fit$cache$initialization, fit$cache$initialization) &&
          identical(selected_fit$backend, fit$backend) && identical(selected_fit$provenance, fit$provenance),
        "selected refresh preserves unrelated cache backend and provenance")
    } else {
      cat("SKIP selected-parameter refresh controls: exact posterior 1.7.0 is unavailable\n")
    }
    # The ordinary reader keeps its default profile. Optional depth comments
    # have no default-reader meaning. Compare every value/key on these same
    # files, including the two equality columns and reversed path/chunk order.
    active <- "default_reader"
    default <- private(".bh_posterior_ingest_csv")(rev(paths), variables, policy,
      states = "all", alias_groups = list(1:2), expected_model = "scalar_components_model",
      expected_chains = 2, chunk_bytes = 47L)
    active <- NULL
    oracle <- lapply(rev(paths), function(path) utils::read.csv(path, comment.char = "#", check.names = FALSE))
    legacy_columns <- c("R_component.1.2", "R_component.3.4", "obs_sd.1", "alpha.1")
    expected_draws <- matrix(NA_real_, 14L, 4L, dimnames = list(NULL, variables))
    for (j in seq_along(legacy_columns))
      expected_draws[, j] <- as.vector(t(vapply(oracle, function(tab) tab[[legacy_columns[j]]], numeric(7L))))
    expected_keys <- data.frame(chain_id = rep(c(3L, 11L), 7L), saved_row = rep(1:7, each = 2L))
    ok(identical(default$draws, expected_draws) && identical(default$draw_keys, expected_keys),
      "default reader retains exact independent values and all-row actual keys")
    ok(identical(names(default), c("draws", "draw_keys", "receipt")) &&
        is.null(default$receipt$diagnostic) && default$receipt$buffer_schedule_version == 1L,
      "default reader preserves its non-diagnostic return and accounting profile")
    if (!is.null(selected_fit)) {
      ok(identical(acquired$selected$draws[, variables, drop = FALSE], default$draws) &&
          identical(acquired$selected$draw_keys, default$draw_keys),
        "diagnostic projection equals the unchanged default reader on common columns")
      table <- parameter_summary(selected_fit)
      for (i in seq_along(keys)) {
        x <- matrix(default$draws[, table$variable[i]], 7L, 2L, byrow = TRUE)
        refs <- c(mean = mean(x), median = stats::median(x), sd = stats::sd(x),
          q5 = unname(stats::quantile(x, .05, type = 7)), q95 = unname(stats::quantile(x, .95, type = 7)),
          rhat = posterior::rhat(x), ess_bulk = posterior::ess_bulk(x),
          ess_tail = posterior::ess_tail(x), mcse_mean = posterior::mcse_mean(x))
        raw <- vapply(selected_fit$cache$posterior_reporting$parameter_metrics[[keys[i]]]$metrics,
          function(record) record$raw, numeric(1))
        ok(same(raw, refs), paste("selected monitor", i, "matches intact-matrix direct pinned references"))
      }
    }
    invalid_fit <- shell(c(paths[1L], invalid))
    refused <- run_refresh("invalid_source", invalid_fit, character(), hmc_budget)
    ok(inherits(refused, "measurement_error") && identical(refused$code, "E_DIAGNOSTIC_VALUES") &&
        !is.null(refused$receipt) && refused$receipt$projected_rows == 0,
      "invalid retained divergence refuses with a bounded pre-projection receipt")
    for (label in names(acquired)) {
      a <- acquired[[label]]
      receipt <- a$receipt
      ok(identical(a$draw_keys, expected_keys) && nrow(a$draws) == 14L &&
          receipt$source_rows == 14 && receipt$selected_rows == 14 && receipt$projected_rows == 14,
        paste(label, "acquires every observed state using actual chain IDs"))
      ok(receipt$source_bytes == B && receipt$read_bytes_charged == 7*B &&
          all(unlist(receipt$read_passes) == B) &&
          receipt$required_numeric_conversions == 2*14*ncol(a$draws),
        paste(label, "charges all seven passes and every required conversion"))
      ok(receipt$max_materialized_values == sum(unlist(receipt$buffer_schedule)) &&
          receipt$max_materialized_values <= 1000000 && !isTRUE(receipt$reader$provider_called),
        paste(label, "itemized finite numeric slots and no backend provider"))
      ok(a$diagnostic$dimensions$split_rows == 12 && a$diagnostic$dimensions$omitted_rows == 2,
        paste(label, "odd split omissions are distinct from acquired rows"))
    }
    ok(default$receipt$read_bytes_charged == 7*B &&
        default$receipt$required_numeric_conversions == 2*14*4 &&
        default$receipt$alias_validation$comparisons == 14,
      "default-reader work and alias equality retain exact all-row accounting")
    if (!is.null(selected_fit)) ok(acquired$selected$receipt$alias_validation$comparisons == 14 &&
      acquired$selected$receipt$diagnostic$dimensions$monitors == 3,
      "one equality alias adds source work without adding a monitor")

    # Everything below is zero-acquisition: argument refusals, saved records,
    # serialization replay or explicitly labelled in-memory lifecycle doubles.
    count_before <- length(attempts)
    # Labelled header-layout double: no files or parser are involved. A source
    # with no optional sampler fields still needs rows, keys and numeric slots.
    zero <- new.env(parent = emptyenv())
    zero$diagnostic_columns <- NULL
    zero$population_variables <- character()
    zero$diagnostic_scan_default <- TRUE
    zero$limits <- list(max_source_rows = 32)
    zero$receipt <- list(budget = list())
    zero$chains <- 2
    zero$diagnostic_observed_layout <- TRUE
    zero$diagnostic <- list(monitors = 0, max_metric_rows = 32, max_monitors = 0, max_fft_length = 0)
    private(".bh_diagnostic_source_columns")(zero, c("lp__", "alpha[1]"))
    zero_slots <- private(".bh_diagnostic_source_slots")(zero, 14)
    ok(length(zero$variables) == 0L && zero$limits$max_scan_values == 0 &&
        zero$diagnostic_dimensions$rows == 14 && zero$diagnostic_dimensions$fft_length == 0 &&
        sum(zero_slots) > 0, "zero-column header double retains full-row and key-work costs")
    expect_error <- function(expr, code, label) {
      result <- tryCatch(force(expr), error = identity)
      ok(inherits(result, "measurement_error") && identical(result$code, code), label)
      invisible(result)
    }
    with_bindings(list(.bh_diagnostic_ingest_csv = forbidden), {
      expect_error(refresh_diagnostics(fit, rep(keys[1], 2), budget),
        "E_DIAGNOSTIC_SELECTION", "duplicate public selector refuses before acquisition")
      expect_error(refresh_diagnostics(fit, "unknown:key", budget),
        "E_DIAGNOSTIC_SELECTION", "unknown public selector refuses before acquisition")
      bad <- budget
      bad$max_catalogue_rows <- 0
      expect_error(refresh_diagnostics(fit, character(), bad),
        "E_DIAGNOSTIC_BUDGET", "catalogue allowance refuses before identity or acquisition")
      bad <- fit
      bad$run$chains <- 3L
      expect_error(refresh_diagnostics(bad, character(), hmc_budget),
        "E_DIAGNOSTIC_SOURCE", "retained chain count contradicting paths refuses before acquisition")
    })
    project <- function(x) fit_checks(x)$sampling$diagnostic_refresh
    projected <- project(hmc_fit)
    ok(projected$hmc_metrics$availability == "retained" &&
        projected$phases$hmc$record$status == "partial" &&
        projected$phases$parameter_summary$record$status == "not_requested",
      "saved v2 phases agree with HMC and empty-parameter evidence")
    bad_hmc <- hmc_fit
    bad_hmc$cache$posterior_reporting$hmc_metrics[["3"]]$arithmetic$numerator <- 99
    p <- project(bad_hmc)
    ok(p$hmc_metrics$record[["3"]]$availability == "unavailable_invalid_metadata" &&
        p$hmc_metrics$record[["11"]]$availability == "retained" &&
        p$selection$availability == "retained", "malformed BFMI is local to one saved chain")
    bad_phase <- hmc_fit
    bad_phase$cache$posterior_reporting$phases$hmc$status <- "computed"
    p <- project(bad_phase)
    ok(p$phases$hmc$availability == "unavailable_invalid_metadata" &&
        p$hmc_metrics$availability == "retained", "contradictory phase labels cannot promote partial evidence")
    bad_retained <- hmc_fit
    bad_retained$cache$admissibility <- list(status = "deferred_by_policy", reason = "engineering deferred")
    bad_retained$cache$posterior_reporting$phases$conditioning <- list(status = "retained_evidence",
      reason = "engineering inconsistent witness", source = "cache$admissibility",
      record = bad_retained$cache$admissibility, provenance = list())
    p <- project(bad_retained)
    ok(p$phases$conditioning$availability == "unavailable_invalid_metadata" &&
        p$hmc_metrics$availability == "retained", "matching deferred payload is not computed evidence")
    if (!is.null(selected_fit)) {
      bad_parameter <- selected_fit
      bad_parameter$cache$posterior_reporting$parameter_metrics[[keys[1L]]]$global_constant <- TRUE
      bad_parameter$cache$posterior_reporting$parameter_metrics[[keys[1L]]]$chain_constant[] <- TRUE
      p <- project(bad_parameter)
      ok(p$parameter_metrics$record[[keys[1L]]]$availability == "unavailable_invalid_metadata" &&
          p$parameter_metrics$record[[keys[2L]]]$availability == "retained" &&
          p$hmc_metrics$availability == "retained", "malformed parameter evidence is local to one monitor")
    }
    replay <- function(input, mode = "success") {
      double_calls <- 0L
      hmc_index <- 0L
      bytes <- serialize(input, NULL, version = 2L)
      answer <- with_bindings(list(
        .bh_diagnostic_ingest_csv = function(...) { double_calls <<- double_calls + 1L; acquired$hmc_only },
        .bh_diagnostic_hmc_metrics = function(...) {
          if (mode == "error") stop("ENGINEERING post-acquisition error double")
          if (mode == "interrupt") stop(structure(list(message = "ENGINEERING interrupt double", call = NULL),
            class = c("interrupt", "condition")))
          hmc_index <<- hmc_index + 1L
          hmc_records[[hmc_index]]
        }), tryCatch(refresh_diagnostics(input, character(), hmc_budget), error = identity, interrupt = identity))
      ok(double_calls == 1L && identical(bytes, serialize(input, NULL, version = 2L)),
        paste(mode, "labelled acquisition double preserves atomic input"))
      answer
    }
    old <- if (is.null(selected_fit)) hmc_fit else selected_fit
    cleared <- replay(old)
    ok(nrow(parameter_summary(cleared)) == 0L && identical(cleared$cache$hmc, hmc_fit$cache$hmc) &&
        cleared$cache$posterior_reporting$phases$parameter_summary$status == "not_requested",
      "HMC-only replacement clears any old parameter table and replaces HMC evidence")
    failed <- replay(old, "error")
    ok(inherits(failed, "measurement_error") && identical(failed$code, "E_DIAGNOSTIC_REFRESH") &&
        !is.null(failed$receipt), "post-acquisition error double returns typed bounded receipt")
    interrupted <- replay(old, "interrupt")
    ok(inherits(interrupted, "interrupt"), "post-acquisition interruption propagates without partial cache")
    # Saving/reloading current-version objects is deliberately independent of
    # any historical producer namespace or retained external RDS fixture.
    views <- function(x) {
      checks <- fit_checks(x)
      summary <- summary(x)
      list(parameters = parameter_summary(x), posterior = posterior_summary(x),
        checks = checks, summary = summary, print_fit = capture.output(print(x)),
        print_summary = capture.output(print(summary)), print_checks = capture.output(print(checks)))
    }
    saved <- list(hmc = hmc_fit, malformed_hmc = bad_hmc, malformed_phase = bad_phase,
      malformed_retained = bad_retained, cleared = cleared)
    if (!is.null(selected_fit)) saved <- c(saved, list(selected = selected_fit, malformed_parameter = bad_parameter))
    saved <- lapply(saved, function(x) { x$backend <- structure(list(), class = "detached_engineering_backend"); x })
    reference_views <- lapply(saved, views)
    serialized <- serialize(saved, NULL, version = 2L)
    restored <- unserialize(serialized)
    passive_names <- c("refresh_diagnostics", "diagnostic_parameters", ".bh_diagnostic_ingest_csv",
      ".bh_posterior_ingest_csv", ".bh_diagnostic_metric_profile", ".bh_diagnostic_parameter_metrics",
      ".bh_diagnostic_hmc_metrics", ".mp_hash", ".bh_validate_spec")
    passive_names <- passive_names[vapply(passive_names, exists, logical(1), envir = ns, inherits = FALSE)]
    passive_guards <- setNames(rep(list(forbidden), length(passive_names)), passive_names)
    passive_namespaces <- sort(loadedNamespaces())
    base_names <- c("file", "readLines", "readBin", "file.info", "file.exists", "file.access",
      "normalizePath", "source", "sys.source", "readRDS", "load", "requireNamespace",
      "loadNamespace", "library", "require")
    base_guards <- setNames(rep(list(forbidden), length(base_names)), base_names)
    with_bindings(passive_guards, {
      with_bindings(base_guards, {
        for (name in names(restored)) ok(identical(views(restored[[name]]), reference_views[[name]]),
          paste(name, "saved current-version passive views replay exactly"))
      }, env = baseenv())
    })
    ok(identical(serialized, serialize(restored, NULL, version = 2L)) &&
        identical(passive_namespaces, sort(loadedNamespaces())),
      "passive serialized replay changes neither fit bytes nor loaded namespaces")
    ok(length(attempts) == count_before && guard_hits == 0L,
      "saved and lifecycle controls do no actual acquisition or guarded work")
    expected_attempts <- c("hmc_only", if (!is.null(selected_fit)) "selected", "default_reader", "invalid_source")
    ok(identical(attempts, expected_attempts), "exact bounded acquisition attempt sequence without retries")
    ok(identical(input_bytes, tools::md5sum(all_files)), "all engineering input CSV bytes preserved")
  })
  ok(identical(before, state()) && identical(owned_before, owned_directories()),
    "whole controller suite preserves RNG, caller options, cwd and owned-directory state")
  cat("SUCCESS", passed, "diagnostic refresh assertions;", length(attempts), "actual reader attempts\n")
})
