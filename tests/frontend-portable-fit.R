source(file.path("helpers", "native-guard.R"))
# Current installed namespace only. These are invented engineering fixtures,
# never sampled posteriors. Runtime execution requires the release-check budget.
# Fixed work: 1 preparation, 2 exports, 1 detached RDS roundtrip, 4 refresh calls,
# 1 acquisition of 2 chains x 4 rows x 4 columns, 0 parameter estimators/FFT,
# 0 fitting, compilation, generation, historical RDS loads or namespace repair.
library(bmeas)

local({
  ns <- asNamespace("bmeas")
  private <- function(name) get(name, ns, inherits = FALSE)
  passed <- 0L
  check <- function(id, value, label) {
    if (!identical(id, passed + 1L) || !isTRUE(value)) stop(label, call. = FALSE)
    passed <<- id
    cat("PASS", id, label, "\n")
  }
  error <- function(expr) tryCatch({ force(expr); NULL }, error = identity)
  code_is <- function(x, code) inherits(x, "measurement_error") && identical(x$code, code)
  with_bindings <- function(replacements, expr) {
    fields <- names(replacements)
    original <- lapply(fields, get, envir = ns, inherits = FALSE)
    locked <- vapply(fields, bindingIsLocked, logical(1), env = ns)
    on.exit(for (i in seq_along(fields)) {
      if (bindingIsLocked(fields[i], ns)) unlockBinding(fields[i], ns)
      assign(fields[i], original[[i]], ns)
      if (locked[i]) lockBinding(fields[i], ns)
    }, add = TRUE)
    for (i in seq_along(fields)) {
      if (locked[i]) unlockBinding(fields[i], ns)
      assign(fields[i], replacements[[i]], ns)
      if (locked[i]) lockBinding(fields[i], ns)
    }
    force(expr)
  }
  hits <- character()
  deny <- function(name) local({
    name <- name
    function(...) { hits <<- c(hits, name); stop(paste("Forbidden portable-test route:", name)) }
  })
  guards_for <- function(names) {
    stopifnot(all(vapply(names, exists, logical(1), envir = ns, inherits = FALSE)))
    setNames(lapply(names, deny), names)
  }
  state <- function() list(cwd = getwd(), options = options(),
    rng = if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
      get(".Random.seed", .GlobalEnv, inherits = FALSE) else NULL)
  owned <- function() sort(list.files(tempdir(),
    pattern = "^(bmeas-portable-|bmeas-streaming-)", full.names = TRUE))
  before <- state()
  owned_before <- owned()
  work <- tempfile("bmeas-release-fixture-")
  stopifnot(dir.create(work))
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  # Four sampler columns, four literal rows per chain. The only varying field
  # is the single-character chain ID. writeLines adds exactly one LF per line.
  csv <- function(id) c(
    "# ENGINEERING ONLY: invented values, no posterior qualification",
    "# stan_version_major = 2", "# stan_version_minor = 40",
    "# stan_version_patch = 0", "# model = scalar_components_model",
    paste0("# id = ", id), "# method = sample (Default)",
    "# num_samples = 4", "# num_warmup = 0", "# save_warmup = false (Default)",
    "# thin = 1", "# algorithm = hmc (Default)", "# engine = nuts (Default)",
    "# metric = diag_e (Default)", "# sig_figs = 18", "# max_depth = 4",
    "lp__,divergent__,treedepth__,energy__",
    "-1,0,1,1", "-2,0,2,2", "-3,1,4,4", "-4,0,3,3",
    "# Elapsed Time: 0 seconds (Warm-up)", "# 0 seconds (Sampling)", "# 0 seconds (Total)")
  paths <- file.path(work, c("engineering-chain-7.csv", "engineering-chain-3.csv"))
  write_csv <- function(lines, path) {
    con <- file(path, "wb")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con, useBytes = TRUE)
  }
  for (i in seq_along(paths)) write_csv(csv(c(7L, 3L)[i]), paths[i])
  source_bytes <- sum(file.info(paths)$size)
  source_hashes <- tools::md5sum(paths)
  check(1L, source_bytes == 2 * sum(nchar(csv(7L), type = "bytes") + 1L) &&
    source_bytes <= 4096, "two tiny CSVs fit the fixed 4096-byte source ceiling")

  terminal_guards <- guards_for(c(".bh_compile_scalar", ".bh_fit_scalar",
    ".bh_cache_scalar_fit", ".mp_toolchain_fingerprint", ".bh_backend_ir",
    ".bh_materialize_init", ".bh_provider_runtime_info", ".bh_provider_preflight",
    ".bh_provider_sample", "make_stan_code", ".bh_ppc_ingest_csv", ".bh_individual_ingest_csv"))
  with_bindings(terminal_guards, {
    d <- data.frame(person = as.character(1:4), x1 = c(.2, .8, .3, 1),
      x2 = c(.7, .6, -.3, .1), x3 = c(-.2, .6, .9, .4))
    spec <- bmeas(bmf(G = ~x1 + x2 + x3), d, subject = "person", fit = FALSE)
    check(2L, identical(class(spec), c("bmeas_spec", "bhmm_spec")),
      "one current-namespace prepared context supplies the original seal")
    backend <- new.env(parent = emptyenv())
    makeActiveBinding("must_not_inspect", function(value) {
      hits <<- c(hits, "backend"); stop("The backend must be omitted without inspection")
    }, backend)
    fit <- structure(list(spec = spec,
      run = structure(list(chains = 2L, parallel_chains = 1L, threads_per_chain = 1L,
        iter_warmup = 0L, iter_sampling = 4L, seed = 1L, cpu_budget = 1L,
        max_observations = 12L, max_subjects = 4L, max_saved_draws = 8L,
        reduction = "serial", grainsize = 1L, max_treedepth = 4L, model_budget = NULL,
        kind = "engineering_metadata_only", wall_time_enforcement = "not_run",
        memory_enforcement = "not_run"), class = "measurement_run_config"),
      backend = backend,
      cache = list(computation = list(status = "warning", reason = "Synthetic retained adverse history"),
        inference = list(status = "not_assessed", reason = "No posterior qualification"),
        posterior_propriety = list(status = "not_assessed", reason = "Engineering fixture")),
      code = "ENGINEERING A: NO GENERATED OR EXECUTED MODEL SOURCE",
      provenance = list(kind = "engineering_synthetic_fixture_no_posterior_qualification",
        started = structure(1700000000, class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        finished = NULL, compile_key = "not_generated", cache_hit = FALSE,
        executable_sha256 = "unavailable_no_executable",
        toolchain = list(cmdstan = character(), cmdstan_path = character(), cmdstanr = character(),
          R = character(), platform = character(), compiler = character(), compiler_version = character(),
          environment = character(), make_local = character(), cpp_options = list(), stanc_options = list()),
        initialization = list(status = "not_run", scope = "No sampler initialization"),
        model_fingerprint = spec$fingerprint, iter_total = 4L,
        execution_profile = list(requested = "engineering_metadata_only", used = "not_run"),
        exploratory_requested = FALSE, exploratory_rank_override = FALSE,
        save_log_lik = FALSE, estimated_csv_bytes = NA_real_, output_files = paths)),
      class = c("bmeas_fit", "bhmm_fit"))
    original <- fit
    improper <- fit
    improper$cache$posterior_propriety <- list(status = "proven_improper",
      reason = "Synthetic proof-status refusal control; no scientific conclusion")
    original_improper <- improper
    context_budget <- list(max_context_bytes = 2 * 1024^2,
      max_context_nodes = 100000, max_depth = 64)
    diagnostic_budget <- list(posterior = list(reader = "streaming", max_source_bytes = 4096,
      max_read_bytes = 7 * 4096, max_materialized_values = 500000,
      max_record_bytes = 1024, max_columns = 4, max_source_rows = 8, max_chains = 2,
      max_temp_bytes = 2 * 4096, max_scan_values = 48),
      max_monitors = 0, max_metric_rows = 8, max_fft_length = 0,
      max_catalogue_rows = 100, max_context_rows = 1000)

    codec_calls <- c(count = 0L, copy = 0L, write = 0L)
    codec_real <- private(".bh_portable_codec_run")
    codec_counter <- function(x, policy, locale, mode, connection = NULL) {
      codec_calls[[mode]] <<- codec_calls[[mode]] + 1L
      if (sum(codec_calls) > 18L) stop("Unbudgeted canonical traversal")
      codec_real(x, policy, locale, mode, connection)
    }
    acquisitions <- 0L
    acquisition_allowed <- FALSE
    reader_real <- private(".bh_posterior_ingest_csv")
    reader_counter <- function(...) {
      acquisitions <<- acquisitions + 1L
      if (!acquisition_allowed || acquisitions > 1L) stop("Unbudgeted source acquisition")
      reader_real(...)
    }
    route_guards <- guards_for(c(".bh_prepare", ".bh_normalize_spec", ".bh_formula_upgrade",
      ".bh_validate_basis", ".bh_capture_prior_target", ".mp_hash"))
    route_guards$.bh_portable_codec_run <- codec_counter
    route_guards$.bh_posterior_ingest_csv <- reader_counter
    with_bindings(route_guards, {
      # Independent byte/count oracle for one primitive value, with no hash file.
      small <- list(max_context_bytes = 4096, max_context_nodes = 100, max_depth = 8)
      locale <- Sys.getlocale("LC_CTYPE")
      logical_values <- c(TRUE, NA, FALSE)
      expected <- charToRaw("BMEAS-PORTABLE-CONTEXT-1\nB3:A0:t;a;f;")
      count <- private(".bh_portable_codec_count")(logical_values, small, locale)
      copied <- private(".bh_portable_codec_copy")(logical_values, small, locale)
      con <- rawConnection(raw(), "wb")
      written <- tryCatch({
        counts <- private(".bh_portable_codec_write")(logical_values, con, small, locale)
        list(counts = counts, bytes = rawConnectionValue(con))
      }, finally = close(con))
      check(3L, identical(count, list(bytes = as.double(length(expected)), nodes = 5, depth = 2)),
        "codec primitive counts match the independent format oracle")
      check(4L, identical(copied, logical_values) && identical(written$counts, count) &&
        identical(written$bytes, expected), "codec count/copy/write agree with exact bytes")
      tiny <- small; tiny$max_context_bytes <- 1
      refused <- error(private(".bh_portable_codec_count")(logical_values, tiny, locale))
      check(5L, code_is(refused, "E_PORTABLE_BUDGET"), "codec refuses insufficient bytes")

      portable <- portable_fit(fit, context_budget)
      protected <- portable_fit(improper, context_budget)
      check(6L, identical(fit, original) && identical(improper, original_improper),
        "export leaves both original shells and their original seals unchanged")
      check(7L, identical(class(portable), "bmeas_portable_fit") &&
        identical(names(portable), c("schema_version", "immutable", "cache", "identity", "export_receipt")),
        "export has the detached class and fixed envelope")
      check(8L, identical(portable$immutable$ancestry$original_spec_fingerprint, spec$fingerprint) &&
        identical(portable$immutable$source_binding$original_paths$output_files, paths),
        "original seal ancestry and ordered source paths survive unchanged")
      check(9L, identical(portable$cache, fit$cache) &&
        identical(portable$immutable$protected_evidence$original_fit_history$computation, fit$cache$computation),
        "mutable cache and protected adverse history are retained")
      check(10L, identical(portable$immutable$protected_evidence$original_provenance$started$representation,
        "bmeas_inert_timestamp_v1") && !"backend" %in% names(portable),
        "timestamp becomes inert and live backend is omitted")
      check(11L, length(portable$immutable$ancestry$callback_disposition) == 7L &&
        all(vapply(portable$immutable$ancestry$callback_disposition,
          function(x) identical(x$disposition, "execution_only_omitted_not_semantically_replayed_or_equated"), logical(1))),
        "all seven execution callback slots have explicit omission markers")
      check(12L, all(vapply(list(portable, protected), function(x) {
        r <- x$export_receipt
        r$canonical_payload_bytes <= context_budget$max_context_bytes &&
          r$canonical_payload_nodes <= context_budget$max_context_nodes &&
          r$canonical_payload_depth <= context_budget$max_depth &&
          identical(r$hash_bytes_written, r$immutable_bytes) &&
          identical(r$hash_read_bytes_reserved, r$immutable_bytes) &&
          identical(r$hash_file_cleanup, "owned_file_removed_before_return")
      }, logical(1))), "both exports have finite self-inclusive accounting and cleaned hash files")

      passive <- function(x) with_bindings(guards_for(c(".bh_portable_codec_run",
        ".bh_pf_owned_hash", ".bh_posterior_ingest_csv", ".bh_diagnostic_ingest_csv")), {
        list(summary = summary(x), checks = fit_checks(x), parameters = parameter_summary(x),
          posterior = posterior_summary(x), text = capture.output(print(x)))
      })
      warm <- passive(portable)
      check(13L, identical(warm$summary$portable_context$status, "not_verified_by_this_passive_call") &&
        identical(warm$checks$portable_context$status, "not_verified_by_this_passive_call"),
        "passive reports explicitly disclaim current integrity verification")
      check(14L, any(grepl("integrity not checked", warm$text, fixed = TRUE)) &&
        any(grepl("Protected original original_fit_history computation : warning", warm$text, fixed = TRUE)),
        "printing keeps the integrity notice and protected adverse status visible")
      rds <- file.path(work, "detached.rds")
      saveRDS(portable, rds, version = 2)
      restored <- readRDS(rds)
      check(15L, identical(restored, portable) && identical(passive(restored), warm),
        "one detached RDS roundtrip preserves saved context and passive reports")

      changed <- restored
      changed$immutable$retained_artifacts$saved_source_text <-
        "ENGINEERING B: NO GENERATED OR EXECUTED MODEL SOURCE"
      bad_identity <- error(refresh_diagnostics(changed, character(), diagnostic_budget))
      check(16L, code_is(bad_identity, "E_PORTABLE_IDENTITY") &&
        identical(bad_identity$context_validation$phase, "immutable_encoding_and_digest") && acquisitions == 0L,
        "equal-length immutable mutation refuses before acquisition without resealing")
      disabled <- restored
      disabled$immutable$capabilities$active_interfaces <- character()
      bad_capability <- error(refresh_diagnostics(disabled, character(), diagnostic_budget))
      check(17L, code_is(bad_capability, "E_PORTABLE_SCHEMA") &&
        identical(bad_capability$context_validation$count_passes, 0) && acquisitions == 0L,
        "disabled capability profile refuses before canonical count or acquisition")
      protected$cache$posterior_propriety <- list(status = "not_assessed", reason = "Mutable edit")
      bad_history <- error(refresh_diagnostics(protected, character(), diagnostic_budget))
      check(18L, code_is(bad_history, "E_DIAGNOSTIC_POSTERIOR") &&
        identical(bad_history$context_validation$status, "verified") && acquisitions == 0L,
        "immutable proved-improper history survives mutable-cache editing and blocks active work")
      check(19L, code_is(error(stancode(restored)), "E_PORTABLE_UNSUPPORTED") &&
        code_is(error(standata(restored)), "E_PORTABLE_UNSUPPORTED"),
        "portable objects refuse source and data generation")

      acquisition_allowed <- TRUE
      refreshed <- refresh_diagnostics(restored, character(), diagnostic_budget)
      acquisition_allowed <- FALSE
      check(20L, acquisitions == 1L && identical(class(refreshed), "bmeas_portable_fit"),
        "the sole successful HMC-only refresh uses exactly one source acquisition")
      check(21L, identical(restored, portable) && identical(refreshed$immutable, portable$immutable) &&
        identical(refreshed$identity, portable$identity) && identical(refreshed$export_receipt, portable$export_receipt),
        "active refresh updates a cache without changing the caller or immutable export")
      reporting <- refreshed$cache$posterior_reporting
      receipt <- reporting$provenance$source
      context <- receipt$context_validation
      check(22L, identical(context$status, "verified") && identical(context$phase, "complete") &&
        identical(context$identity, portable$identity) && identical(context$count_passes, 1) &&
        identical(context$schema_validation_passes, 1) && identical(context$immutable_encoding_passes, 1) &&
        identical(context$hash_bytes_written, portable$export_receipt$immutable_bytes),
        "source receipt retains a separately completed immutable-validation phase")
      check(23L, receipt$source_bytes == source_bytes && receipt$read_bytes_charged == 7 * source_bytes &&
        receipt$temp_bytes_written == 2 * source_bytes &&
        receipt$max_materialized_values <= diagnostic_budget$posterior$max_materialized_values &&
        receipt$required_numeric_conversions == 48,
        "acquisition respects seven reads, two snapshots and finite numeric work")
      check(24L, reporting$observed$chains == 2 && reporting$observed$rows_per_chain == 4 &&
        reporting$observed$rows == 8 && identical(as.double(reporting$observed$chain_ids), c(3, 7)),
        "all eight invented states retain original chain identity")
      check(25L, nrow(parameter_summary(refreshed)) == 0L &&
        identical(reporting$phases$parameter_summary$status, "not_requested"),
        "HMC-only work neither requests population monitors nor calls FFT estimators")
      hmc <- refreshed$cache$hmc
      check(26L, identical(hmc$num_divergent, c(1, 1)) &&
        identical(hmc$num_max_treedepth, c(1, 1)) &&
        isTRUE(all.equal(hmc$ebfmi, rep(sum(diff(c(1, 2, 4, 3))^2) / 4 / stats::var(c(1, 2, 4, 3)), 2))),
        "HMC counts and BFMI agree with independent four-row arithmetic")
      after <- passive(refreshed)
      check(27L, identical(after$summary$portable_context$status, "not_verified_by_this_passive_call") &&
        identical(after$summary$portable_context$saved_active_receipts$diagnostics$availability, "retained_not_reverified") &&
        identical(after$summary$portable_context$protected_original_evidence,
          portable$immutable$protected_evidence), "saved active receipts do not upgrade passive integrity or erase history")
      check(28L, identical(codec_calls, c(count = 9L, copy = 3L, write = 6L)),
        "canonical traversals match the fixed eighteen-call envelope")
      check(29L, identical(tools::md5sum(paths), source_hashes), "source files remain byte-for-byte unchanged")
      check(30L, identical(after$summary$portable_context$runtime_support, "portable_postprocessing_v1") &&
        identical(after$summary$portable_context$producer_dispatch_status, "stage1_export_only_consumers_closed"),
        "current passive support is distinct from the immutable historical producer marker")
      legacy_report <- after$summary
      legacy_report$portable_context$runtime_support <- "portable_postprocessing_v1_runtime_qualification_pending"
      legacy_text <- with_bindings(guards_for(c(".bh_portable_codec_run", ".bh_pf_owned_hash",
        ".bh_posterior_ingest_csv", ".bh_diagnostic_ingest_csv")), capture.output(print(legacy_report)))
      check(31L, any(grepl("Protected original original_fit_history computation : warning", legacy_text, fixed = TRUE)),
        "an older saved-report support label still displays protected adverse history")
    })
  })
  check(32L, !length(hits) && identical(owned(), owned_before),
    "no forbidden backend/native route or leaked package-owned temporary file")
  check(33L, identical(state(), before), "RNG, options and working directory are unchanged")
  stopifnot(passed == 33L)
  cat("Portable release regression: 33 assertions; 2 exports; 1 acquisition; 32 invented CSV values.\n")
})
