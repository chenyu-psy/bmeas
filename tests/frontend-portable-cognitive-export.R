source(file.path("helpers", "native-guard.R"))
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if (nzchar(root)) {
  for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
} else library(bmeas)

# R-only engineering declarations and literal CSV values. No fitted posterior,
# likelihood qualification, native compilation, simulator or sampling is used.
(function() {
  ns <- if (nzchar(root)) .GlobalEnv else asNamespace("bmeas")
  private <- function(n) get(n, ns, inherits = FALSE)
  source(file.path("helpers", "m3cs-saved-fixture.R"), local = TRUE)
  passed <- 0L
  ok <- function(value, label) {
    if (!isTRUE(value)) stop(label, call. = FALSE)
    passed <<- passed + 1L
    cat("PASS", label, "\n")
  }
  refuses <- function(action, codes, label) {
    e <- tryCatch({ action(); NULL }, error = identity)
    ok(inherits(e, "measurement_error") && e$code %in% codes, label)
    invisible(e)
  }
  with_bindings <- function(replacements, expr) {
    fields <- names(replacements)
    old <- mget(fields, ns, inherits = FALSE)
    locked <- vapply(fields, bindingIsLocked, logical(1), env = ns)
    replace <- function(n, v) {
      if (bindingIsLocked(n, ns)) unlockBinding(n, ns)
      assign(n, v, ns)
      if (locked[[match(n, fields)]]) lockBinding(n, ns)
    }
    on.exit(for (n in fields) replace(n, old[[n]]), add = TRUE)
    for (n in fields) replace(n, replacements[[n]])
    force(expr)
  }
  hits <- character()
  deny <- function(label) local({
    label <- label
    function(...) { hits <<- c(hits, label); stop(paste("FORBIDDEN", label)) }
  })
  guarded <- c("compile_m3cs_simulator", ".bh_compile_scalar", ".bh_fit_scalar",
    ".bh_backend_ir", ".bh_materialize_init", ".mp_toolchain_fingerprint",
    ".bh_provider_runtime_info", ".bh_provider_preflight", ".bh_provider_sample",
    "make_stan_code", ".bh_ppc_ingest_csv", ".bh_covariance_probe", ".bh_gaussian_identification")
  stopifnot(all(vapply(guarded, exists, logical(1), envir = ns, inherits = FALSE)))
  state <- function() list(cwd = getwd(), options = options(),
    rng = if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL)
  before <- state()
  owned <- tempfile("cognitive-export-")
  stopifnot(dir.create(owned))
  on.exit(unlink(owned, recursive = TRUE), add = TRUE)
  context <- list(max_context_bytes = 20 * 1024^2, max_context_nodes = 2000000, max_depth = 64)
  posterior <- list(reader = "streaming", max_source_bytes = 4096, max_read_bytes = 7 * 4096,
    max_materialized_values = 500000, max_record_bytes = 1024, max_columns = 4,
    max_source_rows = 8, max_chains = 2, max_temp_bytes = 2 * 4096, max_scan_values = 48)
  db <- list(posterior = posterior, max_monitors = 0, max_metric_rows = 8,
    max_fft_length = 0, max_catalogue_rows = 100, max_context_rows = 1000)
  pb <- diagnostic_budget(4, 100, 4000, posterior = posterior)
  write_csv <- function(path, id) writeLines(c(
    "# ENGINEERING ONLY; invented metadata values, not posterior evidence",
    "# stan_version_major = 2", "# stan_version_minor = 40", "# stan_version_patch = 0",
    "# model = scalar_components_model", paste0("# id = ", id),
    "# method = sample (Default)", "# num_samples = 4", "# num_warmup = 0",
    "# save_warmup = false (Default)", "# thin = 1", "# algorithm = hmc (Default)",
    "# engine = nuts (Default)", "# metric = diag_e (Default)", "# max_depth = 4", "# sig_figs = 18",
    "lp__,divergent__,treedepth__,energy__", "-1,0,1,1", "-2,0,2,2", "-3,1,4,4", "-4,0,3,3",
    "# Elapsed Time: 0 seconds (Warm-up)", "# 0 seconds (Sampling)", "# 0 seconds (Total)"), path)
  live_bytes <- function(x) serialize(x[setdiff(names(x), "backend")], NULL, version = 2)
  endpoint_fields <- c("layer", "row", "col", "state", "fixed_value", "parameter", "primitive_index", "source")
  capsules <- list()
  with_bindings(setNames(lapply(guarded, deny), guarded), {
    tasks <- c("t2", "t1")
    d <- expand.grid(person = c("z", "a", "b"), task = tasks, stringsAsFactors = FALSE)
    d$mean_rt <- .7; d$var_rt <- .04; d$n_upper <- 6L; d$n_trials <- 10L
    ez_block <- function(parameters) observe(ezdm_model(), d, "person", "task",
      fields = list(mean_rt = "mean_rt", var_rt = "var_rt", n_upper = "n_upper", n_trials = "n_trials"),
      units = "second", parameters = parameters)
    task_block <- ez_block(list(drift = "task", bound = "task", ndt = "task"))
    specs <- list(EZ = with_checks(function() bmeas(bmf(), list(ez = task_block), fit = FALSE)))
    resource <- Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
    copied_resource <- NULL
    if (nzchar(resource) && file.exists(resource)) {
      resource_hash <- tools::md5sum(resource)
      copied_resource <- file.path(owned, "original-m3.stanfunctions")
      stopifnot(file.copy(resource, copied_resource))
      md <- d[c("person", "task")]
      md$counts <- I(matrix(0L, nrow(md), 10L)); md$counts[, 1L] <- 2L; md$counts[, 10L] <- 1L
      md$options <- I(matrix(1L, nrow(md), 10L)); md$options[, 10L] <- 3L
      block <- observe(m3_cs_model(copied_resource), md, "person", "task",
        fields = list(counts = "counts", options = "options"), units = "count",
        parameters = list(a = list(fixed = .2, scale = "predictor"),
          c = list(fixed = -.5, scale = "predictor"), ra = list(fixed = .3), rc = list(fixed = .7)))
      specs$M3CS_all_fixed <- with_checks(function() bmeas(bmf(), list(cs = block), fit = FALSE))
    } else cat("SKIP all-fixed M3-CS export: external pinned resource absent\n")

    for (family in names(specs)) {
      spec <- specs[[family]]
      directory <- file.path(owned, family); stopifnot(dir.create(directory))
      paths <- file.path(directory, paste0("original-", 1:2, ".csv"))
      for (i in seq_along(paths)) write_csv(paths[i], c(7L, 3L)[i])
      hashes <- tools::md5sum(paths)
      live <- cd_shell(spec, paths)
      original <- live_bytes(live); spec_bytes <- serialize(spec, NULL, version = 2)
      live$cache$correlation_constraints <- private(".bh_affine_cache_metadata")(
        spec, private(".bh_affine_summary_map")(spec))
      original <- live_bytes(live)
      fit <- portable_fit(live, context)
      capsules[[family]] <- fit
      detached <- fit$immutable$scientific_context
      ok(identical(live_bytes(live), original) && identical(serialize(spec, NULL, version = 2), spec_bytes) &&
        identical(fit$immutable$ancestry$original_spec_fingerprint, spec$fingerprint) &&
        identical(detached$fingerprint, spec$fingerprint), paste(family, "export preserves original bytes and seals"))
      ok(identical(detached$models[[1L]]$model$provenance, spec$models[[1L]]$model$provenance) &&
        identical(fit$immutable$protected_evidence$original_provenance, live$provenance),
        paste(family, "retains complete producer and execution provenance"))
      cells <- spec$tables$correlation_affine_cells
      ok(identical(dim(cells), c(0L, 8L)) && identical(names(cells), endpoint_fields) &&
        identical(detached$tables$correlation_affine_cells, cells),
        paste(family, "retains exact historical endpoint-free empty table without repair"))
      ok(identical(fit$cache, live$cache) &&
        identical(fit$immutable$protected_evidence$original_fit_history$computation, live$cache$computation),
        paste(family, "retains adverse checks and cache"))
      exported_bytes <- serialize(fit, NULL, version = 2)
      refreshed <- refresh_diagnostics(fit, character(), db)
      ok(identical(serialize(fit, NULL, version = 2), exported_bytes) &&
        identical(refreshed$immutable, fit$immutable) && identical(refreshed$identity, fit$identity) &&
        identical(refreshed$export_receipt, fit$export_receipt), paste(family, "diagnostics preserve immutable capsule"))
      ok(identical(refreshed$cache$hmc$num_divergent, c(1, 1)) &&
        identical(refreshed$cache$hmc$num_max_treedepth, c(1, 1)) &&
        nrow(parameter_summary(refreshed)) == 0L &&
        identical(refreshed$cache$posterior_reporting$provenance$source$context_validation$status, "verified"),
        paste(family, "bounded saved-value diagnostics validate context with no population estimators"))
      ok(identical(tools::md5sum(paths), hashes), paste(family, "source CSV bytes remain unchanged"))
      a <- archive_fit(refreshed, file.path(directory, "archive"))
      moved <- file.path(directory, "relocated")
      stopifnot(file.rename(a$root, moved)); a$root <- moved
      unlink(paths)
      if (family == "M3CS_all_fixed") unlink(copied_resource)
      files <- list.files(a$root, recursive = TRUE, full.names = TRUE)
      archive_hashes <- tools::md5sum(files); archive_bytes <- serialize(a, NULL, version = 2)
      restored <- refresh_diagnostics(a, character(), db)
      retained <- restored$report_snapshot$portable
      ok(identical(retained$cache$hmc, refreshed$cache$hmc) &&
        identical(retained$immutable, fit$immutable) && identical(retained$identity, fit$identity) &&
        identical(retained$immutable$source_binding$original_paths$output_files, paths),
        paste(family, "relocated archive refresh works without original paths or rewritten history"))
      ok(identical(serialize(a, NULL, version = 2), archive_bytes) &&
        identical(tools::md5sum(files), archive_hashes), paste(family, "relocated archive bytes and handle remain unchanged"))
      for (action in list(function() stancode(fit), function() standata(fit), function() check_model(fit)))
        refuses(action, "E_PORTABLE_UNSUPPORTED", paste(family, "portable backend/source/data routes remain closed"))
      if (family == "EZ") {
        original_ez <- live
        for (action in list(function() ppc_plan(fit, 4, pb, "population"),
          function() assess_measurement_fit(fit, level = "population", draws = 4, seed = 11, budget = pb)))
          refuses(action, c("E_PPC_FAMILY", "E_PORTABLE_UNSUPPORTED", "E_CHECK_UNSUPPORTED"),
            "genuine EZ portable PPC remains unsupported before acquisition or execution")
      }
    }
    if (!is.null(copied_resource)) ok(identical(tools::md5sum(resource), resource_hash), "external pinned resource bytes untouched")

    fit <- capsules$EZ
    p <- fit$immutable$scientific_context$models$ez$model$provenance
    ok(identical(p$contract_commit, "54a07b6fa34506bd14116d295eea9db234071f28") &&
      identical(p$contract_url, "https://github.com/popov-lab/bmm/tree/54a07b6fa34506bd14116d295eea9db234071f28") &&
      identical(p$diffusion_sd, 1) && identical(p$relative_start, .5) && is.character(p$derivation),
      "genuine revised EZ provenance has complete pinned plain-data contract")
    bad <- original_ez
    bad$spec$models$ez$model$provenance$diffusion_sd <- 2
    bad_bytes <- live_bytes(bad)
    refuses(function() portable_fit(bad, context), "E_PORTABLE_ORIGINAL_SEAL", "changed live provenance cannot bypass original seal")
    ok(identical(live_bytes(bad), bad_bytes), "failed live export never repairs or reseals caller")

    # Public consumers validate malformed scientific data before acquisition or
    # digest/accounting checks. No malformed case is resealed to make it pass.
    callback_counter <- ".bmeas_cognitive_export_callback_hits"
    stopifnot(!exists(callback_counter, .GlobalEnv, inherits = FALSE))
    assign(callback_counter, 0L, .GlobalEnv)
    on.exit(rm(list = callback_counter, envir = .GlobalEnv), add = TRUE)
    hostile <- function(...) {
      .GlobalEnv$.bmeas_cognitive_export_callback_hits <- .GlobalEnv$.bmeas_cognitive_export_callback_hits + 1L
      stop("FORBIDDEN hostile provenance/table dispatch")
    }
    # A stable environment keeps serialization assertions independent of this
    # test's changing local counters, rather than capturing the whole test.
    environment(hostile) <- baseenv()
    method_names <- c("$.portable_cognitive_hostile", "[[.portable_cognitive_hostile",
      "length.portable_cognitive_hostile", "as.character.portable_cognitive_hostile")
    stopifnot(!any(vapply(method_names, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)))
    for (n in method_names) assign(n, hostile, .GlobalEnv)
    on.exit(rm(list = method_names, envir = .GlobalEnv), add = TRUE)
    mutations <- list(
      unknown_field = function(p) { p$unknown <- "unapproved"; p },
      incomplete_profile = function(p) { p$derivation <- NULL; p },
      wrong_commit = function(p) { p$contract_commit <- paste(rep("0", 40), collapse = ""); p },
      wrong_url = function(p) { p$contract_url <- "https://example.invalid/other"; p },
      wrong_diffusion = function(p) { p$diffusion_sd <- 2; p },
      wrong_start = function(p) { p$relative_start <- .4; p },
      nan_diffusion = function(p) { p$diffusion_sd <- NaN; p },
      infinite_diffusion = function(p) { p$diffusion_sd <- Inf; p },
      text_diffusion = function(p) { p$diffusion_sd <- "1"; p },
      logical_diffusion = function(p) { p$diffusion_sd <- TRUE; p },
      vector_diffusion = function(p) { p$diffusion_sd <- c(1, 1); p },
      named_diffusion = function(p) { p$diffusion_sd <- c(value = 1); p },
      matrix_diffusion = function(p) { p$diffusion_sd <- matrix(1, 1, 1); p },
      classed_diffusion = function(p) { p$diffusion_sd <- structure(1, class = "portable_cognitive_hostile"); p },
      attributed_diffusion = function(p) { p$diffusion_sd <- structure(1, units = "custom"); p },
      null_start = function(p) { p["relative_start"] <- list(NULL); p },
      missing_start = function(p) { p$relative_start <- NA_real_; p },
      vector_start = function(p) { p$relative_start <- c(.5, .5); p },
      classed_provenance = function(p) { class(p) <- "portable_cognitive_hostile"; p },
      callable_derivation = function(p) { p$derivation <- hostile; p },
      reference_derivation = function(p) { p$derivation <- new.env(parent = emptyenv()); p })
    validate_bad <- function(bad, label, codes = "E_PORTABLE_SCHEMA") {
      bytes <- serialize(bad, NULL, version = 2)
      e <- refuses(function() refresh_diagnostics(bad, character(), db), codes, label)
      ok(identical(serialize(bad, NULL, version = 2), bytes) &&
        identical(e$context_validation$immutable_encoding_passes, 0), paste(label, "does not reseal or reach immutable hashing"))
    }
    with_bindings(list(.bh_posterior_ingest_csv = deny("negative source acquisition")), {
      for (label in names(mutations)) {
        bad <- fit
        bad$immutable$scientific_context$models$ez$model$provenance <- mutations[[label]](p)
        validate_bad(bad, paste("malformed EZ provenance", label),
          if (label %in% c("callable_derivation", "reference_derivation")) "E_PORTABLE_TYPE" else "E_PORTABLE_SCHEMA")
      }
      for (field in c("name", "version")) {
        bad <- fit; bad$immutable$scientific_context$models$ez$model[[field]] <- "unapproved"
        validate_bad(bad, paste("pinned EZ provenance rejects wrong model", field))
      }
      empty <- fit$immutable$scientific_context$tables$correlation_affine_cells
      table_mutations <- list(
        only_first_endpoint = function(t) { t$endpoint1 <- character(); t },
        only_second_endpoint = function(t) { t$endpoint2 <- character(); t },
        extra_field = function(t) { t$extra <- character(); t },
        missing_field = function(t) { t$source <- NULL; t },
        wrong_column_type = function(t) { t$row <- character(); t },
        classed_table = function(t) { class(t) <- c("portable_cognitive_hostile", "data.frame"); t },
        nonempty_without_endpoints = function(t) data.frame(layer = "parameter", state = "zero",
          source = "invented", row = 1L, col = 2L, parameter = NA_character_, primitive_index = NA_integer_, fixed_value = 0))
      for (label in names(table_mutations)) {
        bad <- fit; bad$immutable$scientific_context$tables$correlation_affine_cells <- table_mutations[[label]](empty)
        validate_bad(bad, paste("malformed endpoint-free affine table", label))
      }
    })

    # Genuine required-affine contexts: zero components with a parameter edge,
    # and one component even though its component edge table is empty.
    subject_block <- ez_block(list(bound = "task", ndt = "task"))
    required <- list(
      parameter_edge = with_checks(function() bmeas(bmf(residual = list(targets = ~ez$drift[tasks])), list(ez = subject_block), fit = FALSE)),
      one_component = with_checks(function() bmeas(bmf(G = ~ez$drift[tasks]), list(ez = subject_block), fit = FALSE)))
    for (label in names(required)) {
      cache_live <- cd_shell(required[[label]], c("unread-one.csv", "unread-two.csv"))
      cache_live$cache$correlation_constraints <- private(".bh_affine_cache_metadata")(
        required[[label]], private(".bh_affine_summary_map")(required[[label]]))
      candidate <- portable_fit(cache_live, context)
      ok(identical(candidate$cache$correlation_constraints, cache_live$cache$correlation_constraints),
        paste(label, "genuine complete cached correlation metadata exports unchanged"))
      cache_live$cache$correlation_constraints$cells <- empty
      unchanged <- live_bytes(cache_live)
      refuses(function() portable_fit(cache_live, context), "E_PORTABLE_SCHEMA",
        paste(label, "cache-only endpoint-free substitution is refused against untouched valid spec"))
      ok(identical(live_bytes(cache_live), unchanged), paste(label, "cache refusal leaves original spec and history unchanged"))
      tables <- candidate$immutable$scientific_context$tables
      ok(if (label == "parameter_edge") nrow(tables$components) == 0L && nrow(tables$parameter_correlations) > 0L else
        nrow(tables$components) == 1L && nrow(tables$correlations) == 0L, paste(label, "genuine producer provides required-affine context"))
      candidate$immutable$scientific_context$tables$correlation_affine_cells <- empty
      with_bindings(list(.bh_posterior_ingest_csv = deny("required-affine source acquisition")),
        validate_bad(candidate, paste(label, "cannot accept endpoint-free empty affine cells")))
    }

    old_path <- Sys.getenv("BMEAS_OLD_CAPSULE")
    if (nzchar(old_path)) {
      stopifnot(file.exists(old_path))
      hash <- tools::md5sum(old_path); old <- readRDS(old_path)
      bytes <- serialize(old, NULL, version = 2)
      validated <- private(".bh_pf_active")(old, "refresh_diagnostics", archive_inspection = TRUE)
      report <- summary(old)
      ok(identical(validated$receipt$status, "verified") &&
        identical(report$portable_context$status, "not_verified_by_this_passive_call"),
        "untouched prior-producer capsule validates and remains passively inspectable")
      ok(identical(old$immutable$context_profile, fit$immutable$context_profile) &&
        identical(old$immutable$capabilities, fit$immutable$capabilities) &&
        identical(old$immutable$ancestry$conversion_profile, fit$immutable$ancestry$conversion_profile),
        "new exports retain prior-producer context, capabilities and conversion profile")
      ok(identical(serialize(old, NULL, version = 2), bytes) && identical(tools::md5sum(old_path), hash),
        "prior-producer capsule original file and serialized bytes stay unchanged")
    } else cat("SKIP prior-producer capsule: BMEAS_OLD_CAPSULE absent\n")
  })
  ok(!length(hits) && identical(get(callback_counter, .GlobalEnv), 0L),
    "no forbidden native, backend, callback or negative-acquisition route")
  ok(identical(state(), before), "caller RNG, options and working directory unchanged")
  cat("Portable cognitive export:", passed, "assertions; R-only engineering fixtures, no model execution.\n")
})()
