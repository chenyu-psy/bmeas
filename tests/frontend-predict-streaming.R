source(file.path("helpers", "native-guard.R"))
# OWNER-SIDE STUB CONTROLS ONLY. Invented in-memory joint states and engineering
# controller doubles are not fitted objects or posterior evidence. No CSV is
# written/read; neither actual reader, backend, compiler nor sampler may run.
# Source: MEASUREMENTPKG_SOURCE=/path/to/package Rscript --vanilla this-file.R
# Installed: omit MEASUREMENTPKG_SOURCE after a separately authorized install.
# BMEAS_PPC_STREAMING_MODE=stub selects this in-memory test script.
# Actual synthetic-source integration lives in frontend-predict-streaming-integration.R.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(source_root)
mode <- Sys.getenv("BMEAS_PPC_STREAMING_MODE", "stub")
if (!identical(mode, "stub")) stop("This script supports only labelled in-memory reader stub mode")
if (source_mode) {
  for (f in list.files(file.path(source_root, "R"), full.names = TRUE)) source(f)
} else library(bmeas)
package_env <- if (source_mode) .GlobalEnv else asNamespace("bmeas")
private <- function(name) get(name, package_env, inherits = FALSE)

local({
  passed <- 0L
  failures <- character()
  ok <- function(value, label) {
    if (isTRUE(value)) {
      passed <<- passed + 1L
      cat("PASS", label, "\n")
    } else {
      failures <<- c(failures, label)
      cat("FAIL", label, "\n")
    }
    invisible(value)
  }
  err <- function(expr, code, label) {
    e <- tryCatch(force(expr), error = identity)
    ok(inherits(e, "measurement_error") && identical(e$code, code), label)
    if (inherits(e, "error") && !identical(e$code, code))
      cat("DETAIL expected", code, "observed", conditionMessage(e), "\n")
    invisible(e)
  }
  binding_env <- function(name, env) {
    while (!exists(name, env, inherits = FALSE) && !identical(env, emptyenv())) env <- parent.env(env)
    env
  }
  with_bindings <- function(replacements, expr, env = package_env) {
    old <- lapply(names(replacements), function(name) {
      target <- if (identical(env, .GlobalEnv)) env else binding_env(name, env)
      if (identical(target, emptyenv())) stop("Cannot locate binding: ", name)
      present <- exists(name, target, inherits = FALSE)
      list(env = target, present = present, locked = present && bindingIsLocked(name, target),
           value = if (present) get(name, target, inherits = FALSE) else NULL)
    })
    names(old) <- names(replacements)
    on.exit(for (name in names(old)) {
      saved <- old[[name]]
      if (saved$present) assign(name, saved$value, saved$env) else rm(list = name, envir = saved$env)
      if (saved$locked) lockBinding(name, saved$env)
    }, add = TRUE)
    for (name in names(replacements)) {
      saved <- old[[name]]
      if (saved$locked) unlockBinding(name, saved$env)
      assign(name, replacements[[name]], saved$env)
    }
    force(expr)
  }
  trapped <- character()
  traps <- function(names) setNames(lapply(names, function(name) local({
    terminal <- name
    function(...) { trapped <<- c(trapped, terminal); stop("Forbidden control boundary: ", terminal) }
  })), names)
  terminal_names <- c("stancode", "standata", ".bh_backend_ir", ".bh_materialize_init",
    ".bh_compile_scalar", ".bh_fit_scalar", ".bh_cache_scalar_fit", ".bh_provider_runtime_info",
    ".bh_provider_preflight", "compile_measurement", "fit_measurement")

  # The only prepared domains: four people, two tasks, two repeated responses;
  # a 31-person metadata-only domain checks the unchanged PPC geometry ceiling.
  people <- paste0("person", 1:4)
  tasks <- c("a", "b")
  data <- expand.grid(person = people, task = tasks, repeat_id = 1:2, stringsAsFactors = FALSE)
  data$y <- sin(seq_len(nrow(data)))
  data$obs <- paste0("row", seq_len(nrow(data)))
  prepare <- function(d) bmeas(bmf(G = ~score$mu[tasks],
    loadings = list(fixed = list(list(value = 1, cells = list(G = ~score$mu[tasks])))),
    residual = list(targets = ~score$mu[tasks])),
    data = list(score = observe(gaussian_model(), d, "person", "task", "y", units = "score", observation_id = "obs")), fit = FALSE)
  spec <- with_bindings(traps(terminal_names), prepare(data))
  wide <- expand.grid(person = paste0("person", 1:31), task = tasks, stringsAsFactors = FALSE)
  wide$y <- sin(seq_len(nrow(wide))); wide$obs <- paste0("wide", seq_len(nrow(wide)))
  wide_spec <- with_bindings(traps(terminal_names), prepare(wide))
  ok(length(trapped) == 0L, "public preparation avoids model/native/provider/dense-init terminals")

  policy <- list(reader = "streaming", max_source_bytes = 1048576,
    max_read_bytes = 7 * 1048576, max_materialized_values = 50000)
  old_budget <- diagnostic_budget(20, 100, 2000)
  budget <- diagnostic_budget(20, 100, 2000, posterior = policy)
  exact_old <- structure(list(max_draws = 20L, max_observations = 100L,
    max_simulated_values = 2000L), class = "measurement_diagnostic_budget")
  ok(identical(serialize(old_budget, NULL, version = 2), serialize(exact_old, NULL, version = 2)),
     "original three-field diagnostic budget retains exact serialized shape")
  ok(identical(diagnostic_budget(20, 100, 2000, posterior = NULL), old_budget),
     "explicit NULL keeps historical diagnostic budget exact")
  ok(identical(names(budget), c(names(exact_old), "posterior")), "streaming opt-in adds only nested posterior policy")
  for (field in c("max_source_bytes", "max_posterior_rows", "max_draw_values")) {
    args <- list(max_draws = 20, max_observations = 100, max_simulated_values = 2000, posterior = policy)
    args[[field]] <- 1
    err(do.call(diagnostic_budget, args), "E_DIAGNOSTIC_BUDGET", paste("mixed legacy acquisition field refuses:", field))
  }
  ok(diagnostic_budget(21,100,2100,posterior=policy)$max_draws==21L,"explicit D21 admitted")

  ids <- c(2L, 7L, 11L, 19L)
  # These are inert, deliberately nonexistent labels. No source identity or
  # source-byte claims are made by this in-memory control.
  paths <- paste0("IN_MEMORY_READER_DOUBLE_ONLY/chain-", c(19L, 2L, 11L, 7L), ".never-read")
  backend_trap <- function(...) stop("inert engineering backend trap")
  environment(backend_trap) <- baseenv()
  make_controller <- function(s = spec) structure(list(spec = s,
    code = "ENGINEERING CONTROLLER DOUBLE: no executable/model source retained",
    run = list(chains = 4L, iter_sampling = 32L),
    backend = list(draws = backend_trap, metadata = backend_trap),
    cache = list(parameter_summary = data.frame(variable = "alpha[1]", mean = .2, sd = .1, rhat = 1.01, ess_bulk = 30, ess_tail = 20),
      inference = list(status = "not_assessed"), posterior_propriety = list(status = "not_assessed"),
      posterior_normalizability = list(status = "not_assessed"), predictive_fit = list(status = "not_run", reason = "in-memory engineering control")),
    provenance = list(kind = "in_memory_reader_double_engineering_only_not_posterior_evidence",
      model_fingerprint = s$fingerprint, output_files = paths)), class = c("bmeas_fit", "bhmm_fit"))
  fit <- make_controller()
  bytes <- serialize(fit, NULL)
  set.seed(707); rng <- .Random.seed; kind <- RNGkind()
  reader_calls <- list()
  live_readers <- c(".bh_posterior_ingest_csv", ".bh_ppc_ingest_csv")
  passive_terminals <- unique(c(terminal_names, live_readers, "file", "readLines", "file.info", "file.exists",
    "normalizePath", "loadNamespace", "requireNamespace", "tempfile", "saveRDS", "check_model",
    ".bh_validate_fingerprint", ".bh_ppc_validate_saved_spec", ".mp_hash"))
  plan <- with_bindings(traps(passive_terminals), ppc_plan(fit, 8L, budget, level = "population"))
  old_plan <- with_bindings(traps(passive_terminals), ppc_plan(fit, 8L, old_budget, level = "population"))
  ok(length(trapped) == 0L && identical(bytes, serialize(fit, NULL)) && identical(rng, .Random.seed) && identical(kind, RNGkind()),
     "both public PPC plans preserve bytes/RNG and do no file/hash/provider/dense-init work")
  ok(identical(plan$version, "bmeas_basic_ppc_plan_v3") && identical(old_plan$version, "bmeas_basic_ppc_plan_v3") &&
     is.na(plan$source$observed_rows) && plan$source$recorded_path_count == 4L,
     "streaming plan exposes recorded counts without claiming source acquisition")
  ok(!any(c("max_source_bytes", "max_posterior_rows", "max_draw_values") %in% names(plan$budget)) &&
     plan$source$policy$max_source_rows == 1000000 &&
     plan$source$policy_origin[["max_source_rows"]] == "finite_default" &&
     plan$source$policy_origin[["max_read_bytes"]] == "explicit",
     "resolved new acquisition quotas stay nested and disclose explicit/default origins")
  ok(old_plan$source$maximum_admissible_source_rows == 100L &&
     !"posterior" %in% names(old_plan$budget), "old plan keeps all-row acquisition meanings and ceilings")
  ok(plan$source$expected_chains$value == 4 && identical(plan$source$selection, list(method = "balanced", draws = 8L)),
     "saved configured chain count and balanced selection reach the plan")

  with_bindings(traps(live_readers), {
    err(ppc_plan(fit, 3L, budget, level = "population"), "E_PPC_SOURCE_SELECTION", "D below all four recorded chains refuses before acquisition")
    err(ppc_plan(fit, 21L, budget, level = "population"), "E_DIAGNOSTIC_BUDGET", "D above twenty refuses before acquisition")
    short <- fit; short$provenance$output_files <- paths[1:3]
    extra <- fit; extra$provenance$output_files <- c(paths, "IN_MEMORY_READER_DOUBLE_ONLY/extra.never-read")
    err(ppc_plan(short, 8L, budget, level = "population"), "E_PPC_SOURCE_IDENTITY", "missing recorded path versus expected chain count refuses")
    err(ppc_plan(extra, 8L, budget, level = "population"), "E_PPC_SOURCE_IDENTITY", "extra recorded path versus expected chain count refuses")
    conflicting <- fit; conflicting$execution_receipt <- list(version = "bmeas_execution_receipt_v1", sampling = list(configured_chains = 3L))
    err(ppc_plan(conflicting, 8L, budget, level = "population"), "E_PPC_SOURCE_IDENTITY", "contradictory saved chain contexts refuse")
    small_policy <- policy; small_policy$max_chains <- 3L
    err(ppc_plan(fit, 8L, diagnostic_budget(20, 100, 2000, posterior = small_policy), level = "population"),
        "E_DIAGNOSTIC_BUDGET", "recorded paths cannot be dropped to meet a smaller streaming chain quota")
    small_policy <- policy; small_policy$max_materialized_values <- 1L
    err(ppc_plan(fit, 8L, diagnostic_budget(20, 100, 2000, posterior = small_policy), level = "population"),
        "E_DIAGNOSTIC_BUDGET", "known consumer and projected buffers refuse insufficient materialized allowance")
    receipt_only <- fit; receipt_only$run <- NULL
    receipt_only$execution_receipt <- list(version = "bmeas_execution_receipt_v1", sampling = list(configured_chains = 4L))
    ok(ppc_plan(receipt_only, 8L, budget, level = "population")$source$expected_chains$value == 4,
       "saved execution receipt supplies expected chain count when run context is absent")
    unknown <- fit; unknown$run <- NULL
    ok(is.null(ppc_plan(unknown, 8L, budget, level = "population")$source$expected_chains$value),
       "absent expected context stays unavailable without inventing chain IDs")
    legacy <- structure(list(backend = fit$backend), class = "measurement_fit")
    err(ppc_plan(legacy, 8L, budget), "E_DIAGNOSTIC_BUDGET", "legacy measurement_fit rejects new policy before context/backend access")
    err(assess_measurement_fit(legacy, draws = 8L, seed = 23L, budget = budget), "E_DIAGNOSTIC_BUDGET",
        "legacy assessment rejects new policy before context/backend access")
  })

  # Invented finite source states: four chains x 32 ranks = 128 complete joint
  # rows. The test selection is independent of either real reader algorithm.
  make_state <- function(level, chain, rank) {
    p <- private(".bh_ppc_forward_plan")(spec, level)
    x <- setNames(rep(0, length(p$required_variables)), p$required_variables)
    x[grepl("^alpha\\[", names(x))] <- .1 + chain / 1000 + rank / 10000
    x[grepl("^effective_loadings\\[", names(x))] <- 1
    x[grepl("^obs_sd\\[", names(x))] <- .4
    x[grepl("^task_row_scale\\[", names(x))] <- .8
    for (base in c("L_component", "L_parameter", "L_task_work", "D_parameter")) for (i in 1:12) {
      key <- paste0(base, "[", i, ",", i, "]")
      if (key %in% names(x)) x[key] <- if (base == "D_parameter") .5 else 1
    }
    x[grepl("^eta\\[", names(x))] <- seq_len(sum(grepl("^eta\\[", names(x)))) / 10 + chain / 1000 + rank / 10000
    x
  }
  sources <- lapply(c("population", "conditional"), function(level)
    setNames(lapply(ids, function(id) do.call(rbind, lapply(seq_len(32L), function(rank) make_state(level, id, rank)))), ids))
  names(sources) <- c("population", "conditional")
  source_slots <- sum(vapply(sources, function(s) sum(vapply(s, length, integer(1))), integer(1)))
  ok(source_slots <= 8192L && all(vapply(sources, function(s) all(vapply(s, nrow, integer(1)) == 32L), logical(1))),
     "in-memory joint-source fixture stays within four chains/32 ranks/8192 numeric slots")
  selection <- function(draws) {
    allocation <- rep(draws %/% 4L, 4L) + as.integer(seq_len(4L) <= draws %% 4L)
    ranks <- lapply(allocation, function(a) if (a == 1L) 16L else as.integer(1 + floor((0:(a - 1L)) * 31 / (a - 1L))))
    rows <- list()
    for (r in seq_len(max(allocation))) for (c in seq_len(4L)) if (r <= allocation[c])
      rows[[length(rows) + 1L]] <- data.frame(chain_id = ids[c], saved_row = ranks[[c]][r])
    list(keys = do.call(rbind, rows), allocations = data.frame(chain_id = ids, source_rows = rep(32L, 4L),
      selected_rows = allocation, omitted_rows = 32L - allocation))
  }
  active_level <- "population"
  reader_mode <- "success"
  partial <- list(version = 1L, status = "failed", engineering_control = "partial in-memory double receipt")
  acquired_value <- function(variables, draws, streaming) {
    chosen <- selection(draws)
    values <- do.call(rbind, lapply(seq_len(nrow(chosen$keys)), function(i)
      sources[[active_level]][[as.character(chosen$keys$chain_id[i])]][chosen$keys$saved_row[i], variables, drop = FALSE]))
    receipt <- if (streaming) list(version = 1L, status = "acquired", source_rows = 128L,
      selected_rows = draws, selected_required_values = length(values),
      selection = list(method = "balanced", allocations = chosen$allocations),
      reader = list(package = "bmeas", function_name = "private_streaming_csv"),
      required_numeric_conversions = (128L + draws) * length(variables),
      lexical_fields = list(validation = 0L, projection = 0L), alias_comparisons = 0L,
      buffer_schedule = list(scope = "in-memory control only; actual acquisition accounting untested"),
      read_bytes_measured = 0, hash_bytes_reserved = 0, read_bytes_charged = 0) else
      list(actual_posterior_rows = 128L, all_row_shape = c(iterations = 32L, chains = 4L, variables = length(variables)),
        materialized_required_values = 128L * length(variables),
        reader = list(package = "labelled_legacy_in_memory_double", called = FALSE), original_fit_hashes_present = FALSE)
    receipt$engineering_control <- "IN-MEMORY DOUBLE; no source/parser/provider qualification"
    list(draws = values, draw_keys = chosen$keys, receipt = receipt)
  }
  reader_effect <- function() {
    # Deliberately contaminate every reader attempt; controller must restore the
    # caller and reset the response stream after successful acquisition.
    RNGkind("Wichmann-Hill", "Inversion", "Rejection"); runif(3)
    if (reader_mode == "interrupt") stop(structure(list(message = "controlled in-memory reader interrupt"), class = c("interrupt", "condition")))
    if (reader_mode == "error") stop(structure(list(message = "controlled in-memory source failure", call = NULL,
      code = "E_PPC_SOURCE_CHANGED", receipt = partial), class = c("bh_ppc_source_error", "measurement_error", "error", "condition")))
  }
  streaming_reader <- function(files, variables, posterior, states, alias_groups = list(), expected_model = NULL, expected_chains = NULL, consumer_buffers = NULL, ...) {
    reader_calls[[length(reader_calls) + 1L]] <<- list(path = "streaming_double", files = files, variables = variables,
      posterior = posterior, states = states, expected_model = expected_model, expected_chains = expected_chains, consumer_buffers = consumer_buffers)
    reader_effect()
    acquired_value(variables, states$draws, TRUE)
  }
  legacy_reader <- function(files, variables, budget, draws, expected_model = NULL, ...) {
    reader_calls[[length(reader_calls) + 1L]] <<- list(path = "legacy_double", files = files, variables = variables, draws = draws)
    reader_effect()
    acquired_value(variables, draws, FALSE)
  }
  replacements <- c(traps(terminal_names), list(.bh_posterior_ingest_csv = streaming_reader, .bh_ppc_ingest_csv = legacy_reader))
  assessment <- function(x = fit, level = active_level, draws = 8L, use_budget = budget)
    assess_measurement_fit(x, checks = "ppc", level = level, draws = draws, seed = 23L, budget = use_budget)
  with_bindings(replacements, {
    results <- list()
    for (level in c("population", "conditional")) {
      active_level <- level
      set.seed(317); before <- .Random.seed; before_kind <- RNGkind()
      out <- assessment()
      old_out <- assessment(use_budget = old_budget)
      ok(out$cache$predictive_fit$status %in% c("computed", "warning"), paste(level, "public streaming assessment computes tiny R response discrepancies"))
      ok(identical(out$cache$ppc$table, old_out$cache$ppc$table) &&
         identical(out$cache$ppc$replicate_statistics, old_out$cache$ppc$replicate_statistics),
         paste(level, "exact table and replicate parity with identical legacy/streaming double joint states"))
      ok(identical(before, .Random.seed) && identical(before_kind, RNGkind()) && identical(bytes, serialize(fit, NULL)),
         paste(level, "whole-call reader and response success restores caller RNG and input bytes"))
      work <- out$cache$ppc$metadata$actual_work
      ok(work$source_rows == 128L && work$selected_rows == 8L && work$selected_required_values == 8L * length(out$cache$ppc$metadata$plan$required_variables) &&
         is.null(work$materialized_required_values) && identical(out$cache$ppc$metadata$draw_keys, selection(8L)$keys),
         paste(level, "all-source and selected work remain distinct with exact selected keys"))
      ok(identical(out$cache$ppc$metadata$posterior_selection, out$cache$ppc$metadata$source_receipt$selection) &&
         identical(out$cache$inference, fit$cache$inference) && identical(out$cache$posterior_normalizability, fit$cache$posterior_normalizability),
         paste(level, "selection receipt and upstream evidence retained without reinterpretation"))
      results[[level]] <- out
    }
    active_level <- "population"
    first <- reader_calls[[1L]]
    ok(identical(first$files, paths) && first$expected_chains == 4L && identical(first$variables, plan$required_variables) &&
       identical(first$states, list(method = "balanced", draws = 8L)) && identical(first$consumer_buffers, plan$source$consumer_buffers),
       "controller forwards every recorded path in caller order, expected count and exact projection/buffers")
    for (draws in c(4L, 7L, 20L)) {
      out <- assessment(draws = draws)
      ok(out$cache$ppc$metadata$completed_replications == draws && identical(out$cache$ppc$metadata$draw_keys, selection(draws)$keys),
         paste("public controller keeps requested D and complete joint keys:", draws))
    }
    ok(identical(selection(4L)$keys$saved_row, rep(16L, 4L)) &&
       identical(selection(7L)$allocations$selected_rows, c(2L, 2L, 2L, 1L)),
       "test double uses documented midpoint/endpoints and balanced remainder allocation")

    current <- results$population
    before <- .Random.seed; before_kind <- RNGkind(); current_bytes <- serialize(current, NULL)
    reader_mode <- "error"
    failed <- assessment(current)
    ok(identical(failed$cache$predictive_fit$status, "failed") && is.null(failed$cache$ppc$table) &&
       is.null(failed$cache$ppc$replicate_statistics) && identical(failed$cache$ppc$metadata$source_receipt, partial) &&
       identical(failed$cache$ppc$metadata$failure$code, "E_PPC_SOURCE_CHANGED"),
       "current acquisition failure replaces saved success with current failure and exact partial receipt")
    ok(identical(before, .Random.seed) && identical(before_kind, RNGkind()) && identical(current_bytes, serialize(current, NULL)),
       "whole-call source error restores caller RNG and leaves original success untouched")
    older <- current; older$cache$ppc$metadata$posterior_selection <- NULL
    older$cache$ppc$metadata$source_receipt <- list(historical = "older reader receipt")
    failed_old <- assessment(older)
    ok(is.null(failed_old$cache$ppc$table) && is.null(failed_old$cache$ppc$replicate_statistics) &&
       is.null(failed_old$cache$ppc$metadata$posterior_selection) && identical(failed_old$cache$ppc$metadata$source_receipt, partial),
       "current source failure replaces historical-reader success without stale selection")
    reader_mode <- "interrupt"
    interrupted <- tryCatch(assessment(current), interrupt = identity)
    ok(inherits(interrupted, "interrupt") && identical(before, .Random.seed) && identical(before_kind, RNGkind()) &&
       identical(current_bytes, serialize(current, NULL)), "reader interrupt propagates after full RNG restoration and preserves historical result")
    reader_mode <- "success"
    rm(.Random.seed, envir = .GlobalEnv)
    invisible(assessment())
    ok(!exists(".Random.seed", .GlobalEnv, inherits = FALSE), "absent caller seed restored after successful reader and response work")
    reader_mode <- "error"; invisible(assessment())
    ok(!exists(".Random.seed", .GlobalEnv, inherits = FALSE), "absent caller seed restored after acquisition error")
    reader_mode <- "interrupt"; invisible(tryCatch(assessment(), interrupt = identity))
    ok(!exists(".Random.seed", .GlobalEnv, inherits = FALSE), "absent caller seed restored after acquisition interrupt")
    reader_mode <- "success"; set.seed(707)
    before <- .Random.seed; before_kind <- RNGkind()
    for (failure_type in c("error", "interrupt")) {
      response_failure <- local({
        type <- failure_type
        function(...) {
          RNGkind("Wichmann-Hill", "Inversion", "Rejection"); runif(2)
          if (type == "interrupt") stop(structure(list(message = "controlled response interrupt"), class = c("interrupt", "condition")))
          stop(structure(list(message = "controlled response numerical failure", call = NULL, code = "E_PPC_NUMERICAL"),
            class = c("measurement_error", "error", "condition")))
        }
      })
      answer <- with_bindings(list(.bh_ppc_simulate_responses = response_failure),
        tryCatch(assessment(current), interrupt = identity))
      ok(if (failure_type == "interrupt") inherits(answer, "interrupt") else
        identical(answer$cache$predictive_fit$status, "failed") && is.null(answer$cache$ppc$table) &&
        is.null(answer$cache$ppc$replicate_statistics) &&
        identical(answer$cache$ppc$metadata$failure$phase, "forward_simulation") &&
        identical(answer$cache$ppc$metadata$failure$code, "E_PPC_NUMERICAL"),
        paste("response", failure_type, "preserves current-attempt failure/propagation semantics"))
      ok(identical(before, .Random.seed) && identical(before_kind, RNGkind()) && identical(current_bytes, serialize(current, NULL)),
        paste("whole-call response", failure_type, "restores RNG and original cached success"))
    }

    # Baseline loader is read-only and optional outside this development tree.
    # It supplies old controller functions only, with the same labelled reader
    # double; it never invokes its actual CSV reader.
    baseline_root <- Sys.getenv("MEASUREMENTPKG_PPC_BASELINE")
    if (!nzchar(baseline_root) && source_mode) baseline_root <- file.path(dirname(source_root), "checkpoint-parser-1")
    if (nzchar(baseline_root) && file.exists(file.path(baseline_root, "R", "frontend-predict.R"))) {
      baseline <- new.env(parent = package_env)
      for (name in c("predict.R", "frontend-predict.R", "frontend-fit-checks-ppc.R"))
        sys.source(file.path(baseline_root, "R", name), envir = baseline)
      assign(".bh_ppc_ingest_csv", legacy_reader, baseline)
      old_reference_plan <- baseline$ppc_plan(fit, 8L, old_budget, level = "population")
      old_reference <- baseline$assess_measurement_fit(fit, checks = "ppc", level = "population", draws = 8L, seed = 23L, budget = old_budget)
      current_old <- assessment(use_budget = old_budget)
      ok(identical(serialize(old_plan, NULL, version = 2), serialize(old_reference_plan, NULL, version = 2)),
         "old plan is byte-exact against immutable pre-controller baseline functions")
      ok(identical(current_old$cache$ppc, old_reference$cache$ppc) && identical(current_old$cache$predictive_fit, old_reference$cache$predictive_fit),
         "old cached output is exact against immutable pre-controller baseline with identical double")
    } else cat("NOT RUN immutable old-controller comparison: set MEASUREMENTPKG_PPC_BASELINE to its source root\n")

    passive_bytes <- serialize(current, NULL); passive_rng <- .Random.seed
    with_bindings(traps(passive_terminals), {
      shown <- fit_checks(current)
      printed <- capture.output(print(summary(current)))
      invisible(capture.output(print(current$cache$ppc$table)))
      ok(identical(shown$predictive$cached$posterior_source$availability, "retained") &&
         any(grepl("128 validated retained states; 8 balanced selected states", printed, fixed = TRUE)),
         "passive summary discloses source and selected denominators")
      ok(identical(shown$predictive$cached$table$record, current$cache$ppc$table), "passive fit_checks keeps cached discrepancy table exact")
      ok(identical(passive_bytes, serialize(current, NULL)) && identical(passive_rng, .Random.seed),
         "summary fit_checks and saved-output reads preserve bytes/RNG under all terminal traps")
      bad_cases <- list()
      bad <- current; bad$cache$ppc$metadata$posterior_selection <- structure(list(method = "balanced"), class = "bad_optional_selection"); bad_cases$classed_selection <- bad
      bad <- current; bad$cache$ppc$metadata$source_receipt$selection$allocations$omitted_rows[1L] <- -1L; bad_cases$invalid_omitted <- bad
      bad <- current; bad$cache$ppc$metadata$source_receipt$selected_rows <- 128L; bad_cases$selected_exceeds_ppc <- bad
      bad <- current; bad$cache$ppc$metadata$source_receipt$source_rows <- 127L; bad_cases$source_count_mismatch <- bad
      bad <- current; bad$cache$ppc$metadata$source_receipt$selection$allocations$chain_id <- rev(ids); bad_cases$unsorted_chains <- bad
      bad <- current
      bad$cache$ppc$metadata$source_receipt$selection$allocations$selected_rows <- c(3L, 2L, 2L, 1L)
      bad$cache$ppc$metadata$source_receipt$selection$allocations$omitted_rows <- c(29L, 30L, 30L, 31L)
      bad$cache$ppc$metadata$posterior_selection <- bad$cache$ppc$metadata$source_receipt$selection
      bad_cases$unbalanced_allocation <- bad
      bad <- current
      bad$cache$ppc$metadata$source_receipt$selected_rows <- 7L
      bad$cache$ppc$metadata$source_receipt$selected_required_values <- 7L * plan$required_columns
      bad$cache$ppc$metadata$source_receipt$selection$allocations$selected_rows <- c(1L, 2L, 2L, 2L)
      bad$cache$ppc$metadata$source_receipt$selection$allocations$omitted_rows <- c(31L, 30L, 30L, 30L)
      bad$cache$ppc$metadata$posterior_selection <- bad$cache$ppc$metadata$source_receipt$selection
      bad_cases$remainder_assigned_to_wrong_ids <- bad
      for (name in names(bad_cases)) {
        bad <- bad_cases[[name]]; snapshot <- serialize(bad, NULL)
        report <- fit_checks(bad); invisible(capture.output(print(summary(bad))))
        ok(identical(report$predictive$cached$posterior_source$availability, "unavailable_invalid_metadata") &&
           identical(report$predictive$cached$table$record, current$cache$ppc$table) &&
           identical(report$predictive$cached$status, current$cache$predictive_fit$status) && identical(snapshot, serialize(bad, NULL)),
           paste("malformed optional selection remains local:", name))
      }
      historical <- current; historical$cache$ppc$metadata$posterior_selection <- NULL
      historical$cache$ppc$metadata$source_receipt <- NULL
      report <- fit_checks(historical)
      ok(is.null(report$predictive$cached$posterior_source) && identical(report$predictive$cached$table$record, current$cache$ppc$table),
         "historical records without source-selection annotations retain their exact table")
    })
  })

  # Forecasts perform saved-context validation/hashing as before, but no CSV,
  # provider, code-generation, fitting or dense initialization occurs.
  with_bindings(traps(c(terminal_names, live_readers)), {
    resource <- execution_plan(spec, chains = 4L, iter = 64L, warmup = 32L, cores = 1L,
      resources = list(max_output_bytes = 2^24, cache = "deferred"))
    support <- resource$postprocessing_support$ppc
    ok(identical(support$status, "streaming_policy_required") &&
       identical(support$legacy_reader$status, "configured_layout_incompatible") &&
       identical(support$streaming$status, "explicit_policy_required") && grepl("unobserved", support$actual_source_support),
       "four-chain/128-row forecast distinguishes legacy incompatibility from unverified streaming policy")
    geometry <- execution_plan(wide_spec, chains = 4L, iter = 64L, warmup = 32L, cores = 1L,
      resources = list(max_output_bytes = 2^24, cache = "deferred"))$postprocessing_support$ppc
    ok(identical(geometry$status, "streaming_policy_required") &&
       identical(geometry$streaming$status, "explicit_policy_required") && !any(grepl("N30", geometry$findings, fixed = TRUE)),
       "streaming forecast requires explicit geometry accounting policy")
    many_chains <- execution_plan(spec, chains = 21L, iter = 4L, warmup = 2L, cores = 1L,
      resources = list(max_output_bytes = 2^24, cache = "deferred"))$postprocessing_support$ppc
    ok(identical(many_chains$streaming$status, "explicit_policy_required"), "forecast requires explicit higher chain and replication policy")
  })
  ok(length(trapped) == 0L, "no actual reader model/native/provider or forbidden passive terminal was reached")
  cat("TOTAL", passed, "passed;", length(failures), "failed; STUB MODE ONLY; no parser/source qualification\n")
  if (length(failures)) stop(paste(failures, collapse = "\n"))
})
