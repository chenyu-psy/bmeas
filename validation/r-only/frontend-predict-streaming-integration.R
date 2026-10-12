source(file.path("helpers", "native-guard.R"))
# Separately authorized PUBLIC CONTROLLER + CLEARED REAL PARSER integration.
# Synthetic 2.40 HMC-format files and explicitly labelled engineering controller
# doubles only. No genuine fitted object, provider, native execution or sampling.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(source_root)
if (source_mode) for (f in list.files(file.path(source_root, "R"), full.names = TRUE)) source(f) else library(bmeas)
package_env <- if (source_mode) .GlobalEnv else asNamespace("bmeas")
private <- function(name) get(name, package_env, inherits = FALSE)
local({
  passed <- 0L; forbidden_calls <- character(); actual_calls <- 0L
  ok <- function(x, label) { if (!isTRUE(x)) stop(label); passed <<- passed + 1L; cat("PASS", label, "\n") }
  replace <- function(name, value) {
    if (source_mode) assign(name, value, package_env) else assignInNamespace(name, value, "bmeas")
  }
  terminals <- c("stancode", "standata", ".bh_backend_ir", ".bh_materialize_init", ".bh_compile_scalar",
    ".bh_fit_scalar", ".bh_cache_scalar_fit", ".bh_provider_runtime_info", ".bh_provider_preflight",
    "compile_measurement", "fit_measurement", ".bh_ppc_ingest_csv", ".bh_ppc_source_reader_version")
  saved <- setNames(lapply(terminals, private), terminals)
  real_reader <- private(".bh_posterior_ingest_csv")
  on.exit({for (name in names(saved)) replace(name, saved[[name]]); replace(".bh_posterior_ingest_csv", real_reader)}, add = TRUE)
  for (name in terminals) replace(name, local({ terminal <- name
    function(...) { forbidden_calls <<- c(forbidden_calls, terminal); stop("Forbidden native/provider/legacy boundary: ", terminal) }
  }))
  actual_reader <- function(...) { actual_calls <<- actual_calls + 1L; if (actual_calls > 4L) stop("four-acquisition test envelope exceeded"); real_reader(...) }
  replace(".bh_posterior_ingest_csv", actual_reader)
  evidence <- Sys.getenv("BMEAS_PPC_INTEGRATION_EVIDENCE")
  if (!nzchar(evidence)) evidence <- tempfile("bmeas-public-streaming-integration-")
  if (!dir.exists(evidence)) dir.create(evidence, recursive = TRUE)
  fixture_dir <- file.path(evidence, "fixtures")
  if (!dir.create(fixture_dir)) stop("fixture destination must be new; existing evidence is never overwritten")
  d <- expand.grid(person = paste0("person", 1:4), task = c("a", "b"), repeat_id = 1:2, stringsAsFactors = FALSE)
  d$y <- sin(seq_len(nrow(d))); d$obs <- paste0("row", seq_len(nrow(d)))
  tasks <- c("a", "b")
  spec <- bmeas(bmf(G = ~score$mu[tasks],
    loadings = list(fixed = list(list(value = 1, cells = list(G = ~score$mu[tasks])))),
    residual = list(targets = ~score$mu[tasks])),
    data = list(score = observe(gaussian_model(), d, "person", "task", "y", units = "score", observation_id = "obs")), fit = FALSE)
  forwards <- setNames(lapply(c("population", "conditional"), function(level) private(".bh_ppc_forward_plan")(spec, level)), c("population", "conditional"))
  variables <- unique(unlist(lapply(forwards, `[[`, "required_variables"), use.names = FALSE))
  ok(length(variables) <= 32L, "narrow combined population/conditional source has at most 32 required columns")
  ids <- c(2L, 7L, 11L, 19L)
  state <- function(id, rank) {
    x <- setNames(rep(0, length(variables)), variables)
    # Dyadic fixture offsets survive exact decimal-to-binary roundtrip across
    # the verified R runtimes; the oracle remains independently generated.
    x[grepl("^alpha\\[", names(x))] <- 1/8 + id / 1024 + rank / 16384
    x[grepl("^effective_loadings\\[", names(x))] <- 1
    x[grepl("^obs_sd\\[", names(x))] <- .4
    x[grepl("^task_row_scale\\[", names(x))] <- .8
    for (base in c("L_component", "L_parameter", "L_task_work", "D_parameter")) for (i in 1:12) {
      key <- paste0(base, "[", i, ",", i, "]")
      if (key %in% names(x)) x[key] <- if (base == "D_parameter") .5 else 1
    }
    x[grepl("^eta\\[", names(x))] <- seq_len(sum(grepl("^eta\\[", names(x)))) / 16 + id / 1024 + rank / 16384
    x
  }
  states <- setNames(lapply(ids, function(id) do.call(rbind, lapply(1:32, function(rank) state(id, rank)))), ids)
  csv_names <- gsub("\\]", "", gsub("[\\[,]", ".", variables))
  write_fixture <- function(id) {
    path <- file.path(fixture_dir, paste0("ENGINEERING-ONLY-chain-", id, ".csv"))
    header <- c("# ENGINEERING FIXTURE: invented joint values, not posterior evidence",
      "# stan_version_major = 2", "# stan_version_minor = 40", "# stan_version_patch = 0",
      "# model = scalar_components_model", paste0("# id = ", id), "# method = sample (Default)",
      "# num_samples = 32", "# num_warmup = 0", "# save_warmup = false (Default)", "# thin = 1 (Default)",
      "# algorithm = hmc (Default)", "# engine = nuts (Default)", "# metric = diag_e (Default)", "# sig_figs = 18",
      paste(c("lp__", csv_names), collapse = ","))
    rows <- vapply(seq_len(32L), function(rank) {
      value <- states[[as.character(id)]][rank, ]
      text <- format(value, digits = 17, scientific = TRUE, trim = TRUE)
      stopifnot(identical(as.double(text), unname(value)))
      paste(c("-1", text), collapse = ",")
    }, "")
    footer <- c("# Elapsed Time: 0 seconds (Warm-up)", "# 0.01 seconds (Sampling)", "# 0.01 seconds (Total)")
    writeLines(c(header, rows, footer), path, useBytes = TRUE)
    path
  }
  paths <- unname(vapply(c(19L, 2L, 11L, 7L), write_fixture, ""))
  B <- sum(file.info(paths)$size)
  original_hashes <- tools::md5sum(paths)
  ok(length(paths) == 4L && B <= 1048576 && nrow(d) == 16L, "four complete 32-row sources fit the one-MiB aggregate/16-response-row envelope")
  backend_trap <- function(...) stop("engineering controller backend trap")
  environment(backend_trap) <- baseenv()
  fit <- structure(list(spec = spec, code = "ENGINEERING CONTROLLER DOUBLE: no executable/model source retained",
    run = list(chains = 4L, iter_sampling = 32L), backend = list(draws = backend_trap, metadata = backend_trap),
    cache = list(parameter_summary = data.frame(variable = "alpha[1]", mean = .2, sd = .1, rhat = 1.01, ess_bulk = 30, ess_tail = 20),
      inference = list(status = "not_assessed"), posterior_propriety = list(status = "not_assessed"),
      posterior_normalizability = list(status = "not_assessed"), predictive_fit = list(status = "not_run", reason = "synthetic CSV engineering integration")),
    provenance = list(kind = "synthetic_240_csv_engineering_controller_only_not_posterior_evidence",
      model_fingerprint = spec$fingerprint, output_files = paths)), class = c("bmeas_fit", "bhmm_fit"))
  budget <- diagnostic_budget(8L, 16L, 128L, posterior = list(reader = "streaming", max_source_bytes = 1048576,
    max_read_bytes = 7 * 1048576, max_materialized_values = 1000000))
  expected_keys <- data.frame(chain_id = rep(ids, 2L), saved_row = rep(c(1L, 32L), each = 4L))
  allocations <- data.frame(chain_id = ids, source_rows = rep(32L, 4L), selected_rows = rep(2L, 4L), omitted_rows = rep(30L, 4L))
  oracle_reader <- function(files, variables, posterior, states, ...) {
    values <- do.call(rbind, lapply(seq_len(8L), function(i) {
      draw <- state(expected_keys$chain_id[i], expected_keys$saved_row[i])
      unname(draw[variables])
    }))
    colnames(values) <- variables
    list(draws = values, draw_keys = expected_keys,
      receipt = list(version = 1L, status = "acquired", source_rows = 128L, selected_rows = 8L,
        selected_required_values = length(values), reader = list(package = "bmeas", function_name = "private_streaming_csv"),
        selection = list(method = "balanced", allocations = allocations),
        engineering_control = "in-memory joint-value oracle only"))
  }
  environment_state <- function() list(kind = RNGkind(), seed = .Random.seed, cwd = getwd(),
    owned = sort(list.files(tempdir(), pattern = "^bmeas-streaming-", full.names = TRUE)),
    connections = showConnections(all = TRUE))
  records <- list()
  bytes <- serialize(fit, NULL)
  for (level in c("population", "conditional")) {
    set.seed(947); before <- environment_state()
    plan <- ppc_plan(fit, 8L, budget, level = level)
    out <- assess_measurement_fit(fit, checks = "ppc", level = level, draws = 8L, seed = 23L, budget = budget)
    if (identical(out$cache$predictive_fit$status, "failed")) print(out$cache$ppc$metadata$failure)
    ok(out$cache$predictive_fit$status %in% c("computed", "warning"), paste(level, "public actual-reader integration computes discrepancies"))
    ok(identical(before, environment_state()) && identical(bytes, serialize(fit, NULL)), paste(level, "actual acquisition and response simulation restore RNG/cwd/connections/owned paths and input bytes"))
    receipt <- out$cache$ppc$metadata$source_receipt
    ok(identical(receipt$status, "acquired") && identical(receipt$reader$function_name, "private_streaming_csv") &&
      identical(receipt$reader$provider_called, FALSE) && receipt$source_rows == 128L && receipt$selected_rows == 8L &&
      receipt$selected_required_values == 8L * length(forwards[[level]]$required_variables) &&
      identical(out$cache$ppc$metadata$draw_keys, expected_keys), paste(level, "real parser receipt has full/selected counts and exact joint keys"))
    ok(receipt$read_bytes_charged == 7 * B && receipt$read_bytes_measured == 4 * B && receipt$hash_bytes_reserved == 3 * B &&
      receipt$temp_bytes_written == 2 * B && receipt$required_numeric_conversions == 136 * length(forwards[[level]]$required_variables),
      paste(level, "public receipt retains exact seven-pass accounting and full-source-plus-selected conversions"))
    ok(identical(receipt$recorded_paths, paths) && receipt$expected_chain_count_context$value == 4 &&
      identical(out$cache$inference, fit$cache$inference) && identical(out$cache$posterior_normalizability, fit$cache$posterior_normalizability) &&
      identical(out$cache$ppc$metadata$law, plan$law), paste(level, "all recorded sources saved context law and upstream statuses stay intact"))
    replace(".bh_posterior_ingest_csv", oracle_reader)
    oracle <- assess_measurement_fit(fit, checks = "ppc", level = level, draws = 8L, seed = 23L, budget = budget)
    replace(".bh_posterior_ingest_csv", actual_reader)
    ok(identical(out$cache$ppc$table, oracle$cache$ppc$table) && identical(out$cache$ppc$replicate_statistics, oracle$cache$ppc$replicate_statistics),
      paste(level, "actual source/controller exact table and replicate parity against independent in-memory joint-value oracle"))
    reversed <- fit; reversed$provenance$output_files <- rev(paths)
    replay <- assess_measurement_fit(reversed, checks = "ppc", level = level, draws = 8L, seed = 23L, budget = budget)
    ok(identical(out$cache$ppc$table, replay$cache$ppc$table) && identical(out$cache$ppc$replicate_statistics, replay$cache$ppc$replicate_statistics) &&
      identical(expected_keys, replay$cache$ppc$metadata$draw_keys) && identical(replay$cache$ppc$metadata$source_receipt$recorded_paths, rev(paths)),
      paste(level, "reversed actual path order preserves exact scientific output and honest caller order"))
    ok(identical(before, environment_state()) && identical(original_hashes, tools::md5sum(paths)), paste(level, "all bounded integration calls leave process state and synthetic sources unchanged"))
    shown <- fit_checks(out); printed <- capture.output(print(summary(out)))
    ok(identical(shown$predictive$cached$posterior_source$availability, "retained") &&
      identical(shown$predictive$cached$table$record, out$cache$ppc$table) &&
      any(grepl("128 validated retained states; 8 balanced selected states", printed, fixed = TRUE)),
      paste(level, "public passive reports display the saved real-parser selection receipt"))
    ok(if (level == "conditional") identical(out$cache$measurement_structure_fit$status, "not_assessed") else
      out$cache$measurement_structure_fit$status %in% c("computed", "warning"), paste(level, "unchanged population versus conditional assessment scope"))
    records[[level]] <- list(provenance = fit$provenance$kind, plan = plan, source_receipt = receipt,
      draw_keys = out$cache$ppc$metadata$draw_keys, table = out$cache$ppc$table,
      replicate_statistics = out$cache$ppc$replicate_statistics, law = out$cache$ppc$metadata$law,
      predictive_status = out$cache$predictive_fit, measurement_structure_status = out$cache$measurement_structure_fit)
  }
  payload <- list(scope = "synthetic engineering integration only, not posterior evidence", source_bytes = B,
    fixtures = paths, fixture_md5 = original_hashes, records = records)
  artifact <- file.path(evidence, "saved-engineering-receipts-and-tables.rds")
  saveRDS(payload, artifact, version = 2)
  ok(identical(readRDS(artifact), payload), "saved receipts keys law discrepancy tables and replicate matrices round-trip exactly")
  ok(actual_calls == 4L && length(forbidden_calls) == 0L, "exactly four small actual acquisitions and no native/provider/legacy calls")
  cat("TOTAL", passed, "public actual-reader assertions passed; source bytes", B, "; actual acquisitions", actual_calls,
    "; synthetic engineering only; no fitting/native/provider/sampling\n")
  cat("EVIDENCE", evidence, "\n")
})
