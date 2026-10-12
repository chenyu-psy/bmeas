source(file.path("helpers", "native-guard.R"))
# Focused CSV compatibility controls. Invented HMC-shaped fixtures are engineering
# inputs, not posterior qualification. This script never fits, compiles, simulates
# responses, or invokes a native model. Optional real-file checks are read-only.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(source_root)
if (source_mode) {
  source(file.path(source_root, "R", "frontend-predict-source.R"))
  source(file.path(source_root, "R", "frontend-individual-source.R"))
} else library(bmeas)
private <- function(name) if (source_mode) get(name, .GlobalEnv) else
  getFromNamespace(name, "bmeas")

run_csv_compatibility <- function() {
  evidence_dir <- Sys.getenv("BMEAS_CSV_COMPAT_EVIDENCE_DIR")
  if (nzchar(evidence_dir)) dir.create(evidence_dir, recursive = TRUE, showWarnings = FALSE)
  fixture_dir <- if (nzchar(evidence_dir)) file.path(evidence_dir, "fixtures") else
    tempfile("bmeas-csv-fixtures-")
  dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
  if (!nzchar(evidence_dir)) on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)
  passed <- 0L
  events <- list()
  accepted <- list()
  status <- "failed"
  failure <- NULL
  ok <- function(value, label) {
    if (!isTRUE(value)) stop(label, call. = FALSE)
    passed <<- passed + 1L
    cat("PASS", label, "\n")
  }
  on.exit({
    report <- list(status = status, assertions = passed, failure = failure,
      scope = "bounded private reader compatibility only; no native execution or inference qualification",
      r_version = R.version.string, cmdstanr_version = as.character(utils::packageVersion("cmdstanr")),
      cases = events, accepted = accepted)
    if (nzchar(evidence_dir)) {
      saveRDS(report, file.path(evidence_dir, "csv-compatibility-results.rds"))
      jsonlite::write_json(report, file.path(evidence_dir, "csv-compatibility-results.json"),
        pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null")
    }
  }, add = TRUE)
  tryCatch({
    ok(requireNamespace("cmdstanr", quietly = TRUE) &&
         identical(as.character(utils::packageVersion("cmdstanr")), "0.9.0"),
       "actual qualified CmdStanR 0.9.0 reader is available")
    if (nzchar(evidence_dir)) stopifnot(requireNamespace("jsonlite", quietly = TRUE))
    helpers <- list(ppc = private(".bh_ppc_ingest_csv"),
                    individual = private(".bh_individual_ingest_csv"))
    vars <- c("eta[1,1]", "obs_sd[1]")
    budget <- list(max_source_bytes = 16L * 1024L, max_posterior_rows = 10L,
                   max_draw_values = 20L)
    input_state <- function(paths) list(md5 = tools::md5sum(paths),
      stat = file.info(paths)[, c("size", "mtime", "ctime"), drop = FALSE])
    rng_state <- function() list(kind = RNGkind(),
      present = exists(".Random.seed", .GlobalEnv, inherits = FALSE),
      seed = if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
        get(".Random.seed", .GlobalEnv) else NULL)
    owned_dirs <- function() sort(list.files(tempdir(), pattern = "^bmeas-(ppc|individual)-", full.names = TRUE))
    invoke <- function(helper, paths, label, variables = vars, draws = 4L,
                       expected_code = NULL, limits = budget) {
      before <- list(files = input_state(paths), rng = rng_state(), cwd = getwd(),
                     owned = owned_dirs())
      arguments <- list(files = paths, variables = variables,
        budget = c(limits, if (helper == "ppc") list(max_draws = 10L) else list(max_parameter_cells = 0L)),
        expected_model = "scalar_components_model")
      if (helper == "ppc") arguments$draws <- draws
      result <- tryCatch(do.call(helpers[[helper]], arguments), error = identity)
      actual_code <- if (inherits(result, "error")) result$code else NULL
      event <- list(label = label, helper = helper, expected_code = expected_code,
        actual_code = actual_code, status = if (inherits(result, "error")) "refused" else "acquired",
        reader_called = isTRUE(result$receipt$reader$called))
      events[[length(events) + 1L]] <<- event
      tag <- paste(helper, label)
      ok(identical(before$files, input_state(paths)), paste(tag, "source bytes and stat unchanged"))
      ok(identical(before$rng, rng_state()) && identical(before$cwd, getwd()),
         paste(tag, "RNG and cwd unchanged"))
      ok(identical(before$owned, owned_dirs()), paste(tag, "owned temporary copies cleaned"))
      if (!is.null(expected_code)) {
        ok(inherits(result, "measurement_error") && identical(actual_code, expected_code),
           paste(tag, "refuses", expected_code))
        ok(identical(result$receipt$status, "failed") && !event$reader_called,
           paste(tag, "failed receipt and refusal before reader"))
      } else {
        if (inherits(result, "error")) stop(paste(tag, conditionMessage(result)), call. = FALSE)
        ok(identical(result$receipt$status, "acquired"), paste(tag, "acquired receipt"))
        ok(identical(result$receipt$reader$called, length(variables) > 0L) &&
             identical(result$receipt$reader$version, "0.9.0") &&
             identical(result$receipt$reader$sampler_diagnostics, "none"),
           paste(tag, "actual reader or explicit zero-column skip"))
        ok(identical(result$receipt$bounded_copy_passes, 2L) &&
             identical(result$receipt$source_bytes, sum(file.info(paths)$size)) &&
             identical(result$receipt$sources$copy_md5,
               unname(tools::md5sum(result$receipt$sources$recorded_path))) &&
             identical(result$receipt$sources$copy_md5, result$receipt$sources$current_source_md5),
           paste(tag, "exact byte costs and current-file hash receipt"))
        ok(identical(result$receipt$original_fit_hashes_present, FALSE) &&
             identical(result$receipt$historical_identity,
               "read-time receipt; original fit-time hashes absent"),
           paste(tag, "receipt does not claim original fit authentication"))
        accepted[[paste(helper, label, sep = ":")]] <<- result
      }
      result
    }
    fixture_lines <- function(id = 7L, warmup = "false") c(
      "# ENGINEERING FIXTURE: invented states; not real posterior qualification",
      "# stan_version_major = 2", "# stan_version_minor = 40", "# stan_version_patch = 0",
      "# model = scalar_components_model", paste("# id =", id),
      "# method = sample (Default)", "# num_samples = 3", "# num_warmup = 0",
      paste("# save_warmup =", warmup), "# thin = 1 (Default)",
      "# algorithm = hmc (Default)", "# engine = nuts (Default)",
      "# metric = diag_e (Default)", "# sig_figs = 18",
      "lp__,accept_stat__,eta.1.1,obs_sd.1",
      vapply(seq_len(3L), function(row) paste(-1, 0.9, id * 100L + row,
        id * 100L + 10L + row, sep = ","), ""),
      "# Elapsed Time: 0 seconds (Warm-up)", "# 0 seconds (Sampling)", "# 0 seconds (Total)")
    fixture <- function(label, lines, newline = TRUE) {
      path <- file.path(fixture_dir, paste0(label, ".csv"))
      if (newline) writeLines(lines, path, useBytes = TRUE) else
        writeChar(paste(lines, collapse = "\n"), path, eos = NULL, useBytes = TRUE)
      path
    }
    check_values <- function(result, helper, label, ids = c(2L, 7L), iterations = 3L,
                             variables = vars, selected = c(1L, 3L)) {
      rows <- if (helper == "individual") seq_len(iterations) else selected
      keys <- data.frame(chain_id = rep(ids, times = length(rows)),
                         saved_row = rep(rows, each = length(ids)))
      expected <- if (length(variables)) cbind(
        keys$chain_id * 100 + keys$saved_row,
        keys$chain_id * 100 + 10 + keys$saved_row) else matrix(numeric(), nrow(keys), 0L)
      colnames(expected) <- variables
      ok(identical(result$draw_keys, keys) && identical(result$draws, expected),
         paste(helper, label, "exact scalar values and saved-row/chain order"))
      ok(identical(unname(result$receipt$all_row_shape),
           as.integer(c(iterations, length(ids), length(variables)))) &&
           result$receipt$actual_posterior_rows == iterations * length(ids) &&
           result$receipt$materialized_required_values == iterations * length(ids) * length(variables) &&
           identical(result$receipt$sources$chain_id, ids) &&
           identical(unname(result$receipt$selected_shape), as.integer(dim(expected))),
         paste(helper, label, "exact all-row and selected costs"))
    }
    # Same-token and mixed-token chains must normalize to the same zero value.
    layouts <- list(false = c("false", "false"), zero = c("0", "0"),
      false_default = rep("false (Default)", 2L), zero_default = rep("0 (Default)", 2L),
      mixed = c("false", "0"), mixed_default = c("false (Default)", "0 (Default)"))
    set.seed(20261006L)
    for (label in names(layouts)) {
      paths <- vapply(seq_along(layouts[[label]]), function(i)
        fixture(paste0(label, "-", c(7L, 2L)[i]),
          fixture_lines(c(7L, 2L)[i], layouts[[label]][i])), "")
      for (helper in names(helpers)) {
        result <- invoke(helper, paths, label)
        check_values(result, helper, label)
      }
    }
    paths <- c(fixture("zero-column-7", fixture_lines(7L, "false (Default)")),
               fixture("zero-column-2", fixture_lines(2L, "0 (Default)")))
    empty_limits <- budget; empty_limits$max_draw_values <- 0L
    for (helper in names(helpers)) {
      result <- invoke(helper, paths, "zero-columns", character(), limits = empty_limits)
      check_values(result, helper, "zero-columns", variables = character())
    }
    saved_seed <- .Random.seed
    rm(".Random.seed", envir = .GlobalEnv)
    for (helper in names(helpers)) {
      result <- invoke(helper, paths, "absent-seed")
      check_values(result, helper, "absent-seed")
    }
    assign(".Random.seed", saved_seed, .GlobalEnv)
    # All refusals use both orchestrators and must happen before CmdStanR reads.
    reject <- function(label, lines, code, newline = TRUE, limits = budget) {
      path <- fixture(label, lines, newline)
      for (helper in names(helpers)) invisible(invoke(helper, path, label,
        draws = 2L, expected_code = code, limits = limits))
    }
    for (token in c("true", "1", "true (Default)", "1 (Default)"))
      reject(paste0("warmup-", gsub("[^A-Za-z0-9]", "_", token)), fixture_lines(warmup = token),
        "E_PPC_SOURCE_WARMUP")
    invalid <- c("FALSE", "TRUE", "False", "True", "fAlSe", "F", "T", "yes", "no", "NA",
      "", "00", "01", "0.0", "1.0", "+0", "-0", "+1", "-1", "2", "0e0", "1e0",
      "false(Default)(Default)", "false (default)", "false (Default) trailing", "false true")
    for (i in seq_along(invalid)) reject(paste0("unknown-", i),
      fixture_lines(warmup = invalid[i]), "E_PPC_SOURCE_HEADER")
    lines <- fixture_lines()
    warmup_at <- grep("^# save_warmup =", lines)
    reject("absent-warmup", lines[-warmup_at], "E_PPC_SOURCE_HEADER")
    reject("duplicate-warmup", append(lines, lines[warmup_at], after = warmup_at), "E_PPC_SOURCE_HEADER")
    reject("misplaced-warmup", append(lines[-warmup_at], lines[warmup_at],
      after = grep("^lp__", lines[-warmup_at])), "E_PPC_SOURCE_HEADER")
    reject("extra-equals-warmup", sub("^# save_warmup =.*", "# save_warmup = false = 0", lines),
      "E_PPC_SOURCE_HEADER")
    integer_fields <- c("stan_version_major", "stan_version_minor", "stan_version_patch",
                        "num_samples", "num_warmup", "thin", "id", "sig_figs")
    for (key in integer_fields) for (value in c("false", "true", "1.0", "+1", "1e0"))
      reject(paste0("integer-", key, "-", gsub("[^A-Za-z0-9]", "_", value)),
        sub(paste0("^# ", key, " =.*"), paste0("# ", key, " = ", value), lines),
        "E_PPC_SOURCE_HEADER")
    for (entry in list(c("method", "optimize"), c("method", "variational"),
      c("algorithm", "fixed_param"), c("algorithm", "metropolis"),
      c("engine", "fixed_param"), c("metric", "unknown")))
      reject(paste0("method-", paste(entry, collapse = "-")),
        sub(paste0("^# ", entry[1L], " =.*"), paste0("# ", entry[1L], " = ", entry[2L]), lines),
        "E_PPC_SOURCE_METHOD")
    for (kind in c("Warm-up", "Sampling", "Total"))
      reject(paste0("missing-footer-", kind), lines[!endsWith(lines, paste0("(", kind, ")"))],
        "E_PPC_SOURCE_INCOMPLETE")
    reject("footer-order", c(head(lines, -3L), rev(tail(lines, 3L))), "E_PPC_SOURCE_INCOMPLETE")
    reject("duplicate-footer", c(lines, tail(lines, 1L)), "E_PPC_SOURCE_INCOMPLETE")
    reject("unterminated-record", lines, "E_PPC_SOURCE_INCOMPLETE", newline = FALSE)
    reject("row-count-mismatch", sub("^# num_samples =.*", "# num_samples = 4", lines),
      "E_PPC_SOURCE_INCOMPLETE")
    reject("data-after-footer", c(lines, "-1,0.9,704,714"), "E_PPC_SOURCE_INCOMPLETE")
    tight <- budget; tight$max_source_bytes <- 1L
    reject("byte-cap", lines, "E_PPC_SOURCE_BYTES", limits = tight)
    tight <- budget; tight$max_posterior_rows <- 2L
    reject("row-cap", lines, "E_PPC_SOURCE_ROWS", limits = tight)
    tight <- budget; tight$max_draw_values <- 5L
    reject("all-row-value-cap", lines, "E_PPC_SOURCE_VALUES", limits = tight)

    # The optional retained real source is never copied into bundled fixtures or
    # altered. Its decimal strings independently supply only the two requested
    # numeric columns; no full draw table is materialized for the expectation.
    real_path <- Sys.getenv("BMEAS_CSV_COMPAT_REAL_FILE")
    if (nzchar(real_path)) {
      real_lines <- readLines(real_path, warn = FALSE)
      plain <- real_lines[nzchar(real_lines) & !startsWith(real_lines, "#")]
      names <- strsplit(plain[1L], ",", fixed = TRUE)[[1L]]
      positions <- match(c("eta.1.1", "obs_sd.1"), names)
      expected <- t(vapply(plain[-1L], function(line)
        as.numeric(strsplit(line, ",", fixed = TRUE)[[1L]][positions]), numeric(2L)))
      dimnames(expected) <- list(NULL, vars)
      ok(identical(dim(expected), c(10L, 2L)) && all(is.finite(expected)),
        "retained real CSV independent expectation is exactly ten rows and twenty values")
      for (helper in names(helpers)) {
        result <- invoke(helper, real_path, "retained-real-file", draws = 10L)
        keys <- data.frame(chain_id = rep(1L, 10L), saved_row = seq_len(10L))
        ok(identical(result$draws, expected) && identical(result$draw_keys, keys),
          paste(helper, "retained real CSV exact values and order"))
        ok(result$receipt$actual_posterior_rows == 10L &&
          result$receipt$materialized_required_values == 20L &&
          identical(unname(result$receipt$all_row_shape), c(10L, 1L, 2L)) &&
          result$receipt$sources$sig_figs == 12L,
          paste(helper, "retained real CSV exact cost and precision receipt"))
        zero <- invoke(helper, real_path, "retained-real-zero-columns", character(), draws = 10L,
                       limits = empty_limits)
        ok(identical(dim(zero$draws), c(10L, 0L)) && identical(zero$draw_keys, keys) &&
          zero$receipt$actual_posterior_rows == 10L && zero$receipt$materialized_required_values == 0L,
          paste(helper, "retained real CSV zero-column costs and keys"))
      }
      subset <- invoke("ppc", real_path, "retained-real-four-selected", draws = 4L)
      rows <- c(1L, 4L, 7L, 10L)
      ok(identical(subset$draws, expected[rows, , drop = FALSE]) &&
        identical(subset$draw_keys$saved_row, rows) &&
        subset$receipt$materialized_required_values == 20L,
        "real PPC selection preserves all-row cost and exact evenly spaced rows")
    }
    status <- "passed"
    cat("CSV COMPATIBILITY:", passed, "assertions;", length(events), "helper calls;",
      length(accepted), "accepted calls\n")
    invisible(passed)
  }, error = function(e) {
    failure <<- conditionMessage(e)
    stop(e)
  })
}

# Optional dependency convention matches the existing bundled frontend tests.
if (requireNamespace("cmdstanr", quietly = TRUE) &&
    identical(as.character(utils::packageVersion("cmdstanr")), "0.9.0")) {
  run_csv_compatibility()
} else cat("SKIP CSV compatibility: qualified CmdStanR 0.9.0 reader unavailable\n")
