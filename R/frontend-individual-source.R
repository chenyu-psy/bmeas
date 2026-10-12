# All-retained-row projection of the qualified PPC acquisition path.
# Uses the shared .bh_ppc_source_* parsing/copy/version/shape helpers.
# Deliberately keep this narrow orchestration copy separate so PPC selection,
# public budgets, errors, and receipt behavior retain their qualified bytes.
# Changed branches: exact four-field individual budget (including validation of
# the upstream max_parameter_cells cap); no draw request; minimum two retained
# states; all-row keys/allocation/fill; an individual temporary-directory prefix;
# and explicit all-row/current-input-origin receipt labels. Parameter-cell work
# is checked by the caller; acquisition checks bytes, rows and scalar values.
# No selection helper, thinning, RNG, backend methods, or fitting is used here.
# original_hashes accepts the same plain saved MD5 metadata as the PPC path.
.bh_individual_ingest_csv <- function(files, variables, budget,
                                      expected_model = NULL, original_hashes = NULL) {
  receipt <- list(status = "acquiring", source_kind = "recorded_cmdstan_sample_hmc_csv",
                  hash_algorithm = "MD5 of exact file bytes",
                  historical_identity = "read-time receipt; original fit-time hashes absent",
                  origin_authentication = "current-byte hashes and supplied identities do not authenticate original fit-time CSV, executable, or data origin",
                  saved_row_convention = "1-based retained post-warmup CSV data row; not an original sampler iteration")
  owned <- NULL
  original_cwd <- getwd()
  on.exit(suspendInterrupts({
    # Restore cwd before removing any owned copies, including on interruption.
    restored <- tryCatch({ setwd(original_cwd); TRUE }, error = function(e) FALSE)
    if (restored && !is.null(owned)) try(unlink(owned, recursive = TRUE, force = TRUE), silent = TRUE)
  }), add = TRUE)
  tryCatch({
    if (missing(files) || missing(variables) || missing(budget))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "files, variables and budget are required")
    if (typeof(files) != "character" || is.object(files) ||
        !is.null(attr(files, "dim", exact = TRUE)) || length(files) < 1L || length(files) > 2L ||
        anyNA(files) || any(!nzchar(files)) || anyDuplicated(files))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "supply one or two distinct plain recorded CSV paths")
    if (typeof(variables) != "character" || is.object(variables) ||
        !is.null(attr(variables, "dim", exact = TRUE)) || anyNA(variables) ||
        any(!grepl("^[A-Za-z][A-Za-z0-9_]*(\\[[1-9][0-9]*(,[1-9][0-9]*)*\\])?$", variables)) ||
        anyDuplicated(variables) || any(endsWith(variables, "__")))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "variables must be distinct exact scalar names excluding sampler diagnostics")
    receipt$variables <- variables
    caps <- c(max_source_bytes = 10 * 1024^2, max_posterior_rows = 100,
              max_draw_values = 50000, max_parameter_cells = 1000)
    lower <- c(max_source_bytes = 1, max_posterior_rows = 2,
               max_draw_values = 0, max_parameter_cells = 0)
    if (typeof(budget) != "list" || is.object(budget) || !is.null(attr(budget, "dim", exact = TRUE)))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "a plain-list individual-parameter budget is required")
    budget_names <- names(budget)
    if (typeof(budget_names) != "character" || is.object(budget_names) ||
        !is.null(attr(budget_names, "dim", exact = TRUE)) || anyNA(budget_names) ||
        any(!nzchar(budget_names)) || anyDuplicated(budget_names) || !setequal(names(caps), budget_names))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "the four exact individual-parameter budget fields are required")
    for (key in names(caps))
      caps[key] <- .bh_ppc_source_int(budget[[key]], key, lower[[key]], caps[[key]])
    # Two retained states are the smallest admitted summary capability. The
    # actual all-row cost is checked incrementally by the shared preflight.
    if (length(variables) * 2 > caps[["max_draw_values"]])
      .bh_ppc_source_abort("E_PPC_SOURCE_VALUES", "minimum all-row required values exceed the draw-value budget")
    if (!is.null(expected_model) && (typeof(expected_model) != "character" || is.object(expected_model) ||
                                    !is.null(attr(expected_model, "dim", exact = TRUE)) ||
                                    length(expected_model) != 1L || is.na(expected_model) || !nzchar(expected_model)))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "expected_model must be NULL or one saved CSV model name")
    receipt$original_fit_hashes_present <- !is.null(original_hashes)
    if (receipt$original_fit_hashes_present)
      receipt$historical_identity <- "read-time receipt; original fit-time hashes supplied but unverified"
    supplied <- original_hashes
    if (!is.null(supplied)) {
      if (is.object(supplied) || !is.null(attr(supplied, "dim", exact = TRUE)))
        .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "original_hashes must be a plain vector or plain metadata list")
      if (typeof(supplied) == "list") {
        hash_names <- names(supplied)
        if (typeof(hash_names) != "character" || is.object(hash_names) ||
            !is.null(attr(hash_names, "dim", exact = TRUE)) || anyNA(hash_names) ||
            anyDuplicated(hash_names) || !setequal(hash_names, c("algorithm", "hashes")) ||
            !identical(supplied[["algorithm"]], "md5"))
          .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "original hash metadata requires plain algorithm='md5' and hashes fields")
        supplied <- supplied[["hashes"]]
      }
      if (typeof(supplied) != "character" || is.object(supplied) ||
          !is.null(attr(supplied, "dim", exact = TRUE)) ||
          length(supplied) != length(files) || anyNA(supplied) ||
          any(!grepl("^[0-9A-Fa-f]{32}$", supplied)))
        .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "original_hashes must contain one plain original MD5 per file")
      hash_names <- names(supplied)
      if (!is.null(hash_names)) {
        if (typeof(hash_names) != "character" || is.object(hash_names) ||
            !is.null(attr(hash_names, "dim", exact = TRUE)) || anyNA(hash_names) ||
            anyDuplicated(hash_names) || !setequal(hash_names, files))
          .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "named original hashes must identify the recorded paths")
        supplied <- supplied[match(files, hash_names)]
      }
      supplied <- unname(tolower(supplied))
    }
    receipt$budget <- as.list(caps)
    receipt$requested_rows <- "all_retained"
    receipt$recorded_paths <- unname(files)
    receipt$expected_model <- expected_model
    paths <- normalizePath(files, winslash = "/", mustWork = TRUE)
    if (anyDuplicated(paths))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "multiple recorded paths resolve to the same source")
    before <- .bh_ppc_source_stat(paths)
    if (sum(before$size) > caps[["max_source_bytes"]])
      .bh_ppc_source_abort("E_PPC_SOURCE_BYTES", "actual aggregate source bytes exceed the acquisition budget")
    owned <- tempfile("bmeas-individual-")
    if (!dir.create(owned, mode = "0700"))
      .bh_ppc_source_abort("E_PPC_SOURCE_IO", "could not create an owned acquisition directory")
    safe <- sprintf("chain%02d.csv", seq_along(paths))
    copied <- file.path(owned, safe)
    verify <- file.path(owned, sprintf("verify%02d.csv", seq_along(paths)))
    bytes <- numeric(length(paths))
    for (i in seq_along(paths))
      bytes[i] <- .bh_ppc_source_copy(paths[i], copied[i], caps[["max_source_bytes"]] - sum(bytes))
    hashes <- .bh_ppc_source_hash(copied)
    verified_bytes <- numeric(length(paths))
    for (i in seq_along(paths))
      verified_bytes[i] <- .bh_ppc_source_copy(paths[i], verify[i], caps[["max_source_bytes"]] - sum(verified_bytes))
    current_hashes <- .bh_ppc_source_hash(verify)
    after <- .bh_ppc_source_stat(paths)
    receipt$sources <- data.frame(recorded_path = unname(files), source_bytes = bytes,
                                  copy_md5 = hashes, current_source_md5 = current_hashes,
                                  stringsAsFactors = FALSE)
    receipt$source_bytes <- sum(bytes)
    receipt$bounded_copy_passes <- 2L
    if (!identical(before, after) || !identical(bytes, verified_bytes) ||
        !identical(hashes, current_hashes) || !identical(as.numeric(before$size), bytes))
      .bh_ppc_source_abort("E_PPC_SOURCE_CHANGED", "recorded source changed during bounded acquisition")
    if (!is.null(supplied)) {
      receipt$sources$original_fit_md5 <- supplied
      if (!identical(supplied, hashes)) {
        receipt$historical_identity <- "current bytes do not match the supplied original fit-time MD5 hashes"
        .bh_ppc_source_abort("E_PPC_SOURCE_CHANGED", "current bytes differ from the supplied original fit-time hashes")
      }
      receipt$historical_identity <- "current bytes match the supplied original fit-time MD5 hashes"
    }
    parsed <- vector("list", length(paths))
    total_rows <- 0L
    for (i in seq_along(paths)) {
      parsed[[i]] <- .bh_ppc_source_preflight(copied[i], variables,
          caps[["max_posterior_rows"]] - total_rows,
          caps[["max_draw_values"]] - total_rows * length(variables))
      total_rows <- total_rows + parsed[[i]]$n
    }
    ids <- vapply(parsed, function(x) x$metadata$id, 0L)
    counts <- vapply(parsed, function(x) x$n, 0L)
    if (anyDuplicated(ids) || length(unique(counts)) != 1L)
      .bh_ppc_source_abort("E_PPC_SOURCE_CHAINS", "chains need distinct known integer IDs and equal positive actual row counts")
    comparable <- setdiff(c("model", "method", "algorithm", "engine", "metric", "num_samples",
                            "thin", "save_warmup", "num_warmup", "stan_version_major",
                            "stan_version_minor", "stan_version_patch"), "id")
    if (length(parsed) == 2L && (!identical(parsed[[1]]$columns, parsed[[2]]$columns) ||
        !identical(parsed[[1]]$metadata[comparable], parsed[[2]]$metadata[comparable])))
      .bh_ppc_source_abort("E_PPC_SOURCE_CHAINS", "chain headers describe inconsistent sampling layouts")
    model <- parsed[[1]]$metadata$model
    if (!is.null(expected_model) && !identical(model, expected_model))
      .bh_ppc_source_abort("E_PPC_SOURCE_IDENTITY", "CSV model name differs from the saved expected identity")
    order <- order(ids)
    parsed <- parsed[order]
    safe <- safe[order]
    ids <- ids[order]
    receipt$sources <- receipt$sources[order, , drop = FALSE]
    rownames(receipt$sources) <- NULL
    receipt$sources$chain_id <- ids
    receipt$sources$retained_rows <- counts[order]
    receipt$sources$sig_figs <- vapply(parsed, function(x) {
      if (is.null(x$metadata$sig_figs)) NA_integer_ else x$metadata$sig_figs
    }, 0L)
    receipt$model_name <- model
    receipt$source_precision_convention <- "sig_figs is recorded CSV precision; NA means absent, -1 means CmdStan default"
    receipt$all_row_shape <- c(iterations = counts[1L], chains = length(ids), variables = length(variables))
    receipt$actual_posterior_rows <- total_rows
    receipt$materialized_required_values <- total_rows * length(variables)
    if (total_rows < 2L)
      .bh_ppc_source_abort("E_INDIVIDUAL_SOURCE_CAPABILITY",
          "individual summaries require at least two retained source rows; a one-row source is outside this report capability")
    keys <- data.frame(chain_id = rep(ids, times = counts[1L]),
                       saved_row = rep(seq_len(counts[1L]), each = length(ids)))
    receipt$selection <- keys
    receipt$selection_method <- "all retained rows; saved-row rank then ascending numeric chain ID; no selection or thinning"
    result <- matrix(numeric(total_rows * length(variables)), nrow = total_rows,
                     ncol = length(variables), dimnames = list(NULL, variables))
    receipt$reader <- list(package = "cmdstanr", function_name = "read_cmdstan_csv",
                           version = if (requireNamespace("cmdstanr", quietly = TRUE))
                             .bh_ppc_source_reader_version() else NA_character_,
                           supported_version = "0.9.0",
                           format = "draws_array", called = FALSE,
                           sampler_diagnostics = "none")
    if (length(variables)) {
      if (!requireNamespace("cmdstanr", quietly = TRUE))
        .bh_ppc_source_abort("E_PPC_SOURCE_READER", "CmdStanR is required for bounded scalar ingestion")
      if (!identical(receipt$reader$version, receipt$reader$supported_version))
        .bh_ppc_source_abort("E_PPC_SOURCE_READER_VERSION", "this bounded ingestion slice requires the qualified CmdStanR 0.9.0 reader")
      # Only these generated ASCII basenames reach CmdStanR's shell reader.
      setwd(owned)
      receipt$reader$called <- TRUE
      fresh <- tryCatch(withCallingHandlers(
          cmdstanr::read_cmdstan_csv(safe, variables = variables,
                                    sampler_diagnostics = "", format = "draws_array"),
          warning = function(w) .bh_ppc_source_abort("E_PPC_SOURCE_READER", conditionMessage(w))),
          error = function(e) {
            if (inherits(e, "bh_ppc_source_error")) stop(e)
            .bh_ppc_source_abort("E_PPC_SOURCE_READER", conditionMessage(e))
          })
      setwd(original_cwd)
      array <- fresh$post_warmup_draws
      expected_dim <- as.integer(c(counts[1L], length(ids), length(variables)))
      if (!is.numeric(array) || !identical(dim(array), expected_dim) ||
          !identical(dimnames(array)[[3L]], variables) || any(!is.finite(array)) ||
          !identical(as.integer(fresh$metadata$id), ids) ||
          !is.null(fresh$warmup_draws) || !is.null(fresh$post_warmup_sampler_diagnostics))
        .bh_ppc_source_abort("E_PPC_SOURCE_READER_SHAPE", "fresh reader output disagrees with the validated exact scalar/chain shape")
      for (i in seq_along(ids)) {
        actual <- matrix(as.numeric(array[, i, , drop = FALSE]), nrow = counts[1L],
                         ncol = length(variables), dimnames = list(NULL, variables))
        if (!isTRUE(all.equal(actual, parsed[[i]]$values, tolerance = 0, check.attributes = TRUE)))
          .bh_ppc_source_abort("E_PPC_SOURCE_READER_VALUES", "fresh reader changed the validated scalar or chain mapping")
      }
      for (i in seq_len(total_rows))
        result[i, ] <- as.numeric(array[keys$saved_row[i], match(keys$chain_id[i], ids), ])
    } else {
      receipt$reader$skip_reason <- "zero required scalar columns; validated source keys suffice"
    }
    if (!identical(.bh_ppc_source_hash(copied), hashes))
      .bh_ppc_source_abort("E_PPC_SOURCE_CHANGED", "owned copied bytes changed during extraction")
    receipt$selected_shape <- c(draws = nrow(result), variables = ncol(result))
    receipt$status <- "acquired"
    list(draws = result, draw_keys = keys, receipt = receipt)
  }, error = function(e) {
    receipt$status <- "failed"
    if (inherits(e, "bh_ppc_source_error")) {
      e$receipt <- receipt
      stop(e)
    }
    .bh_ppc_source_abort("E_PPC_SOURCE_IO", conditionMessage(e), receipt)
  })
}
