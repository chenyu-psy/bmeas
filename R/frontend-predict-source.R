# Bounded acquisition for the basic-family PPC frontend. This file does not
# inspect a backend, fit a model, simulate responses, or update a result cache.
.bh_ppc_source_abort <- function(code, message, receipt = NULL) {
  stop(structure(list(message = paste0(code, ": ", message), call = NULL,
                      code = code, receipt = receipt),
                 class = c("bh_ppc_source_error", "measurement_error",
                           "error", "condition")))
}

.bh_ppc_source_int <- function(x, name, lower = 0, upper = .Machine$integer.max) {
  if (!(typeof(x) %in% c("integer", "double")) || is.object(x) ||
      !is.null(attr(x, "dim", exact = TRUE)) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < lower || x > upper)
    .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", paste(name, "is not a supported integer"))
  as.integer(x)
}

.bh_ppc_source_copy <- function(from, to, limit) {
  input <- file(from, open = "rb")
  on.exit(close(input), add = TRUE)
  output <- file(to, open = "wb")
  on.exit(close(output), add = TRUE)
  count <- 0
  repeat {
    bytes <- readBin(input, what = "raw", n = min(65536, limit - count + 1))
    if (!length(bytes)) break
    count <- count + length(bytes)
    if (count > limit)
      .bh_ppc_source_abort("E_PPC_SOURCE_BYTES", "source bytes exceed the acquisition budget")
    if (any(bytes == as.raw(0)))
      .bh_ppc_source_abort("E_PPC_SOURCE_ROW", "NUL bytes are not a supported CSV encoding")
    writeBin(bytes, output)
  }
  count
}

.bh_ppc_source_stat <- function(paths) {
  info <- file.info(paths)
  if (anyNA(info$size) || any(info$isdir) || any(!file_test("-f", paths)) ||
      any(file.access(paths, mode = 4) != 0))
    .bh_ppc_source_abort("E_PPC_SOURCE_UNAVAILABLE", "each recorded source must be a readable regular file")
  info[, c("size", "mtime", "ctime"), drop = FALSE]
}

.bh_ppc_source_hash <- function(paths) {
  hashes <- unname(tools::md5sum(paths))
  if (anyNA(hashes))
    .bh_ppc_source_abort("E_PPC_SOURCE_HASH", "could not hash the bounded source copies")
  hashes
}

.bh_ppc_source_reader_version <- function() {
  as.character(utils::packageVersion("cmdstanr"))
}

.bh_ppc_source_repair_names <- function(x) {
  indexed <- grepl(".", x, fixed = TRUE)
  x[indexed] <- paste0(gsub(".", ",", sub(".", "[", x[indexed], fixed = TRUE),
                           fixed = TRUE), "]")
  x
}

# Parse one already bounded copy, retaining only requested scalar values.
# The values retained here also verify the installed reader's column/chain map.
.bh_ppc_source_preflight <- function(path, variables, row_limit, value_limit) {
  ending <- file(path, open = "rb")
  last_byte <- tryCatch({
    size <- file.info(path)$size
    if (!is.finite(size) || size < 1) raw() else {
      seek(ending, where = size - 1, origin = "start")
      readBin(ending, what = "raw", n = 1L)
    }
  }, finally = close(ending))
  if (!identical(last_byte, as.raw(10)))
    .bh_ppc_source_abort("E_PPC_SOURCE_INCOMPLETE", "CSV must end with a complete newline-terminated record")
  con <- file(path, open = "rt", encoding = "ASCII")
  on.exit(close(con))
  metadata <- list()
  columns <- NULL
  wanted <- integer()
  rows <- list()
  n <- 0L
  footer <- character()
  footer_started <- FALSE
  relevant <- c("stan_version_major", "stan_version_minor", "stan_version_patch",
                "model", "method", "num_samples", "num_warmup", "save_warmup",
                "thin", "id", "algorithm", "engine", "metric", "sig_figs")
  number <- "^[+-]?(([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?|[Ii][Nn][Ff](inity)?|[Nn][Aa][Nn])$"
  repeat {
    line <- readLines(con, n = 1L, warn = FALSE)
    if (!length(line)) break
    if (!nzchar(line)) next
    if (startsWith(line, "#")) {
      text <- trimws(sub("^#", "", line))
      if (grepl("=", text, fixed = TRUE)) {
        pair <- strsplit(text, "=", fixed = TRUE)[[1L]]
        key <- trimws(pair[1L])
        if (key %in% relevant) {
          if (!is.null(columns) || length(pair) != 2L || !is.null(metadata[[key]]))
            .bh_ppc_source_abort("E_PPC_SOURCE_HEADER", paste("ambiguous or misplaced metadata:", key))
          metadata[[key]] <- trimws(sub("[[:space:]]*\\(Default\\)[[:space:]]*$", "", pair[2L]))
        }
      }
      for (kind in c("Warm-up", "Sampling", "Total")) {
        if (endsWith(text, paste0("(", kind, ")"))) {
          if (is.null(columns) || !n || kind %in% footer)
            .bh_ppc_source_abort("E_PPC_SOURCE_INCOMPLETE", "invalid completion footer")
          seconds <- trimws(sub("^Elapsed Time:[[:space:]]*", "", text))
          seconds <- sub(paste0("[[:space:]]+seconds[[:space:]]+\\(", kind, "\\)$"), "", seconds)
          seconds <- suppressWarnings(as.numeric(seconds))
          if (length(seconds) != 1L || !is.finite(seconds) || seconds < 0)
            .bh_ppc_source_abort("E_PPC_SOURCE_INCOMPLETE", "invalid elapsed-time footer")
          footer <- c(footer, kind)
          footer_started <- TRUE
        }
      }
      next
    }
    if (footer_started)
      .bh_ppc_source_abort("E_PPC_SOURCE_INCOMPLETE", "data appears after the completion footer")
    fields <- strsplit(paste0(line, ",."), ",", fixed = TRUE)[[1L]]
    fields <- fields[-length(fields)]
    if (is.null(columns)) {
      if (!length(fields) || any(!grepl("^[A-Za-z][A-Za-z0-9_]*(\\.[1-9][0-9]*)*$", fields)) ||
          anyDuplicated(fields))
        .bh_ppc_source_abort("E_PPC_SOURCE_HEADER", "CSV needs unique canonical scalar column names")
      columns <- .bh_ppc_source_repair_names(fields)
      if (anyDuplicated(columns) || anyNA(match(variables, columns)))
        .bh_ppc_source_abort("E_PPC_SOURCE_VARIABLES", "required scalar columns are missing or ambiguous")
      wanted <- match(variables, columns)
      next
    }
    n <- n + 1L
    if (n > row_limit)
      .bh_ppc_source_abort("E_PPC_SOURCE_ROWS", "actual retained rows exceed the posterior-row budget")
    if (n * length(variables) > value_limit)
      .bh_ppc_source_abort("E_PPC_SOURCE_VALUES", "all-row required values exceed the draw-value budget")
    if (length(fields) != length(columns) || any(!grepl(number, fields, perl = TRUE)))
      .bh_ppc_source_abort("E_PPC_SOURCE_ROW", "CSV has a malformed numeric row or inconsistent row width")
    values <- suppressWarnings(as.numeric(fields[wanted]))
    if (any(!is.finite(values)))
      .bh_ppc_source_abort("E_PPC_SOURCE_NONFINITE", "a required scalar is nonfinite")
    rows[[n]] <- values
  }
  if (is.null(columns) || !n || !identical(footer, c("Warm-up", "Sampling", "Total")))
    .bh_ppc_source_abort("E_PPC_SOURCE_INCOMPLETE", "CSV needs positive retained rows and a complete ordered sampling footer")
  if ((!is.null(metadata$method) && metadata$method != "sample") ||
      (!is.null(metadata$algorithm) && metadata$algorithm != "hmc"))
    .bh_ppc_source_abort("E_PPC_SOURCE_METHOD", "only recognized sample/hmc output is supported")
  required <- setdiff(relevant, "sig_figs")
  if (!all(required %in% names(metadata)))
    .bh_ppc_source_abort("E_PPC_SOURCE_HEADER", "required sampling metadata is absent")
  # CmdStan's Boolean argument serializer emits lowercase false/true. Retain
  # numeric 0/1 compatibility, but never accept other numeric or truthy values.
  if (!metadata$save_warmup %in% c("false", "true", "0", "1"))
    .bh_ppc_source_abort("E_PPC_SOURCE_HEADER", "invalid Boolean metadata: save_warmup")
  metadata$save_warmup <- if (metadata$save_warmup %in% c("false", "0")) 0L else 1L
  integer_fields <- c("stan_version_major", "stan_version_minor", "stan_version_patch",
                      "num_samples", "num_warmup", "thin", "id")
  for (key in integer_fields) {
    value <- metadata[[key]]
    if (!grepl("^[0-9]+$", value))
      .bh_ppc_source_abort("E_PPC_SOURCE_HEADER", paste("invalid integer metadata:", key))
    metadata[[key]] <- .bh_ppc_source_int(suppressWarnings(as.numeric(value)), key)
  }
  if (metadata$method != "sample" || metadata$algorithm != "hmc" ||
      !(metadata$engine %in% c("nuts", "static")) ||
      !(metadata$metric %in% c("diag_e", "dense_e", "unit_e")))
    .bh_ppc_source_abort("E_PPC_SOURCE_METHOD", "only recognized sample/hmc output is supported")
  if (metadata$save_warmup != 0L)
    .bh_ppc_source_abort("E_PPC_SOURCE_WARMUP", "save_warmup must explicitly be zero")
  if (metadata$id < 1L || metadata$thin < 1L || metadata$num_samples < 1L ||
      !nzchar(metadata$model) || metadata$stan_version_major < 2L)
    .bh_ppc_source_abort("E_PPC_SOURCE_HEADER", "invalid sampling identity or iteration metadata")
  if (n != ceiling(metadata$num_samples / metadata$thin))
    .bh_ppc_source_abort("E_PPC_SOURCE_INCOMPLETE", "actual retained rows disagree with the declared sampling layout")
  if (!is.null(metadata$sig_figs)) {
    if (!grepl("^-?[0-9]+$", metadata$sig_figs))
      .bh_ppc_source_abort("E_PPC_SOURCE_HEADER", "invalid sig_figs metadata")
    metadata$sig_figs <- .bh_ppc_source_int(as.numeric(metadata$sig_figs), "sig_figs", -1L)
  }
  values <- if (length(variables)) do.call(rbind, rows) else matrix(numeric(), n, 0L)
  storage.mode(values) <- "double"
  colnames(values) <- variables
  list(metadata = metadata, columns = columns, n = n, values = values)
}

.bh_ppc_source_selection <- function(chain_ids, iterations, draws) {
  chains <- length(chain_ids)
  if (draws < chains || draws > chains * iterations)
    .bh_ppc_source_abort("E_PPC_SOURCE_SELECTION", "draw request cannot be selected without replacement across the recorded chains")
  allocation <- rep(draws %/% chains, chains) + (seq_len(chains) <= draws %% chains)
  indices <- lapply(allocation, function(a) {
    if (a == 1L) as.integer(floor((iterations + 1) / 2)) else
      as.integer(1 + floor((0:(a - 1)) * (iterations - 1) / (a - 1)))
  })
  keys <- data.frame(chain_id = integer(draws), saved_row = integer(draws))
  at <- 0L
  for (rank in seq_len(max(allocation))) {
    for (chain in seq_len(chains)) {
      if (rank <= allocation[chain]) {
        at <- at + 1L
        keys[at, ] <- c(chain_ids[chain], indices[[chain]][rank])
      }
    }
  }
  if (anyDuplicated(keys))
    .bh_ppc_source_abort("E_PPC_SOURCE_SELECTION", "posterior selection contains duplicate joint states")
  keys
}

# Arguments are saved plain paths and exact scalar names, never backend methods.
# original_hashes: NULL, or one MD5 per input file (optionally named by its path),
# or list(algorithm = "md5", hashes = that_character_vector).
.bh_ppc_ingest_csv <- function(files, variables, budget, draws,
                               expected_model = NULL, original_hashes = NULL) {
  receipt <- list(status = "acquiring", source_kind = "recorded_cmdstan_sample_hmc_csv",
                  hash_algorithm = "MD5 of exact file bytes",
                  historical_identity = "read-time receipt; original fit-time hashes absent",
                  saved_row_convention = "1-based retained post-warmup CSV data row; not an original sampler iteration")
  owned <- NULL
  original_cwd <- getwd()
  on.exit(suspendInterrupts({
    # Restore cwd before removing any owned copies, including on interruption.
    restored <- tryCatch({ setwd(original_cwd); TRUE }, error = function(e) FALSE)
    if (restored && !is.null(owned)) try(unlink(owned, recursive = TRUE, force = TRUE), silent = TRUE)
  }), add = TRUE)
  tryCatch({
    if (missing(files) || missing(variables) || missing(budget) || missing(draws))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "files, variables, budget and draws are required")
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
    caps <- c(max_draws = 20, max_source_bytes = 10 * 1024^2,
              max_posterior_rows = 100, max_draw_values = 50000)
    if (typeof(budget) != "list" || is.object(budget) || !is.null(attr(budget, "dim", exact = TRUE)))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "a normalized plain-list PPC budget is required")
    budget_names <- names(budget)
    if (typeof(budget_names) != "character" || is.object(budget_names) ||
        !is.null(attr(budget_names, "dim", exact = TRUE)) || anyNA(budget_names) ||
        any(!nzchar(budget_names)) || anyDuplicated(budget_names) || !all(names(caps) %in% budget_names))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "a normalized PPC budget is required")
    for (key in names(caps))
      caps[key] <- .bh_ppc_source_int(budget[[key]], key, if (key == "max_draw_values") 0 else 1, caps[key])
    draws <- .bh_ppc_source_int(draws, "draws", 2L, caps[["max_draws"]])
    if (length(variables) * draws > caps[["max_draw_values"]])
      .bh_ppc_source_abort("E_PPC_SOURCE_VALUES", "required selected values already exceed the draw-value budget")
    if (!is.null(expected_model) && (typeof(expected_model) != "character" || is.object(expected_model) ||
                                    !is.null(attr(expected_model, "dim", exact = TRUE)) ||
                                    length(expected_model) != 1L || is.na(expected_model) || !nzchar(expected_model)))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "expected_model must be NULL or one saved CSV model name")
    receipt$original_fit_hashes_present <- !is.null(original_hashes)
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
    receipt$requested_draws <- draws
    receipt$recorded_paths <- unname(files)
    receipt$expected_model <- expected_model
    paths <- normalizePath(files, winslash = "/", mustWork = TRUE)
    if (anyDuplicated(paths))
      .bh_ppc_source_abort("E_PPC_SOURCE_ARGUMENT", "multiple recorded paths resolve to the same source")
    before <- .bh_ppc_source_stat(paths)
    if (sum(before$size) > caps[["max_source_bytes"]])
      .bh_ppc_source_abort("E_PPC_SOURCE_BYTES", "actual aggregate source bytes exceed the acquisition budget")
    owned <- tempfile("bmeas-ppc-")
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
      if (!identical(supplied, hashes))
        .bh_ppc_source_abort("E_PPC_SOURCE_CHANGED", "current bytes differ from the supplied original fit-time hashes")
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
    keys <- .bh_ppc_source_selection(ids, counts[1L], draws)
    receipt$selection <- keys
    receipt$selection_method <- "balanced allocation by sorted chain ID; evenly spaced saved rows; round-robin by selection rank"
    result <- matrix(numeric(draws * length(variables)), nrow = draws,
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
      for (i in seq_len(draws))
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
