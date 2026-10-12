# Private, package-owned retained CSV streaming acquisition. No provider or RNG.
# This is a logical numeric-buffer envelope, not an R heap/RSS limit.
.bh_posterior_abort <- function(code, message, receipt = NULL) {
  stop(structure(list(message = paste0(code, ": ", message), call = NULL,
    code = code, receipt = receipt), class = c("bh_posterior_source_error",
    "bh_ppc_source_error", "measurement_error", "error", "condition")))
}
.bh_posterior_plain <- function(x, type) {
  typeof(x) %in% type && !is.object(x) && is.null(attr(x, "dim", exact = TRUE))
}
.bh_posterior_encoding <- function(x, code = "E_PPC_SOURCE_ARGUMENT") {
  # Inspect declared/native encodings without lossy conversion or regex dispatch.
  if (any(Encoding(x) == "bytes") || any(!validEnc(x)))
    .bh_posterior_abort(code, "text must have a valid declared/native encoding")
  invisible(NULL)
}
.bh_posterior_names <- function(x, allowed = NULL, required = character()) {
  n <- attr(x, "names", exact = TRUE)
  if (!.bh_posterior_plain(n, "character") || length(n) != length(x) || anyNA(n))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "unknown, duplicate, or malformed field names")
  .bh_posterior_encoding(n)
  if (any(!nzchar(n)) || base::anyDuplicated.default(n) ||
      (!is.null(allowed) && any(!n %in% allowed)) || any(!required %in% n))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "unknown, duplicate, or malformed field names")
  n
}
.bh_posterior_optional_names <- function(x) {
  if (!is.null(attr(x, "names", exact = TRUE))) .bh_posterior_names(x)
  invisible(NULL)
}
.bh_posterior_int <- function(x, name, lower = 0, upper = 2^53 - 1) {
  if (!.bh_posterior_plain(x, c("integer", "double")) || length(x) != 1L ||
      is.na(x) || !is.finite(x) || x != floor(x) || x < lower || x > upper)
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", paste(name, "must be a finite exact supported integer"))
  as.double(x)
}
.bh_posterior_add <- function(a, b, name = "count") {
  if (a > (2^53 - 1) - b)
    .bh_posterior_abort("E_PPC_SOURCE_CAPABILITY", paste(name, "exceeds exact count representation"))
  a + b
}
.bh_posterior_mul <- function(a, b, name = "count") {
  if (a && b > floor((2^53 - 1) / a))
    .bh_posterior_abort("E_PPC_SOURCE_CAPABILITY", paste(name, "exceeds exact count representation"))
  value <- a * b
  if (!is.finite(value) || value > 2^53 - 1)
    .bh_posterior_abort("E_PPC_SOURCE_CAPABILITY", paste(name, "exceeds exact count representation"))
  value
}
.bh_posterior_sum <- function(x, name = "count") {
  ans <- 0
  for (v in x) ans <- .bh_posterior_add(ans, v, name)
  ans
}
.bh_posterior_variables <- function(variables) {
  if (!.bh_posterior_plain(variables, "character") || anyNA(variables))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "variables must be distinct exact plain scalar names excluding diagnostics")
  .bh_posterior_encoding(variables)
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_]*(\\[[1-9][0-9]*(,[1-9][0-9]*)*\\])?$", variables)) ||
      base::anyDuplicated.default(variables) || any(endsWith(variables, "__")))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "variables must be distinct exact plain scalar names excluding diagnostics")
  .bh_posterior_optional_names(variables)
  if (length(variables) > .Machine$integer.max)
    .bh_posterior_abort("E_PPC_SOURCE_CAPABILITY", "required-column count exceeds R dimensions")
  invisible(NULL)
}
# Pure resolver: no files, stat, hash, provider, serialization, or callbacks.
.bh_posterior_policy <- function(posterior, variables) {
  .bh_posterior_variables(variables)
  required <- c("reader", "max_source_bytes", "max_read_bytes", "max_materialized_values")
  optional <- c("max_record_bytes", "max_columns", "max_source_rows", "max_chains",
                "max_temp_bytes", "max_scan_values")
  if (!.bh_posterior_plain(posterior, "list"))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "posterior must be a plain named streaming-policy list")
  n <- .bh_posterior_names(posterior, c(required, optional), required)
  if (!identical(posterior[["reader"]], "streaming"))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "posterior reader must be exactly 'streaming'")
  limits <- list()
  for (key in setdiff(n, "reader")) {
    lo <- if (key == "max_scan_values" && !length(variables)) 0 else 1
    hi <- if (key %in% c("max_columns", "max_source_rows", "max_chains", "max_record_bytes"))
      .Machine$integer.max else 2^53 - 1
    limits[[key]] <- .bh_posterior_int(posterior[[key]], key, lo, hi)
  }
  defaults <- list(max_record_bytes = 1048576, max_columns = 100000,
                   max_source_rows = 1000000, max_chains = 32)
  for (key in names(defaults)) if (!key %in% n) limits[[key]] <- defaults[[key]]
  if (!"max_temp_bytes" %in% n)
    limits$max_temp_bytes <- .bh_posterior_mul(2, limits$max_source_bytes, "default temporary bytes")
  if (!"max_scan_values" %in% n)
    limits$max_scan_values <- .bh_posterior_mul(.bh_posterior_mul(2,
      limits$max_source_rows, "default scan rows"), length(variables), "default scan values")
  order <- c(setdiff(required, "reader"), optional)
  limits <- limits[order]
  origins <- setNames(ifelse(order %in% n, "explicit", "finite_default"), order)
  list(limits = limits, origins = origins)
}
.bh_posterior_states <- function(states) {
  if (identical(states, "all")) return(list(method = "all", draws = NULL))
  if (!.bh_posterior_plain(states, "list"))
    .bh_posterior_abort("E_PPC_SOURCE_SELECTION", "states must be 'all' or list(method='balanced', draws=D)")
  .bh_posterior_names(states, c("method", "draws"), c("method", "draws"))
  if (!identical(states$method, "balanced"))
    .bh_posterior_abort("E_PPC_SOURCE_SELECTION", "only balanced selection is supported")
  list(method = "balanced", draws = .bh_posterior_int(states$draws, "selected draws", 1, .Machine$integer.max))
}
.bh_posterior_buffers <- function(x) {
  if (!.bh_posterior_plain(x, "list"))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "consumer_buffers must be plain slot coefficients")
  .bh_posterior_names(x, c("fixed", "per_selected_row"), c("fixed", "per_selected_row"))
  for (kind in names(x)) {
    v <- x[[kind]]
    if (!.bh_posterior_plain(v, c("integer", "double")))
      .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "consumer buffer slots must be plain numeric vectors")
    if (length(v)) .bh_posterior_names(v) else .bh_posterior_optional_names(v)
    for (i in seq_along(v)) .bh_posterior_int(v[[i]], "consumer buffer slots")
  }
  x
}
.bh_posterior_work <- function(x) {
  if (!.bh_posterior_plain(x, "list"))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "consumer_work must be a plain named list")
  .bh_posterior_names(x, c("per_selected_row", "limits"), c("per_selected_row", "limits"))
  for (kind in names(x)) {
    v <- x[[kind]]
    if (!.bh_posterior_plain(v, c("integer", "double")))
      .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "consumer work must use plain numeric coefficients and limits")
    if (length(v)) .bh_posterior_names(v) else .bh_posterior_optional_names(v)
    for (i in seq_along(v)) .bh_posterior_int(v[[i]], "consumer work coefficient/limit")
  }
  if (!identical(names(x$per_selected_row), names(x$limits)) || length(x$per_selected_row) != length(x$limits))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "consumer work coefficients and limits require identical keys")
  x
}
.bh_posterior_aliases <- function(groups, V) {
  if (!.bh_posterior_plain(groups, "list"))
    .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "alias_groups must be a plain list of required-column indices")
  .bh_posterior_optional_names(groups)
  # Groups are disjoint: each retained alias column belongs to one equality set.
  used <- integer()
  for (g in groups) {
    if (!.bh_posterior_plain(g, c("integer", "double")) || length(g) < 2L || anyNA(g) ||
        any(!is.finite(g)) || any(g != floor(g)) || any(g < 1 | g > V) ||
        base::anyDuplicated.default(g) || any(g %in% used))
      .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "alias groups must have distinct disjoint valid required-column indices")
    .bh_posterior_optional_names(g)
    used <- c(used, g)
  }
  list(groups = groups, slots = length(used), redundant = length(used) - length(groups))
}
# All explicit numeric/boolean/index overlap is conservatively charged together.
.bh_posterior_envelope <- function(tx, Q, D) {
  C <- tx$chains; V <- length(tx$variables)
  values <- .bh_posterior_mul(D, V, "projected matrix")
  if (D > .Machine$integer.max || V > .Machine$integer.max ||
      values > if (.Machine$sizeof.pointer == 8L) 2^52 else .Machine$integer.max)
    .bh_posterior_abort("E_PPC_SOURCE_CAPABILITY", "projected matrix or keys exceed R index/dimension representation")
  fixed <- tx$consumer_buffers$fixed
  per <- tx$consumer_buffers$per_selected_row
  diagnostic_slots <- if (is.null(tx$diagnostic)) numeric() else
    .bh_diagnostic_source_slots(tx, D)
  parts <- c(parser_and_accounting_scalars = 256,
    scanner_positions_masks_indices = .bh_posterior_mul(4, tx$chunk_bytes),
    chain_metadata_stat_selection = .bh_posterior_mul(64, C),
    reservation_metadata = .bh_posterior_mul(8, .bh_posterior_sum(c(length(fixed), length(per), length(diagnostic_slots),
      .bh_posterior_mul(2, length(tx$consumer_work$limits))))),
    full_width_header_and_lexical_indices = .bh_posterior_mul(6, Q),
    required_maps_and_current_row = .bh_posterior_mul(6, V),
    alias_indices_and_validation_overlap = .bh_posterior_mul(4, tx$aliases$slots),
    projected_matrix = values, projection_assignment_overlap = values,
    chain_and_rank_keys = .bh_posterior_mul(2, D),
    key_construction_overlap = .bh_posterior_mul(2, D))
  if (length(fixed)) parts <- c(parts, setNames(fixed, paste0("consumer_fixed:", names(fixed))))
  if (length(per)) for (i in seq_along(per))
    parts[paste0("consumer_per_selected_row:", names(per)[i])] <-
      .bh_posterior_mul(per[[i]], D, "consumer selected-row scratch")
  if (length(diagnostic_slots)) parts <- c(parts, diagnostic_slots)
  maximum <- .bh_posterior_sum(parts, "materialized envelope")
  if (is.null(tx$receipt$max_materialized_values) || maximum >= tx$receipt$max_materialized_values) {
    tx$receipt$buffer_schedule_version <- if (is.null(tx$diagnostic)) 1L else 2L
    tx$receipt$buffer_schedule <- base::as.list.default(parts)
    tx$receipt$max_materialized_values <- maximum
    tx$receipt$buffer_dimensions <- c(chains = C, header_columns = Q,
      selected_rows = D, required_columns = V)
  }
  if (maximum > tx$limits$max_materialized_values)
    .bh_posterior_abort("E_PPC_SOURCE_VALUES", "declared simultaneous numeric-buffer envelope exceeds max_materialized_values")
  invisible(maximum)
}
.bh_posterior_stat <- function(paths, tx) {
  tx$receipt$metadata_stat_calls <- tx$receipt$metadata_stat_calls + 1
  tx$receipt$metadata_stat_paths <- tx$receipt$metadata_stat_paths + length(paths)
  tx$receipt$metadata_probes$file_info_batches <- tx$receipt$metadata_probes$file_info_batches + 1
  tx$receipt$metadata_probes$file_info_paths <- tx$receipt$metadata_probes$file_info_paths + length(paths)
  tx$receipt$metadata_probes$regular_file_tests <- tx$receipt$metadata_probes$regular_file_tests + length(paths)
  tx$receipt$metadata_probes$readability_tests <- tx$receipt$metadata_probes$readability_tests + length(paths)
  info <- file.info(paths)
  if (!is.null(tx$diagnostic)) info <- unclass(info)
  if (anyNA(info$size) || any(info$isdir) ||
      any(!(if (is.null(tx$diagnostic)) file_test("-f", paths) else
        .bh_diagnostic_source_file_test(paths))) ||
      any(file.access(paths, mode = 4) != 0) || any(!is.finite(info$size)) ||
      any(info$size < 0) || any(info$size != floor(info$size)))
    .bh_posterior_abort("E_PPC_SOURCE_UNAVAILABLE", "each source must be a readable regular file with finite size")
  if (is.null(tx$diagnostic)) info[, c("size", "mtime", "ctime"), drop = FALSE] else
    .subset(info, c("size", "mtime", "ctime"))
}
.bh_posterior_meter <- function(tx, n, phase, reserved = FALSE) {
  total <- .bh_posterior_add(tx$receipt$read_bytes_charged, n, "logical read bytes")
  if (total > tx$limits$max_read_bytes)
    .bh_posterior_abort("E_PPC_SOURCE_READ_BYTES", "cumulative logical reads exceed max_read_bytes")
  tx$receipt$read_bytes_charged <- total
  key <- if (reserved) "hash_bytes_reserved" else "read_bytes_measured"
  tx$receipt[[key]] <- .bh_posterior_add(tx$receipt[[key]], n)
  tx$receipt$read_passes[[phase]] <- .bh_posterior_add(tx$receipt$read_passes[[phase]], n)
}
.bh_posterior_read <- function(con, tx, phase, remaining) {
  available <- tx$limits$max_read_bytes - tx$receipt$read_bytes_charged
  # A one-byte growth probe is measured, including on rejection. EOF costs zero.
  request <- min(tx$chunk_bytes, remaining + 1, available)
  if (request < 1)
    .bh_posterior_abort("E_PPC_SOURCE_READ_BYTES", "no allowance remains for a bounded read")
  bytes <- readBin(con, "raw", n = request)
  .bh_posterior_meter(tx, length(bytes), phase)
  if (length(bytes) > remaining)
    .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "file grew beyond its preflight byte size")
  if (any(bytes == as.raw(0)))
    .bh_posterior_abort("E_PPC_SOURCE_ROW", "NUL bytes are not a supported CSV encoding")
  bytes
}
.bh_posterior_copy <- function(from, to, size, tx, phase) {
  input <- file(from, "rb"); on.exit(base::close.connection(input), add = TRUE)
  output <- file(to, "wb"); on.exit(base::close.connection(output), add = TRUE)
  n <- 0
  repeat {
    bytes <- .bh_posterior_read(input, tx, phase, size - n)
    if (!length(bytes)) break
    future <- .bh_posterior_add(tx$receipt$temp_bytes_written, length(bytes))
    if (future > tx$limits$max_temp_bytes)
      .bh_posterior_abort("E_PPC_SOURCE_TEMP_BYTES", "owned copy storage exceeds max_temp_bytes")
    writeBin(bytes, output)
    n <- n + length(bytes)
    tx$receipt$temp_bytes_written <- future
  }
  if (n != size) .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "source size changed during copying")
  n
}
.bh_posterior_hash <- function(path, size, tx, phase) {
  before <- .bh_posterior_stat(path, tx)
  if (!identical(as.double(before$size), as.double(size)))
    .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "owned copy size changed before hashing")
  .bh_posterior_meter(tx, size, phase, reserved = TRUE)
  # R tools::md5sum reads a regular file sequentially to EOF in bounded native
  # blocks. This is a reserved full-file logical pass, not measured bytes.
  hash <- unname(tools::md5sum(path))
  after <- .bh_posterior_stat(path, tx)
  if (!identical(before, after))
    .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "owned copy changed during hashing")
  if (length(hash) != 1L || is.na(hash))
    .bh_posterior_abort("E_PPC_SOURCE_HASH", "could not hash bounded owned copy")
  hash
}
.bh_posterior_metadata <- function(metadata) {
  required <- c("stan_version_major", "stan_version_minor", "stan_version_patch", "model",
    "method", "num_samples", "num_warmup", "save_warmup", "thin", "id", "algorithm", "engine", "metric")
  if (!all(required %in% names(metadata)))
    .bh_posterior_abort("E_PPC_SOURCE_HEADER", "required sampling metadata is absent")
  if (!metadata$save_warmup %in% c("false", "true", "0", "1"))
    .bh_posterior_abort("E_PPC_SOURCE_HEADER", "invalid Boolean save_warmup metadata")
  metadata$save_warmup <- if (metadata$save_warmup %in% c("false", "0")) 0 else 1
  ints <- c("stan_version_major", "stan_version_minor", "stan_version_patch", "num_samples", "num_warmup", "thin", "id")
  for (key in ints) {
    if (!grepl("^[0-9]+$", metadata[[key]]))
      .bh_posterior_abort("E_PPC_SOURCE_HEADER", paste("invalid integer metadata:", key))
    value <- suppressWarnings(as.double(metadata[[key]]))
    if (!is.finite(value) || value > .Machine$integer.max)
      .bh_posterior_abort("E_PPC_SOURCE_CAPABILITY", "sampling metadata exceeds R index representation")
    metadata[[key]] <- value
  }
  if (!identical(unname(unlist(metadata[c("stan_version_major", "stan_version_minor", "stan_version_patch")])), c(2, 40, 0)))
    .bh_posterior_abort("E_PPC_SOURCE_READER_VERSION", "streaming grammar is qualified only for CmdStan 2.40.0")
  if (metadata$method != "sample" || metadata$algorithm != "hmc" ||
      !metadata$engine %in% c("nuts", "static") || !metadata$metric %in% c("diag_e", "dense_e", "unit_e"))
    .bh_posterior_abort("E_PPC_SOURCE_METHOD", "only recognized sample/HMC output is supported")
  if (metadata$save_warmup != 0)
    .bh_posterior_abort("E_PPC_SOURCE_WARMUP", "save_warmup must explicitly be false or zero")
  if (metadata$id < 1 || metadata$thin < 1 || metadata$num_samples < 1 || !nzchar(metadata$model))
    .bh_posterior_abort("E_PPC_SOURCE_HEADER", "invalid sampling identity or iteration metadata")
  if (!is.null(metadata$sig_figs)) {
    if (!grepl("^-?[0-9]+$", metadata$sig_figs))
      .bh_posterior_abort("E_PPC_SOURCE_HEADER", "invalid sig_figs metadata")
    metadata$sig_figs <- .bh_posterior_int(suppressWarnings(as.double(metadata$sig_figs)), "sig_figs", -1, .Machine$integer.max)
  }
  metadata
}
.bh_posterior_rank <- function(rank, allocation, L) {
  if (allocation == 1) return(floor((L + 1) / 2))
  numerator <- .bh_posterior_mul(rank - 1, L - 1, "balanced rank numerator")
  denominator <- allocation - 1
  quotient <- floor(numerator / denominator)
  # Floating division can round a fraction immediately below an integer upward.
  # Verify its exact representable product rather than accepting that rounding.
  if (.bh_posterior_mul(quotient, denominator, "balanced quotient check") > numerator)
    quotient <- quotient - 1
  1 + quotient
}
.bh_posterior_layout <- function(tx, L, Q) {
  R <- .bh_posterior_mul(L, tx$chains, "source rows")
  if (R > tx$limits$max_source_rows)
    .bh_posterior_abort("E_PPC_SOURCE_ROWS", "declared complete source rows exceed max_source_rows")
  D <- if (tx$states$method == "all") R else tx$states$draws
  if (D < tx$chains || D > R)
    .bh_posterior_abort("E_PPC_SOURCE_SELECTION", "selection must cover every chain without replacement")
  work <- .bh_posterior_mul(.bh_posterior_add(R, D), length(tx$variables), "required conversions")
  if (work > tx$limits$max_scan_values)
    .bh_posterior_abort("E_PPC_SOURCE_SCAN_VALUES", "validation and selected projection conversions exceed max_scan_values")
  # Check the largest endpoint multiplication before any key allocation.
  a <- ceiling(D / tx$chains)
  if (a > 1) .bh_posterior_mul(a - 1, L - 1, "balanced rank numerator")
  requested <- admitted <- numeric()
  tx$receipt$consumer_work <- list(selected_rows = D, requested = requested, admitted = admitted,
    limits = tx$consumer_work$limits, per_selected_row = tx$consumer_work$per_selected_row)
  if (length(tx$consumer_work$per_selected_row)) for (key in names(tx$consumer_work$per_selected_row)) {
    terms <- .bh_posterior_mul(D, tx$consumer_work$per_selected_row[[key]], "consumer selected-state work")
    requested[key] <- terms
    tx$receipt$consumer_work$requested <- requested
    if (terms > tx$consumer_work$limits[[key]])
      .bh_posterior_abort("E_PPC_SOURCE_CONSUMER_WORK", paste("selected-state consumer work exceeds limit:", key))
    admitted[key] <- terms
    tx$receipt$consumer_work$admitted <- admitted
  }
  .bh_posterior_envelope(tx, Q, D)
  list(R = R, D = D, L = L)
}
.bh_posterior_scan <- function(path, size, tx, phase, chain_position = NULL) {
  con <- file(path, "rb"); on.exit(base::close.connection(con), add = TRUE)
  metadata <- list(); columns <- NULL; wanted <- integer(); n <- 0
  footer <- character(); footer_started <- FALSE
  depth_raw <- character(); depth_misplaced <- FALSE
  pending <- raw(); commas <- 0; bytes_seen <- 0; terminated <- FALSE
  relevant <- c("stan_version_major", "stan_version_minor", "stan_version_patch", "model",
    "method", "num_samples", "num_warmup", "save_warmup", "thin", "id", "algorithm", "engine", "metric", "sig_figs")
  number <- "^[+-]?(([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?|[Ii][Nn][Ff](inity)?|[Nn][Aa][Nn])$"
  selection_rank <- 1; selected_target <- Inf
  if (phase == "projection") selected_target <- .bh_posterior_rank(1, tx$allocation[chain_position], tx$L)
  # Only this package-owned closure sees parsed records; no caller callback.
  consume <- function(line, commas) {
    if (!nzchar(line)) return(invisible(NULL))
    if (startsWith(line, "#")) {
      text <- trimws(sub("^#", "", line))
      if (!is.null(tx$diagnostic) && grepl("^max_depth([[:space:]=]|$)", text)) {
        depth_raw <<- c(depth_raw, line)
        if (!is.null(columns)) depth_misplaced <<- TRUE
      }
      equal <- regexpr("=", text, fixed = TRUE)[1L]
      if (equal > 0) {
        key <- trimws(substr(text, 1, equal - 1))
        if (key %in% relevant) {
          value <- substr(text, equal + 1, nchar(text, type = "bytes"))
          if (!is.null(columns) || grepl("=", value, fixed = TRUE) || !is.null(metadata[[key]]))
            .bh_posterior_abort("E_PPC_SOURCE_HEADER", paste("ambiguous or misplaced metadata:", key))
          metadata[[key]] <<- trimws(sub("[[:space:]]*\\(Default\\)[[:space:]]*$", "", value))
        }
      }
      for (kind in c("Warm-up", "Sampling", "Total")) if (endsWith(text, paste0("(", kind, ")"))) {
        if (is.null(columns) || n == 0 || kind %in% footer)
          .bh_posterior_abort("E_PPC_SOURCE_INCOMPLETE", "invalid completion footer")
        seconds <- trimws(sub("^Elapsed Time:[[:space:]]*", "", text))
        seconds <- sub(paste0("[[:space:]]+seconds[[:space:]]+\\(", kind, "\\)$"), "", seconds)
        if (!grepl(number, seconds, perl = TRUE))
          .bh_posterior_abort("E_PPC_SOURCE_INCOMPLETE", "invalid elapsed-time footer")
        seconds <- suppressWarnings(as.double(seconds))
        if (!is.finite(seconds) || seconds < 0)
          .bh_posterior_abort("E_PPC_SOURCE_INCOMPLETE", "invalid elapsed-time footer")
        footer <<- c(footer, kind); footer_started <<- TRUE
      }
      return(invisible(NULL))
    }
    if (footer_started)
      .bh_posterior_abort("E_PPC_SOURCE_INCOMPLETE", "data appears after completion footer")
    Q <- commas + 1
    if (Q > tx$limits$max_columns)
      .bh_posterior_abort("E_PPC_SOURCE_COLUMNS", "record column count exceeds max_columns before token allocation")
    if (is.null(columns)) {
      metadata <<- .bh_posterior_metadata(metadata)
      L <- ceiling(metadata$num_samples / metadata$thin)
      .bh_posterior_layout(tx, L, Q)
    }
    fields <- strsplit(paste0(line, ",."), ",", fixed = TRUE)[[1L]]
    fields <- fields[-length(fields)]
    if (is.null(columns)) {
      if (length(fields) != Q || any(!grepl("^[A-Za-z][A-Za-z0-9_]*(\\.[1-9][0-9]*)*$", fields)) || base::anyDuplicated.default(fields))
        .bh_posterior_abort("E_PPC_SOURCE_HEADER", "CSV needs unique canonical scalar column names")
      indexed <- grepl(".", fields, fixed = TRUE)
      fields[indexed] <- paste0(gsub(".", ",", sub(".", "[", fields[indexed], fixed = TRUE), fixed = TRUE), "]")
      columns <<- fields
      if (!is.null(tx$diagnostic)) {
        .bh_diagnostic_source_columns(tx, columns)
        .bh_posterior_layout(tx, ceiling(metadata$num_samples / metadata$thin), Q)
      }
      wanted <<- match(tx$variables, columns)
      if (base::anyDuplicated.default(columns) || anyNA(wanted))
        .bh_posterior_abort("E_PPC_SOURCE_VARIABLES", "required scalar columns are missing or ambiguous")
      return(invisible(NULL))
    }
    n <<- n + 1
    tx$receipt$lexical_fields[[phase]] <- .bh_posterior_add(tx$receipt$lexical_fields[[phase]], Q, "lexical fields")
    if (n > tx$limits$max_source_rows || (phase == "validation" && tx$validated_rows + n > tx$limits$max_source_rows))
      .bh_posterior_abort("E_PPC_SOURCE_ROWS", "actual retained rows exceed max_source_rows")
    if (length(fields) != length(columns) || any(!grepl(number, fields, perl = TRUE)))
      .bh_posterior_abort("E_PPC_SOURCE_ROW", "malformed numeric token or inconsistent row width")
    selected <- phase == "projection" && n == selected_target
    if (phase == "validation" || selected) {
      count <- .bh_posterior_add(tx$receipt$required_numeric_conversions, length(wanted))
      if (count > tx$limits$max_scan_values)
        .bh_posterior_abort("E_PPC_SOURCE_SCAN_VALUES", "required numeric conversions exceed max_scan_values")
      tx$receipt$required_numeric_conversions <- count
      tx$receipt$numeric_conversions[[phase]] <- .bh_posterior_add(tx$receipt$numeric_conversions[[phase]], length(wanted))
      values <- suppressWarnings(as.double(fields[wanted]))
      if (any(!is.finite(values)))
        .bh_posterior_abort("E_PPC_SOURCE_NONFINITE", "a required scalar is nonfinite")
      if (!is.null(tx$diagnostic)) .bh_diagnostic_source_validate_row(tx, values)
      if (phase == "validation") for (group in tx$aliases$groups) {
        for (j in seq.int(2L, length(group))) {
          tx$receipt$alias_comparisons <- .bh_posterior_add(tx$receipt$alias_comparisons, 1)
          if (values[group[j]] != values[group[1L]])
            .bh_posterior_abort("E_INDIVIDUAL_SHARED_ALIAS", "declared shared alias columns disagree in a retained source row")
        }
      }
      if (selected) {
        out <- (selection_rank - 1) * tx$chains + chain_position
        if (length(wanted)) tx$draws[out, ] <- values
        tx$receipt$projected_rows <- tx$receipt$projected_rows + 1
        selection_rank <<- selection_rank + 1
        selected_target <<- if (selection_rank <= tx$allocation[chain_position])
          .bh_posterior_rank(selection_rank, tx$allocation[chain_position], tx$L) else Inf
      }
    }
    invisible(NULL)
  }
  repeat {
    bytes <- .bh_posterior_read(con, tx, phase, size - bytes_seen)
    if (!length(bytes)) break
    bytes_seen <- bytes_seen + length(bytes)
    ends <- which(bytes == as.raw(10))
    start <- 1L
    for (end in c(ends, length(bytes) + 1L)) {
      last <- end - 1L
      nseg <- max(0, last - start + 1L)
      # Before concatenation, string conversion, or token splitting.
      if (length(pending) + nseg > tx$limits$max_record_bytes)
        .bh_posterior_abort("E_PPC_SOURCE_RECORD", "physical record exceeds max_record_bytes before line assembly")
      if (nseg) {
        segment <- bytes[seq.int(start, last)]
        commas <- commas + sum(segment == as.raw(44))
        pending <- c(pending, segment)
      }
      if (end <= length(bytes)) {
        if (length(pending) && pending[length(pending)] == as.raw(13)) pending <- pending[-length(pending)]
        line <- rawToChar(pending)
        if (!validUTF8(line))
          .bh_posterior_abort("E_PPC_SOURCE_ROW", "CSV records must have valid UTF-8/ASCII byte encoding")
        Encoding(line) <- "UTF-8"
        consume(line, commas)
        pending <- raw(); commas <- 0; terminated <- TRUE
      } else if (nseg) terminated <- FALSE
      start <- end + 1L
    }
  }
  if (bytes_seen != size)
    .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "owned copy size changed during parsing")
  if (!terminated || length(pending) || is.null(columns) || !n ||
      !identical(footer, c("Warm-up", "Sampling", "Total")))
    .bh_posterior_abort("E_PPC_SOURCE_INCOMPLETE", "CSV needs newline-terminated rows and complete ordered sampling footer")
  if (n != ceiling(metadata$num_samples / metadata$thin))
    .bh_posterior_abort("E_PPC_SOURCE_INCOMPLETE", "actual retained rows disagree with declared sampling layout")
  if (!is.null(tx$diagnostic)) metadata$max_depth <-
    .bh_diagnostic_source_max_depth(depth_raw, depth_misplaced)
  list(metadata = metadata, columns = columns, n = n)
}

.bh_posterior_ingest_csv <- function(files, variables, posterior, states = "all",
  alias_groups = list(), expected_model = NULL, expected_chains = NULL,
  original_hashes = NULL,
  consumer_buffers = list(fixed = numeric(), per_selected_row = numeric()),
  consumer_work = list(per_selected_row = numeric(), limits = numeric()),
  chunk_bytes = 65536L, diagnostic = NULL, archive_hashes = NULL, archive_layout = FALSE) {
  tx <- new.env(parent = emptyenv())
  tx$diagnostic <- NULL
  tx$diagnostic_requested <- FALSE
  tx$receipt <- list(version = 1L, status = "acquiring", reader = list(package = "bmeas",
    function_name = "private_streaming_csv", grammar = "CmdStan 2.40.0 sample/HMC", provider_called = FALSE),
    buffer_formula = "256 + 4*chunk_bytes + 64*C + 8*(F+P+2*W) + 6*Q + 6*V + 4*A + 2*D*V + 4*D + sum(consumer_fixed) + D*sum(consumer_per_selected_row)",
    source_kind = "recorded_cmdstan_sample_hmc_csv", hash_algorithm = "MD5 of exact file bytes",
    historical_identity = "read-time receipt; original fit-time hashes absent",
    origin_authentication = "current-file hashes and supplied identities do not authenticate original sampling-time CSV, executable, or data",
    saved_row_convention = "1-based retained post-warmup CSV data row; not an original sampler iteration",
    read_accounting = "logical source-byte work; copy/parse measured and bounded owned-file hash reads reserved; not physical storage traffic",
    read_bytes_charged = 0, read_bytes_measured = 0, hash_bytes_reserved = 0,
    read_passes = list(copy_primary = 0, copy_verify = 0, hash_primary = 0, hash_verify = 0,
      validation = 0, projection = 0, hash_final = 0),
    metadata_stat_calls = 0, metadata_stat_paths = 0,
    metadata_probes = list(normalize_path_batches = 0, normalize_path_paths = 0,
      file_info_batches = 0, file_info_paths = 0, regular_file_tests = 0, readability_tests = 0),
    metadata_count_scope = "logical metadata requests through normalizePath/file.info/file_test/file.access; not OS syscall counts",
    temp_bytes_written = 0,
    required_numeric_conversions = 0, numeric_conversions = list(validation = 0, projection = 0),
    lexical_fields = list(validation = 0, projection = 0), alias_comparisons = 0, projected_rows = 0,
    memory_limit_scope = "declared explicit numeric slots; excludes strings, object overhead, invisible R/native copies and RSS",
    other_io_scope = "CSV acquisition only; retained-spec serialization/hash and pinned scientific-resource inspection are separate consumer work")
  owned <- NULL
  on.exit(suspendInterrupts({ if (!is.null(owned)) unlink(owned, recursive = TRUE, force = TRUE) }), add = TRUE)
  tryCatch({
    tx$diagnostic_requested <- !is.null(diagnostic)
    policy <- .bh_posterior_policy(posterior, variables)
    tx$limits <- policy$limits; tx$variables <- variables
    if (!is.null(diagnostic)) {
      tx$diagnostic <- .bh_diagnostic_source_profile(diagnostic, variables)
      tx$population_variables <- variables
      tx$diagnostic_columns <- NULL
      tx$diagnostic_observed_layout <- FALSE
      tx$diagnostic_sampler_columns <- character()
      tx$diagnostic_scan_default <- !"max_scan_values" %in% names(posterior)
      # Preheader finite planning includes all three optional fields. The first
      # header re-resolves this default to actual V; explicit ceilings stay fixed.
      if (tx$diagnostic_scan_default)
        tx$limits$max_scan_values <- .bh_posterior_mul(.bh_posterior_mul(2,
          tx$limits$max_source_rows), .bh_posterior_add(length(variables), 3),
          "diagnostic default scan values")
      tx$receipt$buffer_formula <- paste(tx$receipt$buffer_formula,
        "+ diagnostic metric inventory + diagnostic integration inventory + 8*diagnostic_schedule_entries")
    }
    tx$receipt$budget <- tx$limits; tx$receipt$budget_origins <- policy$origins
    tx$states <- .bh_posterior_states(states)
    if (!is.null(tx$diagnostic) && tx$states$method != "all")
      .bh_posterior_abort("E_DIAGNOSTIC_LAYOUT", "diagnostic source acquisition requires every retained state")
    tx$consumer_buffers <- .bh_posterior_buffers(consumer_buffers)
    tx$consumer_work <- .bh_posterior_work(consumer_work)
    tx$chunk_bytes <- .bh_posterior_int(chunk_bytes, "chunk_bytes", 1, 65536)
    if (!.bh_posterior_plain(files, "character") || !length(files) || anyNA(files) ||
        any(!nzchar(files)) || base::anyDuplicated.default(files))
      .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "supply distinct plain recorded CSV paths")
    .bh_posterior_encoding(files)
    .bh_posterior_optional_names(files)
    tx$chains <- length(files)
    if (tx$chains > tx$limits$max_chains)
      .bh_posterior_abort("E_PPC_SOURCE_CHAINS", "source chain count exceeds max_chains")
    if (!is.null(expected_chains) && tx$chains != .bh_posterior_int(expected_chains, "expected_chains", 1, .Machine$integer.max))
      .bh_posterior_abort("E_PPC_SOURCE_CHAINS", "recorded file count differs from expected chain count")
    tx$aliases <- list(slots = 0, groups = list(), redundant = 0)
    # Admit minimum unavoidable buffers before alias-map allocations or I/O.
    .bh_posterior_envelope(tx, 0, if (tx$states$method == "all") tx$chains else tx$states$draws)
    if (!.bh_posterior_plain(alias_groups, "list"))
      .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "alias_groups must be a plain list")
    alias_slots <- 0
    for (group in alias_groups) {
      if (!.bh_posterior_plain(group, c("integer", "double")))
        .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "alias groups must be plain numeric index vectors")
      alias_slots <- .bh_posterior_add(alias_slots, length(group))
    }
    tx$aliases$slots <- alias_slots
    .bh_posterior_envelope(tx, 0, if (tx$states$method == "all") tx$chains else tx$states$draws)
    tx$aliases <- .bh_posterior_aliases(alias_groups, length(variables))
    .bh_posterior_envelope(tx, 0, if (tx$states$method == "all") tx$chains else tx$states$draws)
    if (!is.null(expected_model) && (!.bh_posterior_plain(expected_model, "character") ||
        length(expected_model) != 1L || is.na(expected_model) || !nzchar(expected_model)))
      .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "expected_model must be one plain saved model name")
    if (!is.null(expected_model)) .bh_posterior_encoding(expected_model)
    tx$receipt$original_fit_hashes_present <- !is.null(original_hashes)
    if (tx$receipt$original_fit_hashes_present)
      tx$receipt$historical_identity <- "read-time receipt; original fit-time hashes supplied but unverified"
    supplied <- original_hashes
    if (!is.null(supplied)) {
      if (!.bh_posterior_plain(supplied, c("list", "character")))
        .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "original_hashes must be plain MD5 metadata")
      if (typeof(supplied) == "list") {
        .bh_posterior_names(supplied, c("algorithm", "hashes"), c("algorithm", "hashes"))
        if (!identical(supplied$algorithm, "md5")) .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "only MD5 original hashes are supported")
        supplied <- supplied$hashes
      }
      if (!.bh_posterior_plain(supplied, "character") || length(supplied) != tx$chains || anyNA(supplied))
        .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "one plain original MD5 is required per recorded path")
      .bh_posterior_encoding(supplied)
      if (any(!grepl("^[0-9A-Fa-f]{32}$", supplied)))
        .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "one plain original MD5 is required per recorded path")
      if (!is.null(names(supplied))) {
        .bh_posterior_names(supplied, files, files)
        supplied <- supplied[match(files, names(supplied))]
      }
      supplied <- unname(tolower(supplied))
      tx$receipt$historical_identity <- "read-time receipt; supplied original fit-time hashes pending comparison"
    }
    if (!is.null(archive_hashes) && (!is.character(archive_hashes) ||
        !is.null(attributes(archive_hashes)) || length(archive_hashes) != tx$chains ||
        anyNA(archive_hashes) || any(!grepl("^[a-f0-9]{32}$", archive_hashes))))
      .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "archive_hashes requires one archive-time MD5 per source")
    tx$receipt$recorded_paths <- unname(files); tx$receipt$variables <- variables
    tx$receipt$expected_model <- expected_model
    tx$receipt$metadata_probes$normalize_path_batches <- 1
    tx$receipt$metadata_probes$normalize_path_paths <- length(files)
    paths <- normalizePath(files, winslash = "/", mustWork = TRUE)
    if (base::anyDuplicated.default(paths)) .bh_posterior_abort("E_PPC_SOURCE_ARGUMENT", "recorded paths resolve to duplicate sources")
    before <- .bh_posterior_stat(paths, tx)
    sizes <- as.double(before$size)
    B <- .bh_posterior_sum(sizes, "aggregate source bytes")
    tx$receipt$source_bytes <- B
    tx$receipt$read_bytes_preflight <- .bh_posterior_mul(7, B, "seven-pass reads")
    tx$receipt$temp_bytes_preflight <- .bh_posterior_mul(2, B, "owned snapshots")
    if (B > tx$limits$max_source_bytes) .bh_posterior_abort("E_PPC_SOURCE_BYTES", "source bytes exceed max_source_bytes")
    if (tx$receipt$read_bytes_preflight > tx$limits$max_read_bytes)
      .bh_posterior_abort("E_PPC_SOURCE_READ_BYTES", "seven-pass acquisition exceeds max_read_bytes")
    if (tx$receipt$temp_bytes_preflight > tx$limits$max_temp_bytes)
      .bh_posterior_abort("E_PPC_SOURCE_TEMP_BYTES", "two owned snapshots exceed max_temp_bytes")
    suspendInterrupts({
      candidate <- tempfile("bmeas-streaming-")
      if (!dir.create(candidate, mode = "0700", showWarnings = FALSE))
        .bh_posterior_abort("E_PPC_SOURCE_IO", "could not create owned snapshot directory")
      # Ownership starts only after successful creation. A collision is never
      # ours to clean, and a real interrupt cannot split creation/registration.
      owned <- candidate
    })
    primary <- file.path(owned, paste0("primary", seq_along(files), ".csv"))
    verify <- file.path(owned, paste0("verify", seq_along(files), ".csv"))
    hashes <- current <- character(tx$chains)
    for (i in seq_along(files)) .bh_posterior_copy(paths[i], primary[i], sizes[i], tx, "copy_primary")
    for (i in seq_along(files)) hashes[i] <- .bh_posterior_hash(primary[i], sizes[i], tx, "hash_primary")
    for (i in seq_along(files)) .bh_posterior_copy(paths[i], verify[i], sizes[i], tx, "copy_verify")
    for (i in seq_along(files)) current[i] <- .bh_posterior_hash(verify[i], sizes[i], tx, "hash_verify")
    after <- .bh_posterior_stat(paths, tx)
    tx$receipt$sources <- if (is.null(tx$diagnostic))
      data.frame(recorded_path = unname(files), source_bytes = sizes,
        copy_md5 = hashes, current_source_md5 = current, stringsAsFactors = FALSE) else
      .bh_diagnostic_source_frame(list(recorded_path = unname(files), source_bytes = sizes,
        copy_md5 = hashes, current_source_md5 = current), tx$chains)
    tx$receipt$bounded_copy_passes <- 2L
    if (!identical(before, after) || !identical(hashes, current))
      .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "recorded source changed during snapshot acquisition")
    if (!is.null(supplied)) {
      if (is.null(tx$diagnostic)) tx$receipt$sources$original_fit_md5 <- supplied else
        tx$receipt$sources <- .bh_diagnostic_source_frame_add(tx$receipt$sources, "original_fit_md5", supplied)
      if (!identical(supplied, hashes)) {
        tx$receipt$historical_identity <- "current bytes differ from supplied original fit-time MD5 hashes"
        .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "current bytes differ from supplied original fit-time MD5 hashes")
      }
      tx$receipt$historical_identity <- "current bytes match supplied original fit-time MD5 hashes"
    }
    if (!is.null(archive_hashes)) {
      if (!identical(archive_hashes, hashes))
        .bh_posterior_abort("E_ARCHIVE_CHANGED", "owned posterior snapshot differs from archive-time bytes")
      tx$receipt$archive_integrity <- list(scope="archive-time bytes; not sampler-time authentication",
        algorithm="md5", expected=archive_hashes, status="matched_owned_snapshot")
    }
    manifests <- vector("list", tx$chains); tx$validated_rows <- 0
    for (i in seq_along(files)) {
      manifests[[i]] <- .bh_posterior_scan(primary[i], sizes[i], tx, "validation")
      tx$validated_rows <- .bh_posterior_add(tx$validated_rows, manifests[[i]]$n)
    }
    ids <- vapply(manifests, function(x) x$metadata$id, 0)
    counts <- vapply(manifests, function(x) x$n, 0)
    comparable <- c("model", "method", "algorithm", "engine", "metric", "num_samples", "thin",
      "save_warmup", "num_warmup", "stan_version_major", "stan_version_minor", "stan_version_patch")
    if (base::anyDuplicated.default(ids) || length(base::unique.default(counts)) != 1L)
      .bh_posterior_abort("E_PPC_SOURCE_CHAINS", "chains require distinct numeric IDs and equal positive actual row counts")
    for (i in seq_along(manifests)) if (!identical(manifests[[1]]$columns, manifests[[i]]$columns) ||
        !identical(manifests[[1]]$metadata[comparable], manifests[[i]]$metadata[comparable]))
      .bh_posterior_abort("E_PPC_SOURCE_CHAINS", "chain headers describe inconsistent sampling layouts")
    model <- manifests[[1]]$metadata$model
    if (!is.null(expected_model) && !identical(model, expected_model))
      .bh_posterior_abort("E_PPC_SOURCE_IDENTITY", "CSV model name differs from saved expected identity")
    ord <- order(ids); ids <- ids[ord]
    if (!is.null(tx$diagnostic)) tx$diagnostic_observed_layout <- TRUE
    layout <- .bh_posterior_layout(tx, counts[1], length(manifests[[1]]$columns))
    tx$L <- layout$L; D <- layout$D
    tx$allocation <- rep(floor(D / tx$chains), tx$chains)
    i <- 1
    while (i <= D %% tx$chains) { tx$allocation[i] <- tx$allocation[i] + 1; i <- i + 1 }
    chain_key <- rank_key <- integer(D)
    pos <- 0; rank <- 1
    while (rank <= max(tx$allocation)) {
      i <- 1
      while (i <= tx$chains) {
        if (rank <= tx$allocation[i]) {
          pos <- pos + 1
          chain_key[pos] <- as.integer(ids[i])
          rank_key[pos] <- as.integer(.bh_posterior_rank(rank, tx$allocation[i], tx$L))
        }
        i <- i + 1
      }
      rank <- rank + 1
    }
    keys <- if (is.null(tx$diagnostic)) data.frame(chain_id = chain_key, saved_row = rank_key) else
      .bh_diagnostic_source_frame(list(chain_id = chain_key, saved_row = rank_key), D)
    tx$draws <- matrix(0, nrow = D, ncol = length(tx$variables), dimnames = list(NULL, tx$variables))
    for (i in seq_along(ord)) {
      j <- ord[i]
      check <- .bh_posterior_scan(primary[j], sizes[j], tx, "projection", i)
      if (!identical(check, manifests[[j]]))
        .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "owned copy structure changed between validation and projection")
    }
    for (i in seq_along(files)) if (!identical(.bh_posterior_hash(primary[i], sizes[i], tx, "hash_final"), hashes[i]))
      .bh_posterior_abort("E_PPC_SOURCE_CHANGED", "owned copy bytes changed during extraction")
    if (tx$receipt$projected_rows != D)
      .bh_posterior_abort("E_PPC_SOURCE_INCOMPLETE", "projection did not recover every selected key")
    if (is.null(tx$diagnostic)) {
      tx$receipt$sources <- tx$receipt$sources[ord, , drop = FALSE]
      rownames(tx$receipt$sources) <- NULL
      tx$receipt$sources$chain_id <- ids
      tx$receipt$sources$retained_rows <- counts[ord]
      tx$receipt$sources$sig_figs <- vapply(manifests[ord], function(x)
        if (is.null(x$metadata$sig_figs)) NA_real_ else x$metadata$sig_figs, 0)
    } else {
      source_columns <- unclass(.bh_diagnostic_source_frame_rows(tx$receipt$sources, ord))
      source_columns$chain_id <- ids
      source_columns$retained_rows <- counts[ord]
      source_columns$sig_figs <- vapply(manifests[ord], function(x)
        if (is.null(x$metadata$sig_figs)) NA_real_ else x$metadata$sig_figs, 0)
      tx$receipt$sources <- .bh_diagnostic_source_frame(source_columns, tx$chains)
    }
    tx$receipt$model_name <- model
    tx$receipt$all_row_shape <- c(iterations = tx$L, chains = tx$chains, variables = length(tx$variables))
    tx$receipt$actual_posterior_rows <- tx$validated_rows
    tx$receipt$selected_shape <- c(draws = D, variables = length(tx$variables))
    tx$receipt$source_rows <- tx$validated_rows
    tx$receipt$selected_rows <- D
    tx$receipt$selected_required_values <- .bh_posterior_mul(D, length(tx$variables))
    tx$receipt$selection_method <- if (tx$states$method == "all")
      "all retained rows; saved-row rank then ascending numeric chain ID" else
      "balanced allocation by ascending numeric chain ID; evenly spaced retained ranks; interleaved selection rank"
    tx$receipt$selection_scope <- list(method = tx$states$method, full_source_rows = tx$validated_rows,
      selected_rows = D, omitted_rows = tx$validated_rows - D,
      per_chain = if (is.null(tx$diagnostic)) data.frame(chain_id = ids, source_rows = counts[ord],
        selected_rows = tx$allocation, omitted_rows = counts[ord] - tx$allocation) else
        .bh_diagnostic_source_frame(list(chain_id = ids, source_rows = counts[ord],
          selected_rows = tx$allocation, omitted_rows = counts[ord] - tx$allocation), tx$chains),
      guarantee = if (tx$states$method == "all") "complete validated empirical source" else
        "selected joint-state empirical set; no representativeness, quantile-error, or inference guarantee")
    tx$receipt$selection <- list(method = tx$states$method, allocations = tx$receipt$selection_scope$per_chain)
    tx$receipt$alias_validation <- list(scope = "all retained source rows", redundant_columns = tx$aliases$redundant,
      comparisons = tx$receipt$alias_comparisons)
    tx$receipt$raw_buffer_bounds <- c(chunk_bytes = tx$chunk_bytes, record_bytes = tx$limits$max_record_bytes,
      conservative_raw_overlap_bytes = 2 * tx$limits$max_record_bytes + 3 * tx$chunk_bytes)
    tx$receipt$status <- "acquired"
    result <- list(draws = tx$draws, draw_keys = keys, receipt = tx$receipt)
    if (isTRUE(archive_layout)) result$archive_layout <- lapply(manifests,
      function(x) list(columns=x$columns, metadata=x$metadata, retained_rows=x$n))
    if (!is.null(tx$diagnostic)) {
      chain_metadata <- vector("list", tx$chains)
      for (i in seq_along(ord)) {
        metadata <- manifests[[ord[[i]]]]$metadata
        chain_metadata[[i]] <- list(id = metadata$id, thin = metadata$thin,
          engine = metadata$engine, max_depth = metadata$max_depth,
          retained_rows = counts[[ord[[i]]]])
      }
      result$diagnostic <- list(dimensions = tx$diagnostic_dimensions,
        chain_metadata = chain_metadata, sampler_columns = tx$diagnostic_sampler_columns,
        parameter_columns = tx$population_variables)
    }
    result
  }, error = function(e) {
    tx$receipt$status <- "failed"
    if (tx$diagnostic_requested && inherits(e, "measurement_error")) {
      # Keep typed diagnostic dimension/budget failures and the bounded receipt.
      stop(.bh_diagnostic_source_condition(e, tx$receipt))
    }
    if (inherits(e, "bh_ppc_source_error")) { e$receipt <- tx$receipt; stop(e) }
    message <- if (!tx$diagnostic_requested) conditionMessage(e) else
      .bh_diagnostic_metric_condition_text(e)
    .bh_posterior_abort("E_PPC_SOURCE_IO", message, tx$receipt)
  }, interrupt = function(e) {
    if (tx$diagnostic_requested) {
      tx$receipt$status <- "interrupted"
      e <- .bh_diagnostic_source_condition(e, tx$receipt)
    }
    stop(e)
  })
}
