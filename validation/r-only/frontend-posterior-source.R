source(file.path("helpers", "native-guard.R"))
# Private streaming parser qualification only. Synthetic fixtures are engineering
# inputs, never posterior/scientific validation. No provider, fitting, or compile.
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if (nzchar(root)) {
  ns <- new.env(parent = globalenv())
  sys.source(file.path(root, "R", "frontend-posterior-source.R"), envir = ns)
} else {
  package_ns <- asNamespace("bmeas")
  ns <- new.env(parent = package_ns)
  # Use installed helper bodies verbatim. Rebind only their environments so
  # test-only mutation controls remain local and the package namespace is intact.
  helper_names <- ls(package_ns, all.names = TRUE, pattern = "^\\.bh_posterior_")
  if (!length(helper_names)) stop("installed bmeas lacks the streaming source helpers")
  for (helper_name in helper_names) {
    helper <- get(helper_name, package_ns, inherits = FALSE)
    if (!is.function(helper)) stop("streaming helper binding is not a function")
    environment(helper) <- ns
    assign(helper_name, helper, envir = ns)
  }
}
get_private <- function(n) get(n, ns, inherits = FALSE)
ingest <- get_private(".bh_posterior_ingest_csv")
policy <- get_private(".bh_posterior_policy")
evidence <- Sys.getenv("BMEAS_STREAMING_EVIDENCE_DIR")
if (nzchar(evidence)) dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
fixtures <- tempfile("bmeas-streaming-fixtures-")
dir.create(fixtures)
passed <- 0L; cases <- list()
ok <- function(x, label) {
  if (!isTRUE(x)) stop(label, call. = FALSE)
  passed <<- passed + 1L; cat("PASS", label, "\n")
}
header <- function(id, L = 33, Q = 4) c(
  "# ENGINEERING FIXTURE: invented numbers, not posterior validation",
  "# stan_version_major = 2", "# stan_version_minor = 40", "# stan_version_patch = 0",
  "# model = synthetic_streaming_model", paste("# id =", id),
  "# method = sample (Default)", paste("# num_samples =", L), "# num_warmup = 0",
  "# save_warmup = false (Default)", "# thin = 1 (Default)",
  "# algorithm = hmc (Default)", "# engine = nuts (Default)",
  "# metric = diag_e (Default)", "# sig_figs = 18",
  paste(c("lp__", "eta.1.1", "eta.1.2", if (Q == 4) "eta.1.3" else paste0("z.", seq_len(Q - 3))), collapse = ","))
footer <- c("# Elapsed Time: 0 seconds (Warm-up)", "# 0.01 seconds (Sampling)", "# 0.01 seconds (Total)")
fixture <- function(id, L = 33, label = paste0("chain", id), Q = 4, crlf = FALSE) {
  p <- file.path(fixtures, paste0(label, ".csv"))
  con <- file(p, "wb"); on.exit(close(con))
  emit <- function(lines) writeChar(paste0(paste(lines, collapse = if (crlf) "\r\n" else "\n"),
    if (crlf) "\r\n" else "\n"), con, eos = NULL, useBytes = TRUE)
  emit(header(id, L, Q))
  # At most one wide text record is resident, never a wide numeric data table.
  tail <- if (Q == 4) NULL else paste(rep("123456789.125", Q - 3), collapse = ",")
  for (r in seq_len(L)) {
    value <- id * 1000 + r
    emit(if (Q == 4) paste("-Inf", value, -value, -value, sep = ",") else
      paste("NaN", value, -value, tail, sep = ","))
  }
  emit(footer)
  p
}
vars <- c("eta[1,2]", "eta[1,1]")
paths <- vapply(c(19, 3, 11, 7), fixture, "")
base_policy <- list(reader = "streaming", max_source_bytes = 16 * 1024^2,
  max_read_bytes = 112 * 1024^2, max_materialized_values = 1000000)
owned <- function() setdiff(sort(list.files(tempdir(), pattern = "^bmeas-streaming-", full.names = TRUE)), fixtures)
state <- function() list(cwd = getwd(), kind = RNGkind(),
  seed = if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL,
  owned = owned())
invoke <- function(files = paths, variables = vars, posterior = base_policy,
                   label, code = NULL, ...) {
  before <- state()
  result <- tryCatch(ingest(files, variables, posterior, ...), error = identity, interrupt = identity)
  ok(identical(before, state()), paste(label, "cleans owned files and preserves process/RNG state"))
  if (is.null(code)) {
    if (inherits(result, "condition")) stop(conditionMessage(result))
    ok(identical(result$receipt$status, "acquired"), paste(label, "acquired"))
  } else {
    ok(inherits(result, "bh_ppc_source_error") && identical(result$code, code), paste(label, code))
    ok(identical(result$receipt$status, "failed"), paste(label, "partial failed receipt"))
  }
  cases[[label]] <<- if (inherits(result, "condition")) list(code = result$code, receipt = result$receipt) else result$receipt
  result
}
change <- function(source, label, fn) {
  p <- file.path(fixtures, paste0(label, ".csv"))
  writeLines(fn(readLines(source)), p, useBytes = TRUE); p
}
run <- function() {
  p <- policy(base_policy, vars)
  ok(identical(p$limits$max_scan_values, 4000000) && p$limits$max_temp_bytes == 32 * 1024^2,
    "pure policy resolves finite arithmetic defaults")
  ok(p$origins[["max_read_bytes"]] == "explicit" && p$origins[["max_columns"]] == "finite_default",
    "resolved quota origins distinguished")
  a <- invoke(label = "four-chain all 132 states")
  ids <- sort(c(19L, 3L, 11L, 7L))
  expected_keys <- data.frame(chain_id = rep(ids, 33), saved_row = rep(1:33, each = 4))
  v <- expected_keys$chain_id * 1000 + expected_keys$saved_row
  expected <- cbind(-v, v); colnames(expected) <- vars; storage.mode(expected) <- "double"
  ok(identical(a$draw_keys, expected_keys) && identical(a$draws, expected), "all values and keys equal independent oracle")
  B <- sum(file.info(paths)$size)
  ok(a$receipt$read_bytes_charged == 7 * B && a$receipt$read_bytes_measured == 4 * B &&
      a$receipt$hash_bytes_reserved == 3 * B && all(unlist(a$receipt$read_passes) == B) &&
      a$receipt$temp_bytes_written == 2 * B, "exact seven-pass accounting and two owned copies")
  ok(a$receipt$required_numeric_conversions == 2 * 132 * 2 &&
      all(unlist(a$receipt$lexical_fields) == 132 * 4), "numeric conversions distinct from full-width lexical work")
  s <- invoke(label = "balanced nine", states = list(method = "balanced", draws = 9))
  expected_subset <- data.frame(chain_id = c(ids, ids, ids[1]), saved_row = c(rep(1L, 4), 17L, 33L, 33L, 33L, 33L))
  ok(identical(s$draw_keys, expected_subset) && s$receipt$required_numeric_conversions == (132 + 9) * 2,
    "balanced allocation and endpoint keys equal independent oracle")
  ok(s$receipt$selection_scope$omitted_rows == 123 &&
      identical(s$receipt$selection$allocations$selected_rows, c(3, 2, 2, 2)), "honest subset source and omission counts")
  ok(s$receipt$source_rows == 132 && s$receipt$selected_rows == 9 && s$receipt$selected_required_values == 18 &&
      identical(s$receipt$selection$method, "balanced") && is.null(s$receipt$materialized_required_values),
    "stable source and selected receipt units")
  for (chunk in c(1L, 7L, 127L, 65536L)) {
    z <- invoke(files = rev(paths), label = paste("path order chunk", chunk), chunk_bytes = chunk,
      states = list(method = "balanced", draws = 9))
    ok(identical(s$draw_keys, z$draw_keys) && identical(s$draws, z$draws), paste("exact path/chunk invariance", chunk))
  }
  cr <- fixture(3, label = "CRLF", crlf = TRUE)
  z <- invoke(files = cr, label = "CRLF tiny chunk", chunk_bytes = 3)
  ok(nrow(z$draws) == 33, "CRLF splitting preserves records")
  z <- invoke(variables = character(), label = "zero-column keys", states = list(method = "balanced", draws = 9))
  ok(identical(z$draw_keys, s$draw_keys) && identical(dim(z$draws), c(9L, 0L)) &&
      z$receipt$required_numeric_conversions == 0 && z$receipt$read_bytes_charged == 7 * B,
    "V zero still validates seven passes and budgets keys")
  aliases <- c("eta[1,2]", "eta[1,3]")
  z <- invoke(variables = aliases, alias_groups = list(c(1L, 2L)), label = "all-row shared aliases",
    states = list(method = "balanced", draws = 4))
  ok(z$receipt$alias_comparisons == 132 && z$receipt$alias_validation$redundant_columns == 1,
    "shared comparisons cover every retained row")
  bad <- change(paths[1], "late-alias", function(x) { i <- which(!startsWith(x, "#"))[32]; x[i] <- sub(",-19031$", ",-1", x[i]); x })
  invoke(files = c(bad, paths[-1]), variables = aliases, alias_groups = list(c(1, 2)),
    states = list(method = "balanced", draws = 4), label = "unselected alias disagreement", code = "E_INDIVIDUAL_SHARED_ALIAS")
  bad <- change(paths[1], "late-nonfinite", function(x) { i <- which(!startsWith(x, "#"))[32]; x[i] <- sub(",19031,", ",Inf,", x[i]); x })
  invoke(files = c(bad, paths[-1]), states = list(method = "balanced", draws = 4),
    label = "unselected required nonfinite", code = "E_PPC_SOURCE_NONFINITE")
  for (what in c("model", "header", "count", "id")) {
    bad <- change(paths[3], paste0("third-", what), function(x) {
      if (what == "model") x <- sub("synthetic_streaming_model", "different_model", x)
      if (what == "header") x <- sub("eta.1.3", "other.1.3", x, fixed = TRUE)
      if (what == "id") x <- sub("# id = 11", "# id = 3", x, fixed = TRUE)
      if (what == "count") { x <- sub("# num_samples = 33", "# num_samples = 32", x, fixed = TRUE); x <- x[-which(!startsWith(x, "#"))[34]] }
      x
    })
    invoke(files = c(paths[1:2], bad, paths[4]), label = paste("third-chain", what), code = "E_PPC_SOURCE_CHAINS")
  }
  for (value in c("true", "1", "2", "FALSE", "0.0")) {
    bad <- change(paths[1], paste0("warmup-", value), function(x) sub("false (Default)", value, x, fixed = TRUE))
    invoke(files = bad, label = paste("warmup", value), code = if (value %in% c("true", "1")) "E_PPC_SOURCE_WARMUP" else "E_PPC_SOURCE_HEADER")
  }
  for (value in c("0", "false")) {
    good <- change(paths[1], paste0("warmup-good-", value), function(x) sub("false (Default)", value, x, fixed = TRUE))
    invoke(files = good, label = paste("accepted warmup", value))
  }
  corruptions <- list(
    duplicate_metadata = list("E_PPC_SOURCE_HEADER", function(x) append(x, "# id = 19", after = 6)),
    missing_metadata = list("E_PPC_SOURCE_HEADER", function(x) x[!grepl("# thin", x, fixed = TRUE)]),
    duplicate_header = list("E_PPC_SOURCE_HEADER", function(x) sub("eta.1.3", "eta.1.2", x, fixed = TRUE)),
    data_after_footer = list("E_PPC_SOURCE_INCOMPLETE", function(x) c(x, "1,2,3,4")),
    missing_footer = list("E_PPC_SOURCE_INCOMPLETE", function(x) head(x, -1)),
    wrong_footer = list("E_PPC_SOURCE_INCOMPLETE", function(x) c(head(x, -3), footer[c(2,1,3)])),
    malformed_numeric = list("E_PPC_SOURCE_ROW", function(x) { x[18] <- "1,12x,3,4"; x }),
    wrong_width = list("E_PPC_SOURCE_ROW", function(x) { x[18] <- "1,2,3"; x }),
    unsupported_version = list("E_PPC_SOURCE_READER_VERSION", function(x) sub("minor = 40", "minor = 39", x, fixed = TRUE)),
    wrong_method = list("E_PPC_SOURCE_METHOD", function(x) sub("method = sample", "method = optimize", x, fixed = TRUE)))
  for (name in names(corruptions)) {
    item <- corruptions[[name]]; bad <- change(paths[1], name, item[[2]])
    invoke(files = bad, label = name, code = item[[1]])
  }
  missing_newline <- file.path(fixtures, "missing-newline.csv")
  writeChar(paste(readLines(paths[1]), collapse = "\n"), missing_newline, eos = NULL, useBytes = TRUE)
  invoke(files = missing_newline, label = "missing final newline", code = "E_PPC_SOURCE_INCOMPLETE")
  invoke(files = c(paths[1], paths[1]), label = "duplicate paths", code = "E_PPC_SOURCE_ARGUMENT")
  symlink <- file.path(fixtures, "symlink.csv")
  stopifnot(file.symlink(paths[1], symlink))
  invoke(files = c(paths[1], symlink), label = "resolved duplicate paths", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(label = "C greater than D", states = list(method = "balanced", draws = 3), code = "E_PPC_SOURCE_SELECTION")
  invoke(label = "expected chains", expected_chains = 3, code = "E_PPC_SOURCE_CHAINS")
  invoke(label = "expected model", expected_model = "other", code = "E_PPC_SOURCE_IDENTITY")
  hash <- unname(tools::md5sum(paths))
  invoke(label = "supplied historical hashes", original_hashes = list(algorithm = "md5", hashes = hash))
  invoke(label = "historical hash mismatch", original_hashes = rep(strrep("0",32), 4), code = "E_PPC_SOURCE_CHANGED")
  limits <- list(max_source_bytes = B - 1, max_read_bytes = 7 * B - 1, max_temp_bytes = 2 * B - 1,
    max_columns = 3, max_source_rows = 131, max_chains = 3, max_scan_values = 527,
    max_record_bytes = 10, max_materialized_values = 1)
  codes <- c(max_source_bytes = "BYTES", max_read_bytes = "READ_BYTES", max_temp_bytes = "TEMP_BYTES",
    max_columns = "COLUMNS", max_source_rows = "ROWS", max_chains = "CHAINS", max_scan_values = "SCAN_VALUES",
    max_record_bytes = "RECORD", max_materialized_values = "VALUES")
  for (key in names(limits)) {
    p <- base_policy; p[[key]] <- limits[[key]]
    z <- invoke(posterior = p, label = paste("quota", key), code = paste0("E_PPC_SOURCE_", codes[[key]]))
    if (key %in% c("max_source_bytes", "max_read_bytes", "max_temp_bytes", "max_chains", "max_materialized_values"))
      ok(z$receipt$read_bytes_charged == 0, paste(key, "rejects before acquisition reads"))
  }
  p <- base_policy; p$max_materialized_values <- s$receipt$max_materialized_values - 1
  z <- invoke(posterior = p, states = list(method = "balanced", draws = 9), label = "selected envelope one under", code = "E_PPC_SOURCE_VALUES")
  ok(z$receipt$projected_rows == 0, "selected envelope refused before projection allocation")
  z <- invoke(states = list(method = "balanced", draws = 9), label = "consumer work admitted",
    consumer_work = list(per_selected_row = c(transformation_terms = 2), limits = c(transformation_terms = 18)))
  ok(z$receipt$consumer_work$admitted[["transformation_terms"]] == 18, "selected work receipt")
  z <- invoke(states = list(method = "balanced", draws = 9), label = "consumer work refused",
    consumer_work = list(per_selected_row = c(transformation_terms = 2), limits = c(transformation_terms = 17)),
    code = "E_PPC_SOURCE_CONSUMER_WORK")
  ok(z$receipt$projected_rows == 0, "consumer work refusal precedes projection")
  for (bad in list(NA_real_, Inf, 1.5, structure(1, class = "evil"), matrix(1), -1)) {
    p <- base_policy; p$max_read_bytes <- bad
    invoke(posterior = p, label = paste("malformed quota", length(cases)), code = "E_PPC_SOURCE_ARGUMENT")
  }
  p <- base_policy; p$unknown <- 1
  invoke(posterior = p, label = "unknown policy field", code = "E_PPC_SOURCE_ARGUMENT")
  p <- c(base_policy, base_policy[2])
  invoke(posterior = p, label = "duplicate policy field", code = "E_PPC_SOURCE_ARGUMENT")
  invoked <- 0
  length.evil <- function(x) { invoked <<- invoked + 1; stop("dispatched") }
  Ops.evil <- function(...) { invoked <<- invoked + 1; stop("dispatched") }
  assign("length.evil", length.evil, .GlobalEnv); assign("Ops.evil", Ops.evil, .GlobalEnv)
  on.exit(rm(list = c("length.evil", "Ops.evil"), envir = .GlobalEnv), add = TRUE)
  invoke(files = structure(paths, class = "evil"), label = "classed file paths", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(variables = structure(vars, class = "evil"), label = "classed variables", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(alias_groups = structure(list(1:2), class = "evil"), label = "classed alias container", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(consumer_buffers = list(fixed = structure(1, class = "evil"), per_selected_row = numeric()),
    label = "classed consumer coefficient", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(alias_groups = list(c(1, NA)), label = "missing alias index", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(alias_groups = list(c(1, 1)), label = "duplicate alias index", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(alias_groups = list(c(1, 2), c(1, 2)), label = "overlapping alias groups", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(consumer_work = list(per_selected_row = c(a = 1), limits = c(b = 1)),
    label = "consumer work key mismatch", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(consumer_buffers = list(fixed = c(a = 0, a = 0), per_selected_row = numeric()),
    label = "duplicate consumer slot names", code = "E_PPC_SOURCE_ARGUMENT")
  invoke(posterior = structure(base_policy, names = c("reader", NA, "max_read_bytes", "max_materialized_values")),
    label = "missing policy field name", code = "E_PPC_SOURCE_ARGUMENT")
  ok(invoked == 0, "hostile methods never dispatched")
  cap <- tryCatch(get_private(".bh_posterior_mul")(2^32, 2^32), error = identity)
  ok(identical(cap$code, "E_PPC_SOURCE_CAPABILITY"), "exact-count product overflow refuses")
  cap <- tryCatch(get_private(".bh_posterior_rank")(2147483647, 2147483647, 2147483647), error = identity)
  ok(identical(cap$code, "E_PPC_SOURCE_CAPABILITY"), "endpoint numerator overflow refuses")
  # Test-only rebinding exercises source mutation, owned mutation, and interrupt
  # without introducing callbacks/options into the production boundary.
  original_hash <- get_private(".bh_posterior_hash")
  mutate_source <- fixture(23, label = "source-mutates")
  assign(".bh_posterior_hash", function(path, size, tx, phase) {
    ans <- original_hash(path, size, tx, phase)
    if (phase == "hash_primary") cat("# changed\n", file = mutate_source, append = TRUE)
    ans
  }, ns)
  invoke(files = mutate_source, label = "source changes between copies", code = "E_PPC_SOURCE_CHANGED")
  assign(".bh_posterior_hash", original_hash, ns)
  original_scan <- get_private(".bh_posterior_scan")
  assign(".bh_posterior_scan", function(path, size, tx, phase, chain_position = NULL) {
    ans <- original_scan(path, size, tx, phase, chain_position)
    if (phase == "projection") {
      con <- file(path, "r+b"); seek(con, 1, "start", rw = "write"); writeBin(charToRaw("!"), con); close(con)
    }
    ans
  }, ns)
  invoke(files = paths[1], label = "owned copy changes after projection", code = "E_PPC_SOURCE_CHANGED")
  assign(".bh_posterior_scan", function(...) stop(structure(list(message = "synthetic interrupt", call = NULL),
    class = c("interrupt", "condition"))), ns)
  before <- state(); z <- tryCatch(ingest(paths, vars, base_policy), interrupt = identity)
  ok(inherits(z, "interrupt") && identical(before, state()), "interrupt propagates and owned copies clean")
  assign(".bh_posterior_scan", original_scan, ns)
  one <- fixture(5, L = 1, label = "one-row")
  z <- invoke(files = one, label = "generic one-row source")
  ok(nrow(z$draws) == 1, "consumer-neutral one-row acquisition")
  # Single wide fixture: <=16 MiB sources, <=32 MiB copies, <=112 MiB charged reads.
  wide <- vapply(c(19, 3, 11, 7), function(id) fixture(id, 125, paste0("wide",id), Q = 2000), "")
  BW <- sum(file.info(wide)$size)
  ok(BW > 10 * 1024^2 && BW <= 16 * 1024^2, "wide fixture within predeclared aggregate envelope")
  z <- invoke(files = wide, label = "wide four-chain narrow projection", states = list(method = "balanced", draws = 9))
  ok(identical(dim(z$draws), c(9L, 2L)) && z$receipt$actual_posterior_rows == 500 &&
      z$receipt$required_numeric_conversions == 1018 && z$receipt$read_bytes_charged == 7 * BW &&
      z$receipt$read_bytes_charged <= 112 * 1024^2 && z$receipt$temp_bytes_written <= 32 * 1024^2,
    "wide acquisition reports exact narrow materialization and bounded work")
  expected <- z$draw_keys$chain_id * 1000 + z$draw_keys$saved_row
  ok(identical(unname(z$draws[,1]), -as.double(expected)) && identical(unname(z$draws[,2]), as.double(expected)),
    "wide selected values equal independent algebraic oracle")
  real <- Sys.getenv("BMEAS_STREAMING_REAL_CSV")
  if (nzchar(real)) {
    ok(file.info(real)$size == 11962, "retained real CSV exact authorized size")
    before <- tools::md5sum(real)
    z <- invoke(files = real, variables = c("eta[1,1]", "obs_sd[1]"), label = "retained real producer equality",
      expected_model = "scalar_components_model")
    raw <- read.csv(real, comment.char = "#", check.names = FALSE)
    expected <- as.matrix(raw[, c("eta.1.1", "obs_sd.1")]); colnames(expected) <- c("eta[1,1]", "obs_sd[1]")
    rownames(expected) <- NULL
    ok(identical(z$draws, expected) && identical(before, tools::md5sum(real)), "retained real values equal independent base reader and bytes unchanged")
  }
  if (nzchar(evidence)) saveRDS(list(status = "passed", assertions = passed, cases = cases,
    envelope = list(wide_source_bytes = BW, wide_charged_read_bytes = 7 * BW,
      wide_copy_bytes = 2 * BW), r_version = R.version.string), file.path(evidence, "results.rds"))
  cat("SUCCESS", passed, "assertions\n")
}
# Narrow repair controls can run independently; the established wide/selection
# body above remains byte-identical to the first qualified parser checkpoint.
run_repairs <- function() {
  before_options <- options()
  on.exit(options(before_options), add = TRUE)
  invalid <- rawToChar(as.raw(255))
  Encoding(invalid) <- "UTF-8"
  warn_count <- 0L
  warn_handler <- function(w) { warn_count <<- warn_count + 1L }
  for (warn_level in c(0, 2)) {
    options(warn = warn_level)
    tag <- paste("repair warn", warn_level)
    withCallingHandlers({
      invoke(variables = invalid, label = paste(tag, "invalid variable encoding"), code = "E_PPC_SOURCE_ARGUMENT")
      invoke(files = invalid, label = paste(tag, "invalid path encoding"), code = "E_PPC_SOURCE_ARGUMENT")
      invoke(expected_model = invalid, label = paste(tag, "invalid model encoding"), code = "E_PPC_SOURCE_ARGUMENT")
      malformed <- invoke(original_hashes = rep(invalid, length(paths)),
        label = paste(tag, "invalid hash encoding"), code = "E_PPC_SOURCE_ARGUMENT")
      ok(isTRUE(malformed$receipt$original_fit_hashes_present) &&
          identical(malformed$receipt$historical_identity,
            "read-time receipt; original fit-time hashes supplied but unverified"),
        paste(tag, "invalid hash remains supplied and unverified"))
      p <- base_policy; names(p)[2] <- invalid
      invoke(posterior = p, label = paste(tag, "invalid policy name encoding"), code = "E_PPC_SOURCE_ARGUMENT")
      v <- vars; names(v) <- c(invalid, "b")
      invoke(variables = v, label = paste(tag, "invalid variable attribute name"), code = "E_PPC_SOURCE_ARGUMENT")
      slots <- 1; names(slots) <- invalid
      invoke(consumer_buffers = list(fixed = slots, per_selected_row = numeric()),
        label = paste(tag, "invalid consumer slot name"), code = "E_PPC_SOURCE_ARGUMENT")
      for (kind in c("numeric", "comment", "header")) {
        p <- file.path(fixtures, paste0("invalid-", kind, "-", warn_level, ".csv"))
        lines <- readLines(paths[1])
        at <- if (kind == "numeric") 18L else if (kind == "comment") 1L else 17L
        prefix <- paste0(paste(lines[seq_len(at - 1L)], collapse = "\n"), if (at > 1L) "\n" else "")
        record <- if (kind == "comment") "# bad " else if (kind == "header") "lp__," else "1,"
        suffix <- paste0("\n", paste(lines[seq.int(at + 1L, length(lines))], collapse = "\n"), "\n")
        con <- file(p, "wb")
        writeBin(c(charToRaw(prefix), charToRaw(record), as.raw(255), charToRaw(suffix)), con)
        close(con)
        invoke(files = p, label = paste(tag, "invalid raw", kind), code = "E_PPC_SOURCE_ROW")
      }
      malformed <- invoke(original_hashes = "bad", label = paste(tag, "malformed supplied hashes"),
        code = "E_PPC_SOURCE_ARGUMENT")
      ok(isTRUE(malformed$receipt$original_fit_hashes_present) &&
          grepl("supplied but unverified", malformed$receipt$historical_identity, fixed = TRUE),
        paste(tag, "malformed supplied hashes not labeled absent"))
      refusal <- invoke(states = list(method = "balanced", draws = 9),
        consumer_work = list(per_selected_row = c(first = 1, second = 2), limits = c(first = 9, second = 17)),
        label = paste(tag, "requested versus admitted work"), code = "E_PPC_SOURCE_CONSUMER_WORK")
      ok(identical(refusal$receipt$consumer_work$requested, c(first = 9, second = 18)) &&
          identical(refusal$receipt$consumer_work$admitted, c(first = 9)),
        paste(tag, "over-limit term is requested and never admitted"))
      preexisting <- file.path(fixtures, paste0("preexisting-", warn_level))
      dir.create(preexisting)
      sentinel <- file.path(preexisting, "sentinel.txt")
      writeLines("must survive", sentinel)
      assign("tempfile", function(...) preexisting, envir = ns)
      invoke(label = paste(tag, "directory collision"), code = "E_PPC_SOURCE_IO")
      ok(dir.exists(preexisting) && file.exists(sentinel) && identical(readLines(sentinel), "must survive"),
        paste(tag, "failed creation preserves preexisting directory and sentinel"))
      assign("dir.create", function(...) stop(structure(list(message = "creation interrupt", call = NULL),
        class = c("interrupt", "condition"))), envir = ns)
      before <- state()
      interrupted <- tryCatch(ingest(paths, vars, base_policy), interrupt = identity)
      ok(inherits(interrupted, "interrupt") && identical(before, state()) && file.exists(sentinel),
        paste(tag, "creation interruption never unlinks an unowned path"))
      rm(list = c("tempfile", "dir.create"), envir = ns)
    }, warning = warn_handler)
  }
  ok(warn_count == 0L, "repair controls emit no warnings under warn zero or two")
  # A successful tiny acquisition confirms the changed ownership/encoding path.
  tiny <- fixture(5, L = 1, label = "repair-one-row")
  z <- invoke(files = tiny, label = "repair successful acquisition")
  ok(z$receipt$read_bytes_charged == 7 * file.info(tiny)$size &&
      identical(unname(z$draws[1, ]), c(-5001, 5001)), "repair tiny values and seven-pass receipt")
  if (nzchar(evidence)) saveRDS(list(status = "passed", assertions = passed, cases = cases),
    file.path(evidence, "repair-results.rds"))
  cat("REPAIR_SUCCESS", passed, "assertions\n")
}
tryCatch({
  if (!identical(Sys.getenv("BMEAS_STREAMING_REPAIR_ONLY"), "1")) run()
  run_repairs()
}, finally = unlink(fixtures, recursive = TRUE, force = TRUE))
