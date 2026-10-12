source(file.path("helpers", "native-guard.R"))
library(bmeas)
local({
  private <- function(name) getFromNamespace(name, "bmeas")
  maximum <- 2^53 - 1
  error <- function(expr) tryCatch({ force(expr); NULL }, error = identity)
  refused <- function(expr, code) {
    result <- error(expr)
    stopifnot(inherits(result, "measurement_error"), identical(result$code, code))
  }
  policy <- list(max_context_bytes = maximum, max_context_nodes = maximum, max_depth = 64)
  for (field in c("max_context_bytes", "max_context_nodes")) {
    for (value in c(maximum - 1, maximum)) {
      item <- policy; item[[field]] <- value
      stopifnot(identical(private(".bh_pf_policy")(item), item))
    }
    item <- policy; item[[field]] <- maximum + 1
    stopifnot(inherits(error(private(".bh_pf_policy")(item)), "error"))
  }
  add <- private(".bh_portable_codec_add")
  stopifnot(identical(add(maximum - 1, 1), maximum), identical(add(maximum, 0), maximum))
  refused(add(maximum, 1), "E_PORTABLE_SIZE")
  refused(add(maximum - 1, 2), "E_PORTABLE_SIZE")
  stopifnot(identical(rawToChar(private(".bh_portable_codec_decimal")(maximum)), "9007199254740991"))
  stopifnot(identical(writeBin(maximum, raw(), size = 8L, endian = "big"),
    as.raw(c(0x43, 0x3f, rep(0xff, 6)))))
  product <- private(".bh_pf_exact_product")
  dimension_product <- function(dimensions) {
    answer <- 1.0
    for (width in dimensions)
      answer <- product(answer, as.double(width), "test dimensions", "dimension product overflow")
    answer
  }
  stopifnot(identical(product(maximum, 1, "count", "overflow"), maximum),
    identical(product(maximum - 1, 1, "count", "overflow"), maximum - 1),
    identical(product(maximum, 0, "count", "overflow"), 0),
    identical(dimension_product(c(3L, 4L)), 12))
  stopifnot(inherits(error(dimension_product(c(67108864L, 134217728L))), "error"),
    inherits(error(product(2^52, 2, "raw count", "overflow")), "error"))
  # A historical admitted policy retains its actual doubles and serialization.
  historical <- list(max_context_bytes = maximum - 1, max_context_nodes = maximum - 1, max_depth = 64)
  original <- serialize(historical, NULL, version = 2)
  admitted <- private(".bh_pf_policy")(historical)
  stopifnot(identical(admitted, historical), identical(serialize(admitted, NULL, version = 2), original))
  locale <- Sys.getlocale("LC_CTYPE")
  bytes <- function(value) {
    con <- rawConnection(raw(), "wb")
    on.exit(close(con))
    private(".bh_portable_codec_write")(value, con, policy, locale)
    rawConnectionValue(con)
  }
  stopifnot(identical(bytes(historical), bytes(admitted)),
    identical(writeBin(historical$max_context_bytes, raw(), size = 8L, endian = "big"),
      as.raw(c(0x43, 0x3f, rep(0xff, 5), 0xfe))))
  cat("PASS exact ceiling below/at/above, addition, decimal/binary64, shared product guard, historical policy serialization and canonical bytes\n")
})
