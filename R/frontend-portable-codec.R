# Canonical portable-context codec, contract v1.1, implementation-only stage.
# The caller admits positional schemas/classes/attributes, projects timestamps,
# verifies old seals and counts the complete planned representation before copy.
# This module neither owns a connection nor reads files or invokes model code.

.bh_portable_codec_fail <- function(code, message) {
  .mp_abort(code, message)
}

.bh_portable_codec_exact_max <- function() (2^53 - 1)

.bh_portable_codec_add <- function(a, b) {
  if (b < 0 || a > .bh_portable_codec_exact_max() - b)
    .bh_portable_codec_fail("E_PORTABLE_SIZE", "Canonical size arithmetic overflowed.")
  a + b
}

.bh_portable_codec_decimal <- function(n) {
  # Arithmetic digit extraction, independent of OutDec, scipen and LC_NUMERIC.
  digits <- base::charToRaw("0123456789")
  answer <- base::raw(16L)
  at <- 16L
  repeat {
    q <- base::floor(n / 10.0)
    r <- n - q * 10.0
    if (r < 0 || r > 9 || r != base::floor(r))
      .bh_portable_codec_fail("E_PORTABLE_REPRESENTATION", "Exact decimal counts are unavailable.")
    answer[[at]] <- digits[[r + 1.0]]
    if (q == 0) break
    n <- q
    at <- at - 1L
  }
  base::.subset(answer, base::seq.int(at, 16L))
}

.bh_portable_codec_length <- function(x) {
  # length() is generic; unclass() and .subset2() do not dispatch S3 methods.
  base::as.double(base::length(base::unclass(x)))
}

.bh_portable_codec_locale <- function(locale) {
  if (!base::identical(base::Sys.getlocale("LC_CTYPE"), locale))
    .bh_portable_codec_fail("E_PORTABLE_ENCODING", "LC_CTYPE changed during portable conversion.")
  invisible(NULL)
}

.bh_portable_codec_capabilities <- function(locale) {
  if (base::typeof(locale) != "character" || base::isS4(locale) ||
      !base::is.null(base::attributes(locale)) || base::length(locale) != 1L ||
      base::is.na(locale))
    .bh_portable_codec_fail("E_PORTABLE_ENCODING", "A captured plain LC_CTYPE label is required.")
  .bh_portable_codec_locale(locale)
  m <- base::.Machine
  if (.subset2(m, "double.base") != 2 || .subset2(m, "double.digits") != 53 ||
      .subset2(m, "double.min.exp") != -1022 || .subset2(m, "double.max.exp") != 1024)
    .bh_portable_codec_fail("E_PORTABLE_REPRESENTATION", "IEEE-754 binary64 support is required.")
  actual <- base::tryCatch(
    base::writeBin(c(1.0, -0.0), base::raw(), size = 8L, endian = "big"),
    error = function(e) .bh_portable_codec_fail(
      "E_PORTABLE_REPRESENTATION", "Big-endian binary64 encoding is unavailable."))
  expected <- base::as.raw(c(63L, 240L, 0L, 0L, 0L, 0L, 0L, 0L,
                            128L, 0L, 0L, 0L, 0L, 0L, 0L, 0L))
  if (!base::identical(actual, expected))
    .bh_portable_codec_fail("E_PORTABLE_REPRESENTATION", "Binary64 encoding or signed zero is unavailable.")
  invisible(NULL)
}

.bh_portable_codec_byte_chunk <- function(s, start, size) {
  # A private scalar byte view permits bounded slicing without decoding text or
  # retaining a raw copy of an entire character vector or canonical stream.
  byte_view <- s
  base::Encoding(byte_view) <- "bytes"
  base::charToRaw(base::substr(byte_view, start, start + size - 1.0))
}

.bh_portable_codec_text_scan <- function(s, utf8) {
  n <- base::as.double(base::nchar(s, type = "bytes", allowNA = FALSE, keepNA = TRUE))
  position <- 1.0
  continuation <- 0L
  lower <- 128L
  upper <- 191L
  ascii <- TRUE
  while (position <= n) {
    size <- base::min(65536.0, n - position + 1.0)
    part <- .bh_portable_codec_byte_chunk(s, position, size)
    octets <- base::as.integer(part)
    if (base::all(octets < 128L)) {
      if (utf8 && continuation != 0L)
        .bh_portable_codec_fail("E_PORTABLE_ENCODING", "Character text is not well-formed UTF-8.")
      position <- position + size
      next
    }
    ascii <- FALSE
    if (!utf8) {
      position <- position + size
      next
    }
    j <- 1L
    while (j <= size) {
      b <- octets[[j]]
      if (utf8) {
        if (continuation > 0L) {
          if (b < lower || b > upper)
            .bh_portable_codec_fail("E_PORTABLE_ENCODING", "Character text is not well-formed UTF-8.")
          continuation <- continuation - 1L
          lower <- 128L
          upper <- 191L
        } else if (b <= 127L) {
          # ASCII includes no embedded NUL in an ordinary R character scalar.
        } else if (b >= 194L && b <= 223L) {
          continuation <- 1L
        } else if (b >= 224L && b <= 239L) {
          continuation <- 2L
          if (b == 224L) lower <- 160L
          if (b == 237L) upper <- 159L
        } else if (b >= 240L && b <= 244L) {
          continuation <- 3L
          if (b == 240L) lower <- 144L
          if (b == 244L) upper <- 143L
        } else {
          .bh_portable_codec_fail("E_PORTABLE_ENCODING", "Character text is not well-formed UTF-8.")
        }
      }
      j <- j + 1L
    }
    position <- position + size
  }
  if (utf8 && continuation != 0L)
    .bh_portable_codec_fail("E_PORTABLE_ENCODING", "Character text ends inside a UTF-8 sequence.")
  list(bytes = n, ascii = ascii)
}

.bh_portable_codec_same_bytes <- function(a, b) {
  na <- base::nchar(a, type = "bytes", allowNA = FALSE, keepNA = TRUE)
  nb <- base::nchar(b, type = "bytes", allowNA = FALSE, keepNA = TRUE)
  if (na != nb) return(FALSE)
  position <- 1.0
  while (position <= na) {
    size <- base::min(65536.0, na - position + 1.0)
    if (!base::identical(.bh_portable_codec_byte_chunk(a, position, size),
                         .bh_portable_codec_byte_chunk(b, position, size))) return(FALSE)
    position <- position + size
  }
  TRUE
}

.bh_portable_codec_text <- function(s, locale) {
  .bh_portable_codec_locale(locale)
  if (base::is.na(s)) return(list(value = NA_character_, bytes = 0.0, missing = TRUE))
  encoding <- base::Encoding(s)
  if (encoding == "bytes")
    .bh_portable_codec_fail("E_PORTABLE_ENCODING", "Byte-marked character text is not portable.")
  original <- .bh_portable_codec_text_scan(s, utf8 = encoding == "UTF-8")
  if (original$ascii) {
    answer <- base::character(1L)
    answer[[1L]] <- s
    return(list(value = answer, bytes = original$bytes, missing = FALSE))
  }
  if (encoding == "UTF-8") {
    answer <- s
  } else if (encoding == "latin1" || encoding == "unknown") {
    from <- if (encoding == "latin1") "latin1" else ""
    answer <- base::tryCatch(
      base::iconv(s, from = from, to = "UTF-8", sub = NA_character_, mark = FALSE),
      error = function(e) .bh_portable_codec_fail(
        "E_PORTABLE_ENCODING", "Strict conversion to UTF-8 is unavailable."))
    .bh_portable_codec_locale(locale)
    if (base::is.na(answer))
      .bh_portable_codec_fail("E_PORTABLE_ENCODING", "Character text cannot be converted losslessly to UTF-8.")
    # Explicit Latin-1 mapping is also checked; the native case must reproduce
    # the original bytes in the captured, unchanged locale, not just compare ==.
    roundtrip <- base::tryCatch(
      base::iconv(answer, from = "UTF-8", to = from, sub = NA_character_, mark = FALSE),
      error = function(e) .bh_portable_codec_fail(
        "E_PORTABLE_ENCODING", "Strict character round-trip is unavailable."))
    .bh_portable_codec_locale(locale)
    if (base::is.na(roundtrip) || !.bh_portable_codec_same_bytes(s, roundtrip))
      .bh_portable_codec_fail("E_PORTABLE_ENCODING", "Character conversion does not reproduce the original bytes.")
  } else {
    .bh_portable_codec_fail("E_PORTABLE_ENCODING", "An unsupported character encoding was encountered.")
  }
  scan <- .bh_portable_codec_text_scan(answer, utf8 = TRUE)
  base::Encoding(answer) <- "UTF-8"
  list(value = answer, bytes = scan$bytes, missing = FALSE)
}

.bh_portable_codec_compare_text <- function(a, b) {
  na <- base::as.double(base::nchar(a, type = "bytes"))
  nb <- base::as.double(base::nchar(b, type = "bytes"))
  both <- base::min(na, nb)
  position <- 1.0
  while (position <= both) {
    size <- base::min(65536.0, both - position + 1.0)
    aa <- .bh_portable_codec_byte_chunk(a, position, size)
    bb <- .bh_portable_codec_byte_chunk(b, position, size)
    j <- 1L
    while (j <= size) {
      ai <- base::as.integer(aa[[j]])
      bi <- base::as.integer(bb[[j]])
      if (ai < bi) return(-1L)
      if (ai > bi) return(1L)
      j <- j + 1L
    }
    position <- position + size
  }
  if (na < nb) -1L else if (na > nb) 1L else 0L
}

.bh_portable_codec_attributes <- function(x) {
  # attributes(x) itself expands compact row names. Remove that attribute on a
  # local inspection copy before enumeration; do not call row.names()/nrow().
  rn <- base::.row_names_info(x, type = 0L)
  if (base::is.null(rn)) return(base::attributes(x))
  probe <- x
  base::attr(probe, "row.names") <- NULL
  a <- base::attributes(probe)
  if (base::is.null(a)) a <- list()
  a[[base::length(a) + 1L]] <- rn
  an <- base::names(a)
  if (base::is.null(an)) an <- base::character(base::length(a))
  an[[base::length(a)]] <- "row.names"
  base::names(a) <- an
  a
}

.bh_portable_codec_dimensions <- function(a, n) {
  d <- base::.subset2(a, "dim")
  if (base::is.null(d)) return(invisible(NULL))
  if (base::typeof(d) != "integer" || base::isS4(d) ||
      !base::is.null(base::attributes(d)) || base::length(d) == 0L)
    .bh_portable_codec_fail("E_PORTABLE_SHAPE", "Dimensions must be an unclassed integer vector.")
  product <- 1.0
  zero <- FALSE
  i <- 1.0
  while (i <= base::length(d)) {
    z <- base::.subset2(d, i)
    if (base::is.na(z) || z < 0L)
      .bh_portable_codec_fail("E_PORTABLE_SHAPE", "Dimensions must be finite nonnegative integers.")
    if (z == 0L) zero <- TRUE
    i <- i + 1.0
  }
  if (zero) product <- 0.0 else {
    i <- 1.0
    while (i <= base::length(d)) {
      z <- base::as.double(base::.subset2(d, i))
      if (product > base::floor(.bh_portable_codec_exact_max() / z))
        .bh_portable_codec_fail("E_PORTABLE_SIZE", "The dimension product exceeds exact arithmetic.")
      product <- product * z
      i <- i + 1.0
    }
  }
  if (product != n)
    .bh_portable_codec_fail("E_PORTABLE_SHAPE", "Dimensions do not match the stored vector length.")
  invisible(NULL)
}

.bh_portable_codec_run <- function(x, policy, locale, mode, connection = NULL) {
  .bh_portable_codec_capabilities(locale)
  copy <- mode == "copy"
  writing <- mode == "write"
  byte_limit <- base::.subset2(policy, "max_context_bytes")
  node_limit <- base::.subset2(policy, "max_context_nodes")
  depth_limit <- base::.subset2(policy, "max_depth")
  bytes <- 0.0
  nodes <- 0.0
  depth <- 0.0
  buffer <- if (writing) base::raw(65536L) else NULL
  buffered <- 0L

  need_bytes <- function(n) {
    z <- .bh_portable_codec_add(bytes, n)
    if (z > byte_limit)
      .bh_portable_codec_fail("E_PORTABLE_BUDGET", "The canonical byte allowance was exceeded.")
    invisible(z)
  }
  need_nodes <- function(n) {
    z <- .bh_portable_codec_add(nodes, n)
    if (z > node_limit)
      .bh_portable_codec_fail("E_PORTABLE_BUDGET", "The canonical node allowance was exceeded.")
    invisible(z)
  }
  node <- function(n, at) {
    if (at > depth_limit)
      .bh_portable_codec_fail("E_PORTABLE_BUDGET", "The canonical depth allowance was exceeded.")
    nodes <<- need_nodes(n)
    if (n > 0 && at > depth) depth <<- at
    invisible(NULL)
  }
  flush_buffer <- function() {
    if (buffered > 0L) {
      part <- base::.subset(buffer, base::seq_len(buffered))
      base::withCallingHandlers(
        base::writeBin(part, connection, useBytes = TRUE),
        warning = function(w) .bh_portable_codec_fail(
          "E_PORTABLE_WRITE", "The owned canonical connection reported an incomplete write."))
      buffered <<- 0L
    }
    invisible(NULL)
  }
  emit <- function(raw_bytes) {
    n <- base::as.double(base::length(raw_bytes))
    bytes <<- need_bytes(n)
    if (writing && n > 0) {
      start <- 1.0
      while (start <= n) {
        take <- base::min(65536L - buffered, n - start + 1.0)
        buffer[base::seq.int(buffered + 1L, buffered + take)] <<-
          base::.subset(raw_bytes, base::seq.int(start, start + take - 1.0))
        buffered <<- buffered + base::as.integer(take)
        start <- start + take
        if (buffered == 65536L) flush_buffer()
      }
    }
    invisible(NULL)
  }
  fixed <- function(s) emit(base::charToRaw(s))
  count_field <- function(n) {
    emit(.bh_portable_codec_decimal(n))
    fixed(":")
  }
  text_scalar <- function(t) {
    if (t$missing) {
      fixed("a;")
    } else {
      fixed("s")
      count_field(t$bytes)
      if (writing) {
        start <- 1.0
        while (start <= t$bytes) {
          take <- base::min(65536.0, t$bytes - start + 1.0)
          emit(.bh_portable_codec_byte_chunk(t$value, start, take))
          start <- start + take
        }
      } else bytes <<- need_bytes(t$bytes)
    }
    invisible(NULL)
  }

  if (writing && !base::isOpen(connection, "write"))
    .bh_portable_codec_fail("E_PORTABLE_WRITE", "An already-open writable binary connection is required.")
  fixed("BMEAS-PORTABLE-CONTEXT-1\n")
  # Explicit traversal frames avoid relying on the caller's expressions/stack
  # options. Shared occurrences are revisited; no reference table is retained.
  stack <- list(list(value = x, at = 1.0, phase = "begin"))
  top <- 1.0
  last <- NULL
  while (top > 0) {
    f <- stack[[top]]
    if (f$phase == "begin") {
      value <- f$value
      type <- base::typeof(value)
      if (base::isS4(value) || !(type %in% c("NULL", "list", "logical", "integer", "double", "character", "raw")))
        .bh_portable_codec_fail("E_PORTABLE_TYPE", "Executable, reference, language or unsupported atomic content is not portable.")
      node(1.0, f$at)
      if (type == "NULL") {
        fixed("N;")
        last <- NULL
        stack[[top]] <- NULL
        top <- top - 1.0
        next
      }
      n <- .bh_portable_codec_length(value)
      if (!base::is.finite(n) || n < 0 || n > .bh_portable_codec_exact_max())
        .bh_portable_codec_fail("E_PORTABLE_SIZE", "The vector length cannot be represented exactly.")
      # Lower bounds are checked before metadata/output allocation. The caller
      # has already admitted the whole payload before invoking copy mode.
      need_nodes(.bh_portable_codec_add(1.0, n))
      need_bytes(if (type == "raw") n else .bh_portable_codec_add(n, n))
      attrs <- .bh_portable_codec_attributes(value)
      na <- base::length(attrs)
      need_nodes(.bh_portable_codec_add(1.0, .bh_portable_codec_add(na, na)))
      .bh_portable_codec_dimensions(attrs, n)
      attr_names <- base::character(na)
      attr_text <- base::vector("list", na)
      order <- base::integer(na)
      j <- 1L
      while (j <= na) {
        t <- .bh_portable_codec_text(base::.subset2(base::names(attrs), j), locale)
        if (t$missing || t$bytes == 0)
          .bh_portable_codec_fail("E_PORTABLE_ATTRIBUTE", "Attributes must have distinct nonempty text names.")
        attr_names[[j]] <- t$value
        attr_text[[j]] <- t
        order[[j]] <- j
        k <- j
        while (k > 1L) {
          previous <- order[[k - 1L]]
          comparison <- .bh_portable_codec_compare_text(attr_names[[previous]], t$value)
          if (comparison == 0L)
            .bh_portable_codec_fail("E_PORTABLE_ATTRIBUTE", "Attribute names collide after UTF-8 normalization.")
          if (comparison < 0L) break
          order[[k]] <- previous
          k <- k - 1L
        }
        order[[k]] <- j
        j <- j + 1L
      }
      tag <- switch(type, list = "L", logical = "B", integer = "I",
                    double = "D", character = "S", raw = "R")
      fixed(tag)
      count_field(n)
      node(1.0, f$at + 1.0)
      fixed("A")
      count_field(na)
      f$type <- type
      f$n <- n
      f$attrs <- attrs
      f$attr_names <- attr_names
      f$attr_text <- attr_text
      f$order <- order
      f$na <- na
      f$index <- 1.0
      f$phase <- "attributes"
      if (copy) f$copied_attrs <- base::vector("list", na)
      stack[[top]] <- f
      next
    }
    if (f$phase == "attribute_return") {
      if (copy) f$copied_attrs[f$actual] <- list(last)
      f$index <- f$index + 1.0
      f$phase <- "attributes"
      stack[[top]] <- f
      next
    }
    if (f$phase == "attributes") {
      if (f$index <= f$na) {
        actual <- f$order[[f$index]]
        node(1.0, f$at + 2.0)
        text_scalar(f$attr_text[[actual]])
        f$actual <- actual
        f$phase <- "attribute_return"
        stack[[top]] <- f
        top <- top + 1.0
        stack[[top]] <- list(value = base::.subset2(f$attrs, actual),
                             at = f$at + 2.0, phase = "begin")
        next
      }
      need_nodes(f$n)
      need_bytes(if (f$type == "raw") f$n else .bh_portable_codec_add(f$n, f$n))
      if (copy) f$output <- base::vector(f$type, f$n)
      f$index <- 1.0
      f$phase <- "elements"
      if (f$type != "list" && f$n > 0) node(f$n, f$at + 1.0)
      stack[[top]] <- f
      next
    }
    if (f$phase == "element_return") {
      if (copy) f$output[f$index] <- list(last)
      f$index <- f$index + 1.0
      f$phase <- "elements"
      stack[[top]] <- f
      next
    }
    if (f$phase == "elements") {
      if (f$type == "list" && f$index <= f$n) {
        f$phase <- "element_return"
        stack[[top]] <- f
        top <- top + 1.0
        stack[[top]] <- list(value = base::.subset2(f$value, f$index),
                             at = f$at + 1.0, phase = "begin")
        next
      }
      if (f$type == "raw") {
        if (!writing) bytes <- need_bytes(f$n)
        start <- 1.0
        while ((copy || writing) && start <= f$n) {
          take <- base::min(65536.0, f$n - start + 1.0)
          ix <- base::seq.int(start, start + take - 1.0)
          piece <- base::.subset(f$value, ix)
          if (copy) f$output[ix] <- piece
          if (writing) emit(piece)
          start <- start + take
        }
      } else if (f$type != "list") {
        i <- 1.0
        while (i <= f$n) {
          z <- base::.subset2(f$value, i)
          if (f$type == "character") {
            t <- .bh_portable_codec_text(z, locale)
            text_scalar(t)
            if (copy) f$output[[i]] <- t$value
          } else {
            if (f$type == "logical") {
              fixed(if (base::is.na(z)) "a;" else if (z) "t;" else "f;")
            } else if (f$type == "integer") {
              if (base::is.na(z)) fixed("a;") else {
                fixed("i")
                zz <- base::as.double(z)
                if (zz < 0) fixed("-")
                emit(.bh_portable_codec_decimal(base::abs(zz)))
                fixed(";")
              }
            } else {
              if (base::is.nan(z)) {
                fixed("n;")
              } else if (base::is.na(z)) {
                fixed("a;")
              } else if (z == Inf) {
                fixed("p;")
              } else if (z == -Inf) {
                fixed("m;")
              } else {
                fixed("f")
                if (writing) {
                  emit(base::writeBin(z, base::raw(), size = 8L, endian = "big"))
                } else bytes <- need_bytes(8.0)
              }
            }
            if (copy) f$output[[i]] <- z
          }
          i <- i + 1.0
        }
      }
      if (copy) {
        if (f$na > 0L) {
          base::names(f$copied_attrs) <- f$attr_names
          base::attributes(f$output) <- f$copied_attrs
          # Base R can compress a supplied integer 1:n row-name vector. Never
          # silently claim preservation if the actual stored form changed.
          original_rows <- base::.subset2(f$copied_attrs, "row.names")
          if (!base::is.null(original_rows) &&
              !base::identical(base::.row_names_info(f$output, 0L), original_rows))
            .bh_portable_codec_fail("E_PORTABLE_REPRESENTATION", "Base R changed the stored row-name representation.")
        }
        last <- f$output
      } else last <- NULL
      stack[[top]] <- NULL
      top <- top - 1.0
    }
  }
  .bh_portable_codec_locale(locale)
  if (writing) flush_buffer()
  .bh_portable_codec_locale(locale)
  if (copy) last else list(bytes = bytes, nodes = nodes, depth = depth)
}

.bh_portable_codec_count <- function(x, policy, locale) {
  .bh_portable_codec_run(x, policy, locale, mode = "count")
}

.bh_portable_codec_copy <- function(x, policy, locale) {
  .bh_portable_codec_run(x, policy, locale, mode = "copy")
}

.bh_portable_codec_write <- function(x, connection, policy, locale) {
  .bh_portable_codec_run(x, policy, locale, mode = "write", connection = connection)
}
