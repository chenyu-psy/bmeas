.mp_abort <- function(code, message) {
  stop(structure(list(message = paste0(code, ": ", message), call = NULL,
                      code = code), class = c("measurement_error", "error", "condition")))
}
.mp_scalar_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    .mp_abort("E_ARGUMENT", paste(name, "must be one nonempty string"))
  invisible(x)
}
.mp_names <- function(x, name) {
  if (!length(x) || is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x)))
    .mp_abort("E_NAMES", paste(name, "must have distinct nonempty names"))
  invisible(x)
}
.mp_key_component <- function(x, name) {
  x <- as.character(x)
  if (anyNA(x) || any(!nzchar(x)) || any(grepl("::", x, fixed = TRUE)))
    .mp_abort("E_KEY", paste(name, "cannot be missing, empty, or contain '::'"))
  x
}
.mp_hash <- function(object) {
  f <- tempfile("measurement-hash-"); on.exit(unlink(f))
  saveRDS(object, f, version = 2)
  unname(tools::md5sum(f))
}
.mp_integer <- function(x, name, lower = 1L) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < lower || x > .Machine$integer.max || x != floor(x))
    .mp_abort("E_ARGUMENT", paste(name, "must be an integer >=", lower))
  as.integer(x)
}
