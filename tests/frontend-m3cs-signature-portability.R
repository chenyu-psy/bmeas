source(file.path("helpers", "native-guard.R"))
library(bmeas)
local({
  private <- function(name) getFromNamespace(name, "bmeas")
  resource <- Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
  stopifnot(nzchar(resource), file.exists(resource))
  model <- m3_cs_model(resource)
  kind <- private(".bh_m3cs_kind")
  callback_state <- new.env(parent = baseenv())
  callback_state$calls <- 0L
  model$validate_data <- eval(quote(function(...) {
    calls <<- calls + 1L; stop("callback")
  }), callback_state)
  for (hash in c("d5b7dd069d872aa66e0239be94cbbc2a", "db7e78458a367eb1b1a150c843a0e015")) {
    saved <- model; saved$adapter$wrapper_hash <- hash
    original <- serialize(saved, NULL, version = 2)
    stopifnot(kind(saved), callback_state$calls == 0L, identical(original, serialize(saved, NULL, version = 2)))
    wrong <- saved; wrong$adapter$wrapper_hash <- paste(rep("0", 32), collapse = "")
    stopifnot(!kind(wrong))
    wrong <- saved; wrong$adapter$function_code[1L] <- "altered wrapper"
    stopifnot(!kind(wrong))
    wrong <- saved; wrong$adapter$resources[[1L]]$sha256 <- paste(rep("0", 64), collapse = "")
    stopifnot(!kind(wrong), callback_state$calls == 0L)
  }
  cat("PASS both verified exact passive CS signatures; unchanged serialized values; wrong hash, wrapper and resource refused; zero callbacks\n")
})
