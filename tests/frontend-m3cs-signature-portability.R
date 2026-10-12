source(file.path("helpers", "native-guard.R"))
library(bmeas)
local({
  private <- function(name) getFromNamespace(name, "bmeas")
  # This frozen, synthetic 0.0.33 descriptor is independent of today's
  # constructor and canonical-signature helper. It is an engineering oracle,
  # not qualification of the external Stan resource or native execution.
  capsule <- readRDS(file.path("fixtures-archive", "legacy-joint-m3cs-v033.rds"))
  model <- capsule$immutable$scientific_context$models$left$model
  stopifnot(identical(model$adapter$wrapper_hash, "d5b7dd069d872aa66e0239be94cbbc2a"),
            identical(model$adapter$resources[[1L]]$sha256,
                      "4b070861489b188e63575c4e0a8dae18cd54d01afd8f1d56a804714061fdc4d1"))
  kind <- private(".bh_m3cs_kind")
  callback_state <- new.env(parent = baseenv())
  callback_state$calls <- 0L
  model$validate_data <- eval(quote(function(...) {
    calls <<- calls + 1L; stop("callback")
  }), callback_state)
  model$prepare_data <- model$validate_data
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
