source(file.path("helpers", "native-guard.R"))
library(bmeas)
local({
  private <- function(name) getFromNamespace(name, "bmeas")
  models <- list(gaussian = gaussian_model(), binomial = binomial_model(), lognormal = lognormal_model())
  passive <- private(".bh_pf_basic_kind")
  live <- private(".bh_builtin_kind")
  for (family in names(models)) stopifnot(identical(live(models[[family]]), family))
  ns <- asNamespace("bmeas")
  guarded <- c("gaussian_model", "binomial_model", "lognormal_model", ".mp_hash")
  saved <- lapply(guarded, get, envir = ns, inherits = FALSE)
  locked <- vapply(guarded, bindingIsLocked, logical(1), env = ns)
  on.exit(for (i in seq_along(guarded)) {
    if (bindingIsLocked(guarded[i], ns)) unlockBinding(guarded[i], ns)
    assign(guarded[i], saved[[i]], ns)
    if (locked[i]) lockBinding(guarded[i], ns)
  })
  for (i in seq_along(guarded)) {
    if (locked[i]) unlockBinding(guarded[i], ns)
    assign(guarded[i], function(...) stop("passive classifier constructor/hash trap"), ns)
    if (locked[i]) lockBinding(guarded[i], ns)
  }
  for (family in names(models)) {
    model <- models[[family]]
    before <- serialize(model, NULL, version = 2)
    stopifnot(identical(passive(model), family), identical(serialize(model, NULL, version = 2), before))
    for (field in c("data_schema", "parameters", "likelihood", "backend", "constants", "adapter")) {
      changed <- model; changed[[field]] <- list(impostor = TRUE)
      stopifnot(is.na(passive(changed)))
    }
  }
  cat("PASS three exact family descriptors match live classification; all six altered fields refused per family; zero constructor/hash calls; original serialization preserved\n")
})
