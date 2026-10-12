normal <- function(mean, sd) {
  if (!is.numeric(mean) || length(mean) != 1L || !is.finite(mean) ||
      !is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd <= 0)
    .mp_abort("E_PRIOR", "normal prior requires finite mean and positive sd")
  list(distribution = "normal", mean = mean, sd = sd)
}
half_normal <- function(sd) {
  result <- normal(0, sd); result$distribution <- "half_normal"; result
}
lkj <- function(shape) {
  if (!is.numeric(shape) || length(shape) != 1L || !is.finite(shape) || shape <= 0)
    .mp_abort("E_PRIOR", "LKJ shape must be positive")
  list(distribution = "lkj", shape = shape)
}
prior_set <- function(intercept, loading, sigma, correlation) {
  if (!identical(intercept$distribution, "normal") || !identical(loading$distribution, "normal") ||
      loading$mean != 0 || !identical(sigma$distribution, "half_normal") || !identical(correlation$distribution, "lkj"))
    .mp_abort("E_PRIOR", "P0 requires normal intercepts, zero-centered normal loadings, half-normal sigma and LKJ correlation")
  normal(intercept$mean, intercept$sd); normal(loading$mean, loading$sd)
  half_normal(sigma$sd); lkj(correlation$shape)
  structure(list(intercept = intercept, loading = loading, sigma = sigma,
                 correlation = correlation, source = "user", units = NULL, kind = "gaussian_shared"), class = "measurement_priors")
}
prior_preset <- function(name) {
  if (!identical(name, "gaussian_cfa_standardized_v1")) .mp_abort("E_PRIOR_PRESET", "unknown preset; priors are never guessed")
  result <- prior_set(normal(0, 1), normal(0, 0.5), half_normal(1), lkj(2))
  result$source <- name; result$units <- "externally_standardized"; result
}
block_prior <- function(intercept, loading, observation_sd = NULL, units, predictor_scale) {
  .mp_scalar_string(units, "prior observation units")
  predictor_scale <- match.arg(predictor_scale, c("response", "logit", "log_response"))
  if (!identical(intercept$distribution, "normal") || !identical(loading$distribution, "normal") || loading$mean != 0)
    .mp_abort("E_PRIOR", "block priors require normal intercept and zero-centered normal loadings")
  normal(intercept$mean, intercept$sd); normal(loading$mean, loading$sd)
  if (!is.null(observation_sd)) {
    if (!identical(observation_sd$distribution, "half_normal")) .mp_abort("E_PRIOR", "observation SD prior must be half_normal")
    half_normal(observation_sd$sd)
  }
  list(intercept = intercept, loading = loading, observation_sd = observation_sd,
       units = units, predictor_scale = predictor_scale)
}
mixed_prior_set <- function(blocks, correlation) {
  .mp_names(blocks, "prior blocks")
  if (!identical(correlation$distribution, "lkj")) .mp_abort("E_PRIOR", "factor correlation requires LKJ prior")
  lkj(correlation$shape)
  for (b in blocks) block_prior(b$intercept, b$loading, b$observation_sd, b$units, b$predictor_scale)
  structure(list(blocks = blocks, correlation = correlation, source = "user", kind = "block_specific"), class = "measurement_priors")
}
.mp_family_info <- function(name) {
  switch(name,
    gaussian = list(primary = "mu", nuisance = "sigma", scale = "response", family_id = 1L),
    binomial = list(primary = "p", nuisance = character(), scale = "logit", family_id = 2L),
    lognormal = list(primary = "meanlog", nuisance = "sdlog", scale = "log_response", family_id = 3L),
    .mp_abort("E_BACKEND_UNSUPPORTED", paste("no basic-family compiler for", name)))
}
.mp_legacy_gaussian <- function(spec) !spec$priors$kind %in% c("block_specific","parameter_specific") &&
  all(vapply(spec$models, function(b) b$model$name == "gaussian", logical(1)))
.mp_resolve_priors <- function(spec) {
  p <- spec$priors
  if(identical(p$kind,"parameter_specific")) return(.mp_resolve_parameter_priors(spec))
  if (!identical(p$kind, "block_specific")) {
    if (!all(vapply(spec$models, function(b) b$model$name == "gaussian", logical(1))))
      .mp_abort("E_PRIOR_SCALE", "mixed/non-Gaussian families require explicit mixed_prior_set with each block's units and predictor scale")
    prior_set(p$intercept, p$loading, p$sigma, p$correlation)
    return(stats::setNames(lapply(spec$models, function(b) {
      if (!is.null(p$units) && b$units != p$units) .mp_abort("E_PRIOR_UNITS", "standardized preset requires explicitly standardized units")
      block_prior(p$intercept, p$loading, p$sigma, b$units, "response")
    }), names(spec$models)))
  }
  mixed_prior_set(p$blocks, p$correlation)
  if (!setequal(names(p$blocks), names(spec$models))) .mp_abort("E_PRIOR_COVERAGE", "one prior block is required for every observation block")
  for (name in names(spec$models)) {
    b <- spec$models[[name]]; prior <- p$blocks[[name]]; info <- .mp_family_info(b$model$name)
    if (prior$units != b$units) .mp_abort("E_PRIOR_UNITS", paste("prior/observation units differ for block", name))
    if (prior$predictor_scale != info$scale) .mp_abort("E_PRIOR_SCALE", paste("prior must be on", info$scale, "scale for block", name))
    if (length(info$nuisance) != as.integer(!is.null(prior$observation_sd)))
      .mp_abort("E_PRIOR_COVERAGE", paste("observation SD prior required only for Gaussian/lognormal block", name))
  }
  p$blocks[names(spec$models)]
}
prior_table <- function(spec) {
  if (!inherits(spec, "measurement_spec")) .mp_abort("E_SPEC", "expected measurement_spec")
  if(identical(spec$structure$type,"component_covariance"))return(.mp_component_prior_table(spec))
  if (spec$structure$type != "cfa") .mp_abort("E_PRIOR_UNSUPPORTED", "covariance prior resolver is not implemented")
  if(identical(spec$priors$kind,"parameter_specific")) return(.mp_parameter_prior_table(spec))
  resolved <- .mp_resolve_priors(spec); source <- spec$priors$source
  indicators <- unlist(spec$structure$factors, use.names = FALSE)
  anchors <- unname(spec$structure$positive_anchors)
  rows <- list()
  for (key in indicators) {
    parts <- strsplit(key, "::", fixed = TRUE)[[1L]]; b <- parts[1L]
    p <- resolved[[b]]; info <- .mp_family_info(spec$models[[b]]$model$name)
    unit <- switch(info$scale, response = p$units, logit = "log_odds", log_response = paste0("log(response/",p$units,")"))
    rows[[length(rows)+1L]] <- data.frame(parameter = c(paste0("intercept:",key),paste0("loading:",key)),
      distribution = c("normal",if(key %in% anchors) "half_normal" else "normal"),
      location = c(p$intercept$mean,0), scale = c(p$intercept$sd,p$loading$sd),
      support = c("real",if(key %in% anchors) "positive" else "real"),
      prior_scale = info$scale, units = unit, source = source, stringsAsFactors = FALSE)
    if (length(info$nuisance)) rows[[length(rows)+1L]] <- data.frame(
      parameter = paste0(info$nuisance, ":", targets(b,info$nuisance,parts[3L])), distribution = "half_normal",
      location = 0, scale = p$observation_sd$sd, support = "positive",
      prior_scale = paste0(info$nuisance, " (positive SD; not its log)"), units = unit, source = source)
  }
  if (length(spec$structure$factors)>1L) rows[[length(rows)+1L]] <- data.frame(parameter="factor_correlation",
    distribution="lkj",location=NA_real_,scale=spec$priors$correlation$shape,support="correlation_matrix",
    prior_scale="unit_variance_factor",units="unitless",source=source)
  rows[[length(rows)+1L]] <- data.frame(parameter="factor_z_raw[*]",distribution="normal",location=0,scale=1,
    support="real",prior_scale="noncentered_standard_normal",units="unitless",source="structure")
  do.call(rbind,rows)
}
