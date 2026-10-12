targets <- function(model, parameter, tasks) {
  model <- .mp_key_component(model, "model key")
  parameter <- .mp_key_component(parameter, "parameter key")
  tasks <- .mp_key_component(tasks, "task key")
  if (length(model) != 1L || !length(parameter) || !length(tasks)) .mp_abort("E_TARGET", "one model and nonempty parameters/tasks required")
  grid <- expand.grid(parameter = parameter, task = tasks, stringsAsFactors = FALSE)
  paste(model, grid$parameter, grid$task, sep = "::")
}
task_parameters <- function(parameter) {
  list(parameter = .mp_key_component(parameter, "nuisance parameter"), level = "task")
}
cfa_structure <- function(factors, scale = "unit_variance", positive_anchors,
                          parameter_residual = "none", nuisance) {
  .mp_names(factors, "factors")
  if (!identical(scale, "unit_variance")) .mp_abort("E_FACTOR_SCALE", "P0 supports unit-variance factors only")
  if (missing(positive_anchors)) .mp_abort("E_FACTOR_ANCHOR", "provide one named positive anchor per factor")
  if (missing(nuisance)) .mp_abort("E_PARAMETER_COVERAGE", "explicit task-level nuisance parameters are required")
  if (!identical(parameter_residual, "none"))
    .mp_abort("E_DOUBLE_RESIDUAL", "Basic-family CFA has no extra latent parameter residual; Gaussian/lognormal retain only observation sigma/sdlog")
  if (!setequal(names(positive_anchors), names(factors)) || anyDuplicated(names(positive_anchors)))
    .mp_abort("E_FACTOR_ANCHOR", "positive_anchors must name each factor exactly once")
  for (factor in names(factors)) {
    if (length(factors[[factor]]) < 3L || anyDuplicated(factors[[factor]]))
      .mp_abort("E_FACTOR_IDENTIFICATION", paste("factor", factor, "requires at least three distinct indicators in this prototype"))
    if (!positive_anchors[[factor]] %in% factors[[factor]])
      .mp_abort("E_FACTOR_ANCHOR", paste("anchor is not an indicator of factor", factor))
  }
  if (anyDuplicated(unlist(factors, use.names = FALSE))) .mp_abort("E_CROSS_LOADING", "P0 supports simple structure only; a parameter cannot load on two factors")
  structure(list(type = "cfa", factors = factors, scale = scale,
                 positive_anchors = positive_anchors[names(factors)],
                 parameter_residual = parameter_residual, nuisance = nuisance), class = "measurement_structure")
}
parameter_covariance <- function(parameters, nuisance) {
  if (missing(nuisance)) .mp_abort("E_PARAMETER_COVERAGE", "explicit nuisance specification is required")
  if (!is.character(parameters) || !length(parameters) || anyDuplicated(parameters))
    .mp_abort("E_STRUCTURE", "parameters must be distinct target keys")
  structure(list(type = "parameter_covariance", parameters = parameters,
                 nuisance = nuisance, scale = "predictor"), class = "measurement_structure")
}
parameter_keys <- function(spec) {
  if (!inherits(spec, "measurement_spec")) .mp_abort("E_SPEC", "expected measurement_spec")
  spec$parameter_keys
}
.mp_parameter_keys <- function(models, mapped) {
  result <- list()
  for (b in names(models)) for (parameter in names(models[[b]]$model$parameters)) {
    d <- models[[b]]$model$parameters[[parameter]]
    tasks <- if(!is.null(models[[b]]$parameter_bindings)) {
      domain<-models[[b]]$parameter_bindings$domains
      domain$task[domain$parameter==parameter]
    } else sort(unique(mapped$blocks[[b]]$.task))
    if(!length(tasks))next
    result[[length(result) + 1L]] <- data.frame(model = b, parameter = parameter, task = tasks,
      key = targets(b, parameter, tasks), predictor = d$predictor, receives = d$receives,
      support = d$support, units = if(!is.null(models[[b]]$model$adapter)) d$units else switch(parameter,
        p = "probability", meanlog = paste0("log(response/",models[[b]]$units,")"),
        sdlog = paste0("log(response/",models[[b]]$units,")"), models[[b]]$units),
      observation_units = models[[b]]$units,
      predictor_units = if(!is.null(models[[b]]$model$adapter)) d$predictor_units else switch(parameter,p="log_odds",meanlog=paste0("log(response/",models[[b]]$units,")"),
        sigma=paste0("log(sigma/",models[[b]]$units,")"),sdlog="log(sdlog)",models[[b]]$units),stringsAsFactors = FALSE)
  }
  do.call(rbind, result)
}
