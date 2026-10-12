.mp_builtin <- function(name, schema, parameters, symbol, arguments, types, statistics) {
  simulator <- switch(name,gaussian=.mp_simulate_gaussian,binomial=.mp_simulate_binomial,lognormal=.mp_simulate_lognormal)
  epred <- switch(name,gaussian=.mp_epred_gaussian,binomial=.mp_epred_binomial,lognormal=.mp_epred_lognormal)
  model <- new_observation_model(name, "1.1.0", schema, parameters,
    stan_density(symbol = symbol, arguments = arguments, argument_types = types,
                 normalized = TRUE, approximation = "none", builtin = TRUE, license = "Stan Math"),
    capabilities = model_capabilities(fit = TRUE, simulate = TRUE, posterior_predict = TRUE, posterior_epred = TRUE),
    check_statistics = statistics, provenance = list(source = "Original package contract; Stan Math density", license = "MIT"),
    methods = list(simulate = simulator, posterior_predict = simulator, posterior_epred = epred))
  model$backend <- if (name == "gaussian") "gaussian_cfa" else "basic_cfa"
  model
}
gaussian_model <- function() {
  .mp_builtin("gaussian", response_schema(y = "real"),
    list(mu = parameter_spec("real", "identity", "theta", "subject_task", "response", "Conditional response mean"),
         sigma = parameter_spec("positive", "log", "theta", "task", "response", "Observation-level standard deviation",predictor_units="log(response/unit)")),
    "normal_lpdf", c("y", "mu", "sigma"), c("real", "real", "real"),
    c("mean", "sd", "tail_quantiles", "cross_indicator_covariance"))
}
binomial_model <- function() {
  .mp_builtin("binomial", response_schema(successes = "nonnegative_integer", trials = "nonnegative_integer"),
    list(p = parameter_spec("unit_interval", "logit", "eta", "subject_task", "probability", "Success probability",predictor_units="log_odds")),
    "binomial_logit_lpmf", c("successes", "trials", "p"), c("int", "int", "real"),
    c("success_counts_given_trials", "all_success_all_failure"))
}
lognormal_model <- function() {
  .mp_builtin("lognormal", response_schema(y = "positive_real"),
    list(meanlog = parameter_spec("real", "identity", "theta", "subject_task", "log(response/unit)", "Log-response location"),
         sdlog = parameter_spec("positive", "log", "theta", "task", "log(response/unit)", "Log-response standard deviation",predictor_units="log(SD of log response)")),
    "lognormal_lpdf", c("y", "meanlog", "sdlog"), c("real", "real", "real"),
    c("median", "upper_quantiles", "log_scale_sd"))
}
sdm_model <- function(...) .mp_abort("E_MODEL_NOT_IMPLEMENTED", "Generic SDM selection is not implemented; use sdm_reference_model with the explicit pinned original source")

.mp_simulate_gaussian <- function(eta, observation_sd = NULL, trials = NULL) stats::rnorm(length(eta),eta,observation_sd)
.mp_simulate_binomial <- function(eta, observation_sd = NULL, trials = NULL) stats::rbinom(length(eta),trials,stats::plogis(eta))
.mp_simulate_lognormal <- function(eta, observation_sd = NULL, trials = NULL) stats::rlnorm(length(eta),eta,observation_sd)
.mp_epred_gaussian <- function(eta, observation_sd = NULL, trials = NULL) eta
.mp_epred_binomial <- function(eta, observation_sd = NULL, trials = NULL) trials*stats::plogis(eta)
.mp_epred_lognormal <- function(eta, observation_sd = NULL, trials = NULL) exp(eta+observation_sd^2/2)
