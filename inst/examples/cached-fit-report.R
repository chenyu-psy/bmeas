# Call this on an existing bmeas fit. Defining it does not fit or simulate a model.
read_saved_fit <- function(fit) {
  report <- bmeas::fit_checks(fit)
  print(summary(fit))
  print(report$context$blocks)
  print(report$sampling$extrema$rhat)
  print(report$sampling$chains)
  print(report$domains$predictive_fit)
  # Mean MCSE and actual cached interval columns, with no draw extraction.
  parameters <- bmeas::parameter_summary(fit)
  stopifnot(identical(parameters, bmeas::posterior_summary(fit)))
  invisible(list(checks = report, parameters = parameters))
}
# Sampling diagnostics do not validate empirical model fit. Present frontend
# predictive checks remain unsupported; reading this report starts no PPC/LOO.
