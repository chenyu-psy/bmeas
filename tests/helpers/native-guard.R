# Regression launcher guard: block real CmdStan model construction in every test.
setHook(packageEvent('cmdstanr','onLoad'), function(...) {
 trace('cmdstan_model',tracer=quote(stop('FORBIDDEN: real CmdStan model construction in R-only qualification')),where=asNamespace('cmdstanr'),print=FALSE)
})
