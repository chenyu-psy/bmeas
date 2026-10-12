# Run from package root; writes outputs outside the distributable source tree.
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) normalizePath(args[1L]) else normalizePath(".")
for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
out <- if (length(args) > 1L) args[2L] else file.path(dirname(root), "bhmm-validation")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
spec <- demo_measurement_spec()
run <- run_config(chains = 1, parallel_chains = 1, threads_per_chain = 1,
  iter_warmup = 20, iter_sampling = 20, seed = 20261003, cpu_budget = 1,
  max_observations = 48, max_subjects = 8, max_saved_draws = 20)
print(execution_plan(spec, run))
fit <- fit_measurement(spec, run, cache_dir = file.path(out, "compile"), output_dir = file.path(out, "sampling"))
stopifnot(length(fit$backend$return_codes()) == 1L, all(fit$backend$return_codes() == 0L))
stopifnot(fit$execution_plan$saved_draws == 20L, nrow(parameter_summary(fit)) > 0L,
          fit$cache$computation$status != "failed", fit$cache$predictive_fit$status == "not_assessed")
# Summary must still work with an intentionally unusable backend object.
original_summary <- capture.output(print(summary(fit)))
cached <- fit; cached$backend <- NULL
stopifnot(identical(original_summary, capture.output(print(summary(cached)))))
writeLines(original_summary, file.path(out, "smoke-summary.txt"))
utils::write.csv(parameter_summary(fit), file.path(out, "parameter-summary.csv"), row.names = FALSE)
saveRDS(fit, file.path(out, "smoke-fit.rds"))
cat(paste(original_summary, collapse = "\n"), "\n")
cat("PASS: Gaussian CFA compiler, sampler, cached diagnostics and backend-free summary\n")
