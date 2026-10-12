source(file.path("helpers", "native-guard.R"))
# Invented joint eta states exercise extraction and reporting only.
# No posterior-origin, recovery or usable-inference evidence is claimed.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(source_root)
if (source_mode) {
  for (file in list.files(file.path(source_root, "R"), full.names = TRUE)) source(file)
} else library(bmeas)
private <- function(name) if (source_mode) get(name, .GlobalEnv) else getFromNamespace(name, "bmeas")
plan_fun <- private(".bh_individual_plan")
passed <- 0L
ok <- function(x, label) {
  if (!isTRUE(x)) stop(label)
  passed <<- passed + 1L
  cat("PASS", label, "\n")
}
err <- function(expr, code) {
  e <- tryCatch(force(expr), error = identity)
  ok(inherits(e, "measurement_error") && identical(e$code, code), paste("refusal", code))
  invisible(e)
}
budget <- list(max_source_bytes = 10485760, max_posterior_rows = 100,
               max_draw_values = 50000, max_parameter_cells = 1000)
scales <- c("predictor", "natural", "received")
people <- c("person:2", "person:1", "unobserved")
tasks <- c("one", "two")
d <- expand.grid(person = people[1:2], task = tasks, replicate = 1:2,
                 stringsAsFactors = FALSE)
d$y <- seq_len(nrow(d)) / 10
d$obs <- paste0("r", seq_len(nrow(d)))
obs <- observe(gaussian_model(), d, "person", "task", "y", units = "score", observation_id = "obs")
spec <- bmeas(bmf(G = ~score$mu[tasks],
  loadings = list(fixed = list(list(value = 1, cells = list(G = ~score$mu[tasks])))),
  residual = list(targets = ~score$mu[tasks]),
  person = list(distribution = "sample_centered", ids = people),
  augmentation = list(type = "task_cholesky", targets = ~score$mu[c("two", "one")])),
  data = list(score = obs), fit = FALSE)
make_fit <- function(spec, paths = character()) structure(list(spec = spec,
  code = as.character(stancode(spec)), backend = list(draws = function(...) stop("backend trap"),
    metadata = function(...) stop("metadata trap")),
  cache = list(parameter_summary = data.frame(variable = "alpha[1]", mean = .2, sd = .1),
    computation = list(status = "engineering_fixture"),
    inference = list(status = "not_assessed"), identification = list(status = "not_assessed"),
    posterior_propriety = list(status = "not_assessed"),
    posterior_normalizability = list(status = "not_assessed")),
  provenance = list(kind = "synthetic_joint_states_engineering_only_not_posterior_evidence",
    model_fingerprint = spec$fingerprint, output_files = paths)), class = c("bmeas_fit", "bhmm_fit"))
fit <- make_fit(spec)
err(individual_parameter_summary(fit), "E_INDIVIDUAL_OPTIONS")
err(individual_parameter_summary(fit, scales = scales), "E_INDIVIDUAL_OPTIONS")
plan <- plan_fun(fit, scales, budget)
ok(nrow(plan$cells) == 6L && length(plan$required_variables) == 6L,
   "full centered roster and both logical targets determine the source domain")
ok(sum(plan$cells$person_id == "unobserved") == 2L &&
     all(plan$cells$n_likelihood_rows[plan$cells$person_id == "unobserved"] == 0L),
   "unobserved roster members remain with qualified zero local coverage")
local({
  had_method <- exists("Ops.Date", .GlobalEnv, inherits = FALSE)
  previous <- if (had_method) get("Ops.Date", .GlobalEnv) else NULL
  on.exit(if (had_method) assign("Ops.Date", previous, .GlobalEnv) else rm("Ops.Date", envir = .GlobalEnv), add = TRUE)
  calls <- 0L
  assign("Ops.Date", function(e1, e2) { calls <<- calls + 1L; NextMethod() }, .GlobalEnv)
  bad <- fit
  bad$spec$tables$augmentation$raw_coordinate_variance <- structure(
    bad$spec$tables$augmentation$raw_coordinate_variance, class = "Date")
  err(individual_parameter_summary(bad, scales = scales, budget = budget), "E_INDIVIDUAL_LAYOUT")
  ok(calls == 0L, "centered variance annotation refuses before arithmetic callbacks")
})

# Historical backend double tests only the unchanged one-argument contract.
legacy_calls <- 0L
legacy_values <- matrix(c(1, 3, 2, 4), 2L, 2L,
  dimnames = list(NULL, c("eta[1,1]", "eta[2,1]")))
legacy <- structure(list(spec = list(structure = list(type = "component_covariance",
  keys = "old::mu::task"), models = list(old = list(model = gaussian_model())),
  data_map = list(subject_keys = c("old1", "old2"), blocks = list(old = data.frame(
    .person_key = c("old1", "old2"), .task = "task", .observation_id = c("oldr1", "oldr2"))))),
  backend = list(draws = function(variable, format) {
    stopifnot(identical(variable, "eta"), identical(format, "draws_matrix"))
    legacy_calls <<- legacy_calls + 1L
    legacy_values
  })), class = "measurement_fit")
legacy_result <- individual_parameter_summary(legacy)
ok(legacy_calls == 1L && identical(legacy_result$mean, c(2, 3)) &&
     all(legacy_result$sd == sqrt(2)) && all(legacy_result$n_likelihood_rows == 1L) &&
     all(legacy_result$scale == "predictor") && all(legacy_result$saved_draws == 2L),
   "legacy one-argument helper retains its original draw/table behavior")
err(individual_parameter_summary(legacy, scales = "natural"), "E_INDIVIDUAL_OPTIONS")
err(individual_parameter_summary(legacy, budget = budget), "E_INDIVIDUAL_OPTIONS")
ok(legacy_calls == 1L, "new options on the legacy helper refuse before its backend")

if (requireNamespace("cmdstanr", quietly = TRUE) &&
    identical(as.character(utils::packageVersion("cmdstanr")), "0.9.0")) {
  root <- tempfile("bmeas-individual-test-")
  dir.create(root)
  write_states <- function(label, p, disagreement = FALSE) {
    vars <- p$required_variables
    cols <- gsub("\\]", "", gsub("[\\[,]", ".", vars))
    vapply(c(7L, 2L), function(id) {
      path <- file.path(root, paste0(label, "-", id, ".csv"))
      values <- matrix(0, 2L, length(vars), dimnames = list(NULL, vars))
      for (j in seq_along(vars)) {
        row <- p$cells[match(vars[j], p$cells$source_variable), , drop = FALSE]
        n <- if (is.na(row$shared_alias_key)) row$person_index else 0L
        t <- match(row$parameter_key, unique(p$cells$parameter_key))
        values[, j] <- id / 10 + seq_len(2L) / 100 + n / 1000 + t / 10000
      }
      if (disagreement && length(vars)) values[1L, 1L] <- values[1L, 1L] + .01
      lines <- c("# ENGINEERING FIXTURE: invented eta states, not posterior evidence",
        "# stan_version_major = 2", "# stan_version_minor = 35", "# stan_version_patch = 0",
        "# model = scalar_components_model", paste("# id =", id), "# method = sample",
        "# num_samples = 2", "# num_warmup = 0", "# save_warmup = 0", "# thin = 1",
        "# algorithm = hmc", "# engine = nuts", "# metric = diag_e", "# sig_figs = 18",
        paste(c("lp__", "accept_stat__", cols), collapse = ","),
        vapply(seq_len(2L), function(r) paste(c(-1, .9,
          format(values[r, ], digits = 17, scientific = TRUE, trim = TRUE)), collapse = ","), ""),
        "# Elapsed Time: 0 seconds (Warm-up)", "# 0 seconds (Sampling)", "# 0 seconds (Total)")
      writeLines(lines, path, useBytes = TRUE)
      path
    }, "")
  }
  fit$provenance$output_files <- write_states("centered", plan)
  before <- serialize(fit, NULL); set.seed(20261006); seed <- .Random.seed; cwd <- getwd()
  out <- individual_parameter_summary(fit, scales = scales, budget = budget)
  ok(inherits(out, "bmeas_individual_parameter_summary") && nrow(out) == 18L,
     "public result contains every person-parameter-scale row")
  expected <- .45 + .015 + out$person_index / 1000 +
    match(out$parameter_key, unique(out$parameter_key)) / 10000
  ok(max(abs(out$mean - expected)) < 1e-14 && all(out$n_draws == 4L),
     "all retained chain states have the independent expected eta means")
  ok(all(out$status == "available") && all(is.na(out$rhat)) &&
       all(out$diagnostics_status == "unavailable"),
     "numerical summaries do not invent eta convergence diagnostics")
  ok(identical(before, serialize(fit, NULL)) && identical(seed, .Random.seed) && identical(cwd, getwd()),
     "public acquisition preserves fit bytes RNG and cwd")
  ok(identical(attr(out, "draw_keys"), data.frame(chain_id = c(2L, 7L, 2L, 7L), saved_row = c(1L, 1L, 2L, 2L))),
     "all-row keys use saved-row rank then numeric chain ID")
  ok(identical(attr(out, "metadata")$source_qualification, fit$provenance$kind) &&
       !attr(out, "source_receipt")$original_fit_hashes_present,
     "synthetic qualification and missing original source authentication remain explicit")
  ok(identical(attr(out, "metadata")$upstream$inference, fit$cache$inference),
     "upstream inference status is unchanged")
  empty_seed <- .Random.seed; rm(.Random.seed, envir = .GlobalEnv)
  invisible(individual_parameter_summary(fit, scales = "predictor", budget = budget))
  ok(!exists(".Random.seed", .GlobalEnv, inherits = FALSE), "extraction creates no absent RNG seed")
  assign(".Random.seed", empty_seed, envir = .GlobalEnv)
  output_path <- file.path(root, "result.rds"); saveRDS(out, output_path)
  reread <- readRDS(output_path); captured <- capture.output(printed <- print(reread))
  ok(identical(out, reread) && identical(printed, reread) &&
       any(grepl("synthetic_joint_states", captured)) && identical(seed, .Random.seed),
     "saved result retains provenance and prints passively")
  local({
    calls <- 0L
    for (field in c("metadata", "work")) {
      bad <- out; record <- new.env(parent = emptyenv())
      makeActiveBinding(if (field == "metadata") "status" else "logical_parameter_cells",
        function(value) { calls <<- calls + 1L; stop("result metadata callback") }, record)
      attr(bad, field) <- record
      err(print(bad), "E_INDIVIDUAL_RESULT")
    }
    bad <- out; bad$mean <- structure(bad$mean, class = "malformed_saved_numeric")
    err(print(bad), "E_INDIVIDUAL_RESULT")
    bad <- out; work <- attr(bad, "work"); attr(bad, "work") <- c(work, work[1L])
    err(print(bad), "E_INDIVIDUAL_RESULT")
    ok(calls == 0L, "passive result guards reject malformed fields without callbacks")
  })
  shared_obs <- observe(gaussian_model(), d, "person", "task", "y", units = "score",
    observation_id = "obs", parameters = list(mu = "task"))
  shared_spec <- bmeas(bmf(), data = list(score = shared_obs), fit = FALSE)
  shared_fit <- make_fit(shared_spec); shared_plan <- plan_fun(shared_fit, scales, budget)
  shared_fit$provenance$output_files <- write_states("shared", shared_plan)
  shared_out <- individual_parameter_summary(shared_fit, scales = scales, budget = budget)
  ok(all(shared_out$effective_role == "shared_task_predictor") && all(shared_out$sd > 0),
     "task means are uncertain shared aliases rather than fixed zero values")
  shared_fit$provenance$output_files <- write_states("shared-bad", shared_plan, TRUE)
  err(individual_parameter_summary(shared_fit, scales = scales, budget = budget), "E_INDIVIDUAL_SOURCE")
  fixed_obs <- observe(gaussian_model(), d, "person", "task", "y", units = "score",
    observation_id = "obs", parameters = list(mu = list(fixed = c(one = .4, two = .7))))
  fixed_spec <- bmeas(bmf(), data = list(score = fixed_obs), fit = FALSE)
  fixed_fit <- make_fit(fixed_spec); fixed_plan <- plan_fun(fixed_fit, scales, budget)
  fixed_fit$provenance$output_files <- write_states("fixed", fixed_plan)
  zero_budget <- budget; zero_budget$max_draw_values <- 0
  fixed_out <- individual_parameter_summary(fixed_fit, scales = scales, budget = zero_budget)
  ok(all(fixed_out$estimate_kind == "known_input") && all(fixed_out$sd == 0) &&
       all(fixed_out$n_sampled_values == 0L) && !attr(fixed_out, "source_receipt")$reader$called,
     "fixed-only predictor values retain source rows without reading a dummy variable")
  bad <- fit; bad$provenance$output_files <- file.path(root, "absent.csv")
  e <- tryCatch(individual_parameter_summary(bad, scales = scales, budget = budget), error = identity)
  ok(inherits(e, "bh_ppc_source_error") && identical(before, serialize(fit, NULL)) && identical(seed, .Random.seed),
     "missing-source error does not mutate original fit or caller RNG")
  unlink(root, recursive = TRUE)
} else cat("SKIP: qualified CmdStanR 0.9.0 reader unavailable\n")
cat("frontend-individual:", passed, "checks passed\n")
