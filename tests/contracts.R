source(file.path("helpers", "native-guard.R"))
# Dependency-free unit/contract tests. No compiler, sampler, PPC or LOO is invoked.
if (Sys.getenv("MEASUREMENTPKG_SOURCE") != "") {
  for (f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"), "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
} else library(bmeas)
passed <- 0L
ok <- function(value, label) {
  stopifnot(isTRUE(value)); passed <<- passed + 1L; cat("PASS:", label, "\n")
}
err <- function(expr, code) {
  result <- tryCatch({ force(expr); NULL }, error = identity)
  ok(inherits(result, "measurement_error") && identical(result$code, code), code)
}
d <- demo_gaussian_cfa(); spec <- demo_measurement_spec(d)
ok(nrow(d) == 48L && length(unique(d$person)) == 8L, "tiny wholly synthetic fixture")
set.seed(1); before <- .Random.seed; invisible(demo_gaussian_cfa()); ok(identical(before, .Random.seed), "fixture preserves RNG state")
ok(all(vapply(list(gaussian_model(), binomial_model(), lognormal_model()), inherits, logical(1), "observation_model")), "one observation_model class for basic families")
ok(identical(spec$checks$identification$status, "not_assessed"), "static checks are not identification evidence")
ok(nrow(prior_table(spec)) == 20L && all(nzchar(prior_table(spec)$source)), "all priors explicit and visible")
ok(identical(stan_data(spec), stan_data(demo_measurement_spec(d[nrow(d):1L, ]))), "row order does not change Stan data")
d_factor <- d; d_factor$person <- factor(d_factor$person, levels = rev(unique(d_factor$person)))
ok(identical(stan_data(spec), stan_data(demo_measurement_spec(d_factor))), "factor IDs map labels, not integer codes")
err(observe(gaussian_model(), d, "person", "item", "value_z"), "E_UNITS")
d_bad <- d; d_bad$person[1] <- NA
err(demo_measurement_spec(d_bad), "E_KEY")
d_bad <- rbind(d, d[1, ])
err(demo_measurement_spec(d_bad), "E_OBSERVATION_KEY")
d_bad$obs <- c(as.character(seq_len(nrow(d))), "1")
err(observe(gaussian_model(), d_bad, "person", "item", "value_z", units = "z", observation_id = "obs"), "E_DUPLICATE_OBSERVATION")
d_bad <- d; d_bad$value_z[1] <- Inf
err(demo_measurement_spec(d_bad), "E_FIELD_SUPPORT")
d_bad <- d; d_bad$value_z[1] <- NA
err(demo_measurement_spec(d_bad), "E_MISSING_RESPONSE")
missing_block <- observe(gaussian_model(), d_bad, "person", "item", "value_z", units = "externally_standardized")
missing_spec <- measurement(list(scores = missing_block), shared_subjects("demo"), spec$structure,
                            spec$priors, missing_policy(response = "observed_only"))
ok(stan_data(missing_spec)$O == 47L && nrow(missing_spec$data_map$excluded) == 1L, "explicit observed_only records dropped response")
err(cfa_structure(list(A = targets("scores", "mu", c("v1","v2","v3"))), nuisance = task_parameters("sigma")), "E_FACTOR_ANCHOR")
err(cfa_structure(list(A = targets("scores", "mu", c("v1","v2"))), positive_anchors = c(A = "scores::mu::v1"), nuisance = task_parameters("sigma")), "E_FACTOR_IDENTIFICATION")
err(cfa_structure(spec$structure$factors, positive_anchors = spec$structure$positive_anchors, nuisance = task_parameters("sigma"), parameter_residual = "free"), "E_DOUBLE_RESIDUAL")
factors_bad <- spec$structure$factors; factors_bad$Spatial[3] <- factors_bad$Verbal[2]
err(cfa_structure(factors_bad, positive_anchors = spec$structure$positive_anchors, nuisance = task_parameters("sigma")), "E_CROSS_LOADING")
err(measurement(list(scores = observe(gaussian_model(), d, "person", "item", "value_z", units = "seconds")), shared_subjects("demo"), spec$structure, spec$priors), "E_PRIOR_UNITS")
err(prior_preset("guess_priors"), "E_PRIOR_PRESET")
err(prior_set(normal(0,1), normal(1,1), half_normal(1), lkj(2)), "E_PRIOR")
# All blocks share global person identity only through explicit subjects declaration.
v <- d[d$item %in% c("v1","v2","v3") & d$person != "synthetic_01", ]
s <- d[d$item %in% c("s1","s2","s3") & d$person != "synthetic_02", ]
make_joint <- function(v, s, subjects = shared_subjects("same_people"), source = NULL) {
  blocks <- list(verbal = observe(gaussian_model(), v, "person", "item", "value_z", units = "externally_standardized", source_id = source),
                 spatial = observe(gaussian_model(), s, "person", "item", "value_z", units = "externally_standardized", source_id = source))
  a <- targets("verbal", "mu", c("v1","v2","v3")); b <- targets("spatial", "mu", c("s1","s2","s3"))
  st <- cfa_structure(list(A = a, B = b), positive_anchors = c(A = a[1], B = b[1]), nuisance = task_parameters("sigma"))
  measurement(blocks, subjects, st, prior_preset("gaussian_cfa_standardized_v1"))
}
joint <- make_joint(v,s)
ok(length(joint$data_map$subject_keys) == 8L && joint$data_map$overlap[1,2] == 6L, "subject union and overlap are explicit")
ok(length(unique(joint$parameter_keys$key)) == 12L, "block names namespace parameter keys")
s_local <- s; s_local$person <- sub("synthetic", "different_local", s_local$person)
map <- rbind(data.frame(model = "verbal", local_id = unique(v$person), person_id = unique(v$person)),
             data.frame(model = "spatial", local_id = unique(s_local$person), person_id = unique(s$person)))
walk <- subject_crosswalk(map, namespace = "same_people")
walk_spec <- make_joint(v, s_local, walk)
ok(identical(stan_data(joint), stan_data(walk_spec)), "different local IDs join only through crosswalk")
err(subject_crosswalk(rbind(map, map[1, ])), "E_CROSSWALK")
err(make_joint(v, s_local), "E_DISCONNECTED_CORRELATION")
err(make_joint(v, s_local, subject_crosswalk(map[-1, ])), "E_CROSSWALK")
# Irregular repeated observations are retained with unique observation IDs.
ragged <- rbind(d, d[1:2,]); ragged$obs <- paste0("obs",seq_len(nrow(ragged)))
rb <- observe(gaussian_model(), ragged, "person", "item", "value_z", units = "externally_standardized", observation_id = "obs")
rs <- measurement(list(scores = rb), shared_subjects("demo"), spec$structure, spec$priors)
ok(stan_data(rs)$O == 50L, "ragged repetitions are not aggregated, dropped, or padded")
counts <- data.frame(person = c("a","b"), task = "t", successes = c(0,2), trials = c(0,3))
err(observe(binomial_model(), counts, "person", "task", fields = list(successes = "successes"), units = "count"), "E_TRIALS_MISSING")
ok(inherits(observe(binomial_model(), counts, "person", "task", fields = list(successes = "successes", trials = "trials"), units = "count"), "observation_block"), "binomial zero-trial data accepted as zero information")
counts$successes[2] <- 4
err(observe(binomial_model(), counts, "person", "task", fields = list(successes = "successes", trials = "trials"), units = "count"), "E_BINOMIAL_SUPPORT")
counts$successes[2] <- .5
err(observe(binomial_model(), counts, "person", "task", fields = list(successes = "successes", trials = "trials"), units = "count"), "E_FIELD_SUPPORT")
err(observe(lognormal_model(), transform(d, value_z = 0), "person", "item", "value_z", units = "seconds"), "E_FIELD_SUPPORT")
counts$successes[2] <- 1; counts$trials[1] <- NA
err(observe(binomial_model(), counts, "person", "task", fields = list(successes = "successes", trials = "trials"), units = "count"), "E_MISSING_AUXILIARY")
# Typed custom contracts are real declarations, but P0 does not promise a generic compiler.
f <- tempfile(fileext = ".stanfunctions")
writeLines("real custom_lpmf(int y, real eta) { return bernoulli_logit_lpmf(y | eta); }", f)
custom <- new_observation_model("custom", "0.1", response_schema(y = "binary"),
  list(eta = parameter_spec("real", "identity", "eta", "subject_task", "logit", "Known contract parameter")),
  stan_density(f, "custom_lpmf", c("y","eta"), c("int","real"), TRUE, "none"), model_capabilities(fit = TRUE))
ok(inherits(custom, "observation_model") && custom$likelihood$license == "unknown" && nzchar(custom$likelihood$resource_hash), "custom contract preserves unknown license and hashes source")
ok(!custom$capabilities$available[custom$capabilities$capability == "simulate"], "likelihood-only does not imply simulation")
err(m3_model(), "E_RESOURCE"); err(sdm_model(), "E_MODEL_NOT_IMPLEMENTED")
err(csmu_gpc(), "E_PARAMETER_DOMAIN")
err(assess_measurement_fit(NULL), "E_FIT")
run <- run_config(1,1,1,20,20,20261003,1,48,8,20)
ok(execution_plan(spec, run)$saved_draws == 20, "run plan separates explicit budgets")
err(run_config(1,1,2,20,20,1,2,48,8,20), "E_THREADING_UNSUPPORTED")
err(run_config(2,2,1,20,20,1,1,48,8,40), "E_RUN_BUDGET")
err(run_config(1,1,1,1000,1000,1,1,48,8,1000), "E_SMOKE_BUDGET")
err(execution_plan(rs, run), "E_RUN_BUDGET")
code <- as.character(make_stan_code(spec))
ok(grepl("tau_mu = 0",code,fixed=TRUE) && !grepl("vector[O] log_lik",code,fixed=TRUE) && !grepl("y_rep[",code,fixed=TRUE), "no double residual or default prediction/log-likelihood arrays")
ok(grepl("transpose(Lambda)", code,fixed=TRUE) && nrow(stan_symbol_map(spec)) == 6L, "inspectable covariance and key-to-symbol mapping")
# A pure synthetic cache fixture checks summary without a backend or simulation.
status <- function(s, r) list(status=s,reason=r)
cache <- list(computation=status("warning","Synthetic printing fixture only"), parameter_summary=NULL, hmc=NULL,
 inference=status("not_assessed","SMOKE ONLY"), predictive_fit=status("unsupported","No predictor registered"),
 identification=status("not_assessed","No identification evidence"), admissibility=status("failed","Synthetic failure fixture"),
 prior_sensitivity=status("not_assessed","Not run"), conventional_fit=status("not_assessed","Not run"))
fixture_fit <- structure(list(cache=cache,execution_plan=execution_plan(spec,run),spec=spec),class="measurement_fit")
text <- capture.output(print(summary(fixture_fit)))
ok(any(grepl("SMOKE ONLY",text)) && any(grepl("unsupported",text)) && any(grepl("failed",text)), "summary preserves distinct statuses and never needs a backend")
bad_spec <- spec; bad_spec$structure$positive_anchors["Verbal"] <- "scores::mu::s1"
err(check_measurement(bad_spec), "E_FACTOR_ANCHOR")
bad_spec <- spec; bad_spec$priors$sigma$sd <- -1
err(check_measurement(bad_spec), "E_PRIOR")
err(observe(gaussian_model(),d,"person","item",fields=list(y="value_z",y="value_z"),units="z"), "E_NAMES")
err(subject_crosswalk(map, namespace=c("a","b")), "E_ARGUMENT")
bad_custom <- tryCatch(new_observation_model("bad","1",response_schema(.task="real"), list(eta=parameter_spec("real","identity","eta","subject_task","z","description")),stan_density(symbol="normal_lpdf",arguments=c(".task","eta"),argument_types=c("real","real"),normalized=TRUE,approximation="none",builtin=TRUE)), error=identity)
ok(inherits(bad_custom,"measurement_error") && bad_custom$code == "E_SCHEMA_NAME", "reserved internal field names rejected")
internal_extreme <- if (exists(".mp_extreme")) get(".mp_extreme") else getFromNamespace(".mp_extreme","bmeas")
ok(is.infinite(internal_extreme(c(Inf,1.01),"max")), "infinite Rhat remains visible")
bad_run <- run; bad_run$iter_sampling <- 10000
err(execution_plan(spec,bad_run), "E_SMOKE_BUDGET")
bad_spec <- spec; bad_spec$models$scores$model$capabilities$available[bad_spec$models$scores$model$capabilities$capability == "fit"] <- FALSE
err(check_measurement(bad_spec), "E_CAPABILITY")
one_person <- demo_measurement_spec(d[d$person == d$person[1L],])
ok(stan_data(one_person)$N == 1L, "one-person mapping stays a matrix; identification remains unassessed")
err(make_joint(v,v,source="same_origin"), "E_DOUBLE_USE")
sync_source <- if (exists(".mp_sync_stan_source")) get(".mp_sync_stan_source") else getFromNamespace(".mp_sync_stan_source","bmeas")
cache_file <- tempfile(fileext=".stan")
ok(sync_source(code,cache_file) && !sync_source(code,cache_file), "compilation cache reuses identical source")
writeLines("altered model",cache_file)
ok(sync_source(code,cache_file) && identical(paste(readLines(cache_file),collapse="\n"),code), "tampered cached Stan source is restored and recompilation required")
disconnected <- d
select <- (disconnected$item %in% c("v1","v2","v3") & disconnected$person %in% unique(d$person)[1:4]) | (disconnected$item %in% c("s1","s2","s3") & disconnected$person %in% unique(d$person)[5:8])
disconnected <- disconnected[select,]
b <- observe(gaussian_model(),disconnected,"person","item","value_z",units="externally_standardized")
t <- targets("scores","mu",c("v1","v2","v3","s1","s2","s3"))
st <- cfa_structure(list(A=t),positive_anchors=c(A=t[1]),nuisance=task_parameters("sigma"))
err(measurement(list(scores=b),shared_subjects("demo"),st,spec$priors), "E_DISCONNECTED_INDICATOR")
cache_fit <- if (exists(".mp_cache_fit")) get(".mp_cache_fit") else getFromNamespace(".mp_cache_fit","bmeas")
# Test double only; no mock result is returned by application fitting functions.
mock_backend <- list(summary=function(...) data.frame(variable=paste0("sigma[",1:6,"]"),mean=1),
 diagnostic_summary=function(...) list(num_divergent=0,num_max_treedepth=0,ebfmi=1))
sigma_cache <- cache_fit(mock_backend,spec,run)$parameter_summary
expected_sigma <- prior_table(spec)$parameter[grepl("^sigma:",prior_table(spec)$parameter)]
ok(setequal(sigma_cache$parameter_key,expected_sigma), "cached sigma labels match sigma prior/parameter keys")
cat("TOTAL",passed,"passed; no sampling performed\n")
