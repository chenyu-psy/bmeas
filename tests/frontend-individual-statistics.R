source(file.path("helpers", "native-guard.R"))
# Tiny deterministic engineering states only: no fitted posterior evidence,
# simulation, source-file acquisition, model/native execution, or HMC.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if (nzchar(source_root)) {
  source(file.path(source_root, "R", "errors.R"))
  source(file.path(source_root, "R", "frontend-individual-statistics.R"))
} else {
  library(bmeas)
  .bh_individual_summarize <- getFromNamespace(".bh_individual_summarize", "bmeas")
}
passed <- 0L
ok <- function(value, label) {
  if (!isTRUE(value)) stop(label)
  passed <<- passed + 1L
  cat("PASS", label, "\n")
}
eq <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE, tolerance = 1e-12))
err <- function(expr, code, label) {
  caught <- tryCatch(force(expr), error = identity)
  ok(inherits(caught, "measurement_error") && identical(caught$code, code), label)
}
cell <- function(n = 1L, predictor = "log", receives = "eta", support = "positive") {
  data.frame(person_key = paste0("person/", seq_len(n)), person_id = paste0("id ", seq_len(n)),
    person_index = seq_len(n), parameter_key = rep("opaque/target:key", n),
    model = rep("cs", n), parameter = rep("a", n), task = rep("opaque/task", n),
    declared_level = rep("subject_task", n), effective_role = rep("person_specific_predictor", n),
    shared_alias_key = rep(NA_character_, n), source_variable = paste0("eta[", seq_len(n), ",1]"),
    predictor = rep(predictor, n), receives = rep(receives, n), support = rep(support, n),
    units = rep("activation", n), predictor_units = rep("log activation", n),
    received_units = rep(if (receives == "eta") "log activation" else "activation", n),
    fixed_received = rep(NA_real_, n), fixed_predictor = rep(NA_real_, n), fixed_natural = rep(NA_real_, n),
    predictor_representation = rep("free", n), natural_representation = rep("free", n),
    n_likelihood_rows = rep(0L, n), n_informative_rows = rep(0L, n),
    coverage_qualification = rep("engineering state; not identification evidence", n),
    stringsAsFactors = FALSE)
}
plan <- function(cells = cell(), scales = c("predictor", "natural", "received")) {
  list(cells = cells, scales = scales, budget = list(max_source_bytes = 10485760L,
    max_posterior_rows = 100L, max_draw_values = 50000L, max_parameter_cells = 1000L))
}
acquire <- function(values, cells) {
  values <- as.matrix(values)
  colnames(values) <- unique(cells$source_variable[!is.na(cells$source_variable)])
  list(draws = values, draw_keys = data.frame(chain_id = rep(7L, nrow(values)),
    saved_row = seq_len(nrow(values))), receipt = list(status = "engineering_fixture"))
}
fixed_cell <- function(value, predictor = "logit", receives = "theta", support = "unit_interval") {
  c <- cell(predictor = predictor, receives = receives, support = support)
  c$declared_level <- "fixed"
  c$effective_role <- "fixed_by_declaration"
  c$source_variable <- NA_character_
  c$fixed_natural <- c$fixed_received <- value
  c$fixed_predictor <- if (predictor == "logit") qlogis(value) else value
  c$predictor_representation <- if (is.finite(c$fixed_predictor)) "available" else "no_finite_predictor"
  if (!is.finite(c$fixed_predictor)) c$fixed_predictor <- NA_real_
  c$natural_representation <- "supplied_exact"
  c
}
row <- function(x, scale) x[x$scale == scale, , drop = FALSE]

rng_present <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
rng_before <- if (rng_present) get(".Random.seed", .GlobalEnv) else NULL
cwd_before <- getwd()
p <- plan()
a <- acquire(matrix(log(c(1, 9)), ncol = 1L), p$cells)
p_before <- serialize(p, NULL)
a_before <- serialize(a, NULL)
o <- .bh_individual_summarize(p, a)
ok(eq(row(o, "natural")$mean, 5) && !eq(row(o, "natural")$mean, exp(row(o, "predictor")$mean)),
   "nonlinear pushforward mean is 5, not exp(mean(log(1,9))) = 3")
ok(eq(row(o, "natural")$lower90, 1.4) && eq(row(o, "natural")$upper90, 8.6) &&
   !eq(row(o, "natural")$lower90, exp(quantile(log(c(1, 9)), .05, type = 7))),
   "type-7 quantile interpolation occurs after transforming every state")
ok(eq(row(o, "natural")$sd, sd(c(1, 9))) && all(o$n_draws == 2L) && all(o$quantile_type == 7L),
   "sample SD, original retained denominator and empirical quantile convention")
ok(eq(row(o, "received")$mean, row(o, "predictor")$mean) &&
   identical(o$summary_units, c("log activation", "activation", "log activation")),
   "CS receives eta with scale-specific units")
ok(all(o$status == "available") && all(o$scale_status == "available") &&
   all(o$estimate_kind == "retained_state_summary") && all(is.na(o$known_value)),
   "sampled summaries retain availability and nonconstant provenance separately")
ok(all(o$diagnostics_status == "unavailable") && all(is.na(o$ess_bulk)) && all(is.na(o$rhat)) &&
   all(is.na(o$mcse_mean)), "no ESS, Rhat or MCSE is invented or borrowed across scales")
ok(identical(serialize(p, NULL), p_before) && identical(serialize(a, NULL), a_before),
   "summary operation leaves plan and acquired joint-state bytes unchanged")

theta <- plan(cell(receives = "theta"), scales = c("received", "natural", "predictor"))
t <- .bh_individual_summarize(theta, acquire(matrix(log(c(1, 9)), ncol = 1L), theta$cells))
ok(eq(t$mean[1:2], c(5, 5)) && identical(t$scale, theta$scales),
   "theta-received view uses the declared per-draw inverse map and requested scale order")

# Same alpha and deviation marginals; pairing is part of eta's joint state.
joint <- plan(cell(predictor = "identity", support = "real"), "predictor")
alpha <- c(0, 1)
deviation <- c(0, 1)
j1 <- .bh_individual_summarize(joint, acquire(matrix(alpha + deviation, ncol = 1L), joint$cells))
j2 <- .bh_individual_summarize(joint, acquire(matrix(alpha + rev(deviation), ncol = 1L), joint$cells))
ok(eq(j1$mean, j2$mean) && eq(j1$sd, sqrt(2)) && identical(j2$sd, 0),
   "equal alpha/deviation margins cannot replace eta joint dependence")

for (endpoint in c(0, 1)) {
  fixed <- plan(fixed_cell(endpoint))
  z <- .bh_individual_summarize(fixed, acquire(matrix(numeric(), nrow = 4L, ncol = 0L), fixed$cells))
  ok(row(z, "predictor")$scale_status == "unavailable" &&
     row(z, "predictor")$scale_reason == "fixed_no_finite_predictor" &&
     all(z$mean[z$scale != "predictor"] == endpoint) &&
     all(z$sd[z$scale != "predictor"] == 0),
     paste("saved theta endpoint", endpoint, "is known while its infinite predictor is unavailable"))
  ok(all(z$n_draws == 4L) && all(z$n_sampled_values == 0L) && all(z$n_invalid_states == 0L) &&
     all(z$estimate_kind == "known_input") && all(z$uncertainty_status == "fixed_by_declaration_no_estimation"),
     paste("fixed endpoint", endpoint, "keeps source-row receipt separate from zero sampled values"))
}

sat <- fixed_cell(.5, receives = "eta")
sat$fixed_received <- sat$fixed_predictor <- 1000
sat$fixed_natural <- 1
sat$predictor_representation <- "supplied_exact"
sat$natural_representation <- "optional_transform_not_representable"
s <- .bh_individual_summarize(plan(sat), acquire(matrix(numeric(), nrow = 2L, ncol = 0L), sat))
ok(row(s, "natural")$scale_status == "unavailable" && is.na(row(s, "natural")$known_value) &&
   row(s, "natural")$n_invalid_fixed_values == 1L && all(s$mean[s$scale != "natural"] == 1000),
   "finite saturated fixed natural value is rejected by authoritative saved representation flag")

extreme <- plan(cell(2L))
extreme$cells$predictor[2L] <- "identity"
extreme$cells$support[2L] <- "real"
v <- cbind(c(-1000, 0, 1000), c(3, 4, 5))
e <- .bh_individual_summarize(extreme, acquire(v, extreme$cells))
bad <- e[e$person_index == 1L & e$scale == "natural", ]
ok(bad$scale_status == "unavailable" && bad$n_invalid_transformed_states == 2L &&
   bad$n_draws == 3L && bad$n_sampled_values == 3L &&
   identical(bad$scale_reason, "exp_overflow;exp_underflow") &&
   all(is.na(unlist(bad[c("mean", "median", "sd", "lower90", "upper90")]))),
   "exp overflow and underflow invalidate the whole cell-scale, without finite-subset summaries")
ok(all(e$status[e$person_index == 2L] == "available") &&
   all(e$status[e$person_index == 1L & e$scale != "natural"] == "available"),
   "transform failures remain local to the affected cell and scale")

logit <- plan(cell(predictor = "logit", support = "unit_interval"))
l <- .bh_individual_summarize(logit, acquire(matrix(c(0, 1000), ncol = 1L), logit$cells))
ok(row(l, "natural")$scale_reason == "inverse_logit_saturation" &&
   row(l, "natural")$n_invalid_states == 1L && is.na(row(l, "natural")$mean) &&
   row(l, "predictor")$status == "available", "inverse-logit saturation refuses endpoint substitution")
lv <- .bh_individual_summarize(logit, acquire(matrix(c(-2, 2), ncol = 1L), logit$cells))
ok(eq(row(lv, "natural")$mean, .5) && eq(row(lv, "natural")$sd, sd(plogis(c(-2, 2)))) &&
   eq(row(lv, "natural")$lower90, quantile(plogis(c(-2, 2)), .05, type = 7)),
   "finite logit natural summaries match a direct transformed-state oracle")

huge <- .bh_individual_summarize(joint, acquire(matrix(c(-1e308, 1e308), ncol = 1L), joint$cells))
ok(huge$scale_status == "available" && huge$status == "partially_available" &&
   huge$sd_status == "unavailable" && huge$sd_reason == "sd_arithmetic_not_finite" &&
   is.na(huge$sd) && huge$mean == 0 && huge$median == 0 && is.finite(huge$lower90) &&
   huge$mean_status == "available" && huge$n_invalid_states == 0L,
   "arithmetic SD overflow does not erase valid mean, median or interval statistics")

sdm <- plan(cell(predictor = "identity", support = "real"))
sdm$cells$model <- "sdm"
sdm$cells$parameter <- "log_kappa"
sdm$cells$units <- sdm$cells$predictor_units <- sdm$cells$received_units <- "declared log kappa"
d <- .bh_individual_summarize(sdm, acquire(matrix(c(-1, 2), ncol = 1L), sdm$cells))
ok(all(d$mean == .5) && all(d$median == .5) && all(d$summary_units == "declared log kappa"),
   "SDM identity scales remain the declared log quantity without name-based exponentiation")

shared <- plan(cell(3L, predictor = "identity", support = "real"))
shared$cells$effective_role <- "shared_mean_only_predictor"
shared$cells$shared_alias_key <- shared$cells$parameter_key
aliases <- acquire(matrix(rep(c(1, 2, 3), 3L), nrow = 3L), shared$cells)
sh <- .bh_individual_summarize(shared, aliases)
ok(all(sh$mean == 2) && identical(sh$person_index, rep(1:3, each = 3L)) &&
   identical(sh$scale, rep(shared$scales, 3L)) && all(sh$n_sampled_values == 3L),
   "all shared aliases retain full person cells and requested scale order")
disagree <- aliases
disagree$draws[3L, 3L] <- 4
err(.bh_individual_summarize(shared, disagree), "E_INDIVIDUAL_SOURCE",
    "disagreement in the final shared eta column and final state refuses before summaries")
cap <- shared
cap$budget$max_draw_values <- 8L
err(.bh_individual_summarize(cap, aliases), "E_INDIVIDUAL_BUDGET",
    "alias verification counts every inspected source column against draw-value budget")
nonfinite <- a
nonfinite$draws[2L, 1L] <- Inf
err(.bh_individual_summarize(p, nonfinite), "E_INDIVIDUAL_SOURCE", "required nonfinite eta fails explicitly")
nonfinite$draws[2L, 1L] <- NaN
err(.bh_individual_summarize(p, nonfinite), "E_INDIVIDUAL_SOURCE", "required NaN eta fails explicitly")
err(.bh_individual_summarize(p, acquire(matrix(0, ncol = 1L), p$cells)), "E_INDIVIDUAL_SOURCE",
    "one source row is a report-capability refusal")

empty <- plan(cell()[FALSE, , drop = FALSE])
empty$budget$max_parameter_cells <- 0L
empty$budget$max_draw_values <- 0L
em <- .bh_individual_summarize(empty)
ok(nrow(em) == 0L && identical(names(em), names(o)) &&
   identical(vapply(em, typeof, character(1)), vapply(o, typeof, character(1))),
   "zero logical predictors return the same typed summary schema without source acquisition")
ok(identical(o$coverage_qualification, rep(p$cells$coverage_qualification, 3L)),
   "saved coverage qualifications survive cell-scale expansion")

small <- extreme
small$budget$max_parameter_cells <- 1L
err(.bh_individual_summarize(small, acquire(v, extreme$cells)), "E_INDIVIDUAL_BUDGET",
    "cell cap rejects the complete domain without truncating it")
duplicate <- p
duplicate$cells <- rbind(duplicate$cells, duplicate$cells)
err(.bh_individual_summarize(duplicate, a), "E_INDIVIDUAL_PLAN", "duplicate structured cell keys fail")
classed <- p
classed$cells$person_key <- structure(classed$cells$person_key, class = "tripwire")
err(.bh_individual_summarize(classed, a), "E_INDIVIDUAL_PLAN", "classed cell fields refuse before method dispatch")
classed <- p
classed$budget$max_draw_values <- structure(100L, class = "tripwire")
err(.bh_individual_summarize(classed, a), "E_INDIVIDUAL_BUDGET", "classed budget values refuse before arithmetic dispatch")
classed <- a
classed$draws <- structure(classed$draws, class = c("tripwire", "matrix"))
err(.bh_individual_summarize(p, classed), "E_INDIVIDUAL_SOURCE", "classed acquired matrix refuses before indexing dispatch")
ok(identical(getwd(), cwd_before) &&
   identical(exists(".Random.seed", .GlobalEnv, inherits = FALSE), rng_present) &&
   (!rng_present || identical(get(".Random.seed", .GlobalEnv), rng_before)),
   "success and refusal paths preserve cwd and caller RNG state")
tiny_plan <- plan(cell(predictor = "identity", receives = "eta", support = "real"))
tiny <- .bh_individual_summarize(tiny_plan,
  list(draws = matrix(c(1e-200, 2e-200), 2L, 1L, dimnames = list(NULL, "eta[1,1]"))))
ok(all(is.na(tiny$sd)) && all(tiny$sd_status == "unavailable") &&
     all(tiny$sd_reason == "sd_arithmetic_underflow") &&
     all(tiny$scale_status == "available") && all(tiny$n_invalid_states == 0L) &&
     all(is.finite(tiny$mean)) && all(tiny$n_draws == 2L),
   "nonconstant SD underflow is statistic-local with full source denominator")
constant <- .bh_individual_summarize(tiny_plan,
  list(draws = matrix(c(1e-200, 1e-200), 2L, 1L, dimnames = list(NULL, "eta[1,1]"))))
ok(all(constant$sd == 0) && all(constant$sd_status == "available"),
   "genuine tiny constant series retains available SD zero")
cat("frontend-individual-statistics:", passed, "checks passed\n")
