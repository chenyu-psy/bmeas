source(file.path("helpers", "native-guard.R"))
# R-only engineering controls; no posterior sampling or native code.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if (nzchar(source_root)) {
  for (file in list.files(file.path(source_root, "R"), full.names = TRUE)) source(file)
} else library(bmeas)
private <- function(name) if (nzchar(source_root)) get(name, .GlobalEnv) else getFromNamespace(name, "bmeas")
plan_fun <- private(".bh_m3cs_statistics_plan")
stat_fun <- private(".bh_m3cs_statistics")
empty_fun <- private(".bh_ppc_stats_empty")
aggregate_fun <- private(".bh_ppc_aggregate")
eq <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
d <- data.frame(.task = c("a", "a", "a", "a", "b", "b", "b", "c", "c"),
  .person_key = c("p1", "p1", "p2", "p3", "p1", "p2", "p4", "p2", "p4"),
  .observation_id = paste0("o", 1:9))
x <- matrix(0, 9, 10); x[, 1] <- c(1, 0, 1, 0, 0, 1, 1, 1, 0)
x[, 10] <- c(0, 3, 0, 0, 1, 0, 1, 0, 1)
d$counts <- I(x); d$options <- I(matrix(1, 9, 10))
s <- list(models = list(cs = list(model = list(name = "m3_cs_reference"), data = d)),
  data_map = list(blocks = list(cs = d), subject_keys = paste0("p", 1:4)))
budget <- list(max_draws = 4, max_observations = 1000, max_simulated_values = 4000,
  max_statistics = 200, max_pair_terms = 50000)
p <- plan_fun(s, 4, budget, "population"); o <- stat_fun(s, p)
stopifnot(nrow(p$groups) == 15, nrow(p$pairs) == 75, nrow(o) == 105,
  identical(names(o)[seq_len(ncol(empty_fun()))], names(empty_fun())))
g <- which(p$groups$task == "a" & p$groups$category_index == 1)
stopifnot(eq(o$estimate[o$group_index == g & !is.na(o$group_index) & o$metric == "person_category_rate_mean"], .625),
  eq(o$estimate[o$group_index == g & !is.na(o$group_index) & o$metric == "person_category_rate_sd"], sd(c(.25, 1))),
  p$groups$n_informative_people[g] == 2, p$groups$n_zero_trial_rows[g] == 1)
left <- p$groups$task[p$pairs$left]; right <- p$groups$task[p$pairs$right]
stopifnot(all(p$pairs$overlap[left == "a" & right == "b"] == 2),
  all(p$pairs$overlap[left == "a" & right == "c"] == 1),
  all(p$pairs$overlap[left == "b" & right == "c"] == 2), p$counts$pair_terms == 625)
pp <- which(p$pairs$left == g & p$groups$task[p$pairs$right] == "b" & p$groups$category_index[p$pairs$right] == 1)
stopifnot(eq(o$estimate[!is.na(o$pair_index) & o$pair_index == pp], .375))
reps <- matrix(rep(o$estimate, 4), nrow = 4, byrow = TRUE, dimnames = list(NULL, o$statistic_id))
a <- aggregate_fun(o, reps); stopifnot(nrow(a$table) == 105)
stopifnot(eq(stat_fun(s, p, list(cs = x))$estimate, o$estimate))
for (field in c("max_statistics", "max_pair_terms")) {
  small <- budget; small[[field]] <- if (field == "max_statistics") 104 else 624
  e <- tryCatch(plan_fun(s, 4, small, "population"), error = identity)
  stopifnot(inherits(e, "measurement_error"), e$code == "E_DIAGNOSTIC_BUDGET")
}
cat("PASS M3-CS pooled rates, zero totals, asymmetric overlap, covariance, 105 statistics, schema, aggregation and bounds\n")
