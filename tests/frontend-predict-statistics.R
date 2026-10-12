source(file.path("helpers", "native-guard.R"))
# Explicit engineering/statistic controls only. No posterior, RNG, Stan or HMC.
source_root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode<-nzchar(source_root)
if(source_mode) {
  for(file in list.files(file.path(source_root,"R"),full.names=TRUE))source(file)
}else library(bmeas)
private<-function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
.bh_ppc_aggregate<-private(".bh_ppc_aggregate")
.bh_ppc_statistics<-private(".bh_ppc_statistics")
.bh_ppc_statistics_plan<-private(".bh_ppc_statistics_plan")
passed <- 0L
ok <- function(value, label) { if (!isTRUE(value)) stop(label); passed <<- passed + 1L; cat('PASS', label, '\n') }
err <- function(expr, code, label) { e <- tryCatch(force(expr), error = identity); ok(inherits(e, 'measurement_error') && identical(e$code, code), label) }
eq <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
budget <- diagnostic_budget(4, 1000, 4000)
plan <- function(s, level = 'population', b = budget) .bh_ppc_statistics_plan(s, 4, b, level)
value <- function(x, target, metric) x$estimate[x$target == target & x$metric == metric]
gblock <- function(d, units = 'points', model = gaussian_model()) observe(model, d, 'person', 'task', 'y',
  units = units, observation_id = 'obs', parameters = list(mu = list(fixed = 0), sigma = list(fixed = 1)))
gb <- function(p, t, y) data.frame(person = p, task = t, y = y, obs = paste0('o', seq_along(y)))
prepare <- function(blocks, missing = 'error') bhmm(bhmf(), data = blocks, subjects = 'shared', missing = missing, fit = FALSE)

# Same margins, different paired association; exact zero covariance degeneracy.
d <- gb(rep(c('p1', 'p2', 'p3'), 2), rep(c('a', 'b'), each = 3), rep(c(1, 2, 3), 2))
s <- prepare(list(g = gblock(d)))
p <- plan(s); o <- .bh_ppc_statistics(s, p)
ok(nrow(p$groups) == 2 && nrow(p$statistics) == 9 && p$counts$pair_terms == 15, 'fixed-only groups retain 8 marginal plus one paired statistic and D+1 overlap budget')
ok(eq(o$estimate[o$metric == 'paired_person_covariance'], 1), 'per-person response covariance agrees with explicit array')
r <- list(g = s$data_map$blocks$g$y); at <- which(s$data_map$blocks$g$.task == 'b'); r$g[at] <- rev(r$g[at])
other <- .bh_ppc_statistics(s, p, r)
ok(identical(o$estimate[o$domain == 'marginal'], other$estimate[other$domain == 'marginal']) &&
   eq(other$estimate[other$metric == 'paired_person_covariance'], -1), 'equal univariate margins preserve marginal summaries and change pair association')
r$g[at] <- 7
constant <- .bh_ppc_statistics(s, p, r)
pair <- constant[constant$metric == 'paired_person_covariance', ]
ok(pair$estimate == 0 && pair$degenerate && pair$low_information && pair$status == 'computed', 'zero observed variance preserves a defined zero covariance with low-information flags')
ok(identical(o$unit[o$metric == 'paired_person_covariance'], 'points * points'), 'paired covariance has explicit product response units')

# Canonical density contract, never model display or advertised callbacks.
named <- s; named$models$g$model$name <- 'renamed'; named$models$g$model$version <- 'changed'
named$models$g$model$methods$posterior_predict <- function(...) stop('CALLBACK MUST NEVER RUN')
named$models$g$model$capabilities <- NULL
ok(identical(plan(named), p) && identical(.bh_ppc_statistics(named, p), o), 'descriptive rename and replaced callback do not dispatch scientific law or execute callback')
bad <- s; bad$models$g$model$likelihood$symbol <- 'other_lpdf'
err(plan(bad), 'E_CHECK_UNSUPPORTED', 'unqualified likelihood contract refuses before callback')

# Mixed families, repeated rows and unequal trial weights; p0 has zero trials.
ga <- gb(c('p1', 'p1', 'p2', 'p3'), 'g', c(1, 3, 4, 7))
bd <- data.frame(person = c('p0', 'p1', 'p1', 'p2', 'p3'), task = 'b',
  successes = c(0, 0, 9, 1, 2), trials = c(0, 1, 9, 2, 2), obs = paste0('b', 1:5))
bin <- observe(binomial_model(), bd, 'person', 'task', fields = list(successes = 'successes', trials = 'trials'),
  units = 'count', observation_id = 'obs', parameters = list(p = list(fixed = .5)))
ld <- gb(c('p1', 'p2', 'p3'), 'l', exp(c(0, 1, 2)))
ln <- observe(lognormal_model(), ld, 'person', 'task', 'y', units = 'seconds', observation_id = 'obs',
  parameters = list(meanlog = list(fixed = 0), sdlog = list(fixed = 1)))
mix <- prepare(list(g = gblock(ga), b = bin, l = ln)); mp <- plan(mix); mo <- .bh_ppc_statistics(mix, mp)
ok(nrow(mp$statistics) == 18 && nrow(mp$pairs) == 3, 'mixed families produce all fixed presets and every pair')
ok(eq(value(mo, 'b::p::b', 'total_successes'), 12) && eq(value(mo, 'b::p::b', 'weighted_success_rate'), 12 / 14), 'binomial total and success rate use trial weights')
ok(eq(value(mo, 'b::p::b', 'row_rate_sd_given_trials'), sd(c(0, 1, .5, 1))) &&
   value(mo, 'b::p::b', 'zero_success_rows') == 1 && value(mo, 'b::p::b', 'all_success_rows') == 2,
   'zero trials excluded from rates and extremes; positive-trial counts correct')
brows <- mo[mo$target == 'b::p::b', ]
ok(all(brows$n_rows == 5 & brows$n_informative_rows == 4 & brows$n_zero_trial_rows == 1 &
       brows$total_trials == 14 & brows$n_people == 4 & brows$n_informative_people == 3), 'per-statistic consumed/informative row, person, zero-trial and trial counts retained')
gbpair <- mo[mo$target1 == 'g::mu::g' & !is.na(mo$target2) & mo$target2 == 'b::p::b', ]
ok(eq(gbpair$estimate, cov(c(2, 4, 7), c(.9, .5, 1))) && gbpair$overlap == 3 &&
   gbpair$rows_left == 4 && gbpair$rows_right == 4 && gbpair$trials_right == 14,
   'repeat means and person-specific summed success/trial ratios use exact common-person overlap')
ok(eq(value(mo, 'l::meanlog::l', 'log_mean'), 1) && eq(value(mo, 'l::meanlog::l', 'log_sd'), 1) &&
   identical(mo$unit[mo$target == 'l::meanlog::l' & mo$metric == 'log_mean'], 'log(response/seconds)'),
   'lognormal response and log-response statistics retain explicit units')
ok(all(mp$pairs$overlap == 3) && mp$counts$pair_terms == 45, 'zero-trial-only person never creates pair overlap')

# Missingness, wholly excluded groups, no positive trials and sparse pairs.
missing <- gb(c('p1', 'p2', 'p3', 'p4', 'p1', 'p2', 'p3'), c('a', 'a', 'b', 'b', 'empty', 'empty', 'partial'), c(1, 2, 4, 5, NA, NA, 9))
missing <- rbind(missing, transform(missing[1, ], task = 'partial', y = NA_real_, obs = 'extra'))
ex <- prepare(list(g = gblock(missing), wholly = gblock(gb(c('p1', 'p2'), 'gone', c(NA_real_, NA_real_)))), 'observed_only')
ep <- plan(ex); eo <- .bh_ppc_statistics(ex, ep)
ok(nrow(ep$groups) == 5 && nrow(ep$pairs) == 10, 'wholly excluded task and wholly excluded block remain in complete pair plan')
ok(all(eo$status[eo$target %in% c('g::mu::empty', 'wholly::mu::gone')] == 'no_observations') &&
   all(eo$n_excluded_rows[eo$target == 'g::mu::empty'] == 2), 'wholly excluded groups have explicit unavailable status and missingness counts')
partial <- eo[eo$target == 'g::mu::partial', ]
ok(all(partial$n_rows == 1 & partial$n_excluded_rows == 1) && partial$status[partial$metric == 'sd'] == 'insufficient_rows', 'n<2 SD unavailable without discarding defined one-row summaries')
ok(all(eo$status[eo$target1 == 'g::mu::a' & !is.na(eo$target2) & eo$target2 == 'g::mu::b'] == 'no_paired_people'), 'two observed targets with disjoint people have unavailable covariance')
zero <- mix; zero$data_map$blocks$b$trials[] <- 0; zero$data_map$blocks$b$successes[] <- 0
zp <- plan(zero); zo <- .bh_ppc_statistics(zero, zp)
ok(all(zo$status[zo$target == 'b::p::b'] == 'no_positive_trials') && all(is.na(zo$estimate[zo$target == 'b::p::b'])), 'all-zero-trial group never gets successful count or rate evidence')

# Resource refusals include fixed-only/excluded domains, no omitted pairs.
small <- budget; small$max_statistics <- 8L
err(plan(s, b = small), 'E_DIAGNOSTIC_BUDGET', 'lower statistic cap refuses complete fixed-only plan')
small <- budget; small$max_pair_terms <- 14L
err(plan(s, b = small), 'E_DIAGNOSTIC_BUDGET', 'D+1 paired-person term budget enforced before pair table expansion')
small$max_pair_terms <- 15L
ok(plan(s, b = small)$counts$pair_terms == 15, 'exact paired-person budget boundary passes')
huge <- s
huge$tables$parameter_roles <- do.call(rbind, lapply(seq_len(1000), function(j) { z <- s$tables$parameter_roles; z$task <- paste0(z$task, j); z$key <- paste(z$model, z$parameter, z$task, sep = '::'); z }))
huge$models$g$data <- do.call(rbind, lapply(seq_len(1000), function(j) { z <- s$models$g$data; z$.task <- paste0(z$.task, j); z }))
err(plan(huge), 'E_DIAGNOSTIC_BUDGET', 'large fixed-only declared domain refuses without constructing a pair list')
too_many <- budget; too_many$max_statistics <- 201
ok(is.list(plan(s, b = too_many)), 'explicit statistic budget may exceed former engineering ceiling')

# Exact ordered D by M retained aggregates and type-7 empirical intervals.
reps <- rbind(o$estimate, other$estimate, o$estimate, other$estimate)
colnames(reps) <- o$statistic_id
a <- .bh_ppc_aggregate(o, reps)
ok(identical(dim(a$replicate_statistics), c(4L, 9L)) && identical(colnames(a$replicate_statistics), o$statistic_id), 'aggregate matrix retains all D by M values and exact planned identities')
covrow <- which(o$metric == 'paired_person_covariance')
ok(eq(a$table$lower90[covrow], quantile(reps[, covrow], .05, type = 7)) &&
   eq(a$table$upper90[covrow], quantile(reps[, covrow], .95, type = 7)) && all(a$table$quantile_type == 7), 'empirical interval explicitly uses R quantile type 7')
ok(a$coverage$status == 'complete' && a$measurement_structure$status == 'complete', 'nonempty evaluable paired set gets complete computation coverage only')
damaged <- reps; damaged[1:2, 1] <- NA_real_; damaged[1:3, 2] <- NA_real_
da <- .bh_ppc_aggregate(o, damaged)
ok(da$table$finite_replications[1] == 2 && da$table$requested_replications[1] == 4 &&
   da$table$replication_coverage[1] == 'partial' && is.finite(da$table$replicated_median[1]) &&
   is.na(da$table$lower90[2]) && identical(unname(da$replicate_statistics), unname(damaged)), 'finite replication coverage explicit, no missing states replaced or erased, n<2 interval unavailable')
err(.bh_ppc_aggregate(o, reps[, rev(seq_len(ncol(reps))), drop = FALSE]), 'E_PREDICTION_SUPPORT', 'wrong aggregate column identity/order refuses')
dep <- o; dep$estimate[1] <- 100
ok(.bh_ppc_aggregate(dep, reps)$table$outside_interval[1], 'outside-interval indicator is descriptive and row-specific')
cp <- plan(s, 'conditional'); co <- .bh_ppc_statistics(s, cp); colnames(reps) <- co$statistic_id
ca <- .bh_ppc_aggregate(co, reps)
ok(all(co$domain[co$metric == 'paired_person_covariance'] == 'conditional_association') &&
   ca$measurement_structure$status == 'unassessed' && ca$conditional_association$status == 'complete', 'conditional covariance retains association meaning without qualifying population measurement structure')
single <- prepare(list(g = gblock(gb(c('p1', 'p2'), 'only', c(1, 2)))))
sp <- plan(single); so <- .bh_ppc_statistics(single, sp); sr <- matrix(rep(so$estimate, each = 4), 4, dimnames = list(NULL, so$statistic_id))
ok(.bh_ppc_aggregate(so, sr)$measurement_structure$status == 'unavailable', 'single-target model has no cross-target success from all(logical(0))')
empty <- s; empty$models <- empty$data_map$blocks <- setNames(list(), character()); empty$tables$parameter_roles <- s$tables$parameter_roles[FALSE, ]; empty$data_map$subject_keys <- character()
np <- plan(empty); no <- .bh_ppc_statistics(empty, np); nr <- matrix(numeric(), 4, 0, dimnames = list(NULL, character()))
na <- .bh_ppc_aggregate(no, nr)
ok(np$status == 'no_statistics' && attr(no, 'coverage')$status == 'no_statistics' && na$coverage$status == 'unavailable', 'empty statistic sets remain explicit non-success')

# Explicit zero work caps align with the normalized controller budget.
zero_caps <- budget; zero_caps$max_statistics <- 0L; zero_caps$max_pair_terms <- 0L
ezp <- plan(empty, b = zero_caps); ezo <- .bh_ppc_statistics(empty, ezp)
ok(identical(ezp$statistics, np$statistics) && identical(ezo, no) &&
   ezp$counts$statistics == 0 && ezp$counts$pair_terms == 0 &&
   ezp$limits$max_statistics == 0L && ezp$limits$max_pair_terms == 0L,
   'zero statistic and pair-term caps preserve typed empty plans and explicit non-success')
err(plan(single, b = zero_caps), 'E_DIAGNOSTIC_BUDGET', 'zero statistic cap refuses any actual planned statistic')
zero_pairs <- budget; zero_pairs$max_pair_terms <- 0L
szp <- plan(single, b = zero_pairs)
ok(szp$counts$pair_terms == 0 && nrow(szp$pairs) == 0 && identical(szp$statistics, sp$statistics),
   'zero pair-term cap allows all single-target marginal statistics without adding pairs')
err(plan(s, b = zero_pairs), 'E_DIAGNOSTIC_BUDGET', 'zero pair-term cap refuses positive paired-person work')
disjoint <- prepare(list(g = gblock(gb(c('p1', 'p2', 'p3', 'p4'), c('a', 'a', 'b', 'b'), c(1, 2, 3, 4)))))
dzp <- plan(disjoint, b = zero_pairs); dzo <- .bh_ppc_statistics(disjoint, dzp)
ok(dzp$counts$pair_terms == 0 && nrow(dzp$pairs) == 1 &&
   identical(dzo$status[dzo$metric == 'paired_person_covariance'], 'no_paired_people'),
   'zero pair-term cap retains unavailable disjoint pair with zero overlap work')
negative <- budget; negative$max_statistics <- -1L
err(plan(empty, b = negative), 'E_DIAGNOSTIC_BUDGET', 'negative statistic cap remains invalid')
negative <- budget; negative$max_pair_terms <- -1L
err(plan(single, b = negative), 'E_DIAGNOSTIC_BUDGET', 'negative pair-term cap remains invalid')

# Whole response vector contract and family support are checked, with no RNG.
err(.bh_ppc_statistics(s, p, list(g = 1)), 'E_PREDICTION_SUPPORT', 'wrong response lengths refuse')
err(.bh_ppc_statistics(mix, mp, list(g = ga$y)), 'E_PREDICTION_SUPPORT', 'missing block responses refuse')
mr <- lapply(mix$data_map$blocks, function(d) if ('successes' %in% names(d)) d$successes else d$y)
mr$l[1] <- 0
err(.bh_ppc_statistics(mix, mp, mr), 'E_PREDICTION_SUPPORT', 'zero lognormal responses refuse without repair')
mr$l <- mix$data_map$blocks$l$y; mr$b[1] <- 1
err(.bh_ppc_statistics(mix, mp, mr), 'E_PREDICTION_SUPPORT', 'positive zero-trial successes refuse without repair')
mr$b <- mix$data_map$blocks$b$successes; mr$g[1] <- Inf
err(.bh_ppc_statistics(mix, mp, mr), 'E_PREDICTION_SUPPORT', 'nonfinite response refuses')
seed_present <- exists('.Random.seed', .GlobalEnv, inherits = FALSE)
seed <- if (seed_present) get('.Random.seed', .GlobalEnv) else NULL
kind <- RNGkind(); input <- serialize(mix, NULL)
invisible(plan(mix)); invisible(.bh_ppc_statistics(mix, mp)); invisible(.bh_ppc_aggregate(o, reps))
ok(identical(serialize(mix, NULL), input) && identical(RNGkind(), kind) &&
   identical(exists('.Random.seed', .GlobalEnv, inherits = FALSE), seed_present) &&
   (!seed_present || identical(get('.Random.seed', .GlobalEnv), seed)), 'planner/evaluator/aggregate are pure reads and preserve complete existing RNG state')
cat(passed, 'bounded statistic controls passed. Synthetic engineering evidence only; no posterior validation.\n')
