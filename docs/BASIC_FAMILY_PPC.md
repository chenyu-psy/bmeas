# Explicit predictive checks for basic response families

`summary()` and `fit_checks()` read saved evidence. To compute response discrepancies, explicitly request a small posterior predictive check on a main-API fit:

```r
budget <- diagnostic_budget(max_draws=8, max_observations=1000,
                            max_simulated_values=8000)
ppc_plan(fit, draws=8, budget=budget, level="population")
checked <- assess_measurement_fit(fit, checks="ppc", level="population",
                                 draws=8, seed=24, budget=budget)
summary(checked)
fit_checks(checked)$predictive$cached$table$record
```

The input `fit` is unchanged. Keep separate returned objects to compare population and conditional checks. No LOO, refitting or compilation is performed. The implementation has passed separate R-only acquisition, numerical-law and lifecycle reviews using synthetic fixtures. These controls establish engineering behavior; they do not validate posterior inference.

## Choose the predictive question

- `level="population"` with iid people generates fresh common and residual effects from each selected joint draw, using the saved loading and factor maps. The same generated person effect is reused across all blocks and repeated observations.
- `level="population"` with sample-centered/task-Cholesky people generates a fresh group of the original size. It retains the complete roster, including people without consumed responses, and the original task order and dependence. It never recenters a subset or creates independent new people.
- `level="conditional"` keeps each selected draw's inferred person predictors and replicates responses only. Its paired-person response checks remain conditional; they do not assess the population measurement structure.

The level must be explicit for a main-API fit. Gaussian, binomial-logit and lognormal response models use canonical R simulation kernels selected by their saved likelihood/operand contract. Task-level and fixed operands, observation-scale roles, missingness, repeated rows and binomial trial counts are preserved. A custom model name or a supplied prediction callback does not grant support. Cognitive and arbitrary custom response simulation remain outside this increment. The earlier Exp1 reference evidence concerns the original pure measurement portion, not a joint external-Gf association model.

## What is compared

Marginal summaries include response location, spread and tails; lognormal checks also summarize log responses. Binomial checks include trial-weighted success rate and extreme-response counts. Zero-trial rows stay in the design but provide no rate or paired-person information.

For every pair of declared response groups, the population check compares covariance of person response summaries on their actual overlapping people. Repeats are summarized within person; binomial person summaries divide total successes by total positive trials. These are response covariances, not estimated latent correlations. A one-person overlap, empty group or missing pair remains explicitly unavailable. Fixed-only and fully excluded groups remain visible.

The saved table contains observed values, replicated medians, empirical 5% and 95% limits (R quantile type 7), units, contributing counts, finite replication counts and interval departures. Constant or low-information summaries are labelled. With small or potentially dependent posterior selections, these are coarse descriptive screens. Increasing the replication budget does not establish effective sample size or calibration. Coverage means computations are available; neither complete coverage nor an interval comparison is a general good-fit verdict. Sampling quality, identification, prior sensitivity and posterior propriety remain separate records. A retained proof of an improper or non-normalizable target prevents posterior predictive assessment.

## Cost and acquisition

`ppc_plan()` reads saved metadata only. It does not open CSV files, inspect backend metadata, extract draws or change RNG state. It reports bounds and required scalar columns; actual posterior availability stays uninspected until assessment.

The three predictive budget arguments (`max_draws`, `max_observations`,
`max_simulated_values`) are required. Published main-API policy starts at 20
replications, 1,000 response rows and 20,000 simulated values. Optional defaults
are 10,000 fresh latent values, 200 statistics, 50,000 paired-person products,
and 1,000,000 logical allocation/work slots. Explicit higher representable work
allowances are admitted; see SCALABLE_PPC.md. More than 20 replications requires
the nested streaming policy.

Without streaming, acquisition remains capped at one or two chains, 10 MiB of
unique source CSVs, 100 retained rows and 50,000 required values over all retained
rows. Outer acquisition overrides may lower these limits, not raise them.

Geometry requires representable dimensions, JT<=J, the supported person law and
checked full-design allocations. The earlier N<=30, J<=12, K+U<=12 and related
development geometry caps are no longer fixed main-API PPC limits. Full design,
all statistics and required person pairs are counted, including fixed-only
projections. There is no automatic subset selection to satisfy a limit. One
response replication is processed at a time, with the replication-by-statistic
matrix retained. Logical work counts do not guarantee wall time or peak memory.

On that legacy bounded path, assessment reads the fit's recorded CSV paths through bounded verified temporary copies. It supports the qualified CmdStanR 0.9.0 reader, recognized sample/HMC output, explicit `save_warmup=false` or `save_warmup=0` (the exact lowercase Boolean or numeric spelling; `true`/`1` remain unsupported saved-warmup layouts), one or two distinct chain IDs and equal positive actual retained-row counts. Saved warmup, fixed-parameter, optimizer, variational, incomplete and ambiguous layouts are refused. Draws are selected without replacement by a deterministic balanced chain/retained-row rule. Scalar columns stay jointly aligned. Constants-only projections validate source rows/keys without requesting an empty variable filter that would load all columns.

The receipt binds current bytes, shapes, numeric chain IDs, retained-row keys, CSV precision and exact predictive maps to saved fit/source identities. A read-time hash cannot establish that historical CSVs are unchanged since fitting. Current producers do not retain original CSV hashes, and that absence remains explicit. Synthetic HMC-shaped test files exercise engineering behavior only; any recorded synthetic-source qualification is preserved. Cached means or quantiles are never used to reconstruct joint draws.

## Opt-in streaming sources

A main consumer budget may instead contain `posterior=list(reader="streaming", max_source_bytes=..., max_read_bytes=..., max_materialized_values=...)`. Do not mix it with outer legacy source/row/value limits. See SCALABLE_POSTERIOR.md for the complete field set, configurable defaults and seven-pass accounting.

The package-owned parser validates every row of every complete equal-layout CmdStan 2.40.0 chain, while retaining only the requested columns and balanced selected states. PPC needs at least two replications and at least one from every chain. Explicit higher predictive and allocation budgets permit larger supported full designs; a reader allowance alone does not admit that extra work. Current bytes, validated source counts and selected-state counts remain separate; saved report reads start no acquisition.

## Failure and reproducibility

The explicit seed uses a pinned full RNG kind and records the R version, draw selection and saved ordering. Caller RNG state and working directory are restored on success, error or interrupt. Unsupported RNG kinds and locked/active seed bindings refuse before mutation. Arbitrary roster or declaration reorderings do not imply bitwise invariance.

Required nonfinite arithmetic, informative binomial inverse-logit saturation, and lognormal overflow or zero underflow fail explicitly. No clipping, retries or discarded replications conceal these failures. Unsupported requests fail before an attempt. A failure after acquisition starts returns a failed current attempt and clears an older PPC table; upstream model/sampler evidence is unchanged. Interruptions propagate after cleanup without a partial result.

Required common/residual correlation factors have positive triangular diagonals and unit row-Gram diagonals within absolute tolerance 2e-5, accommodating ordinary six-significant-digit CSV rounding. This is a retained-factor role check, not an exact SPD proof or source authentication; factors are never renormalized. The task work factor has its distinct covariance role. A bounded check rejects individual nonzero products that overflow or round to zero before required matrix multiplications. This is conservative even for a dominated term; exact cancellation of representable terms is permitted.

`fit_checks()` and `summary()` display only the current cached result, including law, level, requested/completed replications, discrepancies and unavailable checks. Malformed optional PPC metadata stays locally unavailable and does not erase valid sampler diagnostics. Historical `measurement_fit` behavior keeps its original separate scope.
