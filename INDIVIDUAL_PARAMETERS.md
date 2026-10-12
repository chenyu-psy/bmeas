# Individual cognitive-parameter summaries

This 0.0.20.9002 R-only development increment adds an explicit main-API reader for person × logical-parameter estimates. Independent R-only source review passed. Exact archive and installed-package results are recorded separately with the release artifacts. It does not run or extend a fitted model.

For a retained fit with its original recorded sample/HMC CSV files, request the scales and source budget explicitly:

```r
individual <- individual_parameter_summary(
  fit,
  scales = c("predictor", "natural", "received"),
  budget = list(max_source_bytes = 10485760,
                max_posterior_rows = 100,
                max_draw_values = 50000,
                max_parameter_cells = 1000))
head(individual)
attr(individual, "source_receipt")
attr(individual, "context")
saveRDS(individual, "individual-parameters.rds")
```

The call reads aligned joint states and returns a summary table. It leaves the input fit unchanged. Reading, printing and saving this returned result does not read the posterior files again. The ordinary `summary(fit)`, `fit_checks(fit)` and `posterior_summary(fit)` remain passive cached reports.

## What a row means

Each row identifies the original person, logical model/parameter/task and requested scale. The complete saved roster remains visible, including an unobserved member of a centered group. Task-level parameters and exact mean-only targets appear as shared aliases; a fixed parameter is labelled as a known input with zero estimation uncertainty under that declaration. A structural zero loading does not make a cognitive parameter fixed or zero.

The table summarizes the full `eta[n,j]` predictor, not an intercept alone, a task deviation alone or a white-basis coordinate. Mean, median, sample SD and empirical 5%/95% quantiles use the retained states. Quantiles use R type 7. Source states can be dependent, and a small retained set does not establish accurate Monte Carlo intervals. No new ESS, Rhat or MCSE is calculated.

The scales follow the saved declaration:

- `predictor`: the complete eta value
- `natural`: the declared inverse predictor map, applied to every state
- `received`: the value actually passed to the likelihood, eta or the declared natural value

For CS activation a/c, natural values are `exp(eta)` while the kernel receives eta. CS ra/rc use inverse-logit for the natural view. SDM `log_c` and `log_kappa` are declared identity-scale quantities: their generic natural view remains the declared log quantity. Names do not trigger extra exponentiation.

Transform first, then summarize. For natural values 1 and 9, the mean is 5; exponentiating their mean log gives 3. Cached intercept/deviation marginals likewise cannot determine the uncertainty of their joint sum. If a requested transform overflows, underflows or saturates, that cell/scale is explicitly unavailable, with its original state denominator and invalid-state count. It is never summarized after silently dropping invalid states. Other valid scales remain available; a single nonrepresentable summary statistic is marked separately. In particular, a nonconstant represented series whose sample-SD arithmetic underflows to zero has an unavailable SD; genuine constant and fixed values keep SD zero.

## Coverage and routed models

Consumed and saved-policy informative row counts use structured observation operands and person identities. Aliased likelihood arguments do not multiply a target's observation count. The context retains exclusion/missingness evidence and its attribution limits. A zero local count does not establish a prior-only or unidentified estimate in a shared model.

For full M3, routed a1/a2/c1/c2 arguments refer to their logical parameter tasks; display-task labels are not the join key. Intrinsic MU `ra_fixed = 100` is a received-predictor literal, retained in context rather than invented as a person parameter. A saved CS ra override does not replace it.

Observation SDs and structural scales remain in their existing parameter reports. This first reader covers declared scalar predictor-role parameters. A model without any such parameters returns a typed not-applicable empty result before file access.

## Explicit cost and source limits

On the original bounded path, all four budget fields are required plain integer-valued numeric scalars. They may lower these ceilings:

| Field | Allowed range |
| --- | --- |
| `max_source_bytes` | 1 to 10,485,760 bytes across complete files |
| `max_posterior_rows` | 2 to 100 actual retained rows across chains |
| `max_draw_values` | 0 to 50,000 required scalar values across all retained rows |
| `max_parameter_cells` | 0 to 1,000 full-roster logical person-parameter cells |

Zero required values permits only a zero-column projection, such as fixed-only predictor parameters. Zero cells permits only an empty logical domain. Additional hard limits are three unique scales, 3,000 summary rows, 300,000 cell × retained-row × scale terms, 1,000 declared and consumed observation rows each, and 10,000 saved operand/context rows. The implementation streams scalar summaries. It does not silently select people, parameters or source states to fit a limit.

The original bounded acquisition supports the qualified CmdStanR 0.9.0 reader, one or two distinct numeric chain IDs, equal positive retained-row counts, sample/HMC output and no saved warmup. The shared preflight accepts the producer's exact `save_warmup=false` spelling and numeric `0`; it continues to refuse `true`/`1`, duplicate or malformed metadata, and non-MCMC outputs. At least two total retained states are required by this reporting slice. All validated retained rows are used, ordered by saved-row rank then numeric chain ID. Saved row numbers are not original sampler iteration numbers. The separate predictive-check API retains its 2–20-state selection on legacy acquisition; explicit main-API streaming budgets may admit larger selections (SCALABLE_PPC.md).

Bounded copies, required-column verification and read-time hashes record exactly what was read. They associate current input with the saved supplied identities; they cannot authenticate an original fit-time posterior source when no original receipt exists. Synthetic engineering sources stay labelled as such. Missing or incompatible historical sources require an explicit reconstruction outside this accessor; no fit is migrated or resealed automatically. Known improper/non-normalizable targets are refused, while unassessed and poor-sampling evidence stays visible.

The historical `measurement_fit` one-argument helper retains its original behavior and scope. The new options apply only to main-API fits.

## Opt-in streaming and explicit empirical subsets

A new-form budget contains a nested `posterior` streaming policy plus explicit complete-cell, output, transformation, original/consumed observation, operand/context and metadata-join work limits. These replace the corresponding old development ceilings only on the opt-in path. See SCALABLE_POSTERIOR.md for a full example and exact resource units. The whole logical roster and scientific scale/alias/coverage semantics stay unchanged.

`states="all"` remains default. An explicit `states=list(method="balanced",draws=D)` requires the streaming budget and labels the table as selected-state empirical evidence. Full validated rows, selected rows, omitted states, per-chain allocation and exact keys are separate; `n_draws` is the selected denominator. Every raw required scalar and alias is checked on all source rows. Nonlinear transforms and exact type7 summaries have selected-state scope. A subset has no new representativeness, quantile-error, ESS or convergence guarantee.

The reader initially accepts multiple equal complete CmdStan 2.40.0 chains under finite source/read/buffer limits. Exact summaries retain an admitted projected matrix and process one cell/scale at a time. No rows are silently discarded to fit memory; missing sources, exceeded work or nonrepresentable results refuse or become explicitly unavailable under the existing numerical rules. Saved larger tables remain passive to print.

## Later comparison work

This table supplies keyed estimates and uncertainty for a later standalone-versus-hierarchical comparison. Such a comparison still needs matched people, logical parameters, data routes and scales, plus explicit differences in pooling, priors and MUra treatment. Matching draw indices from independent fits does not create paired posterior states. This increment makes no new comparison, cognitive-simulation, posterior-recovery or inference-quality claim.
