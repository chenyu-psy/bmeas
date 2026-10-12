# Stream retained posterior sources under explicit budgets

The opt-in `posterior` policy extends the two main-API consumers to multiple complete chains and longer saved runs. It changes acquisition and the admitted reporting work, not the fitted model. Without this policy, the earlier bounded reader and its existing budget payloads retain their behavior.

## Explicit policy, existing operations

```r
source_limits <- list(reader="streaming",
  max_source_bytes=128*1024^2,
  max_read_bytes=7*128*1024^2,
  max_materialized_values=4000000)

ppc_budget <- diagnostic_budget(max_draws=8, max_observations=1000,
  max_simulated_values=8000, posterior=source_limits)
ppc_plan(fit, draws=8, budget=ppc_budget, level="population")
# Explicit computation on an existing supported fit with complete CSVs:
# checked <- assess_measurement_fit(fit, checks="ppc", level="population",
#   draws=8, seed=24, budget=ppc_budget)
```

These numbers are illustrative finite choices, not a universal recommendation or memory guarantee. The plan only reads saved metadata. It neither opens posterior files nor discovers a provider. Assessment performs acquisition explicitly. The complete design and scientific family/person-law constraints still apply. Higher replication, geometry, response and statistic work requires explicit predictive allowances in addition to the reader policy; see SCALABLE_PPC.md. Larger reader allowances alone do not add arbitrary custom PPC or permit omitting people or response rows. Live-fit and saved single-block pinned M3-CS retain their explicit simulator contract; see SAVED_COGNITIVE_WORKFLOW_zh.md for current portable/archive support and its narrower recorded native evidence.

For individual summaries, declare the complete reporting work too:

```r
individual_budget <- list(posterior=source_limits,
  max_parameter_cells=1200, max_summary_rows=3600,
  max_transformation_terms=2000000,
  max_declared_observation_rows=10000,
  max_consumed_observation_rows=10000,
  max_operand_context_rows=50000,
  max_context_terms=50000000)
# Every validated state, with exact empirical summaries:
# individual <- individual_parameter_summary(fit,
#   scales=c("predictor", "natural", "received"), budget=individual_budget)
# An explicitly chosen empirical subset:
# subset <- individual_parameter_summary(fit,
#   scales=c("predictor", "natural", "received"), budget=individual_budget,
#   states=list(method="balanced", draws=200))
```

`states="all"` is the default. A subset needs the streaming policy and is labelled as selected-state evidence: `n_draws` is its selected denominator, while full-source rows, omitted rows, per-chain allocations and exact selected keys remain separate. Exact subset quantiles are not full-source quantiles. No representativeness, Monte Carlo precision, Rhat/ESS/MCSE or inference-quality guarantee is added.

Do not mix `posterior` with the outer legacy acquisition fields `max_source_bytes`, `max_posterior_rows` or `max_draw_values`. The old four-field individual budget remains a separate unchanged path. New options refuse on historical `measurement_fit` objects; their one-argument helper remains unchanged.

## What each allowance means

The required nested fields are `reader="streaming"`, `max_source_bytes`, `max_read_bytes` and `max_materialized_values`. Optional finite defaults, all overridable, are:

| Field | Default | Meaning |
| --- | ---: | --- |
| `max_record_bytes` | 1,048,576 | Physical header/comment/data record bound before line assembly |
| `max_columns` | 100,000 | Complete CSV header width, before splitting names/maps |
| `max_source_rows` | 1,000,000 | Aggregate actual retained rows across every chain |
| `max_chains` | 32 | Every recorded source chain must fit |
| `max_temp_bytes` | twice source allowance | Owned CSV copy storage |
| `max_scan_values` | twice row allowance times required columns | Required numeric conversions across validation and projection |

A wide generated row/header can exceed the record default; raise it explicitly if appropriate. None of these is a scientific model-size ceiling. Inputs must be plain finite integer-valued scalars within checked representability; R matrix/index limits still apply. There is no unbounded mode or silent quota reduction.

Source bytes, repeated reading, temporary storage and numeric payload are different costs. The initial reader performs seven complete byte traversals: two original-to-owned copies, two initial snapshot hashes, validation, projection and a final selected-snapshot hash. It reserves seven times measured source size and twice that size for copies before starting. Copy/scan reads are metered; bounded owned-file hash reads are charged by reserved size, not mislabelled as instrumented I/O. This is logical byte work, not physical disk traffic or page-cache use. Failed attempts retain partial counters.

The scanner uses at most 64 KiB raw chunks and checks unfinished records/column counts before their corresponding allocations. Both passes inspect all row widths and numeric token syntax. Required values are converted on every validation row, then on selected projection rows: R×V+D×V, where R is full-source rows and D selected rows. Full-width lexical work uses all Q columns and is reported separately. Shared-alias comparisons cover all source rows.

`max_materialized_values` bounds a conservative declared simultaneous numeric-slot envelope. Its itemized receipt includes projected values, chain/rank keys even with zero requested columns, header/alias indices, row and scanner masks, known overlapping copies and the consumer's declared workspace/output buffers. It is not merely D×V and is not the fitting policy's cumulative eager-cache quota. Byte estimates from slots are payload estimates; strings, object headers, invisible R/native copies, sorting overhead and peak RSS remain unknown. No OS memory, hard filesystem or wall-time enforcement is provided.

The seven individual work quotas count complete logical cells, all requested scale rows, selected-state transformations, original/consumed observations, operand/route/exclusion context and conservative metadata-join/expansion work before allocation. They never authorize dropping a roster member, fixed/shared alias or coverage row. Retained-spec fingerprint serialization and scientific source inspection remain existing explicit-operation work outside the CSV byte/temp ledger; the ledger does not bound the original fit object or all process I/O.

## Complete sources and deterministic state identity

The streaming parser initially recognizes complete CmdStan 2.40.0 sample/HMC CSVs. It does not require CmdStanR, a shell command, backend draws/metadata or a format-option fallback. The legacy reader keeps its independently qualified version/layout scope.

Every recorded source path is consumed, with any usable saved expected chain count reconciled. Chains must have unique actual numeric IDs and equal positive complete layouts/counts. Each count must agree with its own num_samples/thin header. Saved warmup, non-MCMC, duplicate/ambiguous metadata, malformed/truncated rows and incomplete footers are refused. Exact save_warmup false/true/0/1 syntax is recognized, but only false/0 is allowed. No shortest-chain truncation, missing-chain omission or repeated thinning occurs.

Every required scalar must be finite in every source row, including unselected rows. Other numeric fields retain the existing valid optional Inf/NaN token semantics. Exact shared-alias equality is also checked across all rows. Selected nonlinear transformations and statistics are evaluated only on the explicitly selected empirical set and labelled accordingly.

Balanced selection sorts chain IDs, allocates floor(D/C) rows each with remainders given to the first IDs, and includes every chain. One selected rank uses floor((L+1)/2); several use exact evenly spaced ranks including endpoints. Selection is interleaved by selected rank then ascending chain ID. Checked arithmetic prevents rounded or duplicate indices. Input path order and scanner chunk size do not change values, keys or the predictive RNG sequence. Caller path order remains separately recorded. A saved row is a CSV rank, not an original sampler iteration.

PPC requires at least two selected states and at least one from every chain. The main streaming path accepts more than 20 replications when the explicit predictive, allocation and source allowances admit them. The legacy acquisition path still allows at most 20 selected states. Individual reports require at least two selected states. A genuinely empty predictor domain needs no source; any retained balanced request is labelled unexecuted and zero selected states do not fulfill it. This requires an empty result, zero cells, not-applicable status, a no-acquisition receipt, and empty keys/allocations together. Nonempty fixed-only predictors still validate source counts/keys with zero requested value columns.

## Exact summaries, failure and provenance

Individual means, medians, sample SDs and R type7 5%/95% quantiles use the stated all-row or selected-state values after each declared transform. Fixed inputs and shared means retain their scientific roles. Invalid transformations do not get dropped; the cell/scale is unavailable with the original selected denominator. Statistic overflow and nonconstant SD underflow remain local. No approximate quantiles, online-mean substitute or hidden disk sort is used.

Owned snapshots preserve read-time byte identity and detect observed source/copy changes. They are not atomic/adversarial snapshots or authentication of original fit-time CSV, executable or data. Supplied historical hash presence/verification, recorded synthetic qualification, precision, source paths, chains and keys remain explicit. Missing sources do not trigger reconstruction from marginal means, provider caches or resealed historical fits.

On failure or interruption, close connections and remove only files/directories actually created by that transaction. An existing path collision is preserved. Interrupts propagate. PPC keeps its whole-call RNG restoration around acquisition and resets its response stream afterward. Individual extraction uses no RNG. Input fits and their scientific/sampler evidence remain unchanged; PPC retains its existing current-attempt replacement rules.

Saved result and fit reports remain passive. Malformed optional source/selection metadata is locally unavailable without deleting a usable historical table. This increment adds no general diagnostic-cache acquisition, new fit/native/HMC, study-scale inference or standalone/HMM comparison qualification. Independent parser and combined consumer gates, and later archive qualification, are separate.
