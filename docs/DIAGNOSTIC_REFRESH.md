# Explicit diagnostic workflow

The diagnostic workflow adds semantic monitor discovery and an explicit all-retained-state refresh. Bounded R-only source and lifecycle checks have passed. Exact archive and installed-package qualification are recorded separately; this does not add a new fitted-model or inference qualification. The unpublished fixed-block branch is excluded.

For an existing main-API fit, discover readable labels and select the public keys you want:

```r
available <- diagnostic_parameters(fit)
available[c("key", "label", "role")]
selected <- available$key[available$role == "Intercept"]
stopifnot(length(selected) > 0L)
refreshed <- refresh_diagnostics(fit, selected, budget = list(
  posterior = list(reader = "streaming",
    max_source_bytes = 256 * 1024^2,
    max_read_bytes = 7 * 256 * 1024^2,
    max_materialized_values = 2e6),
  max_monitors = 100, max_metric_rows = 4000, max_fft_length = 2048))
parameter_summary(refreshed)
fit_checks(refreshed)
```

These are example caller allowances, not universal recommendations. Four complete chains with 1,000 retained rows each use the 4,000-row allowance; actual output width, aliases and source sizes also matter. The numeric-slot allowance counts known package buffers, while provider/R/sort/FFT/native overhead and total RSS remain unknown. A refusal never silently drops rows, chains or monitors.

Catalogue rows cover population intercept/loading/scale/amplitude/share primitives, unique final marginal correlations, raw-B coordinates and explicitly labelled raw-law derived correlations. Fixed constants, person/basis-score coordinates, share weights and other derived roles remain excluded. A shared basis amplitude is selectable. Each equality primitive has one row and complete declared source aliases. Source-variable spellings are inspection metadata; the user selects semantic keys and reads structured labels.

Refresh validates every recorded complete equal-layout chain through the package-owned streaming reader. It computes the requested quantities on intact chain matrices. Empirical summaries use all observed states; any private odd-row split omission belongs only to the pinned convergence estimator. Parameter diagnostics require posterior 1.7.0. R >= 4.1 remains the package requirement, with actual environment versions recorded. Mean MCSE uses raw-mean ESS, not bulk ESS. Undefined, infinite or underflowed outcomes remain explicit.

Use `parameters=character()` for HMC-only refresh without loading posterior or CmdStanR. Divergence/depth fractions use all retained rows per original chain. Missing sampler fields or missing/malformed depth limits are unavailable. E-BFMI needs unthinned nonconstant energy and the pinned CmdStanR formula; no lag-thin approximation or favorable zero is invented.

The returned fit is a copy. Its selected parameter table and HMC/report provenance replace those requested phases atomically; a failure leaves the caller's fit untouched. Unrelated cached reports and scientific/inference statuses retain their provenance. A successful refresh says which diagnostics were computed, not that every latent variable is monitored or that convergence, identification, empirical fit or inference is satisfactory. Passive `parameter_summary()`, `posterior_summary()`, `summary()`, `print()` and `fit_checks()` do no new source acquisition or metric computation.

Reviewed concrete estimator bindings prevent caller S3 overrides from replacing the numerical calculation. A cold provider load removes only the recognized cli handler installed by that same load, preserving existing and distinct intervening caller values. See DIAGNOSTIC_BINDINGS.md, DIAGNOSTIC_METRIC_COSTS.md and DIAGNOSTIC_OPTION_CUSTODY.md for exact limits and construction provenance.

The source receipt hashes the bytes read now and associates them with saved model identifiers; it does not authenticate the original executable, data or output origin.


## Saved-fit identity across package versions

The strict fingerprint of an ordinary live fit includes serialized model closures and their namespace version. An older full fit loaded into a newer namespace can still receive `E_SPEC_CHANGED` before any CSV read, even when its RDS bytes are unchanged. No force or reseal option is provided.

For supported post-processing, explicitly export with `portable_fit(fit, budget)` while the original seal still verifies, then save/load the detached object. Export before upgrading when the producing version provides this function. Producers that predate it additionally need a separately supplied reviewed exporter in their original serialization environment. Detached contexts preserve their source paths and do not bundle or relocate CSVs. See [PORTABLE_FITS.md](PORTABLE_FITS.md).

The earlier live-fit comparison used the original producer namespace with new diagnostic source functions. The later detached check exported under the original 0.0.20.9003 namespace and used the detached context with current consumers. These are distinct scoped checks; neither promises arbitrary upgraded live-fit portability or refitting.
