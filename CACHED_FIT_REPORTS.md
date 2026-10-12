# Read saved fit evidence

For a fit created by `bmeas()`, these calls use its saved cache only:

```r
summary(fit)
checks <- fit_checks(fit)
parameter_summary(fit)   # exactly the same table as posterior_summary(fit)
checks$context$blocks
checks$sampling$extrema$rhat
checks$sampling$chains
checks$domains$predictive_fit
```

A prepared `fit=FALSE` context is not a fit: use `check_model()` for its prior, structure, identification and capability checks.

The report separates five kinds of evidence. Block context gives model/version, units, consumed likelihood rows, observed people, roster and missingness. Sampling diagnostics show cached Rhat, bulk/tail ESS, per-parameter mean MCSE and chain diagnostics. Predictive fit reports only predictive evidence actually retained. Identification and posterior normalizability/propriety retain their own status and scope. Numerical screening and optional-report availability remain separate from both model fit and sampling quality.

Rhat/ESS/E-BFMI are sampling diagnostics, not measures of agreement between the model and observations. Finite values or a completed native calculation do not establish good model fit. This report applies no new convergence threshold or global verdict. Explicit basic-family predictive checks are described in BASIC_FAMILY_PPC.md. This report reads their saved result only; no PPC, LOO, WAIC, refitting or posterior-file reading is triggered.

`sampling$parameters` is the unchanged cached table. `sampling$extrema` retains all tied rows with their semantic keys, roles and variable names. Coverage counts are cached rows, not independent scientific unknowns. Computational and latent diagnostics remain visible. A recorded infinite Rhat is preserved; missing and invalid values do not turn into favorable extrema. ESS minima are explicitly over finite nonnegative entries, with all other values retained in the table and coverage.

`sampling$chains` preserves per-metric vectors. Unequal vectors are never recycled. An absent chain identity is shown as unavailable with a stored-row index, not guessed from configured chains. Configured iterations/counts and actually retained observed run counts remain distinct. The explicit `resources="development"` preset preserves the earlier engineering run limits; caller resource policies are described in RESOURCE_POLICY.md.

`mcse_mean` is Monte Carlo error of the posterior mean, not of a quantile or interval endpoint. The actual cached quantile columns are listed, including historical alternatives; an interval estimator/version is unavailable if it was not recorded. No cross-parameter MCSE ratio or universal scale comparison is constructed.

Every domain distinguishes a retained record (including a recorded `unsupported`, `not_assessed` or failure) from evidence absent in an older cache. Missing historical evidence does not establish that a check was never attempted. An empty recorded table is not the same as an absent table. Reading does not validate or reseal old prepared contexts or grant them a newer execution profile.

For historical `measurement_fit` objects, `fit_checks()` retains its original PPC-cache return, including NULL and its existing metadata annotations. Use `bmeas::fit_checks(fit)` when another attached package masks the exported helper. Current bmeas-leading fit classes retain their S3 printer; old objects are not reclassified to force dispatch. `correlation_draws()`, `individual_parameter_summary()`, `ppc_plan()` and `assess_measurement_fit()` keep their separate draw/simulation boundaries.

Malformed optional context is marked unavailable with a field-level reason; valid cached sampling evidence remains usable. Optional interval or summary annotations use `unavailable_invalid_metadata` when their interpretation is invalid, distinct from an absent annotation. The actual named quantile columns remain visible without inferring estimator details. These metadata states do not rewrite any saved diagnostic or domain status. Missing context does not imply zero people/rows or iid law; an explicitly saved empty collection retains its zero count. Old serialized summary objects without the new report remain printable through their saved cache/run only, with unavailable additional context and unchanged classes/bytes. The recognized cached numerical-message count remains visible, with incomplete coverage and unknown total failed evaluations. Ambiguous required cache or metric schemas still receive a typed reporting error.

## Numerical-message evidence

`fit_checks(fit)$domains$numerical_evaluations$interpretation` interprets the
already-retained numerical-message record. `summary()` and both fit/checks
printers use the same passive projection.
The original record remains unchanged in `domains$numerical_evaluations$record`.

A historical `observed_failure_messages` value is shown as a **recognized cached
message-line count**. A stored zero does not establish that all output was
captured or all numerical exceptions were recognized. Missing historical capture
or recognition provenance stays unavailable; no modern scan is inferred.
Coverage is incomplete and the total number of failed numerical evaluations is
unknown in every branch.

New caches use the single existing fit-time output-method call. They retain its
success, error or unusable-text status and the recognition rule. Successful empty
text has zero recognized lines. Unavailable capture has an unavailable count.
Recognition is the positional union of the existing structural messages and
literal `Exception:`. A line matching both categories counts once in the union;
category counts can overlap. Equal repeated lines, including repetitions across
printed and returned channels, remain separate captured occurrences. None of
these counts identifies distinct failed proposals or all sampler evaluations.

A recognized line or unavailable capture produces a computation warning; existing
summary/HMC cache errors retain failed precedence. Inference, identification,
propriety, sampler metrics and predictive results are not reclassified by this
message record. Malformed optional metadata is unavailable locally and does not
erase otherwise valid cached evidence. Contradictory count/category metadata is
not silently repaired, including when optional capture provenance is malformed.
Invalid text encodings are unavailable before recognition or metadata lookup;
valid text is preserved without replacement. Duplicate numerical domains in a
saved report are unavailable locally, just like duplicate fields in a fit cache.

Reporting performs no output-method call, posterior/file read, model validation,
simulation, RNG operation or new diagnostic threshold assessment. The numerical
projection exposes `recognized_lines`, `output_capture`, `recognition`,
`category_counts`, `coverage` and `total_failed_evaluations`. Its interpretation
schema is `bmeas_numerical_message_report_v1`; versioned producer records use
`bmeas_numerical_messages_v1`.

## Explicitly deferred reporting

A main fit requested with a caller resource policy and `cache="deferred"` retains a separate posterior-reporting record. Sampling completion and posterior-diagnostic completion are separate: parameter summaries, HMC diagnostics, matrix screens and numerical-message capture are deliberately uncomputed. Passive `summary()` and `fit_checks()` show that policy reason; they never start the deferred work or fill missing evidence with zero. Configured saved-state counts are not observed CSV counts. The existing numerical-message producer-v1 record is not given a new deferred capture status.

The five report views and historical cache meanings are retained. The explicit selected-population/HMC refresh described below supplies a later diagnostic acquisition path, while numerical-message capture and matrix/other reports remain separate. See RESOURCE_POLICY.md for fitting policy and SCALABLE_POSTERIOR.md for the legacy and opt-in streaming consumer boundaries.

The deferred guarantee covers bmeas post-return work and posterior/sampler-diagnostic draw arrays. The pinned CmdStanR constructor still scans complete CSV files for metadata and retains header/adaptation values before returning. That metadata is not observed posterior-row validation or a computed HMC diagnostic report. Provider-construction failures retain their cause/location and leave sampler completion unknown; see RESOURCE_POLICY.md.


## Explicit refresh and selected coverage

`diagnostic_parameters()` discovers readable population keys without reading draws. `refresh_diagnostics()` is the separately requested, finite-budget acquisition/computation step. Its returned copy replaces the selected parameter table and HMC evidence atomically. The optional v2 reporting record labels parameter, HMC and other cached phases separately; selected-monitor extrema are not the worst of every model variable. Local undefined metrics, missing sampler fields and malformed optional metadata remain explicit. Existing summary, print, parameter-summary and fit-check accessors stay passive. See DIAGNOSTIC_REFRESH.md for the short workflow and saved-fit compatibility limit.
