# Retain joint component uncertainty

`component_draws(fit)` extracts all retained joint states of the component correlation matrix and component SDs. It derives the corresponding covariance from those saved values, retains original availability flags, and returns stable component IDs, measurement constraints and chain/draw keys with the arrays.

Tiny R-only controls cover aligned extraction, matrix arithmetic, metadata, source precision and cold saved-result reopening. This adds no new model fit or native-model evidence.

## One state means one joint matrix

The result's correlation, SD and covariance arrays share exactly the same `draw_keys`. The draw axis follows saved-row rank and chain ID; component axes use stable coordinate IDs. Display names, expanded basis coordinates, unit/marker scales, orientation and zero/fixed/equality constraints remain explicit. No matrix is assembled from marginal means or independently selected entries. This first interface always uses all retained states and refuses an insufficient budget instead of silently thinning.

Component covariance is distinct from `Sigma_predictor`: the latter also includes the measurement loading map and parameter-residual contribution on target-predictor scale. An individual's `eta` or component score is another quantity. These are not interchangeable interpretations of a latent matrix.

## Finite defaults and explicit admission

```r
joint <- component_draws(fit)
saveRDS(joint, "component-draws.rds")
```

For advanced control, inspect a fresh complete list and edit only what you need:

```r
budget <- postprocess_budget("component_draws")
budget$max_components <- 6
joint <- component_draws(fit, budget = budget)
```

The finite defaults reproduce the complete worked example in
[MEASUREMENT_REUSE_zh.md](MEASUREMENT_REUSE_zh.md). They are starting limits for
modest fits; insufficient limits refuse without automatic growth. Explicit
lists are validated unchanged, without filling missing fields; NULL refuses.

These are illustrative allowances, not a promise that every fit fits them. Source bytes, repeated reads, numerical buffers, returned cells, matrix-check work and annotation context have separate limits. Numeric-slot and context limits do not cap total R/native allocation or RSS. Original live-specification seal serialization remains a separately disclosed cost; detached objects use their saved immutable-validation allowance.

The original posterior CSV files must still exist when extracting directly from a fit. Use `archive_fit()` and `read_fit_archive()` to preserve and reopen the complete existing CSV files with relocated paths; see [MEASUREMENT_ARCHIVES.md](MEASUREMENT_ARCHIVES.md). Saving the returned object makes these extracted arrays self-contained. It does not package other fit data or make the existing `portable_fit()` a full posterior archive.

## Preserve unavailable values and source precision

Saved `R_component` and `component_row_sd` are authoritative operands. Covariance and its log magnitude are labelled `derived_from_saved_R_and_SD`; they need not equal the separately rounded generated covariance column bit for bit. Original and reconstructed availability are separate, and the final availability requires both. A newly representable calculation cannot erase an original unavailable result.

Every matrix state remains in the result, including finite matrices that fail symmetry, constraint or positive-definiteness checks. Status reports the problem. The accessor never jitters, clips, forces a diagonal, replaces a matrix by a nearest-PD matrix, or drops a draw. A later structural calculation must state its own admissibility rule. Proven-improper or proven-non-normalizable targets refuse, including protected original evidence that later cache edits cannot remove. Poor or unknown sampler diagnostics retain their original meaning.

Known CSV precision is retained per chain; unknown precision is not guessed. Old 12-digit values cannot regain lost digits through conversion to RDS. New main-API fits use `control$csv_sig_figs=17` by default; explicit settings from 1 through 18 are supported. Seventeen digits targets double round-trip storage, not good conditioning or statistical accuracy.

Output planning reserves 32 bytes per numerical field through 12 digits and 40 above 12. The default17 planning reserve is therefore 25% larger than the historical12 reserve, while actual file growth depends on formatting and values. Headers/comments and other text are outside that heuristic; actual measured sizes remain separate. The scientific model and number of saved values do not change.

## Measurement reuse and later structure

The measurement fit keeps its user-declared joint correlation structure, including orthogonality and selective zeros. Extracted aligned matrices can support draw-wise posterior transformations such as partial correlations or standardized regression coefficients, subject to numerical admissibility. They retain uncertainty under the original measurement prior.

A new constrained structural model or new path prior defines a different statistical target. The covariance posterior is not automatically its likelihood, and scalar posterior summaries are not substitutes for joint states. No SAM structural-fitting algorithm is introduced here. Changes to measurement likelihoods, factors, loadings or identifying constraints do not silently reinterpret the saved fit.

The historical `correlation_draws()` accessor remains a separate interface for historical `measurement_fit` objects. This component accessor admits only its documented main-fit and exact portable-context profiles.
