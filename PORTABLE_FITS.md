# Explicit detached fit export

The iid Gaussian product-affine saved-value profile supports passive reports, component draws and individual summaries. A separate current 0.0.35.9001 R-only reader admits metadata planning and explicit population/conditional PPC for complete existing product capsules and relocated archives; diagnostic refresh remains closed. Historical producer capabilities and no-PPC notices are unchanged. See [PRODUCT_FIT_ARCHIVES.md](PRODUCT_FIT_ARCHIVES.md) for the producer boundary and [SAVED_PRODUCT_PPC.md](SAVED_PRODUCT_PPC.md) for current reader integrity, budget and evidence limits. Legacy profile behavior described below is unchanged.

`portable_fit(fit, budget)` exports a detached context for the existing diagnostic,
individual-summary and supported predictive-check interfaces. Bounded R-only
export, consumer and cold-report checks passed. Exact archive and installed-package
results are recorded separately with the release artifacts; these checks do not
establish sampling quality or scientific validity.

An ordinary saved fit can contain model closures, backend objects and namespace
references. Its specification fingerprint depends on the original R serialization
context. Loading it after a package upgrade can therefore fail the strict
fingerprint check. A fresh fingerprint is not evidence that old draws belong to
a changed specification, and this operation has no force or reseal option.

Export with a package version that provides `portable_fit()` before changing
environments, while the original seal and saved fit/model association still
verify. The result is a distinct `bmeas_portable_fit` with a declarative context,
retained artifact/source bindings, protected original history, compatible cached
reports and an export receipt. It has no live backend, functions, environments,
external pointers or unevaluated language. The original fit is unchanged.

```r
# Illustrative caller allowances; choose them for the retained context/cache size.
portable <- portable_fit(fit, budget = list(
  max_context_bytes = 8 * 1024^2,
  max_context_nodes = 1000000,
  max_depth = 64
))
saveRDS(portable, "fit-postprocessing.rds")
portable <- readRDS("fit-postprocessing.rds")
```

The example performs no fitting and saves no user output file automatically;
the explicit `saveRDS()` call writes that file. Export itself uses owned temporary
files for original-seal verification and canonical hashing. The existing diagnostic,
individual-summary and canonical basic-PPC consumer routes are described below.
Fitting, model-source generation and arbitrary saved-method execution remain
unavailable on the detached object.

## What moves

The detached object preserves admitted data, ordered subjects/targets/maps,
constraints, priors and coordinate measures, law declarations, source/resource
pins, source paths, and original scientific/execution/inference/numerical history.
Known factor/ordered columns retain codes and levels. Known provenance timestamps
become inert epoch/class/timezone records. Only the seven inventoried execution
callback slots are omitted after original-seal verification; their presence and
type remain recorded. Unknown executable content or unsupported schema/class
combinations refuse the whole export.

Compatible cached tables and arrays are copied under the export allowance. The
historical attribute-free `num_args = list()` metadata is preserved only on the
`cache$parameter_summary` data.frame root. Cached tables and arrays may already
contain posterior-valued entries. Conversion performs no new
posterior acquisition and never opens, probes, copies or relocates the CSVs.
Moving the RDS alone does not make unavailable paths usable. This first profile
offers no source-path rebinding or CSV bundle.

The immutable core has a versioned canonical MD5 identity using
`tools::md5sum`. It detects non-adversarial changes to the admitted representation;
it is not a signature, a collision-resistant security claim, or authentication of
original posterior origin. Export-time hashes of retained code/data cannot prove
which input bytes a historical compiler or sampler actually used. Existing
fit-time identities retain their original availability and limitations.

## Allowances and costs

`max_context_bytes` and `max_context_nodes` are required finite exact positive
integers, at most 2^53 - 1. `max_depth` defaults to 64 and is at most 2^31 - 1.
The complete detached representation, including compatible caches and the
generated receipt, is counted before expanded copying. Repeated shared values
count as repeated occurrences. The receipt records exact canonical bytes, nodes,
depth and reserved logical hash-file work. Its accounting fields use fixed-width
double encodings so filling the receipt cannot increase the admitted length.

Production performs four canonical traversals: a complete preflight count, a
complete copy into fresh storage, an immutable-section write, and a final
complete count. The receipt's three named pass counters describe the count,
final validation and encoding passes; they do not include the additional copy
traversal or claim to enumerate all schema checks or original-seal work.

These are representation allowances, not hard RAM, disk or wall-time limits.
Reproducing the old seal first serializes the original spec through its existing
protocol, with separate unknown serialization and temporary-storage costs.
Inspecting/materializing an existing R representation can involve R/native work.
Later user `saveRDS()` compression and file size are outside the canonical quota.
No backend or model callback is deliberately invoked by the package.

Strings are stored as valid UTF-8 without Unicode normalization. Byte-marked
character text refuses. Native non-ASCII text requires an exact conversion
round-trip under the captured locale; it is never guessed or transliterated.
Output atomic and attribute vectors are newly constructed ordinary base storage.
Unsupported representations refuse instead of being silently flattened.

## Older fits

An older fit must be loaded in an evidenced producer/serialization context where
its original seal actually reproduces. A producer version that predates
`portable_fit()` does not gain that function merely by restoring its environment.
It additionally needs a separately supplied, reviewed exporter helper that runs
there without replacing the original namespace. This release does not install
or select such an environment or provide an automatic legacy migration command.

The retained historical check used the original 0.0.20.9003 namespace with the
reviewed exporter source, then used the resulting detached context with current
post-processing functions. That establishes the scoped detached route, not
arbitrary live-fit upgrade compatibility. A helper cannot repair a fit already
resolved through a newer namespace or substitute a newly computed seal. If the
original seal or required exporter context cannot be verified, export refuses.

This explicit conversion does not add unbudgeted capture to future fits and does
not make ordinary full-fit RDS files independent of their retained dependencies.


## Explicit active consumers and passive reports

Use the existing `refresh_diagnostics()`, `individual_parameter_summary()` and
`assess_measurement_fit()` signatures and operation budgets. Each portable call
first validates the exact stored finite context policy, counts and validates the
whole immutable profile, checks ancestry/source bindings, and verifies its
canonical MD5 with the qualified owned-file transaction. It does not rerun old
formula, basis, prior-selector or prepared-spec serialization seals. There is no
public coercion to a live or prepared specification.

The existing scientific scopes stay in force: population-monitor keys and
aliases; individual predictor/natural/received scales and fixed/shared/person
roles; canonical Gaussian, binomial-logit or lognormal PPC descriptors
with package-owned kernels; and the separate saved single-block pinned M3-CS
reader with an explicit matching rng_v2 simulator. See SAVED_COGNITIVE_WORKFLOW_zh.md. Typed/routed models do not gain basic PPC from their
names or omitted callbacks. Same-N centered-roster laws and positive-Helmert
normalization, raw-B/final-R measures, sign orientation and declared priors remain
distinct and unchanged.

An active source receipt contains `context_validation` with a separate immutable
policy, byte/node/depth counts, completed count/schema/encoding phases, closed-file
bytes written and one logical MD5 read reservation. Context costs do not replace
or inflate the CSV, numeric-buffer, metric or simulation budgets. A refusal
before admission has no trusted input-derived identity/policy; later errors or
ordinary R interrupts preserve the completed prefix and original condition.
External termination cannot guarantee a returned receipt. No-acquisition individual output and failed PPC
attempts also retain their context phase without implying a posterior read.
These representation controls do not bound total R/native allocations or RSS.

`diagnostic_parameters()` and `ppc_plan()` remain metadata-only and explicitly
report that their call did not verify integrity. PPC planning still honors saved
protected proved-improper/non-normalizable statuses after local record-shape
checks; it does not recompute those proofs. `summary()`, `fit_checks()`,
`parameter_summary()`, `posterior_summary()` and printing use saved evidence;
none creates a digest file, acquires a source, calls a provider or computes new
posterior metrics. Structurally valid prior verification receipts are labelled
retained, not newly verified. Malformed saved receipt evidence is unavailable.

New diagnostic/PPC output updates only a copied mutable cache. Original
scientific checks and computation/numerical/inference history remain in the
immutable core and are visible in reports. Original `proven_improper` or
`proven_non_normalizable` evidence retains the existing refusal even after a
mutable cache is edited. Other original warnings are not converted into a new
inference gate or a favorable conclusion.

Later active calls freshly verify the same immutable target. A shallow cache
container check and reached local validators govern current cache evidence;
there is no new whole-cache canonical/schema pass over unrelated retained
posterior arrays. New context receipts are validated in their reached existing
source-receipt slots. The original export receipt remains producer history:
its complete-payload count is not a limit or current-size assertion for a grown
mutable cache. Its saved counters must still fit the original finite policy and
contain the immutable child, using fixed metadata comparisons only. Immutable
digest, policy and immutable counts stay unchanged.

Original paths cannot be rebound. The qualified producer records original file
hashes only as absent or `not_collected`; this portable consumer profile accepts
those unavailable forms (including an explicit NULL hashes field). Present hashes,
contradictory availability or unknown positive statuses refuse active use as
unsupported retained evidence. Their original records are preserved. No positive
fit-time provenance schema is invented from the existing reader's separate MD5
argument; that reader interface is unchanged for future reviewed producer
metadata. Previously unsealed output metadata is not authenticated by conversion,
and an export-time identity is never relabelled fit-time evidence.

The bound `stage1_export_only_consumers_closed` marker describes the producer
stage and is preserved. Current support is shown separately as
`portable_postprocessing_v1`, under the exact known schema/profile/interface/
exclusion lists. The earlier pending-support label remains readable in saved
reports as historical metadata; neither label is a fresh integrity check. Unknown/disabled profiles refuse.
Caller-supplied private views or detached scientific-context objects refuse at
public consumer entries; every active call needs the complete portable envelope.
Fitting, model rebuilding, `check_model()` reevaluation, source/data generation,
backend/callback operations and source-path rebinding remain unavailable.
