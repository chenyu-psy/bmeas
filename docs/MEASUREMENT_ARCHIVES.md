# Retained measurement archives, development v1

`archive_fit(fit, path)` creates a new directory containing a validated,
unchanged portable capsule, every byte of every originally associated posterior
CSV, and surviving declared external resource files. A live main fit is first
converted through `portable_fit`; no force/reseal operation is available.
`read_fit_archive(path)` verifies a relocated archive and returns a plain
handle. `component_draws(handle, component_budget)` reopens/verifies that handle
and compares its final owned posterior snapshots with archive-time hashes before
returning values. Reopen a moved directory; do not edit capsule paths.

The directory holds `manifest.rds`, `portable.rds`, `draws/000001.csv`, etc., and
`resources/000001`, etc. Both RDS files are ordinary base-R serialized data,
version 3, uncompressed. The manifest identifies `bmeas_measurement_archive_v1`,
the original portable canonical identity, original ordered path associations,
member byte sizes and MD5 hashes, exact CSV column names and sampling metadata,
chain IDs and retained row counts, resource descriptor linkage, completeness,
and conservative logical I/O reservations. The capsule remains authoritative
for the entire scientific context and protected historical evidence.

Full CSVs retain all already-saved measurement, nuisance, person-state, generated
output, and sampler fields. No thinning, resampling, covariance-only filtering,
Stan execution, diagnostics, source generation, or imputing omitted output occurs.
Draw keys remain `(chain_id, saved_row)`; saved row is not sampler iteration.
Existing saved R, SD and covariance/log columns are never overwritten. Component
consumer calculations remain explicitly derived from the saved values.

## Budget

Omitting `budget` uses a finite starting list for modest fits. Inspect it with
`postprocess_budget("archive_fit")` or `postprocess_budget("read_fit_archive")`;
both return the same values in fresh ordinary lists. Edit a field and pass the
list as `budget` for explicit control. No automatic ceiling growth occurs.
Explicit lists keep their existing validation; partial lists and NULL are not
filled in. See MEASUREMENT_REUSE_zh.md for the four-line workflow and all values.

The plain named list contains (field order is immaterial):

- `max_members`: total files including manifest and capsule
- `max_member_bytes`: maximum size of any member including manifest and capsule
- `max_total_bytes`: complete archive bytes, including manifest
- `max_context_bytes`: each serialized context file size ceiling
- `max_read_bytes`: conservative logical archive I/O reservation
- `max_temp_bytes`: staging/snapshot disk reservation
- `posterior`: existing streaming posterior policy (see COMPONENT_DRAWS.md)
- `context`: existing portable context policy

There are no unbounded archive defaults. Copy buffers are at most 65,536 bytes.
Writer reservations cover two source copies, both MD5 reads, posterior validation
through the seven-pass reader, possible SHA-256 resource verification, and context
I/O. Reader reservations cover manifest inspection, member hashing, capsule read,
posterior validation, and optional resource-pin verification. Reader temporary
space includes both posterior snapshots. Component extraction then uses its own
separate numeric/context/posterior budget; archive validation does not replace it.
Portable validation has its existing separate canonical-context accounting.

RDS is for trusted, locally produced archives. `readRDS` and `serialize` allocate
before serialized-size checks can be completed. File and logical I/O quotas are
not hostile-input deserialization safety, hard heap/RSS, or elapsed-time bounds.
The canonical context policy bounds admitted data, not the R runtime allocator.

## Integrity and missing evidence

MD5 is non-adversarial corruption detection, not a signature or collision-resistant
authenticity. All new hashes describe archive-time bytes. They never become
fit-time hashes, executable authentication, or authentication of original inputs.
An independently replaced archive plus matching manifest is not detectable as
forgery. Handles pin the manifest observed when opened; extraction rejects a
changed manifest. Posterior snapshot checks also reject changes after reopening.

All required original posterior files must be present and satisfy the admitted
CmdStan 2.40 sample/HMC CSV grammar. Files are copied twice, hashes compared,
source metadata rechecked, and copied CSV layout validated. Manifest is written
last; the owned staging directory is renamed to a previously absent destination.
Existing destinations are refused. Use a single writer per destination: base-R
rename does not provide a portable adversarial concurrent no-replace primitive.
Failure removes only the invocation's owned staging directory.

Missing declared resources are marked `missing_now`, never fetched. Supported
SHA-256 pins are verified against copied bytes with the existing system hashing
helper; disagreement refuses the archive. The current portable profile admits SHA-256 pins only; unknown pin formats are
refused by capsule validation. Reopening reads resource members, never their old paths.
Exact sampler input/init JSON was not collected by the saved-fit contract. Absent
warmup and unsampled quantities are not reconstructed. Optional log likelihood
is retained only if already in the CSV. Re-execution completeness is not claimed.

Archiving/passive reopening preserves proven-adverse historical evidence; numeric
component use still applies the existing protected-adverse gate. Archive handles
do not add diagnostic, PPC, person-summary, fitting, or general SAM dispatch.
A draw-wise structural transformation is a measurement-posterior pushforward;
a new constrained structural posterior needs its own inferential construction,
including measurement priors, nuisance integration and normalizers. `lp__` or
conditional `log_lik` is not an authenticated structural likelihood.

## Qualification status

This is a development implementation, not a released or comprehensive scientific
qualification. Focused controls include the fixed K=4 two-chain/four-row fixture,
a retained genuine K=1 two-chain/ten-row fit, relocation with original fixture
CSVs unavailable, independent base-R byte/column-row checks, corruption/schema/
path/association rejection, and quota/existing-destination controls. These are bounded engineering results, not a general numerical or inferential guarantee. Additional archive controls now cover K=0 with retained files, raw-B, expanded
bases, present/missing/mismatched resource pins, and proven-improper historical
evidence. Literal R interrupt delivery and exact manifest-heavy boundary cases
remain unqualified. No new sampling or compilation is
needed for those controls.

## Example

Given an existing main or portable `fit` whose CSVs are still available:

```r
budget <- list(
  max_members = 64, max_member_bytes = 64 * 1024^2,
  max_total_bytes = 256 * 1024^2, max_context_bytes = 16 * 1024^2,
  max_read_bytes = 4 * 1024^3, max_temp_bytes = 2 * 1024^3,
  posterior = list(reader = "streaming", max_source_bytes = 256 * 1024^2,
    max_read_bytes = 7 * 256 * 1024^2, max_materialized_values = 1000000,
    max_record_bytes = 1048576, max_columns = 100000,
    max_source_rows = 100000, max_chains = 16,
    max_temp_bytes = 2 * 256 * 1024^2, max_scan_values = 1000000),
  context = list(max_context_bytes = 16 * 1024^2,
    max_context_nodes = 2000000, max_depth = 64))
archive <- archive_fit(fit, "measurement-archive", budget)
archive <- read_fit_archive("measurement-archive", budget)
# After moving the complete directory, reopen its new path with the same budget.
```

These example allowances are deliberate ceilings, not estimated fit requirements;
increase them only after inspecting the refusal and the intended evidence size.
For numeric extraction, pass a separate component budget described in
COMPONENT_DRAWS.md to `component_draws(archive, component_budget)`.


## Passive retained reports

`archive_fit()` and `read_fit_archive()` now return handles containing a detached
`report_snapshot` of the unchanged portable capsule. The disk v1 manifest and
capsule format, bytes, and portable identity are unchanged. The handle is larger
in memory because it retains that capsule.

`summary(handle)`, `print(handle)`, `fit_checks(handle)`,
`parameter_summary(handle)`, and `posterior_summary(handle)` use the same passive
portable implementations. They do not reopen, hash, scan, regenerate, diagnose,
simulate, or automatically refresh anything. Reports continue to work if the
archive or original source files become unavailable after opening. To acquire a
new snapshot, explicitly call `read_fit_archive(current_path)` again.

Summary/fit-check reports add `archive_context`; nonmissing parameter tables add
an `archive_context` attribute. Status is `retained_snapshot_not_reverified`.
A missing parameter cache remains `NULL`, exactly as for the portable fit.
Protected adverse evidence is preserved, and unavailable caches remain
unavailable. Local schema and saved-identity consistency validation is not a new
integrity check. Legacy three-field handles require explicit reopening for
reporting. Active `component_draws()` behavior and its defaults are unchanged.

## Explicit active archive postprocessing

Relocated archive handles now support the existing `refresh_diagnostics()`,
`individual_parameter_summary()`, `ppc_plan()` and basic-family
`assess_measurement_fit()` APIs. Supply the same explicit options and finite
streaming posterior budgets used with portable fits. Older reader budgets are
rejected rather than falling back to historical paths. No new default budget
presets are introduced in this slice.

Every active call (including archive PPC planning) reopens the archive, checks
its pinned manifest, and validates the original portable capsule. Relocated
member paths and archive-time MD5 pins go separately to streaming acquisition;
historical path bindings and original-fit provenance are unchanged. The
in-memory snapshot is never an immutable authority. Only its admitted mutable
report cache is carried into fresh validation. Original protected adverse
posterior evidence and existing scientific gates still apply.

Refresh and assessment return a copied archive handle with an updated report
snapshot; they never write archive members. Save the returned handle if you
want to keep those reports in memory or in your own RDS. Explicitly reopening
the archive starts again from its stored capsule's cache. Individual summaries
return their existing table type. Passive summaries use the retained snapshot,
without automatically opening sources. PPC acquisition failures retain the
existing API's failed-attempt cache semantics, rather than silently succeeding.

Canonical Gaussian, binomial and lognormal PPC and the separate saved single-block
pinned M3-CS reader are supported. M3-CS requires an explicit matching rng_v2
simulator; its bounded fixed_param response replications do execute that supplied
simulator. No implicit compilation, posterior fitting, detached backend/source/data
execution, EZ PPC, SAM or LOO is added. Full/routed M3, multiple/mixed cognitive
blocks, SDM and arbitrary adapters remain refused. See SAVED_COGNITIVE_WORKFLOW_zh.md.
