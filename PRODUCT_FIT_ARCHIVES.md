# Durable experimental Gaussian product fits

Current 0.0.35.9001 reader supplement: complete existing product capsules and relocated archives admit metadata planning and explicit population/conditional PPC through a separate current reader. Diagnostic refresh remains closed. See [SAVED_PRODUCT_PPC.md](SAVED_PRODUCT_PPC.md) for its R-only evidence, integrity and budget requirements. The 0.0.33.9000 text below records the historical producer boundary, including its original no-PPC exclusions; immutable producer capabilities, history, source, paths and seals remain unchanged.

Version 0.0.33.9000 introduces a distinct saved-value context for the existing
explicit-experimental iid canonical Gaussian common/residual/dual Cartesian-product
correlation profile. It does not change any correlation chart, prior, generated
source, backend admission or scientific qualification.

Export a genuine live fit while its original specification seal still verifies:

```r
saved <- portable_fit(fit, postprocess_budget("archive_fit")$context)
archive_fit(saved, "fit-archive")
reopened <- read_fit_archive("fit-archive")
summary(reopened)
component_draws(reopened)
individual_parameter_summary(reopened, scales = "predictor",
  budget = individual_budget) # explicit finite consumer budget supplied by caller
```

The archive retains the complete CSV files and the unchanged portable capsule.
The caller chooses the destination and budgets. Export does not repair an old
live seal or invent a fit from detached metadata. Reopening does not generate
source, initialize models, compile, fit, invoke saved callbacks or use a backend.

## Exact scope

- Canonical Gaussian observations, iid people, ordinary common-plus-parameter-
  residual augmentation, and at least one saved product layer.
- Product common/residual/dual layers; remaining layers retain their existing
  MGS, fixed or one-label scientific representation.
- Passive cached reports, component population draws and individual parameter
  summaries retain their existing meanings, source checks and work allowances.
  Component draws do not become a parameter-residual correlation accessor.
- PPC planning, active PPC and diagnostic refresh explicitly refuse this new
  saved profile. Legacy portable objects retain their old supported interfaces.
- Identification, propriety, computation warnings and inference evidence remain
  historical evidence. Inference stays not assessed; no HMC or recovery claim
  follows from successful saving, reopening or postprocessing.

## Integrity and compatibility

The portable envelope and canonical byte codec remain v1. New product exports
carry context profile `bmeas_iid_gaussian_product_context_v1`, conversion profile
`bmeas_iid_gaussian_product_export_v1`, and their own immutable capability record.
The legacy context and its capability bytes are unchanged. Existing capsules are
not upgraded, resealed or assigned live classes.

Product validation checks exact record fields, component partition and unique
label coverage, shared-label unions, full fixed matrices, subdomain directions,
interval arithmetic and retained factor-certificate consistency. These are
saved-value consistency checks, not new numerical/scientific certification.
The original seal and fit/spec association are retained along with callback and
backend omission markers. The export-time digest does not retroactively
authenticate historically unsealed fit fields or synthetic CSV provenance.

Budgets remain operation-specific. An archive can relocate retained files
without rebinding original scientific identities. Mutable cached reports do not
erase adverse evidence retained in the immutable context. New evidence does not
silently grant saved objects additional execution capabilities.
