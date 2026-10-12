# Explicit larger main-API predictive requests (0.0.29.9001)

Current saved-reader supplements: new-profile two-block M3-CS PPC is described in [SAVED_JOINT_M3CS_PPC.md](SAVED_JOINT_M3CS_PPC.md) (0.0.34.9000); the separate reader for complete existing Gaussian-product capsules and relocated archives is described in [SAVED_PRODUCT_PPC.md](SAVED_PRODUCT_PPC.md) (0.0.35.9001). Both have R-only engineering evidence and do not upgrade historical producer capabilities. The 0.0.32.9002 single-block saved boundary below is historical; complete-design budgets and the planning/assessment distinction remain in force.

Current 0.0.32.9002 supplement: exactly two live pinned M3-CS blocks use one joint state and combined response call, with complete block/task statistics and budgets. See JOINT_M3_CS_PPC.md. Saved execution remains single-block only.

Finite published defaults are policy, not scientific model-dimension limits.
Main-API PPC accepts explicit higher replication, observation, simulation,
latent, statistic and pair allowances subject to checked representability and
allocation admission. More than 20 replications requires the explicit posterior
streaming policy. Omitting `posterior` preserves legacy acquisition: one or two
files, 10 MiB, 100 retained rows, 50,000 required values and at most 20 selected
states. No acquisition route changes silently.

## Plan before explicit assessment

For an existing supported main-API fit, construct a finite budget and inspect
its plan. The following illustrative allowances cover 21 replications of at most
1,001 single-cell basic-family response rows, subject to the fit's actual latent,
statistic, pair, source and allocation needs:

```r
budget <- diagnostic_budget(
  max_draws = 21, max_observations = 1001,
  max_simulated_values = 21021,
  max_latent_values = 30000, max_statistics = 300,
  max_pair_terms = 1000000, max_work_values = 2000000,
  posterior = list(reader = "streaming",
    max_source_bytes = 128 * 1024^2,
    max_read_bytes = 7 * 128 * 1024^2,
    max_materialized_values = 2000000))
plan <- ppc_plan(fit, draws = 21, budget = budget, level = "population")
plan$allocation_accounting
plan$budget_origin
```

The numbers are illustrative, not sufficient for every fit or a memory guarantee.
The three leading arguments are required; `diagnostic_budget()` has no zero-argument
form. Optional predictive defaults remain 10,000 latent values, 200 statistics,
50,000 pair terms and 1,000,000 work slots. Use plain finite integer-valued
allowances within checked representation. Do not combine `posterior` with the
outer legacy source/row/value fields. Reader quotas remain independent of
predictive-work allowances and fitting resource policies.

M3-CS has ten response cells per row: 21 replications of 1,001 rows require at
least 210,210 simulated values, not 21,021. Pass the same explicit higher budget
to M3 source/compile/data/probability helpers when preparing that design; see
M3_CS_PPC_zh.md. Planning may refuse other insufficient allowances rather than
silently reducing the request. Full designs, all declared statistics and all
required person pairs remain included. JT<=J, supported person laws and CS
category support remain scientific constraints. Budgets do not bypass them.

`max_work_values` conservatively counts predictor/generated-coordinate matrices,
covariances, scalar-name/operand maps, replicated-statistic storage, statistic
tables, person/group maps and pair-scan work. Geometry and statistic slots share
this allowance; planning checks before the corresponding expanded allocations.
Checked products refuse integer overflow. Logical counts do not bound R object,
header or string overhead, temporary copies, peak RSS or elapsed time.

For live and portable fits, planning inspects metadata without source acquisition;
archive planning additionally reopens and verifies its archive context. See
ARCHIVE_POSTPROCESSING_zh.md for archive integrity and persistence boundaries.

New plans use v3 and completed result metadata v2. Historical v1/v2 plans and
result-v1 validation retain their meanings. Source receipts retain their schema;
larger selected-row counts are admitted by the validated parent result profile.

## M3 simulator identity and evidence

`bmeas_m3cs_rng_v2` removes only the development O<=1000 Stan data ceiling.
Existing v1 executables are not silently reused. Assessment passes its admitted
policy through the M3 helpers and never compiles implicitly.

A separate tiny native gate passed exactly one compile and two fixed-parameter
invocations: O=1 and O=1001, each one chain, one draw, zero warmup, zero model
parameters and one thread. Every row retained ten probability/count coordinates,
valid support and totals; inactive categories 5–9 stayed zero. The independent
moderate-activation probability oracle discrepancy was 2.7755575615628914e-17.
Source, resource, executable and version identities and R RNG preservation passed.
The harness was corrected to recognize both `lp__` and `accept_stat__`; the O=1
output was revalidated from saved bytes without rerunning it. The separate
read-only toolchain probe emitted a warning, while the requested model compiled
successfully without retry.

This is boundary engineering evidence, not HMC, posterior fitting/refitting,
large-study performance, calibrated fit testing or inference qualification.
Portable/archive single-block pinned M3-CS planning and explicit execution are
now supported by a separate saved reader. Its prior-source iid/centered native
seam evidence is separate from the boundary gate above and does not cover the
final all-fixed export repair; see SAVED_COGNITIVE_WORKFLOW_zh.md. Already
retained cognitive predictive evidence can still be reported passively. Package
release checks are separate from this native gate.
