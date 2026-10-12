# Explicit person, correlation-coordinate and augmentation IR

Version 0.0.13.9008 exposes a scoped experimental runtime profile for the conjunction of a sample-centered roster, task-Cholesky augmentation and ordered raw-B priors. Independent retained fixed-state evidence covers 20 configurations and 1,794 derivatives, including keyed original/generic Exp1 correspondence. Other combinations retain separate R/source evidence without inheriting this runtime profile. This is bounded numerical evidence, not HMC, recovery or inference validation.

The separate centered/task ordinary final-marginal common profile is described in CENTERED_FINAL_MARGINAL.md. It supports selective zeros with a joint final-R determinant density and MGS chart; it does not inherit the ordered raw-B scientific prior or its exact reference-model equivalence.

The existing `bmf()` accepts:

```r
m <- bmf(
  components = named_components,
  decomposition = named_owners,
  cor = list(default = "free", coordinates = list(
    type = "ordered_blocks", blocks = list(first = ~G + P, second = ~H))),
  person = list(distribution = "sample_centered", ids = ordered_public_ids),
  augmentation = list(type = "task_cholesky", targets = target_order)
)
p <- set_prior("normal(0,0.2)", "cor_raw", block = "second")
```

Components, owners, target order and roster are explicit user declarations. Ordered raw blocks require the resolved scientific graph to be identity within blocks and free between blocks. They do not create zeros. Group expansion and arbitrary noncontiguous coordinate order are captured. Each raw B cell has exactly one zero-centered Normal prior; there is no default, common LKJ term, MGS coordinate or applied B-to-R density Jacobian. The geometric B-to-R Jacobian is generally nonzero and is not applied under a prior directly on B. Parameter-residual correlations retain their separately declared law.

A sample-centered roster requires at least two distinct plain character IDs, supplied before exclusion in the public shared/crosswalk person-ID domain. Declared unobserved people remain. The centered covariance is H=N/(N-1)(I-11'/N), represented by metadata rather than an automatically allocated dense matrix. The default iid path remains valid at N=1.

The first new augmentation supports centered persons plus task-Cholesky only. Its active target domain is the subject-task rows with potentially nonzero symbolic common/residual support. Exact fixed-zero/basis-zero mean-only rows retain their means, noise and observation roles but have no task white. A free coefficient at numeric zero remains active. Each active row needs explicit positive parameter-residual coverage for this initial positive-definiteness certificate. A selector must cover that domain exactly in order; `targets=~0` denotes an empty domain, while omitted/NULL targets use recorded canonical active order.

There are J_active*(N-1) task white coordinates and no common/residual person coordinates under task augmentation. Raw white coordinates index an isometric sum-to-zero basis, not individual people. Constrained scores index the roster. White variance is N/(N-1); the Stan2.40 positive-Helmert convention is recorded. No extra density or transform determinant is added for task deviations. Common/residual generative scores are unsampled and unavailable under this augmentation.

The R state map forms normalized common/residual Gram factors directly. Whole-row share owners use row scale d=total_sd; unowned rows use d=1. Owned normalized expressions are constructed before multiplying by the total SD. Required normalized Cholesky, task deviations and received operands must be representable. Full covariance/loadings may be unavailable as optional reports, with explicit flags and log magnitudes; no jitter, clipping or invented structural zero is used. Adaptive unowned row normalization remains outside this increment.

Inspection separates posterior normalizability, likelihood identification and numerical/sampling evidence. Centered active-person models do not inherit iid no-overlap/no-repeat/collapse proofs. Exact covariance-preserving ambiguity proofs remain applicable. An empty active domain has an explicit no-person-effect diagnostic certificate, so adding centered metadata cannot erase applicable Gaussian mean/noise proofs. A narrow exact full-M3/canonical-parameter/valid-route/proper-prior certificate establishes 0<Z<=1 relative to normalized prior and PMFs. It does not establish cognitive identification or finite-precision/HMC behavior; overrides and other kernels remain unassessed by that certificate.

Prepared schema `reference-law-spec-v1` requires explicit rebuilding from the original declaration, observation data and intended priors. Older fingerprints are never resealed. Default scientific data, priors, source and numeric starts preserve the released default behavior; singleton initializer vectors now explicitly retain declared JSON rank. Covered requests require `execution="experimental"`; exploratory flags cannot bypass proven failures. The explicit `resources="development"` preset retains caps of 30 roster members, 1,000 consumed rows, 12 logical predictor targets, 12 combined covariance coordinates, and bounded chains/iterations/threads and output size. Larger requests require an explicit caller resource policy as described in RESOURCE_POLICY.md. Resource admission does not expand scientific eligibility or the retained native evidence, including the 27-target Exp1 manual fixed-state gate. Fully fixed sampling raises E_NO_FREE_PARAMETERS, while empty active domains with free means/noise remain distinct.

In raw-mode task augmentation, the common Gram factor uses the ordered block factor scattered by rows into global component order. Its columns retain the ordered whitening axes. It is a square covariance factor, not a positive global Cholesky. A fresh Cholesky of rounded global R is not required on this task path. The existing final-mask and iid/common-score conventions remain unchanged; a rounded stored R is not thereby numerically PD-qualified.


Cached summaries preserve the full roster, task order, ordered raw blocks and resolved priors. Raw-B primitives and derived R are distinct; task-white basis coordinates, centered whites and task deviations are distinct. Optional unavailable covariance/loading reports preserve draws, flags and log magnitudes. The Stan 2.40 unconstrained-gradient CSV label issue uses an explicit packed semantic mapping in validation; constrained/GQ output labels were separately verified and are not permuted in caches.

The retained native gate verified stanc --O0 and the sole model-translation-unit C++ -O0 flags. Linked libraries remain separate. Original header provenance records deterministic regeneration after make removed the intermediate header; the successful binary was unchanged. Reference compilation removes actual inherited optimization tokens while preserving quoted macro contents, restores the environment on exit, and uses a distinct cache identity. Custom toolchain commands still require their own verification.
