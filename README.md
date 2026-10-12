# bmeas

Experimental Bayesian hierarchical measurement modeling in R.

## Install the current development snapshot

This version is **0.0.47.9000**, an experimental development snapshot. There is no
GitHub Release yet. Install this dedicated version branch with R >= 4.1.0:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("chenyu-psy/bmeas", ref = "work/current-0.0.47.9000")
```

For preparation and inspection, only R's standard `stats` and `utils` packages
are required. Optional features use `posterior`, `jsonlite`, `lavaan`, and
`cmdstanr`. Native fitting additionally needs CmdStan and a supported compiler.
Provider support is narrow and version-specific: the documented deferred profile
requires CmdStanR 0.9.0 and CmdStan 2.40.0 on supported POSIX platforms; the optional
lavaan backend requires lavaan 0.7.2. Installing newer versions does not establish
compatibility. Start with `fit = FALSE` and read the relevant capability notes
before fitting. No installation command here installs CmdStan or runs a sampler.

Formal Bayes factors and density MCSE are not implemented. Synthetic engineering
tests and mocked providers do not establish native execution or scientific
validity. See [current capabilities](CURRENT_CAPABILITIES_zh.md),
[resource policies](RESOURCE_POLICY.md), and [migration](MIGRATION.md).

## Prepare a model without sampling

```r
library(bmeas)
d <- data.frame(person = 1:4, x1 = c(.2, .8, .3, 1),
                x2 = c(.7, .6, -.3, .1), x3 = c(-.2, .6, .9, .4))
spec <- bmeas(bmf(G = ~ x1 + x2 + x3), d, subject = "person", fit = FALSE)
check_model(spec)
```

## Development status

Current source is maintained on `work/current-0.0.47.9000`. Changes to `develop`
and `main`, pull requests targeting them, and the first publication require the
maintainer's explicit authorization. No current version is published to CRAN.
The inherited DESCRIPTION author/contact fields are prototype metadata.

## Detailed feature history

> .39 adds optional deterministic single-coefficient KDE diagnostics through `structural_sddr()`. Formal Bayes factors and density MCSE remain unavailable. See `STRUCTURAL_SDDR_DIAGNOSTICS_zh.md` for scope, evidence binding and limitations.

# bmeas

The unpublished 0.0.47.9000 candidate inherits the .38 optional version-pinned lavaan covariance-regression/path backend after the .36 population contract/analytic mapping and .37 narrow native joint prior generator. The same deterministic specification maps prior and posterior draws; saturated regression and constrained covariance projection are labelled separately, and failed/boundary draws remain visible. The .39 S4a increment provides single-coefficient point-KDE/log-density-ratio diagnostics; formal Bayes factors and density MCSE remain unavailable. The interface is restricted to the documented covariance mapping scope. The inherited callback capture repair supports normal and keep.source installs. See [current capabilities](CURRENT_CAPABILITIES_zh.md), [lavaan scope](STRUCTURAL_LAVAAN_zh.md), and [module structure](STRUCTURAL_MODULES.md). Focused and exact-tar qualification receipts remain separate.

The isolated 0.0.35.9001 R-only candidate adds a separate current Gaussian-product PPC reader for complete existing product capsules and relocated archives. Both predictive levels are supported; historical producer capabilities and no-PPC notices are preserved, and product diagnostic refresh remains closed. No full-package, native or inference qualification is claimed. See [SAVED_PRODUCT_PPC.md](SAVED_PRODUCT_PPC.md).

The isolated 0.0.34.9000 candidate adds new-profile saved two-block M3-CS PPC. Legacy capsules are not upgraded. This increment is R-only engineering work; full-package and native saved-joint qualification remain separate. See [SAVED_JOINT_M3CS_PPC.md](SAVED_JOINT_M3CS_PPC.md).

bmeas is an R package designed for Bayesian hierarchical measurement modeling. It combines flexible response models, including standard statistical distributions and cognitive-model likelihoods, with user-specified latent measurement structures. Individual-level model parameters, factor loadings, latent traits, and their correlations are estimated jointly from the observed data, allowing parameter-estimation uncertainty to be carried through the measurement model. The framework accommodates structures ranging from simple factor models to decompositions of individual differences into general and domain-specific components.

Use `bmeas()` to prepare or fit a model and `bmf()` to declare its measurement structure. The older `bhmm()` and `bhmf()` names remain callable aliases in this package. See [MIGRATION.md](MIGRATION.md) for saved-object compatibility.

The current implementation is experimental. Its documented execution profiles, numerical evidence, and unresolved statistical limits remain separate from this modeling scope. Fixed/equal marginal correlations support inspectable affine maps and an explicit experimental iid runtime subset with fully fixed, one-label and exact independent one-label-product layers; see [AFFINE_CORRELATIONS.md](AFFINE_CORRELATIONS.md) for its evidence and limits.

Version 0.0.16.9005 adds an explicit experimental profile for the existing sample-centered/task-Cholesky/ordered raw common law with a separate fixed or one-label residual correlation. Bounded Gaussian and pinned CS/SDM fixed states passed independent density, derivative and output checks; see [CENTERED_AFFINE_RESIDUAL.md](CENTERED_AFFINE_RESIDUAL.md).

The 0.0.17.9005 release adds an explicit experimental profile for centered/task models with ordinary final-marginal common zero/free graphs, including selective cross-parameter zeros. Residual MGS, fixed and one-label correlation layers can be combined with this profile. Its joint final-R prior remains distinct from ordered raw B. Bounded native checks cover 14 configurations, 412 derivatives and 264 normalized likelihood rows; see [CENTERED_FINAL_MARGINAL.md](CENTERED_FINAL_MARGINAL.md).

Saved main-API fits now support `fit_checks()` and `parameter_summary()` as well as `summary()` / `posterior_summary()`. They expose cached tail ESS, mean MCSE, E-BFMI, keyed extrema and block/missingness context while keeping predictive fit, identification and normalizability distinct. No backend or posterior-file access, LOO or predictive simulation is triggered; see [CACHED_FIT_REPORTS.md](CACHED_FIT_REPORTS.md).

The 0.0.20.9005 reporting amendment makes numerical-message coverage explicitly incomplete and total failed evaluations unknown. Historical counts retain their original values and unknown provenance. New caches distinguish output-capture failure from successful empty output and recognize generic Stan `Exception:` lines as well as the existing structural messages. These are captured-line occurrences, with potentially overlapping categories, not a count of distinct failed proposals. The amendment adds no sampling or scientific qualification.

The 0.0.21.9003 resource-policy increment adds non-executing main-API planning and finite caller policies. Former tiny limits are an explicit development preset. Caller-configured fitting uses deferred diagnostics while a conservative general eager-cache estimate remains unavailable. The pinned provider still scans CSV metadata during construction; deferred does not mean zero I/O. Model support, resource admission and inference evidence stay separate; see [RESOURCE_POLICY.md](RESOURCE_POLICY.md). Independent R-only source review has passed; exact archive and installed results are recorded separately with the release artifacts.

The 0.0.22.9005 R-only source increment adds an opt-in streaming posterior policy inside existing consumer budgets. It validates complete multiple-chain sources while materializing selected columns/states, with explicit byte/read/buffer/work limits. Individual summaries default to all rows or accept a labelled balanced subset. The original PPC development limits described by that release are superseded by the explicit allocation-aware policy below. Legacy bounded consumers remain separate. See [SCALABLE_POSTERIOR.md](SCALABLE_POSTERIOR.md). Independent R-only source review has passed; exact archive and installed results are recorded separately with the release artifacts. No new fitted-model or inference qualification is claimed.

The diagnostic increment adds `diagnostic_parameters()` and explicit `refresh_diagnostics()` for deferred fits. Users select readable population keys and provide separate source/read/row/monitor/FFT allowances; passive reports remain passive. Bounded R-only source/lifecycle checks passed; archive and installed qualification are recorded separately. See [DIAGNOSTIC_REFRESH.md](DIAGNOSTIC_REFRESH.md) for the example, unknown total-memory costs and the strict saved-fit namespace-version limitation. This adds no new fitted-model or inference qualification.

Use `portable_fit(fit, budget)` before changing environments to save a detached context for diagnostic refresh, individual summaries and supported basic-family or single-block pinned M3-CS PPC. Active operations freshly validate its immutable context under a separate finite allowance; discovery/planning stay structural and reports stay passive. Original source paths and protected history remain intact. CSV relocation, refitting and arbitrary live-fit migration are outside this interface. Bounded export, active-consumer and cold-report checks passed; archive and installed results are recorded separately. See [PORTABLE_FITS.md](PORTABLE_FITS.md) for save/load usage, older-producer requirements and context/consumer costs.

The earlier component-draws increment added `component_draws(fit)` for aligned component correlation, SD and covariance states. It preserves the measurement model's declared zero/fixed/equality constraints and labels. Covariance is explicitly derived from the saved correlation and SD precision; every state and original availability flag is retained. New main fits default to 17-digit CSV output, with an explicit `control$csv_sig_figs` override. Its engineering checks are recorded with the release artifacts; see [COMPONENT_DRAWS.md](COMPONENT_DRAWS.md). It does not fit a structural model or turn the covariance posterior into a likelihood.

Explicit basic-family predictive checking is available via `ppc_plan()` and `assess_measurement_fit()`. Choose population or conditional replication, an explicit seed and a bounded diagnostic budget. Population checks preserve either iid people or the complete same-N centered group. Saved summaries remain passive; see [BASIC_FAMILY_PPC.md](BASIC_FAMILY_PPC.md) for acquisition, cost, estimands and current review scope.

The 0.0.20.9003 R-only compatibility amendment recognizes CmdStan's exact `save_warmup=false` header alongside numeric `0`; `true`/`1` and ambiguous or malformed layouts remain refused. Both bounded CSV consumers share this fix. Current source and archive qualification are separate gates.

The 0.0.20.9002 R-only development branch adds explicit `individual_parameter_summary(fit, scales, budget)` for complete person × logical-parameter estimates from bounded saved joint states. Predictor, declared natural and likelihood-received scales preserve task/fixed/shared roles and coverage. Independent R-only source review passed; exact archive/installed results are recorded with the release artifacts. See [INDIVIDUAL_PARAMETERS.md](INDIVIDUAL_PARAMETERS.md). Ordinary fit summaries remain passive.

## Explicit larger predictive requests (0.0.29.9001)

Main-API PPC accepts higher finite work allowances with checked allocation admission. More than 20 replications requires an explicit streaming posterior policy; default allowances and legacy acquisition caps remain finite. Complete designs, all declared statistics and all required person pairs are retained. See [SCALABLE_PPC.md](SCALABLE_PPC.md) for valid budget syntax, logical allocation accounting and scientific limits.

The M3-CS simulator identity is `bmeas_m3cs_rng_v2`. Its separate tiny native gate passed one compile and two single-draw, single-thread fixed-parameter runs at O=1 and O=1001. This is boundary engineering evidence, not posterior fitting, a large-study qualification or inference validation. Portable/archive single-block pinned M3-CS planning and explicit execution are now available through the separate saved reader; see [SAVED_COGNITIVE_WORKFLOW_zh.md](SAVED_COGNITIVE_WORKFLOW_zh.md). Recorded saved-reader native evidence covers iid and centered fixtures on the prior source only; all-fixed support has R-double coverage, not a native gate. Retained evidence stays passive unless an active operation is requested.

## Inherited integrated scope (0.0.35.9001)

See [当前能力速查](CURRENT_CAPABILITIES_zh.md) and [当前工作流](CURRENT_WORKFLOW_zh.md).
Live pinned M3-CS PPC admits exactly two canonical blocks with one joint latent state and one combined response call. New-profile portable/archive two-block PPC is supported by the separate 0.0.34.9000 producer/reader; legacy two-block capsules remain refused and single-block behavior is unchanged. See [SAVED_JOINT_M3CS_PPC.md](SAVED_JOINT_M3CS_PPC.md) for its R-only evidence and exact scope. Public joint probabilities are block-named matrices, and joint simulator data has an explicit row map. See [JOINT_M3_CS_PPC.md](JOINT_M3_CS_PPC.md).

An explicit experimental iid canonical-Gaussian common and/or residual-product affine profile admits multiple labels only through zero-separated one-label blocks, preserving all aliases and a separate complete-layer determinant prior for each layer. Qualified inference and general connected multilabel charts are not available. Product export/archive and retained component/individual postprocessing use a distinct saved-value profile (see [PRODUCT_FIT_ARCHIVES.md](PRODUCT_FIT_ARCHIVES.md)). The separate current 0.0.35.9001 reader supports metadata planning and explicit population/conditional PPC on complete existing product capsules and relocated archives, preserving historical producer capabilities and no-PPC notices; diagnostic refresh remains closed. See [SAVED_PRODUCT_PPC.md](SAVED_PRODUCT_PPC.md) for its R-only scope and operation-specific integrity and budget requirements. See [AFFINE_PRODUCT_CORRELATIONS.md](AFFINE_PRODUCT_CORRELATIONS.md).

The [historical structural roadmap](STRUCTURAL_SDDR_PLAN_zh.md) records earlier milestones. Current APIs include `population_draws()`, `component_population_draws()`, `component_prior_draws()`, `structural_draws()` and diagnostic-only `structural_sddr()`. See [S4a diagnostic scope](STRUCTURAL_SDDR_DIAGNOSTICS_zh.md); formal Bayes factors and density MCSE remain unavailable. Bounded arithmetic/native engineering evidence does not establish scientific adequacy, recovery, posterior propriety, convergence or cluster-scale performance.

## Prepare and inspect

```r
library(bmeas)
d <- data.frame(person=1:4, x1=c(.2,.8,.3,1),
                x2=c(.7,.6,-.3,.1), x3=c(-.2,.6,.9,.4))
m <- bmf(Ability = ~x1 + x2 + x3)
s <- bmeas(m, data=d, subject="person", fit=FALSE)
model_table(s, "loadings")
get_prior(s)
check_model(s)
stancode(s)
standata(s)
```

Wide input is Gaussian identity with unspecified response units. Explicit named `observe()` blocks carry family schemas, units, links, observation identities and shared/crosswalk person mappings. Task-vector selectors are captured at declaration. Grouped priors select independent unique coefficients; they never impose equality.

The limited formula grammar names targets and component memberships. It does not implement arbitrary regression formulas. An ordinary scalar component has unit variance. Loading sign anchors follow the resolved fixed/equal loading and correlation symmetry under symmetric priors; the general correlation parity case is exposed as a GF(2) basis. Overlapping membership adds loading cells, never duplicate likelihood rows. Correlation declarations constrain the final marginal matrix, including nonchordal zero/free graphs.

```r
p <- c(set_prior("normal(0,.7)", "loading"),
       set_prior("normal(0,.4)", "loading", target=~x1+x2))
```

`get_prior()` shows actual defaults and calibration inputs. Known basic-family defaults use response/predictor scale information; custom cognitive priors remain missing until supplied. Rules resolve by order-independent predicate refinement. Every explicit selector member must match a free role; ambiguous overlaps fail.

## Advanced modes and retained evidence


Explicit reference-law settings use the same component API: an ordered sample-centered roster, ordered raw-B correlation coordinates, and task-Cholesky augmentation. Retained independent native evidence covers 20 configurations and 1,794 derivatives, including all 798 original/generic Exp1 derivative pairs. The covered conjunction can request `execution="experimental"` with an explicit resource policy. The retained 27-target manual Exp1 comparison remains bounded pure-measurement arithmetic evidence; a larger resource allowance does not extend that qualification. No new MCMC or recovery claim is made.

```r
m <- bmf(A=~x1+x2+x3, B=~x4+x5+x6,
  residual=list(targets=~x1+x2+x3+x4+x5+x6),
  cor=list(default="free", coordinates=list(type="ordered_blocks",
    blocks=list(first=~A, second=~B))),
  person=list(distribution="sample_centered", ids=as.character(1:4)),
  augmentation=list(type="task_cholesky"))
s <- bmeas(m, d, subject="person", fit=FALSE,
  prior=set_prior("normal(0,0.2)", "cor_raw", block="second"))
check_model(s)
```

Here `d` contains repeated synthetic rows for the four declared people. See REFERENCE_IR.md for prior, coordinate and numerical boundaries. Cache summaries distinguish raw-B primitives, derived R, N-1 white-basis coordinates, N centered white scores and task deviations. Common/residual generative scores are unsampled. Declared subject-task targets and symbolically active person effects are reported separately. Fully fixed models refuse sampling before the backend.


The previous increment added marker scales: `bmf(G=~x1+x2+x3, scale=list(G=list(marker=~x1,value=1)))`. The marker fixes declared beta and adds one `component_sd` primitive; effective loadings are beta times that population SD. Inspect `component_scales`, `component_sd_calibration`, declared `loading_cells` and primitive priors. Fixed/equal constraints resolve before orientation and scale allocation. Marker defaults are explicitly parameterization-specific, and unavailable defaults require explicit priors.

The unified engine passed24 bounded fixed states,438 named gradients and4088 generated-value checks against independent Gaussian and original CS/SDM oracles (maximum discrepancy4.53e-9). This includes signed markers, mixed marked/unit columns, shared marker classes, four Gaussian replays and a full six-target cognitive direct-covariance replay. Gaussian execution retains its bounded profile; other supported compositions require explicit experimental execution. These are fixed-state arithmetic checks, not HMC or scientific validation. Pure-R covariance/rank/propriety checks use effective loadings; nonrepresentable diagnostics remain unassessed. Earlier prepared contexts require an explicit rebuild.

Posterior summaries distinguish declared beta, effective beta times SD, standardized z, scaled f, marginal R and scientific component covariance. Fixed declared beta can have an uncertain effective loading. Optional scaled-score/covariance exports retain availability flags and log magnitudes; unavailable reports do not delete draws or reject an otherwise finite target.

`check_model(s)$initialization` exposes a deterministic role-aware recipe. Marked SDs use their resolved positive-prior scales and signed marked beta starts at its prior center. Other legacy starts are preserved when finite, with resolved-prior fallback when necessary. Primitive support, inverse transforms, individual and aggregate prior arithmetic and typed received operands are screened before backend work. The fit cache records the execution-time recipe, primitive starts and exact constrained-init hash. An unsupported start raises `E_INITIALIZATION_NUMERICAL` without changing data/priors or blocking inspection/code generation. This screen does not establish finite arbitrary external likelihoods or extreme-scale sampling qualification.

## Earlier unit-SD affine release

This increment adds exact fixed/equal ordinary loadings to the common coefficient compiler. `bmf(A=~x1+x2+x3, loadings=list(equal=list(parallel=list(A=~x1+x2)), fixed=list(list(value=-.5,cells=list(A=~x3)))))` declares one shared free loading and one fixed negative loading. Inspect `loading_cells`, `loading_primitives`, `loading_constraint_rules`, `orientation`, `loading_prior_calibration`, `loading_prior_aliases` and `prior_resolution` through `model_table()`. Equality is exact numeric equality on declared predictor coordinates; it introduces one density per free class. A default tied-class prior uses the maximum member calibration scale only when every member supplies a valid calibration. Fixed loading declarations alone retain unit component variances; an explicit scale declaration adds the separate marker SD.

The affine compiler passed24 bounded fixed states,368 named gradients,2494 generated values and20 earlier-runtime comparisons, including original CS/SDM row densities. The crossed two-class native fixture is retained as an arithmetic control and is now explicitly rejected by a global column-permutation certificate; it is not an identification positive. No MCMC was added. Gaussian execution retains its bounded engineering profile; other supported typed compositions remain explicitly experimental. The changed schema requires explicitly rebuilding old prepared contexts. Wide inputs now name their numeric coordinate convention `unscaled_response`; this is not inferred physical-unit equivalence. The detailed older evidence below remains historical; exact fixture validation is separate from arbitrary-composition or scientific qualification.

This increment separates compiler support from numerical evidence. A controlled self-authored Normal adapter plus two Gaussian blocks (4 synthetic people,24 rows) passed two fixed states,44 analytic gradients and400 generated-value checks, including serial/reduced agreement; the maximum independent discrepancy was7.11e-15. One O0 build took63.96 seconds and deterministic calls0.94 seconds. This added no MCMC and does not qualify arbitrary custom likelihoods or the separate three-family composition. Multiple canonical basic blocks and explicitly registered typed custom row densities can request `execution="experimental"` through the same component compiler. The default stays conservative: a supported but unqualified composition stops before backend work unless that opt-in is explicit. Inspect `check_model(s)$backend$support` and `$qualification`; a custom density receives no native, identification, HMC or predictive validation merely from registration. Existing priors, proven identification/propriety errors, source integrity and declared resource policies still apply.

Gaussian diagnostic certificates survive multiplication by parameter-independent fixed factors. Inspect `check_model(s)$gaussian_diagnostics`: valid fixed binomial and empty factors retain the Gaussian proof. If an opaque fixed density multiplies a certified divergent Gaussian integral, `proven_non_normalizable` blocks execution while leaving that density’s value and validity unevaluated. A fixed factor cannot produce a finite positive normalizer from that divergence. Structurally constant Gaussian covariance entries do not inflate diagnostic moment counts. Every original likelihood row stays in the model, and mixed execution remains explicitly experimental.

In the component frontend, display names do not select scientific roles or information rules. The exact pinned M3 conditional density and row layout retain the zero-total-row rule independently of valid scalar parameterization; original active-option/inactive-count checks are enforced. Full scientific support, priors, links and retained-reference identity remain separate. An exactly reconstructed pinned M3 contract retains that rule, and bare registered Stan Math binomial calls retain the relational count domain and zero-trial rule through their typed argument bindings. Full family/evidence classification remains separate. A familiar name or `fit=TRUE` flag does not grant executable support.

Registered validators are eagerly compiled to avoid JIT-only fingerprint changes. Serialization-unstable captured callback environments are rejected with `E_CALLBACK_LIFECYCLE`. Even a stable serialized closure does not freeze mutable globals, namespace dependencies or stateful behavior: those custom dependencies remain caller-managed and are explicitly outside the frozen-context reproducibility guarantee. No arbitrary-code security sandbox is promised.

This version adds parameter variation levels to the same observation/component generator. Bounded deterministic checks cover 22 configurations, 274 named gradients and 1854 generated values; no MCMC was added. The Gaussian engine retains a limited fixed-state execution profile. Pinned M3-CS/SDM compositions outside the previous direct-covariance control remain explicitly experimental. `stancode()`/`standata()` expose the source and data for review.

`observe(..., parameters=list(c="task", ra=list(fixed=.5, scale="natural")))` changes a parameter's variation level without changing its observation-model role, link or density. A task predictor has one free predictor intercept per logical task. A fixed value replaces the whole parameter and receives no prior or latent score. `bmf()` can be empty for explicit all-task/fixed blocks. Existing subject-task defaults still need declared structural coverage.

Inspect `model_table(s, "parameter_roles")`, `"fixed_parameters"`, `"individual_targets"`, `"response_domains"` and `"observation_operands"`. Fixed values are converted only to the argument scale required by the density. Natural probability endpoints received through a finite logit interface are an explicit implementation limit; finite received predictors are not round-tripped through probabilities. The binder's finite representation does not guarantee that an external density can evaluate extreme values.

Logical task domains are retained from the original observation block even when responses are excluded. Empty blocks can remain alongside observed blocks, with zero likelihood rows. Unobserved free task means/scales are flagged; they receive no invented data calibration. Fixed positive Gaussian sigma is excluded from the free-noise confounding and posterior-collapse proofs. A fully fixed model has no unknowns to sample. CmdStan2.40 log_prob refuses zero-parameter models; the two fixed-only configurations were verified through normalized generated row likelihoods and their sum/sensitivity, not fixed_param lp__.

Saved contexts with an information table that no longer matches the current verified density policy require an explicit rebuild (E_INFORMATION_POLICY_REBUILD). Their original tables and fingerprints are never rewritten. Compatible historical canonical contexts retain their prior source/data meaning.

Older sealed contexts are not silently expanded to the new retained-domain semantics. Rebuild explicitly from the saved formula, original observation blocks/data and intended priors before using the parameter-level compiler. The original object and its fingerprint remain unchanged.

An experimental Bayesian measurement package. The main workflow is `bmf()` → `bmeas(..., fit=FALSE)` → inspect priors/model checks → explicitly fit. Scalar components, overlapping loadings, fixed numeric bases, parameter residuals and variance-share expressions feed one measurement IR and one generic Stan generator. Component names have no special scientific meaning.

The former small engineering-run limits are retained as the explicit development preset. Main fitting now requires a finite caller resource policy; fixed-state numerical checks do not establish scientific validity, convergence, recovery or general identification. Full study inference has not been run.

## Common effects and parameter residuals

The model is `eta = alpha + Lambda*z + u`. Common scores and parameter residuals are independent layers. Each person's `u` is shared across that target's repeated observations. Gaussian/lognormal observation noise is separate.

```r
repeated <- rbind(d, transform(d, x1=x1+.11, x2=x2-.13, x3=x3+.17))
m <- bmf(Ability=~x1+x2+x3,
          residual=list(targets=~x1+x2+x3, cor="independent"))
s <- bmeas(m, repeated, subject="person", fit=FALSE)
# Common-empty direct covariance uses the same generator.
md <- bmf(residual=list(targets=~x1+x2+x3, cor="free"))
sd <- bmeas(md, repeated, subject="person", fit=FALSE)
```

Under iid people with unconstrained residual covariance, separating free Gaussian parameter SDs from observation SDs can require repeated observations or known noise. Fixed nonzero correlation constraints and centered cross-person covariance can change this identification result. Where a scoped overlap condition applies, it is checked at the actual residual endpoints; a common factor cannot create a missing endpoint pair. Structural proof, numerical local rank, posterior propriety and runtime qualification are distinct results in `check_model()`.

## Fixed numeric bases

```r
Q <- matrix(c(1,-1,1), 3, 1,
            dimnames=list(c("x1","x2","x3"),"pattern"))
m <- bmf(Pattern=component(~x1+x2+x3, Q, loading="shared"))
s <- bmeas(m, repeated, subject="person",
          prior=set_prior("half_normal(.5)","amplitude",component="Pattern"),
          fit=FALSE)
```

`loading="shared"` means one positive amplitude times the supplied matrix. Multiple columns create separate latent coordinates but retain one amplitude. Numeric entries and signs are unchanged. Named rows match captured targets; nonzero shared rows must have compatible predictor units. Standard inert `model.matrix()` annotations are copied. Duplicate, zero and rank-deficient columns are not silently dropped. Explicit zero rows can declare mean-only targets; they do not create information overlap.

## Variance-share expressions

```r
m <- bmf(
  General=~x1+x2+x3,
  Pattern=component(~x1+x2+x3, Q, loading="decomposition"),
  decomposition=list(score=list(
    type="variance_share", targets=~x1+x2+x3,
    components=~General+Pattern, residual=TRUE, shares=~1)))
s <- bmeas(m, repeated, subject="person", fit=FALSE)
model_table(s, "share_targets")
model_table(s, "share_strata")
get_prior(s)
```

An owner replaces its loading and residual-SD expressions with target total SD times square-root share times the supplied row. It does not multiply by an old loading or amplitude. Observation noise remains separate. Each active basis-group row must already have squared norm one; values are checked, never normalized. Ordinary scalar rows are positive on owned targets, a scientific loading restriction. Remaining unowned scalar loadings stay signed without a replacement positive anchor.

Every owner explicitly chooses residual=TRUE/FALSE. A residual row already declared globally is consumed once, with both declarations recorded. A fully consumed amplitude/loading/residual SD has no extra parameter or prior. `loading="decomposition"` requires every nonzero row to be owned. An explicit shared-amplitude fallback can survive on unowned rows.

Each owner requires its active component-coordinate union to be orthogonal. Paths in the required-zero graph do not themselves imply additional zero correlations. Explicit equality classes do propagate a required zero to every class member. Required zeros override correlation backgrounds; an explicit conflicting free edge fails. Residual correlations apply to the resolved global/owner target union, and use one separate joint prior.

`shares=~1` gives shared logit coefficients within each active-part set, not fixed uniform weights. Varying support requires `support="separate"`; absent parts receive no tiny weight or phantom coefficient. Expert input accepts named target-level X and contrast C matrices. Default C has explicit Helmert signs/order. One-part simplexes have zero beta coefficients. Custom contrasts can define a restricted logit subspace. Original X/C are retained; column-offset removal during evaluation is only the softmax gauge.

```r
p <- c(set_prior("half_normal(.7)", "total_sd", target=~x1+x2+x3),
       set_prior("normal(0,.6)", "share", decomposition="score"),
       set_prior("normal(-.2,.4)", "share", decomposition="score",coordinate="h1"))
s <- bmeas(m, repeated, subject="person", prior=p, fit=FALSE)
```

Beta priors are Normal densities on unique contrast coefficients, with no softmax Jacobian. Total SDs have positive half-Normal/half-Student-t priors. `model_table()` distinguishes primitives, derived cells, parts, X/C, required zeros and residual rows versus free residual SDs. Rank diagnostics use scaled probe coordinates without changing scientific matrices or priors. A numerical diagnostic cap or overflow produces an unassessed result, not a nonidentification proof.

## Execution and results

Sampling requires explicit `chains`, total `iter`, `warmup` and a finite `resources` policy. Use `execution_plan()` on the prepared main-API spec to inspect an ordinary run without executing it. `resources="development"` explicitly selects the earlier tiny validation limits; they are not the default product ceiling. A caller policy can admit larger supported declarations with `cache="deferred"`, which leaves posterior diagnostics explicitly uncomputed. General eager-cache accounting remains unavailable for caller policies. See [RESOURCE_POLICY.md](RESOURCE_POLICY.md) for the minimal call, quotas, output estimates and separately limited posterior readers.

Known Gaussian blocks have bounded fixed-state qualification. The pinned six-target M3-CS/SDM direct-covariance control retains its narrower qualification. Other canonical pinned M3-CS/SDM component/residual/share compositions require `execution="experimental"`, with no demo-graph whitelist. Cognitive likelihood identification remains unassessed. Other family/custom compositions retain explicit execution limits.

```r
# Optional engineering lifecycle only; this short run cannot support inference.
# fit <- bmeas(s, chains=1, iter=20, warmup=10, cores=1, threads=1,
#              resources="development",
#             control=list(max_treedepth=6))
# summary(fit)
# posterior_summary(fit)
```

`summary()` and `posterior_summary()` read cached results only. Cache labels distinguish total SD, beta coefficients, derived shares and derived residual row SDs. No hidden sampling, compilation, simulation or LOO is triggered. Explicit new-frontend basic-family predictive computation has a separate bounded R-only implementation and review scope; see BASIC_FAMILY_PPC.md.

Serial is the default. Explicit threading uses the generic observation-row reducer, with each joint count row kept intact and every hierarchy/prior term outside the reduction. The compiler/cache pins stanc O0 and STAN_THREADS. The installed stanc2.40 O1 was observed to miscompile the original SDM early-return search; the research likelihood was not rewritten.

## Numerical evidence and open scope

The share generator was checked at26 frozen synthetic states: all896 named unconstrained-coordinate gradients and5580 generated values agreed with independent oracles. Tests included exact S17 marginal zeros, unequal simplexes, custom X/C, one-part/no-residual and residual-only cases, signed hybrid loadings, surviving free coefficients, original normalized CS/SDM rows, no-owner prior-output replay and serial/reduced equivalence. Two historical stanc-O0 builds used 110.367s total; their effective C++ optimization was unverified. Deterministic execution used 9.225s. No MCMC or larger dataset was used in that stage.

These checks do not prove global identification, posterior recovery, full floating-point boundary support, HMC adequacy or useful threading speedup. Exact Gaussian constant/within-person-repeat collapse paths are checked separately under scoped prior assumptions, without jitter. Proper priors do not resolve likelihood nonidentification. Low-eta correlation priors remain inspectable but are not execution-qualified; the default eta is2.

Fixed/equal ordinary loading constraints, task/fixed parameter levels and marker component SDs are implemented. Fixed/equal final marginal correlation constraints now have a separate fixed/one-label source increment (see [AFFINE_CORRELATIONS.md](AFFINE_CORRELATIONS.md)); the fixed/one-label iid subset requires explicit experimental execution; explicit sample-centered/task/ordered-raw declarations have the bounded profile above. Generic iid/final-R variance shares do not reproduce the original centered/raw-cross-block reference posterior. General custom likelihoods remain the registrant's mathematical responsibility. SDM simulation and a general trial-level DDM are not implemented. The pinned revised three-parameter EZ summary likelihood is available as ezdm_model(); its hierarchy/native evidence is bounded and its PPC remains unsupported. See [CURRENT_CAPABILITIES_zh.md](CURRENT_CAPABILITIES_zh.md) for the current crosswalk.

## External reference and advanced historical APIs

`gaussian_model()`, `binomial_model()` and `lognormal_model()` provide basic observation contracts. `m3_cs_model()`, `m3_model()` and `sdm_reference_model()` require exact external source hashes. They retain the original likelihood definitions and received scales. Fixed-width count/options arrays and typed argument bindings are inspectable. The package contains no private research source or observations and grants no relicensing rights for those external resources.

The preserved advanced `measurement()` path supports the original full M3 CS/MU routes and explicit reference G/P/C blueprint, including its original priors/centering. See `inst/examples/full-m3-gpc.R`. It was independently compared with the original model at tiny fixed states; its short retained MCMC was inadequate for inference. This historical path remains separate from the default iid/final-R model and the new explicit centered/task/raw profile.

Historical measurement_fit basic-family PPC and the full-M3 same-N centered-group simulator retain their previously qualified scopes. They preserve observed design/count totals, stream aggregate discrepancies, keep small per-replicate aggregate matrices, and make no goodness-of-fit verdict from tiny runs. SDM simulation and full/routed-M3 conditional-person simulation remain unsupported in this historical path. The separate single-block pinned M3-CS main-API reader supports explicit population and conditional checks on live, portable and archive inputs, within its documented contract. Legacy aggregate evidence unavailable in an older cache is labelled unavailable rather than regenerated implicitly.

This release accepts compatible reference-law and affine-correlation prepared schemas with a valid unchanged fingerprint. Incompatible older prepared contexts require an explicit rebuild from the saved declaration, original observation data and intended priors. Supported older formula captures can supply that new construction and retain formula provenance; this is not automatic migration of a prepared fit. Original fingerprints and scientific fields are never rewritten. Cached summaries remain historical records and do not rebuild or invoke backends.

## Install and check

```sh
R CMD build source
R CMD INSTALL bmeas_0.0.31.9001.tar.gz
R CMD check --no-manual bmeas_0.0.31.9001.tar.gz
```

Explicit native execution needs CmdStanR and a working CmdStan toolchain. Reference tests additionally need `MEASUREMENTPKG_M3_RESOURCE` and `MEASUREMENTPKG_SDM_RESOURCE` pointing to the authorized hash-matched files. Package tests use deterministic examples and explicit backend doubles; they do not run full studies or automatically fit the reference models.

Retain joint component uncertainty with `component_draws()` and preserve complete existing measurement CSVs plus scientific context with `archive_fit()` / `read_fit_archive()`. See [COMPONENT_DRAWS.md](COMPONENT_DRAWS.md), [MEASUREMENT_ARCHIVES.md](MEASUREMENT_ARCHIVES.md), and the [Chinese reuse guide](MEASUREMENT_REUSE_zh.md). These operations never refit the measurement model or automatically define a new structural likelihood.


A usual saved-fit workflow needs no budget boilerplate:

```r
archive <- archive_fit(fit, "measurement-archive")
archive <- read_fit_archive("measurement-archive")
joint <- component_draws(archive)
saveRDS(joint, "joint-components.rds")
```

Finite defaults are starting allowances for modest fits, not memory/RSS or
runtime guarantees. Insufficient limits refuse without automatic growth,
thinning, or refitting. For advanced control, inspect
`budget <- postprocess_budget("component_draws")`, edit a field, and pass
`budget = budget`. Use `"archive_fit"` or `"read_fit_archive"` for archive limits.
Each call returns a fresh ordinary list; explicit lists retain existing validation.

## Revised EZ summary model

The additive `ezdm_model()` builtin uses an immutable revised three-parameter contract. See [EZDM_REVISED.md](EZDM_REVISED.md) for data units, priors, derivation and scope. Numerical/native engineering evidence is separately recorded; arbitrary hierarchical fits are experimental, and EZ simulator/PPC remains unsupported.
