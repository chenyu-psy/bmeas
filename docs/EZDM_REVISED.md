# Three-parameter revised EZ summary likelihood (source candidate)

Public constructor: `ezdm_model()`. Sole explicit profile:
`ezdm_model(version = "revised_54a07b6_v1")`. Other versions fail rather than
silently selecting a different likelihood. The additive integration candidate 0.0.30.9001 is based on the latest
archive/scalable 0.0.29.9001 source. No existing R core or frozen test was changed.
Bounded native density, gradient, and all-fixed frontend checks have passed.
Arbitrary hierarchical execution, HMC and scientific adequacy are not qualified.

## Provenance and independence

Scientific contract pinned to bmm commit
54a07b6fa34506bd14116d295eea9db234071f28 (2026-10-07):
https://github.com/popov-lab/bmm/tree/54a07b6fa34506bd14116d295eea9db234071f28
The source manifest in the accompanying design snapshot identifies the exact
upstream blobs. The mutable develop branch is not a dependency.

Upstream declares GPL-2; bmeas declares MIT. This implementation does not embed,
translate, or require upstream implementation files. It was authored from the
mathematical contract and the first-exit derivation below. Coefficients were
independently generated symbolically; the independent 75-digit oracle differentiates
the Laplace transform rather than reusing those coefficients or formulas.
This documents the engineering provenance, not a legal certification. An upstream
code-level parity comparison has NOT been run. No claim of exact historical fit
reproduction is made: the released 1.3.1/1.3.2 law differs scientifically.

## Data and interface

One row is one independent, nonoverlapping aggregate of retained trials.
`mean_rt` is in seconds; `var_rt` is the unbiased sample variance in seconds².
Both must be finite and positive. `n_trials` is integer >=3;
`n_upper` is integer between 0 and n_trials inclusive. All-error/all-correct
cells are valid. All four fields are mandatory and observed; partial or wholly
missing summaries are rejected before frontend missing-response filtering.
The n>=3 boundary matches upstream's public contract, not a mathematical
singularity or merely the original study's preprocessing. The exact summary
moment formulas are well-defined at n=2; upstream nevertheless rejects it:
https://github.com/popov-lab/bmm/blob/54a07b6fa34506bd14116d295eea9db234071f28/R/model_ezdm.R#L247-L254
and https://github.com/popov-lab/bmm/blob/54a07b6fa34506bd14116d295eea9db234071f28/R/distributions.R#L1371-L1379 .
A future n=2 support extension must be declared as a contract difference.
No RT filtering, rescaling or ndt < mean restriction is performed. Aggregate
independence and consistent trial sets cannot be checked from summaries alone.

```
block <- observe(ezdm_model(), dat, subject="person", task="task",
  fields=list(mean_rt="mean_rt", var_rt="var_rt",
              n_upper="n_upper", n_trials="n_trials"), units="second")
```

All three outcomes are response fields; n_trials is auxiliary. Parameters drift,
bound, ndt are predictor roles, default subject_task; ordinary task/fixed bindings
and multi-block shared-subject alignment apply. Drift has identity link; bound
and ndt have log links. The registered function receives natural theta values:
the existing compiler exponentiates free bound/ndt exactly once, while natural
fixed values are consumed directly. Diffusion SD=1, relative start=.5 are fixed.
The likelihood describes count toward the upper boundary, which must be coded
consistently across rows. Negative drift is meaningful.

## Independent derivation

Let X(t)=v t+B(t), starting at zero, with absorbing boundaries ±h, h=a/2.
The backward equation .5 f''+v f'=s f, f(±h)=1 gives
E[exp(-sT)] = cosh(hv)/cosh(h sqrt(v²+2s)).
Its log derivatives at s=0 give cumulants k_r=(-1)^r d^r log L(s)/ds^r.
Euler's product for cosh also gives
k_r=(r-1)! 2^r h^(2r) sum_{j>=0}[((j+.5)² pi²+x²)^(-r)], x=hv.
These equations are sufficient to derive the implementation without upstream code.
For |x|<=.5 expand in z=x². The coefficient of z^j is
(r-1)! 2^r (-1)^j choose(r+j-1,j) (2^(2(r+j))-1) zeta(2(r+j))/pi^(2(r+j)).
We keep j=0..20. Terms alternate and decrease on this interval; the first
omitted term is an absolute truncation bound (before floating-point rounding).
At r=4, |x|=.5 the relative bound is below 1e-17. Coefficients are stored to
17 significant digits. This is an approximation with a stated domain/order,
not a drift floor or a softened absolute drift.

Outside the series region, logarithmic differentiation yields the closed forms
stored in the Stan and R functions. They use inverse powers of |x| and
q=sech²(x)=4 exp(-2|x|)/(1+exp(-2|x|))², avoiding overflowing cosh.
For zero drift the first four cumulants are h², (2/3)h⁴, (16/15)h⁶,
(272/105)h⁸. No clamping or added variance floors are applied. Unrepresentable
moments or nonpositive derived variance fail explicitly rather than silently
changing the likelihood. Extremely large/small parameters may exceed double
precision; no universal finite-domain promise is made.

The first-exit cumulants and moments of the empirical summaries below are
mathematically exact (up to their documented numerical evaluation). The
Gamma/conditional-Normal family is an approximate summary likelihood.
With M=k1, V=k2 and n=n_trials:
W=k4/n+2V²/(n-1), C=k3/n,
S² ~ Gamma(shape=V²/W, rate=V/W),
mean_RT | S² ~ Normal(ndt+M+(C/W)(S²-V), sqrt(V/n-C²/W)),
n_upper ~ Binomial-logit(n,a v).
The row density is the sum of these three normalized log densities, including
all constants. Count is independent of the RT summaries in this approximation.
Normal mean support is all real: requiring positive observed means does not
add a truncation normalizer. Conditional means can fall below ndt.

## Priors and scope

No EZ-specific automatic prior calibration was added. Existing missing-prior
safeguards apply. A scientifically chosen example for intercepts is:

```
tasks <- unique(dat$task)
prior <- c(
  set_prior("student_t(1,0,1)", "Intercept", target=~ez$drift[tasks]),
  set_prior("normal(0,.5)", "Intercept", target=~ez$bound[tasks]),
  set_prior("normal(-1.5,.5)", "Intercept", target=~ez$ndt[tasks]))
```

These are examples on predictor scales, not defaults; adapt to task and units.
The drift prior is Cauchy(0,1). Loading/component/residual priors are additionally
required as appropriate to the chosen measurement structure. They are not copied
from bmm's hierarchy. A demonstration residual prior `half_normal(.5)` is an
explicit example, not calibrated evidence or a universal recommendation.

No simulator, posterior prediction, PPC, HMC, LOO, full study rerun, or publication
is included. fit/log_lik capabilities remain explicitly `unvalidated`; these metadata
labels do not block the separately gated experimental execution path. Bounded
fixed-state native evidence is not general fit qualification.

## Historical bounded native gate (completed under explicit amendment)

The original plan was to compile one tiny standalone Stan program containing these
functions, data arrays for the existing 351-row oracle grid, and generated
quantities for all four cumulants and each joint row log density. Execute one
fixed_param draw, one chain, one thread, with no HMC and no study data. Compare
finite outputs against the oracle with relative/scaled tolerance 2e-10 and
absolute tolerance 2e-10 near zero. Independently compile the already generated
all-fixed frontend model to check the actual typed row call; evaluate its three
rows once, requiring the summed density to match the R row sum. Additionally
check finite differences or exposed autodiff for drift around 0 and x=.5 on
both sides with a declared step-size sweep. Fail rather than introducing floors
if values or derivatives disagree. Budget: at most 2 native compiles, 240 seconds
per compile and 30 seconds per execution, serial. A timeout is inconclusive.
Any later hierarchical smoke fit would require another explicit decision.

## Recorded native evidence and remaining scope

The original two-program gate passed under an explicit amendment: the first
program used effective O2 due to a retained toolchain flag; the second used
verified effective O0. Neither used unsafe-math flags. Exactly two fixed_param
draws and six existing-executable autodiff evaluations covered 351 supported
summary rows, three ID-aligned all-fixed frontend rows, and zero/seam gradients.
Maximum scaled derivative discrepancy was 3.81e-14. The first O0 deviation and
O0 compiler warning are retained in the native-gate report accompanying this
candidate. This is not HMC, recovery, or arbitrary-hierarchy qualification.
The separate two-person/two-row/12-coordinate free-hierarchy fixed-state gate
completed on 2026-10-08 at 04:00:35 UTC. One serial effective-O0 compilation,
one fixed_param draw and two existing-executable log_prob evaluations verified
the independent-residual alpha/SD/white → eta → natural-parameter path, normalized
row likelihoods, prior kernels, positive-SD unconstraining Jacobian and all 24
coordinate gradients at two frozen states. Maximum scaled gradient discrepancy
was 2.66e-15. The recorded gate covers only this two-person independent-residual
fixture, with no factors, loadings or free correlations; it does not establish
arbitrary-hierarchy, HMC, identification, recovery or scientific qualification.
See the accompanying FREE_HIERARCHY_GATE_REPORT.md and retained receipts.
Capability labels and generic experimental gates remain unchanged.


## Portable/archive export in 0.0.31.9001

The exact pinned revised-EZ provenance extension is now accepted during detached
export, with all scientific provenance retained and strict plain scalar, exact
contract/commit/URL and fixed-constant validation. This repairs the older
source/license-only schema refusal; arbitrary extra provenance remains rejected.
Portable/archive reporting and admitted diagnostic refresh are available. EZ
prediction/PPC, detached backend execution, source generation and standata remain
refused. Serialization support adds no scientific likelihood or inference claim.
