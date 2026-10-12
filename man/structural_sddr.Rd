\name{structural_sddr}
\alias{structural_sddr}
\title{Experimental Single-Coefficient Point-Density Diagnostics}
\description{Evaluate a deterministic Gaussian kernel density ratio at zero
from a newly bound structural mapping. This S4a interface never qualifies or
returns a formal Bayes factor and never samples, remaps or refits.}
\usage{
structural_sddr(x, predictor, outcome, null = 0,
  null_prior = "encompassing_conditional", control = list())
}
\arguments{
\item{x}{A newly created \code{structural_draws()} result with a creation-time
scientific evidence binding. Legacy unbound results are refused; no post-hoc seal
is created.}
\item{predictor, outcome}{One stable mapped coordinate ID each. Only an ordinary
free coefficient is supported, not a fixed/omitted lavaan path, product or joint constraint.}
\item{null}{Exactly numeric zero.}
\item{null_prior}{Exactly \code{"encompassing_conditional"}, defining the intended
null comparator. This declaration cannot establish mathematical compatibility.}
\item{control}{Named list with \code{bandwidth=NULL}, \code{max_draws=10000}
per distribution and \code{max_kernel_evaluations=100000} across both distributions
and five bandwidths. Explicit bandwidths must be a positive finite numeric vector
named exactly \code{c(prior=..., posterior=...)}. At most 32 chains per distribution
are admitted for pairwise repetition diagnostics. These are explicit operation
budgets, not wall-time or peak-memory guarantees.}
}
\details{
All prior and posterior rows must be mapped; failures retain their keys and block
KDE instead of being deleted. Draw counts may differ and rows are not paired.
Repeated states are retained with their original weights. Automatic bandwidths
are conservatively blocked when coefficient values or full population states
repeat. Repetition can be legitimate MCMC behavior; it is not evidence of fraud.
Explicit bandwidths permit empirical diagnostics, but not constant coefficients.

The reference bandwidth is \eqn{0.9\min(s,IQR/1.349)n^{-1/5}}. This transparent
raw-count smoothing heuristic is not adjusted for dependence and is not an ESS
or independence assertion. Gaussian point densities are evaluated using
log-sum-exp, without fitting a normal distribution or interpolating a density
grid. Both bandwidths are independently multiplied by 0.5, 0.75, 1, 1.5 and 2,
producing 25 log-density ratios. A maximum shift above 0.25 log units is
flagged as bandwidth-sensitive; this is an engineering screen, not a statistical
error bound. The numerator is posterior density; the
denominator is prior density. Numerical density underflow is flagged while
representable log densities are retained.

Counts on either side of zero within the narrowest bandwidth, observed ranges,
repetition and bandwidth sensitivity are descriptive. Fewer than 25 observed
values per side triggers a conservative sparse-support flag. These counts are
not effective visits; Gaussian kernel positivity cannot establish true support,
continuity, absence of a boundary or a proper finite density. No threshold
certifies a Bayes factor. Optional \pkg{posterior} coefficient diagnostics require
regularly spaced ordered equal-length nonidentical chain sequences, at least two
chains and twenty rows each. No padding, truncation or sorting is performed.
Those diagnostics do not establish independent chains, a valid sampled law, or
kernel-density precision. MCSE remains explicitly not assessed.

The evidence binding uses a bounded canonical plain-data codec with limits of
64 MiB canonical bytes, four million nodes and depth 64, admitted before writing
or hashing. It covers mapped arrays, keys, statuses, mapping specification,
diagnostics and scientific input evidence. Native prior evidence retains all
resolved parameterization, prior specification, generation arrays and receipt;
its legacy prepared specification is represented in the binding by the
creation-validated fingerprint and scientific checks. The remainder, including
callback captures, remains in returned provenance but is not traversed or
authenticated by S4. Changed bound fields or fingerprint are rejected.
The binding detects accidental mutation, not coherently forged provenance.

Formal BF fields are always NA. Native prior provenance cannot upgrade posterior
sampling, propriety, identification or a declared compatible conditional null
prior. Draw-dependent SD standardization can change that conditional null prior
even when the zero set agrees. Projected paths remain discrepancy functionals,
not comparisons of independently specified generative SEMs. Joint-zero tests
require joint densities and cannot multiply marginal ratios.
}
\value{A \code{bmeas_structural_sddr} list with computation/density/BF status,
reason codes, target, retained evidence, point densities and bandwidths,
all 25 sensitivity combinations, an explicitly experimental log-density ratio,
and diagnostics. Formal \code{log_bf01}, \code{bf01}, \code{bf10} are always NA.
No summary or print method converts the experimental value into inferential evidence.}
\seealso{\code{\link{structural_draws}}}
