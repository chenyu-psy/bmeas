\name{component_prior_draws}
\alias{component_prior_draws}
\title{Generate a Narrow Joint Component Population Prior}
\usage{component_prior_draws(spec, draws, seed, control = list())}
\arguments{
\item{spec}{A currently prepared frontend specification with ordinary iid scalar components, full uniquely free final-marginal correlations, fixed unit component SDs and resolved directions.}
\item{draws}{Positive integer number of IID prior states.}
\item{seed}{Explicit nonnegative integer RNG seed. Previous RNG kind and state are restored on exit.}
\item{control}{Structural numeric/work limits, as for \code{structural_draws}. Generation has additional explicit accounting.}
}
\description{Generates the normalized joint marginal prior of the complete declared component population covariance for full LKJ shape between one and 10000 and deterministic unit component SDs. All supported proper independent nuisance factors are integrated out. This is not a sample of every measurement parameter.}
\details{Uses independent canonical partial correlations with dimension-dependent symmetric Beta shapes, followed by Cholesky reconstruction. It never samples independent marginal matrix entries. The complete declared dimension is retained before downstream variable selection.

The finite upper shape limit is a conservative numerical capability limit, not a mathematical propriety boundary. Initial implementation excludes restricted/fixed/equal/zero/product correlation charts, raw-B, marker, basis, share, loading constraints and all parameter-residual layers. Exclusion of ordinary independent proper residual priors is an implementation limitation, not a mathematical requirement.

Resolved data-calibrated prior parameters remain fixed. No prior-predictive data or model fitting is used. Numerical failures retain draw keys and unavailable matrices; no clipping, retries or failed-row deletion is performed. Native evidence is integrity-checked, not cryptographically authenticated external provenance. The prepared-spec seal and generation-evidence hashing have separately disclosed metadata/serialization costs not included in numeric work limits.

The output may be paired with a matching \code{component_population_draws} result in \code{structural_draws}. All model, prior, coordinate, scale, orientation and scientific-map metadata must agree exactly. Proper prior sampling does not prove posterior propriety, identification, convergence, a compatible SDDR null prior or Bayes-factor validity.}
\value{A \code{bmeas_population_draws} prior object with joint covariance arrays, stable coordinate IDs, unique IID-batch draw keys, retained failure statuses, generation receipt and sealed prepared specification evidence.}
\seealso{\code{\link{component_population_draws}}, \code{\link{structural_draws}}}
