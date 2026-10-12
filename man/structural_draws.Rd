\name{structural_draws}
\alias{structural_draws}
\alias{population_draws}
\alias{component_population_draws}
\alias{summary.bmeas_structural_draws}
\title{Optional Deterministic Population Covariance Mapping}
\description{Declare joint population covariance draws or read aligned retained
component posterior states, then map each matrix to a regression functional.
No sampling, measurement refitting, density estimation or Bayes factor is performed.}
\usage{
population_draws(covariance, draw_keys, variables, distribution, model_id,
  prior_id, source_kind, source, joint_scope, control = list())
component_population_draws(fit,
  budget = postprocess_budget("component_draws"), control = list())
structural_draws(posterior_draws, predictors, outcomes, prior_draws = NULL,
  coefficient_scale = c("unstandardized", "standardized"),
  units = NULL, control = list(),
  backend = c("analytic", "lavaan"), model = NULL, backend_control = list())
\method{summary}{bmeas_structural_draws}(object, ...)
}
\arguments{
\item{covariance}{Numeric array with dimensions draws by variables by variables.
Both variable axes must be named with the stable IDs in \code{variables}, in
exactly that order. Supply population parameter draws, never a sample covariance
of individual factor scores, fitted prior-predictive data or independent marginal draws.
Correlation matrices without the required scale information are not a covariance input.}
\item{draw_keys}{Data frame with exactly \code{chain_id} and \code{saved_row}:
positive integer-valued numeric columns with unique row pairs. Draw count is not ESS.}
\item{variables}{Data frame containing exactly character columns \code{id},
\code{scale}, \code{orientation}; IDs must be unique, all entries nonempty.
External orientation and scale are caller declarations, not identification certification.}
\item{distribution}{Explicitly \code{"prior"} or \code{"posterior"}.}
\item{model_id, prior_id}{Single nonempty model and encompassing-prior identity
strings. Prior and posterior must use the identical contracts. These external
declarations cannot verify that an actual prior or posterior law was sampled.}
\item{source_kind}{Explicitly \code{"external"} or \code{"synthetic"}.}
\item{source}{Nonempty description of input origin.}
\item{joint_scope}{Explicitly \code{"joint_population_covariance_parameters"}.}
\item{control}{Named list overriding finite positive defaults:
\code{max_draws=10000}, \code{max_variables=128}, \code{max_values=10000000},
\code{max_matrix_work=100000000}, \code{near_singular_rcond=1e-10}.
The last must be below one. Counts bound each input and combined mapping values/work,
not peak RSS or elapsed time. Inputs already resident and saved-reader acquisition
are separately controlled. No jitter, pseudoinverse or matrix repair is available.}
\item{fit, budget}{A supported fit/archive and existing reader budget passed
unchanged to \code{\link{component_draws}}. No backend is evaluated.}
\item{posterior_draws, prior_draws}{Objects from the population contract.
The optional prior may have a different number of draws; rows are never paired.
The saved component adapter is posterior-only. For the separate narrow native
prior generator see \code{\link{component_prior_draws}}. External draw declarations
remain unverified and cannot be certified as native draws.}
\item{predictors, outcomes}{Nonempty disjoint character vectors of stable
coordinate IDs, not display labels. The same mapping is applied to both sets.}
\item{coefficient_scale}{Unstandardized regression coefficients or coefficients
standardized using each draw's population standard deviations. Standardized
residual covariance is divided by the outcome SD outer product.}
\item{units}{Optional fixed nonzero finite numeric multipliers named in full
coordinate order. Negative multipliers explicitly reverse directions.}
\item{backend}{Default analytic Cholesky regression or optional \code{"lavaan"} covariance-discrepancy mapping. Only installed lavaan 0.7.2 is currently validated; the optional dependency is required only when selected.}
\item{model}{For the lavaan backend, NULL generates saturated regression. Otherwise supply restricted alias-based syntax: \code{x1}, \code{x2}, ... follow predictor order and \code{y1}, \code{y2}, ... follow outcome order. Only Y~X paths and Y~~Y residual (co)variances, with free or finite numeric fixed terms, are accepted. Omitted paths and off-diagonal residuals are zero; omitted residual variances are free. Predictor covariance is unrestricted. No labels, equality constraints, latent, recursive, mean, categorical, multigroup or multilevel models. For example \code{"y1 ~ x1 + x2; y2 ~ x2; y1 ~~ 0*y2"}.}
\item{backend_control}{Lavaan-only named positive numerical controls: \code{max_fits=100}, \code{reference_n=1000}, \code{iter_max=1000}, \code{eval_max=2000}, \code{rel_tol=1e-10}, \code{boundary_tol=1e-8}. Counts are integer-valued, reference N is at least two, and tolerances are below one. Combined prior/posterior row counts are checked before fits. Reference N is solely an API constant, not a new sample size. Options are fixed across both distributions, with no stochastic/adaptive retries. Iteration limits do not guarantee wall time.}
\item{object}{A structural mapping result.}
\item{...}{Reserved; currently unused.}
}
\details{The analytic map is \eqn{B=\Sigma_{XX}^{-1}\Sigma_{XY}} and
\eqn{\Psi=\Sigma_{YY}-\Sigma_{YX}B}, computed by Cholesky triangular solves.
Numerically, the solve and residual crossproduct are evaluated on the
correlation scale; unstandardized outputs use guarded log-scale unit conversion.
A representable standardized path is not lost by first underflowing an
unstandardized coefficient. Nonzero output underflow/overflow is a failure status.
Every input covariance must be exactly symmetric and positive definite as stored.
All declared coordinates are checked, including coordinates outside the regression.
Failed and unavailable rows retain their keys and NA output slots; descriptive
summaries are unavailable if any row failed. Near-singular full matrices or predictor blocks are
mapped with an explicit status and threshold. No draws are silently filtered.
The component adapter retains the full original extraction and all its scale,
orientation, prior and upstream evidence. Consistency against the retained
extraction and its adverse evidence is checked again at mapping. This is an
accidental-mutation check, not cryptographic authentication of coherently edited
evidence. Unresolved component directions are
not upgraded. An ordinary anchor or pin, or a zero row in the remaining affine
sign-system basis, establishes a declared direction only, not global identification.
The optional lavaan backend uses the same analytic eligibility checks as a conservative prerequisite, then fixed normal-ML covariance discrepancy minimization with covariance rescaling disabled, no means/SE/tests, one deterministic nlminb attempt and no random starts. It retains every failure and boundary row. Candidate estimates in diagnostics are unstandardized and are not approved mapped values. Boundary screening uses the minimum eigenvalue of residual covariance scaled by input outcome SDs; the threshold is numerical, not a proof of an exact mathematical boundary. Global optimum, multiple-local-optimum exclusion and identification are not certified by convergence. Requested options and parsed parameter contracts are checked against each fit; resolved options are retained and checked across both draw sets. No second data likelihood or generative SEM comparison is created.
Repeated covariance values are retained and counted descriptively; neither that
count nor unique keys establishes statistical independence.}
\value{\code{population_draws} and \code{component_population_draws} return
\code{bmeas_population_draws} objects. \code{structural_draws} returns prior and
posterior coefficient/residual arrays, original draw keys, per-row statuses,
fixed mapping specification, diagnostics and complete input provenance.
New results also retain a bounded creation-time scientific binding for optional
\code{\link{structural_sddr}} diagnostics; it detects accidental mutation, not
source authenticity. See that interface for the exact legacy prior-spec projection.
Its estimand type is \code{regression_functional} for analytic/generated saturated mapping, or \code{projected_path} for explicit restricted lavaan models. The summary returns descriptive
means, SDs and quantiles only for complete mappings, with diagnostics; it supplies
no Monte Carlo standard errors, effective sample sizes or inferential certification.}
\examples{
ids <- c("x", "y")
S <- array(c(2, 1, 1, 3), c(1, 2, 2), list(NULL, ids, ids))
z <- population_draws(S, data.frame(chain_id=1, saved_row=1),
  data.frame(id=ids, scale=c("fixed units", "fixed units"),
    orientation=c("positive axis", "positive axis")),
  distribution="posterior", model_id="synthetic example",
  prior_id="synthetic prior", source_kind="synthetic",
  source="deterministic algebra only",
  joint_scope="joint_population_covariance_parameters")
structural_draws(z, "x", "y")$posterior$coefficients
}
\seealso{\code{\link{component_draws}}}
