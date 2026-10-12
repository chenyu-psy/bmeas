\name{bmeas-fit-checks}
\alias{fit_checks}
\alias{parameter_summary}
\alias{print.bmeas_fit_checks}
\title{Read cached fit diagnostics without starting computation}
\description{Inspect saved data context, sampling diagnostics and separately recorded scientific and predictive evidence. For main-API fits these functions never call a backend, read posterior files, rerun model checks or simulate predictions.}
\usage{
fit_checks(fit)
parameter_summary(fit)
\method{print}{bmeas_fit_checks}(x, ...)
}
\arguments{
\item{fit}{A fitted bmeas object, its compatible historical bhmm_fit class, a legacy measurement_fit, a portable fit, or an archive handle with a retained report snapshot. A prepared specification is not a fit; use check_model for preparation checks.}
\item{x}{A cached report returned by fit_checks for a main-API fit.}
\item{...}{Unused printing arguments.}
}
\value{For a main-API fit, fit_checks returns a bmeas_fit_checks list with version, context, sampling, domains and predictive fields. The version describes the report schema, not historical package or evidence provenance. parameter_summary returns exactly the same cached table as posterior_summary. For measurement_fit, the existing fit_checks PPC-cache return is preserved, including NULL and its replicate-status metadata behavior.}
\details{
Archive handles delegate to passive portable reports from their retained in-memory snapshot; no archive or original file is reopened or reverified. Archive provenance is added as archive_context (a field for fit_checks and an attribute for nonmissing parameter tables). Missing parameter caches remain NULL. See \code{\link{archive_fit}}.

Sampling diagnostics include keyed Rhat and bulk/tail ESS extrema, cached mean MCSE and interval columns, and per-metric chain diagnostics with coverage. Infinite Rhat remains visible. Missing or invalid evidence does not become a favorable pass, and unequal HMC vectors are never recycled into complete chain rows. Coverage counts refer to cached rows, not independent unknowns. Configured run counts do not substitute for observed counts. Cached values are not rewritten from chain-count metadata.

A retained record with status unsupported or not_assessed differs from an unavailable historical field. Malformed optional context has local invalid-metadata reasons and does not discard valid sampling diagnostics; ambiguous required cache/metric schemas give a typed reporting error. Missing context is not defaulted to zero observations or an iid person law. Saved older summary objects are printed from their retained cache/run without reclassification. Identification, posterior normalizability/propriety, numerical admissibility and predictive fit retain their recorded meanings. No new statistical threshold, universal fit verdict, LOO, WAIC, PPC or prior-sensitivity computation is applied. Explicit main-API basic-family predictive computation uses \code{assess_measurement_fit}; this accessor only projects its saved law, level, discrepancy table and availability. Malformed optional PPC fields remain locally unavailable.

The parameter table retains estimand roles, fixed/alias distinctions and scientific/computational coordinate meanings. mcse_mean concerns the posterior mean only; no quantile MCSE is implied. Recorded interval names remain visible without guessing an unavailable historical estimator. Domain and context fields preserve provenance and explicit absence.

summary for a main-API fit adds the same report as its checks field, plus context, blocks, overlap and excluded_responses, while preserving earlier fields and classes. Repeated and detached reads leave the fit, specification, cache and RNG state unchanged. Use namespace-qualified exported helpers when historical packages mask their names; old fit classes are not rewritten to force a new S3 printer.
}
\seealso{\code{\link{posterior_summary}}, \code{\link{check_model}}}
