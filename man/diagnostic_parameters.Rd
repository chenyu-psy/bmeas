\name{diagnostic_parameters}
\alias{diagnostic_parameters}
\title{Discover Semantic Population Diagnostic Monitors}
\usage{
diagnostic_parameters(fit,
  budget = list(max_rows = 10000, max_context_rows = 100000))
}
\arguments{
\item{fit}{A main-API fitted object or \code{bmeas_portable_fit} with supported retained structured tables.}
\item{budget}{Plain finite limits on complete catalogue rows and consumed saved
metadata rows. Defaults are explicit resource choices and may be increased.}
}
\description{Reads saved population maps and returns selectable public keys,
labels, roles, supports and source identities. It acquires no posterior data
and computes no diagnostics.}
\details{This first catalogue includes population primitives and explicit
raw-law derived common correlations. It excludes fixed constants, person and
basis-score coordinates, share weights and other derived reports. Scientific
final-marginal correlation primitives remain distinct from raw correlation
coordinates and derived marginal correlations. Declared equality aliases are
recorded once per primitive. Source-variable names are inspection metadata;
callers select public keys. Budgets refuse rather than truncate. Metadata-row
limits are not byte or process-memory guarantees. Use \code{refresh_diagnostics} for an explicit separately budgeted
all-retained-state update. Existing summary functions remain passive and fitting
behavior is unchanged. For a \code{bmeas_portable_fit}, this metadata-only call
does not reverify immutable integrity and reports that limitation explicitly.
See \code{portable_fit} for the separate context allowance used by active
consumers.}
\value{A detached plain data frame with semantic monitor keys and bounded
metadata in the diagnostic_catalogue attribute.}
\seealso{\code{refresh_diagnostics}, \code{parameter_summary}, \code{fit_checks}}
