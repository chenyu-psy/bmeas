\name{refresh_diagnostics}
\alias{refresh_diagnostics}
\title{Refresh Selected Population and HMC Diagnostics Explicitly}
\usage{
refresh_diagnostics(fit, parameters, budget)
}
\arguments{
\item{fit}{A retained main-API fit or \code{bmeas_portable_fit} with supported
structured context and recorded CmdStan 2.40.0 sample/HMC output paths.}
\item{parameters}{An explicit unique plain character vector of keys returned
by \code{diagnostic_parameters}. Order is preserved. Use \code{character()}
for HMC-only refresh. Fixed, person and unsupported keys are refused.}
\item{budget}{An explicit plain list with \code{posterior}, \code{max_monitors},
\code{max_metric_rows} and \code{max_fft_length}. Optional
\code{max_catalogue_rows} and \code{max_context_rows} default to 10000 and
100000. The nested posterior policy selects the existing streaming reader
and supplies finite source-byte, logical-read and numeric-slot allowances.}
}
\description{Validates every retained row of every complete equal-layout chain,
computes diagnostics for the requested population monitors and available
retained sampler fields, and returns an updated copy of the fit.}
\details{The receipt hashes current file bytes and associates them with saved model
identifiers; it does not authenticate original executable, data or output
origin. The selected parameter table replaces an earlier table; HMC-only
requests install an explicit empty parameter table. Failed or interrupted
calls do not install partial caches. Passive summary, parameter-summary and
fit-check operations do not acquire files or compute these statistics.

Empirical mean, median, sample SD and type-7 5/95 percent quantiles use all
observed states. Selected parameter diagnostics use the qualified posterior
1.7.0 concrete estimator profile on intact original-chain matrices. Mean MCSE
uses the raw-mean ESS algorithm. Original chain count and metric-specific
minimums remain explicit; the provider's private odd-row split omission does
not remove observations from empirical summaries.

Divergence/depth counts describe retained states only. Recorded depth metadata
is required for NUTS depth-hit counts. E-BFMI is unavailable for thinned,
constant, missing or unrepresentable energy. Missing optional sampler fields
produce unavailable metrics; invalid present values refuse. No favorable
values or convergence verdicts are substituted.

Saved specification fingerprints include serialized namespace provenance. An
older ordinary fit loaded into a newer bmeas namespace may refuse with
\code{E_SPEC_CHANGED} before acquisition. This operation does not migrate or
reseal old fits. A detached \code{bmeas_portable_fit} exported while its original
seal verifies uses its canonical immutable identity instead; see
\code{portable_fit} for export and older-environment requirements.

Portable calls validate the immutable context under its saved finite allowance,
separately from this operation's posterior/metric budget. The source receipt
retains a distinct \code{context_validation} record. Later metadata discovery
and cached reports do not perform that verification again.

The numeric-slot allowance counts declared package-owned buffers. Provider,
R, sort/FFT/native temporary allocations, existing fit storage and process RSS
remain unbounded by that allowance. Row, monitor and FFT ceilings are finite
size controls, not a hard RAM or elapsed-time guarantee. The operation does
not fit a model, simulate predictive responses, compute LOO, establish
identification or certify inference.}
\value{A copied main-API or portable fit, preserving its input class and immutable
portable identity where applicable, with a replacement selected-parameter table,
per-chain HMC evidence and a versioned per-phase reporting receipt. Existing
scientific and unrelated reporting evidence is preserved.}
\examples{
# For an already retained main-API fit; explicit allowances are illustrative:
# available <- diagnostic_parameters(fit)
# selected <- available$key[available$role == "Intercept"]
# stopifnot(length(selected) > 0L)
# refreshed <- refresh_diagnostics(fit, selected, budget = list(
#   posterior = list(reader = "streaming",
#     max_source_bytes = 256 * 1024^2,
#     max_read_bytes = 7 * 256 * 1024^2,
#     max_materialized_values = 2e6),
#   max_monitors = 100, max_metric_rows = 4000, max_fft_length = 2048))
# parameter_summary(refreshed)
# fit_checks(refreshed)
}
\seealso{\code{diagnostic_parameters}, \code{parameter_summary},
\code{posterior_summary}, \code{fit_checks}}
