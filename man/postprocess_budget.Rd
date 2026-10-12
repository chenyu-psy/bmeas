\name{postprocess_budget}
\alias{postprocess_budget}
\title{Inspect finite postprocessing budget defaults}
\usage{postprocess_budget(operation)}
\arguments{
\item{operation}{Exactly one of \code{"archive_fit"}, \code{"read_fit_archive"}, or
\code{"component_draws"}. Abbreviations are not accepted.}
}
\description{Returns a fresh ordinary named list of finite starting allowances
for a modest retained fit. Archive writing and reading use identical defaults.}
\value{A complete budget list accepted by the named operation. Inspect or modify
this local list and pass it as \code{budget}; future defaults are unaffected.}
\details{These are the complete allowances from the measurement-reuse worked
example, not fit-size estimates or guarantees of memory, RSS, disk availability,
elapsed time, numerical quality, or inferential validity. Insufficient budgets
refuse; no ceiling growth, thinning, refitting, or source changes occur.
Explicit budgets retain existing validation and refusal semantics.}
\examples{
budget <- postprocess_budget("component_draws")
budget$max_components <- 6
budget$posterior$max_source_bytes
}
\seealso{\code{archive_fit}, \code{read_fit_archive}, \code{component_draws}}
