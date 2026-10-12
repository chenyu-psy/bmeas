\name{archive_fit}
\alias{archive_fit}
\alias{read_fit_archive}
\title{Preserve and reopen retained measurement evidence}
\usage{
archive_fit(fit, path, budget = postprocess_budget("archive_fit"))
read_fit_archive(path, budget = postprocess_budget("read_fit_archive"))
}
\arguments{
\item{fit}{A current main fit or a validated portable fit.}
\item{path}{An absent destination directory for writing, or the current archive directory for reading.}
\item{budget}{When omitted, uses \code{\link{postprocess_budget}} for this operation. Explicit lists are validated unchanged; \code{NULL} and partial lists are not filled in. A named list containing max_members, max_member_bytes, max_total_bytes,
max_context_bytes, max_read_bytes, max_temp_bytes, posterior, and context; field order is immaterial.
The last two are existing streaming and portable-context policy lists.}
}
\description{Copies all retained CSV bytes and an unchanged portable capsule into a
relocatable plain-data directory. No model computation or diagnostics are performed.}
\value{A plain bmeas_fit_archive handle accepted by component_draws and passive reports: summary, print, fit_checks, parameter_summary, and posterior_summary.}
\details{The development v1 format uses archive-time MD5 corruption checks, not
sampler-time authentication. Exact uncollected inputs and unsampled quantities
remain unavailable. RDS decoding is restricted to trusted locally produced archives;
byte budgets do not bound heap memory. Missing resources are reported; surviving
supported resource pins are checked. See docs/MEASUREMENT_ARCHIVES.md for the complete
format, reservations, compatibility, and qualification limits.
Creation and explicit reopening retain the unchanged portable capsule as an
in-memory report snapshot. Passive reports delegate to the portable report
implementations and never reopen, hash, scan, or refresh archive files. They
remain available after the archive directory becomes unavailable. Summary and
fit-check reports carry an archive_context field; nonmissing parameter tables
carry an archive_context attribute. These identify retained snapshots that were
not reverified by the report. Absent parameter caches remain NULL. Legacy handles
without a snapshot require explicit read_fit_archive(path); reports never
silently reopen them. This does not change component_draws, which still reopens
and verifies the archive for active acquisition. Local report validation checks
schema and saved identity agreement, not integrity or authenticity.}
