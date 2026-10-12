\name{portable_fit}
\alias{portable_fit}
\title{Export a detached declarative post-processing context}
\usage{
portable_fit(fit, budget)
}
\arguments{
  \item{fit}{An existing main-API \code{bmeas_fit}/\code{bhmm_fit} whose original specification seal and saved model association still verify.}
  \item{budget}{A plain named list with required finite positive integer-valued \code{max_context_bytes} and \code{max_context_nodes}, and optional \code{max_depth} (default 64). Unknown or malformed fields refuse.}
}
\description{
Constructs a detached representation of an admitted saved context and compatible
cache for explicit diagnostic, individual-summary and supported predictive
post-processing. The original object is unchanged.
}
\details{
The original specification seal must reproduce before any execution-only callback
is omitted. There is no force, reseal, automatic migration, refit or source
regeneration option. The live backend is not traversed. Saved scientific context,
source paths and protected adverse history are retained, with only inventoried
callback slots removed. Unsupported content refuses the export.

The byte/node/depth allowances cover the complete canonical detached output,
including copied caches and generated metadata. They are not hard RAM, disk or
wall-time limits. Original-seal serialization and R/native materialization have
separately unknown costs. The immutable canonical identity uses
\code{tools::md5sum} for non-adversarial integrity only, not authentication of
original sampler output. User-initiated RDS serialization is outside this quota.
There are four canonical traversals: complete preflight count, complete fresh
copy, immutable-section write, and complete final count. The receipt's three
named pass counters have their narrower count/validation/encoding meanings;
they do not count the additional copy traversal or all schema/seal work.

No posterior file is opened, copied or relocated. The object can contain
already-retained posterior-valued cache arrays. Existing source paths remain
unchanged, so moving the RDS does not restore missing CSV files. An old fit needs
an evidenced producer/serialization context where its unchanged original seal
verifies. If that producer predates \code{portable_fit}, a separately supplied,
reviewed exporter helper is also required in that original environment. This
package does not automatically install, select or migrate old environments.
For future moves, export using a version that provides this function before
upgrading, then save and load the detached object. Ordinary full-fit RDS
deserialization dependencies are not removed automatically.

The existing \code{refresh_diagnostics}, \code{individual_parameter_summary}
and \code{assess_measurement_fit} interfaces validate the complete immutable
canonical identity, positional schemas and retained source binding before
acquisition or computation. They use the original protected finite context
allowance separately from their unchanged operation budgets. Their source
receipts contain a distinct \code{context_validation} phase with canonical
counts, owned-file bytes written and a logical digest-read reservation.
Later source or computation failure preserves this completed context evidence.
A matching digest grants no convergence, scientific validity or original
posterior authentication.

\code{diagnostic_parameters} and \code{ppc_plan} inspect saved structural
metadata only. Summaries, fit checks, parameter-summary accessors and printing
remain passive. Their portable notices explicitly state that integrity was
not checked by that call. Original adverse scientific and computation history
remains protected and visible after fresh cache updates. Proved improper or
non-normalizable original targets retain the existing active-operation refusal.

The immutable producer-stage dispatch marker remains historical evidence;
current consumer support is reported separately. Unknown or disabled immutable
interface profiles refuse. Updated diagnostic/PPC objects preserve their
immutable core, identity and export receipt without resealing. Portable objects
cannot be used for fitting, prepared-model rebuilding, source/data generation,
backend operations, saved-model callbacks or source-path rebinding.
}
\value{
A distinct object of sole class \code{bmeas_portable_fit}, with ordered fields
\code{schema_version}, \code{immutable}, \code{cache}, \code{identity}, and
\code{export_receipt}. It is not a prepared model or a new fit.
}
\examples{
\dontrun{
portable <- portable_fit(fit, budget = list(
  max_context_bytes = 8 * 1024^2,
  max_context_nodes = 1000000,
  max_depth = 64
))
saveRDS(portable, "fit-postprocessing.rds")
portable <- readRDS("fit-postprocessing.rds")
# Saved reports are passive; original CSV paths remain unchanged.
summary(portable)
}
}
