\name{component_draws}
\alias{component_draws}
\title{Extract All Aligned Retained Component Matrix States}
\usage{
component_draws(fit, budget = postprocess_budget("component_draws"))
}
\arguments{
  \item{fit}{A current main \code{bmeas_fit}, or the reviewed complete
  \code{bmeas_portable_fit_v1} object with context profile
  \code{bmeas_postprocess_context_v1}. Private consumer views, unknown profiles,
  prepared specifications and legacy \code{measurement_fit} objects refuse.}
  \item{budget}{When omitted, uses \code{\link{postprocess_budget}} for this operation. Explicit lists are validated unchanged; \code{NULL} and partial lists are not filled in. A plain named list containing exactly \code{posterior},
  \code{max_components}, \code{max_output_values}, \code{max_matrix_work}, and
  \code{context}. All counters must be finite positive supported integers.
  \code{posterior} is the existing streaming policy, with
  \code{reader = "streaming"}, \code{max_source_bytes}, \code{max_read_bytes},
  and \code{max_materialized_values}. Its existing optional finite record,
  column, row, chain, temporary-byte and scan-value allowances remain available.
  \code{context} requires \code{max_context_bytes} and
  \code{max_context_nodes}; optional \code{max_depth} defaults to 64.}
}
\description{
Returns every retained joint state of the declared component correlation,
standard deviation and derived covariance on one exact chain and saved-row
axis. It does not fit a model or return the complete measurement posterior.
}
\details{
Only finite saved \code{R_component}, positive finite
\code{component_row_sd}, and binary \code{component_covariance_available}
columns are acquired. All chain files must retain the qualified CmdStan 2.40.0
sample/HMC, no-saved-warmup, equal-layout CSV profile. Their common column order
may be arbitrary. File order is independent of the ascending numeric chain-ID
order in the returned keys. No thinning, marginal-summary reconstruction,
provider-wide draw acquisition or posterior refit occurs.

For each represented correlation \eqn{r} and saved scales \eqn{s_i,s_j}, the
derived log magnitude is \eqn{\log|r|+\log s_i+\log s_j}. A represented zero has
zero covariance, log magnitude \code{-Inf}, and true derived availability.
Nonzero reconstructed values must be finite and nonzero to be available.
Overflow and underflow retain their derived log magnitude. Final covariance
is \code{NaN} whenever either original or derived availability is false.
Original unavailability is never erased. The covariance is labelled
\code{derived_from_saved_R_and_SD}; it need not equal independently rounded
generated covariance columns. Those original columns are not modified.

Raw correlation matrices are authoritative. Range, diagonal, symmetry and
declared zero/fixed/equality relationships receive per-state status and actual
maximum diagonal, symmetry and constraint residuals. A declared decimal
precision \eqn{p} supplies envelope zero at zero and
\eqn{0.5\,10^{\lfloor\log_{10}|x|\rfloor-p+1}} elsewhere. Saved pairs use
both envelopes plus \eqn{64K\epsilon\max(1,|x|,|y|)}; an exact metadata
constant uses only the saved-entry envelope and the same machine term.
Comparisons use log arithmetic to preserve subnormal bounds. This is a
consistency check, not a repair or a claim of numerical equivalence.

Only exactly symmetric finite matrices reach unpivoted, status-only Cholesky.
Failures remain \code{not_positive_definite_at_saved_precision}; nonexact
symmetry skips Cholesky. Matrices and keys are never dropped, symmetrized,
clipped, jittered or replaced. No inversion, regression or structural model
follows automatically.

Source precisions 1 through 18 are recorded per chain. Missing precision and
the provider-default sentinel remain unknown; mixed known precisions stay
explicit. Unknown precision gives \code{not_assessed_unknown_precision} for
precision-dependent comparisons, while exact symmetry, range and Cholesky
checks remain available. Explicit retained run/provenance precision must
agree with every CSV header. Historical source precision is never increased.

With \eqn{D} draws and \eqn{K} components, source width is
\eqn{V=2K^2+K}, and output accounting uses \eqn{W=6K^2+2K+5}.
Admission requires \eqn{DW} output values and
\eqn{D(K^3+12K^2+2K)} matrix-work units. Two source conversions per requested
cell reserve \eqn{2DV}. Consumer numeric slots add
\eqn{4DW+12K^2+8K} to the unchanged reader formula. Matrix-work units are
declared work units, not exact CPU instructions or a wall-time guarantee.

The separate context policy admits copied scientific maps, prior context,
selected original statuses, character statuses, labels and receipts before
fresh copying. Its receipt discloses canonical counts, count/copy traversals,
and conservative numeric metadata and index/copy workspace reservations.
Draw-dependent annotation receives a second admission after source dimensions
are known and before result arrays are allocated. These policies do not bound
total process RSS. The historical main-spec serialization seal remains a
separately disclosed unknown-cost operation. Portable immutable validation
retains its protected stored allowance and uses the distinct operation receipt
\code{bmeas_component_context_validation_v1}; old capability lists and
immutable digests are unchanged.

Current or protected-original \code{proven_improper} or
\code{proven_non_normalizable} evidence refuses with
\code{E_COMPONENT_POSTERIOR} before source acquisition. Other retained
diagnostic statuses remain literal and are not upgraded by successful
extraction. Source paths, hashes and actual read counts are retained; read-time
hashes do not authenticate historically unobserved sampler-time bytes.

When \eqn{K=0}, status is \code{not_applicable_no_components}, arrays and
draw keys are empty, and there is no posterior acquisition. This says nothing
about whether target-predictor covariance exists.
}
\value{
A self-contained plain-data \code{bmeas_component_draws} object. The
\code{correlation}, \code{covariance}, \code{log_abs_covariance},
\code{original_available}, \code{derived_available} and \code{available}
arrays have dimensions draw by component by component. \code{sd} and
\code{log_sd} are draw by component. Their draw-axis dimension names are
\code{NULL}; component axes use stable coordinate IDs. \code{draw_keys}
contains \code{chain_id} and \code{saved_row}; the latter is retained CSV row
rank, not original sampler iteration.

The result also includes \code{coordinates}, \code{maps}, \code{prior},
\code{upstream}, \code{matrix_status}, per-chain \code{precision},
\code{source}, \code{work}, and \code{context_receipt}. There is no backend,
executable callback or source-reading closure. Saving this result preserves
these extracted values, not the rest of the measurement fit or posterior CSVs.
}
\seealso{\code{portable_fit}, \code{correlation_draws},
\code{refresh_diagnostics}}
