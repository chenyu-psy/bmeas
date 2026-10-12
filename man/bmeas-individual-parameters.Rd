\name{bmeas-individual-parameters}
\alias{individual_parameter_summary}
\title{Explicit Bounded Individual Parameter Summaries}
\description{Read aligned saved joint states and summarize declared person-parameter quantities on explicit predictor, natural or likelihood-received scales. This operation does not sample, simulate, refit or update the input fit cache.}
\usage{
individual_parameter_summary(fit, scales, budget, states = "all")
}
\arguments{
\item{fit}{A retained main-API fit or \code{bmeas_portable_fit} with supported structured context and recorded sampling CSV paths, or a historical measurement_fit for the unchanged one-argument legacy route.}
\item{scales}{For main fits, an explicit plain character vector of one to three unique values from predictor, natural and received. Omit for the legacy route.}
\item{budget}{For main fits, either the original plain four-field bounded budget (max_source_bytes, max_posterior_rows, max_draw_values, max_parameter_cells), or a plain streaming form containing posterior plus max_parameter_cells, max_summary_rows, max_transformation_terms, max_declared_observation_rows, max_consumed_observation_rows, max_operand_context_rows and max_context_terms. The old ranges stay unchanged on the old path. New finite work allowances cover complete domains and checked joins before expansion. Omit for the legacy route.}
\item{states}{Default "all" uses every validated retained row. The sole subset form is list(method="balanced", draws=D), requiring the streaming budget and at least two selected states with every chain represented. Selected empirical summaries are labelled separately from full-source summaries. Explicit new options refuse on historical fits.}
}
\details{
Portable fits first validate their immutable context under the finite allowance
stored by \code{portable_fit}, separately from this operation's work budget.
The result's source receipt retains a \code{context_validation} phase, including
when no acquisition is needed. Result inspection and cached reports remain
passive and do not freshly verify context integrity. Original source paths and
protected adverse history are unchanged.

The full saved roster and declared scalar predictor-role domain are reported, including task-level shared aliases, fixed values and mean-only structural-zero targets. Actual eta columns and structured indices are used; cached marginal means cannot reconstruct their joint uncertainty. Observation-scale and structural-scale parameters stay in their existing reports. Routed likelihood literals are separate context, not person-parameter rows.

Natural scales follow declared inverse maps per state; received scales follow the saved eta/theta binding. In particular, SDM log_c/log_kappa keep their declared identity scales. Mean, median, sample SD and empirical 5/95 percentiles use R quantile type 7. Fixed values are labelled known inputs. Nonrepresentable transforms and individual statistics have explicit availability and invalid-state counts, without clipping or finite-subset summaries. A nonconstant represented series whose SD arithmetic underflows to zero has an unavailable SD, while true constant/fixed zeros remain available. No new Rhat, ESS or MCSE is computed.

On the original bounded path, the qualified CmdStanR 0.9.0 reader accepts one or two balanced sample/HMC chains without saved warmup. All validated retained rows are used in saved-row-rank then numeric-chain order, with at least two rows total. Current-input hashes and supplied identities do not authenticate original fit-time outputs. Missing sources and known improper/non-normalizable targets refuse explicitly. Synthetic qualification and upstream diagnostic limitations remain visible.

On that bounded path, additional hard caps are three scales, 3000 output rows, 300000 scalar transformation terms, 1000 declared and consumed observation rows each, and 10000 saved operand/context rows. Required values count all inspected columns, including shared-alias verification. A zero-column fixed-only projection retains a source receipt; a domain without predictor parameters returns a typed not-applicable empty table without file access. No silent subsetting is performed.

The opt-in streaming path recognizes complete equal-layout CmdStan 2.40.0 chains and has overridable finite source/read/record/column/buffer limits; it uses no CmdStanR parser. Raw required values and shared aliases are validated across every source row, while nonlinear transforms and exact type7 summaries apply to the stated all-row or selected empirical set. n_draws is the selected denominator; source counts, omitted states, allocations and keys are separate. The declared simultaneous numeric-slot envelope includes projected matrices, keys and known consumer buffers, and is distinct from cumulative transformation work or peak RSS. See bmeas-posterior-source and docs/SCALABLE_POSTERIOR.md. Three scientific scales remain the maximum; no approximate quantiles or automatic thinning is introduced. A genuine zero-predictor result needs no acquisition; a retained balanced request is labelled unexecuted, never fulfilled by zero selected states. Saved selection fields and nonempty request/count denominators are checked exactly.

The input object, RNG and cwd are preserved; temporary copies are cleaned on normal, error and interrupt exits. Saved result inspection and ordinary fit summary helpers are passive. This development slice does not add a paired-comparison API, cognitive PPC, raw-draw export or new inference-quality verdict. The historical one-argument helper retains its exact return behavior and refuses the new options.
}
\value{A data frame with leading class bmeas_individual_parameter_summary. Rows contain person/logical-parameter/scale keys, uncertainty summaries, roles, coverage and numerical statuses. Plain metadata, source_receipt, draw_keys, context, budget and work attributes retain provenance and cost. No live backend or source-reading callback is returned. The legacy route returns its historical table.}
\seealso{\code{posterior_summary}, \code{parameter_summary}, \code{fit_checks}, \code{bmeas}}
