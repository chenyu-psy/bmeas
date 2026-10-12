\name{bmeas-predictive}
\alias{ppc_plan}
\alias{assess_measurement_fit}
\alias{diagnostic_budget}
\title{Explicit Bounded Predictive Checks}
\description{Plan or compute descriptive response discrepancies from aligned joint draws. Planning and cached reporting are passive. Assessment is an explicit, bounded operation; no LOO, refitting or compilation is performed.}
\usage{
diagnostic_budget(max_draws, max_observations, max_simulated_values,
  max_source_bytes = NULL, max_posterior_rows = NULL,
  max_draw_values = NULL, max_latent_values = NULL,
  max_statistics = NULL, max_pair_terms = NULL, posterior = NULL,
  max_work_values = NULL)
ppc_plan(fit, draws, budget, level = NULL)
assess_measurement_fit(fit, checks = "ppc", level = "population",
  draws, seed, budget, simulator = NULL)
}
\arguments{
\item{fit}{A saved main-API fit, \code{bmeas_portable_fit}, \code{bmeas_fit_archive}, or historical measurement_fit. Main-API and portable assessment require supported structured context, saved source identity and recorded sampling CSV paths.}
\item{draws, max_draws}{Selected joint states/response replications, at least 2, within explicit finite budget. More than 20 requires the main-API streaming policy.}
\item{max_observations}{Maximum complete consumed response rows per replication, default policy 1000.}
\item{max_simulated_values}{Maximum draws times consumed response cells, default policy 20000. M3-CS counts all ten cells per row.}
\item{max_source_bytes}{Optional lowered source-byte ceiling; frontend default 10485760.}
\item{max_posterior_rows}{Optional lowered ceiling for all retained source rows across chains; default 100.}
\item{max_draw_values}{Optional lowered ceiling for all retained rows times required scalar columns before selection; default 50000.}
\item{max_latent_values}{Optional finite allowance for fresh latent Normal values; default 10000.}
\item{max_statistics}{Optional finite allowance for the number of discrepancy statistics; default 200.}
\item{max_pair_terms}{Optional finite allowance for observed-plus-replicated paired-person products; default 50000.}
\item{max_work_values}{Main-API conservative logical allocation/work-slot allowance, default 1000000. Includes geometry, scalar names, operands, statistic workspace and pair scanning. Explicit higher representable allowances are permitted. Does not guarantee peak RSS or elapsed time.}
\item{budget}{An explicit diagnostic_budget. Original three-field legacy payloads are unchanged; main consumers resolve additional work defaults or an explicit streaming policy.}
\item{posterior}{NULL preserves the bounded acquisition path. A main-only plain nested list with reader="streaming", max_source_bytes, max_read_bytes and max_materialized_values selects the package-owned streaming parser. It cannot be mixed with outer legacy source/row/value overrides. See bmeas-posterior-source.}
\item{level}{For main-API fits, explicitly supply population or conditional to both functions. Population preserves the declared iid law or complete same-N centered group; conditional holds inferred person predictors fixed and does not assess population measurement structure. The legacy default remains population.}
\item{checks}{Only ppc is supported.}
\item{seed}{Explicit positive integer for assessment. Full caller RNG kind/state is restored.}
\item{simulator}{Main-API M3-CS requires an explicit compile_m3cs_simulator result. Basic families use canonical R kernels and require NULL. Historical routes retain their own simulator contract.}
}
\details{
For a \code{bmeas_portable_fit}, assessment verifies immutable context using the
finite allowance stored by \code{portable_fit}, separately from predictive and
posterior budgets. Its source receipt retains a \code{context_validation}
phase. Planning and cached reports inspect saved metadata only, explicitly
without fresh integrity verification. Protected original proved-improper or
non-normalizable statuses still refuse both planning and assessment.

The main-API slice supports saved canonical Gaussian, binomial-logit and lognormal observation contracts, including fixed/task operands and mixed blocks. It reuses each generated person effect across blocks and repeats. Centered replication retains the original full roster, task order and group dependence; it does not recenter a subset. One or exactly two live pinned M3-CS blocks are additionally supported through compile_m3cs_simulator; see bmeas-m3cs-predictive. Other cognitive and arbitrary custom simulators remain unsupported.

Live/portable planning reads metadata only. Archive planning additionally reopens and verifies its archive context; portable and archive single-block pinned M3-CS planning and explicit execution are supported by the separate saved reader with a matching rng_v2 simulator. Saved two-block prediction remains refused; live two-block prediction uses a separate joint path. Full/routed M3, three-plus or mixed cognitive blocks, SDM and EZ prediction remain unsupported. On the original bounded path, assessment validates and copies complete recorded sample/HMC CSVs with explicit save_warmup=0 before using the qualified CmdStanR 0.9.0 reader. One or two distinct chain IDs with equal positive retained counts are supported. The complete materialized source projection, not just selected draws, is budgeted. No draw values are reconstructed from cached means or intervals. A read-time receipt binds current bytes to saved identities; it cannot authenticate unchanged historical output when original fit-time hashes are absent.

With an explicit streaming policy, the package-owned parser accepts multiple equal complete CmdStan 2.40.0 chains within finite source/read/record/column/numeric-buffer limits. It validates required values across every source row and materializes only selected columns/states. Balanced selection includes every chain, so the explicit replication allowance must cover at least one selected state from each chain. Exact keys are independent of path/chunk order. The whole-call RNG restoration still surrounds acquisition; response simulation starts afterward. Source rows, selected rows, metered reads, reserved hash passes and declared buffers have separate receipt fields. Larger geometry requires explicit allocation admission; arbitrary custom simulators and new inference qualification do not follow. Portable execution supports the canonical basic families and the narrow single-block pinned M3-CS saved-reader contract; no other cognitive simulator capability follows.

Marginal response and paired-person response-covariance discrepancies retain units, overlap, missing/degenerate groups and finite replication coverage. Empirical 5/50/95 percentiles use R quantile type 7. These potentially dependent selections are descriptive screens, not calibrated tests or a good-fit verdict. Sampling quality, identification and posterior normalizability/propriety remain separate; retained proofs of improper or non-normalizable targets refuse assessment.

Geometry requires representable dimensions, JT<=J and checked matrix, name-map and statistic-work accounting within max_work_values. There is no silent subsetting. Required nonfinite arithmetic, informative inverse-logit saturation and lognormal overflow/zero underflow fail without clipping, retrying or dropping replications. After acquisition begins, an ordinary failure returns a failed current cache and clears older predictive tables; interrupts propagate after cleanup. The input fit is unchanged. Cached summary and fit_checks read the result without files, backend access, simulation or RNG changes.

Historical measurement_fit predictive routes retain their earlier separate scientific and resource scopes. Additional budget fields apply only to the new frontend.
}
\value{diagnostic_budget returns a bounded budget object. ppc_plan returns saved-domain and worst-case work metadata without reading posterior files. assess_measurement_fit returns a copy of the fit, preserving its main-API or portable class and immutable portable identity where applicable, with the current predictive/measurement-check cache updated; upstream sampler/scientific records are preserved.}
\seealso{\code{fit_checks}, \code{summary}, \code{bmeas}}
