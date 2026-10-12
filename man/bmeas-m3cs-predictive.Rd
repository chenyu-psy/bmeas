\name{compile_m3cs_simulator}
\alias{compile_m3cs_simulator}
\alias{make_m3cs_simulator_code}
\alias{m3cs_simulator_data}
\alias{m3cs_probabilities}
\title{Explicit Main-API M3-CS Predictive Simulator}
\description{Prepare and explicitly compile the bounded, hash-pinned complex-span
response simulator for a main-API specification containing one or exactly two canonical M3-CS blocks.
Assessment never compiles implicitly and performs no posterior fitting or LOO.}
\usage{
make_m3cs_simulator_code(spec, file = NULL, budget = NULL)
compile_m3cs_simulator(spec, cache_dir, budget = NULL)
m3cs_simulator_data(spec, eta, draw_response = TRUE, budget = NULL)
m3cs_probabilities(spec, eta, simulator, budget = NULL)
}
\arguments{
\item{spec}{A retained main-API specification containing one or exactly two canonical
\code{m3_cs_model} blocks. Portable execution and arbitrary adapters are refused.}
\item{budget}{Optional diagnostic budget or resolved finite work policy. NULL uses the published finite defaults. Larger source/data preparation requires an explicit higher policy; assessment propagates its admitted policy.}
\item{file}{Optional generated Stan source destination.}
\item{cache_dir}{Explicit compilation cache directory. Requires an installed CmdStan toolchain.}
\item{eta}{Numeric full-roster by target matrix in saved target-index order.
Required free-operand cells must be finite; unused cells may be missing.}
\item{draw_response}{Logical: generate multinomial responses or probabilities only.}
\item{simulator}{The source-, resource-, and executable-bound result of
\code{compile_m3cs_simulator}.}
}
\details{Use the simulator with \code{assess_measurement_fit}, explicitly selecting
\code{level}, \code{draws}, \code{seed}, and \code{budget}. The four arguments
\code{a}, \code{c}, \code{ra}, and \code{rc} are received on predictor scales;
fixed, task, and subject-task operands retain their authoritative saved bindings.
Conditional replication consumes aligned joint eta cells. Population replication
uses the main-API iid common-plus-residual or same-N sample-centered person law,
including the entire declared roster, even people without observed responses.

The original external \code{cs_logits} implementation retains its stabilization,
including its large-negative activation floor; no additional clipping is applied.
Responses retain ten columns, active categories 1--4 and 10, original options,
and observed row totals. Each replication runs exactly one single-threaded
fixed-parameter draw, zero warmup, and zero model parameters.

Discrepancies pool repeated rows within each person/task before equal-person
category-rate means and sample SDs. All cross-task category pairs use their actual
positive-total person overlap and covariance denominator overlap minus one.
Zero-total people are excluded. Three tasks produce 30 marginal and 75 covariance
statistics. Conditional association is not population measurement-structure fit.
Intervals are coarse descriptive screening, not calibrated tests or fit verdicts.

Explicit finite predictive and allocation allowances apply to the complete design; the published defaults remain finite. The v2 simulator source removes only the development O<=1000 ceiling. A separate tiny native engineering gate covered one compile and one fixed-parameter draw each at O=1 and O=1001, not posterior inference or large-study performance. Simulated-value budgets
count every ten-cell response row. Live/portable planning reads no resource or posterior files and does not authenticate
the portable identity; archive planning reopens and validates its archive context.
Compilation and assessment verify resource identity explicitly. Portable fits and
relocated archives support exactly one pinned M3-CS block with an explicitly
supplied matching rng_v2 simulator. No saved live specification is fabricated or
resealed. Optional simulator resource_file specifies a current hash-matched
resource location without changing historical paths. R-double tests include
iid, centered and all-fixed profiles; recorded native saved-reader evidence covers
only prior-source iid and centered fixtures, not the final all-fixed export repair.
Live two-block inputs share one complete eta and one combined response invocation;
statistics use distinct block/task domains and all cross-domain category pairs.
Saved two-block execution remains refused. A separate frozen-source engineering
gate made four cached fixed-parameter calls, without fitting or scientific qualification.
Full routed M3, three-plus or mixed cognitive blocks, SDM, ezDM,
legacy G/P/C mappings, and generic observation-model simulators are outside scope.}
\value{Source text, a compiled simulator object, a Stan data list, or a numeric
observation-by-ten-category probability matrix, respectively. For two live blocks,
probabilities are a block-named list of matrices and simulator data has a row_map
attribute linking simulator rows to block, task, person and observation identities.}
\seealso{\code{ppc_plan}, \code{assess_measurement_fit}, \code{m3_cs_model}}
