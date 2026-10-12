\name{bmeas-posterior-source}
\alias{bmeas-posterior-source}
\title{Explicit streaming posterior-source policy}
\description{A main-API consumer budget may contain a plain nested posterior policy with reader="streaming". It validates complete retained CSV sources and projects only required columns and selected states under explicit finite acquisition and consumer-work allowances. It does not fit a model or populate a general diagnostic cache.}
\details{
Required nested fields are reader, max_source_bytes, max_read_bytes and max_materialized_values. The reader value is exactly streaming. Optional overridable defaults are max_record_bytes=1048576, max_columns=100000, max_source_rows=1000000, max_chains=32, max_temp_bytes=2*max_source_bytes and max_scan_values=2*max_source_rows*V, where V is the exact required width. Use finite integer-valued plain scalars with checked representation. The record default is not guaranteed to fit a wide generated source.

Do not mix this policy with outer legacy max_source_bytes, max_posterior_rows or max_draw_values fields. Original budget forms remain unchanged. The streaming grammar is complete CmdStan 2.40.0 sample/HMC with exact false/true/0/1 warmup tokens, permitting only false/0. Equal complete chains, unique actual IDs, complete headers/footers and declared/actual counts are required. All required values and individual shared aliases are validated on every source row. No chain omission, shortest-chain truncation or second thinning is applied.

The initial schedule reserves seven complete logical source-byte reads and two owned copies. Copy/scan bytes are metered, bounded owned-file hash bytes reserved and labelled separately. Header/comment/data records and columns are bounded before corresponding allocations. Only artifacts actually created by this transaction are cleaned. Interrupts propagate after cleanup. Current-file identity does not authenticate original sampling-time outputs.

max_materialized_values is a conservative simultaneous declared numeric-slot envelope, itemizing projected values, keys even at zero width, maps, current rows, known overlaps and consumer workspace/output. It is neither only the selected matrix size nor the cumulative eager-cache quota. Strings, object overhead, invisible R/native copies and RSS remain unknown; no hard disk, memory or wall-time enforcement is provided. Consumer spec serialization and original fit storage are outside the CSV ledger.

PPC keeps its scientific response and person laws. The main streaming path admits higher explicit replication, geometry and statistic work allowances under checked allocation admission. Every chain contributes at least one selected state, and at least two states are required. The legacy acquisition path retains at most 20 replications. Individual summaries default to all validated rows or use an explicit balanced subset, with exact empirical type7 quantiles and separate source/selected/omitted counts. Larger complete cells/context/output/transform work needs the seven explicit individual allowances. No new diagnostic, approximate quantile, response model, fitting qualification or inference claim is added.

Planning and saved-result reporting perform no source acquisition. Fitting resource policies do not automatically supply this consumer policy. Full syntax and scope are described in docs/SCALABLE_POSTERIOR.md.
}
\seealso{\code{diagnostic_budget}, \code{ppc_plan}, \code{individual_parameter_summary}, \code{execution_plan}}
