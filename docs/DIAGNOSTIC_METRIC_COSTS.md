# Diagnostic work and allocation limits

These limits describe the explicit diagnostic workflow. Package-owned numeric slots are a finite admission measure, not a whole-process memory guarantee.

## Pinned source-visible provider schedule

Each selected monitor enters as its intact L-by-C matrix. Empirical summaries use all D=C*L states. For eligible convergence work, h=floor(L/2), split columns=2C and split states A=2C*h. Odd middle rows are absent only from the estimator's private split. No external public split operation is called.

Rhat forms rank-normalized split values and a folded counterpart. Its working sequence includes split matrices, ranks/order scratch, normalized values, a median, folded values, and chain means/variances. Bulk ESS uses ranked splits; two tail ESS calculations use quantiles and indicator splits; mean MCSE uses the raw-mean ESS path and sample SD. Each ESS forms an h-by-2C autocovariance array plus chain and lag vectors. Autocovariance uses a padded real vector of F=2*nextn(h), forward/inverse complex FFT intermediates, magnitudes/squares, and h-lag results. One complex element has two real components. Four ESS evaluations yield at most 16*C*M length-F FFT calls. Rhat adds none. Short or constant inputs can do less, but never reduce admission.

Source references: [posterior 1.7.0 convergence](https://raw.githubusercontent.com/stan-dev/posterior/v1.7.0/R/convergence.R), [private split](https://raw.githubusercontent.com/stan-dev/posterior/v1.7.0/R/split_chains.R), [CmdStanR 0.9.0 BFMI formula](https://raw.githubusercontent.com/stan-dev/cmdstanr/v0.9.0/R/utils.R).

The schedule describes dimensions and call multiplicities. It does not bound provider/R/sort/FFT/dependency temporaries, native workspace, strings/object overhead, the existing fit/backend, or peak RSS. Those costs remain explicitly unknown. R 4.5.3/64-bit is the reference environment; there is no patch-exact R admission restriction. The profile records actual R/platform, posterior, matrixStats, checkmate and stats versions. HMC-only work neither loads nor requires posterior or CmdStanR.

## Concrete package-owned numeric-slot inventory

The dimensions helper emits every named item below, using checked exact counts. All items are conservatively charged simultaneously; serialized phases do not excuse copies. A slot counts a numeric, integer, logical or index element, not a byte or total object size. When M=0 the parameter-specific working slots are zero.

- One extracted monitor vector D; one reshape copy D; two finite-validation masks 2D; one global-constant mask D; a chain extraction plus comparison mask 2L; and C chain-loop indices
- Retained per-chain flags C*M
- Parameter records 56*M: nine records containing raw, value, evaluated and full-row denominator (36), nine public values (9), seven count fields (7), one global-constant flag (1), the retained pointer-byte field (1), and two binding-construction flags (2). Per-chain flags are separately counted
- A separately assembled nine-column numeric summary table 9*M. Assembly reserves another 36 scalar slots for the current monitor, including empirical outputs, provider scalar outcomes and value assembly
- HMC extraction of three L-vectors 3L and optional integer-to-double energy conversion L. Reserve three validation masks 3L, the energy constant mask L, and count masks 2L
- Difference operands/indices 4*(L-1) and retained differences/squares 2*(L-1)
- HMC records 46*C: five records with four numeric/logical fields (20), five public values (5), three four-field coverage records (12), the energy flag (1), three count fields (3), the depth value (1), and four arithmetic provenance values (4)
- A separate five-column HMC numeric table 5*C; 32 current-chain assembly scalar slots; and 256 metric-accounting scalar slots for validated dimensions, profiles, admission records and loop/result assembly

The 36/32/256 reserves are explicit conservative fixed scalar allowances, not provider-memory estimates. Returned per-monitor records do not retain input matrices or work vectors. Only the current monitor and current chain require extraction/work buffers.

The existing reader must continue to reserve its own projection and assignment overlap 2*D*V; chain/rank keys and construction overlap 4D; required maps/current-row slots 6V; alias-index validation overlap 4A_alias; and its existing parser, header, per-chain and reservation-accounting buffers. V counts all selected canonical, declared alias and present sampler columns. These reader buffers are not included again in the metric-only inventory. The refresh reader adds the metric inventory and the integration buffers below to its admission before posterior projection. Full numeric conversion remains 2*D*V across validation and projection. Unknown provider costs cannot be represented as zero.

## Return records and local absence

Parameter results contain `values`, `metrics`, `global_constant`, `chain_constant`, `counts`, and method/version `provenance`. The nine values are mean, median, sd, q5, q95, rhat, ess_bulk, ess_tail and mcse_mean. Every metric record stores raw/value/outcome/status/reason/evaluated/denominator/warnings. Outcomes distinguish finite, positive_infinity, negative_infinity, NaN, NA and not_evaluated. Unavailable public values are NA while the raw outcome remains intact. Valid positive-infinite Rhat stays public. Exact constant flags do not replace the pinned provider's tolerance or force a partially constant monitor to NA.

HMC results use num_divergent, fraction_divergent, num_max_treedepth, fraction_max_treedepth and ebfmi, with per-field presence/finite/missing coverage, full retained-row denominators and raw BFMI numerator/variance/ratio. Finite, nonconstant zero SD makes dependent mean MCSE locally unavailable without rewriting its raw return. BFMI numerator, variance or ratio overflow/underflow is local unavailable evidence. Sampler values present on an otherwise ineligible path are still strictly validated first. No partial finite-row subset is used.

The optional `max_depth` input is NULL or an exactly named plain list:

- Available: `list(status="available", value=<positive exact integer>, reason=NA_character_, raw=<plain character vector of header evidence>)`
- Unavailable: `list(status="unavailable", value=NA_real_, reason=<one of missing_max_depth, duplicate_max_depth, malformed_max_depth, contradictory_max_depth>, raw=<plain character vector of header evidence>)`

NULL normalizes to unavailable/missing_max_depth. The diagnostic reader interprets malformed/duplicate/contradictory optional header evidence into an unavailable record. This helper rejects malformed *record shapes* with a typed error; it never calls arbitrary methods or tries to reparse unsafe metadata. An available record is supplied only after header validation. Static-HMC depth metrics are not_applicable. No observed depth or saved default supplies a missing limit.

BFMI assumes the caller has validated complete saved-row ranks for one original chain. It additionally requires thin=1, L>=3 and nonconstant energy. Its numerator divides the squared-difference sum by L, and its denominator is sample variance. Values under thin>1 remain unavailable rather than being labelled an adjacent-transition metric.

## Provider-binding scalar reserve

An additional 64 fixed scalar slots cover concrete binding construction/validation when parameters are requested. Function/environment/AST object overhead remains outside the numeric-slot quota; see DIAGNOSTIC_BINDINGS.md.

## Reader/controller integration

Beyond the metric inventory and existing reader buffers, reserve L current-chain row indices, M canonical-column indices, three sampler-column indices, 3M selected-catalogue logical flags, 8C copied chain/report metadata slots, 256 controller/report assembly scalars, 4C optional-depth manifest slots and 64 source-profile scalars. Their total is L + 4M + 12C + 323. The 30 metric/integration entries also enter the reader's existing eight-accounting-slots-per-entry reservation. All optional values still consume admitted buffers when present; shorter/constant arrays do not excuse the configured size admission.

## Retained catalogue reservation

Metadata discovery/selection first uses its separate finite catalogue/context row limits. Before any posterior source is read or a draw buffer is allocated, the complete new catalogue remains part of the combined numeric-slot admission, even for HMC-only requests. Let K be the complete catalogue row count and M the selected count. The additional fixed entries are 3K retained logical flags, 3K conservative row-record construction flags, the catalogue numeric metadata payload counted once, at most two compact stored catalogue row-name slots, K row-name expansion slots, M selected row-name slots, M selection-index slots and 3M conservative selected row-record construction flags. The selected output's 3M logical columns were already charged separately in the integration inventory and are not charged again here. Shared metadata referenced by catalogue/selection is counted once. Construction/index reserves are conservative across serialized metadata phases, not measured simultaneous copies or a total R heap bound. The numeric quota does not replace the earlier metadata-row admission.

These fixed context entries enter the existing reader consumer-buffer reservation, including its eight accounting slots per named entry. The original fit/backend are preexisting state; they are not mislabelled as new catalogue buffers. Retained-context hashing/serialization and R object/character/provider/native allocation remain separately disclosed unknown costs.
