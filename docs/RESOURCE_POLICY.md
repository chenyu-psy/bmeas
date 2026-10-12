# Plan a run and choose its resources

The main API separates model support, resource admission and evidence about a fitted result. The former tiny run limits were development-validation limits. They are now an explicit `resources="development"` preset rather than an implicit limit on every user's model.

This R-only increment changes planning and lifecycle behavior. It adds no new likelihood, prior, correlation chart, native qualification or study-scale inference evidence. The scientific generator and its output fields retain their existing scope.

## Prepare, plan, then explicitly fit

```r
d <- data.frame(person=1:4, x1=c(.2,.8,.3,1),
                x2=c(.7,.6,-.3,.1), x3=c(-.2,.6,.9,.4))
s <- bmeas(bmf(G=~x1+x2+x3), d, subject="person", fit=FALSE)
get_prior(s)
check_model(s)
policy <- list(max_output_bytes=256*1024^2, cache="deferred")
p <- execution_plan(s, chains=4, iter=2000, warmup=1000,
                    cores=4, threads=1, resources=policy)
p
# Fitting is a separate explicit request; this example does not run it.
# fit <- bmeas(s, chains=4, iter=2000, warmup=1000,
#              cores=4, threads=1, resources=policy)
```

The tiny data illustrate syntax, not scientific adequacy. The 256 MiB allowance is this example's chosen output limit, not a recommended universal budget or memory guarantee. `iter` includes warmup: this request configures four chains with 1,000 retained states each. Four parallel single-thread chains request four simultaneous sampler workers; four sequential chains request one.

Without `resources`, main planning still reports the requested shape and costs, with a policy-required result. Main fitting requires an explicit finite policy before backend work. `fit=FALSE` remains preparation only. The legacy `measurement()` / `run_config()` / `execution_plan(legacy_spec, run)` workflow retains its documented historical behavior.

Planning uses the prepared structural tables. It does not compile, sample, read posterior files, construct dense person-score initializers or populate fit caches. Existing scientific validation can inspect pinned source resources, so this is not a promise that planning never reads any file. Preparation happened before the plan and is not covered by its subsequent-allocation estimates.

## Policy fields

Use a plain named list with exactly recognized fields. Required fields are `max_output_bytes` and `cache`, which is `"deferred"` or `"eager"`. Eager policy additionally requires `max_cache_draw_values`. The current implementation cannot certify a general provider-wide eager materialization schedule, so a caller eager request is refused with an explicit unavailable-estimate reason. It never silently switches to deferred mode. The named development preset retains the earlier bounded eager compatibility path.

Optional tighter quotas are:

| Field | Meaning |
| --- | --- |
| `cpu_budget` | Maximum simultaneous sampler workers: `min(chains, cores) * threads` |
| `max_subjects` | Full unique person domain, including the complete centered roster |
| `max_observations` | Sum of consumed likelihood rows over all blocks |
| `max_saved_draws` | Configured retained states over all chains |
| `max_targets` | Logical predictor roles, including task and fixed aliases |
| `max_observation_scales` | Free observation-scale coordinates |
| `max_latent_coordinates` | Common plus parameter-residual population dimensions, K+U |
| `max_coefficient_primitives` | Unique free loading, component-SD, amplitude, total-SD and share-contrast coordinates |

These are quotas, not model-validity criteria. Actual sampled axes and expanded output cells are reported separately. A centered task model has JT times N−1 sampled white axes; K+U does not count those person-level axes.

Byte and aggregate counts must be finite, exactly represented integers. Checked doubles support totals above the 32-bit integer range, up to 2^53−1; backend controls and dimensions retain their narrower representable ranges. Zero is allowed only for optional dimension quotas with an empty corresponding domain. CPU, output, subject, observation, saved-state and supplied cache-value allowances are positive. Unknown, duplicate, classed or dimensioned policy fields are refused. A cache-value allowance accompanying deferred mode is recorded as inapplicable.

## What the estimates mean

Where a supported lowering exists, output estimates count the unchanged generated fields and configured saved rows, with disclosed formatting/sampler assumptions. A genuinely unavailable lowering retains geometric counts but has no emitted inventory or output-cost estimate; resource admission cannot rely on a nonexistent program. A source-available but runtime-unqualified model remains separately blocked from fitting. The CSV-byte estimate is an admission heuristic and is checked again against actual file sizes after the sampler returns. It is not a hard disk quota. Missing or unstatable sizes are unknown, not zero. An oversize or unavailable-size outcome preserves available output paths and the successful sampler receipt in its typed resource condition.

The eager cache quota means a cumulative sum of posterior scalar slots across materializations, including provider-wide loads, repeated acquisitions and explicit retained/copied arrays. It is not the size of the final summary table or the count of unique requested columns. Reuse or early release is not subtracted. When the provider's complete schedule cannot be conservatively counted, eager admission is unavailable. This count would still not bound peak process memory: internal copies, parser/R overhead, linear algebra workspace, compilation and sampler memory are additional.

There is no package wall-time or operating-system memory enforcement. Sampler-worker counts do not control compiler jobs, BLAS threads or every process thread. The development validation harness's separate timeouts are not a product guarantee. Larger thread counts have no implied speedup or bitwise-equivalence qualification.

## Deferred diagnostics are explicitly uncomputed

Deferred mode makes no bmeas post-return calls to backend summary, HMC diagnostics, output-text capture or draw-dependent cache builders. The provider construction described below still scans CSV metadata. A successful provider return, with complete zero return-code evidence, remains a genuine fit with its saved specification, source, run policy, initialization/scientific checks and available execution receipt. Configured saved states remain configured counts; metadata scanning does not parse, validate or count-reconcile retained posterior rows.

`summary(fit)` and `fit_checks(fit)` remain passive. They distinguish completed execution from posterior diagnostics deferred by policy. Missing Rhat, ESS, MCSE, HMC, numerical-message and matrix-screen values are unavailable, never favorable zeros. `posterior_summary(fit)` has no posterior table until one has been explicitly computed by a supported path. Reading a report does not start that work. The numerical-message producer's existing status schema is unchanged; deferred reporting has its own record.

Use `diagnostic_parameters(fit)` and an explicit separately budgeted `refresh_diagnostics()` call to compute supported selected population and HMC diagnostics from all retained source states. Numerical-message collection, matrix screens and other unrelated reports remain in their prior states. This workflow requires consistent retained specification identity; see [DIAGNOSTIC_REFRESH.md](DIAGNOSTIC_REFRESH.md) for the saved-fit namespace limitation. More iterations or a larger resource allowance do not establish convergence, identification, posterior propriety or useful inference. Historical fits preserve their original kinds, cache values and evidence.

## Provider construction still reads metadata

This initial deferred path is pinned to CmdStanR 0.9.0, CmdStan 2.40.0 and the reviewed non-Windows/non-WSL POSIX branch. Runtime compatibility and the resolved output destination are checked before compilation. Pure planning only records that expected scope; it does not load or discover the provider.

The fitter passes the public `diagnostics=NULL` argument in deferred mode. That skips posterior and sampler-diagnostic numeric draw arrays. CmdStanR still calls `diagnostic_summary(NULL)` internally, which immediately returns an empty list, and scans each entire CSV for metadata before returning. It retains column names, run/timing fields and adaptation/inverse-metric values. This is not zero CSV I/O, zero numeric allocation, constant-memory metadata work or a prefix-only scan. It occurs before bmeas can measure actual output-file sizes.

The plan reports configured header-name/column counts, a count-only header-text estimate and default-diagonal inverse-metric slots from the actual unconstrained parameter dimension. Names are not expanded into a large array merely to estimate them. Actual file bytes, metadata string/object overhead, parsing buffers and temporary copies remain unknown. No dense metric support or extra RSS/time guarantee is introduced.

The reviewed provider quotes CSV paths in a shell command. The actual expanded/resolved output directory must be valid plain text without single quotes or control characters. Ordinary spaces are allowed. Relative paths, tilde expansion and resolved directory components are checked; the package does not rename or sanitize an incompatible destination. Generated CSV basenames remain package-controlled. This is a provider compatibility boundary, not a general security sandbox.

If the provider call fails, including a metadata-construction error after its subprocesses may have finished, the error retains its original cause and known output location. Sampler completion stays unknown without actual return-code evidence. No successful fit is fabricated, no retry is started and output is not deleted. Interrupts propagate. After a successful provider return, the ordinary complete return-code and output-size checks apply.

## Scientific and consumer limits remain visible

Missing/invalid priors, conclusive identification or propriety failures, unsupported observation contracts, unsupported correlation charts/mode combinations, numerical initialization failures and explicit experimental opt-in remain enforced. Models with no free parameters remain inspectable but cannot be sampled. Raising a resource quota does not alter any of these decisions.

The legacy bounded PPC and individual readers retain their 1–2-chain/100-row/10MiB/50,000-value limits. An explicit consumer `budget$posterior` streaming policy offers a separate source path with finite byte/read/buffer limits; it is never inferred from fit resources. PPC retains its scientific family/person-law constraints. Its main streaming path permits explicitly higher replication and geometry/statistic work allowances under checked allocation admission; finite defaults and the legacy 20-state selection remain. Individual summaries require complete reporting-work allowances. See SCALABLE_PPC.md for current PPC work accounting. The resource forecast distinguishes old-reader incompatibility, streaming-policy requirements and operation-specific limits. Provider header metadata does not validate actual source rows or establish consumer support. No chains, people or saved states are silently dropped; see SCALABLE_POSTERIOR.md.

Broader empirical validation and general diagnostic-cache acquisition remain separate work. General multidimensional affine correlations, other unsupported common-correlation modes, custom/cognitive PPC and study-scale inference remain outside this resource amendment.
