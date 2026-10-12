\name{bmeas-resource-policy}
\alias{execution_plan}
\alias{print.bmeas_execution_plan}
\title{Inspect main-API execution resources without running a model}
\description{Keep scientific eligibility, finite caller resource admission and retained numerical evidence separate. A main-spec plan reports configured dimensions and costs without compiler, sampler, posterior ingestion or dense person-score initialization work.}
\usage{
execution_plan(spec, run = NULL, chains = NULL, iter = NULL,
  warmup = NULL, cores = 1L, threads = 1L, seed = 1L,
  grainsize = 1L, control = list(), resources = NULL,
  save_log_lik = FALSE, exploratory = FALSE, execution = "qualified")
\method{print}{bmeas_execution_plan}(x, ...)
}
\arguments{
\item{spec}{A prepared main-API bmeas specification, or a historical measurement specification for the legacy two-argument call.}
\item{run}{The historical run_config for a legacy specification. Main plans accept their conventional run controls directly; ambiguous duplicate spellings are refused.}
\item{chains, iter, warmup, cores, threads, seed, grainsize}{Finite run controls with the same meaning as bmeas. Iter includes warmup. Configured simultaneous sampler workers are min(chains, cores) times threads. No workers start during planning.}
\item{control}{A plain list with supported max_treedepth and adapt_delta controls.}
\item{resources}{NULL for an unresolved-resource main plan, the exact named development preset, or a plain finite policy with max_output_bytes and cache. Main fitting requires an explicit policy. Cache is eager or deferred; eager also requires max_cache_draw_values. Caller eager admission is unavailable when its complete provider-wide materialization schedule cannot be conservatively counted. Optional tighter quotas are cpu_budget, max_subjects, max_observations, max_saved_draws, max_targets, max_observation_scales, max_latent_coordinates and max_coefficient_primitives.}
\item{save_log_lik}{Whether the planned output includes normalized row log likelihoods. FALSE by default; no LOO is run.}
\item{exploratory, execution}{The existing scientific/profile requests. A resource allowance cannot bypass conclusive scientific failures or an unsupported compiler/chart profile.}
\item{x}{A main-API execution plan.}
\item{...}{Additional print arguments.}
}
\details{
The named development preset retains the former tiny validation ceilings. It is not the main-API default. A plan without a finite policy reports resource_policy_required with the requested counts and estimates; fitting refuses before backend work. The historical measurement-spec execution_plan(spec, run) branch retains its prior smoke semantics.

Positive exactly represented byte/count totals are checked using double arithmetic up to 2^53-1; backend controls and array dimensions retain their narrower integer ranges. Optional target, scale, latent and coefficient quotas may be zero for empty dimensions. Unknown, duplicated, classed or dimensioned policy fields are refused. The latent quota counts K+U population dimensions, separately from sampled person/task-white axes. The coefficient quota counts the existing unique loading, component-SD, amplitude, total-SD and share-contrast primitives. Logical target counts include fixed/task aliases; consumed likelihood rows and known response payload counts are distinct.

A genuinely absent lowering has unavailable emitted-output costs; geometric counts remain inspectable and do not establish resource admission. Source-available but runtime-unqualified modes remain blocked separately. The CSV-byte estimate is a disclosed formatting heuristic plus a post-run size check, not a hard disk quota. Unknown sizes are not treated as zero. A post-return output-resource condition retains the successful sampler receipt and available fit/output paths. Package wall-time and process-memory enforcement are unavailable. Sampler-worker admission does not control compiler jobs or every process thread.

An eager cache quota counts the cumulative posterior scalar slots across provider-wide loads, repeated acquisitions and explicit copied/retained arrays. It is not a final summary-table size or a peak-RSS bound. Unknown provider-wide schedules cause explicit eager refusal. The development preset preserves its bounded historical eager behavior; it does not establish a general provider-independent quota guarantee.

Deferred mode avoids bmeas post-return backend summaries, HMC diagnostics, output capture and draw-dependent caches. The pinned CmdStanR 0.9.0 / CmdStan 2.40.0 POSIX constructor receives diagnostics=NULL but still scans complete CSVs for header/adaptation metadata and calls diagnostic_summary(NULL) as a no-op. Posterior and sampler-diagnostic numeric arrays remain unmaterialized; metadata allocation, scan cost and transient memory are separate unknowns. Runtime version/platform/resolved-path checks precede compilation. Single quotes or control characters in the actual output path are unsupported; ordinary spaces are allowed. Provider-construction failure preserves its cause/location and leaves completion unknown, without retry or a fabricated fit. Passive reports distinguish completed execution from diagnostics deferred by policy and keep inference not_assessed. Configured saved-state counts are not observed CSV counts; provider metadata scanning does not validate or reconcile retained posterior rows. This increment adds no general later cache-acquisition method. Legacy bounded PPC/individual readers retain their separate limits. An opt-in consumer streaming policy offers a separate source path and complete-work allowances, never inferred from fitting resources. Forecasts distinguish legacy incompatibility, streaming-policy requirements and explicit PPC work allowances without inspecting CSV files. Larger main-API PPC geometry and replication requests require checked allocation admission; legacy acquisition caps remain unchanged.

Planning uses prepared tables and existing scientific/source-resource validation. It does not call stancode, standata, backend-IR expansion, dense initialization, a compiler, sampler, posterior reader or fit-cache builder. It does not claim to have bounded preparation that already occurred. Empty dimensions remain inspectable; a model without free parameters still cannot be sampled.
}
\value{A structured main-API plan with separate scientific/profile findings, normalized resources, admission results, configured run counts, output/cache estimates and consumer limitations. The print method returns the plan invisibly. A legacy call retains its historical return.}
\examples{
d <- data.frame(person=1:4, x1=c(.2,.8,.3,1),
                x2=c(.7,.6,-.3,.1), x3=c(-.2,.6,.9,.4))
s <- bmeas(bmf(G=~x1+x2+x3), d, subject="person", fit=FALSE)
policy <- list(max_output_bytes=256*1024^2, cache="deferred")
execution_plan(s, chains=4, iter=2000, warmup=1000,
               cores=4, resources=policy)
# The byte allowance is illustrative; no fit or compilation occurs.
}
\seealso{\code{bmeas}, \code{fit_checks}}
