# R-only export fixture copied from frontend-archive-consumers.R.
with_checks <- function(action) {
 old <- get(".bh_checks",ns); locked <- bindingIsLocked(".bh_checks",ns)
 if(locked)unlockBinding(".bh_checks",ns)
 assign(".bh_checks",cd_metadata_checks,ns)
 if(locked)lockBinding(".bh_checks",ns)
 on.exit({if(locked)unlockBinding(".bh_checks",ns);assign(".bh_checks",old,ns);if(locked)lockBinding(".bh_checks",ns)})
 action()
}
cd_metadata_checks <- function(spec) {
  # Same complete historical record shape as the reviewed portable producer.
  # Used only while creating these synthetic declarations; no scientific claims.
  na <- list(status="not_assessed",reason="Engineering declaration only; no scientific evaluation")
  list(structure=na,correlation_feasibility=na,correlation_lowering=na,
    correlation_prior_geometry=na,identification=na,
    priors=list(status="not_assessed",invalid=data.frame(),missing=data.frame()),
    gaussian_diagnostics=list(status="not_assessed",scope="Engineering declaration only"),
    posterior_propriety=na,posterior_normalizability=na,
    numerical_qualification=list(status="not_assessed",eta=NA_real_,reason="Engineering declaration only"),
    initialization=list(status="not_assessed",scope="No sampler initialization"),
    backend=list(status="not_assessed",reason="No backend execution",support=NULL,qualification=NULL,
      evidence_version=NULL,evidence_scope=NULL,additional_evidence=NULL,compiler_policy=NULL,limitations=NULL))
}
cd_shell <- function(spec, paths) {
  backend <- new.env(parent=emptyenv())
  makeActiveBinding("must_not_inspect",function(...)stop("Forbidden backend inspection"),backend)
  structure(list(spec=spec,
    run=structure(list(chains=2L,parallel_chains=1L,threads_per_chain=1L,iter_warmup=0L,
      iter_sampling=4L,seed=1L,cpu_budget=1L,max_observations=16L,max_subjects=4L,
      max_saved_draws=8L,reduction="serial",grainsize=1L,max_treedepth=4L,model_budget=NULL,
      kind="engineering_metadata_only",wall_time_enforcement="not_run",memory_enforcement="not_run"),
      class="measurement_run_config"),backend=backend,
    cache=list(computation=list(status="warning",reason="Literal synthetic adverse history"),
      inference=list(status="not_assessed",reason="No posterior qualification"),
      posterior_propriety=list(status="not_assessed",reason="No propriety evaluation"),
      posterior_normalizability=list(status="not_assessed",reason="No normalizability evaluation")),
    code="ENGINEERING ONLY; NO MODEL GENERATED OR EXECUTED",
    provenance=list(kind="engineering_synthetic_fixture_no_posterior_qualification",
      started=NULL,finished=NULL,compile_key="not_generated",cache_hit=FALSE,
      executable_sha256="unavailable_no_executable",toolchain=list(cmdstan=character(),cmdstan_path=character(),
        cmdstanr=character(),R=character(),platform=character(),compiler=character(),compiler_version=character(),
        environment=character(),make_local=character(),cpp_options=list(),stanc_options=list()),
      initialization=list(status="not_run",scope="No sampler initialization"),model_fingerprint=spec$fingerprint,
      iter_total=4L,execution_profile=list(requested="engineering_metadata_only",used="not_run"),
      exploratory_requested=FALSE,exploratory_rank_override=FALSE,save_log_lik=FALSE,
      estimated_csv_bytes=NA_real_,output_files=paths)),class=c("bmeas_fit","bhmm_fit"))
}

