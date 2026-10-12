source(file.path("helpers", "native-guard.R"))
# Deterministic engineering evidence; no sampling, compilation, or diagnostics.
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root)) for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE)) source(f) else library(bmeas)
(function() {
ns <- if(nzchar(root)) .GlobalEnv else asNamespace("bmeas")
for(n in c(".bh_ppc_forward_plan",".bh_individual_plan",".bh_archive_read",".bh_posterior_ingest_csv",".bh_diagnostic_catalogue"))
 assign(n,get(n,ns),envir=environment())
replace <- function(name,value) { locked<-bindingIsLocked(name,ns);if(locked)unlockBinding(name,ns)
 assign(name,value,ns);if(locked)lockBinding(name,ns) }
# Fail loudly if any fitting, compiler, diagnostic or provider path is reached.
guarded <- c(".bh_compile_scalar", ".bh_fit_scalar", ".bh_backend_ir",
 ".bh_materialize_init", ".mp_toolchain_fingerprint", ".bh_provider_runtime_info",
 ".bh_provider_preflight", ".bh_provider_sample", "make_stan_code", "standata", "stancode",
 ".bh_ppc_ingest_csv",
 ".bh_individual_ingest_csv", ".bh_covariance_probe", ".bh_gaussian_identification")
guarded <- guarded[vapply(guarded,exists,logical(1),envir=ns,inherits=FALSE)]
old_guarded <- mget(guarded,ns);locks <- vapply(guarded,bindingIsLocked,logical(1),env=ns)
put <- function(n,value) {if(bindingIsLocked(n,ns))unlockBinding(n,ns);assign(n,value,ns);if(locks[[n]])lockBinding(n,ns)}
for(n in guarded)put(n,local({ label <- n;function(...)stop(paste("FORBIDDEN",label)) }))
on.exit(for(n in guarded)put(n,old_guarded[[n]]),add=TRUE)

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
  # This replaces exactly four initial .bh_checks calls; no scientific claims.
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

owned <- tempfile("archive-package-test-");dir.create(owned)
on.exit(unlink(owned,recursive=TRUE),add=TRUE)
d <- data.frame(person=as.character(1:4),x=c(.2,.8,.3,1))
spec <- with_checks(function()bmeas(bmf(A=~x),d,subject="person",fit=FALSE))
paths <- file.path(owned,paste0("original-",1:2,".csv"))
live <- cd_shell(spec,paths)
context <- list(max_context_bytes=20*1024^2,max_context_nodes=2000000,max_depth=64)
posterior <- list(reader="streaming",max_source_bytes=100000,max_read_bytes=700000,
 max_materialized_values=5000000,max_record_bytes=8192,max_columns=200,max_source_rows=20,
 max_chains=2,max_temp_bytes=200000,max_scan_values=10000)
ib <- list(posterior=posterior,max_parameter_cells=2000,max_summary_rows=6000,
 max_transformation_terms=1000000,max_declared_observation_rows=2000,
 max_consumed_observation_rows=2000,max_operand_context_rows=30000,max_context_terms=100000000)
db <- list(posterior=posterior,max_monitors=10,max_metric_rows=100,max_fft_length=64)
pb <- diagnostic_budget(4,20,100,posterior=posterior)
vars <- unique(c(.bh_ppc_forward_plan(spec,"population")$required_variables,
 .bh_ppc_forward_plan(spec,"conditional")$required_variables,
 .bh_individual_plan(live,c("predictor","natural","received"),ib,"all")$required_variables,
 "divergent__","treedepth__","energy__"))
for(i in 1:2) {
 x <- setNames(rep(0,length(vars)),vars)
 x[grepl("^obs_sd|^effective_loadings|^task_row_scale",vars)] <- 1
 x[vars=="treedepth__"] <- 2
 for(j in 1:4)for(base in c("L_component","L_parameter","L_task_work","D_parameter")) {
  key <- paste0(base,"[",j,",",j,"]");if(key%in%vars)x[key] <- 1
 }
 rows <- vapply(1:4,function(k) {y<-x;y[grepl("^eta|^alpha|^energy__",vars)] <- .1*k+i/100;paste(y,collapse=",")},"")
 writeLines(c("# ENGINEERING ONLY", "# stan_version_major = 2", "# stan_version_minor = 40", "# stan_version_patch = 0",
  "# model = scalar_components_model",paste0("# id = ",i),"# method = sample (Default)","# num_samples = 4",
  "# num_warmup = 0","# save_warmup = false (Default)","# thin = 1","# algorithm = hmc (Default)",
  "# engine = nuts (Default)","# metric = diag_e (Default)","# max_depth = 4","# sig_figs = 17",
  paste(gsub("\\]","",gsub("[\\[,]",".",vars)),collapse=","),rows,
  "# Elapsed Time: 0 seconds (Warm-up)","# 0 seconds (Sampling)","# 0 seconds (Total)"),paths[i])
}
fit <- portable_fit(live,context)
refuses <- function(action) { e<-tryCatch(action(),error=identity);stopifnot(inherits(e,"error")); invisible(e) }
set.seed(93);rng <- .Random.seed
a <- archive_fit(fit,file.path(owned,"archive"))
cat <- .bh_diagnostic_catalogue(live,list(max_rows=100,max_context_rows=10000))
key <- cat$key[cat$source_variable=="alpha[1]"][1L]
stopifnot(length(key)==1L,!is.na(key))
x <- refresh_diagnostics(fit,key,db)
y <- refresh_diagnostics(a,key,db)
stopifnot(identical(x$cache$parameter_summary,y$report_snapshot$portable$cache$parameter_summary),
 identical(x$cache$posterior_reporting$parameter_metrics,y$report_snapshot$portable$cache$posterior_reporting$parameter_metrics))
manifest <- file.path(a$root,"manifest.rds");saved <- readBin(manifest,"raw",n=file.info(manifest)$size)
m <- readRDS(manifest);m$created_utc <- paste0(m$created_utc," revised");saveRDS(m,manifest,version=3)
e <- refuses(function()ppc_plan(a,4,pb,"population"));stopifnot(grepl("manifest changed",conditionMessage(e)))
writeBin(saved,manifest)
stopifnot(identical(.Random.seed,rng))
cat("PASS selected-parameter diagnostic metric parity and changed-on-disk manifest refusal\n")
})()
