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
baseline_d <- refresh_diagnostics(fit,character(),db)
baseline_i <- individual_parameter_summary(fit,c("predictor","natural","received"),ib)
baseline_p <- assess_measurement_fit(fit,level="population",draws=4,seed=42,budget=pb)
baseline_c <- assess_measurement_fit(fit,level="conditional",draws=4,seed=42,budget=pb)
a <- archive_fit(fit,file.path(owned,"archive"))
stopifnot(file.rename(a$root,file.path(owned,"relocated"))); a$root <- file.path(owned,"relocated")
unlink(paths)
files <- list.files(a$root,recursive=TRUE,full.names=TRUE);hashes <- tools::md5sum(files)
before <- serialize(a,NULL)
ad <- refresh_diagnostics(a,character(),db)
ai <- individual_parameter_summary(a,c("predictor","natural","received"),ib)
ap <- assess_measurement_fit(ad,level="population",draws=4,seed=42,budget=pb)
ac <- assess_measurement_fit(a,level="conditional",draws=4,seed=42,budget=pb)
stopifnot(identical(baseline_d$cache$hmc,ad$report_snapshot$portable$cache$hmc),
 identical(baseline_d$cache$parameter_summary,ad$report_snapshot$portable$cache$parameter_summary),
 all(vapply(names(ai),function(n)identical(baseline_i[[n]],ai[[n]]),FALSE)),
 identical(baseline_p$cache$ppc$table,ap$report_snapshot$portable$cache$ppc$table),
 identical(baseline_p$cache$ppc$replicate_statistics,ap$report_snapshot$portable$cache$ppc$replicate_statistics),
 identical(baseline_c$cache$ppc$table,ac$report_snapshot$portable$cache$ppc$table),
 identical(.Random.seed,rng),identical(serialize(a,NULL),before),identical(tools::md5sum(files),hashes),
 identical(fit$immutable,ap$report_snapshot$portable$immutable),identical(fit$identity,ap$report_snapshot$portable$identity),
 identical(ad$report_snapshot$portable$cache$hmc,ap$report_snapshot$portable$cache$hmc))
stopifnot(identical(attr(ai,"source_receipt")$archive_integrity$status,"matched_owned_snapshot"))
stopifnot(identical(ppc_plan(a,4,pb,"population")$source$archive_context$status,"reopened_and_verified"))
cat("PASS relocated all-consumer parity, fixed-seed population and conditional PPC, RNG, immutable identities, cache-only updates\n")
# Cold reopen uses the real tiny CSV consumer, without producer metadata doubles.
transport <- file.path(owned,"cold.rds"); child <- file.path(owned,"cold.R")
saveRDS(list(root=a$root,budget=ib,values=lapply(ai,identity),libs=.libPaths(),
 guard=normalizePath(file.path("helpers","native-guard.R"))),transport)
writeLines(c(
 'z <- readRDS(commandArgs(TRUE)[1]); .libPaths(z$libs)',
 'source(z$guard); library(bmeas)',
 'ns <- asNamespace("bmeas")',
 'for(n in c(".bh_compile_scalar",".bh_fit_scalar",".bh_provider_sample")) {',
 ' unlockBinding(n,ns); assign(n,function(...)stop("FORBIDDEN cold fitting"),ns); lockBinding(n,ns)',
 '}',
 'a <- read_fit_archive(z$root)',
 'out <- individual_parameter_summary(a,c("predictor","natural","received"),z$budget)',
 'stopifnot(identical(names(out),names(z$values)),',
 ' all(vapply(names(out),function(n)identical(out[[n]],z$values[[n]]),FALSE)),',
 ' is.list(summary(a)),is.list(fit_checks(a)))',
 'cat("PASS cold archive reopen, real two-chain CSV consumer and passive reports\\n")'
),child)
status <- system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(child),shQuote(transport)))
stopifnot(identical(status,0L))
# Tampered snapshot immutable fields cannot become active authority.
fake <- a;fake$report_snapshot$portable$immutable$retained_artifacts$saved_source_text <- "FAKE"
stopifnot(identical(individual_parameter_summary(fake,"predictor",ib),individual_parameter_summary(a,"predictor",ib)))
for(fun in list(function(z)refresh_diagnostics(z,character(),db),function(z)individual_parameter_summary(z,"predictor",ib),
 function(z)assess_measurement_fit(z,level="population",draws=4,seed=42,budget=pb),function(z)ppc_plan(z,4,pb,"population"))) {
 bad <- a;bad$manifest_md5 <- paste(rep("0",32),collapse="");refuses(function()fun(bad))
}
small <- ib;small$posterior$max_source_bytes <- 1;refuses(function()individual_parameter_summary(a,"predictor",small))
small <- db;small$posterior$max_source_bytes <- 1;refuses(function()refresh_diagnostics(a,character(),small))
small <- pb;small$posterior$max_source_bytes <- 1
failed <- assess_measurement_fit(a,level="population",draws=4,seed=42,budget=small)
stopifnot(identical(failed$report_snapshot$portable$cache$predictive_fit$status,"failed"),
 !is.null(failed$report_snapshot$portable$cache$ppc$metadata$failure$code),identical(.Random.seed,rng))
refuses(function()individual_parameter_summary(a,"predictor",list(max_source_bytes=1000)))
refuses(function()ppc_plan(a,4,diagnostic_budget(4,20,100),"population"))
# Classed handles/budgets and protected adverse history cannot escape guards.
hits <- 0L
`$.hostile` <- function(...) { hits <<- hits+1L;stop("callback") }
assign("$.hostile",`$.hostile`,.GlobalEnv)
bad <- a;class(bad) <- c("hostile","bmeas_fit_archive","bhmm_fit")
refuses(function()individual_parameter_summary(bad,"predictor",ib))
badbudget <- ib;class(badbudget) <- "hostile"
refuses(function()individual_parameter_summary(a,"predictor",badbudget))
badbudget <- pb;class(badbudget) <- c("hostile","measurement_diagnostic_budget")
stopifnot(inherits(ppc_plan(a,4,badbudget,"population"),"bmeas_ppc_plan"))
stopifnot(hits==0L);rm("$.hostile",envir=.GlobalEnv)
badlive <- live;badlive$cache$posterior_propriety$status <- "proven_improper"
# Restore originals only long enough to create a protected-adverse capsule archive.
for(i in 1:2)file.copy(file.path(a$root,sprintf("draws/%06d.csv",i)),paths[i])
badfit <- portable_fit(badlive,context);bad <- archive_fit(badfit,file.path(owned,"adverse"));unlink(paths)
refuses(function()refresh_diagnostics(bad,character(),db))
refuses(function()individual_parameter_summary(bad,"predictor",ib))
refuses(function()ppc_plan(bad,4,pb,"population"))
refuses(function()assess_measurement_fit(bad,level="population",draws=4,seed=42,budget=pb))
# Mutation after reopen must still fail the streaming reader's separate pin.
reader <- .bh_posterior_ingest_csv;original_read <- .bh_archive_read
replace(".bh_archive_read",function(...) { out<-original_read(...);cat("x",file=file.path(out$handle$root,"draws/000001.csv"),append=TRUE);out })
csv <- file.path(a$root,"draws/000001.csv");bytes <- readBin(csv,"raw",n=file.info(csv)$size)
refuses(function()individual_parameter_summary(a,"predictor",ib));writeBin(bytes,csv)
refuses(function()refresh_diagnostics(a,character(),db));writeBin(bytes,csv)
failed <- assess_measurement_fit(a,level="population",draws=4,seed=42,budget=pb)
stopifnot(identical(failed$report_snapshot$portable$cache$predictive_fit$status,"failed"))
replace(".bh_archive_read",original_read)
refuses(function()refresh_diagnostics(a,character(),db))
# Passive reports remain available from the retained snapshot despite corruption.
stopifnot(is.list(summary(ap)),is.list(fit_checks(ap)))
cat("PASS snapshot authority isolation, manifest pinning, budget refusal, legacy refusal, late mutation/corruption refusal and passive reports\n")
})()
