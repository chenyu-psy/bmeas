source(file.path("helpers", "native-guard.R"))
# Deterministic engineering evidence; no sampling, compilation, or diagnostics.
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root)) for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE)) source(f) else library(bmeas)
(function() {
ns <- if(nzchar(root)) .GlobalEnv else asNamespace("bmeas")
# Fail loudly if any fitting, compiler, diagnostic or provider path is reached.
guarded <- c(".bh_compile_scalar", ".bh_fit_scalar", ".bh_backend_ir",
 ".bh_materialize_init", ".mp_toolchain_fingerprint", ".bh_provider_runtime_info",
 ".bh_provider_preflight", ".bh_provider_sample", "make_stan_code", "standata", "stancode",
 ".bh_diagnostic_parameter_metrics", ".bh_diagnostic_hmc_metrics", ".bh_ppc_ingest_csv",
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
files <- file.path("fixtures-archive",paste0("mixed17-chain-",c(3,7),".csv"))
stopifnot(all(file.exists(files)))
file.copy(files,owned)
paths <- normalizePath(file.path(owned,basename(files)))
d <- data.frame(person=as.character(1:4),x1=c(.2,.8,.3,1),x2=c(.7,.6,-.3,.1),
 x3=c(-.2,.6,.9,.4),x4=c(.1,-.6,.3,.5))
spec <- with_checks(function()bmeas(bmf(A=~x1,B=~x2,C=~x3,D=~x4,
 cor=list(default="zero",fixed=list(list(pairs=~A:D,value=.25)),equal=list(shared=list(~A:B,~C:D))),
 scale=list(C=list(marker=~x3,value=2),D=list(marker=~x4,value=-3))),d,subject="person",fit=FALSE))
context <- list(max_context_bytes=20*1024^2,max_context_nodes=2000000,max_depth=64)
posterior <- list(reader="streaming",max_source_bytes=32768,max_read_bytes=7*32768,
 max_materialized_values=400000,max_record_bytes=8192,max_columns=200,max_source_rows=20,
 max_chains=2,max_temp_bytes=2*32768,max_scan_values=1000)
budget <- list(max_members=16,max_member_bytes=2^22,max_total_bytes=2^24,max_context_bytes=2^22,
 max_read_bytes=2^28,max_temp_bytes=2^27,posterior=posterior,context=context)
component <- list(posterior=posterior,max_components=4,max_output_values=872,max_matrix_work=2112,context=context)
live <- cd_shell(spec,paths)
stopifnot(identical(component_draws(live),component_draws(live,postprocess_budget("component_draws"))))
fit <- portable_fit(live,context)
# The default path and its explicit spelling have exactly the same receipts.
refuses <- function(action)stopifnot(inherits(tryCatch(action(),error=identity),"error"))
defaults <- postprocess_budget("component_draws")
x <- component_draws(fit)
y <- component_draws(fit, defaults)
stopifnot(identical(x,y))
custom <- defaults;custom$max_components <- 4
z <- component_draws(fit,custom)
for(n in setdiff(names(x),c("source","context_receipt"))) stopifnot(identical(x[[n]],z[[n]]))
custom$max_components <- 3;refuses(function()component_draws(fit,custom))
for(field in c("max_output_values","max_matrix_work")) {
 custom <- defaults;custom[[field]] <- x$work[[sub("^max_","",field)]]
 z <- component_draws(fit,custom)
for(n in setdiff(names(x),c("source","context_receipt"))) stopifnot(identical(x[[n]],z[[n]]))
 custom[[field]] <- custom[[field]]-1
 refuses(function()component_draws(fit,custom))
}
a <- archive_fit(fit,file.path(owned,"default-archive"))
b <- archive_fit(fit,file.path(owned,"explicit-archive"),postprocess_budget("archive_fit"))
stopifnot(identical(a$budget,b$budget),
 identical(read_fit_archive(a$root),read_fit_archive(a$root,postprocess_budget("read_fit_archive"))))
for(member in setdiff(list.files(a$root,recursive=TRUE),"manifest.rds"))
 stopifnot(identical(readBin(file.path(a$root,member),"raw",n=file.info(file.path(a$root,member))$size),
                     readBin(file.path(b$root,member),"raw",n=file.info(file.path(b$root,member))$size)))
ma <- readRDS(file.path(a$root,"manifest.rds"));mb <- readRDS(file.path(b$root,"manifest.rds"))
ma$created_utc <- mb$created_utc <- NULL
stopifnot(identical(ma,mb))
stopifnot(identical(component_draws(a),component_draws(a,defaults)))
# Custom archive allowances are retained literally after canonical field ordering.
custom_archive <- postprocess_budget("archive_fit");custom_archive$max_members <- 4
c <- archive_fit(fit,file.path(owned,"custom-archive"),custom_archive)
stopifnot(identical(c$budget,custom_archive))
custom_archive$max_members <- 3
refuses(function()archive_fit(fit,file.path(owned,"refused-archive"),custom_archive))
refuses(function()read_fit_archive(a$root,custom_archive))
# A late reader refusal occurs after owned staging begins; staging must disappear.
clean_before <- list.files(owned,all.files=TRUE)
late <- postprocess_budget("archive_fit");late$posterior$max_source_bytes <- 1
refuses(function()archive_fit(fit,file.path(owned,"late-refused"),late))
stopifnot(identical(clean_before,list.files(owned,all.files=TRUE)))
for(operation in c("archive_fit","read_fit_archive","component_draws")) {
 fn <- get(operation)
 for(invalid in list(NULL,list())) {
  if(operation=="archive_fit") refuses(function()fn(fit,file.path(owned,"bad"),invalid))
  else if(operation=="read_fit_archive") refuses(function()fn(a$root,invalid))
  else refuses(function()fn(fit,invalid))
 }
}
cat("PASS omitted/explicit default equality, byte-identical members and manifests except creation time, custom and exact boundary budgets, late cleanup\n")

before <- component_draws(fit,component)
archive <- archive_fit(fit,file.path(owned,"archive"),rev(budget))
stopifnot(identical(readRDS(file.path(archive$root,"portable.rds")),fit))
unlink(paths)
after <- component_draws(read_fit_archive(archive$root,budget),component)
for(n in setdiff(names(before),c("source","context_receipt","work")))stopifnot(identical(before[[n]],after[[n]]))
stopifnot(identical(after$source$archive_integrity$status,"matched_owned_snapshot"),
 identical(after$source$historical_identity,"read-time receipt; original fit-time hashes absent"))
cat("PASS archive full scientific roundtrip after original CSV removal and reordered budget keys\n")
refuses <- function(action)stopifnot(inherits(tryCatch(action(),error=identity),"error"))
b <- budget;b$max_temp_bytes <- 1;refuses(function()read_fit_archive(archive$root,b))
b <- budget;b$max_members <- 3;refuses(function()read_fit_archive(archive$root,b))
refuses(function()archive_fit(fit,archive$root,budget))
cat("x",file=file.path(archive$root,"draws/000001.csv"),append=TRUE)
refuses(function()read_fit_archive(archive$root,budget))
refuses(function()component_draws(archive,component))
cat("PASS archive quotas, existing destination, and changed bytes refuse\n")
})()
