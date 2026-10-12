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

fit <- portable_fit(live,context)

fit$cache$parameter_summary <- data.frame(variable="fixture",mean=1,sd=.5)
handle <- archive_fit(fit,file.path(owned,"archive"),budget)
stopifnot(identical(handle$report_snapshot$portable,fit))
old_root <- handle$root
moved <- file.path(owned,"relocated")
stopifnot(file.rename(old_root,moved))
unlink(paths)
handle <- read_fit_archive(moved,budget)
stopifnot(identical(handle$report_snapshot$portable,fit))
# Every archive file can disappear after explicit opening; reporting still works.
unlink(moved,recursive=TRUE)
expected_summary <- summary(fit)
expected_checks <- fit_checks(fit)
expected_parameters <- posterior_summary(fit)
identity_before <- serialize(handle$report_snapshot$portable$immutable,NULL)
manifest_before <- handle$manifest_md5
refuses <- function(action)stopifnot(inherits(tryCatch(action(),error=identity),"error"))
legacy <- handle[1:3];class(legacy)<-"bmeas_fit_archive"
err <- tryCatch(summary(legacy),error=identity)
stopifnot(inherits(err,"error"),grepl("explicitly reopen",conditionMessage(err)))
for(change in list(function(h){h$extra<-1;h},function(h){class(h)<-c("fake","bmeas_fit_archive");h},
 function(h){h$report_snapshot$manifest_md5<-paste(rep("0",32),collapse="");h},
 function(h){h$report_snapshot$portable_identity$digest<-paste(rep("0",32),collapse="");h},
 function(h){h$report_snapshot$portable<-new.env();h},function(h){h$budget<-NULL;h}))
 refuses(function()summary(change(handle)))
# Direct source/acquisition/identity and numerical paths must remain unreachable.
more <- c(".bh_archive_read",".bh_archive_md5",".bh_archive_path",".bh_archive_layout",
 ".bh_pf_active",".bh_pf_owned_hash",".bh_portable_codec_count",".bh_posterior_ingest_csv",".bh_component_execute",
 "refresh_diagnostics","assess_measurement_fit","simulate","readRDS","readBin","readLines",
 "file","file.exists","file.info","list.files","normalizePath")
more <- more[!more %in% guarded]
old_more <- lapply(more,function(n)if(exists(n,ns,inherits=FALSE))get(n,ns)else NULL);names(old_more)<-more
more_locks <- vapply(more,function(n)exists(n,ns,inherits=FALSE)&&bindingIsLocked(n,ns),FALSE)
# Package namespaces are locked: trace base I/O instead of adding bindings there.
local_names <- more[vapply(more,exists,FALSE,envir=ns,inherits=FALSE)]
for(n in local_names){if(bindingIsLocked(n,ns))unlockBinding(n,ns);assign(n,function(...)stop("FORBIDDEN passive dependency"),ns);if(more_locks[[n]])lockBinding(n,ns)}
on.exit(for(n in local_names){if(bindingIsLocked(n,ns))unlockBinding(n,ns);assign(n,old_more[[n]],ns);if(more_locks[[n]])lockBinding(n,ns)},add=TRUE)
trace("md5sum",tracer=quote(stop("FORBIDDEN report hashing")),print=FALSE,where=asNamespace("tools"))
on.exit(untrace("md5sum",where=asNamespace("tools")),add=TRUE)
io <- c("readRDS","readBin","readLines","file","file.exists","file.info","list.files","normalizePath")
for(n in io)trace(n,tracer=quote(stop("FORBIDDEN report file I/O")),print=FALSE,where=baseenv())
on.exit(for(n in io)untrace(n,where=baseenv()),add=TRUE)
actual_summary <- summary(handle);actual_checks<-fit_checks(handle)
stopifnot(identical(actual_summary$archive_context$status,"retained_snapshot_not_reverified"),
 identical(actual_checks$archive_context$status,"retained_snapshot_not_reverified"))
actual_summary$archive_context<-NULL;class(actual_summary)<-class(expected_summary)
actual_checks$archive_context<-NULL;class(actual_checks)<-class(expected_checks)
stopifnot(identical(actual_summary,expected_summary),identical(actual_checks,expected_checks))
for(fn in list(parameter_summary,posterior_summary)) {
 tab<-fn(handle);stopifnot(identical(attr(tab,"archive_context")$status,"retained_snapshot_not_reverified"))
 attr(tab,"archive_context")<-NULL;stopifnot(identical(tab,expected_parameters))
}
text <- capture.output(print(handle))
stopifnot(any(grepl("retained snapshot",text)),any(grepl("not reverified",text)))
capture.output(print(fit_checks(handle)))
missing <- handle;missing$report_snapshot$portable$cache$parameter_summary<-NULL
stopifnot(is.null(parameter_summary(missing)),is.null(posterior_summary(missing)))
# Protected adverse history survives unchanged alongside any mutable cache edits.
adverse <- handle
adverse$report_snapshot$portable$immutable$protected_evidence$original_fit_history$posterior_propriety <- list(status="proven_improper",reason="synthetic retained adverse evidence")
adverse$report_snapshot$portable$cache$posterior_propriety <- list(status="not_assessed")
reported<-fit_checks(adverse)
stopifnot(identical(reported$portable_context$protected_original_evidence$original_fit_history$posterior_propriety$status,"proven_improper"))
stopifnot(identical(serialize(handle$report_snapshot$portable$immutable,NULL),identity_before),identical(handle$manifest_md5,manifest_before))
})()
