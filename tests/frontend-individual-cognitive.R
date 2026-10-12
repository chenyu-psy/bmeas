source(file.path("helpers", "native-guard.R"))
# Public cognitive declarations and invented retained eta values: engineering only.
# No posterior-source authentication, HMC, model execution or comparison claim.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(source_root)
if (source_mode) for (f in list.files(file.path(source_root,"R"),full.names=TRUE)) source(f) else library(bmeas)
private <- function(name) if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
.bh_individual_plan <- private(".bh_individual_plan")
.mp_hash <- private(".mp_hash")
m3 <- Sys.getenv("MEASUREMENTPKG_M3_RESOURCE"); sdm <- Sys.getenv("MEASUREMENTPKG_SDM_RESOURCE")
if (nzchar(m3) && nzchar(sdm) && file.exists(m3) && file.exists(sdm) &&
    requireNamespace("cmdstanr",quietly=TRUE) && identical(as.character(utils::packageVersion("cmdstanr")),"0.9.0")) {
# Actual public declarations, retained as labelled synthetic engineering states.
# Construction is R-only. No compiler, HMC, native likelihood or backend is run.
make_individual_map_fixtures <- function() {
  m3_file<-Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
  sdm_file<-Sys.getenv("MEASUREMENTPKG_SDM_RESOURCE")
  make_fit<-function(spec)structure(list(spec=spec,code="ENGINEERING STATE: saved source identity only; no model execution",
    backend=list(draws=function(...)stop("backend draws trap"),metadata=function(...)stop("backend metadata trap")),
    cache=list(computation=list(status="not_run",reason="R-only engineering state"),
      inference=list(status="not_assessed"),identification=list(status="not_assessed"),
      posterior_propriety=list(status="not_assessed"),posterior_normalizability=list(status="not_assessed")),
    provenance=list(model_fingerprint=spec$fingerprint,kind="synthetic_engineering_state_not_posterior_or_inference_evidence",
      output_files=c("unavailable-engineering-source-7.csv","unavailable-engineering-source-2.csv"))),class=c("bmeas_fit","bhmm_fit"))
  ids<-c("zeta","alpha","unobserved","beta");tasks<-c("late","early")
  cd<-expand.grid(person=ids[c(1,2,4)],task=tasks,stringsAsFactors=FALSE)
  cd$obs<-paste0("cs",seq_len(nrow(cd)));cd$counts<-I(matrix(0L,nrow(cd),10L))
  cd$counts[,1]<-2L;cd$options<-I(matrix(1L,nrow(cd),10L));cd$counts[1,1]<-NA_integer_
  cm<-m3_cs_model(m3_file)
  cb<-observe(cm,cd,"person","task",fields=list(counts="counts",options="options"),units="count",observation_id="obs",
    parameters=list(c="task",ra=list(fixed=100,scale="predictor"),rc=list(fixed=.3,scale="natural")))
  cs<-bmeas(bmf(residual=list(targets=~cs$a[c("late","early")]),person=list(distribution="sample_centered",ids=ids),
    augmentation=list(type="task_cholesky",targets=~cs$a[c("early","late")])),list(cs=cb),fit=FALSE,missing="observed_only")
  sd<-data.frame(person=ids[c(1,2,4)],task="angle",error=c(-.2,.1,.3),run=1:3,obs=paste0("s",1:3))
  sm<-sdm_reference_model(sdm_file)
  sb<-observe(sm,sd,"person","task",fields=list(error="error",run="run"),units="radian",observation_id="obs",
    parameters=list(log_c=list(fixed=-.2),log_kappa="task"))
  sdm<-bmeas(bmf(),list(sdm=sb),fit=FALSE)
  zd<-data.frame(person=ids[c(1,2,4)],x1=c(.1,.3,.2),x2=c(.3,.2,.7),x3=c(.2,.4,.3))
  zero<-bmeas(bmf(G=~x1+x2+x3,loadings=list(fixed=list(list(value=0,cells=list(G=~x3)),list(value=1,cells=list(G=~x1)))),
    residual=list(targets=~x1+x2),person=list(distribution="sample_centered",ids=ids),
    augmentation=list(type="task_cholesky",targets=~x2+x1)),zd,subject="person",fit=FALSE)
  fd<-expand.grid(person=c("p2","p1"),route=1:3,content=1:3,stringsAsFactors=FALSE)
  fd$task<-paste0("opaque_display_",fd$route,"_",fd$content);fd$obs<-paste0("full",seq_len(nrow(fd)))
  fd$counts<-I(matrix(0L,nrow(fd),10L));fd$options<-I(matrix(0L,nrow(fd),10L))
  active<-list(c(1:4,10),c(5:7,9:10),5:10)
  for(i in seq_len(nrow(fd))) {a<-active[[fd$route[i]]];fd$options[i,a]<-1L;fd$counts[i,a[1L]]<-2L}
  fd$counts[fd$route==3L&fd$content==3L,]<-NA_integer_
  fm<-m3_model(m3_file)
  fb<-observe(fm,fd,"person","task",fields=list(counts="counts",options="options",route="route",content="content"),
    units="count",observation_id="obs",parameter_bindings=m3_reference_bindings(fd,"obs"),
    parameters=list(c="task",ra=list(fixed=.4),rc=list(fixed=.6)))
  atasks<-m3_reference_domain()$task[m3_reference_domain()$parameter=="a"]
  full<-bmeas(bmf(residual=list(targets=~full$a[atasks])),list(full=fb),fit=FALSE,missing="observed_only")
  eight_data<-do.call(rbind,lapply(seq_len(8L),function(n) {
    value<-fd[fd$person=="p1",,drop=FALSE];value$person<-paste0("person",n)
    value$obs<-paste0("eight_",n,"_",seq_len(nrow(value)));value
  }))
  eight_block<-observe(fm,eight_data,"person","task",fields=list(counts="counts",options="options",route="route",content="content"),
    units="count",observation_id="obs",parameter_bindings=m3_reference_bindings(eight_data,"obs"),
    parameters=list(c="task",ra=list(fixed=.4),rc=list(fixed=.6)))
  eight<-bmeas(bmf(residual=list(targets=~full$a[atasks])),list(full=eight_block),fit=FALSE,missing="observed_only")
  gd<-data.frame(person=c("two","one"),task="only",y=c(1,2),obs=c("g1","g2"))
  fixed<-bmeas(bmf(),list(g=observe(gaussian_model(),gd,"person","task","y",units="score",observation_id="obs",
    parameters=list(mu=list(fixed=.25)))),fit=FALSE)
  # Observation-scale-only declaration exercises a truly empty predictor role.
  no_model<-new_observation_model("engineering_scale_only","v1",response_schema(y=data_field("real")),
    list(sigma=parameter_spec("positive","log","theta","task","score","scale only",role="observation_scale")),
    stan_density(symbol="engineering_scale_only_lpdf",arguments=c("y","sigma"),argument_types=c("real","real"),normalized=TRUE,approximation="engineering declaration only",builtin=TRUE,license="MIT"),model_capabilities(fit=TRUE))
  empty<-bmeas(bmf(),list(scale=observe(no_model,gd,"person","task","y",units="score",observation_id="obs")),fit=FALSE)
  out<-lapply(list(cs=cs,sdm=sdm,zero=zero,full_m3=full,full_m3_8=eight,fixed=fixed,empty=empty),make_fit)
  out$empty$provenance$output_files<-NULL
  out
}
fits <- make_individual_map_fixtures()
budget <- list(max_source_bytes = 10485760, max_posterior_rows = 100,
  max_draw_values = 50000, max_parameter_cells = 1000)
scales <- c("predictor", "natural", "received")
outdir <- tempfile("individual-cognitive-"); dir.create(outdir)
dir.create(file.path(outdir, "fixtures"), showWarnings = FALSE)
passed <- 0L
ok <- function(x, label) { if (!isTRUE(x)) stop(label); passed <<- passed + 1L; cat("PASS", label, "\n") }
write_states <- function(name, plan) {
  variables <- plan$required_variables
  headers <- gsub("\\]", "", gsub("[\\[,]", ".", variables))
  x <- matrix(rep(c(0, log(9)), length(variables)), 2L, length(variables))
  vapply(c(11L, 3L), function(id) {
    path <- file.path(outdir, "fixtures", paste0(name, "-", id, ".csv"))
    lines <- c("# ENGINEERING: invented joint eta states, not posterior-origin evidence",
      "# stan_version_major = 2", "# stan_version_minor = 35", "# stan_version_patch = 0",
      "# model = scalar_components_model", paste("# id =", id), "# method = sample",
      "# num_samples = 2", "# num_warmup = 0", "# save_warmup = 0", "# thin = 1",
      "# algorithm = hmc", "# engine = nuts", "# metric = diag_e", "# sig_figs = 18",
      paste(c("lp__", "accept_stat__", headers), collapse = ","),
      vapply(seq_len(2L), function(i) paste(c(-1, .9,
        format(x[i, ], digits = 17, scientific = TRUE, trim = TRUE)), collapse = ","), ""),
      "# Elapsed Time: 0 seconds (Warm-up)", "# 0 seconds (Sampling)", "# 0 seconds (Total)")
    writeLines(lines, path, useBytes = TRUE); path
  }, "")
}
results <- list()
set.seed(887); seed <- .Random.seed; cwd <- getwd()
for (name in names(fits)) {
  fit <- fits[[name]]
  # The full-M3 fixtures deliberately include an excluded logical target and
  # retain a visibly synthetic supplied-source identity. They are not promoted
  # through the package's identification/code-generation gate by this test.
  if (!name %in% c("empty", "full_m3", "full_m3_8")) {
    prior_table <- get_prior(fit$spec)
    missing_classes <- unique(prior_table$class[is.na(prior_table$prior)])
    if (length(missing_classes)) {
      prior_values <- list(Intercept = "normal(0,1)", parameter_sd = "half_normal(1)")
      stopifnot(all(missing_classes %in% names(prior_values)))
      explicit <- do.call(c, lapply(missing_classes, function(cl) set_prior(prior_values[[cl]], cl)))
      fit$spec <- bmeas(fit$spec$formula, data = fit$spec$models, subjects = "shared",
        missing = fit$spec$missing$response, prior = explicit, fit = FALSE)
      fit$provenance$model_fingerprint <- fit$spec$fingerprint
    }
    fit$code <- as.character(stancode(fit$spec))
  }
  plan <- .bh_individual_plan(fit, scales, budget)
  if (nrow(plan$cells)) fit$provenance$output_files <- write_states(name, plan)
  before <- serialize(fit, NULL)
  out <- individual_parameter_summary(fit, scales = scales, budget = budget)
  ok(nrow(out) == nrow(plan$cells) * 3L && identical(before, serialize(fit, NULL)) &&
       identical(seed, .Random.seed) && identical(cwd, getwd()), paste(name, "full public extraction preserves input/RNG/cwd"))
  if (nrow(out)) {
    ok(identical(attr(out, "source_receipt")$saved_source_code_hash, .mp_hash(fit$code)) &&
         identical(attr(out, "draw_keys"), data.frame(chain_id = c(3L,11L,3L,11L), saved_row = c(1L,1L,2L,2L))),
       paste(name, "source identity and actual all-row ordering retained"))
    ok(identical(attr(out, "context")$logical_parameters, plan$context$logical_parameters),
       paste(name, "logical role context exact"))
  } else ok(identical(attr(out,"metadata")$status,"not_applicable_no_predictor_parameters") &&
              identical(attr(out,"source_receipt")$status,"not_needed"),
            "no-predictor public result does not acquire unavailable paths")
  results[[name]] <- out
}
cs <- results$cs
ok(all(abs(cs$mean[cs$parameter %in% c("a","c") & cs$scale=="natural"] - 5) < 1e-13) &&
     all(abs(cs$mean[cs$parameter %in% c("a","c") & cs$scale=="received"] - log(3)) < 1e-13),
   "actual CS natural activation means use each state's exp while receiver remains eta")
ok(all(cs$status[cs$parameter=="ra" & cs$scale=="natural"]=="unavailable") &&
     all(cs$mean[cs$parameter=="ra" & cs$scale=="received"]==100),
   "CS saturated optional natural display leaves finite received100 intact")
ok(all(cs$effective_role[cs$parameter=="c"]=="shared_task_predictor") &&
     all(cs$sd[cs$parameter=="c"]>0), "actual CS task predictors retain shared posterior-state variation")
sdm <- results$sdm
ok(all(abs(sdm$mean[sdm$parameter=="log_kappa"]-log(3)) < 1e-13) &&
     all(sdm$mean[sdm$parameter=="log_c"]==-.2), "actual SDM identity scales are never inferred from log names")
ok(all(grepl("not qualified",sdm$coverage_qualification)), "SDM informative flags retain qualification")
full <- results$full_m3
ok(length(unique(full$parameter_key))==27L && nrow(full)==162L, "27 logical full-M3 targets remain within scalar work bounds")
ok(all(full$n_likelihood_rows[full$parameter=="c"&full$task=="MU_ori_color"]==2L) &&
     all(attr(full,"context")$route_literals$received_value==100) && !any(full$parameter=="ra_fixed"),
   "full M3 logical aliases and intrinsic MU-ra constants remain distinct")
zero <- results$zero
ok(all(zero$effective_role[zero$task=="x3"]=="shared_mean_only_predictor") &&
     all(zero$sd[zero$task=="x3"]>0), "zero latent support does not erase shared eta uncertainty")
saveRDS(results,file.path(outdir,"cognitive-results.rds"))
for (name in names(results)) {
  value <- results[[name]]; bytes <- serialize(value,NULL)
  text <- capture.output(returned <- print(value))
  ok(identical(value,returned)&&identical(bytes,serialize(value,NULL))&&identical(seed,.Random.seed),
     paste(name,"saved result printing preserves exact value and RNG"))
}
unlink(outdir,recursive=TRUE)
cat("frontend-individual-cognitive:",passed,"checks passed\n")

} else cat("SKIP: individual cognitive controls require both pinned resources and CmdStanR 0.9.0\n")
