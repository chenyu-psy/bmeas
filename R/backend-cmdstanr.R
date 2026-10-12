.mp_threaded_flag <- function(spec, threaded) {
  if(!is.logical(threaded)||length(threaded)!=1L||is.na(threaded)) .mp_abort("E_RUN_CONFIG","threaded must be a scalar logical")
  if(threaded&&! .mp_registry_mode(spec)) .mp_abort("E_THREADING_UNSUPPORTED","threading is qualified only for the generic registry/component compiler; optimized basic templates remain serial")
  if(threaded) {
    exports<-unlist(lapply(spec$models,function(b)c(b$model$adapter$function_exports,unlist(lapply(b$model$adapter$resources,function(r)r$exports)))),use.names=FALSE)
    if(any(grepl("^mp_",exports))) .mp_abort("E_SYMBOL_COLLISION","mp_ function prefix is reserved for generated reduction helpers")
  }
  invisible(threaded)
}
.mp_reduction <- function(reduction, grainsize, development=TRUE) {
  if(!is.character(reduction)||length(reduction)!=1L||is.na(reduction)||!reduction%in%c("serial","reduce_sum_static")) .mp_abort("E_THREADING_UNSUPPORTED","reduction must be serial or reduce_sum_static")
  .mp_integer(grainsize,"grainsize")
  if(development&&grainsize>1000L) .mp_abort("E_RUN_BUDGET","the legacy development grainsize is capped at1000")
  invisible(TRUE)
}
run_config <- function(chains, parallel_chains, threads_per_chain,
                       iter_warmup, iter_sampling, seed, cpu_budget,
                       max_observations, max_subjects, max_saved_draws,
                       reduction = "serial", grainsize = 1L, max_treedepth=10L, model_budget=NULL) {
  values <- list(chains = chains, parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain, iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, seed = seed, cpu_budget = cpu_budget,
    max_observations = max_observations, max_subjects = max_subjects,
    max_saved_draws = max_saved_draws, grainsize = grainsize, max_treedepth=max_treedepth)
  for (n in names(values)) values[[n]] <- .mp_integer(values[[n]], n)
  if (chains > 2L || iter_warmup > 50L || iter_sampling > 50L || max_subjects > 30L || max_observations > 1000L)
    .mp_abort("E_SMOKE_BUDGET", "P0 only allows tiny engineering runs: <=2 chains, <=50+50 iterations, <=30 people, <=1000 observations")
  if (parallel_chains > chains || .bh_count_product(as.double(parallel_chains),as.double(threads_per_chain),name="legacy simultaneous workers") > cpu_budget ||
      .bh_count_product(as.double(chains),as.double(iter_sampling),name="legacy saved states") > max_saved_draws)
    .mp_abort("E_RUN_BUDGET", "parallel resource or saved-draw budget exceeded")
  .mp_reduction(reduction,grainsize)
  if (threads_per_chain>2L||cpu_budget>2L) .mp_abort("E_RUN_BUDGET","this prototype caps total CPU budget and threads per chain at2")
  if (reduction=="serial" && threads_per_chain!=1L)
    .mp_abort("E_THREADING_UNSUPPORTED", "serial execution requires threads_per_chain=1; threads are never ignored")
  if(max_treedepth>10L) .mp_abort("E_RUN_BUDGET","prototype max_treedepth ceiling is10")
  if(!is.null(model_budget)) {
    if(!inherits(model_budget,"measurement_model_budget")||!identical(model_budget,reference_run_budget(model_budget$name))) .mp_abort("E_RUN_BUDGET","invalid declared reference budget")
    if(chains!=1L||iter_warmup>10L||iter_sampling>10L||max_treedepth>6L) .mp_abort("E_RUN_BUDGET","reference smoke limited to1 chain10+10 and depth6")
  }
  values["model_budget"]<-list(model_budget)
  values$reduction <- reduction; values$kind <- "smoke"
  values$wall_time_enforcement <- "not implemented; iteration and data budgets are enforced"
  values$memory_enforcement <- "not implemented; data and saved-draw counts are bounded"
  structure(values, class = "measurement_run_config")
}
execution_plan <- function(spec, run=NULL, chains=NULL, iter=NULL, warmup=NULL,
                           cores=1L, threads=1L, seed=1L, grainsize=1L, control=list(),
                           resources=NULL, save_log_lik=FALSE, exploratory=FALSE, execution="qualified") {
  .bh_pf_refuse_execution(spec,"execution_plan")
  supplied<-names(as.list(match.call(expand.dots=FALSE)))[-1L]
  if(inherits(spec,"bhmm_spec")) {
    if(!is.null(run)).mp_abort("E_INPUT_AMBIGUITY","a main prepared specification uses chains/iter/warmup and resources directly; do not combine it with a legacy run_config")
    return(.bh_run_plan(spec,chains,iter,warmup,cores,threads,seed,grainsize,control,resources,save_log_lik,exploratory,execution))
  }
  if(any(!supplied%in%c("spec","run")))
    .mp_abort("E_INPUT_AMBIGUITY","legacy measurement specifications require their explicit run_config only; main-API run controls belong to a prepared bmeas specification")
  .mp_execution_plan_legacy(spec,run)
}
.mp_execution_plan_legacy <- function(spec, run) {
  if (!inherits(run, "measurement_run_config")) .mp_abort("E_RUN_CONFIG", "explicit run_config is required")
  do.call(run_config, run[c("chains", "parallel_chains", "threads_per_chain", "iter_warmup",
    "iter_sampling", "seed", "cpu_budget", "max_observations", "max_subjects",
    "max_saved_draws", "reduction", "grainsize", "max_treedepth", "model_budget")])
  .mp_threaded_flag(spec,run$reduction!="serial")
  d <- stan_data(spec,reduction=run$reduction,grainsize=run$grainsize,threaded=run$reduction!="serial")
  allowed_J<-12L
  if(!is.null(run$model_budget)) {
    b<-run$model_budget
    if(!identical(spec$structure$reference_name,b$name)||d$J!=b$max_parameters||d$K!=b$max_components||d$N>b$max_subjects||d$O>b$max_observations)
      .mp_abort("E_RUN_BUDGET","reference budget does not match its exact small blueprint/data scope")
    allowed_J<-b$max_parameters
  }
  if (d$N > run$max_subjects || d$O > run$max_observations || d$J > allowed_J)
    .mp_abort("E_RUN_BUDGET", "dataset exceeds declared tiny-run budget (P0 also limits indicators to 12)")
  # Conservative count of stored real quantities, before CSV formatting overhead.
  real_count <- if(.mp_registry_mode(spec))structure_ir(spec)$estimated_output_reals+12 else d$K*d$N*2 + d$J*4 + d$K^2*2 + d$J*d$K + 2*d$J^2 + 12
  if(isTRUE(spec$verification_outputs$log_lik))real_count<-real_count+d$O
  estimated_csv_bytes<-32*real_count*run$chains*run$iter_sampling
  if(!is.null(run$model_budget)&&estimated_csv_bytes>run$model_budget$max_output_bytes) .mp_abort("E_OUTPUT_BUDGET","declared output estimate exceeds reference budget")
  list(kind = "SMOKE ONLY", N = d$N, observations = d$O, indicators = d$J,
    factors = d$K, chains = run$chains, parallel_chains = run$parallel_chains,
    threads_per_chain = run$threads_per_chain, reduction = run$reduction, grainsize=run$grainsize,
    stan_threads=run$reduction!="serial", compiler_path=if(.mp_registry_mode(spec))"generic"else"optimized_basic",
    saved_draws = run$chains * run$iter_sampling,
    approximate_draw_bytes = 8 * real_count * run$chains * run$iter_sampling, estimated_csv_bytes=estimated_csv_bytes, max_treedepth=run$max_treedepth,
    estimate_limitations = "Draw estimate excludes CSV formatting, R overhead, compilation memory and binary files",
    wall_time_enforcement = run$wall_time_enforcement, memory_enforcement = run$memory_enforcement)
}
# Explicitly disable stanc inlining: installed stanc2.40 O1 was observed to
# mishandle an early return inside the pinned SDM binary-search while loop.
# This flag is part of every compilation identity; no research source is patched.
.mp_stanc_options <- function() list(O0=TRUE)
.mp_toolchain_fingerprint <- function(cpp_options=list(stan_threads=FALSE),stanc_options=.mp_stanc_options()) {
  make_local <- file.path(cmdstanr::cmdstan_path(), "make", "local")
  compiler <- Sys.getenv("CXX", unset = "c++")
  compiler_version <- tryCatch(system2(compiler, "--version", stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  list(cmdstan = as.character(cmdstanr::cmdstan_version()), cmdstan_path = cmdstanr::cmdstan_path(),
    cmdstanr = as.character(utils::packageVersion("cmdstanr")), R = R.version.string,
    platform = R.version$platform, compiler = compiler, compiler_version = compiler_version,
    environment = Sys.getenv(c("CC", "CXX", "CPPFLAGS", "CXXFLAGS", "LDFLAGS")),
    make_local = if (file.exists(make_local)) readLines(make_local, warn = FALSE) else character(),
    cpp_options = cpp_options, stanc_options = stanc_options)
}
compile_measurement <- function(spec, cache_dir, threaded=FALSE) {
  check_measurement(spec); .mp_threaded_flag(spec,threaded)
  if (!requireNamespace("cmdstanr", quietly = TRUE)) .mp_abort("E_BACKEND_MISSING", "install/configure CmdStanR and CmdStan separately")
  .mp_scalar_string(cache_dir, "cache_dir")
  code <- make_stan_code(spec,threaded=threaded)
  cpp_options<-if(identical(spec$structure$type,"component_covariance"))list(stan_threads=threaded,O=0)else list(stan_threads=threaded)
  stanc_options<-.mp_stanc_options()
  fingerprint <- .mp_toolchain_fingerprint(cpp_options,stanc_options)
  identity <- list(code = as.character(code), template_version = spec$version,
    execution_support=list(stan_threads=threaded,reduction=if(threaded)"serial_or_reduce_sum_static"else"serial",grainsize="runtime_data"),
    resources = lapply(spec$models, function(b) b$model[c("name", "version", "provenance", "likelihood", "adapter", "constants")]),
    scientific_spec=list(structure=spec$structure,priors=spec$priors,bindings=lapply(spec$models,function(b)b$parameter_bindings),source_metadata=spec$source_metadata),toolchain = fingerprint)
  key <- .mp_hash(identity); directory <- file.path(cache_dir, key)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) .mp_abort("E_OUTPUT", "could not create compilation cache directory")
  stan_file <- file.path(directory, if(.mp_registry_mode(spec)) "registry_cfa.stan" else if (.mp_legacy_gaussian(spec)) "gaussian_cfa.stan" else "basic_cfa.stan")
  source_changed <- .mp_sync_stan_source(as.character(code), stan_file)
  for(resource in .mp_adapter_resources(spec)) {
    destination<-file.path(directory,resource$cache_name)
    if(!file.exists(destination)||.mp_sha256(destination)!=resource$sha256) {
      if(!file.copy(resource$file,destination,overwrite=TRUE)||.mp_sha256(destination)!=resource$sha256)
        .mp_abort("E_RESOURCE_HASH","failed to materialize exact external function resource")
      source_changed<-TRUE
    }
  }
  manifest_file <- file.path(directory, "compile-manifest.rds")
  # A manifest mismatch never authorizes reuse of an old executable.
  force <- source_changed || !file.exists(manifest_file) || !identical(readRDS(manifest_file), identity)
  model <- cmdstanr::cmdstan_model(stan_file, cpp_options = cpp_options, stanc_options=stanc_options, force_recompile = force)
  saveRDS(identity, manifest_file)
  list(model = model, cache_key = key, code = as.character(code), toolchain = fingerprint,
       stan_file = stan_file, threaded=threaded, symbol_map = stan_symbol_map(spec))
}
fit_measurement <- function(spec, run, cache_dir, output_dir) {
  plan <- execution_plan(spec, run)
  .mp_scalar_string(output_dir, "output_dir")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) .mp_abort("E_OUTPUT", "could not create run output directory")
  compiled <- compile_measurement(spec, cache_dir,threaded=run$reduction!="serial")
  d <- stan_data(spec,reduction=run$reduction,grainsize=run$grainsize,threaded=run$reduction!="serial")
  if(.mp_registry_mode(spec)) init<-structure_ir(spec)$init else {
    init <- list(alpha = rep_len(d$alpha_mean, d$J), loading_anchor = rep(0.5, d$K),loading_free = rep(0.5, d$J-d$K),L_Omega = diag(d$K), z_raw = matrix(0, d$K, d$N))
    if (.mp_legacy_gaussian(spec)) init$sigma <- rep(1,d$J) else init$obs_sd <- rep(1,d$S)
  }
  started <- Sys.time()
  backend <- compiled$model$sample(data = d, chains = run$chains,
    parallel_chains = run$parallel_chains, threads_per_chain=if(compiled$threaded)run$threads_per_chain else NULL,
    iter_warmup = run$iter_warmup, iter_sampling = run$iter_sampling,
    seed = run$seed, init = function() init, output_dir = output_dir,
    refresh = 0, max_treedepth = run$max_treedepth, adapt_delta = 0.8,
    save_warmup = FALSE, sig_figs = 12)
  if (any(backend$return_codes() != 0L)) .mp_abort("E_SAMPLING_FAILED", "CmdStan returned a nonzero status; see saved run output")
  if(!is.null(run$model_budget)&&sum(file.info(backend$output_files())$size)>run$model_budget$max_output_bytes) .mp_abort("E_OUTPUT_BUDGET","actual sampler output exceeded declared budget")
  cache <- .mp_cache_fit(backend, spec, run)
  structure(list(spec = spec, run = run, execution_plan = plan, backend = backend,
    cache = cache, code = compiled$code, symbol_map = compiled$symbol_map,
    provenance = list(data_hash = spec$provenance$data_hash, compile_key = compiled$cache_key,
      toolchain = compiled$toolchain, execution=list(reduction=run$reduction,grainsize=run$grainsize,threads_per_chain=run$threads_per_chain,stan_threads=compiled$threaded), seed = run$seed, started = started, finished = Sys.time(),
      output_files = backend$output_files(), code_hash = .mp_hash(compiled$code))), class = "measurement_fit")
}

.mp_sync_stan_source <- function(code, file) {
  changed <- !file.exists(file) ||
    !identical(paste(readLines(file, warn = FALSE), collapse = "\n"), code)
  if (changed) writeLines(code, file)
  changed
}
