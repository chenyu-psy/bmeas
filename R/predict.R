.mp_replicate_aggregate_cache <- function(table,reps) {
  if(!is.matrix(reps)||nrow(reps)!=nrow(table)) .mp_abort("E_PREDICTION_SUPPORT","replicate statistic dimensions differ from summary table")
  table$statistic_id<-sprintf("stat_%04d",seq_len(nrow(table)))
  aggregates<-t(reps)
  dimnames(aggregates)<-list(paste0("replication_",seq_len(ncol(reps))),table$statistic_id)
  list(table=table,replicate_statistics=aggregates)
}
diagnostic_budget <- function(max_draws, max_observations, max_simulated_values,
  max_source_bytes=NULL,max_posterior_rows=NULL,max_draw_values=NULL,
  max_latent_values=NULL,max_statistics=NULL,max_pair_terms=NULL,posterior=NULL,max_work_values=NULL) {
  max_draws <- .mp_integer(max_draws,"max_draws",2L)
  max_observations <- .mp_integer(max_observations,"max_observations")
  max_simulated_values <- .mp_integer(max_simulated_values,"max_simulated_values")
  out<-structure(list(max_draws=max_draws,max_observations=max_observations,max_simulated_values=max_simulated_values),class="measurement_diagnostic_budget")
  extra<-list(max_source_bytes=max_source_bytes,max_posterior_rows=max_posterior_rows,
    max_draw_values=max_draw_values,max_latent_values=max_latent_values,
    max_statistics=max_statistics,max_pair_terms=max_pair_terms,posterior=posterior,max_work_values=max_work_values)
  for(n in names(extra))if(!is.null(extra[[n]]))out[[n]]<-extra[[n]]
  if(length(out)>3L) {
    checked<-.bh_ppc_budget_resolve(out)
    for(n in names(out)[-(1:3)])out[[n]]<-checked$values[[n]]
  }
  out
}
ppc_plan <- function(fit, draws, budget, level=NULL) {
  if(inherits(fit,"bmeas_fit_archive")) return(.bh_archive_ppc_plan(fit,draws,budget,level))
  .bh_pf_public_input(fit,"ppc_plan")
  if(.bh_pf_is_portable(fit)) {
    if(missing(level)||is.null(level)) .mp_abort("E_CHECK_UNSUPPORTED","portable predictive planning requires an explicit level")
    view<-.bh_pf_metadata(fit,"ppc_plan")
    if(.bh_m3cs_predictive_spec(.bh_pf_get(view,"spec"))||.bh_pf_product_view(view)) {
      root<-.bh_pf_envelope(fit)
      policy<-.bh_pf_stored_policy(root$immutable$validation_policy)
      .bh_portable_codec_count(root$immutable,policy,Sys.getlocale("LC_CTYPE"))
      .bh_pf_immutable_schema(root,policy)
    }
    .bh_pf_protected_gate(view,"ppc_plan")
    out<-.bh_ppc_plan(view,draws,budget,level)
    attrs<-attributes(out);out<-unclass(out)
    out$source$portable_context<-.bh_pf_metadata_notice(fit)
    attributes(out)<-attrs
    return(out)
  }
  if(inherits(fit,"bhmm_fit")&&!inherits(fit,"measurement_fit")) {
    if(missing(level)||is.null(level)) .mp_abort("E_CHECK_UNSUPPORTED","main-API predictive planning requires explicit level='population' or level='conditional'")
    return(.bh_ppc_plan(fit,draws,budget,level))
  }
  if(!inherits(fit,"measurement_fit")) .mp_abort("E_FIT","expected measurement_fit")
  if(.bh_ppc_streaming_budget(budget)) .mp_abort("E_DIAGNOSTIC_BUDGET","streaming posterior policy applies only to main-API fits; legacy PPC keeps its original budget")
  if(!is.null(level)&&!identical(level,"population")) .mp_abort("E_CHECK_UNSUPPORTED","the legacy planner supports population prediction only")
  if(any(names(budget)%in%names(.bh_ppc_budget_defaults())[-(1:3)])) .mp_abort("E_DIAGNOSTIC_BUDGET","new source/work overrides apply to the main frontend; legacy PPC retains its original three-field budget")
  if(budget$max_draws>20L||budget$max_observations>1000L||budget$max_simulated_values>20000L)
    .mp_abort("E_DIAGNOSTIC_BUDGET","legacy PPC retains 20 replications,1000 rows and20000 response values; use main-API streaming PPC for larger explicit requests")
  if(.mp_m3_ppc_supported(fit$spec))return(.mp_m3_ppc_plan(fit,draws,budget))
  if(!identical(fit$spec$structure$type,"cfa")) .mp_abort("E_CHECK_UNSUPPORTED","population PPC currently supports simple CFA structures only; component-generative prediction is not implemented")
  if(!inherits(budget,"measurement_diagnostic_budget")) .mp_abort("E_DIAGNOSTIC_BUDGET","explicit diagnostic_budget is required")
  diagnostic_budget(budget$max_draws,budget$max_observations,budget$max_simulated_values)
  draws <- .mp_integer(draws,"draws",2L)
  rows <- sum(vapply(fit$spec$data_map$blocks,nrow,integer(1)))
  if(draws>budget$max_draws || rows>budget$max_observations || draws*rows>budget$max_simulated_values)
    .mp_abort("E_DIAGNOSTIC_BUDGET","requested predictive work exceeds budget")
  if(any(!vapply(fit$spec$models,function(b)b$model$name%in%c("gaussian","binomial","lognormal"),logical(1)))) .mp_abort("E_CHECK_UNSUPPORTED","CFA population PPC is currently restricted to basic families")
  for(b in fit$spec$models) {
    cap <- b$model$capabilities
    if(!isTRUE(cap$available[cap$capability=="posterior_predict"]) || !is.function(b$model$methods$posterior_predict))
      .mp_abort("E_CAPABILITY","posterior_predict method unavailable for a requested block")
  }
  list(replications=draws,observations_per_rep=rows,simulated_values=rows*draws,
       allocation="one response replicate at a time; only aggregate discrepancies retained",
       level="population",screening_only=TRUE,
       memory_limit="not hard enforced; posterior parameter subset and small aggregate table are retained")
}
.mp_population_responses <- function(spec, draw) {
  info <- .mp_indicator_info(spec); map <- stan_symbol_map(spec)
  K <- length(spec$structure$factors); N <- length(spec$data_map$subject_keys)
  omega_names <- paste0("Omega[",as.vector(row(diag(K))),",",as.vector(col(diag(K))),"]")
  Omega <- matrix(draw[omega_names],K,K)
  # New correlated person effects, shared across every block and repeated row.
  z <- matrix(stats::rnorm(N*K),N,K) %*% chol(Omega)
  alpha <- draw[map$intercept]; lambda <- draw[map$loading]
  responses <- list()
  for(b in names(spec$models)) {
    block <- spec$models[[b]]; d <- spec$data_map$blocks[[b]]
    j <- match(paste(b,.mp_family_info(block$model$name)$primary,d$.task,sep="::"),info$key)
    eta <- alpha[j]+lambda[j]*z[cbind(d$.person_index,info$factor_id[j])]
    sd <- if(block$model$name=="binomial") NULL else unname(draw[map$observation_sd[j]])
    responses[[b]] <- block$model$methods$posterior_predict(eta=unname(eta),observation_sd=sd,
      trials=if(block$model$name=="binomial") d$trials else NULL)
    if(length(responses[[b]])!=nrow(d) || any(!is.finite(responses[[b]])))
      .mp_abort("E_PREDICTION_SUPPORT",paste("nonfinite or wrong-length predictions in block",b))
    validated <- d
    if(block$model$name=="binomial") validated$successes <- responses[[b]] else validated$y <- responses[[b]]
    .mp_validate_fields(block$model,validated,allow_missing_response=FALSE)
  }
  responses
}
.mp_ppc_statistics <- function(spec, responses = NULL) {
  info <- .mp_indicator_info(spec); rows <- list(); person_summaries <- list(); summary_units <- character()
  add <- function(domain,block,target,metric,estimate,unit,n_units,unit_type) {
    rows[[length(rows)+1L]] <<- data.frame(domain=domain,model=block,level="population",target=target,
      metric=metric,estimate=as.numeric(estimate),unit=unit,n_units=n_units,unit_type=unit_type,
      status=if(is.finite(estimate)) "computed" else "not_assessed",stringsAsFactors=FALSE)
  }
  for(j in seq_len(nrow(info))) {
    b <- info$block[j]; block <- spec$models[[b]]; d <- spec$data_map$blocks[[b]]
    all_y <- if(is.null(responses)) {if(block$model$name=="binomial") d$successes else d$y} else responses[[b]]
    index <- which(d$.task==info$task[j]); y <- all_y[index]; sub <- d[index,,drop=FALSE]
    unit <- block$units; key <- info$key[j]
    if(block$model$name=="binomial") {
      effective <- sub$trials>0; y <- y[effective]; sub <- sub[effective,,drop=FALSE]
      add("marginal",b,key,"total_successes",sum(y),"count",nrow(sub),"informative rows")
      add("marginal",b,key,"weighted_success_rate",sum(y)/sum(sub$trials),"probability",nrow(sub),"informative rows")
      add("marginal",b,key,"row_rate_sd_given_trials",stats::sd(y/sub$trials),"probability",nrow(sub),"informative rows; trials held fixed")
      add("marginal",b,key,"zero_success_rows",sum(y==0),"row_count",nrow(sub),"informative rows")
      add("marginal",b,key,"all_success_rows",sum(y==sub$trials),"row_count",nrow(sub),"informative rows")
      totals <- tapply(y,sub$.person_key,sum); trials <- tapply(sub$trials,sub$.person_key,sum)
      person_summaries[[key]] <- totals/trials; summary_units[key] <- "probability"
    } else {
      add("marginal",b,key,"mean",mean(y),unit,length(y),"observations")
      add("marginal",b,key,"median",stats::median(y),unit,length(y),"observations")
      add("marginal",b,key,"sd",stats::sd(y),unit,length(y),"observations")
      add("marginal",b,key,"q90",unname(stats::quantile(y,.9)),unit,length(y),"observations")
      if(block$model$name=="lognormal") {
        logunit <- paste0("log(response/",unit,")")
        add("marginal",b,key,"log_mean",mean(log(y)),logunit,length(y),"observations")
        add("marginal",b,key,"log_sd",stats::sd(log(y)),logunit,length(y),"observations")
      }
      person_summaries[[key]] <- tapply(y,sub$.person_key,mean); summary_units[key] <- unit
    }
  }
  if(length(person_summaries)>1L) for(pair in utils::combn(names(person_summaries),2L,simplify=FALSE)) {
    x <- person_summaries[[pair[1L]]]; y <- person_summaries[[pair[2L]]]
    common <- intersect(names(x),names(y))
    value <- if(length(common)>=2L) stats::cov(x[common],y[common]) else NA_real_
    add("measurement_structure","across_blocks",paste(pair,collapse=" | "),"paired_person_covariance",value,
        paste(summary_units[pair],collapse=" * "),length(common),"paired people")
  }
  do.call(rbind,rows)
}
assess_measurement_fit <- function(fit, checks = "ppc", level = "population", draws, seed, budget, simulator=NULL) {
  if(inherits(fit,"bmeas_fit_archive")) {
    if(missing(level)) .mp_abort("E_CHECK_UNSUPPORTED","archive assessment requires an explicit predictive level")
    return(.bh_archive_assess(fit,checks,level,draws,seed,budget,simulator))
  }
  .bh_pf_public_input(fit,"assess_measurement_fit")
  if(.bh_pf_is_portable(fit)) {
    if(missing(level)) .mp_abort("E_CHECK_UNSUPPORTED","portable assessment requires an explicit predictive level")
    if(!identical(checks,"ppc")) .mp_abort("E_CHECK_UNSUPPORTED","portable assessment requires checks='ppc'")
    validated<-.bh_pf_active(fit,"assess_measurement_fit")
    return(.bh_pf_after_validation(validated,function()
      .bh_pf_commit_cache(fit,.bh_ppc_assess(validated$view,checks,level,draws,seed,budget,simulator,validated$receipt))))
  }
  if(inherits(fit,"bhmm_fit")&&!inherits(fit,"measurement_fit")) {
    if(missing(level)) .mp_abort("E_CHECK_UNSUPPORTED","main-API assessment requires an explicit predictive level")
    return(.bh_ppc_assess(fit,checks,level,draws,seed,budget,simulator))
  }
  if(!inherits(fit,"measurement_fit")) .mp_abort("E_FIT","expected measurement_fit")
  if(.bh_ppc_streaming_budget(budget)) .mp_abort("E_DIAGNOSTIC_BUDGET","streaming posterior policy applies only to main-API fits; legacy PPC keeps its original budget")
  if(!identical(checks,"ppc") || !identical(level,"population"))
    .mp_abort("E_CHECK_UNSUPPORTED","only explicit population-level PPC is supported; no LOO is performed")
  .mp_prediction_rng_preflight()
  if(budget$max_draws>20L||budget$max_observations>1000L||budget$max_simulated_values>20000L)
    .mp_abort("E_DIAGNOSTIC_BUDGET","legacy PPC retains 20 replications,1000 rows and20000 response values; use main-API streaming PPC for larger explicit requests")
  if(.mp_m3_ppc_supported(fit$spec))return(.mp_assess_m3_ppc(fit,draws,seed,budget,simulator))
  plan <- ppc_plan(fit,draws,budget); seed <- .mp_integer(seed,"seed")
  check_measurement(fit$spec)
  map <- stan_symbol_map(fit$spec)
  variables <- unique(c(map$intercept,map$loading,map$observation_sd[!is.na(map$observation_sd)],"Omega"))
  posterior <- as.matrix(fit$backend$draws(variables=variables,format="draws_matrix"))
  # posterior::draws_matrix preserves dimensions on row slicing; use an ordinary
  # named numeric row below, never character-index a one-row draws object.
  class(posterior) <- c("matrix","array")
  if(nrow(posterior)<draws) .mp_abort("E_DIAGNOSTIC_BUDGET","more PPC draws requested than posterior draws available")
  selection <- as.integer(seq(1L,nrow(posterior),length.out=draws))
  had_seed <- exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(had_seed) old_seed <- get(".Random.seed",envir=.GlobalEnv)
  on.exit(if(had_seed) assign(".Random.seed",old_seed,envir=.GlobalEnv) else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv))
  set.seed(seed)
  result <- tryCatch({
    observed <- .mp_ppc_statistics(fit$spec)
    reps <- matrix(NA_real_,nrow(observed),draws)
    for(r in seq_len(draws)) {
      simulated <- .mp_population_responses(fit$spec,posterior[selection[r],])
      reps[,r] <- .mp_ppc_statistics(fit$spec,simulated)$estimate
    }
    table <- observed; table$observed <- observed$estimate; table$estimate <- NULL
    summarize <- function(fun) apply(reps,1L,function(x) if(all(is.finite(x))) fun(x) else NA_real_)
    table$replicated_median <- summarize(stats::median)
    table$lower90 <- summarize(function(x)unname(stats::quantile(x,.05)))
    table$upper90 <- summarize(function(x)unname(stats::quantile(x,.95)))
    table$outside90 <- ifelse(is.finite(table$observed)&is.finite(table$lower90)&is.finite(table$upper90),
      table$observed<table$lower90 | table$observed>table$upper90,NA)
    table$finite_replications <- rowSums(is.finite(reps))
    table$status <- ifelse(is.finite(table$observed)&table$finite_replications==draws,"computed","not_assessed")
    table$replications <- draws
    table$reason <- ifelse(table$status=="computed","Descriptive screening only; intervals unstable with tiny replication count","Insufficient finite observed or replicated statistics/paired people")
    retained<-.mp_replicate_aggregate_cache(table,reps)
    list(table=retained$table,replicate_statistics=retained$replicate_statistics,metadata=list(replicate_statistics_status="retained",replicate_statistics_layout="rows=replications; columns=table statistic_id",seed=seed,posterior_row_indices=selection,replications=draws,level="population",screening_only=TRUE,
      method="Fresh correlated population person effects; fixed observed design/trials; streaming aggregate discrepancies",
      zero_trial_rows=sum(vapply(fit$spec$data_map$blocks,function(d)if("trials"%in%names(d))sum(d$trials==0)else 0,numeric(1))),
      interval="90% empirical equal-tail replication interval; not a calibrated fit test",
      monte_carlo_uncertainty="Not reliably estimated from <=20 potentially dependent posterior draws; tail resolution is limited",
      data_hash=fit$spec$provenance$data_hash,model_hash=fit$spec$provenance$model_hash,plan=plan))
  },error=function(e)e)
  if(inherits(result,"error")) {
    fit$cache$predictive_fit <- .mp_status("failed",conditionMessage(result))
    fit$cache$measurement_structure_fit <- .mp_status("failed","This predictive attempt failed; no covariance assessment is retained")
    fit$cache$ppc <- list(metadata=list(seed=seed,plan=plan),table=NULL)
  } else {
    fit$cache$predictive_fit <- .mp_status(if(all(result$table$status=="computed"))"computed"else"warning",paste(draws,"population-level PPC reps; descriptive screening only, not a fit verdict; inspect statistic coverage"))
    fit$cache$ppc <- result
    complete <- all(result$table$status[result$table$domain=="measurement_structure"]=="computed")
    fit$cache$measurement_structure_fit <- .mp_status(if(complete)"computed"else"warning",
      "Paired-person cross-task covariance discrepancies; sparse pairs reported explicitly, no global fit verdict")
  }
  fit
}
fit_checks <- function(fit) {
  if(inherits(fit,"bmeas_fit_archive"))return(.bh_archive_report(fit,"fit_checks"))
  .bh_pf_public_input(fit,"fit_checks")
  if(.bh_pf_is_portable(fit))return(.bh_pf_fit_checks(fit))
  if(!inherits(fit,"measurement_fit")&&inherits(fit,"bhmm_fit"))return(.bh_fit_checks(fit))
  if(inherits(fit,"bhmm_spec")&&!inherits(fit,"measurement_fit")) .mp_abort("E_FIT","a prepared specification is not a fit; use check_model()")
  if(!inherits(fit,"measurement_fit")) .mp_abort("E_FIT","expected measurement_fit or bmeas_fit")
  result<-fit$cache$ppc
  if(!is.null(result))result$metadata$replicate_statistics_status<-if(!is.null(result$replicate_statistics))"retained"else if(!is.null(result$table))"unavailable_legacy_run"else"unavailable_failed_or_not_run"
  result
}
