.mp_status <- function(status, reason) list(status = status, reason = reason)
.mp_cache_fit <- function(backend, spec, run) {
  component<-identical(spec$structure$type,"component_covariance")
  monitored <- if(component)structure_ir(spec)$monitored else c("alpha", "lambda", if (.mp_legacy_gaussian(spec)) "sigma" else if (if(.mp_registry_mode(spec)) .mp_registry_indices(spec)$S>0 else any(.mp_indicator_info(spec)$sd_id>0)) "obs_sd", "Omega")
  summaries <- tryCatch(as.data.frame(backend$summary(variables = monitored,
    "mean", "median", "sd", "quantile2", "rhat", "ess_bulk", "ess_tail", "mcse_mean")),
    error = function(e) e)
  hmc <- tryCatch(backend$diagnostic_summary(quiet = TRUE), error = function(e) e)
  computation <- if (inherits(summaries, "error") || inherits(hmc, "error"))
    .mp_status("failed", paste("diagnostic caching failed:", paste(c(if (inherits(summaries,"error")) conditionMessage(summaries), if (inherits(hmc,"error")) conditionMessage(hmc)), collapse = "; ")))
    else .mp_status(if (any(hmc$num_divergent > 0) || any(hmc$num_max_treedepth > 0)) "warning" else "computed",
      "Numerical sampling completed; tiny smoke draws do not establish inference reliability")
  if (inherits(summaries, "error")) summaries <- NULL
  if (inherits(hmc, "error")) hmc <- NULL
  if (!is.null(summaries)) {
    map <- stan_symbol_map(spec)
    if(component) {
      symbols<-c(map$intercept,map$task_sd,map$observation_sd)
      labels<-c(paste0("mean:",map$key),paste0("task_sd:",map$key),paste0("observation_sd:",map$observation_sd_key))
      summaries$parameter_key<-labels[match(summaries$variable,symbols)]
      common<-structure_ir(spec)$common_keys
      for(i in seq_along(common))for(j in seq_along(common))summaries$parameter_key[summaries$variable==paste0("Omega_component[",i,",",j,"]")]<-paste0("component_correlation:",common[i]," | ",common[j])
      missing<-is.na(summaries$parameter_key);summaries$parameter_key[missing]<-paste0("structural:",summaries$variable[missing])
    } else {
      symbols <- c(map$intercept, map$loading, map$observation_sd)
      labels <- c(paste0("intercept:",map$key), paste0("loading:",map$key), paste0(map$nuisance_parameter,":",map$observation_sd_key))
      summaries$parameter_key <- labels[match(summaries$variable, symbols)]
      for (i in seq_along(spec$structure$factors)) for (j in seq_along(spec$structure$factors)) {
        variable <- paste0("Omega[",i,",",j,"]")
        summaries$parameter_key[summaries$variable == variable] <- paste0("factor_correlation:",names(spec$structure$factors)[i], "::", names(spec$structure$factors)[j])
      }
    }
  }
  list(computation = computation, parameter_summary = summaries, hmc = hmc,
    monitored_variables = monitored, thresholds = list(rhat = 1.01, ess = 400, ebfmi = 0.3),
    inference = .mp_status("not_assessed", "SMOKE ONLY; no convergence or recovery claim"),
    identification = spec$checks$identification,
    static_constraints = spec$checks$static_constraints,
    predictive_fit = if(.mp_m3_ppc_supported(spec)) .mp_status("not_assessed","M3 group PPC requires explicit simulator compilation and budgeted assess command; no prediction was run") else if(identical(spec$structure$type,"cfa")&&all(vapply(spec$models,function(b)b$model$name%in%c("gaussian","binomial","lognormal"),logical(1)))&&all(vapply(spec$models,function(b)isTRUE(b$model$capabilities$available[b$model$capabilities$capability=="posterior_predict"]),logical(1))))
      .mp_status("not_assessed","PPC is available only by an explicit budgeted command; no prediction was run") else
      .mp_status("unsupported","PPC method is unavailable for the observation models or declared structure"),
    measurement_structure_fit = .mp_status("not_assessed","No population cross-task predictive check performed"),
    admissibility = .mp_status("not_assessed", "No near-boundary or near-singularity screening performed"),
    prior_sensitivity = .mp_status("not_assessed", "No prior-sensitivity runs performed"),
    conventional_fit = .mp_status("not_assessed", "No family-specific SEM indices computed; these are not universal mixed-likelihood fit measures"))
}
summary.measurement_fit <- function(object, ...) {
  .bh_pf_legacy_report_input(object,"summary")
  # Deliberately does not call the backend or touch draws/files.
  structure(list(cache = object$cache, plan = object$execution_plan,
    blocks = stats::setNames(lapply(names(object$spec$models), function(name) {
      b <- object$spec$models[[name]]; d <- object$spec$data_map$blocks[[name]]
      list(model = b$model$name, version = b$model$version, units = b$units,
           people = length(unique(d$.person_key)), observations = nrow(d))
    }), names(object$spec$models)),
    subjects = length(object$spec$data_map$subject_keys), overlap = object$spec$data_map$overlap,
    excluded_responses = nrow(object$spec$data_map$excluded), estimand=if(identical(object$spec$structure$type,"component_covariance")) "Declared common-component correlation and task-predictor covariance" else "Unit-variance factor correlation; mixed parameter covariance is on predictor scales", example_warning = object$spec$example_warning), class = "summary.measurement_fit")
}
.mp_extreme <- function(x, kind = "min") {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  if (kind == "min") min(x, na.rm = TRUE) else max(x, na.rm = TRUE)
}
print.summary.measurement_fit <- function(x, ...) {
  if(inherits(x,"summary.bmeas_portable_fit")) .bh_pf_fail("print","use the portable summary method for portable report tags","E_PORTABLE_UNSUPPORTED")
  cat("Measurement fit:", length(x$blocks), "block(s) |", x$subjects, "people | SMOKE ONLY\n")
  cat("Run:", x$plan$chains, "chain(s),", x$plan$saved_draws, "saved draws total |",x$plan$reduction,"likelihood | threads",x$plan$threads_per_chain,"| grainsize",if(is.null(x$plan$grainsize))1L else x$plan$grainsize,"\n")
  cat("Target:",x$estimand,"\n")
  if(!is.null(x$example_warning)) cat("Scope:",x$example_warning,"\n")
  for (b in names(x$blocks)) cat("  ", b, ":", x$blocks[[b]]$model, "v", x$blocks[[b]]$version, "|", x$blocks[[b]]$people, "people,", x$blocks[[b]]$observations, "observations | units:", x$blocks[[b]]$units, "\n")
  cat("Excluded missing responses:", x$excluded_responses, "| block overlap counts are retained in summary(fit)$overlap\n")
  c <- x$cache
  cat("Computation [", c$computation$status, "]: ", c$computation$reason, "\n", sep = "")
  if (!is.null(c$parameter_summary)) {
    s <- c$parameter_summary
    cat("  Cached max Rhat:", .mp_extreme(s$rhat,"max"), "| min bulk/tail ESS:", .mp_extreme(s$ess_bulk), "/", .mp_extreme(s$ess_tail), "\n")
    if (any(!is.na(s$rhat))) {
      worst <- which.max(replace(s$rhat, is.na(s$rhat), -Inf))
      cat("  Max Rhat target:", if (!is.null(s$parameter_key)) s$parameter_key[worst] else s$variable[worst], "\n")
    }
    cat("  MCSE and parameter intervals: parameter_summary(fit); 90% equal-tail intervals\n")
  }
  if (!is.null(c$hmc)) cat("  Divergences:", sum(c$hmc$num_divergent), "| treedepth hits:", sum(c$hmc$num_max_treedepth), "| min E-BFMI:", .mp_extreme(c$hmc$ebfmi), "\n")
  for (domain in c("inference", "predictive_fit", "identification", "admissibility", "prior_sensitivity", "conventional_fit"))
    cat(domain, " [", c[[domain]]$status, "]: ", c[[domain]]$reason, "\n", sep = "")
  if(!is.null(c$ppc$table)) {
    tab <- c$ppc$table
    target<-if(!is.null(c$ppc$metadata$population_covariance))"new centered same-N group reps"else"population reps"
    cat("PPC cache:",c$ppc$metadata$replications,target,"| zero-total/trial rows excluded from rate summaries:",c$ppc$metadata$zero_trial_rows,"\n")
    cat("  Replicate aggregates:",if(is.null(c$ppc$replicate_statistics))"unavailable (earlier run)"else"retained and keyed to statistic_id","\n")
    outside <- which(tab$outside90 %in% TRUE)
    cat("  Descriptive statistics outside their replication interval:",length(outside),"of",sum(tab$status=="computed"),"; not a significance test or model verdict\n")
    show <- head(unique(c(outside,which(tab$status=="computed"))),3L)
    for(i in show) cat("  ",tab$target[i],tab$metric[i],": observed",signif(tab$observed[i],3),
      "| predictive 90% [",signif(tab$lower90[i],3),",",signif(tab$upper90[i],3),"]",tab$unit[i],"\n")
    cat("  Cross-task paired-person covariance checks:",sum(tab$domain=="measurement_structure"),"| full cached table: fit_checks(fit)\n")
    cat("  Screening only: tiny-replication intervals have substantial Monte Carlo uncertainty\n")
  }
  cat("Static constraints checked. No global model-fit verdict.\n")
  cat("This smoke output is not usable posterior inference or evidence of good model fit.\n")
  invisible(x)
}
print.measurement_fit <- function(x, ...) { .bh_pf_legacy_report_input(x,"print"); print(summary(x)); invisible(x) }
parameter_summary <- function(fit) {
  if(inherits(fit,"bmeas_fit_archive"))return(.bh_archive_report(fit,"parameter_summary"))
  .bh_pf_public_input(fit,"parameter_summary")
  if(.bh_pf_is_portable(fit))return(posterior_summary(fit))
  if(!inherits(fit,"measurement_fit")&&inherits(fit,"bhmm_fit"))return(posterior_summary(fit))
  if(inherits(fit,"bhmm_spec")&&!inherits(fit,"measurement_fit")) .mp_abort("E_FIT","a prepared specification is not a fit; use check_model()")
  if (!inherits(fit, "measurement_fit")) .mp_abort("E_FIT", "expected measurement_fit or bmeas_fit")
  fit$cache$parameter_summary
}
correlation_draws <- function(fit, space) {
  .bh_pf_refuse_execution(fit,"correlation_draws")
  if (!inherits(fit,"measurement_fit")) .mp_abort("E_FIT","expected measurement_fit")
  if(missing(space)) .mp_abort("E_CORRELATION_SPACE","explicit correlation space is required")
  if(identical(fit$spec$structure$type,"component_covariance")) {
    if(!space%in%c("component","task_parameter")) .mp_abort("E_CORRELATION_SPACE","component structure requires space=component or task_parameter; generic factor/derived-score aliases are not assumed")
    symbol<-if(space=="component")"Omega_component"else"Sigma_task"
    out<-fit$backend$draws(symbol,format="draws_array")
    attr(out,"parameter_keys")<-if(space=="component")structure_ir(fit$spec)$common_keys else fit$spec$structure$keys
    attr(out,"estimand")<-if(space=="component")"Reference common-component correlation; within-family diagonal blocks fixed identity"else"Task-parameter population correlation on predictor scale; not nonlinear raw-scale or person point-estimate correlation"
    return(out)
  }
  if(!identical(space,"factor")) .mp_abort("E_CORRELATION_SPACE","simple CFA exports factor correlation; specify space=factor")
  out<-fit$backend$draws("Omega",format="draws_array");attr(out,"factor_names")<-names(fit$spec$structure$factors)
  attr(out,"estimand")<-"Population factor correlation, unit-variance factors, positive anchors";out
}
individual_parameter_summary <- function(fit, scales, budget, states = "all") {
  if(inherits(fit,"bmeas_fit_archive")) {
    if(missing(scales)||missing(budget)) .mp_abort("E_INDIVIDUAL_OPTIONS","archive fits require explicit scales and budget")
    return(.bh_archive_individual(fit,scales,budget,states))
  }
  .bh_pf_public_input(fit,"individual_parameter_summary")
  if(.bh_pf_is_portable(fit)) {
    if(missing(scales)||missing(budget)) .mp_abort("E_INDIVIDUAL_OPTIONS","portable fits require explicit scales and a plain individual-parameter budget")
    validated<-.bh_pf_active(fit,"individual_parameter_summary")
    return(.bh_pf_after_validation(validated,function()
      .bh_individual_parameter_summary(validated$view,scales,budget,states,validated$receipt)))
  }
  if (!inherits(fit, "measurement_fit") && inherits(fit, "bhmm_fit")) {
    if (missing(scales) || missing(budget))
      .mp_abort("E_INDIVIDUAL_OPTIONS", "main fits require explicit scales and a plain individual-parameter budget")
    return(.bh_individual_parameter_summary(fit, scales, budget, states))
  }
  if (!missing(scales) || !missing(budget) || !missing(states))
    .mp_abort("E_INDIVIDUAL_OPTIONS", "scales, budget and states apply only to main-API fits; the legacy one-argument behavior is unchanged")
  if(!inherits(fit,"measurement_fit")||!identical(fit$spec$structure$type,"component_covariance")) .mp_abort("E_FIT","individual eta export currently requires component structure")
  draws<-fit$backend$draws("eta",format="draws_matrix");class(draws)<-c("matrix","array")
  records<-list();bindings<-parameter_binding_table(fit$spec)
  for(j in seq_along(fit$spec$structure$keys))for(n in seq_along(fit$spec$data_map$subject_keys)) {
    x<-draws[,paste0("eta[",n,",",j,"]")]
    key<-fit$spec$structure$keys[j];person<-fit$spec$data_map$subject_keys[n];parts<-strsplit(key,"::",fixed=TRUE)[[1L]];block<-parts[1L]
    if(identical(fit$spec$models[[block]]$model$adapter$mode,"routed")) ids<-unique(bindings$observation_id[!is.na(bindings$target_key)&bindings$target_key==key&bindings$person_key==person]) else {
      d<-fit$spec$data_map$blocks[[block]];ids<-d$.observation_id[d$.person_key==person&d$.task==parts[3L]]
    }
    informative<-.mp_informative_data(fit$spec,block)$.observation_id
    records[[length(records)+1L]]<-data.frame(person_key=fit$spec$data_map$subject_keys[n],parameter_key=fit$spec$structure$keys[j],
      mean=mean(x),sd=stats::sd(x),lower90=unname(stats::quantile(x,.05)),upper90=unname(stats::quantile(x,.95)),
      scale="predictor",saved_draws=length(x),n_likelihood_rows=length(ids),n_informative_rows=sum(ids%in%informative),interpretation="posterior_plugin_summary_not_error_free_input",status="smoke_only_not_usable_inference",stringsAsFactors=FALSE)
  }
  do.call(rbind,records)
}
