measurement <- function(models, subjects, structure, priors, missing = missing_policy(), source_metadata=list()) {
  .bh_pf_refuse_execution(models,"measurement preparation")
  if(!is.list(source_metadata)) .mp_abort("E_PROVENANCE","source_metadata must be an explicit named list")
  .mp_names(models, "models"); .mp_key_component(names(models), "model block names")
  if (any(!vapply(models, inherits, logical(1), "observation_block"))) .mp_abort("E_BLOCK", "models must contain observation_blocks")
  .mp_legacy_parameter_level_gate(models)
  if (!inherits(subjects, "measurement_subjects")) .mp_abort("E_SUBJECTS", "explicit shared_subjects or subject_crosswalk is required")
  if (!inherits(structure, "measurement_structure")) .mp_abort("E_STRUCTURE", "explicit measurement structure is required")
  if (!inherits(priors, "measurement_priors")) .mp_abort("E_PRIOR", "explicit supported priors are required")
  if (!identical(missing$subjects, "union") || !missing$response %in% c("error", "observed_only")) .mp_abort("E_MISSING_POLICY", "invalid missing policy")
  .mp_check_resources(models)
  mapped <- .mp_map_data(models, subjects, missing)
  spec <- base::structure(list(models = models, subjects = subjects, structure = structure,
    priors = priors, missing = missing, source_metadata=source_metadata, data_map = mapped,
    parameter_keys = .mp_parameter_keys(models, mapped), version = "P1-structure-ir-0.0.4"), class = "measurement_spec")
  spec$checks <- check_measurement(spec)
  spec$provenance <- list(data_hash = .mp_hash(mapped), model_hash = .mp_hash(lapply(models, function(x) x$model)),
                          standardization = "none")
  spec
}
.mp_legacy_parameter_level_gate <- function(models) {
  if(any(vapply(models,function(b)!is.null(b$parameter_levels),logical(1))))
    .mp_abort("E_PARAMETER_LEVEL_UNSUPPORTED","block parameter-level overrides belong to the bhmf()/bhmm() frontend; the historical measurement() compiler does not apply them")
  if(any(vapply(models,function(b)any(vapply(b$model$parameters,function(p)!is.null(p$role),logical(1))),logical(1))))
    .mp_abort("E_PARAMETER_LEVEL_UNSUPPORTED","explicit role/default-level contracts use the bhmf()/bhmm() frontend; the historical compiler preserves its older role convention")
  invisible(TRUE)
}
.mp_check_resources <- function(models) {
  definitions <- list()
  for (block in models) {
    likelihood <- block$model$likelihood
    if (likelihood$builtin) next
    for (symbol in likelihood$exports) {
      if (!is.null(definitions[[symbol]]) && !identical(definitions[[symbol]], likelihood$resource_hash))
        .mp_abort("E_SYMBOL_COLLISION", paste("conflicting external Stan symbol", symbol))
      definitions[[symbol]] <- likelihood$resource_hash
    }
  }
  invisible(TRUE)
}
check_measurement <- function(spec) {
  .bh_pf_refuse_execution(spec,"measurement checking or generation")
  if (!inherits(spec, "measurement_spec")) .mp_abort("E_SPEC", "expected measurement_spec")
  .mp_legacy_parameter_level_gate(spec$models)
  s <- spec$structure; keys <- spec$parameter_keys
  if (!s$type %in% c("cfa","component_covariance")) .mp_abort("E_STRUCTURE_NOT_IMPLEMENTED", "direct covariance declaration exists; its prior resolver and compiler are not implemented in P0")
  if (any(vapply(spec$models, function(b) {
      cap <- b$model$capabilities
      flag <- cap$available[cap$capability == "fit"]
      length(flag) != 1L || !isTRUE(flag)
    }, logical(1)))) .mp_abort("E_CAPABILITY", "every observation model must declare fit capability before compilation")
  if (any(vapply(spec$models, function(b) !b$model$backend %in% c("gaussian_cfa", "basic_cfa", "registered_stan"), logical(1))))
    .mp_abort("E_BACKEND_UNSUPPORTED", "Observation model has no executable adapter")
  for(b in names(spec$models)) {
    model<-spec$models[[b]]$model;d<-spec$data_map$blocks[[b]]
    .mp_validate_fields(model,d,allow_missing_response=FALSE)
    if(!is.null(model$validate_data)&&!isTRUE(model$validate_data(d))) .mp_abort("E_CUSTOM_VALIDATION",paste("consumed-data validation failed for",b))
    index<-match(d$.person_key,spec$data_map$subject_keys)
    if(anyNA(index)||!identical(as.integer(d$.person_index),as.integer(index))) .mp_abort("E_SUBJECT_ALIGNMENT","mapped subject indices do not match their global person keys")
  }
  .mp_adapter_resources(spec)
  if(identical(s$type,"component_covariance"))return(.mp_component_validate(spec))
  if(.mp_registry_mode(spec)&&!identical(spec$priors$kind,"parameter_specific")) .mp_abort("E_PRIOR_SCALE","registered adapters require explicit exact-key parameter_prior_set")
  # Inspectable objects are revalidated before every compile/data-generation path.
  cfa_structure(s$factors, scale = s$scale, positive_anchors = s$positive_anchors,
                parameter_residual = s$parameter_residual, nuisance = s$nuisance)
  .mp_resolve_priors(spec)
  selected <- unlist(s$factors, use.names = FALSE)
  if (!all(selected %in% keys$key)) .mp_abort("E_TARGET", paste("unknown/no-data parameter key:", paste(setdiff(selected, keys$key), collapse = ", ")))
  primary <- unlist(lapply(names(spec$models), function(b) {
    info <- .mp_model_info(spec$models[[b]]$model)
    keys$key[keys$model == b & keys$parameter %in% info$primary]
  }), use.names = FALSE)
  if (!setequal(selected, primary))
    .mp_abort("E_PARAMETER_COVERAGE", "every primary parameter (mu/p/meanlog) must be assigned exactly once; scale parameters cannot load")
  needed_nuisance <- unique(unlist(lapply(spec$models, function(b) .mp_model_info(b$model)$nuisance)))
  if (!identical(s$nuisance$level, "task") || !setequal(s$nuisance$parameter, needed_nuisance))
    .mp_abort("E_PARAMETER_COVERAGE", "explicit nuisance must cover exactly Gaussian sigma and lognormal sdlog, with no binomial observation scale")
  for(b in names(spec$models)) {
    d <- spec$data_map$blocks[[b]]
    if(spec$models[[b]]$model$name == "binomial") for(task in unique(d$.task))
      if(!any(d$trials[d$.task==task]>0)) .mp_abort("E_UNINFORMATIVE_PARAMETER",paste("all trials are zero for",b,task))
  }
  binding<-parameter_binding_table(spec)
  factor_people<-lapply(s$factors,function(target_keys)unique(unlist(lapply(target_keys,function(key).mp_people_for_target(spec,key,binding)))))
  adjacency <- outer(seq_along(factor_people), seq_along(factor_people), Vectorize(function(i,j) length(intersect(factor_people[[i]], factor_people[[j]])) > 0))
  reachable <- 1L
  repeat {
    next_reachable <- which(colSums(adjacency[reachable, , drop = FALSE]) > 0)
    if (setequal(reachable, next_reachable)) break
    reachable <- next_reachable
  }
  if (length(reachable) != length(factor_people))
    .mp_abort("E_DISCONNECTED_CORRELATION", "factors occupy disconnected subject sets; free cross-factor correlation has no shared-data connection")
  # A disconnected indicator graph leaves directions ambiguous even with one anchor.
  for (factor in names(s$factors)) {
    indicator_people <- lapply(s$factors[[factor]],function(key).mp_people_for_target(spec,key,binding))
    graph <- outer(seq_along(indicator_people), seq_along(indicator_people),
      Vectorize(function(i,j) length(intersect(indicator_people[[i]], indicator_people[[j]])) > 0))
    reached <- 1L
    repeat {
      next_reached <- which(colSums(graph[reached, , drop = FALSE]) > 0)
      if (setequal(reached, next_reached)) break
      reached <- next_reached
    }
    if (length(reached) != length(indicator_people))
      .mp_abort("E_DISCONNECTED_INDICATOR", paste("indicator co-observation graph is disconnected for factor", factor))
  }
  prior_table(spec)
  if(!is.null(spec$provenance$data_hash)&&!identical(spec$provenance$data_hash,.mp_hash(spec$data_map)))
    .mp_abort("E_DATA_CHANGED","rebuild measurement() after data edits; cached mappings/provenance cannot be mutated")
  list(static_constraints = list(status = "computed", reason = "Simple structure, anchors, parameter coverage, priors and connectivity checked"),
       identification = list(status = "not_assessed", reason = "Static constraints and connectivity do not establish statistical identification"),
       prior_sensitivity = list(status = "not_assessed", reason = "No sensitivity runs requested"),
       data_reuse = list(status = if (length(spec$models) == 1L || all(vapply(spec$models, function(b) !is.null(b$source_id), logical(1)))) "computed" else "not_assessed",
                        reason = "Duplicate source/observation keys checked when source_id is supplied; related summaries versus raw trials cannot be detected automatically"),
       warnings = if (length(spec$data_map$subject_keys) < 10L) "Tiny sample is engineering-only; no inferential adequacy claim" else character())
}
print.measurement_spec <- function(x, ...) {
  cat("Measurement specification:", length(x$models), "block(s),", length(x$data_map$subject_keys), "people\n")
  if(identical(x$structure$type,"component_covariance")) {
    cat("Structure: declared G/P/C variance-share component covariance with task residuals and sample-centered people\n")
  } else cat("Structure: simple CFA over declared parameter predictors; unit factor variance; no extra parameter residual\n")
  cat("Static constraints checked; statistical identification not assessed\n")
  invisible(x)
}

.mp_informative_data <- function(spec,block) {
  d <- spec$data_map$blocks[[block]]
  if(spec$models[[block]]$model$name == "binomial") d <- d[d$trials>0,,drop=FALSE]
  if(!is.null(spec$models[[block]]$model$informative_rows)) d<-d[spec$models[[block]]$model$informative_rows(d),,drop=FALSE]
  d
}
