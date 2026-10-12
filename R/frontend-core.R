# Component-first frontend. This file does not change the historical compilers.
# Scalar free loadings and fixed-basis shared amplitudes use one coefficient map.
.bh_public_id <- function(prefix, ...) {
  parts <- list(...)
  paste(c(prefix,vapply(parts,function(x)utils::URLencode(x,reserved=TRUE,repeated=TRUE),character(1))),collapse=":")
}
.bh_builtin_kind <- function(model) {
  fields <- c("data_schema","parameters","likelihood","backend","constants","adapter")
  factories <- list(gaussian=gaussian_model,binomial=binomial_model,lognormal=lognormal_model)
  for(name in names(factories)) if(identical(model[fields],factories[[name]]()[fields])) return(name)
  NA_character_
}
.bh_information_contract <- function(model) {
  builtin <- .bh_builtin_kind(model)
  if(!is.null(.bh_binomial_row_fields(model)))return("known_binomial_positive_trials")
  if(!is.na(builtin))return("known_basic_observed_rows")
  if(!is.na(.bh_m3_row_information_kind(model)))
    return("known_m3_positive_total")
  "observed_rows_information_not_qualified"
}
.bh_selector <- function(x, env) {
  if (is.character(x)) value <- x else if (is.symbol(x)) {
    name <- as.character(x)
    binding_env<-env
    while(!base::exists(name,binding_env,inherits=FALSE) && !identical(binding_env,emptyenv()))
      binding_env<-base::parent.env(binding_env)
    if(!base::exists(name,binding_env,inherits=FALSE)) .mp_abort("E_SELECTOR",paste("unknown task selector",name))
    if(base::bindingIsActive(name,binding_env)) .mp_abort("E_SELECTOR","task selectors cannot use active bindings; supply an ordinary character-vector value")
    # Capturing an ordinary R symbol can force its normal lazy argument promise;
    # the DSL itself never evaluates an inline call or an active-binding function.
    value <- base::get(name,binding_env,inherits=FALSE)
  } else if (is.call(x) && identical(x[[1L]], as.name("c"))) {
    parts <- as.list(x)[-1L]
    if (!length(parts) || any(!vapply(parts, function(part)typeof(part)=="character"&&!is.object(part), logical(1))))
      .mp_abort("E_SELECTOR", "c() in task selectors accepts character literals only")
    value <- unlist(parts, use.names = FALSE)
  } else .mp_abort("E_SELECTOR", "use task strings, a character-vector name, or literal c(...)")
  if(typeof(value)!="character" || is.object(value))
    .mp_abort("E_SELECTOR","task selectors require plain character vectors, without an S3/S4 class")
  if (!length(value) || anyNA(value) || anyDuplicated(value))
    .mp_abort("E_SELECTOR", "task selectors must be nonempty distinct character values")
  .mp_key_component(value, "task selector")
}
.bh_targets_ast <- function(x, env) {
  if(is.call(x)&&identical(x[[1L]],as.name("("))&&length(x)==2L)return(.bh_targets_ast(x[[2L]],env))
  if (is.call(x) && identical(x[[1L]], as.name("+")) && length(x) == 3L)
    return(c(.bh_targets_ast(x[[2L]], env), .bh_targets_ast(x[[3L]], env)))
  if (is.symbol(x)) return(list(list(kind = "wide", column = as.character(x))))
  if (is.call(x) && identical(x[[1L]], as.name("[")) && length(x) == 3L) {
    left <- x[[2L]]
    if (is.call(left) && identical(left[[1L]], as.name("$")) && length(left) == 3L &&
        is.symbol(left[[2L]]) && is.symbol(left[[3L]])) {
      block <- .mp_key_component(as.character(left[[2L]]), "block")
      parameter <- .mp_key_component(as.character(left[[3L]]), "parameter")
      return(lapply(.bh_selector(x[[3L]], env), function(task)
        list(kind = "target", block = block, parameter = parameter, task = task)))
    }
  }
  .mp_abort("E_FORMULA", "targets must be wide column names or block$parameter[tasks], joined by +")
}
.bh_formula_targets <- function(x) {
  if (!inherits(x, "formula") || length(x) != 2L)
    .mp_abort("E_FORMULA", "each named component needs a one-sided measurement formula")
  out <- .bh_targets_ast(x[[2L]], environment(x))
  ids <- vapply(out, function(z) paste(unlist(z), collapse = "::"), character(1))
  if (anyDuplicated(ids)) .mp_abort("E_DUPLICATE_LOADING", "a target occurs twice within one component")
  out
}
.bh_cor_names <- function(x) {
  if(is.call(x)&&identical(x[[1L]],as.name("("))&&length(x)==2L)return(.bh_cor_names(x[[2L]]))
  if (is.symbol(x)) return(as.character(x))
  if (is.call(x) && identical(x[[1L]], as.name("+")) && length(x) == 3L)
    return(c(.bh_cor_names(x[[2L]]), .bh_cor_names(x[[3L]])))
  .mp_abort("E_CORRELATION_SYNTAX", "correlation selectors use component names with +, or A:B")
}
.bh_cor_pairs <- function(x, components) {
  if (!inherits(x, "formula") || length(x) != 2L)
    .mp_abort("E_CORRELATION_SYNTAX", "correlation rules require one-sided formulas")
  rhs <- .bh_unwrap(x[[2L]])
  cross <- is.call(rhs) && identical(rhs[[1L]], as.name(":")) && length(rhs) == 3L
  if (cross) {
    a <- .bh_cor_names(rhs[[2L]]); b <- .bh_cor_names(rhs[[3L]])
    if (length(intersect(a, b))) .mp_abort("E_CORRELATION_SYNTAX", "cross groups must be disjoint")
    selected <- c(a, b)
    pairs <- as.matrix(expand.grid(a = a, b = b, stringsAsFactors = FALSE))
  } else {
    selected <- .bh_cor_names(rhs)
    if (length(selected) < 2L) .mp_abort("E_CORRELATION_SYNTAX", "a scalar correlation rule needs two or more components")
    pairs <- t(utils::combn(selected, 2L))
  }
  if (anyDuplicated(selected) || !all(selected %in% components))
    .mp_abort("E_CORRELATION_TARGET", "correlation rules must name distinct declared components")
  t(apply(pairs, 1L, function(p) sort(match(p, components))))
}
.bh_cor_table <- function(cor, components, resolve_pairs = .bh_cor_pairs) {
  K <- length(components)
  if (is.character(cor) && length(cor) == 1L && cor %in% c("free", "independent"))
    cor <- list(default = if (cor == "free") "free" else "zero")
  if (!is.list(cor) || is.null(names(cor)) || anyDuplicated(names(cor)))
    .mp_abort("E_CORRELATION_SYNTAX", "use free, independent, or a named zero/free correlation specification")
  if(!is.null(.bh_cor_extensions(cor)))return(.bh_affine_cor_table(cor,components,resolve_pairs))
  cor$fixed<-cor$equal<-NULL
  if (any(!names(cor) %in% c("default", "zero", "free")) ||
      !identical(length(cor$default), 1L) || !cor$default %in% c("zero", "free"))
    .mp_abort("E_CORRELATION_SYNTAX", "correlation default must be zero or free")
  pairs <- if (K > 1L) t(utils::combn(seq_len(K), 2L)) else matrix(integer(), 0L, 2L)
  out <- data.frame(row = pairs[, 1L], col = pairs[, 2L],
    component1 = components[pairs[, 1L]], component2 = components[pairs[, 2L]],
    state = rep(cor$default, nrow(pairs)), source = rep("default", nrow(pairs)), stringsAsFactors = FALSE)
  explicit <- rep(NA_character_, nrow(out))
  for (kind in c("zero", "free")) if (!is.null(cor[[kind]])) {
    rules <- cor[[kind]]
    if (!is.list(rules) || inherits(rules, "formula"))
      .mp_abort("E_CORRELATION_SYNTAX", "put correlation-rule formulas inside a list")
    for (rule in rules) {
      p <- resolve_pairs(rule, components)
      at <- match(paste(p[, 1L], p[, 2L]), paste(out$row, out$col))
      if (any(!is.na(explicit[at]) & explicit[at] != kind))
        .mp_abort("E_CORRELATION_CONFLICT", "the same marginal correlation is explicitly both zero and free")
      explicit[at] <- kind; out$state[at] <- kind; out$source[at] <- "explicit"
    }
  }
  out$parameter <- vapply(seq_len(nrow(out)),function(i)if(out$state[i]=="free")
    .bh_public_id("cor",out$component1[i],out$component2[i])else NA_character_,character(1))
  out
}
bmf <- function(..., cor = "free", components = NULL, residual = NULL,
                 scale = NULL, loadings = NULL, decomposition = NULL, person = "iid", augmentation = NULL) {
  declarations <- list(...)
  if (!is.null(components)) {
    if (length(declarations)) .mp_abort("E_FORMULA", "use named components in ... or components=, not both")
    declarations <- components
  }
  if(length(declarations)) .mp_names(declarations, "components")
  person<-.bh_capture_person(person);augmentation<-.bh_capture_augmentation(augmentation)
  if(is.list(person)!=!is.null(augmentation)).mp_abort("E_REFERENCE_COMBINATION","centered persons and task_cholesky augmentation must be selected together in this first capability")
  raw_coordinates<-if(is.list(cor))cor$coordinates else NULL
  if(is.list(cor))cor$coordinates<-NULL
  parsed <- lapply(declarations,function(x)if(inherits(x,"bhmm_basis_component")){.bh_validate_basis(x);x}else .bh_formula_targets(x))
  owners<-.bh_parse_decomposition(decomposition,parsed)
  residual_parsed <- .bh_parse_residual(residual,owners)
  # An empty declaration is meaningful for task/fixed-only observation blocks.
  # Subject-task defaults still require complete explicit coverage at binding.
  coordinates<-.bh_component_coordinates(parsed)
  share_plan<-.bh_share_plan(parsed,owners,coordinates)
  raw_coordinates<-.bh_capture_raw_coordinates(raw_coordinates,coordinates)
  correlations <- .bh_cor_table(cor,if(nrow(coordinates))coordinates$display else NULL,function(rule,components).bh_expanded_cor_pairs(rule,components,coordinates))
  if(!is.null(raw_coordinates)&&.bh_has_cor_extensions(correlations))
    .mp_abort("E_CORRELATION_COORDINATES","ordered raw-B common coordinates do not accept fixed/equal common correlation rules; the residual layer has its own declaration")
  correlations<-.bh_share_correlations(correlations,share_plan)
  .bh_reference_graph(raw_coordinates,correlations)
  out <- structure(list(components = parsed, correlations = correlations,
    component_names = names(parsed), coordinates=coordinates,person = person, scale = .bh_capture_marker_scales(scale,names(parsed)),
    residual = residual_parsed, version = "loading-expression-components-v2"), class = c("bmeas_formula","bhmm_formula"))
  if(!is.null(share_plan)){out$decomposition<-share_plan;out$version<-"variance-share-components-v1"}
  out$loadings<-.bh_capture_loading_constraints(loadings,names(parsed))
  if(!is.null(out$loadings))out$version<-"ordinary-loading-constraints-v1"
  if(length(.bh_marker_declarations(out)))out$version<-"marker-scale-components-v1"
  if(!is.null(raw_coordinates)||!is.null(augmentation)) {
    out$correlation_coordinates<-raw_coordinates;out$augmentation<-augmentation
    out$reference<-"explicit_person_correlation_augmentation_v1";out$version<-"reference-law-components-v1"
  }
  if(.bh_has_correlation_declarations(out))out$version<-"affine-correlation-components-v1"
  out$fingerprint <- .mp_hash(out)
  out
}
.bh_subjects <- function(blocks, subjects) {
  if (is.null(subjects)) {
    if (length(blocks) > 1L) .mp_abort("E_SUBJECTS", "multiple blocks require subjects='shared', 'separate', or an explicit crosswalk")
    return(shared_subjects("study"))
  }
  if (inherits(subjects, "measurement_subjects")) return(subjects)
  if (is.data.frame(subjects)) return(subject_crosswalk(subjects))
  if (identical(subjects, "shared")) return(shared_subjects("study"))
  if (identical(subjects, "separate")) {
    rows <- lapply(seq_along(blocks), function(i) {
      ids <- sort(unique(blocks[[i]]$data$.local_id))
      data.frame(model = names(blocks)[i], local_id = ids,
        person_id = paste0("block", match(names(blocks)[i], sort(names(blocks))), "_person", seq_along(ids)), stringsAsFactors = FALSE)
    })
    return(subject_crosswalk(do.call(rbind, rows), "separate"))
  }
  .mp_abort("E_SUBJECTS", "unknown subject alignment declaration")
}
.bh_prepare <- function(formula, data, subject, family, subjects, missing, prior) {
  if (!inherits(formula, "bhmm_formula")) .mp_abort("E_FORMULA", "formula must be a bmf() declaration")
  formula <- .bh_formula_upgrade(formula,warn=TRUE)
  # Rebuild the captured declaration's correlation table dimensions before mapping.
  if(length(formula$components)) .mp_names(formula$components, "components")
  all_targets <- c(unlist(lapply(formula$components,.bh_component_targets),recursive=FALSE), if(is.list(formula$residual))formula$residual$targets else list(),unlist(lapply(formula$decomposition$owners,`[[`,"targets"),recursive=FALSE))
  wide <- is.data.frame(data)
  if (wide) {
    if(is.null(names(data)) || anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data)))
      .mp_abort("E_DATA_COLUMN", "wide data must have distinct nonempty column names")
    .mp_scalar_string(subject, "subject column")
    if (!subject %in% names(data)) .mp_abort("E_DATA_COLUMN", "subject column is absent")
    if (is.null(family)) family <- stats::gaussian()
    if (!is.list(family) || !identical(family$family, "gaussian") || !identical(family$link, "identity"))
      .mp_abort("E_FRONTEND_UNSUPPORTED", "wide data currently use Gaussian identity observations; other families require explicit observe() blocks")
    if (any(vapply(all_targets, function(t) t$kind != "wide", logical(1))))
      .mp_abort("E_TARGET", "wide data use literal response-column names, not block selectors")
    columns <- unique(vapply(all_targets, `[[`, character(1), "column"))
    if(!length(columns)) .mp_abort("E_DATA_COLUMN","an empty bhmf() needs explicit observe() blocks with task/fixed parameter levels; wide data have no selected response columns")
    if (!all(columns %in% names(data)) || subject %in% columns)
      .mp_abort("E_DATA_COLUMN", "selected response columns are absent or include the subject ID")
    if (any(!vapply(data[columns], is.numeric, logical(1)))) .mp_abort("E_FIELD_SUPPORT", "Gaussian response columns must be numeric")
    long <- data.frame(person = rep(data[[subject]], length(columns)), task = rep(columns, each = nrow(data)),
      y = unlist(data[columns], use.names = FALSE), observation = paste0("row", rep(seq_len(nrow(data)), length(columns)), "_target", rep(seq_along(columns), each = nrow(data))))
    blocks <- list(scores = observe(gaussian_model(), long, "person", "task", "y",
      units = "unscaled_response", observation_id = "observation"))
  } else {
    if (!is.list(data)) .mp_abort("E_DATA", "data must be a wide data.frame or named observe() blocks")
    .mp_names(data, "observation blocks")
    if (any(!vapply(data, inherits, logical(1), "observation_block"))) .mp_abort("E_BLOCK", "bind each block using observe()")
    if (!is.null(subject) || !is.null(family)) .mp_abort("E_INPUT_AMBIGUITY", "subject/family are already bound inside observe() blocks")
    if (any(vapply(all_targets, function(t) t$kind != "target", logical(1))))
      .mp_abort("E_TARGET", "explicit blocks use block$parameter[tasks] selectors")
    blocks <- data
  }
  alignment <- .bh_subjects(blocks, subjects)
  .mp_check_resources(blocks)
  .mp_adapter_resources(list(models = blocks))
  exported <- unlist(lapply(blocks,function(b)c(b$model$adapter$function_exports,
    unlist(lapply(b$model$adapter$resources,function(r)r$exports)))),use.names=FALSE)
  if(any(grepl("^(bhmm_|bmeas_)",exported))) .mp_abort("E_SYMBOL_COLLISION","bhmm_ and bmeas_ are reserved for generated frontend helpers")
  policy <- missing_policy(response = missing)
  mapped <- .mp_map_data(blocks, alignment, policy,allow_empty_blocks=TRUE)
  mapped<-.bh_bind_person_roster(mapped,formula,blocks,alignment)
  information <- lapply(names(blocks),function(b) {
    d<-mapped$blocks[[b]];rule<-.bh_information_contract(blocks[[b]]$model)
    informative<-.bh_informative_rows(blocks[[b]]$model,d,rule)
    data.frame(block=rep(b,nrow(d)),observation_id=d$.observation_id,informative=informative,policy=rep(rule,nrow(d)),stringsAsFactors=FALSE)
  })
  partial_exclusions <- list()
  if(identical(missing,"observed_only"))for(b in names(blocks)) {
    response_fields <- names(Filter(function(f)f$role=="response",blocks[[b]]$model$data_schema))
    d <- blocks[[b]]$data
    total <- sum(vapply(blocks[[b]]$model$data_schema[response_fields],function(f)f$width,integer(1)))
    missing_cells <- Reduce(`+`,lapply(d[response_fields],function(x)if(is.matrix(x))rowSums(is.na(x))else as.integer(is.na(x))))
    partial <- missing_cells>0L & missing_cells<total
    if(any(partial))partial_exclusions[[b]] <- data.frame(block=b,observation_id=d$.observation_id[partial],
      missing_response_cells=missing_cells[partial],observed_response_cells_discarded=total-missing_cells[partial],stringsAsFactors=FALSE)
  }
  roles<-.bh_parameter_role_table(blocks,mapped)
  key_fields<-names(.mp_parameter_keys(blocks,mapped))
  keys<-roles[key_fields]
  primary<-roles[roles$role=="predictor" & roles$level!="fixed",,drop=FALSE]
  resolve <- function(parts) vapply(parts, function(t)
    if(t$kind=="wide")targets("scores","mu",t$column)else targets(t$block,t$parameter,t$task),character(1))
  selected <- lapply(formula$components,function(x)resolve(.bh_component_targets(x)))
  residual_selected <- if(is.list(formula$residual))resolve(formula$residual$targets)else character()
  supported <- unique(c(unlist(selected,use.names=FALSE),residual_selected,formula$decomposition$owned$target))
  unknown<-setdiff(supported,roles$key)
  if(length(unknown)).mp_abort("E_PARAMETER_COVERAGE",paste("unknown declared structural targets:",paste(unknown,collapse=", ")))
  nonvarying<-supported[!supported%in%primary$key[primary$level=="subject_task"]]
  if(length(nonvarying)).mp_abort("E_PARAMETER_LEVEL_CONFLICT",paste("structural declarations require subject_task predictor targets; task/fixed/observation-scale keys cannot acquire random effects:",paste(nonvarying,collapse=", ")))
  if(!setequal(supported,primary$key[primary$level=="subject_task"]))
    .mp_abort("E_PARAMETER_COVERAGE","every subject-task parameter must be explicitly declared in a common/basis or residual target set; use an explicit task/fixed level to remove individual variation")
  primary <- primary[match(c(supported,primary$key[primary$level=="task"]),primary$key),,drop=FALSE]
  rownames(primary)<-NULL;primary$target_index<-seq_len(nrow(primary))
  map<-.bh_bind_component_map(formula,selected,primary)
  map<-.bh_apply_loading_constraints(map,formula,selected,primary)
  map<-.bh_apply_affine_orientation(map,formula)
  loads<-map$loadings
  residual_table <- data.frame(target=residual_selected,target_index=match(residual_selected,primary$key),
    residual_index=seq_along(residual_selected),parameter=vapply(residual_selected,function(t).bh_public_id("parameter_sd",t),character(1)),
    units=primary$predictor_units[match(residual_selected,primary$key)],stringsAsFactors=FALSE)
  if(!is.null(formula$decomposition)) {
    residual_table$expression<-ifelse(residual_selected%in%formula$decomposition$owned$target,"share_derived","free_parameter_sd")
    residual_table$parameter[residual_table$expression=="share_derived"]<-NA_character_
  }
  residual_cor <- if(is.list(formula$residual))formula$residual$correlations else .bh_cor_table("independent",character())
  names(residual_cor)[names(residual_cor)=="component1"]<-"target1"
  names(residual_cor)[names(residual_cor)=="component2"]<-"target2"
  residual_cor$target1<-residual_selected[residual_cor$row];residual_cor$target2<-residual_selected[residual_cor$col]
  residual_cor$parameter<-vapply(seq_len(nrow(residual_cor)),function(i)if(residual_cor$state[i]=="free")
    .bh_public_id("parameter_cor",residual_cor$target1[i],residual_cor$target2[i])else NA_character_,character(1))
  residual_cor<-.bh_cor_rekey(residual_cor,"parameter")
  nuisance <- roles[roles$role=="observation_scale" & roles$level!="fixed", , drop = FALSE]
  out <- structure(list(formula = formula, models = blocks, subjects = alignment,
    missing = policy, data_map = mapped, parameter_keys = keys,
    tables = list(targets = primary, parameter_roles=roles,
      fixed_parameters=roles[roles$level=="fixed",,drop=FALSE],
      individual_targets=primary[primary$level=="subject_task",,drop=FALSE],
      components = data.frame(component=formula$coordinates$display,
      variance=rep(1,nrow(formula$coordinates)),mean=rep(0,nrow(formula$coordinates)),
      orientation=vapply(seq_len(nrow(formula$coordinates)),function(k)if(formula$coordinates$kind[k]=="basis")"supplied_basis_signs"else selected[[formula$coordinates$group[k]]][1L],character(1)),stringsAsFactors=FALSE),
      loadings=loads,free_loadings=map$free_loadings,amplitudes=map$amplitudes,
      loading_coefficients=map$loading_coefficients,loading_map=map$loading_map,
      loading_cells=map$loading_cells,loading_primitives=map$loading_primitives,loading_constraint_rules=map$loading_constraint_rules,
      orientation=map$orientation,ordinary_loading_map=map$ordinary_loading_map,ordinary_fixed_matrix=map$ordinary_fixed_matrix,
      component_groups=map$component_groups,coordinates=map$coordinates,basis_rows=map$basis_rows,
      basis_multiplier=map$basis_multiplier,basis_amplitude_index=map$basis_amplitude_index,
      correlations = formula$correlations, parameter_residuals=residual_table,
      parameter_correlations=residual_cor, observation_parameters = nuisance,
      information=do.call(rbind,information),
      exclusions=mapped$excluded,
      partial_response_exclusions=if(length(partial_exclusions))do.call(rbind,partial_exclusions)else data.frame()),
    prior = prior, status = "prepared_scalar_frontend", backend = "not_yet_qualified",
    response_missing_semantics="observed_only excludes a whole declared likelihood row; it never constructs marginal likelihoods from partially observed joint responses",
    observation_key_origin = if (wide) "input-row and selected-column positions; no deduplication" else "explicit observation blocks"), class = c("bmeas_spec","bhmm_spec"))
  if(!is.null(formula$decomposition))out<-.bh_attach_share_tables(out,formula$decomposition)
  if(!is.null(map$orientation_sign_system))out$tables$orientation_sign_system<-map$orientation_sign_system
  out<-.bh_attach_component_scales(out)
  out$tables$response_domains<-.bh_gaussian_responses(out)
  out$tables$observation_operands<-.bh_level_operands(out)
  out$tables$components$orientation<-ifelse(map$orientation$pinned,"fixed_or_derived_pin",ifelse(map$orientation$latent_only,"latent_only",map$orientation$anchor))
  if(!is.null(map$orientation_sign_system))out$tables$components$orientation[is.na(out$tables$components$orientation)]<-"resolved_GF2_character; inspect orientation_sign_system"
  if(nrow(formula$coordinates))rownames(out$tables$components)<-formula$coordinates$display
  out$tables$target_coverage <- data.frame(target=primary$key,declared=rep(TRUE,nrow(primary)),
    nonzero_common=primary$key%in%loads$target,parameter_residual=primary$key%in%residual_selected,
    mean_only=!primary$key%in%c(loads$target,residual_selected),stringsAsFactors=FALSE)
  out<-.bh_attach_reference_tables(out)
  out<-.bh_attach_correlation_tables(out)
  out$tables$priors <- .bh_prior_table(out, prior)
  out$tables$correlation_prior_geometry<-.bh_correlation_prior_geometry(out)
  out$tables$loading_prior_calibration<-attr(out$tables$priors,"loading_calibration")
  out$tables$component_sd_calibration<-attr(out$tables$priors,"component_sd_calibration")
  out$tables$loading_prior_aliases<-attr(out$tables$priors,"loading_aliases")
  out$tables$prior_resolution<-attr(out$tables$priors,"prior_resolution")
  attr(out$tables$priors,"prior_resolution")<-NULL
  out<-.bh_correlation_prior_annotations(out)
  mask <- matrix("zero",nrow(formula$coordinates),nrow(formula$coordinates),dimnames=list(formula$coordinates$display,formula$coordinates$display))
  diag(mask) <- "fixed_one"
  for(i in seq_len(nrow(formula$correlations))) {
    edge <- formula$correlations[i,]
    mask[edge$row,edge$col] <- mask[edge$col,edge$row] <- edge$state
  }
  out$tables$correlation_mask <- mask
  out$tables$parameter_correlation_mask <- .bh_mask(residual_cor,residual_selected)
  ordinary <- out$tables$priors[!out$tables$priors$class%in%c("cor","structural_latent"),c("parameter","class","layer"),drop=FALSE]
  free_component<-if(.bh_raw_cor(out))character()else unique(formula$correlations$parameter[formula$correlations$state=="free"])
  free_parameter<-unique(residual_cor$parameter[residual_cor$state=="free"])
  free_edges<-c(free_component,free_parameter)
  out$tables$parameters <- rbind(ordinary,data.frame(parameter=free_edges,class=rep("correlation_entry",length(free_edges)),layer=c(rep("component",length(free_component)),rep("parameter",length(free_parameter)))))
  rownames(out$tables$parameters) <- NULL
  attr(out$tables$parameters,"scope") <- "unique population distribution parameters; latent-person dimensions are listed separately"
  K<-nrow(formula$coordinates)
  out$tables$latent_coordinates <- data.frame(component=formula$coordinates$display,
    noncentered_symbol=if(K)paste0("z_raw[",seq_len(K),",person_index]")else character(),
    count=rep(length(mapped$subject_keys),K),prior=rep("normal(0,1)",K),
    scientific_score=rep("z=L_component*z_raw; f=component_sd*z",K),stringsAsFactors=FALSE)
  out$tables$loading_coefficients$prior_source<-out$tables$priors$source[match(out$tables$loading_coefficients$parameter,out$tables$priors$parameter)]
  out$tables$parameter_latent_coordinates <- data.frame(target=residual_selected,
    noncentered_symbol=if(length(residual_selected))paste0("e_raw[",seq_along(residual_selected),",person_index]")else character(),
    count=rep(length(mapped$subject_keys),length(residual_selected)),prior=rep("normal(0,1)",length(residual_selected)),
    scientific_score=rep("u=parameter_sd * L_parameter * e_raw, shared per person/target",length(residual_selected)))
  if(!is.null(formula$decomposition))out$tables$parameter_latent_coordinates$scientific_score<-rep("u=row_sd(target)*L_parameter*e_raw; row_sd may be derived from total_sd and share",length(residual_selected))
  if(.bh_task_augmentation(out)) {out$tables$latent_coordinates<-out$tables$latent_coordinates[FALSE,,drop=FALSE];out$tables$parameter_latent_coordinates<-out$tables$parameter_latent_coordinates[FALSE,,drop=FALSE]}
  out$schema_version<-if(.bh_has_correlation_declarations(out))"affine-correlation-spec-v1"else"reference-law-spec-v1"
  if(!is.null(formula$schema_migration))out$schema_migration<-formula$schema_migration
  out$checks <- .bh_checks(out)
  out$fingerprint <- .mp_hash(out)
  out
}

bmeas <- function(formula, data = NULL, subject = NULL, family = NULL, subjects = NULL,
                 prior = NULL, missing = "error", fit = TRUE, chains = NULL,
                 iter = NULL, warmup = NULL, cores = 1L, threads = 1L,
                 seed = 1L, grainsize = 1L, cache_dir = "bhmm-cache", output_dir = "bhmm-draws",
                 control = list(), save_log_lik = FALSE, exploratory = FALSE, execution = "qualified", resources = NULL) {
  .bh_pf_refuse_execution(formula,"bmeas")
  if (!is.logical(fit) || length(fit) != 1L || is.na(fit)) .mp_abort("E_ARGUMENT", "fit must be TRUE or FALSE")
  if (inherits(formula, "bhmm_spec")) {
    if (!is.null(data) || !is.null(subject) || !is.null(family) || !is.null(subjects) || !is.null(prior) || !missing(missing))
      .mp_abort("E_INPUT_AMBIGUITY", "a prepared spec already freezes data, identities, family, priors and missing policy")
    spec <- .bh_normalize_spec(formula)
  } else spec <- .bh_prepare(formula, data, subject, family, subjects, missing, prior)
  if (!fit) return(spec)
  if(!sum(spec$tables$priors$parameter_count)) .mp_abort("E_NO_FREE_PARAMETERS","this prepared model has no unknown parameters to sample; inspect its fixed operands and likelihood specification")
  plan <- .bh_run_plan(spec,chains,iter,warmup,cores,threads,seed,grainsize,control,
    resources=resources,save_log_lik=save_log_lik,exploratory=exploratory,execution=execution)
  if(!plan$scientific_eligibility$allowed) {
    finding<-plan$scientific_eligibility$finding;.mp_abort(finding$code,finding$reason)
  }
  .bh_resource_fit_gate(plan)
  .bh_fit_scalar(spec,plan,cache_dir,output_dir,save_log_lik)
}
.bh_run_csv_sig_figs <- function(control=list()) {
  control<-.bh_resource_named_list(control,"control",c("max_treedepth","adapt_delta","csv_sig_figs"),"E_RUN_CONFIG",allow_empty=TRUE)
  as.integer(.bh_count(if("csv_sig_figs"%in%names(control))control$csv_sig_figs else 17L,
    "csv_sig_figs",1,18,"E_RUN_CONFIG"))
}
.bh_run_plan <- function(spec,chains=NULL,iter=NULL,warmup=NULL,cores=1L,threads=1L,seed=1L,grainsize=1L,control=list(),
                         resources=NULL,save_log_lik=FALSE,exploratory=FALSE,execution="qualified") {
  spec<-.bh_normalize_spec(spec)
  if(is.null(chains)||is.null(iter)||is.null(warmup)) .mp_abort("E_RUN_CONFIG","explicit chains, iter and warmup are required; iter includes warmup. Use fit=FALSE to prepare only and execution_plan() to inspect a run")
  for(name in c("chains","iter","warmup","cores","threads","seed","grainsize"))assign(name,.bh_backend_integer(get(name),name))
  if(warmup>=iter) .mp_abort("E_RUN_CONFIG","iter is total iterations and must exceed warmup")
  control<-.bh_resource_named_list(control,"control",c("max_treedepth","adapt_delta","csv_sig_figs"),"E_RUN_CONFIG",allow_empty=TRUE)
  depth<-.bh_backend_integer(if(is.null(control$max_treedepth))10L else control$max_treedepth,"max_treedepth")
  delta<-if(is.null(control$adapt_delta)).8 else control$adapt_delta
  if(!is.numeric(delta)||!is.null(attributes(delta))||length(delta)!=1L||!is.finite(delta)||delta<=0||delta>=1).mp_abort("E_RUN_CONFIG","adapt_delta must be a plain number between zero and one")
  csv_sig_figs<-.bh_run_csv_sig_figs(control)
  for(name in c("save_log_lik","exploratory"))if(!is.logical(get(name))||!is.null(attributes(get(name)))||length(get(name))!=1L||is.na(get(name)))
    .mp_abort("E_ARGUMENT",paste(name,"must be an explicit scalar logical"))
  if(!is.character(execution)||!is.null(attributes(execution))||length(execution)!=1L||is.na(execution)||!execution%in%c("qualified","experimental"))
    .mp_abort("E_EXECUTION_PROFILE","execution must be qualified or an explicit experimental request")
  policy<-.bh_resource_policy(resources);dimensions<-.bh_resource_dimensions(spec)
  parallel<-min(chains,cores)
  workers<-.bh_count_product(as.double(parallel),as.double(threads),name="simultaneous sampler workers")
  saved<-.bh_count_product(as.double(chains),as.double(iter-warmup),name="configured saved states")
  config<-structure(list(chains=chains,parallel_chains=parallel,threads_per_chain=threads,
    iter_warmup=warmup,iter_sampling=iter-warmup,seed=seed,
    cpu_budget=if(is.null(policy$limits$cpu_budget))workers else policy$limits$cpu_budget,
    simultaneous_workers=workers,requested_cores=cores,max_observations=dimensions$O,max_subjects=dimensions$N,max_saved_draws=saved,
    reduction=if(threads==1L)"serial"else"reduce_sum_static",grainsize=grainsize,max_treedepth=depth,csv_sig_figs=csv_sig_figs,model_budget=NULL,
    kind=policy$kind,wall_time_enforcement="not implemented",memory_enforcement="not implemented"),class="measurement_run_config")
  scientific<-.bh_resource_scientific_plan(spec,exploratory,execution);checks<-scientific$checks
  lowering<-.bh_resource_lowering_support(spec,checks)
  output<-.bh_resource_output_estimate(dimensions,config,save_log_lik,lowering)
  provider_metadata<-.bh_provider_metadata_plan(dimensions,config,output,policy)
  provider_metadata$policy_applicability<-if(is.null(policy$cache))"resource_policy_unresolved"else if(policy$cache=="deferred")
    "expected_deferred_constructor_scope"else"deferred_constructor_scope_not_applied_to_historical_eager_work"
  cache<-.bh_resource_cache_estimate(spec,dimensions,config,policy,output)
  admission<-.bh_resource_admission(dimensions,config,policy,output,cache)
  request<-list(chains=chains,iter=iter,warmup=warmup,cores=cores,threads=threads,seed=seed,grainsize=grainsize,
    control=control,resources=resources,save_log_lik=save_log_lik,exploratory=exploratory,execution=execution)
  plan<-list(version="main_resource_execution_plan_v1",kind=policy$kind,run=config,adapt_delta=delta,
    formula_iterations="iter includes warmup",sampling_started=FALSE,request=request,spec_fingerprint=spec$fingerprint,
    dimensions=dimensions,covariance_coordinates=c(common=dimensions$K,parameter=dimensions$U),sampled_white_coordinates=dimensions$sampled_white_coordinates,
    configured_transitions=.bh_count_product(as.double(chains),as.double(iter),name="configured sampler transitions"),
    concurrency=list(requested_cores=cores,parallel_chains=parallel,threads_per_chain=threads,simultaneous_workers=workers,cpu_budget=policy$limits$cpu_budget,
      scope="Sampler workers only; compiler jobs, BLAS and other process threads are outside this policy"),
    resource_policy=policy,output_estimate=output,cache_estimate=cache,resource_admission=admission,lowering_support=lowering,
    provider_metadata=provider_metadata,
    scientific_eligibility=scientific[c("allowed","finding")],scientific_checks=checks,backend_support=checks$backend$support,
    execution_allowed=isTRUE(admission$allowed)&&isTRUE(scientific$allowed),
    exploratory=exploratory,exploratory_rank_override=isTRUE(exploratory)&&identical(checks$identification$status,"possible_nonidentification"),
    identification=checks$identification,execution_profile=scientific$execution_profile,backend_qualification=checks$backend,
    posterior_propriety=checks$posterior_propriety,posterior_normalizability=checks$posterior_normalizability,gaussian_diagnostics=checks$gaussian_diagnostics,
    postprocessing_support=.bh_resource_consumer_support(dimensions,config,output),
    limitations=c("Planning begins after declaration/data preparation and cannot bound those earlier operations.",
      "Planning uses saved tables plus scientific validation/source-resource checks; no code generation, backend IR, dense person initialization, compiler, sampler or posterior reader is invoked.",
      "Resource admission is independent of scientific support, identification, posterior propriety, native qualification and reliable inference."))
  plan$identity<-.mp_hash(list(spec_fingerprint=spec$fingerprint,request=request,resolved_policy=policy,csv_sig_figs=csv_sig_figs,
    plan_version=plan$version,output_estimator=output$version,cache_estimator=cache$version,consumer_projection=plan$postprocessing_support$version,
    provider_estimator=provider_metadata$version,provider_metadata=provider_metadata))
  structure(plan,class="bmeas_execution_plan")
}
.bh_validate_fingerprint <- function(spec) {
  if (!inherits(spec, "bhmm_spec")) .mp_abort("E_SPEC", "expected a prepared bmeas spec")
  original <- spec; original$fingerprint <- NULL
  if (!identical(spec$fingerprint, .mp_hash(original)))
    .mp_abort("E_SPEC_CHANGED", "prepared context changed or was serialized under an incompatible package build; rebuild from the saved formula, original data and intended priors with fit=FALSE. Never reset the stored fingerprint")
  invisible(TRUE)
}
model_table <- function(object, table = NULL) {
  .bh_pf_refuse_execution(object,"model_table")
  object<-.bh_normalize_spec(object)
  if (is.null(table)) return(object$tables)
  .mp_scalar_string(table, "table")
  if (!table %in% names(object$tables)) .mp_abort("E_ARGUMENT", "unknown model table")
  object$tables[[table]]
}
get_prior <- function(object) {
  if (inherits(object, "measurement_spec")) return(prior_table(object))
  model_table(object, "priors")
}
check_model <- function(object) {
  .bh_pf_refuse_execution(object,"check_model")
  object<-.bh_normalize_spec(object)
  .bh_checks(object)
}
print.bhmm_formula <- function(x, ...) {
  cat("bmeas measurement declaration:", length(x$components), "declared component groups\n")
  cat("Named component formulas define loading membership; overlapping targets add loadings, not data rows.\n")
  invisible(x)
}
print.bhmm_spec <- function(x, ...) {
  x<-.bh_normalize_spec(x)
  cat("Prepared bmeas component model:", nrow(x$tables$targets), "targets,", nrow(x$tables$component_groups), "component groups,",nrow(x$tables$components),"scalar coordinates\n")
  cat("Person law:",x$tables$person_law$distribution,";",x$tables$person_law$roster_size,"roster members; augmentation:",x$tables$augmentation$type,".\n")
  cat("Standardized common-coordinate variances are one;",nrow(.bh_free_component_sd(x)),"explicit free component SDs;",nrow(x$tables$parameter_residuals),"parameter residuals.\n")
  if(.bh_task_augmentation(x))cat(x$tables$augmentation$task_white_coordinates,"task-white basis coordinates; common/residual person scores are unsampled.\n")
  if(!is.null(x$tables$parameter_roles))cat(nrow(x$tables$individual_targets),"declared subject-task targets;",sum(x$tables$task_target_domain$level=="subject_task" & x$tables$task_target_domain$has_symbolic_support),"symbolically active person-effect targets;",sum(x$tables$parameter_roles$role=="predictor"&x$tables$parameter_roles$level=="task"),"free task means and",nrow(x$tables$fixed_parameters),"fixed parameter/task values.\n")
  cat("Correlation constraints apply to final marginal R; common primitive coordinates:",x$tables$correlation_coordinate_system$type,".\n")
  if(nrow(x$tables$partial_response_exclusions))cat("Partial joint responses: whole likelihood rows excluded; observed cells were also discarded. Inspect partial_response_exclusions.\n")
  cat("Posterior normalizability:",x$checks$posterior_normalizability$status,"| likelihood identification:", x$checks$identification$status, "| priors:", x$checks$priors$status, "\n")
  cat("Execution qualification:",x$checks$backend$status,"; preparation does not sample.\n")
  if(nrow(.bh_amplitudes(x)))cat(nrow(.bh_amplitudes(x)),"unique shared amplitudes; supplied basis entries are unchanged.\n")
  invisible(x)
}

# Transitional API aliases preserve the exact callable and safe selector capture.
bhmf <- bmf
bhmm <- bmeas

# New leading classes avoid S3 registration collisions with an installed bhmm.
print.bmeas_formula <- print.bhmm_formula
print.bmeas_spec <- print.bhmm_spec
