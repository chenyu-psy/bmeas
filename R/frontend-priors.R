.bh_prior_distribution <- function(prior) {
  .mp_scalar_string(prior, "prior")
  compact <- gsub("[[:space:]]", "", prior)
  hit <- regmatches(compact, regexec("^([a-z_]+)\\(([0-9eE+.,-]+)\\)$", compact))[[1L]]
  if (length(hit) != 3L) .mp_abort("E_PRIOR_SYNTAX", "use a supported distribution with finite numeric literals; prior strings are never evaluated")
  name <- hit[2L]; value <- suppressWarnings(as.numeric(strsplit(hit[3L], ",", fixed = TRUE)[[1L]]))
  value[!is.na(value)&value==0]<-0
  arity <- c(normal=2L, logistic=2L, student_t=3L, half_normal=1L, half_student_t=3L, lkj=1L)
  if (!name %in% names(arity) || length(value) != arity[[name]] || any(!is.finite(value)))
    .mp_abort("E_PRIOR_SYNTAX", "unsupported prior distribution or numeric arguments")
  if (name %in% c("normal", "logistic") && value[2L] <= 0 ||
      name %in% c("student_t", "half_student_t") && (value[1L] <= 0 || value[3L] <= 0) ||
      name %in% c("half_normal", "lkj") && value[1L] <= 0)
    .mp_abort("E_PRIOR", "prior scale, degrees of freedom and shape must be positive")
  if (name == "half_student_t" && value[2L] != 0) .mp_abort("E_PRIOR", "half-Student-t must have zero location")
  list(distribution=name, values=value, text=compact)
}
set_prior <- function(prior, class, component = NULL, target = NULL, layer = "component", decomposition=NULL, stratum=NULL, term=NULL, coordinate=NULL, block=NULL) {
  class <- match.arg(class, c("Intercept", "loading", "amplitude", "sigma", "sdlog", "observation_sd", "parameter_sd", "component_sd", "cor", "cor_raw", "total_sd", "share"))
  if(!is.character(layer)||length(layer)!=1L||is.na(layer)||!layer%in%c("component","parameter")) .mp_abort("E_PRIOR_SELECTOR","correlation layer must be component or parameter")
  if(class!="cor" && !identical(layer,"component")) .mp_abort("E_PRIOR_SELECTOR","layer selects correlation matrices only; parameter_sd is already a separate class")
  if(!is.null(component)&&(typeof(component)!="character"||is.object(component))) .mp_abort("E_PRIOR_SELECTOR","component selectors must be plain character strings")
  if (!is.null(component)) .mp_scalar_string(component, "component")
  target <- .bh_capture_prior_target(target)
  if(!is.null(block)){if(class!="cor_raw"||typeof(block)!="character"||is.object(block)||length(attributes(block))||!length(block)||anyNA(block)||any(!nzchar(block))||anyDuplicated(block)).mp_abort("E_PRIOR_SELECTOR","block selects distinct destination blocks of cor_raw priors only")}
  if(class=="cor_raw"&&(!is.null(component)||!is.null(target)||layer!="component")).mp_abort("E_PRIOR_SELECTOR","cor_raw uses destination block selectors on the common layer, not component/target/parameter-layer selectors")
  if (!is.null(component) && !class %in% c("loading","amplitude","component_sd")) .mp_abort("E_PRIOR_SELECTOR", "component selector applies to free loadings or one shared amplitude per basis group")
  if (class %in% c("amplitude","component_sd") && !is.null(target)) .mp_abort("E_PRIOR_SELECTOR", "component SDs and shared amplitudes are selected by component, never by a target")
  if (class == "cor" && !is.null(target)) .mp_abort("E_PRIOR_SELECTOR", "correlation density is joint, not one prior per edge")
  selectors<-list(decomposition=decomposition,stratum=stratum,term=term,coordinate=coordinate)
  allowed<-if(class=="share")names(selectors)else if(class=="total_sd")"decomposition"else character()
  if(any(vapply(selectors[setdiff(names(selectors),allowed)],Negate(is.null),logical(1)))) .mp_abort("E_PRIOR_SELECTOR","decomposition/stratum/term/coordinate selectors apply only to their declared share or total-SD roles")
  if(class=="share"&&(!is.null(target)||!is.null(component)||!identical(layer,"component"))) .mp_abort("E_PRIOR_SELECTOR","share coefficients use decomposition/stratum/term/coordinate selectors, not target/component/layer")
  for(n in names(selectors))if(!is.null(selectors[[n]])) {
    x<-selectors[[n]]
    if(typeof(x)!="character"||is.object(x)||length(attributes(x))||!length(x)||anyNA(x)||any(!nzchar(x))||anyDuplicated(x)) .mp_abort("E_PRIOR_SELECTOR",paste(n,"requires distinct plain character values"))
    selectors[[n]]<-as.vector(x)
  }
  d <- .bh_prior_distribution(prior)
  if (class == "loading" && (!identical(d$distribution, "normal") || d$values[1L] != 0))
    .mp_abort("E_PRIOR_ORIENTATION", "scalar loading priors currently require zero-centered normal symmetry for the declared orientation constraints")
  if (class %in% c("sigma", "sdlog", "observation_sd", "parameter_sd", "component_sd", "amplitude", "total_sd") && !d$distribution %in% c("half_normal", "half_student_t"))
    .mp_abort("E_PRIOR", paste(class,"requires a positive half-normal or half-Student-t prior"))
  if (class == "Intercept" && !d$distribution %in% c("normal", "logistic", "student_t"))
    .mp_abort("E_PRIOR", "intercepts accept normal, logistic or Student-t")
  if (class == "cor" && d$distribution != "lkj") .mp_abort("E_PRIOR", "cor uses one constrained determinant density lkj(eta), not independent edge priors")
  if(class=="cor_raw"&&(d$distribution!="normal"||d$values[1L]!=0)).mp_abort("E_PRIOR","cor_raw requires a zero-centered Normal density on each raw B primitive")
  if(class=="share"&&d$distribution!="normal") .mp_abort("E_FRONTEND_UNSUPPORTED","share coefficients currently accept Normal priors on explicit logit-contrast coordinates")
  rule<-list(prior=d,class=class,component=component,target=target,layer=layer)
  if(class%in%c("share","total_sd"))rule<-c(rule,selectors)
  if(class=="cor_raw")rule$block<-block
  structure(list(rule),class="bhmm_prior")
}
c.bhmm_prior <- function(..., recursive = FALSE) {
  x <- list(...)
  if (recursive || any(!vapply(x, inherits, logical(1), "bhmm_prior")))
    .mp_abort("E_PRIOR", "combine only set_prior() objects")
  structure(do.call(base::c, lapply(x, unclass)), class="bhmm_prior")
}
.bh_response_calibration <- function(spec, target) {
  d <- spec$data_map$blocks[[target$model]]
  d <- d[d$.task == target$task, , drop=FALSE]
  name <- .bh_builtin_kind(spec$models[[target$model]]$model)
  if (!name %in% c("gaussian", "lognormal", "binomial")) return(NULL)
  if(!nrow(d))return(NULL)
  if (name == "binomial") return(list(location=0, scale=1, source="documented_logit_profile", n=nrow(d)))
  x <- if (name == "lognormal") log(d$y) else d$y
  m <- stats::median(x); s <- stats::mad(x, center=m)
  method <- "MAD"
  if (!is.finite(s) || s <= 0) { s <- stats::sd(x); method <- "sample_SD_fallback" }
  if (!is.finite(s) || s <= 0) { s <- 1; method <- "one_response_unit_fallback_warning" }
  list(location=m, scale=s, source=paste0("data_calibrated_", method), n=length(x),
    observations=d$.observation_id, input_scale=if(name=="lognormal")"log_response"else"response")
}
.bh_prior_table <- function(spec, prior) {
  if (!is.null(prior) && !inherits(prior, "bhmm_prior")) .mp_abort("E_PRIOR", "frontend priors must come from set_prior(); historical prior objects remain valid only for the historical API")
  targets <- spec$tables$targets; loads <- .bh_free_loadings(spec)
  rows <- list(); add <- function(parameter, class, target=NA_character_, component=NA_character_, text=NA_character_, source="missing", location=NA_real_, scale=NA_real_, layer="component", decomposition=NA_character_,stratum=NA_character_,term=NA_character_,coordinate=NA_character_,block=NA_character_) {
    rows[[length(rows)+1L]] <<- data.frame(parameter=parameter, class=class, target=target,
      component=component, layer=layer, prior=text, source=source, calibration_location=location, calibration_scale=scale, stringsAsFactors=FALSE)
    if(.bh_raw_cor(spec))rows[[length(rows)]]<<-cbind(rows[[length(rows)]],data.frame(block=block,stringsAsFactors=FALSE))
    if(.bh_has_shares(spec))rows[[length(rows)]]<<-cbind(rows[[length(rows)]],data.frame(decomposition=decomposition,stratum=stratum,term=term,coordinate=coordinate,stringsAsFactors=FALSE))
  }
  calibration <- lapply(seq_len(nrow(targets)), function(j) .bh_response_calibration(spec, targets[j,,drop=FALSE]))
  sd_calibration<-.bh_marker_calibration(spec,calibration)
  component_sd<-.bh_free_component_sd(spec)
  for(i in seq_len(nrow(component_sd))) {
    cal<-sd_calibration[i,]
    add(component_sd$parameter[i],"component_sd",component=component_sd$component[i],
      text=if(cal$status!="available")NA_character_ else sprintf("half_normal(%.17g)",cal$proposed_scale),
      source=if(cal$status!="available")cal$status else "marker_target_ratio",scale=cal$proposed_scale)
  }
  for(j in seq_len(nrow(targets))) {
    t <- targets[j,]; cal <- calibration[[j]]; family <- .bh_builtin_kind(spec$models[[t$model]]$model)
    text <- if(is.null(cal))NA_character_ else if(family=="binomial")"normal(0,1.5)"else sprintf("student_t(3,%.17g,%.17g)",cal$location,2.5*cal$scale)
    add(paste0("Intercept:",t$key),"Intercept",t$key,text=text,
      source=if(is.null(cal))"missing"else cal$source,location=if(is.null(cal))NA_real_ else cal$location,scale=if(is.null(cal))NA_real_ else cal$scale,layer="predictor")
  }
  loading_calibration<-list()
  for(l in seq_len(nrow(loads))) {
    cells<-spec$tables$ordinary_loading_map
    members<-cells[cells$state=="free"&!is.na(cells$parameter)&cells$parameter==loads$parameter[l],,drop=FALSE]
    proposals<-lapply(seq_len(nrow(members)),function(i).bh_marker_loading_proposal(calibration[[members$target_index[i]]],members$component_index[i],sd_calibration))
    scales<-vapply(proposals,function(cal)if(is.null(cal))NA_real_ else cal$scale,numeric(1))
    valid<-all(is.finite(scales)&scales>0)
    cal<-if(!valid)NULL else if(nrow(members)==1L)proposals[[1L]]else if(valid)list(scale=max(scales),source="all_members_max_scale_loading_class")else NULL
    add(loads$parameter[l],"loading",loads$target[l],loads$component[l],
      text=if(is.null(cal))NA_character_ else sprintf("normal(0,%.17g)",cal$scale),source=if(is.null(cal))"missing"else cal$source,
      scale=if(is.null(cal))NA_real_ else cal$scale)
    field<-function(name,default) vapply(proposals,function(cal)if(is.null(cal[[name]]))default else cal[[name]],default)
    marked<-!is.na(match(members$component_index,sd_calibration$component_index))
    loading_calibration[[l]]<-data.frame(parameter=loads$parameter[l],cell_id=members$cell_id,target=members$target,
      proposed_scale=scales,source=vapply(proposals,function(cal)if(is.null(cal))"missing"else cal$source,character(1)),
      observations=vapply(proposals,function(cal)if(is.null(cal))""else paste(cal$observations,collapse="; "),character(1)),
      original_target_scale=vapply(seq_len(nrow(members)),function(i){cal<-calibration[[members$target_index[i]]];if(is.null(cal))NA_real_ else cal$scale},numeric(1)),
      original_source=vapply(seq_len(nrow(members)),function(i){cal<-calibration[[members$target_index[i]]];if(is.null(cal))"missing"else cal$source},character(1)),
      marker_component=field("marker_component",NA_character_),marker_value=field("marker_value",NA_real_),
      marker_calibration_scale=field("marker_calibration_scale",NA_real_),marker_sd_proposal=field("marker_sd_proposal",NA_real_),
      marker_log_sd_proposal=field("marker_log_sd_proposal",NA_real_),log_beta_proposal=field("log_beta_proposal",NA_real_),
      proposal_status=vapply(proposals,function(cal)if(!is.null(cal$proposal_status))cal$proposal_status else if(is.null(cal))"missing_target_calibration"else if(!is.finite(cal$scale)||cal$scale<=0)"invalid_target_calibration"else"available",character(1)),
      proposal_reason=vapply(proposals,function(cal)if(!is.null(cal$proposal_reason))cal$proposal_reason else if(is.null(cal))"no target calibration is available"else"unscaled target loading calibration",character(1)),
      policy=if(any(marked))if(nrow(members)==1L)"marker_scaled_singleton"else"all_members_max_after_marker_scaling"else if(nrow(members)==1L)"singleton_unchanged"else"all_members_max_scale_loading_class",stringsAsFactors=FALSE)
  }
  amplitudes<-.bh_amplitudes(spec)
  for(a in seq_len(nrow(amplitudes)))add(amplitudes$parameter[a],"amplitude",component=amplitudes$component[a])
  residuals<-spec$tables$parameter_residuals
  free_residuals<-.bh_free_parameter_sd(spec)
  for(j in seq_len(nrow(free_residuals))) {
    cal<-calibration[[free_residuals$target_index[j]]]
    add(free_residuals$parameter[j],"parameter_sd",free_residuals$target[j],
      text=if(is.null(cal))NA_character_ else sprintf("half_student_t(3,0,%.17g)",2.5*cal$scale),
      source=if(is.null(cal))"missing"else cal$source,scale=if(is.null(cal))NA_real_ else cal$scale,layer="parameter")
  }
  if(.bh_has_shares(spec)) {
    owned<-spec$tables$share_targets
    for(j in seq_len(nrow(owned))) {
      cal<-calibration[[owned$target_index[j]]]
      add(owned$parameter[j],"total_sd",owned$target[j],text=if(is.null(cal))NA_character_ else sprintf("half_student_t(3,0,%.17g)",2.5*cal$scale),
        source=if(is.null(cal))"missing"else cal$source,scale=if(is.null(cal))NA_real_ else cal$scale,layer="predictor",decomposition=owned$owner[j])
    }
    beta<-spec$tables$share_coefficients
    for(j in seq_len(nrow(beta)))add(beta$parameter[j],"share",text="normal(0,1)",source="contrast_coordinate_logistic_normal_default",layer="decomposition",
      decomposition=beta$owner[j],stratum=beta$stratum[j],term=beta$term[j],coordinate=beta$coordinate[j])
  }
  for(j in seq_len(nrow(spec$tables$observation_parameters))) {
    t <- spec$tables$observation_parameters[j,]
    cal <- .bh_response_calibration(spec,t)
    role <- if(t$parameter %in% c("sigma","sdlog")) t$parameter else "observation_sd"
    add(paste0("observation_sd:",t$key),role,t$key,
      text=if(is.null(cal))NA_character_ else sprintf("half_student_t(3,0,%.17g)",2.5*cal$scale),
      source=if(is.null(cal))"missing"else cal$source,scale=if(is.null(cal))NA_real_ else cal$scale,layer="observation")
  }
  if(.bh_raw_cor(spec))for(i in seq_len(nrow(spec$tables$raw_correlation_coordinates))) {r<-spec$tables$raw_correlation_coordinates[i,];add(r$parameter,"cor_raw",block=r$block)}
  if(!.bh_raw_cor(spec)&&any(spec$tables$correlations$state=="free")) add("component_R[unique_free_entries]","cor",text="lkj(2)",source="joint_constrained_determinant_default_eta2")
  if(any(spec$tables$parameter_correlations$state=="free"))add("parameter_R[unique_free_entries]","cor",text="lkj(2)",source="joint_constrained_determinant_default_eta2",layer="parameter")
  if(!.bh_task_augmentation(spec)&&nrow(spec$tables$components))add("z_raw[component,person]","structural_latent",text="normal(0,1)",source="fixed_by_unit_component_structure")
  if(!.bh_task_augmentation(spec)&&nrow(residuals))add("e_raw[target,person]","structural_latent",text="normal(0,1)",source="fixed_by_independent_parameter_residual_layer",layer="parameter")
  if(.bh_task_augmentation(spec)&&length(spec$tables$augmentation$target))add("z_task[target,sum_to_zero_basis]","structural_latent",text=sprintf("normal(0,%.17g)",sqrt(length(spec$data_map$subject_keys)/(length(spec$data_map$subject_keys)-1))),source="explicit_centered_task_white_law",layer="task")
  out <- if(length(rows))do.call(rbind,rows)else data.frame(parameter=character(),class=character(),target=character(),component=character(),layer=character(),prior=character(),source=character(),calibration_location=numeric(),calibration_scale=numeric(),stringsAsFactors=FALSE)
  rownames(out) <- NULL
  aliases<-spec$tables$ordinary_loading_map
  aliases<-aliases[aliases$state=="free",,drop=FALSE]
  prior_view<-out[!out$class%in%"loading",,drop=FALSE]
  if(nrow(aliases)) {
    expanded<-out[match(aliases$parameter,out$parameter),,drop=FALSE]
    expanded$target<-aliases$target;expanded$component<-aliases$component
    prior_view<-rbind(prior_view,expanded)
  }
  rules <- if(is.null(prior))list()else lapply(unclass(prior),function(rule) {
    args<-list(prior=rule$prior$text,class=rule$class,component=rule$component,target=rule$target,layer=rule$layer)
    args<-c(args,rule[intersect(c("decomposition","stratum","term","coordinate","block"),names(rule))])
    verified<-unclass(do.call(set_prior,args))[[1L]]
    if(!identical(verified,rule)).mp_abort("E_PRIOR","prior declaration changed; construct it again with set_prior()")
    if(.bh_raw_cor(spec)&&rule$class=="cor"&&rule$layer=="component").mp_abort("E_PRIOR_COORDINATES","ordered raw coordinates replace the common final-R prior; specify cor_raw instead")
    resolved<-.bh_resolve_prior_rule(rule,spec,prior_view)
    selected_ids<-prior_view$parameter[resolved$selected]
    resolved$selected<-out$parameter%in%selected_ids
    resolved
  })
  # Resolve every rule first: a later intersection rule can resolve overlapping
  # incomparable rules. Traversal order never establishes precedence.
  resolution<-list()
  if(length(rules))for(i in seq_len(nrow(out))) {
    applicable<-which(vapply(rules,function(r)r$selected[i],logical(1)))
    if(!length(applicable))next
    maximal<-applicable[!vapply(applicable,function(b)any(vapply(setdiff(applicable,b),
      function(a).bh_prior_refines(rules[[a]],rules[[b]]),logical(1))),logical(1))]
    resolution[[length(resolution)+1L]]<-data.frame(parameter=out$parameter[i],rule_index=applicable,
      maximal=applicable%in%maximal,prior=vapply(rules[applicable],function(rule)rule$prior$text,character(1)),
      component=vapply(rules[applicable],function(rule)if(is.null(rule$component))"*"else rule$component,character(1)),
      targets=vapply(rules[applicable],function(rule)if(is.null(rule$resolved_targets))"*"else paste(rule$resolved_targets,collapse="; "),character(1)),stringsAsFactors=FALSE)
    distributions<-lapply(rules[maximal],function(rule)rule$prior[c("distribution","values")])
    if(length(maximal)>1L&&!all(vapply(distributions,function(d)identical(d,distributions[[1L]]),logical(1)))) {
      descriptions<-vapply(maximal,function(r)paste0("rule #",r," {class=",rules[[r]]$class,
        ", component=",if(is.null(rules[[r]]$component))"*"else rules[[r]]$component,
        ", targets=",if(is.null(rules[[r]]$resolved_targets))"*"else paste(rules[[r]]$resolved_targets,collapse=","),
        ", prior=",rules[[r]]$prior$text,"}"),character(1))
      .mp_abort("E_PRIOR_CONFLICT",paste("different maximal user prior densities reach",out$parameter[i],"through its aliases:",paste(descriptions,collapse="; "),". Supply a uniquely refining rule or remove/narrow competing aliases; list order is not precedence"))
    }
    out$prior[i]<-if(length(maximal)==1L)rules[[maximal]]$prior$text else paste0(distributions[[1L]]$distribution,"(",paste(sprintf("%.17g",distributions[[1L]]$values),collapse=","),")")
    out$source[i]<-"user"
  }
  out$validation_error<-.bh_resolved_prior_errors(out)
  out$status <- ifelse(is.na(out$prior),"missing",ifelse(!is.na(out$validation_error),"invalid","specified"))
  out$calibration_status<-rep(NA_character_,nrow(out))
  for(i in which(!is.na(out$target))) {
    key<-spec$parameter_keys[match(out$target[i],spec$parameter_keys$key),,drop=FALSE]
    if(nrow(key)&&!is.na(key$model)) {
      d<-spec$data_map$blocks[[key$model]]
      if(!any(d$.task==key$task)&&!is.na(.bh_builtin_kind(spec$models[[key$model]]$model)))
        out$calibration_status[i]<-"not_available_no_observed_responses"
    }
  }
  out$scope <- ifelse(out$class=="structural_latent","latent_person",ifelse(out$class=="cor","joint_correlation","population"))
  out$parameter_count <- rep(1L,nrow(out))
  for(layer in c("component","parameter")) {
    out$parameter_count[out$class=="structural_latent" & out$layer==layer] <-
      (if(layer=="component")nrow(spec$tables$components)else nrow(residuals))*length(spec$data_map$subject_keys)
    out$parameter_count[out$class=="cor" & out$layer==layer] <-
      .bh_cor_primitive_count(if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations)
  }
  if(.bh_task_augmentation(spec))out$parameter_count[out$class=="structural_latent" & out$layer=="task"]<-spec$tables$augmentation$task_white_coordinates
  attr(out,"prior_resolution")<-if(length(resolution))do.call(rbind,resolution)else data.frame()
  attr(out,"loading_calibration")<-if(length(loading_calibration))do.call(rbind,loading_calibration)else data.frame()
  if(nrow(aliases)) {
    aliases$prior<-out$prior[match(aliases$parameter,out$parameter)]
    aliases$prior_source<-out$source[match(aliases$parameter,out$parameter)]
  }
  attr(out,"loading_aliases")<-aliases
  attr(out,"component_sd_calibration")<-sd_calibration
  attr(out,"calibration") <- stats::setNames(calibration,targets$key)
  attr(out,"correlation_measure") <- if(.bh_raw_cor(spec))"common layer: independent Normal raw B primitives, applied prior-coordinate Jacobian zero; geometric B-to-R Jacobian not evaluated/applied. Parameter layer retains its declared final-R measure"else"joint det(R)^(eta-1) relative to unique free final marginal correlations; any chart Jacobian required once; no independent per-edge LKJ densities"
  attr(out,"selection_semantics") <- "selectors apply one density to each unique primitive; declared loading aliases share that primitive, and identical maximal distributions are idempotent once"
  attr(out,"parameterization") <- if(.bh_task_augmentation(spec))"explicit-roster centered task whites; no common/residual white priors or transform determinant; scientific covariance and primitive priors remain separately declared"else"iid standardized unit-correlation scores with explicit component SDs; marker defaults are parameterization-specific and do not transport the old joint loading prior"
  out
}

.bh_resolved_prior_errors <- function(table) {
  vapply(table$prior,function(text) {
    if(is.na(text))return(NA_character_)
    tryCatch({.bh_prior_distribution(text);NA_character_},error=function(e)conditionMessage(e))
  },character(1))
}
