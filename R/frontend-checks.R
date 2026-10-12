.bh_observation_targets <- function(spec, informative=FALSE) {
  result <- list()
  for(b in names(spec$models)) {
    d <- spec$data_map$blocks[[b]]
    if(!nrow(d))next
    info <- .bh_model_role_info(spec$models[[b]]$model)
    if(!is.null(spec$models[[b]]$model$adapter) && identical(spec$models[[b]]$model$adapter$mode,"routed")) {
      bindings <- .mp_resolve_bindings(spec$models[[b]],d,b,require_coverage=is.null(spec$tables$parameter_roles))
      keep <- !is.na(bindings$target_key)
      records <- unique(bindings[keep,c("observation_id","target_key"),drop=FALSE])
      at <- match(records$observation_id,d$.observation_id)
      result[[b]] <- data.frame(block=b,observation=records$observation_id,
        person=d$.person_key[at],target=records$target_key,stringsAsFactors=FALSE)
    } else result[[b]] <- do.call(rbind,lapply(info$primary,function(p)data.frame(block=b,
      observation=d$.observation_id,person=d$.person_key,target=targets(b,p,d$.task),stringsAsFactors=FALSE)))
  }
  out<-if(any(lengths(result)))do.call(rbind,result)else data.frame(block=character(),observation=character(),person=character(),target=character(),stringsAsFactors=FALSE)
  if(informative && !is.null(spec$tables$information)) {
    policy<-spec$tables$information
    pair_key<-function(block,observation)vapply(seq_along(block),function(i).bh_public_id("observation",block[i],observation[i]),character(1))
    at<-match(pair_key(out$block,out$observation),pair_key(policy$block,policy$observation_id))
    if(anyNA(at)).mp_abort("E_INFORMATION","information policy does not cover every likelihood row")
    out<-out[policy$informative[at],,drop=FALSE]
  }
  out
}
.bh_covariance_moments <- function(spec) {
  target_keys <- .bh_gaussian_responses(spec)$key
  obs <- .bh_observation_targets(spec,informative=TRUE)
  obs$index <- match(obs$target,target_keys)
  pairs <- list()
  for(person in unique(obs$person)) {
    counts <- table(obs$index[obs$person==person])
    ids <- as.integer(names(counts))
    if(length(ids)>1L) pairs[[length(pairs)+1L]] <- t(utils::combn(ids,2L))
    repeated <- as.integer(names(counts)[counts>1L])
    if(length(repeated)) pairs[[length(pairs)+1L]] <- cbind(repeated,repeated)
  }
  pairs <- if(length(pairs)) do.call(rbind,pairs) else matrix(integer(),0L,2L)
  if(nrow(pairs)) pairs <- unique(t(apply(pairs,1L,sort)))
  observed<-sort(unique(obs$index[!is.na(obs$index)]))
  out <- data.frame(kind=rep("variance",length(observed)),row=observed,col=observed,stringsAsFactors=FALSE)
  if(nrow(pairs)) out <- rbind(out,data.frame(kind=ifelse(pairs[,1]==pairs[,2],"repeat_covariance","cross_covariance"),
    row=pairs[,1],col=pairs[,2],stringsAsFactors=FALSE))
  # Known constant covariance entries are not observable unknown features.
  # In particular a fully fixed Gaussian block cannot dilute a dimension proof.
  response<-.bh_gaussian_responses(spec)
  varying<-vapply(seq_len(nrow(out)),function(i) {
    moment<-out[i,]
    (moment$kind=="variance"&&response$noise_free[moment$row])||
      .bh_covariance_entry_has_unknown(spec,response$key[moment$row],response$key[moment$col])
  },logical(1))
  out<-out[varying,,drop=FALSE]
  out$key <- paste(out$kind,target_keys[out$row],target_keys[out$col],sep="|")
  rownames(out)<-NULL
  out
}
.bh_covariance_layout <- function(spec) {
  loads<-.bh_free_loadings(spec);cor<-spec$tables$correlations;u<-.bh_free_parameter_sd(spec);uc<-spec$tables$parameter_correlations
  owned<-spec$tables$share_targets;beta<-spec$tables$share_coefficients
  responses<-.bh_gaussian_responses(spec)
  noise_keys<-responses$key[responses$noise_free]
  counts<-c(loading=nrow(loads),component_sd=nrow(.bh_free_component_sd(spec)),amplitude=nrow(.bh_amplitudes(spec)),total_sd=if(is.null(owned))0L else nrow(owned),share=if(is.null(beta))0L else nrow(beta),
    component_cor=.bh_cor_primitive_count(cor),parameter_sd=nrow(u),parameter_cor=.bh_cor_primitive_count(uc),observation_variance=length(noise_keys))
  at<-0L;indices<-lapply(counts,function(n){i<-at+seq_len(n);at<<-at+n;i})
  list(counts=counts,indices=indices,names=c(loads$parameter,.bh_free_component_sd(spec)$parameter,.bh_amplitudes(spec)$parameter,owned$parameter,beta$parameter,if(.bh_raw_cor(spec))spec$tables$raw_correlation_coordinates$parameter else unique(cor$parameter[cor$state=="free"]),u$parameter,
    unique(uc$parameter[uc$state=="free"]),vapply(noise_keys,function(key)paste0("observation_variance:",key),character(1))))
}
.bh_moment_function <- function(spec, moments, values) {
  state<-.bh_covariance_state(spec,values);indices<-.bh_covariance_layout(spec)$indices
  responses<-.bh_gaussian_responses(spec)
  Sigma<-.bh_response_covariance(spec,state)
  out<-Sigma[cbind(moments$row,moments$col)];diagonal<-moments$kind=="variance"
  variance<-responses$fixed_sigma^2
  variance[responses$noise_free]<-values[indices$observation_variance]
  out[diagonal]<-out[diagonal]+variance[moments$row[diagonal]]
  out
}
.bh_gaussian_identification <- function(spec) {
  if(nrow(.bh_gaussian_responses(spec))>60L) return(list(status="not_assessed",
    reason="Gaussian moment screen currently limited to60 targets; no pair table materialized",
    scope="bounded iid Gaussian covariance screen"))
  moments <- .bh_covariance_moments(spec)
  loads <- .bh_free_loadings(spec); cor <- spec$tables$correlations
  layout<-.bh_covariance_layout(spec)
  P <- sum(layout$counts)
  M <- nrow(moments)
  result <- list(status="not_assessed", scope=if(.bh_no_active_person_effect(spec))"Gaussian covariance parameters under the explicit no-active-person-effect certificate; free means remain separate"else"iid Gaussian covariance parameters; free means cancel separately",
    observable_covariance_moments=M, free_covariance_parameters=P, covariance_df=M-P,
    moments=moments, identification_claim="a full-rank local check does not prove global identification")
  feasibility<-.bh_correlation_feasibility(spec)
  if(.bh_has_affine_correlations(spec)&&!identical(feasibility$status,"feasible")){result$reason<-"identification is not assessed before a strict correlation-domain interior is established";result$correlation_feasibility<-feasibility;return(result)}
  if(M<P) {
    result$status <- "proven_unidentified"
    result$reason <- "fewer observable covariance moments than free covariance parameters under the actual observation design"
    return(result)
  }
  if(P==0L) {
    result$status<-"local_rank_full_not_global_proof"
    result$reason<-"no free covariance coordinates; free task means, if present, still have their own likelihood information and priors"
    result$probe_ranks<-rep(0L,3L)
    return(result)
  }
  if(.bh_has_affine_correlations(spec)&&!.bh_affine_probes_available(spec)){result$reason<-"unique-label correlation probes require a resolved one-dimensional interval; multilabel/uncertain affine rank diagnostics remain unassessed";return(result)}
  # Bounded numerical diagnosis at three fixed interior states, without RNG.
  if(P>200L || M>2000L) { result$reason <- "numerical identification screen exceeds the declared diagnostic scope"; return(result) }
  ranks <- integer(3L); minimum <- numeric(3L)
  tolerance_ranks<-matrix(0L,3L,3L,dimnames=list(paste0("probe",1:3),c("relative_1e-6","relative_1e-7","relative_1e-8")))
  null_direction<-probe_parameters<-NULL
  nload <- nrow(loads); ncor <- .bh_cor_primitive_count(cor); J <- nrow(spec$tables$targets); K <- nrow(spec$tables$components)
  for(probe in 1:3) {
    evaluated<-tryCatch({
      scales<-.bh_covariance_scales(spec);values<-.bh_covariance_probe(spec,probe)
      jacobian<-vapply(seq_len(P),function(p) {
        plus<-minus<-values;step<-1e-6*max(1,abs(values[p]/scales[p]))
        plus[p]<-plus[p]+step*scales[p];minus[p]<-minus[p]-step*scales[p]
        if(!is.finite(plus[p])||!is.finite(minus[p])||plus[p]==values[p]||minus[p]==values[p]) .mp_abort("E_STRUCTURE_DIAGNOSTIC","finite-difference step is not representable in original primitive units")
        (.bh_moment_function(spec,moments,plus)-.bh_moment_function(spec,moments,minus))/(2*step)
      },numeric(M))
      list(values=values,scales=scales,jacobian=jacobian)
    },error=identity)
    if(inherits(evaluated,"error")) {result$reason<-paste("deterministic moment probe is not numerically representable:",conditionMessage(evaluated));result$diagnostic_error_code<-evaluated$code;return(result)}
    values<-evaluated$values;scales<-evaluated$scales;jacobian<-evaluated$jacobian
    result$diagnostic_parameter_scales<-scales
    result$diagnostic_coordinate_scope<-"finite differences use normalized diagnostic coordinates; marker SD scaling is inverse absolute marker and each beta class uses the smallest incident absolute marker (one for unit components); share-beta scaling is inverse maximum absolute X-column times centered-C-column effect. Scientific coefficients, designs and priors are unchanged"

    if(any(!is.finite(jacobian))) { result$reason<-"finite-difference moment map overflowed for the supplied basis scale; values were not normalized or changed";return(result) }
    decomposition<-svd(jacobian,nu=0,nv=P);singular <- decomposition$d
    ranks[probe] <- sum(singular > max(singular)*1e-7)
    minimum[probe] <- min(singular)/max(singular)
    tolerance_ranks[probe,]<-vapply(c(1e-6,1e-7,1e-8),function(t)sum(singular>max(singular)*t),integer(1))
    if(ranks[probe]<P && is.null(null_direction)) {
      names<-layout$names
      direction<-decomposition$v[,P]*scales;direction<-direction/max(abs(direction))
      null_direction<-stats::setNames(direction,names)
      probe_parameters<-stats::setNames(values,names)
    }
  }
  result$probe_ranks <- ranks; result$relative_smallest_singular_value <- minimum
  result$tolerance_ranks<-tolerance_ranks
  if(!is.null(null_direction)) {
    result$example_local_null_direction<-null_direction
    result$example_probe_parameters<-probe_parameters
    result$null_direction_scope<-"finite-difference covariance coordinates at one displayed interior probe; a diagnostic direction, not a symbolic/global proof"
  }
  result$status <- if(all(ranks==P))"local_rank_full_not_global_proof"else"possible_nonidentification"
  result$reason <- if(all(ranks==P))"three deterministic interior Jacobians have full column rank; global uniqueness and inference remain unassessed"else"one or more deterministic Jacobians are rank deficient; this numerical warning is not a universal symbolic proof"
  result
}
.bh_reference_kind <- function(model) {
  kind<-.bh_reference_density_kind(model)
  if(!kind%in%c("m3_cs_reference","sdm_reference"))return(NA_character_)
  canonical<-switch(kind,m3_cs_reference=m3_cs_model,sdm_reference=sdm_reference_model)(model$adapter$resources[[1L]]$file)
  fields<-c("version","required_units","reference_parameter_hash","capabilities","parameters","likelihood")
  if(!identical(model[fields],canonical[fields]))return(NA_character_)
  kind
}
.bh_canonical_cognitive_composition <- function(spec) {
  if(length(spec$models)!=2L||nrow(spec$tables$observation_parameters)!=0L)return(FALSE)
  kinds<-vapply(spec$models,function(b).bh_reference_kind(b$model),character(1))
  if(anyNA(kinds)||!setequal(kinds,c("m3_cs_reference","sdm_reference")))return(FALSE)
  for(b in names(kinds)) {
    # Canonical support checks cannot be weakened by a replacement callback.
    if(kinds[[b]]=="m3_cs_reference").mp_validate_m3_cs(spec$data_map$blocks[[b]])else .mp_validate_sdm_runs(spec$data_map$blocks[[b]])
  }
  TRUE
}
.bh_cognitive_direct_qualified <- function(spec) {
  if(.bh_has_shares(spec)||nrow(spec$tables$components)!=0L||nrow(spec$tables$targets)!=6L||nrow(spec$tables$parameter_residuals)!=6L||
    sum(spec$tables$parameter_correlations$state=="free")!=15L||!.bh_canonical_cognitive_composition(spec))return(FALSE)
  all(vapply(names(spec$models),function(b)length(unique(spec$tables$targets$task[spec$tables$targets$model==b]))==1L,logical(1)))
}
.bh_checks <- function(spec) {
  .bh_information_policy_assert(spec)
  obs <- .bh_observation_targets(spec,informative=TRUE)
  prior_errors<-.bh_resolved_prior_errors(spec$tables$priors)
  invalid_priors<-data.frame(parameter=spec$tables$priors$parameter,prior=spec$tables$priors$prior,validation_error=unname(prior_errors),stringsAsFactors=FALSE)
  invalid_priors<-invalid_priors[!is.na(invalid_priors$validation_error),,drop=FALSE]
  loads <- spec$tables$loadings; correlations <- spec$tables$correlations
  people <- lapply(seq_len(nrow(spec$tables$components)),function(k)unique(obs$person[obs$target %in% loads$target[loads$component_index==k]]))
  free <- which(correlations$state=="free")
  unsupported_edges <- free[vapply(free,function(e)length(intersect(people[[correlations$row[e]]],people[[correlations$col[e]]]))==0L,logical(1))]
  unsupported_edges<-.bh_correlation_uninformed_edges(correlations,unsupported_edges)
  gaussian <- all(vapply(spec$models,function(b)identical(.bh_builtin_kind(b$model),"gaussian"),logical(1)))
  diagnostic_scope<-.bh_gaussian_diagnostic_scope(spec)
  person_eligible<-identical(spec$formula$person,"iid")||.bh_no_active_person_effect(spec)
  diagnostic_scope$person_law_certificate<-if(identical(spec$formula$person,"iid"))"iid"else if(.bh_no_active_person_effect(spec))"no_active_person_effect"else"centered_active_person_law_not_assessed"
  diagnostic_gaussian<-isTRUE(diagnostic_scope$eligible)&&person_eligible
  identification <- if(diagnostic_gaussian).bh_gaussian_identification(spec) else list(status="not_assessed",
    reason=if(!person_eligible)"iid observable-moment/no-overlap/no-repeat arguments do not apply unchanged to the centered active-person law"else"Gaussian covariance-moment identification rules do not establish identification of a cognitive or mixed likelihood")
  U<-nrow(spec$tables$parameter_residuals);uc<-spec$tables$parameter_correlations
  J<-if(.bh_task_augmentation(spec))length(spec$tables$augmentation$target)else if(is.null(spec$tables$individual_targets))nrow(spec$tables$targets)else nrow(spec$tables$individual_targets)
  q<-min(J,nrow(spec$tables$components)+U)
  latent_bound<-J*q-q*(q-1)/2
  layout<-.bh_covariance_layout(spec)
  latent_parameters<-sum(layout$counts)-layout$counts[["observation_variance"]]
  identification$latent_covariance_dimension_bound<-latent_bound
  identification$latent_covariance_parameters<-latent_parameters
  if(latent_parameters>latent_bound) {
    identification$status<-"proven_unidentified"
    identification$reason<-"more independent latent-covariance parameters than the rank-aware Gaussian covariance image can contain; proper priors do not remove this likelihood equivalence"
  }
  if(length(unsupported_edges)&&! .bh_centered_person(spec)) {
    identification$status <- "proven_unidentified"
    identification$reason <- "free component correlations have no joint person-level likelihood information in this iid design"
    identification$uninformed_correlations <- correlations$parameter[unsupported_edges]
  }
  ufree<-which(uc$state=="free")
  unobserved_u<-ufree[vapply(ufree,function(e)length(intersect(obs$person[obs$target==uc$target1[e]],
    obs$person[obs$target==uc$target2[e]]))==0L,logical(1))]
  unobserved_u<-.bh_correlation_uninformed_edges(uc,unobserved_u)
  if(length(unobserved_u)&&! .bh_centered_person(spec)) {
    identification$status<-"proven_unidentified"
    identification$reason<-"free parameter-residual correlations have no jointly informative person at their target endpoints"
    identification$uninformed_parameter_correlations<-uc$parameter[unobserved_u]
  }
  if(diagnostic_gaussian && U && !.bh_is_affine_cor_table(uc) && !is.null(identification$moments)) {
    response<-.bh_gaussian_responses(spec)
    repeated<-response$key[identification$moments$row[identification$moments$kind=="repeat_covariance"]]
    free_u<-.bh_free_parameter_sd(spec)
    free_noise<-response$key[response$noise_free]
    unseparated<-free_u$target[!free_u$target%in%repeated & free_u$target%in%free_noise]
    if(length(unseparated)) {
      identification$status<-"proven_unidentified"
      identification$reason<-"parameter-residual SD and separate free Gaussian observation SD cannot be separated at targets without a within-person repeat; rescaling residual SD/correlations and compensating observation variance preserves the likelihood"
      identification$unseparated_parameter_observation_noise<-unseparated
      identification$confounding_scope<-"iid Gaussian, separate free parameter and observation SDs, zero/free residual correlations; not a rule for fixed/shared SDs or sample-centered people"
    }
  }
  invisible_edges<-free[vapply(free,function(e)!length(people[[correlations$row[e]]])||!length(people[[correlations$col[e]]]),logical(1))]
  invisible_edges<-.bh_correlation_uninformed_edges(correlations,invisible_edges)
  if(length(invisible_edges)){identification$status<-"proven_unidentified";identification$reason<-"a free common correlation is incident to a coordinate with no informative likelihood support";identification$uninformed_correlations<-correlations$parameter[invisible_edges]}
  empty_targets<-setdiff(spec$tables$targets$key,unique(obs$target))
  if(length(empty_targets)) {
    identification$status<-"proven_unidentified"
    identification$reason<-"declared target parameters have no informative likelihood rows under the qualified constant-row policy"
    identification$uninformed_targets<-empty_targets
  }
  if(!is.null(spec$tables$parameter_roles)) {
    operands<-spec$tables$observation_operands
    policy<-spec$tables$information
    row_key<-function(block,id)paste(nchar(block),block,nchar(id),id,sep=":")
    at<-match(row_key(operands$block,operands$observation_id),row_key(policy$block,policy$observation_id))
    used<-unique(operands$target[policy$informative[at]])
    roles<-spec$tables$parameter_roles
    uninformed<-roles$key[roles$free_parameter & !roles$key%in%used]
    if(length(uninformed)) {
      identification$status<-"proven_unidentified"
      identification$reason<-"free predictor/observation-scale unknowns have no informative likelihood operands after exclusion and constant-row screening"
      identification$uninformed_parameters<-uninformed
    }
  }
  share_failure<-.bh_share_identification(spec,diagnostic_gaussian,identification$moments)
  if(!is.null(share_failure)){identification$status<-"proven_unidentified";identification$reason<-share_failure$reason;identification$share_nonidentification<-share_failure}
  basis_failure<-.bh_basis_identification(spec)
  if(!is.null(basis_failure)) { identification$status<-"proven_unidentified";identification$reason<-basis_failure$reason;identification$basis_nonidentification<-basis_failure }
  marker_failure<-.bh_marker_identification(spec)
  if(!is.null(marker_failure)){identification$status<-"proven_unidentified";identification$reason<-marker_failure$reason;identification$marker_nonidentification<-marker_failure}
  permutation_failure<-.bh_loading_permutation_identification(spec)
  if(!is.null(permutation_failure)) {identification$status<-"proven_unidentified";identification$reason<-permutation_failure$reason;identification$loading_permutation_nonidentification<-permutation_failure}
  affine_alias<-.bh_affine_correlation_alias(spec)
  if(!is.null(affine_alias)){identification$status<-"proven_unidentified";identification$reason<-affine_alias$reason;identification$affine_correlation_alias<-affine_alias}
  feasibility<-.bh_correlation_feasibility(spec)
  if(.bh_has_affine_correlations(spec)&&!identical(feasibility$status,"feasible")){identification$status<-"not_assessed";identification$reason<-"strict correlation-domain feasibility has not been established; no valid-model identification claim is made"}
  if(diagnostic_gaussian&&!isTRUE(diagnostic_scope$positive_factors_certified)) {
    if(identification$status=="proven_unidentified")identification$constant_factor_scope<-"the Gaussian observational-equivalence witness is unchanged by fixed factors; the opaque observation density's value and validity are not certified"else {
      identification$gaussian_factor_status<-identification$status
      identification$status<-"not_assessed"
      identification$reason<-"only the Gaussian factor was assessed; an added opaque fixed density has no certified finite positive value"
    }
  }
  fit_capable<-all(vapply(spec$models,function(b){tab<-b$model$capabilities;flag<-tab$available[tab$capability=="fit"];length(flag)==1L&&isTRUE(flag)},logical(1)))
  support<-.bh_backend_support(spec)
  correlation_lowering<-.bh_affine_lowering_capability(spec)
  product_source_only<-any(vapply(correlation_lowering$layers,function(p)identical(p$mode,3L),logical(1)))
  backend_status<-if(!isTRUE(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1")))"prepared_schema_requires_rebuild"else if(!fit_capable)"fit_capability_unavailable"else if(support$status!="supported_typed_ir")"observation_contract_unsupported"else if(.bh_centered_final_marginal_mode(spec))if(!.bh_final_correlation_metadata_available(spec))"centered_final_marginal_requires_rebuild"else if(.bh_has_affine_correlations(spec)&&correlation_lowering$status!="bounded_source_available")"affine_correlation_not_lowered"else"experimental_centered_task_final_marginal"else if(.bh_has_affine_correlations(spec))if(correlation_lowering$status!="bounded_source_available")"affine_correlation_not_lowered"else if(!.bh_affine_generator_qualified())"affine_correlation_not_runtime_qualified"else if(!.bh_affine_runtime_supported(spec))"affine_combination_not_runtime_qualified"else if(.bh_reference_mode(spec))"experimental_centered_task_raw_affine_residual"else"experimental_affine_correlations"else if(.bh_reference_mode(spec)) {if(!.bh_reference_lowering_available())"reference_law_not_lowered"else if(!.bh_reference_generator_qualified())"reference_law_not_runtime_qualified"else if(!.bh_reference_runtime_supported(spec))"reference_combination_not_runtime_qualified"else"experimental_centered_task_raw"}else if(!.bh_coefficient_generator_qualified())"coefficient_generator_not_runtime_qualified"else if(length(spec$models)==1L&&gaussian)"deterministic_gaussian_qualified"else if(.bh_cognitive_direct_qualified(spec))"deterministic_cognitive_direct_qualified"else if(.bh_canonical_cognitive_composition(spec))if(U==0L&&nrow(spec$tables$components)>0L)"experimental_cognitive_common_only"else"experimental_cognitive_components"else"experimental_typed_composition"
  product_prerequisites<-isTRUE(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1"))&&fit_capable&&identical(support$status,"supported_typed_ir")
  product_runtime<-product_source_only&&product_prerequisites&&.bh_affine_product_runtime_supported(spec)
  if(product_source_only&&product_prerequisites)backend_status<-if(product_runtime)"experimental_iid_gaussian_affine_product"else"affine_product_not_runtime_qualified"
  backend_reason<-switch(backend_status,
    experimental_iid_gaussian_affine_product="explicit experimental iid canonical Gaussian common and/or residual product correlation layers; bounded native density/gradient arithmetic and live cache controls only. Qualified execution, product saved export, general connected multi-label affine support, centered/task, cognitive/custom product combinations, HMC and recovery remain unqualified",
    affine_product_not_runtime_qualified="exact zero-separated one-label product source and R chart only; no native arithmetic, sampling or inferential qualification; execution remains closed",
    centered_final_marginal_requires_rebuild="this historical prepared context remains available for scientific inspection/code generation but lacks structured correlation primitive/cell maps; explicitly rebuild from the original data, formula and intended priors before using the new final-marginal reporting/runtime path. Its sealed bytes are not rewritten",
    experimental_centered_task_final_marginal="explicit sample-centered roster, task-Cholesky augmentation and ordinary zero/free final-marginal common MGS coordinates, with residual MGS, fully fixed or one-label affine maps; bounded Gaussian and pinned CS/SDM states were checked, while the caller's graph, identification, recovery and HMC are not automatically qualified",
    affine_correlation_not_lowered=correlation_lowering$reason,
    affine_correlation_not_runtime_qualified="fully fixed/one-label affine source and data are inspectable; the amended engine has no released native/runtime gate",
    affine_combination_not_runtime_qualified="this affine map does not use a qualified scientific mode: iid/common-plus-residual final-marginal coordinates, or sample-centered/task-Cholesky with ordered raw common coordinates or ordinary zero/free final-marginal common coordinates and a separate fixed or one-label residual layer; its legal source support does not grant execution",
    experimental_centered_task_raw_affine_residual="explicit sample-centered roster, task-Cholesky augmentation, ordered raw-B common priors and a separate fully fixed or one-label affine residual correlation layer; bounded Gaussian and pinned CS/SDM states were checked, while the caller's graph, identification, recovery and HMC are not automatically qualified",
    experimental_affine_correlations="typed iid/common-plus-residual final-marginal correlations with fully fixed or one-label affine layers; explicit experimental execution is required, and bounded arithmetic evidence is not a claim of exact-composition, HMC or inferential qualification",
    experimental_centered_task_raw="explicit sample-centered roster, task-Cholesky augmentation and ordered raw-B priors; bounded Gaussian and exact synthetic M3 reference states were checked, but the caller's graph, identification, recovery and HMC are not automatically qualified",
    reference_combination_not_runtime_qualified="this distinct person/correlation/augmentation combination has R/IR and source support but no runtime profile in this increment; it is not a scientific-invalidity claim",
    reference_law_not_runtime_qualified="the explicit centered/task/raw-correlation generator is available for code/data inspection; its separate native source/state gate has not completed, and execution cannot be enabled by experimental/exploratory flags",
    reference_law_not_lowered="explicit centered/task/raw-correlation R/IR is inspectable, but its unified lowering and native qualification are pending; experimental/exploratory flags cannot enable execution",
    prepared_schema_requires_rebuild="this historical prepared schema remains inspectable but must be explicitly rebuilt from its original data/formula/intended priors before the parameter-level compiler; retained missing domains can change unknowns",
    coefficient_generator_not_runtime_qualified="the unified marker-scale/affine-loading/shared-amplitude/variance-share generator is under development and deterministic qualification; this execution gate includes ordinary-only controls and cannot be bypassed by exploratory or experimental flags",
    deterministic_gaussian_qualified="bounded Gaussian marker-scale and affine-cell checks cover declared/effective loadings, unique SD and coefficient priors, exact fixed/equal maps, signed markers and old ordinary/basis/share/K0/parameter-level replay; fixed states do not establish broad HMC or scientific validity",
    deterministic_cognitive_direct_qualified="bounded pinned M3-CS+SDM six-target direct predictor covariance passed original row-likelihood, prior, all-gradient and matrix checks with stanc O0; cognitive identification and recovery remain unassessed",
    experimental_cognitive_common_only="canonical M3-CS+SDM with common-only loading expressions; bounded neutral membership/basis/share states were checked, but arbitrary graphs and cognitive identification are not generally qualified",
    experimental_cognitive_components="canonical M3-CS+SDM with common/parameter-residual/variance-share expressions; only bounded fixed states were qualified, so other graphs and tasks require explicit experimental execution and carry no cognitive identification or recovery claim",
    experimental_typed_composition="the typed observation and component IR can lower this composition; exact configurations are not automatically matched to retained fixtures and require explicit experimental execution; scientific and inferential qualification remain unassessed",
    observation_contract_unsupported=support$reason,
    fit_capability_unavailable="at least one observation model explicitly disables fitting; preparation remains inspectable",
    "this declared composition has not completed native numerical qualification; this is not a scientific-invalidity or nonidentification claim")
  list(structure=list(status="computed",reason="common/residual target membership and final marginal correlation constraints resolved"),
    correlation_feasibility=feasibility,
    correlation_lowering=correlation_lowering,
    correlation_prior_geometry=spec$tables$correlation_prior_geometry,
    identification=identification,
    priors=list(status=if(nrow(invalid_priors))"invalid"else if(any(spec$tables$priors$status=="missing"))"incomplete"else"complete",
      invalid=invalid_priors,
      missing=spec$tables$priors$parameter[spec$tables$priors$status=="missing"]),
    gaussian_diagnostics=diagnostic_scope,
    posterior_propriety=.bh_gaussian_propriety(spec,diagnostic_scope),
    posterior_normalizability=.bh_posterior_normalizability(spec,diagnostic_scope),
    numerical_qualification=.bh_numerical_policy(spec),
    initialization=.bh_initialization(spec),
    backend=list(status=backend_status,reason=backend_reason,support=support,
      qualification=list(status=if(backend_status%in%c("deterministic_gaussian_qualified","deterministic_cognitive_direct_qualified"))"bounded_density_profile"else"exact_composition_unqualified",
        scope="support for the typed IR is separate from the retained fixed-state evidence; no arbitrary model inherits native, recovery, HMC or predictive validation"),
      evidence_version=if(product_runtime)if(.bh_affine_layer_plan(spec,"parameter")$mode==3L)"bmeas_iid_gaussian_residual_dual_product_fixed_states_v1"else"bmeas_iid_gaussian_common_product_fixed_states_v1"else if(product_source_only)"affine_product_source_only_unqualified"else if(.bh_centered_final_marginal_mode(spec))"bmeas_centered_task_final_marginal_fixed_states_v1"else if(.bh_has_affine_correlations(spec)&&.bh_reference_mode(spec))if(.bh_affine_runtime_supported(spec))"bmeas_centered_task_raw_affine_residual_fixed_states_v1"else"reference_affine_combination_unqualified"else if(.bh_affine_engine(spec))"bmeas_affine_correlation_fixed_states_v1"else if(.bh_reference_mode(spec))"bhmm_centered_task_raw_fixed_states_v1"else"bhmm_marker_scale_fixed_states_v1",evidence_scope=if(product_runtime&&.bh_affine_layer_plan(spec,"parameter")$mode==3L)"Five iid canonical Gaussian residual-product/common-mode0/1/2/3 frontend configurations passed 20 finite density states, 816 named gradients, 15 target contrasts, 320 normalized likelihood rows and two expected saturation refusals using the unchanged effective-O0 executable. Saved-output comparison checked 10225 values/statuses with maximum error 7.82e-14. The original comparator expected transformed fields absent from generate_quantities CSVs and was corrected without native reruns or tolerance changes. Direct transformed R/L/D/u/eta/logJ outputs were not observed; full emitted covariances, component scores, normalized likelihoods and all frontend gradients were checked. Prior chart probes retain their own bounded evidence. This supports arithmetic and live cache use only, not HMC, recovery, identification or exact-composition qualification."else if(product_runtime)"Two effective-O0 compilation units passed 24 finite log-probability states including all 26 frontend gradients, two expected chart-saturation rejections and 24 deterministic generated-quantity rows; 2440 comparisons with maximum error 6.40e-13. This is bounded arithmetic evidence, not HMC, recovery or exact-composition qualification. The complete native frontend in this evidence covers iid Gaussian common-product coordinates; residual/dual product execution carries its own separate evidence version."else if(product_source_only)"No native or inference evidence is inherited by the new Cartesian-product affine chart. Old fixed/one-label and MGS evidence remains limited to those routes."else if(.bh_centered_final_marginal_mode(spec))"14 bounded centered/task ordinary final-marginal common configurations with residual MGS, fixed or one-label affine maps: 412 primary derivatives, 264 normalized rows, 10684 scalar outputs and 194 empty fields matched independent oracles. Two same-state reductions matched 81 repeated derivatives; the empty-common same-law old native bridge matched exactly. Maximum derivative error 2.66e-15 and normalized-row error 5.33e-15. The S17 zero-row likelihood derivative remained nonzero. Actual stanc --O0 and model-TU C++ -O0 were verified. This is fixed-state arithmetic evidence, not generic identification, posterior propriety, recovery, HMC or exact-composition qualification. Common R has a joint final-marginal density and MGS vector chart, distinct from the ordered raw-B Normal prior law. Static zero-log support permits consistent cancellation residues; task-major white coordinates remain distinct from gradient CSV display labels."else if(.bh_has_affine_correlations(spec)&&.bh_reference_mode(spec))if(.bh_affine_runtime_supported(spec))"14 bounded centered/task/ordered raw common configurations with a separate fixed or one-label affine residual layer: 330 primary derivatives, 234 normalized rows, 7754 scalar outputs and 160 empty fields matched independent oracles. Two same-state reductions matched 58 repeated derivatives exactly; old reference state replays passed. Maximum new derivative error 2.99e-15 and row error 4.45e-15. Actual stanc --O0 and model-TU C++ -O0 were verified. This is fixed-state arithmetic evidence, not general identification, posterior propriety, recovery, HMC or exact-composition qualification. Common R is derived from raw B, with task-major packed white coordinates distinct from gradient CSV display labels."else"This reference/affine combination has no matching qualified runtime body; separate iid-affine or centered/task/raw evidence does not establish this composition."else if(.bh_affine_engine(spec))"32 bounded iid/common-plus-residual configurations: 766 primary named gradients, 594 normalized rows, 10 exact prior-runtime state replays and two same-state reductions passed independent oracles; maximum discrepancy 2.03e-13. Both fixed and one-label affine layers and retained MGS/basis/share/marker/K0/CS-SDM roles are covered. Two zero-unknown configurations use normalized generated likelihood only. Actual stanc --O0 and model-TU C++ -O0 were verified. This is arithmetic evidence, not HMC, recovery, global identification or exact-composition qualification. The separately qualified centered/task/ordered raw common plus affine residual body has its own evidence; other reference combinations are not covered here."else if(.bh_reference_mode(spec))"20 bounded configurations/1794 AD values: exact original and generic M3 paths at two synthetic8-person72-row states, plus16 small Gaussian configurations. All798 original/generic keyed derivatives agree, and all1794 values match independent oracles (maximum ordinary absolute derivative error5.684e-14). Includes zero-parameter likelihood-only and tiny-scale mechanics controls; not inference, recovery or HMC qualification. The gradient CSV-label caveat is resolved by an explicit packed semantic map. Other mode combinations have no runtime profile."else"24 bounded fixed states,438 named gradients and4088 generated values passed independent Gaussian/original CS-SDM oracles under stanc O0. Includes signed markers, mixed unit/marked columns, shared marker classes, fixed/equal beta maps, four Gaussian old-state replays, full U6 cognitive direct replay and keyed a-only residual/marked correspondence. Maximum discrepancy4.53e-9; no HMC, recovery or general inferential equivalence claim",
      additional_evidence=list(version="controlled_normal_multiblock_fixed_states_v1",states=2L,named_gradient_coordinates=44L,generated_values=400L,
        source_sha256="ed835f261ba480bce4c183e7f19a5c51e97379fbd1b0cc7c800bb728786cf609",
        scope="self-authored normalized Normal wrapper plus two canonical Gaussian blocks, one shared scalar component,4 synthetic people/24 rows; all analytic gradients and same-binary grain3/two-thread control checked. Not arbitrary custom, three-family, HMC or recovery qualification",
        native_fixture_matching="not_automatically_matched_to_the_callers_configuration",source_status="historical_previous_generator_fixture; not a new exact-composition run"),
      compiler_policy="retained current bodies verified sole actual stanc --O0 and model-translation-unit C++ -O0; compilation removes inherited optimization-level switches before requesting O=0, with environment restoration and a separate cache identity. Linked libraries/linker settings remain separate, and the historical S19/S20 effective C++ level was unverified",
      limitations="fixed-state qualification only, not broad HMC, boundary support or usable inference"))
}
.bh_numerical_policy <- function(spec) {
  rows<-spec$tables$priors[spec$tables$priors$class=="cor",,drop=FALSE]
  eta<-if(nrow(rows))stats::setNames(vapply(rows$prior,function(p).bh_prior_distribution(p)$values[1L],numeric(1)),rows$layer)else numeric()
  list(status=if(any(eta<1))"execution_unqualified_prior"else"interior_only_not_HMC_qualified",
    eta=eta,reason=if(any(eta<1))
      if(.bh_has_affine_correlations(spec))"eta<1 restricted determinant kernels may be proper, improper or unassessed depending on the affine geometry; inspect correlation_prior_geometry. Execution is unqualified and no value is changed"else "eta<1 is a mathematically valid prior, but this numerical implementation has not qualified its boundary mass; declaration and code inspection remain available"
      else "finite precision near singularity can limit evaluations or stored-matrix use; no artificial global correlation cutoff is imposed")
}
.bh_execution_gate <- function(spec,exploratory=FALSE,execution="qualified") {
  if(!is.character(execution)||length(execution)!=1L||is.na(execution)||!execution%in%c("qualified","experimental")) .mp_abort("E_EXECUTION_PROFILE","execution must be qualified or an explicit experimental request")
  if(!is.logical(exploratory)||length(exploratory)!=1L||is.na(exploratory)).mp_abort("E_ARGUMENT","exploratory must be explicit TRUE/FALSE")
  .bh_affine_correlation_lowering_gate(spec)
  checks<-check_model(spec)
  if(identical(checks$posterior_propriety$status,"proven_improper")) .mp_abort("E_POSTERIOR_IMPROPER",paste(checks$posterior_propriety$reason,"Inspect check_model()$posterior_propriety$findings; data and priors were not changed"))
  if(identical(checks$posterior_propriety$status,"proven_non_normalizable")) .mp_abort("E_POSTERIOR_NON_NORMALIZABLE",checks$posterior_propriety$reason)
  if(checks$numerical_qualification$status=="execution_unqualified_prior") .mp_abort("E_NUMERICAL_PRIOR_UNQUALIFIED",checks$numerical_qualification$reason)
  experimental_available<-checks$backend$status%in%c("experimental_cognitive_common_only","experimental_cognitive_components","experimental_typed_composition","experimental_centered_task_raw","experimental_affine_correlations","experimental_centered_task_raw_affine_residual","experimental_centered_task_final_marginal","experimental_iid_gaussian_affine_product")
  if(experimental_available && execution!="experimental") .mp_abort("E_EXECUTION_PROFILE","this supported typed composition has only an experimental execution profile. Use execution='experimental' explicitly if that limited evidence is acceptable; inspect check_model() first")
  if(!checks$backend$status%in%c("deterministic_gaussian_qualified","deterministic_cognitive_direct_qualified","experimental_cognitive_common_only","experimental_cognitive_components","experimental_typed_composition","experimental_centered_task_raw","experimental_affine_correlations","experimental_centered_task_raw_affine_residual","experimental_centered_task_final_marginal","experimental_iid_gaussian_affine_product")) .mp_abort("E_FRONTEND_BACKEND_UNSUPPORTED",checks$backend$reason)
  .bh_initialization_gate(checks$initialization)
  checks$execution_profile<-list(requested=execution,used=if(experimental_available)checks$backend$status else"qualified",
    reason=checks$backend$reason,support=checks$backend$support,qualification=checks$backend$qualification)
  if(experimental_available)warning(structure(list(message=paste("Experimental execution:",checks$backend$reason,
    "No tested demo graph is being imposed as a scientific-validity condition; this specific configuration may lack native/inferential qualification."),
    call=NULL,code="W_EXPERIMENTAL_EXECUTION"),class=c("measurement_warning","warning","condition")))
  custom_callbacks<-vapply(checks$backend$support$blocks,function(b)!is.null(b$callback_dependencies)&&startsWith(b$callback_dependencies,"caller_managed"),logical(1))
  if(any(custom_callbacks))warning(structure(list(message=paste("Custom callback dependencies for",paste(names(custom_callbacks)[custom_callbacks],collapse=", "),
    "remain caller-managed: stable serialization does not freeze mutable global/namespace values or stateful behavior. The frozen-context guarantee does not cover those dependencies."),
    call=NULL,code="W_CALLBACK_DEPENDENCIES"),class=c("measurement_warning","warning","condition")))
  if(identical(checks$identification$status,"not_assessed"))warning(structure(list(
    message=paste("Scientific identification remains unassessed:",checks$identification$reason,"Fixed-state numerical qualification does not establish population-parameter recovery."),
    call=NULL,code="W_IDENTIFICATION_UNASSESSED"),class=c("measurement_warning","warning","condition")))
  if(identical(checks$identification$status,"possible_nonidentification")&&!exploratory)
    .mp_abort("E_IDENTIFICATION_UNRESOLVED",paste(checks$identification$reason,
      "Inspect tolerance_ranks and example_local_null_direction. Fitting is paused unless exploratory=TRUE is explicitly chosen; that does not establish identification"))
  if(identical(checks$identification$status,"possible_nonidentification")&&exploratory)
    warning(structure(list(message=paste("Local identification warning before backend work:",checks$identification$reason,
      "The requested development run cannot establish identified inference."),call=NULL,code="W_IDENTIFICATION"),
      class=c("measurement_warning","warning","condition")))
  invisible(checks)
}
.bh_fit_gate <- function(spec) {
  if(!isTRUE(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1"))) .bh_schema_error("explicitly rebuild this historical context from the saved formula, original data and intended priors before using the affine coefficient compiler; its fingerprint is not rewritten")
  checks <- check_model(spec)
  if(any(!vapply(spec$models,function(b){cap<-b$model$capabilities;flag<-cap$available[cap$capability=="fit"];length(flag)==1L&&isTRUE(flag)},logical(1))))
    .mp_abort("E_CAPABILITY","every observation model must declare fit capability before code generation or compilation")
  if(checks$backend$support$status!="supported_typed_ir") .mp_abort("E_OBSERVATION_CONTRACT_UNSUPPORTED",checks$backend$support$reason)
  if(identical(checks$correlation_feasibility$status,"proven_infeasible_strict_PD")) .mp_abort("E_CORRELATION_DOMAIN","the declared correlation matrix has no strictly positive-definite support; singular PSD models are outside this capability")
  if(identical(checks$priors$status,"invalid")) .mp_abort("E_PRIOR_INVALID",paste("invalid resolved prior distributions; inspect get_prior():",paste(checks$priors$invalid$parameter,collapse=", ")))
  if(identical(checks$identification$status,"proven_unidentified")) .mp_abort("E_IDENTIFICATION",checks$identification$reason)
  if(checks$priors$status!="complete") .mp_abort("E_PRIOR_INCOMPLETE",paste("specify missing priors:",paste(checks$priors$missing,collapse=", ")))
  .bh_affine_correlation_lowering_gate(spec)
  invisible(checks)
}
