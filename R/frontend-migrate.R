# Captured formula upgrades and explicit prepared-context rebuild boundaries.
# Original sealed objects are never rehashed or mutated.
.bh_schema_error <- function(message).mp_abort("E_SCHEMA_VERSION",message)
.bh_formula_upgrade <- function(formula,warn=FALSE) {
  if(!inherits(formula,"bhmm_formula")) .mp_abort("E_FORMULA","expected a bmf declaration")
  original<-formula;original$fingerprint<-NULL
  if(!identical(formula$fingerprint,.mp_hash(original))) .mp_abort("E_SPEC_CHANGED","bhmf declaration changed; construct a new declaration")
  version<-formula$version
  if(!is.character(version)||length(version)!=1L||is.na(version)) .bh_schema_error("measurement declaration has no recognized schema version")
  if(version%in%c("loading-expression-components-v2","variance-share-components-v1","ordinary-loading-constraints-v1","marker-scale-components-v1","reference-law-components-v1","affine-correlation-components-v1")) {
    if(is.null(formula$coordinates)) .bh_schema_error("coefficient-map declaration is missing its scalar coordinate map")
    return(formula)
  }
  if(!version%in%c("scalar-components-v1","scalar-components-residual-v1")) .bh_schema_error(paste("unsupported measurement declaration schema",version))
  allowed<-c("components","correlations","component_names","person","scale","residual","version","fingerprint")
  if(!setequal(names(formula),allowed)||!identical(formula$person,"iid")||!identical(formula$scale,"unit_variance")) .bh_schema_error("historical declaration contains unsupported structure fields")
  if(length(formula$components)) .mp_names(formula$components,"historical components")
  if(!identical(formula$component_names,names(formula$components))) .bh_schema_error("historical component names are inconsistent")
  for(component in formula$components) {
    if(inherits(component,"bhmm_basis_component")||!is.list(component)||!length(component)) .bh_schema_error("historical scalar declarations cannot contain hidden basis or empty components")
    .bh_legacy_target_descriptors(component)
  }
  .bh_legacy_correlation_table(formula$correlations,names(formula$components))
  if(!identical(formula$residual,"none")) {
    r<-formula$residual
    if(!is.list(r)||!identical(names(r),c("targets","correlations"))) .bh_schema_error("unsupported historical residual declaration")
    .bh_legacy_target_descriptors(r$targets)
    .bh_legacy_correlation_table(r$correlations,vapply(r$targets,.bh_target_descriptor_id,character(1)))
  }
  # The previous formula already stores the resolved marginal graph; retain it.
  upgraded<-formula;upgraded$coordinates<-.bh_component_coordinates(formula$components)
  upgraded$version<-"loading-expression-components-v2"
  upgraded$schema_migration<-list(kind="formula",from=version,to=upgraded$version,source_fingerprint=formula$fingerprint,
    operation="scalar coordinates added; target membership and resolved correlation graph unchanged")
  upgraded$fingerprint<-NULL;upgraded$fingerprint<-.mp_hash(upgraded)
  if(warn)warning(structure(list(message="A validated historical measurement formula was upgraded without re-evaluating its selectors. Rebuilding uses the data and priors supplied now; supply the intended resolved priors when reproducing an older analysis.",call=NULL,code="W_SCHEMA_MIGRATION"),class=c("measurement_warning","warning","condition")))
  upgraded
}
.bh_normalize_spec <- function(spec, warn=TRUE) {
  .bh_pf_refuse_execution(spec,"prepared specification normalization")
  .bh_validate_fingerprint(spec)
  if(!isTRUE(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1"))) .bh_schema_error("the explicit person-law/correlation-coordinate/augmentation schema requires an explicit rebuild from the original declaration, data and intended priors; historical contexts are not relabeled, expanded or resealed")
  if(!all(c("loading_cells","loading_primitives","loading_constraint_rules","ordinary_loading_map","ordinary_fixed_matrix","orientation","component_scales")%in%names(spec$tables))) .bh_schema_error("this schema requires complete ordinary cell, primitive, fixed-value and orientation maps")
  if(!all(c("person_roster","person_law","augmentation","task_target_domain","task_latent_coordinates","correlation_blocks","raw_correlation_coordinates","correlation_coordinate_system")%in%names(spec$tables))) .bh_schema_error("reference schema is missing its explicit person/correlation/augmentation tables")
  if(.bh_has_correlation_declarations(spec)&&!identical(spec$schema_version,"affine-correlation-spec-v1")) .bh_schema_error("fixed/equal correlation declarations require their explicit affine prepared schema")
  if(identical(spec$schema_version,"affine-correlation-spec-v1")&&!all(c("correlation_primitives","correlation_affine_cells","correlation_affine_constants","correlation_domains","correlation_constraint_rules","correlation_prior_geometry")%in%names(spec$tables))) .bh_schema_error("affine correlation context lacks its unique-label, domain or prior-measure tables")
  scales<-spec$tables$component_scales;K<-nrow(spec$tables$components)
  if(!is.data.frame(scales)||nrow(scales)!=K||!all(c("component","component_index","state","parameter","primitive_index","fixed_sd","marker_cell","marker_value")%in%names(scales))||
     !identical(scales$component_index,seq_len(K))||!identical(scales$component,spec$tables$components$component)||any(!scales$state%in%c("fixed","free")))
    .bh_schema_error("component-scale rows must cover the captured coordinates exactly")
  .bh_information_policy_assert(spec)
  version<-spec$formula$version
  if(!is.character(version)||length(version)!=1L||is.na(version)) .bh_schema_error("prepared context has no recognized measurement schema")
  if(!version%in%c("loading-expression-components-v2","variance-share-components-v1","ordinary-loading-constraints-v1","marker-scale-components-v1","reference-law-components-v1","affine-correlation-components-v1")) .bh_schema_error("an affine prepared context requires a current captured formula; rebuild an older formula explicitly with data and intended priors")
  if(!is.null(spec$schema_version)&&!spec$schema_version%in%c("coefficient-map-spec-v1","variance-share-spec-v1","reference-law-spec-v1","affine-correlation-spec-v1")) .bh_schema_error("unknown prepared-context schema")
  if(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1")&&!all(c("parameter_roles","fixed_parameters","individual_targets","response_domains","observation_operands")%in%names(spec$tables))) .bh_schema_error("parameter-level context lacks its role/domain/operand tables; rebuild explicitly")
  if(version=="variance-share-components-v1"&&(!spec$schema_version%in%c("variance-share-spec-v1","reference-law-spec-v1")||is.null(spec$formula$decomposition)||!all(c("share_targets","share_parts","share_strata","share_coefficients","share_cells","decompositions")%in%names(spec$tables)))) .bh_schema_error("variance-share context lacks its versioned ownership/expression tables; rebuild explicitly")
  if(version=="loading-expression-components-v2"&&identical(spec$schema_version,"variance-share-spec-v1")) .bh_schema_error("share context schema contradicts its measurement declaration")
  if(version=="ordinary-loading-constraints-v1"&&(is.null(spec$formula$loadings)||!all(c("loading_cells","loading_primitives","ordinary_loading_map","ordinary_fixed_matrix","orientation","component_scales")%in%names(spec$tables)))) .bh_schema_error("loading-constraint context lacks its affine primitive map; rebuild explicitly")
  if(version%in%c("loading-expression-components-v2","variance-share-components-v1","ordinary-loading-constraints-v1","marker-scale-components-v1","reference-law-components-v1","affine-correlation-components-v1")) {
    required<-c("free_loadings","amplitudes","loading_map","loading_coefficients","coordinates","basis_multiplier","basis_amplitude_index")
    if(!all(required%in%names(spec$tables))) .bh_schema_error("coefficient-map context is missing required representation fields")
    frames<-c("targets","components","amplitudes","free_loadings","loading_map","loading_coefficients","coordinates")
    if(any(!vapply(spec$tables[frames],is.data.frame,logical(1)))) .bh_schema_error("coefficient-map tables are malformed")
    J<-nrow(spec$tables$targets);K<-nrow(spec$tables$components);B<-nrow(spec$tables$amplitudes)
    if(!is.matrix(spec$tables$basis_multiplier)||!is.numeric(spec$tables$basis_multiplier)||is.object(spec$tables$basis_multiplier)||
       any(!is.finite(spec$tables$basis_multiplier))||!identical(dim(spec$tables$basis_multiplier),c(J,K))||
       !is.numeric(spec$tables$basis_amplitude_index)||is.object(spec$tables$basis_amplitude_index)||
       length(spec$tables$basis_amplitude_index)!=K||anyNA(spec$tables$basis_amplitude_index)||
       any(spec$tables$basis_amplitude_index<0|spec$tables$basis_amplitude_index>B)||
       any(spec$tables$basis_amplitude_index!=floor(spec$tables$basis_amplitude_index))) .bh_schema_error("coefficient-map context has malformed matrix/index dimensions")
    return(spec)
  }
  if(version!="scalar-components-residual-v1") .bh_schema_error("this saved context predates the supported common/residual schema; rebuild from its saved formula and original data")
  required<-c("targets","components","loadings","correlations","parameter_residuals","parameter_correlations","observation_parameters","information","priors","correlation_mask","parameter_correlation_mask")
  new_fields<-c("free_loadings","amplitudes","loading_map","loading_coefficients","coordinates","basis_multiplier","basis_amplitude_index")
  if(!all(required%in%names(spec$tables))||any(new_fields%in%names(spec$tables))) .bh_schema_error("historical context has missing or contradictory coefficient-map fields")
  .bh_legacy_callback_gate(spec$models)
  # Only known canonical validators may be replayed; opaque callbacks require
  # an explicit rebuild. A deep copy is not a purity guarantee for GlobalEnv.
  source_spec<-spec
  spec<-unserialize(serialize(spec,NULL,version=2))
  formula<-.bh_formula_upgrade(spec$formula)
  resolve<-function(parts)vapply(parts,function(t)if(t$kind=="wide")targets("scores","mu",t$column)else targets(t$block,t$parameter,t$task),character(1))
  selected<-lapply(formula$components,resolve)
  map<-.bh_bind_component_map(formula,selected,spec$tables$targets)
  if(nrow(map$amplitudes)||!identical(map$loadings,spec$tables$loadings)||
    !identical(formula$coordinates$display,spec$tables$components$component)||
    !identical(formula$correlations,spec$tables$correlations)) .bh_schema_error("historical context is not the same independent scalar-loading IR; migration refused")
  K<-nrow(formula$coordinates)
  expected_components<-data.frame(component=formula$coordinates$display,variance=rep(1,K),mean=rep(0,K),
    orientation=vapply(selected,function(keys)keys[1L],character(1)),stringsAsFactors=FALSE)
  if(!identical(spec$tables$components,expected_components)||
     !identical(spec$tables$correlation_mask,.bh_mask(formula$correlations,formula$coordinates$display))) .bh_schema_error("historical component variance/orientation/mask is not the supported unit scalar IR")
  rkeys<-if(is.list(formula$residual))resolve(formula$residual$targets)else character()
  expected_residual<-data.frame(target=rkeys,target_index=match(rkeys,spec$tables$targets$key),residual_index=seq_along(rkeys),
    parameter=vapply(rkeys,function(t).bh_public_id("parameter_sd",t),character(1)),
    units=spec$tables$targets$predictor_units[match(rkeys,spec$tables$targets$key)],stringsAsFactors=FALSE)
  rc<-if(is.list(formula$residual))formula$residual$correlations else .bh_cor_table("independent",character())
  names(rc)[names(rc)=="component1"]<-"target1";names(rc)[names(rc)=="component2"]<-"target2"
  rc$target1<-rkeys[rc$row];rc$target2<-rkeys[rc$col]
  rc$parameter<-vapply(seq_len(nrow(rc)),function(i)if(rc$state[i]=="free").bh_public_id("parameter_cor",rc$target1[i],rc$target2[i])else NA_character_,character(1))
  if(!identical(expected_residual,spec$tables$parameter_residuals)||!identical(rc,spec$tables$parameter_correlations)||
     !identical(.bh_mask(rc,rkeys),spec$tables$parameter_correlation_mask)) .bh_schema_error("historical residual roles or mask are inconsistent")
  if(anyDuplicated(spec$tables$priors$parameter)||any(spec$tables$priors$class=="amplitude")) .bh_schema_error("historical priors contain duplicate or unsupported shared coefficients")
  for(i in seq_len(nrow(spec$tables$priors))) {
    row<-spec$tables$priors[i,]
    if(row$class=="structural_latent") {
      if(!identical(row$prior,"normal(0,1)")) .bh_schema_error("historical latent prior is not the supported unit Normal IR")
    }else if(!is.na(row$prior))tryCatch(set_prior(row$prior,row$class),error=function(e).bh_schema_error(paste("unsupported historical prior:",conditionMessage(e))))
  }
  if(!setequal(unique(c(map$loadings$target,spec$tables$parameter_residuals$target)),spec$tables$targets$key)) .bh_schema_error("historical context has an undeclared target or hidden effect")
  mapped<-.mp_map_data(spec$models,spec$subjects,spec$missing)
  if(!identical(.mp_parameter_keys(spec$models,mapped),spec$parameter_keys)) .bh_schema_error("historical typed model parameter keys no longer reproduce exactly")
  keys<-spec$parameter_keys
  primary<-keys[vapply(seq_len(nrow(keys)),function(i)keys$parameter[i]%in%.mp_model_info(spec$models[[keys$model[i]]]$model)$primary,logical(1)),,drop=FALSE]
  selected_keys<-unique(c(unlist(selected,use.names=FALSE),rkeys))
  if(!setequal(selected_keys,primary$key)) .bh_schema_error("historical declaration does not cover the actual typed primary parameters")
  primary<-primary[match(selected_keys,primary$key),,drop=FALSE];rownames(primary)<-NULL;primary$target_index<-seq_len(nrow(primary))
  if(!identical(primary,spec$tables$targets)||!identical(keys[!keys$key%in%primary$key,,drop=FALSE],spec$tables$observation_parameters)) .bh_schema_error("historical primary or observation-noise roles are inconsistent")
  if(!identical(mapped,spec$data_map)) .bh_schema_error("historical mapped observations no longer reproduce exactly; rebuild from original data")
  information<-do.call(rbind,lapply(names(spec$models),function(b) {
    d<-mapped$blocks[[b]];rule<-.bh_information_contract(spec$models[[b]]$model)
    informative<-switch(rule,known_binomial_positive_trials=d$trials>0,known_m3_positive_total=rowSums(d$counts)>0,rep(TRUE,nrow(d)))
    data.frame(block=b,observation_id=d$.observation_id,informative=informative,policy=rule,stringsAsFactors=FALSE)
  }))
  if(!identical(information,spec$tables$information)) .bh_schema_error("historical information-row policy no longer reproduces exactly")
  upgraded<-spec;upgraded$formula<-formula
  for(name in names(map))upgraded$tables[[name]]<-map[[name]]
  upgraded$tables$loading_coefficients$prior_source<-upgraded$tables$priors$source[match(map$loading_coefficients$parameter,upgraded$tables$priors$parameter)]
  prior_roles<-.bh_prior_table(upgraded,NULL)
  role_fields<-c("parameter","class","target","component","layer","parameter_count","scope")
  # Early common/residual releases labelled target intercept rows "component";
  # their explicit Intercept key/role always lowered to alpha. Preserve those
  # original labels and values, accepting only this known metadata alias.
  intercept<-which(spec$tables$priors$class=="Intercept")
  if(any(!spec$tables$priors$layer[intercept]%in%c("component","predictor"))) .bh_schema_error("unknown historical intercept layer")
  prior_roles$layer[intercept]<-spec$tables$priors$layer[intercept]
  if(!identical(lapply(prior_roles[role_fields],identity),lapply(spec$tables$priors[role_fields],identity))) .bh_schema_error("historical unique prior roles/counts are inconsistent; no prior values were replaced")
  targets<-upgraded$tables$targets$key
  upgraded$tables$target_coverage<-data.frame(target=targets,declared=TRUE,nonzero_common=targets%in%map$loadings$target,
    parameter_residual=targets%in%spec$tables$parameter_residuals$target,mean_only=FALSE,stringsAsFactors=FALSE)
  upgraded$schema_version<-"coefficient-map-spec-v1"
  upgraded$schema_migration<-list(kind="prepared_context",from=version,to=upgraded$schema_version,source_fingerprint=source_spec$fingerprint,
    source_formula_fingerprint=source_spec$formula$fingerprint,operation="B=0 scalar coordinate/coefficient map added; data, likelihoods, existing loading roles and prior values unchanged")
  upgraded$checks<-.bh_checks(upgraded)
  upgraded$fingerprint<-NULL;upgraded$fingerprint<-.mp_hash(upgraded)
  if(warn)warning(structure(list(message="A validated pre-basis context was upgraded to the explicit B=0 coefficient-map schema without changing its model. Save bhmm(old_spec, fit=FALSE) to keep the derived fingerprint and migration record; the original object is unchanged.",call=NULL,code="W_SCHEMA_MIGRATION"),class=c("measurement_warning","warning","condition")))
  upgraded
}

.bh_legacy_target_descriptors <- function(parts) {
  if(!is.list(parts)||!length(parts)) .bh_schema_error("historical target declaration is empty or malformed")
  for(t in parts) {
    fields<-if(identical(t$kind,"wide"))c("kind","column")else if(identical(t$kind,"target"))c("kind","block","parameter","task")else character()
    if(!length(fields)||!identical(names(t),fields)||any(!vapply(t,function(x)typeof(x)=="character"&&!is.object(x)&&length(x)==1L&&!is.na(x)&&nzchar(x),logical(1)))) .bh_schema_error("historical target descriptor is malformed")
  }
  if(anyDuplicated(vapply(parts,.bh_target_descriptor_id,character(1)))) .bh_schema_error("historical target descriptor is duplicated")
  invisible(TRUE)
}
.bh_legacy_correlation_table <- function(cor,labels) {
  expected<-.bh_cor_table("independent",labels)
  if(!is.data.frame(cor)||!identical(names(cor),names(expected))||nrow(cor)!=nrow(expected)||
    !identical(cor[c("row","col")],expected[c("row","col")])||anyNA(cor$state)||any(!cor$state%in%c("zero","free"))||
    anyNA(cor$source)||any(!cor$source%in%c("default","explicit"))) .bh_schema_error("historical correlation table is malformed")
  expected$state<-cor$state;expected$source<-cor$source
  expected$parameter<-vapply(seq_len(nrow(expected)),function(i)if(expected$state[i]=="free").bh_public_id("cor",expected$component1[i],expected$component2[i])else NA_character_,character(1))
  if(!identical(cor,expected)) .bh_schema_error("historical marginal correlation identities are inconsistent")
  invisible(TRUE)
}

.bh_legacy_callback_gate <- function(models) {
  for(block in models) {
    model<-block$model;fields<-c("validate_data","informative_rows")
    if(all(vapply(model[fields],is.null,logical(1))))next
    kind<-.bh_builtin_kind(model)
    canonical<-if(!is.na(kind))switch(kind,gaussian=gaussian_model(),binomial=binomial_model(),lognormal=lognormal_model())else {
      factory<-switch(model$name,m3_cs_reference=m3_cs_model,m3_reference_full=m3_model,sdm_reference=sdm_reference_model,NULL)
      if(is.null(factory)||length(model$adapter$resources)!=1L) .bh_schema_error("opaque historical model callbacks are not replayed during migration; explicitly rebuild with the current model/data contract")
      factory(model$adapter$resources[[1L]]$file)
    }
    if(!identical(model[fields],canonical[fields])) .bh_schema_error("historical callbacks differ from known canonical validators; migration will not execute them. Rebuild explicitly if their validation is intended")
  }
  invisible(TRUE)
}
