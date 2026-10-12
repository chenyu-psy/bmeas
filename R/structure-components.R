component_covariance_structure <- function(families, share_contrast, version, provenance) {
  .mp_names(families,"component families");.mp_scalar_string(version,"structure version");.mp_scalar_string(provenance,"structure provenance")
  if(!is.matrix(share_contrast)||!identical(dim(share_contrast),c(4L,3L))||any(!is.finite(share_contrast))||max(abs(crossprod(share_contrast)-diag(3)))>1e-10||max(abs(colSums(share_contrast)))>1e-10)
    .mp_abort("E_COMPONENT_DESIGN","share contrast must be an orthonormal zero-sum 4x3 matrix")
  keys<-character();common<-integer()
  for(name in names(families)) {
    f<-families[[name]]
    if(!is.character(f$keys)||!length(f$keys)||anyNA(f$keys)||anyDuplicated(f$keys)) .mp_abort("E_COMPONENT_DESIGN","family keys must be distinct")
    for(field in c("paradigm","content","interaction","paradigm_loadings","content_loadings")) {
      x<-f[[field]]
      if(!is.matrix(x)||nrow(x)!=length(f$keys)||!ncol(x)||any(!is.finite(x))) .mp_abort("E_COMPONENT_DESIGN",paste("invalid numeric design matrix",field))
    }
    if(ncol(f$paradigm)!=ncol(f$paradigm_loadings)||ncol(f$content)!=ncol(f$content_loadings)) .mp_abort("E_COMPONENT_DESIGN","loading/contrast dimensions disagree")
    if(max(abs(rowSums(f$paradigm_loadings^2)-1))>1e-10||max(abs(rowSums(f$content_loadings^2)-1))>1e-10)
      .mp_abort("E_COMPONENT_DESIGN","paradigm/content loading design rows must have unit squared norm")
    keys<-c(keys,f$keys);common<-c(common,1L+ncol(f$paradigm)+ncol(f$content))
  }
  if(anyDuplicated(keys)) .mp_abort("E_COMPONENT_DESIGN","component families overlap in parameter keys")
  structure(list(type="component_covariance",families=families,share_contrast=share_contrast,keys=keys,
    common_dimensions=common,common_total=sum(common),version=version,provenance=provenance,
    centering="sum_to_zero_vector; normal SD sqrt(N/(N-1))",scale="predictor",
    estimands=c("common_component_correlation","task_predictor_correlation","individual_eta")),class="measurement_structure")
}
component_prior_set <- function(mean, task_sd, base, paradigm, content, interaction, cross, nuisance=list(), source) {
  .mp_names(mean,"component mean priors");.mp_scalar_string(source,"component prior source")
  if(length(cross)).mp_names(cross,"cross-family prior roles")
  if(length(nuisance)).mp_names(nuisance,"nuisance prior roles")
  for(p in mean){if(!p$distribution%in%c("normal","logistic","student_t")) .mp_abort("E_PRIOR","unsupported component mean prior");normal(p$mean,p$sd);if(p$distribution=="student_t")student_t(p$df,p$mean,p$sd)}
  if(!identical(task_sd$distribution,"student_t")||task_sd$mean!=0) .mp_abort("E_PRIOR","positive task SD prior must be zero-centered Student-t")
  student_t(task_sd$df,task_sd$mean,task_sd$sd)
  for(p in c(list(base,paradigm,content,interaction),cross)){if(!identical(p$distribution,"normal")||p$mean!=0) .mp_abort("E_PRIOR","component coefficient roles require zero-centered normal priors");normal(p$mean,p$sd)}
  for(p in nuisance){if(!identical(p$distribution,"half_normal")) .mp_abort("E_PRIOR","observation nuisance SD requires half-normal");half_normal(p$sd)}
  structure(list(kind="component_roles",mean=mean,task_sd=task_sd,base=base,paradigm=paradigm,content=content,
    interaction=interaction,cross=cross,nuisance=nuisance,source=source),class="measurement_priors")
}
.mp_component_validate <- function(spec) {
  s<-spec$structure;p<-spec$priors
  .mp_validate_component_blueprint(s)
  if(length(spec$data_map$subject_keys)<2L) .mp_abort("E_COMPONENT_CENTERING","sum-to-zero structure requires at least two people")
  if(!identical(p$kind,"component_roles")) .mp_abort("E_PRIOR_SCALE","component structure requires explicit component prior roles, never CFA/LKJ substitution")
  component_prior_set(p$mean,p$task_sd,p$base,p$paradigm,p$content,p$interaction,p$cross,p$nuisance,p$source)
  if(!setequal(names(p$mean),s$keys)||!setequal(names(p$cross),names(s$families)[-1L])) .mp_abort("E_PRIOR_COVERAGE","component mean/cross prior roles are incomplete")
  primary<-unlist(lapply(names(spec$models),function(b){info<-.mp_model_info(spec$models[[b]]$model);spec$parameter_keys$key[spec$parameter_keys$model==b & spec$parameter_keys$parameter%in%info$primary]}),use.names=FALSE)
  if(!setequal(primary,s$keys)) .mp_abort("E_PARAMETER_COVERAGE","structure must cover exactly the declared hierarchical parameter domain")
  nuisance<-unlist(lapply(names(spec$models),function(b){info<-.mp_model_info(spec$models[[b]]$model);spec$parameter_keys$key[spec$parameter_keys$model==b & spec$parameter_keys$parameter%in%info$nuisance]}),use.names=FALSE)
  if(!setequal(names(p$nuisance),nuisance)) .mp_abort("E_PRIOR_COVERAGE","component nuisance priors incomplete")
  binding<-parameter_binding_table(spec)
  # Routed and shorthand ordinary dependencies share the same key-based coverage check below.
  for(b in names(spec$models))if(spec$models[[b]]$model$name%in%c("gaussian","lognormal")) {
    d<-spec$data_map$blocks[[b]]
    for(task in unique(d$.task))if(!any(duplicated(d$.person_key[d$.task==task])))
      .mp_abort("E_DOUBLE_RESIDUAL","Gaussian/lognormal component task residual plus observation SD requires explicit within-person repetitions for each task")
  }
  people<-lapply(s$keys,function(key).mp_people_for_target(spec,key,binding))
  if(any(lengths(people)==0L)) .mp_abort("E_UNINFORMATIVE_PARAMETER","component domain contains a parameter with no informative observation")
  family_people<-lapply(s$families,function(f)unique(unlist(people[match(f$keys,s$keys)])))
  graph<-outer(seq_along(family_people),seq_along(family_people),Vectorize(function(a,b)length(intersect(family_people[[a]],family_people[[b]]))>0))
  reached<-1L
  repeat{next_reached<-which(colSums(graph[reached,,drop=FALSE])>0);if(setequal(reached,next_reached))break;reached<-next_reached}
  if(length(reached)!=length(family_people)) .mp_abort("E_DISCONNECTED_CORRELATION","free component cross-family correlations have disconnected subject sets")
  if(!is.null(spec$provenance$data_hash)&&!identical(spec$provenance$data_hash,.mp_hash(spec$data_map))) .mp_abort("E_DATA_CHANGED","rebuild measurement after changing mapped data")
  list(static_constraints=list(status="computed",reason="Explicit component design, domain, prior roles and centered-subject constraints checked"),
    identification=list(status="not_assessed",reason="Reference algebra and static constraints do not prove statistical identification"),
    prior_sensitivity=list(status="not_assessed",reason="No sensitivity analysis"),
    data_reuse=list(status="computed",reason="Explicit observation keys and source identities checked"),
    warnings="Component means, scales, shares and common correlations are distinct estimands; tiny-run outputs are not inference")
}
.mp_component_prior_table <- function(spec) {
  p<-spec$priors;s<-spec$structure;rows<-list()
  for(key in s$keys) {
    m<-p$mean[[key]];unit<-spec$parameter_keys$predictor_units[match(key,spec$parameter_keys$key)]
    rows[[length(rows)+1L]]<-data.frame(parameter=c(paste0("mean:",key),paste0("task_sd:",key)),
      distribution=c(m$distribution,"student_t_positive_support"),location=c(m$mean,0),scale=c(m$sd,p$task_sd$sd),
      df=c(if(m$distribution=="student_t")m$df else NA_real_,p$task_sd$df),prior_scale="predictor",units=unit,source=p$source)
  }
  for(f in names(s$families))for(role in c("base","paradigm","content","interaction")) {
    rows[[length(rows)+1L]]<-data.frame(parameter=paste0(f,"::",role,"[*]"),distribution="normal",location=0,scale=p[[role]]$sd,df=NA_real_,prior_scale="share_contrast_coefficients",units="unitless",source=p$source)
  }
  for(f in names(p$cross))rows[[length(rows)+1L]]<-data.frame(parameter=paste0(f,"::cross_raw[*]"),distribution="normal",location=0,scale=p$cross[[f]]$sd,df=NA_real_,prior_scale="raw_cross_family",units="unitless",source=p$source)
  rows[[length(rows)+1L]]<-data.frame(parameter="z_task[*]",distribution="normal_on_sum_to_zero",location=0,scale=sqrt(length(spec$data_map$subject_keys)/(length(spec$data_map$subject_keys)-1)),df=NA_real_,prior_scale="centered_task_basis",units="unitless",source=p$source)
  for(key in names(p$nuisance))rows[[length(rows)+1L]]<-data.frame(parameter=paste0("observation_sd:",key),distribution="half_normal",location=0,scale=p$nuisance[[key]]$sd,df=NA_real_,prior_scale="positive_observation_sd",units=spec$parameter_keys$units[match(key,spec$parameter_keys$key)],source=p$source)
  do.call(rbind,rows)
}
evaluate_component_structure <- function(structure, state) {
  s<-structure;.mp_validate_component_blueprint(s)
  J<-length(s$keys);K<-s$common_total;N<-ncol(state$z_task)
  if(!is.matrix(state$z_task)||any(!is.finite(state$z_task))||N<2L||!identical(dim(state$z_task),c(as.integer(J),as.integer(N)))||max(abs(rowSums(state$z_task)))>1e-9)
    .mp_abort("E_COMPONENT_STATE","z_task must be a JxN row-centered matrix, N>1")
  if(!is.numeric(state$mu)||!is.numeric(state$tau)||length(state$mu)!=J||length(state$tau)!=J||any(!is.finite(state$mu))||any(!is.finite(state$tau))||any(state$tau<=0)) .mp_abort("E_COMPONENT_STATE","mean/positive task SD dimensions invalid")
  Lambda<-matrix(0,J,K);shares<-matrix(NA_real_,J,4);L<-matrix(0,K,K);row0<-col0<-0L
  for(i in seq_along(s$families)) {
    name<-names(s$families)[i];f<-s$families[[i]];a<-state$families[[name]];n<-length(f$keys);k<-s$common_dimensions[i]
    if(!is.numeric(a$base)||length(a$base)!=3L||!is.null(dim(a$base))||any(!is.finite(a$base))) .mp_abort("E_COMPONENT_STATE","base coefficient must be a finite3-vector")
    for(role in c("paradigm","content","interaction")) {
      expected_dim<-c(3L,as.integer(ncol(f[[role]])))
      if(!is.matrix(a[[role]])||!identical(dim(a[[role]]),expected_dim)||any(!is.finite(a[[role]]))) .mp_abort("E_COMPONENT_STATE",paste("invalid coefficient matrix",role))
    }
    predictor<-matrix(rep(a$base,each=n),n,3)+f$paradigm%*%t(a$paradigm)+f$content%*%t(a$content)+f$interaction%*%t(a$interaction)
    logits<-predictor%*%t(s$share_contrast)
    probs<-t(apply(logits,1,function(x){e<-exp(x-max(x));e/sum(e)}));r<-row0+seq_len(n);cc<-col0+seq_len(k)
    shares[r,]<-probs;Lambda[r,cc[1]]<-sqrt(probs[,1])
    Lambda[r,cc[1+seq_len(ncol(f$paradigm))]]<-sqrt(probs[,2])*f$paradigm_loadings
    Lambda[r,cc[1+ncol(f$paradigm)+seq_len(ncol(f$content))]]<-sqrt(probs[,3])*f$content_loadings
    if(i==1L)L[cc,cc]<-diag(k)else {
      B<-state$cross[[name]];if(!is.matrix(B)||any(!is.finite(B))||!identical(dim(B),c(as.integer(k),as.integer(col0)))) .mp_abort("E_COMPONENT_STATE","cross matrix has wrong dimensions")
      C<-t(chol(diag(k)+B%*%t(B)));L[cc,seq_len(col0)]<-forwardsolve(C,B);L[cc,cc]<-forwardsolve(C,diag(k))
    }
    row0<-row0+n;col0<-col0+k
  }
  Omega<-tcrossprod(L);Sigma<-tcrossprod(Lambda%*%L)+diag(shares[,4],nrow=J,ncol=J);task_std<-t(t(chol(Sigma))%*%state$z_task)
  delta<-sweep(task_std,2,state$tau,"*");eta<-sweep(delta,2,state$mu,"+")
  list(load_share=shares,load_std=sqrt(shares),load_raw=sweep(sqrt(shares),1,state$tau,"*"),Lambda=Lambda,Omega_component=Omega,Sigma_task=Sigma,
    Sigma_predictor=diag(state$tau,nrow=J,ncol=J)%*%Sigma%*%diag(state$tau,nrow=J,ncol=J),delta=delta,eta=eta)
}
.mp_people_for_target <- function(spec,key,bindings=NULL) {
  parts<-strsplit(key,"::",fixed=TRUE)[[1L]];b<-parts[1L];d<-.mp_informative_data(spec,b)
  if(identical(spec$models[[b]]$model$adapter$mode,"routed")) {
    if(is.null(bindings))bindings<-parameter_binding_table(spec)
    unique(bindings$person_key[!is.na(bindings$target_key)&bindings$target_key==key & bindings$observation_id%in%d$.observation_id])
  } else unique(d$.person_key[d$.task==parts[3L]])
}
reference_run_budget <- function(name) {
  if(!identical(name,"csmu_exp1_memory_gpc_v1")) .mp_abort("E_RUN_BUDGET","no approved small reference budget for this structure")
  structure(list(name=name,max_subjects=8L,max_observations=72L,max_parameters=27L,max_components=15L,max_output_bytes=10*1024^2),class="measurement_model_budget")
}

.mp_validate_component_blueprint <- function(s) {
  rebuilt<-component_covariance_structure(s$families,s$share_contrast,s$version,s$provenance)
  for(field in c("keys","common_dimensions","common_total","centering","scale"))if(!identical(s[[field]],rebuilt[[field]]))
    .mp_abort("E_COMPONENT_DESIGN",paste("derived blueprint field differs from numeric design:",field))
  if(!is.null(s$reference_contract_hash)&&!identical(s$reference_contract_hash,.mp_hash(list(s$families,s$share_contrast,s$keys,s$version))))
    .mp_abort("E_REFERENCE_STRUCTURE","locked reference blueprint changed; construct a separately named generic structure")
  invisible(TRUE)
}
