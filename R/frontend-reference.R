# Explicit person law, common correlation coordinates and latent augmentation.
# New modes remain inspection-only until their separate lowering/native gate.
.bh_centered_person <- function(spec) is.list(spec$formula$person) && identical(spec$formula$person$distribution,"sample_centered")
.bh_raw_cor <- function(spec) identical(spec$formula$correlation_coordinates$type,"ordered_blocks")
.bh_task_augmentation <- function(spec) identical(spec$formula$augmentation$type,"task_cholesky")
.bh_reference_mode <- function(spec) !is.null(spec$formula$reference)
.bh_no_active_person_effect <- function(spec) .bh_task_augmentation(spec) && !length(spec$tables$augmentation$target)
.bh_capture_person <- function(person) {
  if(identical(person,"iid"))return(person)
  if(!is.list(person)||is.object(person)||!setequal(names(person),c("distribution","ids"))||anyDuplicated(names(person))||!identical(person$distribution,"sample_centered"))
    .mp_abort("E_PERSON_LAW","use person='iid' or list(distribution='sample_centered', ids=ordered_public_ids)")
  ids<-person$ids
  if(typeof(ids)!="character"||is.object(ids)||length(attributes(ids))||length(ids)<2L||anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))
    .mp_abort("E_PERSON_ROSTER","centered persons require at least two distinct plain character IDs in the intended order")
  list(distribution="sample_centered",ids=.mp_key_component(ids,"person roster"))
}
.bh_capture_augmentation <- function(augmentation) {
  if(is.null(augmentation))return(NULL)
  if(!is.list(augmentation)||is.object(augmentation)||is.null(names(augmentation))||anyDuplicated(names(augmentation))||any(!names(augmentation)%in%c("type","targets"))||!identical(augmentation$type,"task_cholesky"))
    .mp_abort("E_AUGMENTATION","use list(type='task_cholesky', targets=one_sided_target_formula)")
  target<-augmentation$targets
  if(is.null(target))return(list(type="task_cholesky",targets=NULL,order_source="canonical_symbolic_support"))
  if(!inherits(target,"formula")||length(target)!=2L).mp_abort("E_AUGMENTATION_TARGET","augmentation targets require a captured one-sided formula")
  rhs<-.bh_unwrap(target[[2L]])
  empty<-is.numeric(rhs)&&length(rhs)==1L&&!is.na(rhs)&&rhs==0
  parts<-if(empty)list()else .bh_formula_targets(target)
  list(type="task_cholesky",targets=parts,order_source=if(empty)"explicit_empty"else"explicit_target_order")
}
.bh_capture_raw_coordinates <- function(coordinates,common) {
  if(is.null(coordinates))return(NULL)
  if(!is.list(coordinates)||is.object(coordinates)||!setequal(names(coordinates),c("type","blocks"))||anyDuplicated(names(coordinates))||!identical(coordinates$type,"ordered_blocks"))
    .mp_abort("E_CORRELATION_COORDINATES","coordinates require type='ordered_blocks' and an ordered named blocks list")
  blocks<-coordinates$blocks
  if(!is.list(blocks)||is.object(blocks)).mp_abort("E_CORRELATION_BLOCKS","blocks must be an ordered plain list of coordinate formulas")
  if(length(blocks)).mp_names(blocks,"ordered correlation blocks")
  parsed<-lapply(blocks,function(rule) {
    if(!inherits(rule,"formula")||length(rule)!=2L).mp_abort("E_CORRELATION_BLOCKS","each block needs a nonempty one-sided coordinate formula")
    .bh_coordinate_selection(rule[[2L]],common)
  })
  index<-as.integer(unlist(parsed,use.names=FALSE))
  if(length(index)!=nrow(common)||anyDuplicated(index)||!setequal(index,seq_len(nrow(common))))
    .mp_abort("E_CORRELATION_BLOCKS","ordered blocks must partition every common scalar coordinate exactly once")
  list(type="ordered_blocks",blocks=parsed,ordered_indices=index)
}
.bh_reference_graph <- function(raw,correlations) {
  if(is.null(raw))return(invisible(TRUE))
  membership<-integer(length(raw$ordered_indices))
  for(m in seq_along(raw$blocks))membership[raw$blocks[[m]]]<-m
  expected<-ifelse(membership[correlations$row]==membership[correlations$col],"zero","free")
  if(any(correlations$state!=expected)).mp_abort("E_CORRELATION_COORDINATE_MASK","ordered raw blocks require the already-declared graph to be identity within blocks and free between every pair of distinct blocks; selecting coordinates adds no scientific zeros")
  invisible(TRUE)
}
.bh_bind_person_roster <- function(mapped,formula,blocks,subjects) {
  if(identical(formula$person,"iid"))return(mapped)
  ids<-formula$person$ids
  for(name in names(blocks)) {
    d<-blocks[[name]]$data
    public<-if(subjects$type=="shared")d$.local_id else {
      cw<-subjects$data;at<-match(paste(name,d$.local_id,sep="::"),paste(cw$model,cw$local_id,sep="::"))
      if(anyNA(at)).mp_abort("E_CROSSWALK","every original row must have a public person identity")
      cw$person_id[at]
    }
    if(any(!public%in%ids)).mp_abort("E_PERSON_ROSTER",paste("original observations, including excluded rows, have IDs outside the declared roster in block",name))
  }
  keys<-paste(subjects$namespace,ids,sep="::")
  for(name in names(mapped$blocks))mapped$blocks[[name]]$.person_index<-match(mapped$blocks[[name]]$.person_key,keys)
  mapped$subject_keys<-keys
  mapped$coverage<-matrix(vapply(mapped$blocks,function(d)keys%in%d$.person_key,logical(length(keys))),nrow=length(keys),ncol=length(blocks),dimnames=list(keys,names(blocks)))
  mapped$overlap<-crossprod(mapped$coverage*1L)
  mapped
}
.bh_attach_reference_tables <- function(spec) {
  centered<-.bh_centered_person(spec);task<-.bh_task_augmentation(spec);raw<-.bh_raw_cor(spec)
  if(centered!=task).mp_abort("E_REFERENCE_COMBINATION","the first centered-person capability requires explicit task_cholesky augmentation; iid task augmentation and centered common-plus-residual augmentation are not lowered")
  N<-length(spec$data_map$subject_keys);K<-nrow(spec$tables$components)
  public<-if(centered)spec$formula$person$ids else substring(spec$data_map$subject_keys,nchar(spec$subjects$namespace)+3L)
  spec$tables$person_roster<-data.frame(person_index=seq_len(N),person_id=public,person_key=spec$data_map$subject_keys,
    observed=rowSums(spec$data_map$coverage)>0L,stringsAsFactors=FALSE)
  if(.bh_has_shares(spec)){spec$tables$decompositions$population<-if(centered)"sample_centered"else"iid";spec$tables$decompositions$correlation_prior<-if(raw)"ordered_raw_B_Normal"else"generic_final_marginal_R"}
  spec$tables$person_law<-list(distribution=if(centered)"sample_centered"else"iid",roster_size=N,
    observed_people=sum(rowSums(spec$data_map$coverage)>0L),covariance_law=if(centered)"H=N/(N-1)*(I-11'/N); diagonal 1, off-diagonal -1/(N-1)"else"I",
    covariance_rank=if(centered)N-1L else N,roster_source=if(centered)"explicit_public_ids_before_exclusion"else"consumed_observed_union",
    dense_covariance_stored=FALSE)
  blocks<-list();cells<-list();ordered<-if(raw)spec$formula$correlation_coordinates$ordered_indices else seq_len(K)
  if(raw)for(m in seq_along(spec$formula$correlation_coordinates$blocks)) {
    block<-names(spec$formula$correlation_coordinates$blocks)[m];now<-spec$formula$correlation_coordinates$blocks[[m]]
    previous<-ordered[seq_len(sum(lengths(spec$formula$correlation_coordinates$blocks)[seq_len(m-1L)]))]
    blocks[[m]]<-data.frame(block=block,block_index=m,within_block_index=seq_along(now),component_index=now,
      component=spec$tables$components$component[now],coordinate_id=spec$tables$coordinates$id[now],ordered_index=match(now,ordered),stringsAsFactors=FALSE)
    for(col in seq_along(previous))for(row in seq_along(now))cells[[length(cells)+1L]]<-data.frame(
      parameter=.bh_public_id("cor_raw",block,spec$tables$coordinates$id[now[row]],spec$tables$coordinates$id[previous[col]]),block=block,block_index=m,
      row=row,col=col,destination_index=now[row],previous_index=previous[col],destination=spec$tables$coordinates$id[now[row]],
      previous_whitened_coordinate=spec$tables$coordinates$id[previous[col]],stringsAsFactors=FALSE)
  }
  if(raw){spec$tables$correlations$estimand<-ifelse(spec$tables$correlations$state=="free","derived_final_marginal_correlation","fixed_structural_zero");spec$tables$correlations$primitive_parameter<-rep(NA_character_,nrow(spec$tables$correlations))}
  spec$tables$correlation_blocks<-if(length(blocks))do.call(rbind,blocks)else data.frame(block=character(),block_index=integer(),within_block_index=integer(),component_index=integer(),component=character(),coordinate_id=character(),ordered_index=integer())
  raw_cells<-if(length(cells))do.call(rbind,cells)else data.frame(parameter=character(),block=character(),block_index=integer(),row=integer(),col=integer(),destination_index=integer(),previous_index=integer(),destination=character(),previous_whitened_coordinate=character())
  rownames(raw_cells)<-NULL;raw_cells$primitive_index<-seq_len(nrow(raw_cells));spec$tables$raw_correlation_coordinates<-raw_cells
  spec$tables$correlation_coordinate_system<-list(type=if(raw)"ordered_blocks"else"final_marginal_mask",ordered_global_indices=ordered,
    prior_measure=if(raw)"independent zero-centered Normal raw B cells"else"joint determinant density relative to unique free final marginal entries",
    applied_prior_coordinate_jacobian=if(raw)"zero: the primitive prior coordinates are B"else"MGS chart Jacobian applied once",
    geometric_B_to_R_jacobian=if(raw)"generally nonzero; not evaluated or applied"else"not_applicable")
  target<-spec$tables$targets
  support<-target$key%in%unique(c(spec$tables$loadings$target,spec$tables$parameter_residuals$target))
  active<-target$key[target$level=="subject_task" & support]
  selected<-active;source<-"common_plus_parameter_residual"
  if(task) {
    declaration<-spec$formula$augmentation;source<-declaration$order_source
    if(!is.null(declaration$targets))selected<-vapply(declaration$targets,function(t)if(t$kind=="wide")targets("scores","mu",t$column)else targets(t$block,t$parameter,t$task),character(1))
    if(anyDuplicated(selected)||length(selected)!=length(active)||!setequal(selected,active)).mp_abort("E_AUGMENTATION_TARGET","augmentation order must cover each symbolically active subject-task target exactly once; fixed/task/structural-zero/unknown targets are not silently filtered")
    if(any(!selected%in%spec$tables$parameter_residuals$target)).mp_abort("E_TASK_AUGMENTATION_PD","this task-Cholesky capability requires explicit positive parameter-residual coverage for every active target; no residual is invented")
  }
  spec$tables$task_target_domain<-data.frame(target=target$key,level=target$level,has_symbolic_support=support,
    task_white_target=task & target$key%in%selected,reason=ifelse(target$level!="subject_task","nonvarying_parameter_level",ifelse(support,"potentially_nonzero_latent_support","exact_zero_latent_support_mean_only")),stringsAsFactors=FALSE)
  JI<-if(task)length(selected)else 0L
  spec$tables$augmentation<-list(type=if(task)"task_cholesky"else"common_plus_parameter_residual",target=if(task)selected else character(),target_index=if(task)match(selected,target$key)else integer(),order_source=source,
    common_covariance_coordinates=K,parameter_covariance_coordinates=nrow(spec$tables$parameter_residuals),
    sampled_common_coordinates=if(task)0L else K*N,sampled_parameter_coordinates=if(task)0L else nrow(spec$tables$parameter_residuals)*N,
    task_white_coordinates=if(task)JI*(N-1L)else 0L,task_constrained_scores=JI*N,
    raw_coordinate_variance=if(task)N/(N-1)else NA_real_,isometric_basis=if(task)"Stan2.40_sum_to_zero_positive_Helmert"else"not_applicable",
    normalized_subspace_log_adjustment=if(task)JI/2*log(2*pi*N/(N-1))else 0,
    common_scores=if(task)"unsampled_under_task_augmentation"else"sampled",parameter_scores=if(task)"unsampled_under_task_augmentation"else"sampled",
    numerical_factorization=if(task)"owned row scale tau / unowned scale 1; direct normalized common and residual Gram factors"else"component_noncentering")
  spec$tables$task_latent_coordinates<-data.frame(target=if(task)selected else character(),target_index=if(task)match(selected,target$key)else integer(),
    unconstrained_count=rep(if(task)N-1L else 0L,JI),constrained_count=rep(N,JI),
    raw_coordinate_domain=rep("isometric sum-to-zero basis coordinates; not person IDs",JI),score_domain=rep("ordered person roster",JI),stringsAsFactors=FALSE)
  spec
}
.bh_centering_covariance <- function(spec,person_indices=seq_along(spec$data_map$subject_keys)) {
  if(!.bh_centered_person(spec)).mp_abort("E_PERSON_LAW","this helper requires an explicit sample-centered roster")
  N<-length(spec$data_map$subject_keys)
  if(!is.numeric(person_indices)||anyNA(person_indices)||any(person_indices!=floor(person_indices))||any(person_indices<1L|person_indices>N)).mp_abort("E_PERSON_ROSTER","invalid roster indices")
  N/(N-1)*(outer(person_indices,person_indices,`==`)-1/N)
}
.bh_centering_basis <- function(N) {
  if(length(N)!=1L||!is.finite(N)||N<2L||N!=floor(N)).mp_abort("E_PERSON_ROSTER","centered basis requires integer N>=2")
  Q<-matrix(0,N,N-1L)
  for(k in seq_len(N-1L)){Q[seq_len(k),k]<-1/sqrt(k*(k+1));Q[k+1L,k]<- -k/sqrt(k*(k+1))}
  Q
}
.bh_raw_correlation_state <- function(spec,values,global_cholesky=TRUE) {
  if(!is.logical(global_cholesky)||length(global_cholesky)!=1L||is.na(global_cholesky)).mp_abort("E_STRUCTURE_STATE","global_cholesky must be explicit logical")
  if(!.bh_raw_cor(spec)).mp_abort("E_CORRELATION_COORDINATES","raw B state requires ordered_blocks coordinates")
  tab<-spec$tables$raw_correlation_coordinates;K<-nrow(spec$tables$components)
  if(!is.numeric(values)||length(values)!=nrow(tab)||any(!is.finite(values))).mp_abort("E_STRUCTURE_STATE","raw B state must be finite with one value per captured primitive")
  L<-diag(K);blocks<-spec$formula$correlation_coordinates$blocks;p<-0L
  for(m in seq_along(blocks)) {
    k<-length(blocks[[m]]);now<-p+seq_len(k)
    if(p) {
      B<-matrix(values[tab$block_index==m],k,p);G<-diag(k)+tcrossprod(B)
      if(any(!is.finite(G))).mp_abort("E_STRUCTURE_STATE","raw B Gram matrix is not representable")
      C<-tryCatch(t(chol(G)),error=function(e).mp_abort("E_STRUCTURE_STATE","raw B Gram Cholesky failed; no jitter or clipping"))
      L[now,seq_len(p)]<-forwardsolve(C,B);L[now,now]<-forwardsolve(C,diag(k))
    }
    p<-p+k
  }
  Rordered<-tcrossprod(L);order<-spec$formula$correlation_coordinates$ordered_indices
  R<-matrix(0,K,K);if(K)R[order,order]<-Rordered
  if(any(!is.finite(c(L,R)))||any(diag(L)<=0)).mp_abort("E_STRUCTURE_STATE","raw correlation map is outside finite numerical representation")
  dimnames(R)<-list(spec$tables$components$component,spec$tables$components$component)
  F<-matrix(0,K,K);if(K)F[order,]<-L
  list(R=R,L_ordered=L,R_ordered=Rordered,ordered_global_indices=order,F_global=F,
    square_factor_column_ids=spec$tables$coordinates$id[order],square_factor_convention="global component rows, ordered block whitening columns; not a positive global Cholesky",
    stored_R_scope="rounded factor product; numerical positive definiteness is not certified by mathematical raw-coordinate validity",
    L_global=if(!global_cholesky)NULL else if(K)tryCatch(t(chol(R)),error=function(e).mp_abort("E_STRUCTURE_STATE","global raw correlation Cholesky is not representable"))else matrix(numeric(),0L,0L))
}
.bh_raw_correlation_inverse <- function(spec,R) {
  if(!.bh_raw_cor(spec)).mp_abort("E_CORRELATION_COORDINATES","raw inverse requires ordered_blocks coordinates")
  K<-nrow(spec$tables$components);order<-spec$formula$correlation_coordinates$ordered_indices
  if(!is.matrix(R)||!is.numeric(R)||!identical(dim(R),c(K,K))||any(!is.finite(R))||max(c(0,abs(R-t(R))))>1e-10).mp_abort("E_STRUCTURE_STATE","raw inverse needs a finite symmetric matrix in global coordinate order")
  L<-if(K)tryCatch(t(chol(R[order,order,drop=FALSE])),error=function(e).mp_abort("E_STRUCTURE_STATE","raw inverse requires positive definite R"))else matrix(numeric(),0L,0L)
  out<-numeric(nrow(spec$tables$raw_correlation_coordinates));p<-0L
  for(m in seq_along(spec$formula$correlation_coordinates$blocks)) {
    k<-length(spec$formula$correlation_coordinates$blocks[[m]]);now<-p+seq_len(k)
    if(max(c(0,abs(R[order[now],order[now],drop=FALSE]-diag(k))))>1e-10).mp_abort("E_STRUCTURE_STATE","raw inverse requires identity within each declared block")
    if(p)out[spec$tables$raw_correlation_coordinates$block_index==m]<-as.vector(forwardsolve(L[now,now,drop=FALSE],L[now,seq_len(p),drop=FALSE]))
    p<-p+k
  }
  stats::setNames(out,spec$tables$raw_correlation_coordinates$parameter)
}
.bh_centered_white_transform <- function(white,N) {
  # Q times each raw vector, using suffix sums instead of an N by (N-1)
  # matrix. The explicit dense basis remains a bounded inspection/oracle tool.
  J<-nrow(white);out<-matrix(0,J,N)
  k<-as.double(seq_len(N-1L));scale<-sqrt(k*(k+1))
  if(J)for(j in seq_len(J)) {
    value<-white[j,]/scale
    out[j,]<-c(rev(cumsum(rev(value))),0)
    out[j,-1L]<-out[j,-1L]-k*value
  }
  if(any(!is.finite(out))).mp_abort("E_STRUCTURE_STATE","required centered white transform cannot be represented")
  out
}
.bh_task_state <- function(spec,values,alpha,white=NULL,coordinates=c("unconstrained","constrained")) {
  if(!.bh_task_augmentation(spec)||!.bh_centered_person(spec)).mp_abort("E_AUGMENTATION","task state requires the explicit centered task-Cholesky law")
  coordinates<-match.arg(coordinates);N<-length(spec$data_map$subject_keys);J<-nrow(spec$tables$targets);at<-spec$tables$augmentation$target_index;JI<-length(at)
  if(!is.numeric(alpha)||length(alpha)!=J||any(!is.finite(alpha))).mp_abort("E_STRUCTURE_STATE","alpha must cover each free predictor mean exactly once")
  if(is.null(white))white<-matrix(0,JI,if(coordinates=="unconstrained")N-1L else N)
  if(!is.matrix(white)||!is.numeric(white)||!identical(dim(white),c(JI,if(coordinates=="unconstrained")N-1L else N))||any(!is.finite(white))).mp_abort("E_STRUCTURE_STATE","task white dimensions must match the explicit target order and centered roster")
  Z<-if(coordinates=="unconstrained").bh_centered_white_transform(white,N)else white
  if(any(abs(rowSums(Z))>1e-10*pmax(1,rowSums(abs(Z))))).mp_abort("E_STRUCTURE_STATE","constrained task whites must sum to zero over the declared roster")
  layout<-.bh_covariance_layout(spec);normalized_values<-values;d<-rep(1,J)
  if(.bh_has_shares(spec)){owned<-spec$tables$share_targets;d[owned$target_index]<-values[layout$indices$total_sd];normalized_values[layout$indices$total_sd]<-1}
  if(any(!is.finite(d)|d<=0)).mp_abort("E_STRUCTURE_STATE","task working row scales must be finite and positive")
  state<-.bh_covariance_state(spec,normalized_values,covariance=FALSE)
  A0<-state$effective_loadings;D0<-state$D_parameter
  LC<-if(.bh_raw_cor(spec))state$F_component else if(ncol(A0))t(chol(state$R_component))else matrix(numeric(),0L,0L)
  LU<-if(ncol(D0))t(chol(state$R_parameter))else matrix(numeric(),0L,0L)
  common_work<-A0%*%LC;parameter_work<-D0%*%LU
  Sfull<-tcrossprod(common_work)+tcrossprod(parameter_work);S0<-Sfull[at,at,drop=FALSE]
  if(any(!is.finite(S0))).mp_abort("E_STRUCTURE_STATE","normalized task Gram covariance is nonfinite")
  L0<-if(JI)tryCatch(t(chol(S0)),error=function(e).mp_abort("E_STRUCTURE_STATE","normalized task covariance is not numerically positive definite; no jitter or clipping"))else matrix(numeric(),0L,0L)
  work<-L0%*%Z;delta_active<-sweep(work,1L,d[at],`*`)
  if(any(!is.finite(delta_active))||any(work!=0&delta_active==0)).mp_abort("E_STRUCTURE_STATE","required task deviations overflowed or underflowed; no clipping")
  delta<-matrix(0,N,J);if(JI)delta[,at]<-t(delta_active)
  eta<-sweep(delta,2L,alpha,`+`)
  if(any(!is.finite(eta))).mp_abort("E_STRUCTURE_STATE","required task predictors are nonfinite")
  for(j in seq_len(J)) {
    role<-spec$tables$targets[j,];parameter<-spec$models[[role$model]]$model$parameters[[role$parameter]]
    for(v in eta[,j]).bh_fixed_representation(parameter,v,"predictor")
  }
  export<-function(x,log_multiplier) {
    logs<-log(abs(x))+log_multiplier;value<-sign(x)*exp(logs)
    unavailable<-!is.finite(value)|(x!=0&value==0);value[unavailable]<-NA_real_
    list(value=value,available=!unavailable,log_abs=logs)
  }
  covariance<-export(Sfull,outer(log(d),log(d),`+`))
  component_covariance<-export(state$R_component,outer(log(state$component_sd),log(state$component_sd),`+`))
  loading<-export(A0,matrix(rep(log(d),ncol(A0)),J,ncol(A0)))
  declared<-export(state$Lambda,matrix(rep(log(d),ncol(state$Lambda)),J,ncol(state$Lambda)))
  residual<-export(D0,matrix(rep(log(d),ncol(D0)),J,ncol(D0)))
  dimnames(eta)<-dimnames(delta)<-list(spec$data_map$subject_keys,spec$tables$targets$key)
  list(eta=eta,delta=delta,z_task=Z,task_target=spec$tables$augmentation$target,task_target_index=at,
    working_row_scale=d,working_covariance=S0,working_cholesky=L0,working_common_factor=common_work,working_parameter_factor=parameter_work,
    working_covariance_scope="normalized by owned total SD only; not universally a correlation matrix",
    common_factor_convention=if(.bh_raw_cor(spec))"global component rows, ordered block whitening columns; square covariance factor only"else"positive global Cholesky",
    common_factor_column_ids=if(.bh_raw_cor(spec))spec$tables$coordinates$id[spec$formula$correlation_coordinates$ordered_indices]else spec$tables$coordinates$id,
    stored_common_R_scope="rounded factor product; numerical PD and HMC suitability remain unqualified",
    Sigma_predictor=covariance$value,Sigma_predictor_available=covariance$available,Sigma_predictor_log_abs=covariance$log_abs,
    effective_loadings=loading$value,effective_loadings_available=loading$available,effective_loadings_log_abs=loading$log_abs,
    Lambda=declared$value,Lambda_available=declared$available,D_parameter=residual$value,D_parameter_available=residual$available,
    component_sd=state$component_sd,component_covariance=component_covariance$value,component_covariance_available=component_covariance$available,component_covariance_log_abs=component_covariance$log_abs,
    R_component=state$R_component,R_parameter=state$R_parameter,shares=state$shares,
    common_scores="unsampled_under_task_augmentation",parameter_scores="unsampled_under_task_augmentation")
}
# This is a normalized-measure theorem, not a native/HMC qualification.
.bh_posterior_normalizability <- function(spec,diagnostic_scope) {
  result<-.bh_gaussian_propriety(spec,diagnostic_scope)
  result$dimension<-"posterior normalizability, separate from likelihood identification and numerical/sampling diagnostics"
  result$person_law<-spec$tables$person_law$distribution
  if(!.bh_centered_person(spec)||!.bh_task_augmentation(spec)||!.bh_raw_cor(spec))return(result)
  if(any(spec$tables$priors$status!="specified")||any(!is.na(.bh_resolved_prior_errors(spec$tables$priors))))return(result)
  if(any(spec$tables$parameter_correlations$state!="zero"))return(result)
  kinds<-vapply(spec$models,function(b).bh_reference_density_kind(b$model),character(1))
  if(anyNA(kinds)||any(kinds!="m3_reference_full"))return(result)
  # Keep the first certificate to the canonical full-M3 scalar support/receiver
  # contract and its exact branch binding. Broader S21 spellings remain open.
  if(any(vapply(spec$models,function(b)!is.null(b$parameter_levels),logical(1))))return(result)
  verified<-tryCatch({
    for(b in names(spec$models)) {
      block<-spec$models[[b]];d<-spec$data_map$blocks[[b]]
      .mp_validate_fields(block$model,d,allow_missing_response=FALSE)
      .mp_validate_full_m3(d)
      .mp_validate_m3_bindings(block$data,block$parameter_bindings)
    }
    TRUE
  },error=function(e)FALSE)
  if(!verified)return(result)
  allowed<-c("Intercept","loading","amplitude","component_sd","parameter_sd","total_sd","share","cor_raw","structural_latent")
  if(any(!spec$tables$priors$class%in%allowed))return(result)
  list(status="proven_normalizable",dimension=result$dimension,person_law=result$person_law,
    reason="the checked full-M3 normalized multinomial likelihood is bounded by one and positive on a relative neighbourhood with positive proper-prior measure",
    normalized_evidence_bound="0 < Z <= 1 relative to normalized proper joint prior and normalized per-row PMFs",
    prior_measure="complete supported proper population densities; direct Normal raw B priors; independent parameter residual correlation; centered Gaussian white measure on each N-1-dimensional isometric subspace",
    positive_mass_premise="canonical finite interior received M3 parameters, positive active option multiplicities, valid inactive zeros and finite count vectors give continuous positive category probabilities in a relative neighbourhood",
    checked_blocks=names(spec$models),normalized_rows=sum(vapply(spec$data_map$blocks,nrow,integer(1))),
    scope="exact full-M3 conditional kernel/route/binding and canonical scalar parameter contract with this centered task/raw-B law; parameter-level overrides and arbitrary continuous/custom/mixed likelihoods are outside this first certificate",
    limitations="likelihood identification, finite-precision density evaluation, compilation, sampling diagnostics and HMC remain separately assessed; the bound is not a bound on CmdStan proportional lp")
}
