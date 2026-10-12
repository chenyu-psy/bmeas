# Bounded basic-family PPC forward laws. This module has no posterior reader,
# fit checking, model callback, covariance refactor, or RNG transaction.
# .bh_ppc_forward_plan(spec, level) is a saved-metadata-only plan. Its geometry,
# exact required_variables and per-draw work counts are consumed by the caller.
# .bh_ppc_generate_eta(plan, draw) consumes one aligned named numeric joint row;
# unrequired conditional/constant-only cells are NA, never used by row operands.
# .bh_ppc_simulate_responses(plan, draw) generates one eta and reuses it across
# every block/repeat, returning block-named lists of observation-named vectors.

.bh_ppc_forward_fail <- function(message, code="E_PPC_LAYOUT") .mp_abort(code,message)
.bh_ppc_forward_frame <- function(x, fields, label) {
  if(!identical(class(x),"data.frame") || !all(fields %in% names(x)) ||
     anyDuplicated(names(x))) .bh_ppc_forward_fail(paste(label,"requires its saved plain table"))
  for(name in fields) if(is.object(x[[name]]) || !is.atomic(x[[name]]) ||
                          !is.null(dim(x[[name]])))
    .bh_ppc_forward_fail(paste(label,"contains a nonplain column",name))
  x
}
.bh_ppc_forward_ids <- function(x, label, unique=TRUE) {
  if(!is.character(x)||is.object(x)||anyNA(x)||any(!nzchar(x))||
     (unique&&anyDuplicated(x))) .bh_ppc_forward_fail(paste("invalid saved",label))
  invisible(TRUE)
}
.bh_ppc_forward_index <- function(x, upper, label) {
  if(!is.numeric(x)||is.object(x)||any(!is.finite(x))||any(x!=floor(x))||
     any(x<1|x>upper)) .bh_ppc_forward_fail(paste("invalid saved",label))
  invisible(TRUE)
}
.bh_ppc_forward_names <- function(name, nr, nc=NULL) {
  if(is.null(nc))return(if(nr)paste0(name,"[",seq_len(nr),"]")else character())
  if(!nr||!nc)return(character())
  paste0(name,"[",rep(seq_len(nr),nc),",",rep(seq_len(nc),each=nr),"]")
}
.bh_ppc_forward_plan <- function(spec, level, budget=.bh_ppc_budget_defaults(), saved_joint=FALSE) {
  if(!is.character(level)||length(level)!=1L||is.na(level)||
     !level%in%c("population","conditional"))
    .bh_ppc_forward_fail("level must be explicitly population or conditional")
  if(typeof(spec)!="list") .bh_ppc_forward_fail("a saved frontend specification is required")
  portable<-identical(.bh_pf_class(spec),"bmeas_portable_context")
  if(portable&&.bh_m3cs_joint_spec(spec)&&!isTRUE(saved_joint))
    .mp_abort("E_PORTABLE_UNSUPPORTED","two-block M3-CS prediction requires a live fit; saved execution is not supported")
  cognitive<-.bh_m3cs_predictive_spec(spec)
  spec<-unclass(spec)
  if(!is.list(spec$tables)||is.object(spec$tables)||!is.list(spec$data_map)||
     is.object(spec$data_map)||!is.list(spec$models)||is.object(spec$models))
    .bh_ppc_forward_fail("saved model, data, and role maps must be plain lists")
  tab<-spec$tables
  roster<-.bh_ppc_forward_frame(tab$person_roster,c("person_index","person_id","person_key"),"person roster")
  target<-.bh_ppc_forward_frame(tab$targets,c("key","target_index","level"),"targets")
  component<-.bh_ppc_forward_frame(tab$components,"component","components")
  residual<-.bh_ppc_forward_frame(tab$parameter_residuals,c("target","target_index"),"residual targets")
  scales<-.bh_ppc_forward_frame(tab$observation_parameters,"key","observation scales")
  aug<-tab$augmentation;person<-tab$person_law
  if(!is.list(aug)||is.object(aug)||!is.list(person)||is.object(person))
    .bh_ppc_forward_fail("saved person law and augmentation are required")
  N<-nrow(roster);J<-nrow(target);K<-nrow(component);U<-nrow(residual);S<-nrow(scales);JT<-length(aug$target_index)
  if(JT>J) .bh_ppc_forward_fail("active task dimensions exceed targets","E_PPC_GEOMETRY")
  allocation_accounting <- .bh_ppc_geometry_admit(N,J,K,U,S,JT,budget)
  .bh_ppc_forward_ids(roster$person_key,"person keys")
  .bh_ppc_forward_ids(roster$person_id,"person IDs")
  .bh_ppc_forward_ids(target$key,"target keys")
  .bh_ppc_forward_ids(scales$key,"observation-scale keys")
  .bh_ppc_forward_index(roster$person_index,N,"roster indices")
  .bh_ppc_forward_index(target$target_index,J,"target indices")
  if(!identical(as.integer(roster$person_index),seq_len(N))||
     !identical(as.integer(target$target_index),seq_len(J))||
     !identical(roster$person_key,spec$data_map$subject_keys)||
     !is.numeric(person$roster_size)||length(person$roster_size)!=1L||
     !isTRUE(person$roster_size==N)) .bh_ppc_forward_fail("saved roster/target indexing is inconsistent")
  iid<-identical(person$distribution,"iid")&&identical(aug$type,"common_plus_parameter_residual")
  centered<-identical(person$distribution,"sample_centered")&&identical(aug$type,"task_cholesky")
  if(!iid&&!centered) .bh_ppc_forward_fail("unsupported person/augmentation combination")
  if(centered&&N<2L) .bh_ppc_forward_fail("centered replication needs the full original roster with N>=2")
  if(iid&&JT) .bh_ppc_forward_fail("iid augmentation must not carry task-white targets")
  .bh_ppc_forward_index(aug$target_index,J,"active task indices")
  .bh_ppc_forward_index(residual$target_index,J,"residual target indices")
  if(anyDuplicated(aug$target_index)||anyDuplicated(residual$target_index)||
     !identical(aug$target,target$key[aug$target_index])||
     !identical(residual$target,target$key[residual$target_index]))
    .bh_ppc_forward_fail("saved task/residual target order is inconsistent")
  domain<-.bh_ppc_forward_frame(tab$task_target_domain,c("target","level","has_symbolic_support","task_white_target"),"task domain")
  if(!identical(domain$target,target$key)||!identical(domain$level,target$level)||
     !is.logical(domain$has_symbolic_support)||anyNA(domain$has_symbolic_support)||
     !is.logical(domain$task_white_target)||anyNA(domain$task_white_target)||
     any(!target$level%in%c("subject_task","task"))) .bh_ppc_forward_fail("invalid saved task domain")
  active<-target$key[target$level=="subject_task"&domain$has_symbolic_support]
  if(centered&&(!setequal(aug$target,active)||
      !identical(domain$task_white_target,target$key%in%aug$target)))
    .bh_ppc_forward_fail("centered task order must cover the complete saved active domain")
  if(iid&&any(domain$task_white_target)) .bh_ppc_forward_fail("iid task domain has centered coordinates")
  if(centered&&(!identical(aug$isometric_basis,"Stan2.40_sum_to_zero_positive_Helmert")||
     !isTRUE(aug$raw_coordinate_variance==N/(N-1))))
    .bh_ppc_forward_fail("unsupported centered white basis or normalization")
  roles<-.bh_ppc_forward_frame(tab$parameter_roles,c("key","model","parameter","task","role","level","receives","predictor","fixed_received"),"parameter roles")
  .bh_ppc_forward_ids(roles$key,"role keys")
  for(field in c("model","parameter","task","role","level","receives","predictor"))
    .bh_ppc_forward_ids(roles[[field]],paste("role",field),FALSE)
  ops<-tab$observation_operands
  block_names<-names(spec$models);.bh_ppc_forward_ids(block_names,"block names")
  if(!identical(names(spec$data_map$blocks),block_names)) .bh_ppc_forward_fail("consumed block order differs from saved model order")
  row_counts<-vapply(spec$data_map$blocks,function(x) {
    if(!identical(class(x),"data.frame")) .bh_ppc_forward_fail("consumed rows must be plain data frames")
    nrow(x)
  },integer(1))
  O <- .bh_count_sum(row_counts,"consumed observations")
  if(O>.bh_ppc_stats_cap(budget,"max_observations",1000L))
    .bh_ppc_forward_fail("complete consumed design exceeds max_observations","E_DIAGNOSTIC_BUDGET")
  allocation_accounting$row_map_slots <- .bh_count_product(O,40,name="response and operand map slots")
  .bh_ppc_work_admit(unlist(allocation_accounting),budget)

  op_fields<-c("block","observation_id","argument","target","role","level","receives","predictor","operand","received_constant","eta_index","observation_scale_index")
  if(is.null(ops)&&!sum(row_counts)) {
    ops<-as.data.frame(stats::setNames(rep(list(character()),length(op_fields)),op_fields))
    ops$received_constant<-numeric();ops$eta_index<-ops$observation_scale_index<-integer()
  }
  ops<-.bh_ppc_forward_frame(ops,op_fields,"observation operands")
  if(nrow(ops)>4L*sum(row_counts)) .bh_ppc_forward_fail("surplus saved observation operands")
  for(field in c("block","observation_id","argument","target","role","level","receives","predictor","operand"))
    .bh_ppc_forward_ids(ops[[field]],paste("operand",field),FALSE)
  if(any(!ops$block%in%block_names)) .bh_ppc_forward_fail("observation operands contain an unknown block")
  blocks<-list();used_eta<-character();used_scales<-integer();has_eta<-FALSE
  for(b in block_names) {
    block<-spec$models[[b]]
    if(typeof(block)!="list") .bh_ppc_forward_fail("saved block must be a list")
    block<-unclass(block);model<-block$model
    if(typeof(model)!="list") .bh_ppc_forward_fail("saved observation model must be a list")
    # Both classifiers compare saved descriptors without model construction,
    # callback execution or serialization hashing during passive planning.
    family<-if(cognitive&&.bh_m3cs_kind(model))"m3cs" else
      .bh_pf_basic_kind(unclass(model))
    if(is.na(family)) .bh_ppc_forward_fail(paste(b,"has no canonical Gaussian/binomial/lognormal likelihood contract"),"E_PPC_FAMILY")
    d<-.bh_ppc_forward_frame(spec$data_map$blocks[[b]],c(".observation_id",".person_index",".person_key",".task",if(family=="m3cs")character()else if(family=="binomial")c("successes","trials")else"y"),paste(b,"consumed rows"))
    .bh_ppc_forward_ids(d$.observation_id,paste(b,"observation IDs"))
    .bh_ppc_forward_index(d$.person_index,N,paste(b,"person indices"))
    if(!identical(d$.person_key,roster$person_key[d$.person_index])) .bh_ppc_forward_fail("consumed people differ from full-roster indices")
    args<-switch(family,m3cs=c("a","c","ra","rc"),gaussian=c("mu","sigma"),binomial="p",lognormal=c("meanlog","sdlog"))
    bo<-ops[ops$block==b,,drop=FALSE]
    if(nrow(bo)!=length(args)*nrow(d)||any(!bo$argument%in%args)) .bh_ppc_forward_fail(paste(b,"has incomplete or surplus observation operands"))
    resolve<-function(argument,scale=FALSE) {
      x<-bo[bo$argument==argument,,drop=FALSE]
      if(nrow(x)!=nrow(d)||anyDuplicated(x$observation_id)||!setequal(x$observation_id,d$.observation_id))
        .bh_ppc_forward_fail(paste(b,argument,"has ambiguous row operands"))
      x<-x[match(d$.observation_id,x$observation_id),,drop=FALSE]
      x$person_index<-d$.person_index
      r<-match(x$target,roles$key)
      if(anyNA(r)) .bh_ppc_forward_fail("operand targets are outside the saved role map")
      rr<-roles[r,,drop=FALSE]
      if(any(rr$model!=b|rr$parameter!=argument|rr$task!=d$.task)||
         any(x$role!=if(scale)"observation_scale"else"predictor")||
         any(x$level!=rr$level)||any(x$role!=rr$role)||
         any(x$receives!=rr$receives)||any(x$predictor!=rr$predictor))
        .bh_ppc_forward_fail("observation operand roles disagree with the saved logical target")
      expected_link<-if(family=="m3cs")if(argument%in%c("a","c"))"log"else"logit"else if(scale)"log"else if(family=="binomial")"logit"else"identity"
      expected_receives<-if(family%in%c("binomial","m3cs"))"eta"else"theta"
      if(any(x$predictor!=expected_link)||any(x$receives!=expected_receives)) .bh_ppc_forward_fail("unsupported canonical operand link/receiver")
      fixed<-x$level=="fixed";free<-!fixed
      if(anyNA(fixed)||any(!x$level%in%c("fixed","task","subject_task"))) .bh_ppc_forward_fail("invalid operand variation level")
      if(any(x$operand[fixed]!="fixed_received")||
         any(!is.finite(x$received_constant[fixed]))||
         !identical(x$received_constant[fixed],rr$fixed_received[fixed])||
         (scale&&any(x$received_constant[fixed]<=0))) .bh_ppc_forward_fail("invalid fixed received operand")
      if(scale) {
        if(any(x$level[free]!="task")||any(x$operand[free]!="free_observation_scale")) .bh_ppc_forward_fail("unsupported observation-scale role")
        .bh_ppc_forward_index(x$observation_scale_index[free],S,"observation-scale indices")
        if(!identical(x$target[free],scales$key[x$observation_scale_index[free]])) .bh_ppc_forward_fail("observation-scale target mismatch")
        used_scales<<-c(used_scales,x$observation_scale_index[free])
      }else {
        if(any(x$operand[free]!=ifelse(x$level[free]=="task","task_predictor","individual_predictor"))) .bh_ppc_forward_fail("unsupported predictor operand")
        .bh_ppc_forward_index(x$eta_index[free],J,"predictor indices")
        if(!identical(x$target[free],target$key[x$eta_index[free]])||!identical(x$level[free],target$level[x$eta_index[free]])) .bh_ppc_forward_fail("predictor target/level mismatch")
        has_eta<<-has_eta||any(free)
        if(any(free))used_eta<<-c(used_eta,paste0("eta[",x$person_index[free],",",x$eta_index[free],"]"))
      }
      x
    }
    if(family=="m3cs") {
      operands<-stats::setNames(lapply(args,resolve),args)
      .bh_m3cs_counts(d$counts,d$options,nrow(d))
      blocks[[b]]<-list(family=family,rows=d,operands=operands,mean_operand=operands[[1L]],scale_operand=NULL,
        trials=rowSums(d$counts))
      next
    }
    mean_operand<-resolve(args[1L]);scale_operand<-if(length(args)==2L)resolve(args[2L],TRUE)else NULL
    observed<-if(family=="binomial")d$successes else d$y
    if(!is.numeric(observed)||any(!is.finite(observed))||
       (family=="lognormal"&&any(observed<=0))) .bh_ppc_forward_fail("observed responses violate the canonical support")
    trials<-if(family=="binomial")d$trials else NULL
    if(family=="binomial"&&(!is.numeric(trials)||any(!is.finite(trials))||any(trials<0|trials>.Machine$integer.max|trials!=floor(trials))||any(observed<0|observed>trials|observed!=floor(observed))))
      .bh_ppc_forward_fail("observed binomial successes/trials violate the canonical support")
    blocks[[b]]<-list(family=family,rows=d,mean_operand=mean_operand,scale_operand=scale_operand,trials=trials)
  }
  variables<-character();latent<-0L
  if(level=="conditional")variables<-unique(used_eta)
  else if(has_eta) {
    variables<-.bh_ppc_forward_names("alpha",J)
    if(iid) {
      variables<-c(variables,.bh_ppc_forward_names("effective_loadings",J,K),.bh_ppc_forward_names("D_parameter",J,U),
        .bh_ppc_forward_names("L_component",K,K),.bh_ppc_forward_names("L_parameter",U,U))
      latent<-.bh_count_product(N,.bh_count_sum(c(K,U)))
    }else if(JT) {
      variables<-c(variables,.bh_ppc_forward_names("L_task_work",JT,JT),paste0("task_row_scale[",aug$target_index,"]"))
      latent<-.bh_count_product(JT,N-1L)
    }
  }
  if(length(used_scales))variables<-c(variables,paste0("obs_sd[",sort(unique(used_scales)),"]"))
  law<-if(level=="conditional")"conditional_response"else if(iid)"iid_common_plus_parameter_residual"else"same_N_sample_centered_task_cholesky"
  list(level=level,law=law,geometry=list(N=N,J=J,K=K,U=U,S=S,JT=JT),required_variables=variables,
    latent_values_per_draw=latent,predictor_values_per_draw=.bh_count_product(N,J),
    allocation_accounting=allocation_accounting,
    work_values_per_draw=.bh_count_sum(unlist(allocation_accounting)),
    roster=roster,active_target_index=aug$target_index,residual_target_index=residual$target_index,
    inactive_target_index=which(target$level!="subject_task"|!domain$has_symbolic_support),
    has_eta=has_eta,response_cells_per_row=if(any(vapply(blocks,function(b)identical(b$family,"m3cs"),logical(1))))10L else 1L,blocks=blocks)
}

.bh_ppc_forward_draw <- function(plan, draw) {
  if(!is.numeric(draw)||is.object(draw)||!is.null(dim(draw))||
     (length(draw)&&(is.null(names(draw))||anyNA(names(draw))||anyDuplicated(names(draw)))))
    .bh_ppc_forward_fail("one aligned named numeric posterior state is required","E_PPC_DRAW")
  required<-plan$required_variables
  if(any(!required%in%names(draw))||any(!is.finite(draw[required])))
    .bh_ppc_forward_fail("required joint-state columns are missing or nonfinite","E_PPC_DRAW")
  invisible(TRUE)
}
.bh_ppc_forward_matrix <- function(draw,name,nr,nc,factor=FALSE) {
  x<-matrix(unname(draw[.bh_ppc_forward_names(name,nr,nc)]),nr,nc)
  if(factor&&nr&&(any(x[upper.tri(x)]!=0)||any(diag(x)<=0)))
    .bh_ppc_forward_fail(paste(name,"is not a lower triangular factor with positive diagonal"),"E_PPC_DRAW")
  if(factor&&nr&&name%in%c("L_component","L_parameter")) {
    # Six-significant-digit decimal rounding changes a unit row Gram diagonal
    # by at most about 1e-5 (relative entry error <=5e-6). This explicit 2e-5
    # absolute tolerance admits that CSV precision, without normalizing x.
    # L_task_work is a covariance factor and has no unit-diagonal Gram role.
    gram_diagonal<-rowSums(x*x)
    if(any(!is.finite(gram_diagonal))||any(abs(gram_diagonal-1)>2e-5))
      .bh_ppc_forward_fail(paste(name,"must have unit row Gram diagonal within absolute tolerance 2e-5"),"E_PPC_DRAW")
  }
  x
}
.bh_ppc_forward_finite <- function(x, label) {
  if(any(!is.finite(x))) .bh_ppc_forward_fail(paste(label,"cannot be represented; no clipping or retry is applied"),"E_PPC_NUMERICAL")
  x
}
.bh_ppc_forward_product <- function(left,right,label) {
  # Check one column of one outer-product term at a time: no product tensor,
  # reassociation, alternate summation, jitter, or change to emitted %*% math.
  # A vanished nonzero term refuses conservatively even if other terms would
  # dominate it. Exactly cancelling representable terms remain legitimate.
  for(k in seq_len(ncol(left)))for(j in seq_len(ncol(right))) {
    term<-left[,k]*right[k,j]
    if(any(!is.finite(term)))
      .bh_ppc_forward_fail(paste(label,"has a nonrepresentable scalar product"),"E_PPC_NUMERICAL")
    if(right[k,j]!=0&&any(left[,k]!=0&term==0))
      .bh_ppc_forward_fail(paste(label,"has a nonzero scalar product that underflowed to zero"),"E_PPC_NUMERICAL")
  }
  .bh_ppc_forward_finite(left%*%right,label)
}
.bh_ppc_generate_eta <- function(plan, draw) {
  .bh_ppc_forward_draw(plan,draw)
  g<-plan$geometry;N<-g$N;J<-g$J;K<-g$K;U<-g$U;JT<-g$JT
  eta<-matrix(NA_real_,N,J)
  if(!plan$has_eta)return(eta)
  if(plan$level=="conditional") {
    for(block in plan$blocks)for(x in if(identical(block$family,"m3cs"))block$operands else list(block$mean_operand)) {
      at<-which(x$operand!="fixed_received")
      if(length(at))eta[cbind(x$person_index[at],x$eta_index[at])]<-unname(draw[paste0("eta[",x$person_index[at],",",x$eta_index[at],"]")])
    }
    return(eta)
  }
  alpha<-unname(draw[.bh_ppc_forward_names("alpha",J)])
  eta<-matrix(rep(alpha,each=N),N,J)
  if(plan$law=="iid_common_plus_parameter_residual") {
    A<-.bh_ppc_forward_matrix(draw,"effective_loadings",J,K)
    D<-.bh_ppc_forward_matrix(draw,"D_parameter",J,U)
    Lc<-.bh_ppc_forward_matrix(draw,"L_component",K,K,TRUE)
    Lu<-.bh_ppc_forward_matrix(draw,"L_parameter",U,U,TRUE)
    if(any(A[plan$inactive_target_index,,drop=FALSE]!=0)||any(D[plan$inactive_target_index,,drop=FALSE]!=0))
      .bh_ppc_forward_fail("nonvarying targets have nonzero saved person-effect maps","E_PPC_DRAW")
    if(U) {
      keep<-matrix(FALSE,J,U);keep[cbind(plan$residual_target_index,seq_len(U))]<-TRUE
      if(any(D[!keep]!=0)||any(D[keep]<=0)) .bh_ppc_forward_fail("saved parameter-residual map violates its target/scales","E_PPC_DRAW")
    }
    if(K) {
      Z<-matrix(stats::rnorm(K*N),K,N)
      z<-.bh_ppc_forward_product(Lc,Z,"common-score multiplication")
      eta<-.bh_ppc_forward_finite(eta+.bh_ppc_forward_product(t(z),t(A),"common predictor multiplication"),"common predictor arithmetic")
    }
    if(U) {
      E<-matrix(stats::rnorm(U*N),U,N)
      e<-.bh_ppc_forward_product(Lu,E,"residual-score multiplication")
      eta<-.bh_ppc_forward_finite(eta+.bh_ppc_forward_product(t(e),t(D),"residual predictor multiplication"),"residual predictor arithmetic")
    }
  }else if(JT) {
    L<-.bh_ppc_forward_matrix(draw,"L_task_work",JT,JT,TRUE)
    scale<-unname(draw[paste0("task_row_scale[",plan$active_target_index,"]")])
    if(any(scale<=0)) .bh_ppc_forward_fail("required active task row scales must be positive","E_PPC_DRAW")
    W<-matrix(stats::rnorm(JT*(N-1L),sd=sqrt(N/(N-1L))),JT,N-1L)
    white<-tryCatch(.bh_centered_white_transform(W,N),error=function(e)
      .bh_ppc_forward_fail("centered white transform cannot be represented","E_PPC_NUMERICAL"))
    deviation<-.bh_ppc_forward_product(L,white,"centered task multiplication")
    scaled<-.bh_ppc_forward_finite(deviation*scale,"centered task row scaling")
    if(any(deviation!=0&scaled==0)) .bh_ppc_forward_fail("required centered task row scaling underflowed","E_PPC_NUMERICAL")
    deviation<-scaled
    eta[,plan$active_target_index]<-.bh_ppc_forward_finite(eta[,plan$active_target_index,drop=FALSE]+t(deviation),"centered predictor scatter")
  }
  .bh_ppc_forward_finite(eta,"population predictors")
}
.bh_ppc_forward_operand <- function(x,eta,draw,scale=FALSE) {
  if(!nrow(x))return(numeric())
  value<-x$received_constant;at<-which(x$operand!="fixed_received")
  if(length(at))value[at]<-if(scale)unname(draw[paste0("obs_sd[",x$observation_scale_index[at],"]")])else eta[cbind(x$person_index[at],x$eta_index[at])]
  value<-.bh_ppc_forward_finite(value,"required received operands")
  if(scale&&any(value<=0)) .bh_ppc_forward_fail("required observation scales must be positive","E_PPC_DRAW")
  value
}
.bh_ppc_simulate_responses <- function(plan, draw) {
  eta<-.bh_ppc_generate_eta(plan,draw);out<-list()
  for(b in names(plan$blocks)) {
    block<-plan$blocks[[b]];n<-nrow(block$rows)
    mean<-.bh_ppc_forward_operand(block$mean_operand,eta,draw)
    sd<-if(is.null(block$scale_operand))NULL else .bh_ppc_forward_operand(block$scale_operand,eta,draw,TRUE)
    y<-numeric(n)
    if(n&&block$family=="binomial") {
      informative<-which(block$trials>0)
      if(length(informative)) {
        probability<-stats::plogis(mean[informative])
        if(any(probability<=0|probability>=1)) .bh_ppc_forward_fail("informative binomial inverse-logit saturated numerically","E_PPC_NUMERICAL")
        y[informative]<-stats::rbinom(length(informative),block$trials[informative],probability)
      }
    }else if(n&&block$family=="gaussian")y<-stats::rnorm(n,mean,sd)
    else if(n&&block$family=="lognormal")y<-stats::rlnorm(n,mean,sd)
    y<-.bh_ppc_forward_finite(y,"replicated responses")
    if(block$family=="lognormal"&&any(y<=0)) .bh_ppc_forward_fail("positive lognormal response underflowed to zero","E_PPC_NUMERICAL")
    if(block$family=="binomial"&&any(y<0|y>block$trials|y!=floor(y))) .bh_ppc_forward_fail("replicated binomial response violates its support","E_PPC_NUMERICAL")
    out[[b]]<-stats::setNames(y,block$rows$.observation_id)
  }
  out
}
