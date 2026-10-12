# Deterministic initial values are an execution recipe, not scientific priors.
# This screen covers primitive priors, inverse transforms and typed operands;
# it does not evaluate arbitrary registered likelihoods or qualify HMC.
.bh_initial_prior_check <- function(value,distribution,location,scale,df=1,positive=FALSE) {
  if(!is.finite(value)||positive&&value<=0)return("initial value is outside finite primitive support")
  if(positive&&(!is.finite(log(value))||!is.finite(exp(log(value)))||exp(log(value))<=0))
    return("positive-coordinate inverse transform cannot be represented")
  inv<-1/scale;delta<-value-location;y<-delta*inv
  if(any(!is.finite(c(inv,delta,y))))return("prior centering or reciprocal scale cannot be represented")
  if(distribution%in%c(1L,4L)) {
    kernel<- -.5*y*y;gradient<- -inv*y
    density<-stats::dnorm(value,location,scale,log=TRUE)
  }else if(distribution==2L) {
    density<-stats::dlogis(value,location,scale,log=TRUE)
    kernel<-density;gradient<-(1-2*stats::plogis(y))*inv
  }else {
    square<-((value-location)/scale)^2;ratio<-square/df
    kernel<- -(.5*df+.5)*log1p(ratio)
    gradient<- -(df+1)*delta/((1+ratio)*scale^2*df)
    stable<- -((1+1/df)/(1+ratio))*((value-location)/scale)/scale
    if(any(!is.finite(c(square,ratio,stable)))||!is.finite(gradient)||
       abs(gradient-stable)>1e-8*max(abs(stable),.Machine$double.xmin))
      return("Student-t initial derivative arithmetic is outside the screened numeric range")
    density<-stats::dt((value-location)/scale,df,log=TRUE)-log(scale)
  }
  if(any(!is.finite(c(kernel,gradient,density))))return("initial prior density or derivative is not finite")
  NULL
}
.bh_initialization <- function(spec) {
  scope<-"primitive prior/support/inverse-transform and typed-operand screen only; arbitrary likelihood evaluation, extreme-state gradients and HMC remain unqualified"
  empty<-data.frame(parameter=character(),variable=character(),index=integer(),value=numeric(),policy=character(),status=character(),reason=character(),stringsAsFactors=FALSE)
  out<-list(status="not_assessed",recipe_version="role_scale_aware_v1",primitives=empty,reasons=character(),scope=scope)
  if(.bh_has_affine_correlations(spec)) {
    available<-.bh_affine_lowering_capability(spec)
    if(available$status!="bounded_source_available"){out$reasons<-available$reason;return(out)}
  }
  if(.bh_reference_mode(spec)&&!.bh_reference_lowering_available()){out$reasons<-"reference person/correlation/augmentation initialization is not yet lowered; no iid latent recipe is reused";return(out)}
  if(.bh_reference_mode(spec))out$recipe_version<-"reference_augmented_role_scale_aware_v1"
  if(.bh_has_affine_correlations(spec))out$recipe_version<-if(.bh_reference_mode(spec))"reference_affine_correlation_role_scale_v1"else"affine_correlation_role_scale_v1"
  if(.bh_centered_final_marginal_mode(spec))out$recipe_version<-"centered_final_marginal_role_scale_v1"
  tab<-spec$tables$priors
  if(any(tab$status=="missing")||any(!is.na(.bh_resolved_prior_errors(tab)))) {
    out$reasons<-"resolved priors must be complete and valid before initialization can be screened";return(out)
  }
  pd<-.bh_prior_data(spec);loads<-.bh_free_loadings(spec);marked<-.bh_free_component_sd(spec)
  cells<-spec$tables$ordinary_loading_map
  marked_beta<-unique(cells$parameter[cells$state=="free" & cells$component_index%in%marked$component_index])
  values<-list();rows<-list();prior_terms<-list()
  add<-function(variable,ids,dist,loc,scale,df,legacy,positive=FALSE,preferred=NULL,preferred_policy="legacy_finite_start") {
    vals<-numeric(length(ids))
    for(i in seq_along(ids)) {
      first<-if(is.null(preferred))legacy[i]else preferred[i]
      policy<-if(is.null(preferred))"legacy_finite_start"else preferred_policy[i]
      reason<-.bh_initial_prior_check(first,dist[i],loc[i],scale[i],df[i],positive)
      if(!is.null(reason)) {
        fallback<-if(positive)scale[i]else loc[i]
        second<-.bh_initial_prior_check(fallback,dist[i],loc[i],scale[i],df[i],positive)
        if(is.null(second)){first<-fallback;reason<-NULL;policy<-"resolved_prior_fallback"}else reason<-paste(reason,second,sep="; ")
      }
      vals[i]<-first
      y<-(first-loc[i])/scale[i]
      density<-if(dist[i]%in%c(1L,4L))stats::dnorm(first,loc[i],scale[i],log=TRUE)else if(dist[i]==2L)stats::dlogis(first,loc[i],scale[i],log=TRUE)else stats::dt(y,df[i],log=TRUE)-log(scale[i])
      kernel<-if(dist[i]%in%c(1L,4L)) -.5*y*y else if(dist[i]==2L)density+log(scale[i])else -(.5*df[i]+.5)*log1p(y*y/df[i])
      prior_terms[[length(prior_terms)+1L]]<<-stats::setNames(unname(c(density+if(positive)log(2)else 0,kernel,if(positive)log(first)else 0)),c("normalized","proportional","support_jacobian"))
      rows[[length(rows)+1L]]<<-data.frame(parameter=ids[i],variable=variable,index=i,value=first,policy=policy,
        status=if(is.null(reason))"screened"else"numerically_unavailable",reason=if(is.null(reason))""else reason,stringsAsFactors=FALSE)
    }
    values[[variable]]<<-vals
  }
  positive<-function(variable,ids,prefix,start,prefer_scale=FALSE) {
    n<-length(ids);scale<-pd[[paste0(prefix,"_scale")]]
    add(variable,ids,pd[[paste0(prefix,"_dist")]],rep(0,n),scale,pd[[paste0(prefix,"_df")]],rep(start,n),TRUE,
      if(prefer_scale)scale else NULL,rep("resolved_positive_prior_scale",n))
  }
  positive("component_sd",marked$parameter,"component_sd",.5,TRUE)
  positive("total_sd",spec$tables$share_targets$parameter,"total_sd",.5)
  add("share_beta",spec$tables$share_coefficients$parameter,rep(1L,length(pd$share_beta_scale)),pd$share_beta_location,pd$share_beta_scale,rep(1,length(pd$share_beta_scale)),pd$share_beta_location)
  positive("amplitude",.bh_amplitudes(spec)$parameter,"amplitude",.5)
  add("alpha",vapply(spec$tables$targets$key,function(key)paste0("Intercept:",key),character(1)),pd$alpha_dist,pd$alpha_location,pd$alpha_scale,pd$alpha_df,pd$alpha_location)
  anchor<-loads$sign=="positive_anchor"
  for(kind in c(TRUE,FALSE)) {
    at<-which(anchor==kind);preferred<-rep(.5,length(at));policy<-rep("legacy_finite_start",length(at))
    scaled<-loads$parameter[at]%in%marked_beta & !kind;preferred[scaled]<-0;policy[scaled]<-"marked_signed_beta_prior_center"
    add(if(kind)"loading_anchor"else"loading_free",loads$parameter[at],rep(1L,length(at)),rep(0,length(at)),pd$loading_scale[at],rep(1,length(at)),rep(.5,length(at)),kind,preferred,policy)
  }
  positive("obs_sd",vapply(spec$tables$observation_parameters$key,function(key)paste0("observation_sd:",key),character(1)),"sd",1)
  positive("parameter_sd",.bh_free_parameter_sd(spec)$parameter,"parameter_sd",.5)
  if(.bh_raw_cor(spec)) {ids<-spec$tables$raw_correlation_coordinates$parameter;n<-length(ids);add("correlation_B",ids,rep(1L,n),rep(0,n),pd$raw_cor_scale,rep(1,n),rep(0,n))}
  out$primitives<-if(length(rows))do.call(rbind,rows)else empty
  out$reasons<-out$primitives$reason[out$primitives$status!="screened"]
  totals<-if(length(prior_terms))colSums(do.call(rbind,prior_terms))else c(normalized=0,proportional=0,support_jacobian=0)
  out$primitive_prior_totals<-totals
  if(any(!is.finite(totals))||!is.finite(totals[["proportional"]]+totals[["support_jacobian"]]))
    out$reasons<-c(out$reasons,"aggregate primitive prior or support-Jacobian accumulation is not finite; no scientific prior or data was changed")
  for(layer in c("component","parameter")) {
    if(layer=="component"&&.bh_raw_cor(spec))next
    size<-if(layer=="component")nrow(spec$tables$components)else nrow(spec$tables$parameter_residuals)
    shape<-pd[[if(layer=="component")"cor_eta"else"parameter_cor_eta"]]
    if(size&&!is.finite(2*(shape-1)))out$reasons<-c(out$reasons,paste(layer,"correlation prior arithmetic is not representable at identity"))
  }
  if(!.bh_raw_cor(spec))values$correlation_raw<-rep(0,.bh_cor_primitive_count(spec$tables$correlations))
  values$parameter_correlation_raw<-rep(0,.bh_cor_primitive_count(spec$tables$parameter_correlations))
  if(.bh_final_correlation_reporting(spec)) {
    cor_initial<-.bh_affine_initial_correlations(spec,pd)
    out$correlation_initialization<-cor_initial$table;out$correlation_scientific<-cor_initial$scientific
    out$joint_correlation_prior_kernel<-cor_initial$kernel;out$correlation_chart_log_jacobian<-cor_initial$log_jacobian
    out$reasons<-c(out$reasons,cor_initial$reasons)
    aggregate<-totals[["proportional"]]+totals[["support_jacobian"]]+cor_initial$kernel+cor_initial$log_jacobian
    out$aggregate_kernel_with_correlation_charts<-aggregate
    if(!is.finite(aggregate))out$reasons<-c(out$reasons,"aggregate initial primitive/correlation kernel and chart Jacobians are not finite")
  }
  if(!length(out$reasons)) {
    failure<-tryCatch({
      layout<-.bh_covariance_layout(spec);ix<-layout$indices;v<-numeric(sum(layout$counts))
      loading<-numeric(nrow(loads));loading[anchor]<-values$loading_anchor;loading[!anchor]<-values$loading_free
      parts<-list(loading=loading,component_sd=values$component_sd,amplitude=values$amplitude,total_sd=values$total_sd,share=values$share_beta,
        component_cor=if(.bh_raw_cor(spec))values$correlation_B else if(.bh_final_correlation_reporting(spec))out$correlation_scientific$component else values$correlation_raw,parameter_sd=values$parameter_sd,parameter_cor=if(.bh_final_correlation_reporting(spec))out$correlation_scientific$parameter else values$parameter_correlation_raw,observation_variance=rep(1,length(ix$observation_variance)))
      for(name in names(parts))v[ix[[name]]]<-parts[[name]]
      if(.bh_task_augmentation(spec)).bh_task_state(spec,v,values$alpha)else .bh_covariance_state(spec,v,covariance=FALSE)
      roles<-spec$tables$parameter_roles
      for(i in which(roles$role=="predictor" & roles$free_parameter)) {
        value<-values$alpha[match(roles$key[i],spec$tables$targets$key)]
        parameter<-spec$models[[roles$model[i]]]$model$parameters[[roles$parameter[i]]]
        .bh_fixed_representation(parameter,value,"predictor")
      }
      NULL
    },error=function(e)conditionMessage(e))
    if(!is.null(failure))out$reasons<-c(out$reasons,failure)
  }
  out$status<-if(length(out$reasons))"numerically_unavailable"else"screened"
  out$values<-values
  out
}
.bh_initialization_gate <- function(initialization) {
  if(!identical(initialization$status,"screened")) .mp_abort("E_INITIALIZATION_NUMERICAL",paste(
    "The deterministic initializer could not represent a finite supported start. Inspection and code generation remain available; priors/data were not changed.",
    paste(unique(initialization$reasons),collapse="; ")))
  invisible(initialization)
}
.bh_materialize_init <- function(spec,initialization) {
  if(!identical(initialization$status,"screened"))return(NULL)
  x<-initialization$values;N<-length(spec$data_map$subject_keys)
  if(.bh_task_augmentation(spec))x$z_task<-matrix(0,length(spec$tables$augmentation$target),N)else {
    x$z_raw<-matrix(0,nrow(spec$tables$components),N);x$e_raw<-matrix(0,nrow(spec$tables$parameter_residuals),N)
  }
  order<-c("component_sd","total_sd","share_beta","amplitude","alpha","loading_anchor","loading_free","obs_sd",
    if(.bh_raw_cor(spec))"correlation_B"else"correlation_raw",if(!.bh_task_augmentation(spec))"z_raw","parameter_sd","parameter_correlation_raw",if(.bh_task_augmentation(spec))"z_task"else"e_raw")
  x<-x[order]
  x<-Filter(function(value)length(value)>0L,x)
  # All scalar-valued free roles are declared Stan vectors, even at length1.
  # Keep matrix/array score ranks and genuinely empty roles distinct.
  for(name in names(x))if(is.null(dim(x[[name]]))&&length(x[[name]])==1L)x[[name]]<-array(x[[name]],dim=1L)
  x
}
