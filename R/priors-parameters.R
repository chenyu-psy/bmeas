logistic <- function(location, scale) {
  normal(location,scale)
  list(distribution="logistic",mean=location,sd=scale)
}
student_t <- function(df, location, scale) {
  normal(location,scale)
  if(!is.numeric(df)||length(df)!=1L||!is.finite(df)||df<=0) .mp_abort("E_PRIOR","Student-t degrees of freedom must be positive")
  list(distribution="student_t",mean=location,sd=scale,df=df)
}
cfa_parameter_prior <- function(intercept, loading, predictor, units) {
  if(!intercept$distribution%in%c("normal","logistic","student_t")) .mp_abort("E_PRIOR","supported intercept priors: normal, logistic, student_t")
  normal(intercept$mean,intercept$sd)
  if(intercept$distribution=="student_t") student_t(intercept$df,intercept$mean,intercept$sd)
  if(!identical(loading$distribution,"normal")||loading$mean!=0) .mp_abort("E_PRIOR","loadings require zero-centered normal priors")
  normal(loading$mean,loading$sd)
  predictor<-match.arg(predictor,c("identity","log","logit"));.mp_scalar_string(units,"predictor units")
  list(intercept=intercept,loading=loading,predictor=predictor,units=units)
}
parameter_prior_set <- function(parameters, correlation, nuisance = list()) {
  .mp_names(parameters,"parameter priors")
  for(p in parameters)cfa_parameter_prior(p$intercept,p$loading,p$predictor,p$units)
  if(!identical(correlation$distribution,"lkj")) .mp_abort("E_PRIOR","factor correlation requires LKJ")
  lkj(correlation$shape)
  if(length(nuisance)) {
    .mp_names(nuisance,"nuisance priors")
    for(p in nuisance)if(!identical(p$distribution,"half_normal")) .mp_abort("E_PRIOR","positive nuisance SD requires half-normal") else half_normal(p$sd)
  }
  structure(list(parameters=parameters,nuisance=nuisance,correlation=correlation,source="user_explicit_predictor_priors",
    kind="parameter_specific"),class="measurement_priors")
}
.mp_resolve_parameter_priors <- function(spec) {
  p<-spec$priors;parameter_prior_set(p$parameters,p$correlation,p$nuisance)
  selected<-unlist(spec$structure$factors,use.names=FALSE)
  if(!setequal(names(p$parameters),selected)) .mp_abort("E_PRIOR_COVERAGE","provide one exact-key prior for every hierarchical parameter")
  for(key in selected) {
    row<-spec$parameter_keys[match(key,spec$parameter_keys$key),,drop=FALSE];prior<-p$parameters[[key]]
    if(prior$predictor!=row$predictor) .mp_abort("E_PRIOR_SCALE",paste("prior predictor differs for",key))
    if(prior$units!=row$predictor_units) .mp_abort("E_PRIOR_UNITS",paste("prior predictor units differ for",key))
  }
  nuisance<-unlist(lapply(names(spec$models),function(b) {
    ns<-.mp_model_info(spec$models[[b]]$model)$nuisance
    spec$parameter_keys$key[spec$parameter_keys$model==b & spec$parameter_keys$parameter%in%ns]
  }),use.names=FALSE)
  if(!setequal(names(p$nuisance),nuisance)) .mp_abort("E_PRIOR_COVERAGE","provide exactly the task-level nuisance SD priors")
  p
}
.mp_parameter_prior_table <- function(spec) {
  p<-.mp_resolve_parameter_priors(spec);rows<-list();anchors<-unname(spec$structure$positive_anchors)
  for(key in unlist(spec$structure$factors,use.names=FALSE)) {
    prior<-p$parameters[[key]];intr<-prior$intercept
    rows[[length(rows)+1L]]<-data.frame(parameter=c(paste0("intercept:",key),paste0("loading:",key)),
      distribution=c(intr$distribution,if(key%in%anchors)"half_normal"else"normal"),location=c(intr$mean,0),
      scale=c(intr$sd,prior$loading$sd),df=c(if(intr$distribution=="student_t")intr$df else NA_real_,NA_real_),
      support=c("real",if(key%in%anchors)"positive"else"real"),prior_scale=paste0("predictor:",prior$predictor),
      units=prior$units,source=p$source,stringsAsFactors=FALSE)
  }
  for(key in names(p$nuisance)) rows[[length(rows)+1L]]<-data.frame(parameter=paste0(strsplit(key,"::",fixed=TRUE)[[1]][2],":",key),
    distribution="half_normal",location=0,scale=p$nuisance[[key]]$sd,df=NA_real_,support="positive",prior_scale="positive SD, not its log",
    units=spec$parameter_keys$units[match(key,spec$parameter_keys$key)],source=p$source)
  if(length(spec$structure$factors)>1L) rows[[length(rows)+1L]]<-data.frame(parameter="factor_correlation",distribution="lkj",location=NA_real_,scale=p$correlation$shape,df=NA_real_,support="correlation_matrix",prior_scale="unit_variance_factor",units="unitless",source=p$source)
  rows[[length(rows)+1L]]<-data.frame(parameter="factor_z_raw[*]",distribution="normal",location=0,scale=1,df=NA_real_,support="real",prior_scale="noncentered_standard_normal",units="unitless",source="structure")
  do.call(rbind,rows)
}
