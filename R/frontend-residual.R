# Residual declarations use exactly the same captured target AST as loadings.
.bh_target_descriptor_id <- function(t) .mp_hash(t)
.bh_unwrap <- function(x) {
  while(is.call(x)&&identical(x[[1L]],as.name("("))&&length(x)==2L)x<-x[[2L]]
  x
}
.bh_parse_residual <- function(residual,owners=list()) {
  extra<-unlist(lapply(owners,function(o)if(o$residual)o$targets else list()),recursive=FALSE)
  if(is.null(residual)&&!length(extra))return("none")
  if(is.null(residual))residual<-list()
  if(!is.list(residual)||(length(residual)&&is.null(names(residual)))||anyDuplicated(names(residual))||
     any(!names(residual)%in%c("targets","cor"))||(is.null(residual$targets)&&!length(extra)))
    .mp_abort("E_RESIDUAL","residual needs targets=~... and optional cor; cross-layer correlations and extra scales are unsupported")
  targets <- if(is.null(residual$targets))list()else .bh_formula_targets(residual$targets)
  global_keys<-.bh_target_keys(targets)
  forbidden<-unlist(lapply(owners,function(o)if(!o$residual)o$target_keys else character()),use.names=FALSE)
  if(length(intersect(global_keys,forbidden))) .mp_abort("E_SHARE_RESIDUAL","residual=FALSE conflicts with an existing global residual target")
  targets<-c(targets,extra)
  targets<-targets[!duplicated(vapply(targets,.bh_target_descriptor_id,character(1)))]
  ids <- vapply(targets,.bh_target_descriptor_id,character(1))
  resolve <- function(rule,components) {
    if(!inherits(rule,"formula")||length(rule)!=2L).mp_abort("E_CORRELATION_SYNTAX","residual correlation rules require one-sided formulas")
    rhs<-.bh_unwrap(rule[[2L]]);env<-environment(rule)
    selector<-function(x)vapply(.bh_targets_ast(x,env),.bh_target_descriptor_id,character(1))
    cross<-is.call(rhs)&&identical(rhs[[1L]],as.name(":"))&&length(rhs)==3L
    if(cross) {
      a<-selector(rhs[[2L]]);b<-selector(rhs[[3L]])
      if(length(intersect(a,b))).mp_abort("E_CORRELATION_SYNTAX","residual cross groups must be disjoint")
      selected<-c(a,b);pairs<-as.matrix(expand.grid(a=a,b=b,stringsAsFactors=FALSE))
    }else {
      selected<-selector(rhs)
      if(length(selected)<2L).mp_abort("E_CORRELATION_SYNTAX","a residual correlation rule needs two or more targets")
      pairs<-t(utils::combn(selected,2L))
    }
    if(anyDuplicated(selected)||!all(selected%in%components))
      .mp_abort("E_CORRELATION_TARGET","residual correlation rules must select distinct declared residual targets using the same target syntax")
    t(apply(pairs,1L,function(p)sort(match(p,components))))
  }
  result<-list(targets=targets,correlations=.bh_cor_table(if(is.null(residual$cor))"independent"else residual$cor,ids,resolve))
  if(length(owners)) {
    keys<-.bh_target_keys(targets)
    owner_keys<-unlist(lapply(owners,function(o)if(o$residual)o$target_keys else character()),use.names=FALSE)
    owner_names<-unlist(lapply(owners,function(o)if(o$residual)rep(o$owner,length(o$target_keys))else character()),use.names=FALSE)
    background<-if(is.null(residual$cor))"zero"else if(is.character(residual$cor))if(residual$cor=="free")"free"else"zero"else residual$cor$default
    result$provenance<-data.frame(target=keys,global_declared=keys%in%global_keys,owner_declared=keys%in%owner_keys,
      owner=owner_names[match(keys,owner_keys)],correlation_background=background,
      correlation_scope="resolved_global_and_owner_residual_union",correlation_explicit=!is.null(residual$cor),stringsAsFactors=FALSE)
    result$provenance$declaration_source<-ifelse(result$provenance$global_declared&result$provenance$owner_declared,"global_and_owner",ifelse(result$provenance$global_declared,"global","owner"))
  }
  result
}
.bh_mask <- function(correlations,names) {
  n<-length(names);out<-matrix("zero",n,n,dimnames=list(names,names));diag(out)<-"fixed_one"
  for(i in seq_len(nrow(correlations)))out[correlations$row[i],correlations$col[i]]<-out[correlations$col[i],correlations$row[i]]<-correlations$state[i]
  out
}
