# Capture the same safe target language as bhmf, without retaining formula environments.
.bh_capture_prior_target <- function(target) {
  if(is.null(target))return(NULL)
  if(inherits(target,"bhmm_prior_targets")) {
    original<-target;fingerprint<-original$fingerprint;original$fingerprint<-NULL
    if(!identical(fingerprint,.mp_hash(original)))
      .mp_abort("E_PRIOR_SELECTOR","captured prior target selector changed; construct it again with set_prior()")
    parts<-target$targets
    if(!is.list(parts)||!length(parts)).mp_abort("E_PRIOR_SELECTOR","prior target selector is empty")
    for(part in parts) {
      expected<-if(identical(part$kind,"wide"))c("kind","column")else if(identical(part$kind,"target"))c("kind","block","parameter","task")else character()
      if(!length(expected)||!identical(names(part),expected)||any(!vapply(part,function(x)typeof(x)=="character"&&!is.object(x)&&length(x)==1L&&!is.na(x)&&nzchar(x),logical(1))))
        .mp_abort("E_PRIOR_SELECTOR","invalid captured prior target descriptor")
      for(field in setdiff(expected,"kind")) .mp_key_component(part[[field]],paste("prior",field))
    }
    if(anyDuplicated(vapply(parts,.bh_target_descriptor_id,character(1))))
      .mp_abort("E_PRIOR_SELECTOR","prior target group contains a duplicate target")
    return(target)
  }
  if(inherits(target,"formula")) {
    if(length(target)!=2L).mp_abort("E_PRIOR_SELECTOR","prior targets need a one-sided formula")
    parts<-.bh_targets_ast(target[[2L]],environment(target))
    if(anyDuplicated(vapply(parts,.bh_target_descriptor_id,character(1))))
      .mp_abort("E_PRIOR_SELECTOR","prior target group contains a duplicate target")
    out<-structure(list(targets=parts),class="bhmm_prior_targets")
    out$fingerprint<-.mp_hash(out)
    return(.bh_capture_prior_target(out))
  }
  if(typeof(target)!="character"||is.object(target)) .mp_abort("E_PRIOR_SELECTOR","canonical target selectors must be plain character strings or a supported target formula")
  .mp_scalar_string(target,"canonical target key or one-sided target formula")
  target
}
.bh_resolve_prior_rule <- function(rule,spec,table) {
  selected<-table$class==rule$class
  if(rule$class=="cor")selected<-selected & table$layer==rule$layer
  if(!is.null(rule$component))selected<-selected & !is.na(table$component) & table$component==rule$component
  for(field in c("decomposition","stratum","term","coordinate","block"))if(!is.null(rule[[field]])) {
    if(!field%in%names(table)).mp_abort("E_PRIOR_SELECTOR",paste("no",field,"prior roles in this model"))
    selected<-selected & !is.na(table[[field]]) & table[[field]]%in%rule[[field]]
  }
  target_keys<-NULL
  if(!is.null(rule$target)) {
    target_keys<-if(is.character(rule$target))rule$target else vapply(rule$target$targets,function(t) {
      if(t$kind=="target")return(targets(t$block,t$parameter,t$task))
      if(!identical(spec$observation_key_origin,"input-row and selected-column positions; no deduplication"))
        .mp_abort("E_PRIOR_SELECTOR","explicit observation blocks require block$parameter[tasks] prior targets; bare task names are not guessed")
      parameter<-if(rule$class%in%c("Intercept","loading","parameter_sd","total_sd"))"mu"else if(rule$class=="sigma")"sigma"else
        .mp_abort("E_PRIOR_SELECTOR","wide Gaussian indicator selectors do not provide this prior role")
      targets("scores",parameter,t$column)
    },character(1))
    # Distinct syntax can resolve to the same key; never multiply that prior.
    if(anyDuplicated(target_keys)).mp_abort("E_PRIOR_SELECTOR","prior group resolves the same target more than once")
    unknown<-setdiff(target_keys,spec$parameter_keys$key)
    if(length(unknown)).mp_abort("E_PRIOR_SELECTOR",paste("unknown prior targets:",paste(unknown,collapse=", ")))
    matched<-vapply(target_keys,function(key)any(selected & !is.na(table$target) & table$target==key),logical(1))
    if(any(!matched)).mp_abort("E_PRIOR_SELECTOR",paste("prior targets have no free parameter in the requested role/component:",paste(target_keys[!matched],collapse=", ")))
    selected<-selected & !is.na(table$target) & table$target%in%target_keys
  }
  for(field in c("decomposition","stratum","term","coordinate","block"))if(!is.null(rule[[field]]))
    if(any(!rule[[field]]%in%table[[field]][selected])).mp_abort("E_PRIOR_SELECTOR",paste("every explicit",field,"selector must match a free parameter under all restrictions"))
  if(!any(selected)).mp_abort("E_PRIOR_SELECTOR","prior selector matches no free parameter; fixed-basis loading cells use one class=amplitude prior selected by their component")
  rule$resolved_targets<-if(is.null(target_keys))NULL else sort(target_keys)
  rule$selected<-selected
  rule
}
.bh_prior_refines <- function(a,b) {
  # Compare predicates, not accidental equality of their realized coefficient sets.
  # Explicit restriction still refines NULL when it covers today's whole model.
  restriction<-function(x,y) {
    if(is.null(y))return(c(compatible=TRUE,strict=!is.null(x)))
    if(is.null(x))return(c(compatible=FALSE,strict=FALSE))
    c(compatible=all(x%in%y),strict=length(unique(x))<length(unique(y)))
  }
  component<-restriction(a$component,b$component)
  target<-restriction(a$resolved_targets,b$resolved_targets)
  dimensions<-c(list(component,target),lapply(c("decomposition","stratum","term","coordinate","block"),function(f)restriction(a[[f]],b[[f]])))
  all(vapply(dimensions,`[[`,logical(1),"compatible"))&&any(vapply(dimensions,`[[`,logical(1),"strict"))
}
