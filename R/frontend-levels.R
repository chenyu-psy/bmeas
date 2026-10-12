# Scientific role belongs to the observation model; variation belongs to a block.
# Fixed values are data operands, never narrow priors or fake latent coordinates.
.bh_parameter_role <- function(parameter) {
  if(!is.null(parameter$role))return(parameter$role)
  if(identical(parameter$levels,"subject_task"))return("predictor")
  if(identical(parameter$levels,"task"))return("observation_scale")
  .mp_abort("E_PARAMETER_ROLE","a nonstandard default level requires an explicit parameter role; a level vector is not an executable default")
}
.bh_parameter_tasks <- function(block,parameter) {
  if(!is.null(block$parameter_bindings)) {
    domain<-block$parameter_bindings$domains
    sort(unique(domain$task[domain$parameter==parameter]))
  }else sort(unique(block$data$.task))
}
.bh_plain_names <- function(x) typeof(x)=="character"&&!is.object(x)&&is.null(attributes(x))&&
  !anyNA(x)&&all(nzchar(x))&&!anyDuplicated(x)
.bh_capture_parameter_levels <- function(model,parameters) {
  if(is.null(parameters))return(NULL)
  if(!is.list(parameters)||is.object(parameters)||any(!names(attributes(parameters))%in%"names")||
     !length(parameters)||!.bh_plain_names(names(parameters))||!all(names(parameters)%in%names(model$parameters)))
    .mp_abort("E_PARAMETER_LEVEL","parameters must be a plain named list of distinct existing scientific parameter names")
  out<-lapply(names(parameters),function(name) {
    value<-parameters[[name]]
    if(typeof(value)=="character"&&!is.object(value)&&is.null(attributes(value))&&length(value)==1L&&!is.na(value)&&value%in%c("subject_task","task"))
      return(list(level=value))
    if(!is.list(value)||is.object(value)||any(!names(attributes(value))%in%"names")||
       !.bh_plain_names(names(value))||!"fixed"%in%names(value)||any(!names(value)%in%c("fixed","scale")))
      .mp_abort("E_PARAMETER_LEVEL",paste(name,"needs subject_task, task, or list(fixed=..., scale=...)"))
    number<-value$fixed
    if(!typeof(number)%in%c("integer","double")||is.object(number)||!length(number)||
       any(!names(attributes(number))%in%"names")||any(!is.finite(number)))
      .mp_abort("E_PARAMETER_FIXED","fixed values must be finite plain real numeric scalars or exactly named task vectors")
    labels<-names(number)
    if(is.null(labels)&&length(number)!=1L||!is.null(labels)&&!.bh_plain_names(labels))
      .mp_abort("E_PARAMETER_FIXED","an unnamed fixed value must be scalar; named values need distinct plain task names")
    scale<-if(is.null(value$scale))"natural"else value$scale
    if(typeof(scale)!="character"||is.object(scale)||!is.null(attributes(scale))||length(scale)!=1L||is.na(scale)||!scale%in%c("natural","predictor","received"))
      .mp_abort("E_PARAMETER_SCALE","fixed scale must be natural, predictor or received")
    list(level="fixed",value=stats::setNames(as.numeric(number),labels),scale=scale)
  })
  names(out)<-names(parameters)
  captured<-list(choices=out,version="block-parameter-levels-v1")
  captured$fingerprint<-.mp_hash(captured)
  captured
}
.bh_validate_parameter_levels <- function(model,captured) {
  if(is.null(captured))return(invisible(TRUE))
  original<-captured;original$fingerprint<-NULL
  if(!identical(captured$fingerprint,.mp_hash(original))) .mp_abort("E_SPEC_CHANGED","captured parameter-level choices changed; construct observe() again")
  if(!identical(captured$version,"block-parameter-levels-v1")) .mp_abort("E_PARAMETER_LEVEL","unknown parameter-level schema")
  raw<-lapply(captured$choices,function(x)if(identical(x$level,"fixed"))list(fixed=x$value,scale=x$scale)else x$level)
  if(!identical(.bh_capture_parameter_levels(model,raw),captured)) .mp_abort("E_PARAMETER_LEVEL","captured parameter-level choices are malformed")
  invisible(TRUE)
}
.bh_fixed_representation <- function(parameter,value,scale) {
  theta_input<-identical(scale,"natural")||(identical(scale,"received")&&parameter$receives=="theta")
  theta<-eta<-NA_real_;theta_status<-eta_status<-"not_available"
  if(theta_input) {
    theta<-value
    if(parameter$support=="positive"&&theta<=0||parameter$support=="unit_interval"&&(theta<0||theta>1))
      .mp_abort("E_PARAMETER_FIXED_SUPPORT","fixed natural/received-theta value is outside the declared parameter support")
    theta_status<-"supplied_exact"
    eta<-switch(parameter$predictor,identity=theta,log=log(theta),logit=stats::qlogis(theta))
    eta_status<-if(is.finite(eta))"available"else"no_finite_predictor"
    if(parameter$receives=="eta"&&!is.finite(eta))
      .mp_abort("E_PARAMETER_RECEIVER_UNQUALIFIED","the declared natural endpoint requires an infinite received predictor; finite-receiver binding is the current implementation limit, not an invalid scientific probability")
    received<-if(parameter$receives=="eta")eta else theta
  }else {
    eta<-value;eta_status<-"supplied_exact"
    theta<-switch(parameter$predictor,identity=eta,log=exp(eta),logit=stats::plogis(eta))
    invalid<-!is.finite(theta)||(parameter$support=="positive"&&theta<=0)||
      (parameter$support=="unit_interval"&&(theta<=0||theta>=1))
    theta_status<-if(invalid)"optional_transform_not_representable"else"available"
    if(parameter$receives=="theta"&&invalid)
      .mp_abort("E_PARAMETER_NUMERICAL","the required received-theta transform cannot represent this finite predictor faithfully; no clipping or endpoint substitution is applied")
    received<-if(parameter$receives=="eta")eta else theta
  }
  if(!is.finite(received)).mp_abort("E_PARAMETER_NUMERICAL","fixed received argument must be finite")
  list(received=received,predictor=if(is.finite(eta))eta else NA_real_,natural=if(is.finite(theta))theta else NA_real_,
    predictor_status=eta_status,natural_status=theta_status)
}
.bh_parameter_role_table <- function(blocks,mapped) {
  # Domains are frozen before missing-response exclusion. An excluded task can
  # leave a free mean uninformed; it cannot silently erase a declared parameter.
  original_map<-list(blocks=lapply(blocks,`[[`,"data"))
  keys<-.mp_parameter_keys(blocks,original_map)
  rows<-lapply(seq_len(nrow(keys)),function(i) {
    key<-keys[i,,drop=FALSE];block<-blocks[[key$model]];parameter<-block$model$parameters[[key$parameter]]
    .bh_validate_parameter_levels(block$model,block$parameter_levels)
    if(length(parameter$levels)!=1L||!parameter$levels%in%c("subject_task","task","fixed"))
      .mp_abort("E_PARAMETER_LEVEL","parameter_spec levels must supply one explicit executable default, not an ambiguous menu")
    role<-.bh_parameter_role(parameter)
    choice<-block$parameter_levels$choices[[key$parameter]]
    level<-if(is.null(choice))parameter$levels else choice$level
    if(role=="observation_scale"&&level=="subject_task") .mp_abort("E_PARAMETER_LEVEL_UNSUPPORTED","an observation-scale role cannot be promoted to a person-varying predictor by a block override")
    if(role=="observation_scale"&&(parameter$support!="positive"||parameter$receives!="theta"))
      .mp_abort("E_PARAMETER_ROLE","observation-scale roles require positive support received on theta")
    fixed<-list(received=NA_real_,predictor=NA_real_,natural=NA_real_,predictor_status="free",natural_status="free")
    supplied<-NA_real_;scale<-NA_character_
    if(level=="fixed") {
      if(is.null(choice$value)) .mp_abort("E_PARAMETER_FIXED",paste("fixed default requires an explicit binding value for",key$parameter))
      value<-choice$value;domain<-.bh_parameter_tasks(block,key$parameter)
      if(!is.null(names(value))&&!setequal(names(value),domain)) .mp_abort("E_PARAMETER_FIXED_TASKS",paste("fixed task map must match the complete logical domain for",key$parameter))
      supplied<-if(is.null(names(value)))value else value[[key$task]];scale<-choice$scale
      fixed<-.bh_fixed_representation(parameter,supplied,scale)
    }
    cbind(key,data.frame(role=role,level=level,level_source=if(is.null(choice))"model_default"else"block_override",
      supplied_scale=scale,supplied_value=supplied,fixed_received=fixed$received,fixed_predictor=fixed$predictor,fixed_natural=fixed$natural,
      supplied_units=if(is.na(scale))NA_character_ else if(scale=="natural"||(scale=="received"&&parameter$receives=="theta"))key$units else key$predictor_units,
      received_units=if(parameter$receives=="eta")key$predictor_units else key$units,
      predictor_representation=fixed$predictor_status,natural_representation=fixed$natural_status,
      free_parameter=level!="fixed",individual_variation=role=="predictor"&&level=="subject_task",stringsAsFactors=FALSE))
  })
  out<-do.call(rbind,rows);rownames(out)<-NULL;out
}
.bh_gaussian_responses <- function(spec) {
  roles<-spec$tables$parameter_roles
  if(is.null(roles)) {
    out<-spec$tables$targets
    out$eta_index<-out$target_index;out$mean_level<-"subject_task";out$fixed_mean<-NA_real_
    out$noise_key<-vapply(seq_len(nrow(out)),function(i)targets(out$model[i],"sigma",out$task[i]),character(1))
    out$noise_free<-TRUE;out$fixed_sigma<-NA_real_
    return(out)
  }
  keep<-vapply(seq_len(nrow(roles)),function(i)roles$parameter[i]=="mu"&&identical(.bh_builtin_kind(spec$models[[roles$model[i]]]$model),"gaussian"),logical(1))
  out<-roles[keep,,drop=FALSE]
  out$eta_index<-match(out$key,spec$tables$targets$key);out$mean_level<-out$level;out$fixed_mean<-out$fixed_received
  out$noise_key<-vapply(seq_len(nrow(out)),function(i)targets(out$model[i],"sigma",out$task[i]),character(1))
  noise<-roles[match(out$noise_key,roles$key),,drop=FALSE]
  out$noise_free<-noise$level!="fixed";out$fixed_sigma<-noise$fixed_received
  out$response_index<-seq_len(nrow(out));rownames(out)<-NULL;out
}
.bh_response_covariance <- function(spec,state) {
  response<-.bh_gaussian_responses(spec);out<-matrix(0,nrow(response),nrow(response))
  at<-which(!is.na(response$eta_index))
  if(length(at))out[at,at]<-state$Sigma_predictor[response$eta_index[at],response$eta_index[at],drop=FALSE]
  out
}
.bh_level_operands <- function(spec) {
  roles<-spec$tables$parameter_roles;rows<-list()
  for(b in names(spec$models)) {
    block<-spec$models[[b]];d<-spec$data_map$blocks[[b]]
    if(!nrow(d))next
    if(identical(block$model$adapter$mode,"routed")) {
      binding<-.mp_resolve_bindings(block,d,b,require_coverage=FALSE)
      tab<-data.frame(block=b,observation_id=binding$observation_id,route=binding$route,argument=binding$argument,
        target=binding$target_key,literal=binding$kind=="fixed",literal_value=binding$fixed_value,literal_predictor=binding$fixed_scale,stringsAsFactors=FALSE)
    }else tab<-do.call(rbind,lapply(names(block$model$parameters),function(p)data.frame(block=b,observation_id=d$.observation_id,
      route=NA_character_,argument=p,target=targets(b,p,d$.task),literal=FALSE,literal_value=NA_real_,literal_predictor=NA_character_,stringsAsFactors=FALSE)))
    tab$role<-tab$level<-tab$receives<-tab$predictor<-NA_character_
    tab$operand<-ifelse(tab$literal,"route_literal",NA_character_);tab$received_constant<-tab$literal_value
    tab$value_source<-ifelse(tab$literal,"intrinsic_route_literal",NA_character_)
    tab$role[tab$literal]<-"literal_route_constant";tab$level[tab$literal]<-"fixed"
    tab$receives[tab$literal]<-"eta";tab$predictor[tab$literal]<-tab$literal_predictor[tab$literal]
    tab$eta_index<-tab$observation_scale_index<-NA_integer_
    for(i in which(!tab$literal)) {
      at<-match(tab$target[i],roles$key);if(is.na(at)).mp_abort("E_PARAMETER_BINDING","observation operand is outside the resolved logical parameter domain")
      r<-roles[at,];tab$role[i]<-r$role;tab$level[i]<-r$level;tab$receives[i]<-r$receives;tab$predictor[i]<-r$predictor
      tab$value_source[i]<-r$level_source
      if(r$level=="fixed") {tab$operand[i]<-"fixed_received";tab$received_constant[i]<-r$fixed_received
      }else if(r$role=="predictor") {tab$operand[i]<-if(r$level=="task")"task_predictor"else"individual_predictor";tab$eta_index[i]<-match(r$key,spec$tables$targets$key)
      }else {tab$operand[i]<-"free_observation_scale";tab$observation_scale_index[i]<-match(r$key,spec$tables$observation_parameters$key)}
    }
    rows[[b]]<-tab
  }
  out<-do.call(rbind,rows);rownames(out)<-NULL;out
}
.bh_level_lowering_required <- function(spec) {
  roles<-spec$tables$parameter_roles
  !is.null(roles)&&any(roles$level=="fixed"|(roles$role=="predictor"&roles$level=="task"))
}
