# Execution eligibility follows typed contracts; display names are cosmetic.
.bh_model_role_info <- function(model) {
  roles<-vapply(model$parameters,.bh_parameter_role,character(1))
  list(primary=names(roles)[roles=="predictor"],nuisance=names(roles)[roles=="observation_scale"],scale="predictor",family_id=0L)
}
.bh_reference_density_kind <- function(model) {
  if(length(model$adapter$resources)!=1L)return(NA_character_)
  resource<-model$adapter$resources[[1L]]
  factories<-if(identical(resource$sha256,"4b070861489b188e63575c4e0a8dae18cd54d01afd8f1d56a804714061fdc4d1"))
    list(m3_cs_reference=m3_cs_model,m3_reference_full=m3_model)else if(identical(resource$sha256,"66d521033044ecd68fc6f5253e669773a5fc352cd5bc280fc1b905eff37154a4"))
      list(sdm_reference=sdm_reference_model)else list()
  scientific<-function(x) {
    out<-x[c("data_schema","parameters","parameter_dependencies","constants","adapter","backend")]
    out$data_schema<-lapply(out$data_schema,function(f)f[c("type","role","units","lower","upper","width")])
    out$parameters<-lapply(out$parameters,function(p)c(p[c("support","predictor","receives","levels","units","predictor_units")],list(role=.bh_parameter_role(p))))
    out$parameter_dependencies<-sort(out$parameter_dependencies)
    out$constants<-lapply(out$constants,function(k)k[c("value","type","units")])
    out$likelihood<-x$likelihood[c("symbol","arguments","argument_types","normalized","censoring","truncation")]
    out$adapter$resources<-lapply(out$adapter$resources,function(r)list(sha256=r$sha256,exports=sort(r$exports)))
    out$adapter$function_exports<-sort(out$adapter$function_exports)
    out
  }
  for(kind in names(factories)) {
    canonical<-factories[[kind]](resource$file)
    if(identical(scientific(model),scientific(canonical)))return(kind)
  }
  NA_character_
}
.bh_observation_support <- function(model,data) {
  builtin<-.bh_builtin_kind(model)
  if(is.na(builtin)) {
    if(is.null(model$adapter)||!identical(model$backend,"registered_stan"))
      .mp_abort("E_OBSERVATION_CONTRACT_UNSUPPORTED","requires an actual canonical builtin contract or an explicitly registered typed Stan adapter; display name, backend label or fit flag alone is insufficient")
    .mp_validate_adapter(model)
  }
  if(!is.null(model$prepare_data)) .mp_abort("E_PREPARE_UNSUPPORTED","custom preparation callbacks are not executed; bind explicitly prepared data")
  if(!isTRUE(model$likelihood$normalized)||
     !identical(model$likelihood$censoring,"none")||!identical(model$likelihood$truncation,"none"))
    .mp_abort("E_LIKELIHOOD_UNSUPPORTED","this compiler requires normalized row densities without unsupported censoring or truncation")
  .mp_validate_fields(model,data,allow_missing_response=FALSE)
  reference<-.bh_reference_density_kind(model)
  if(nrow(data)) {
    if(identical(reference,"m3_cs_reference")) .mp_validate_m3_cs(data)
    if(identical(reference,"sdm_reference")) .mp_validate_sdm_runs(data)
    if(identical(reference,"m3_reference_full")) .mp_validate_full_m3(data)
  }
  callbacks<-names(Filter(is.function,model[c("validate_data","validate_bindings","informative_rows")]))
  canonical_callbacks<-if(!is.na(builtin))switch(builtin,gaussian=gaussian_model,binomial=binomial_model,lognormal=lognormal_model)()[callbacks]else
    if(!is.na(reference))switch(reference,m3_cs_reference=m3_cs_model,m3_reference_full=m3_model,sdm_reference=sdm_reference_model)(model$adapter$resources[[1L]]$file)[callbacks]else NULL
  known_callbacks<-length(callbacks)>0L&&!is.null(canonical_callbacks)&&identical(model[callbacks],canonical_callbacks)
  list(contract=if(!is.na(builtin))paste0("canonical_",builtin)else"registered_typed_row_adapter",
       density=model$likelihood$symbol,reference_density=reference,
       callbacks=callbacks,callback_dependencies=if(length(callbacks)&&!known_callbacks)
         "caller_managed_external_dependencies: JIT-stable serialization does not freeze mutable global/namespace values or stateful callbacks; these are outside the frozen-context reproducibility guarantee"
         else if(known_callbacks)"known canonical callback representation; no arbitrary external callback dependencies inferred"else"no registered validation callbacks",
       qualification="contract support only; arbitrary user density correctness and native gradients remain unqualified")
}
.bh_backend_support <- function(spec) {
  records<-lapply(names(spec$models),function(name)tryCatch({
    result<-.bh_observation_support(spec$models[[name]]$model,spec$data_map$blocks[[name]])
    c(list(block=name,status="supported"),result)
  },error=function(e)list(block=name,status="unsupported",code=if(is.null(e$code))"E_OBSERVATION_CONTRACT_UNSUPPORTED"else e$code,reason=conditionMessage(e))))
  names(records)<-names(spec$models)
  bad<-vapply(records,function(x)x$status!="supported",logical(1))
  list(status=if(any(bad))"unsupported_observation_contract"else"supported_typed_ir",
       reason=if(any(bad))paste(vapply(records[bad],function(x)paste0(x$block,": ",x$reason),character(1)),collapse="; ")else
         "normalized typed row densities and resolved individual/task/fixed/noise operands use the common component compiler; empty blocks add no likelihood rows",
       blocks=records,policy_version="typed_experimental_execution_v1")
}

.bh_information_policy_assert <- function(spec) {
  if(!inherits(spec,"bhmm_spec"))return(invisible(TRUE))
  expected<-do.call(rbind,lapply(names(spec$models),function(b) {
    d<-spec$data_map$blocks[[b]];rule<-.bh_information_contract(spec$models[[b]]$model)
    informative<-.bh_informative_rows(spec$models[[b]]$model,d,rule)
    data.frame(block=rep(b,nrow(d)),observation_id=d$.observation_id,informative=informative,policy=rep(rule,nrow(d)),stringsAsFactors=FALSE)
  }))
  actual<-spec$tables$information
  rownames(expected)<-NULL
  if(is.data.frame(actual))rownames(actual)<-NULL
  if(!identical(actual,expected)) .mp_abort("E_INFORMATION_POLICY_REBUILD","this saved context's information table does not match the current verified density policy; explicitly rebuild from its saved formula, original observation blocks and intended priors. The original context and fingerprint are unchanged")
  invisible(TRUE)
}

# A conditional row-likelihood fact, distinct from hierarchy/evidence identity.
# The zero-count multinomial identity holds at valid finite received arguments,
# irrespective of how their scalar priors, links or variation levels were chosen.
.bh_m3_row_information_kind <- function(model) {
  if(length(model$adapter$resources)!=1L ||
     !identical(model$adapter$resources[[1L]]$sha256,"4b070861489b188e63575c4e0a8dae18cd54d01afd8f1d56a804714061fdc4d1"))return(NA_character_)
  .mp_validate_adapter(model)
  key<-function(m)list(
    schema=lapply(m$data_schema,function(f)f[c("type","role","lower","upper","width")]),
    parameter_names=sort(names(m$parameters)),
    density=m$likelihood[c("symbol","arguments","argument_types","normalized","censoring","truncation")],
    constants=lapply(m$constants,function(k)k[c("value","type")]),
    adapter=m$adapter[c("version","mode","selector","routes","independence_unit","parameter_binding","function_code","wrapper_hash")],
    function_exports=sort(m$adapter$function_exports),
    resources=lapply(m$adapter$resources,function(r)list(sha256=r$sha256,exports=sort(r$exports))))
  for(kind in c("m3_cs_reference","m3_reference_full")) {
    factory<-if(kind=="m3_cs_reference")m3_cs_model else m3_model
    if(identical(key(model),key(factory(model$adapter$resources[[1L]]$file))))return(kind)
  }
  NA_character_
}
.bh_informative_rows <- function(model,data,rule=.bh_information_contract(model)) {
  if(identical(rule,"known_m3_positive_total")) {
    kind<-.bh_m3_row_information_kind(model)
    if(is.na(kind)) .mp_abort("E_INFORMATION","known M3 information rule lacks its exact conditional density contract")
    if(nrow(data))if(kind=="m3_cs_reference").mp_validate_m3_cs(data)else .mp_validate_full_m3(data)
    return(rowSums(data$counts)>0)
  }
  if(identical(rule,"known_binomial_positive_trials"))return(data[[.bh_binomial_row_fields(model)$trials]]>0)
  rep(TRUE,nrow(data))
}

# The authoritative Stan Math primitive, with no registered overriding source.
# This proves its row domain/zero-trial fact, not a Gaussian/cognitive hierarchy.
.bh_binomial_row_fields <- function(model) {
  if(identical(.bh_builtin_kind(model),"binomial"))return(list(response="successes",trials="trials"))
  a<-model$adapter
  if(is.null(a)||!identical(model$backend,"registered_stan")||!is.null(a$mode)||
     length(a$resources)||length(a$function_code)||length(a$function_exports))return(NULL)
  d<-model$likelihood
  if(!identical(d$symbol,"binomial_logit_lpmf")||!identical(d$argument_types,c("int","int","real"))||
     length(d$arguments)!=3L||!all(d$arguments[1:2]%in%names(model$data_schema)))return(NULL)
  fields<-model$data_schema[d$arguments[1:2]]
  if(any(!vapply(fields,function(f)f$width==1L&&f$type%in%c("integer","nonnegative_integer","binary"),logical(1))))return(NULL)
  .mp_validate_adapter(model)
  list(response=d$arguments[1L],trials=d$arguments[2L])
}
