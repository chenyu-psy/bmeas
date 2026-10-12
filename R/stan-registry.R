.mp_stan_data_shapes <- function(data, declarations) {
  # CmdStanR JSON auto_unbox turns an ordinary length-one vector into a scalar.
  # Preserve declared array/vector rank, including constrained vectors.
  for(declaration in declarations) if(grepl("^(array|vector)\\[",gsub("<[^>]*>","",declaration))) {
    name<-sub("^.* ([A-Za-z][A-Za-z0-9_]*);$","\\1",declaration)
    if(length(data[[name]])==1L&&is.null(dim(data[[name]])))data[[name]]<-array(data[[name]],dim=1L)
  }
  data
}
.mp_registry_indices <- function(spec) {
  keys<-.mp_structure_keys(spec)
  nuisance<-unlist(lapply(names(spec$models),function(b){ns<-.mp_model_info(spec$models[[b]]$model)$nuisance;spec$parameter_keys$key[spec$parameter_keys$model==b & spec$parameter_keys$parameter%in%ns]}),use.names=FALSE)
  list(keys=keys,nuisance=as.character(nuisance),J=length(keys),S=length(nuisance),K=if(identical(spec$structure$type,"component_covariance"))spec$structure$common_total else length(spec$structure$factors))
}
.mp_registry_symbol_map <- function(spec) {
  i<-.mp_registry_indices(spec);keys<-spec$parameter_keys
  component<-identical(spec$structure$type,"component_covariance")
  out<-data.frame(key=i$keys,intercept=paste0(if(component)"mu["else"alpha[",seq_len(i$J),"]"),loading=if(component)rep(NA_character_,i$J)else paste0("lambda[",seq_len(i$J),"]"),
    observation_sd=NA_character_,observation_sd_key=NA_character_,nuisance_parameter=NA_character_,
    predictor_scale=keys$predictor[match(i$keys,keys$key)],stringsAsFactors=FALSE)
  if(component){out$task_sd<-paste0("tau[",seq_len(i$J),"]");out$individual_eta<-paste0("eta[person,",seq_len(i$J),"]")}
  # A separate nuisance map is required for multi-parameter observations.
  attr(out,"nuisance_map")<-data.frame(key=i$nuisance,symbol=if(i$S)paste0("obs_sd[",seq_len(i$S),"]")else character(),stringsAsFactors=FALSE)
  if(i$S)for(j in seq_len(i$J)) {
    parts<-strsplit(i$keys[j],"::",fixed=TRUE)[[1L]]
    ns<-.mp_model_info(spec$models[[parts[1L]]]$model)$nuisance
    if(length(ns)==1L) {
      key<-targets(parts[1L],ns,parts[3L]);idx<-match(key,i$nuisance)
      out$observation_sd[j]<-paste0("obs_sd[",idx,"]");out$observation_sd_key[j]<-key;out$nuisance_parameter[j]<-ns
    }
  }
  out
}
.mp_registry_ir <- function(spec, threaded=FALSE) {
  index<-.mp_registry_indices(spec);si<-structure_ir(spec);oi<-observation_ir(spec,index,threaded)
  data<-c(list(N=length(spec$data_map$subject_keys),J=index$J,K=index$K,S=index$S,O=oi$observation_count,mp_use_reduce_sum=0L,mp_grainsize=1L),si$data,oi$data)
  if(anyDuplicated(names(data))) .mp_abort("E_IR_COLLISION","structure/observation data symbols collide")
  data<-lapply(data,function(x)if(is.null(dim(x)))unname(x)else x)
  data<-.mp_stan_data_shapes(data,c(si$declarations,oi$declarations))
  list(data=data,structure=si,observation=oi,index=index)
}
.mp_registry_code <- function(spec, threaded=FALSE) {
  ir<-.mp_registry_ir(spec,threaded);si<-ir$structure;oi<-ir$observation;resources<-.mp_adapter_resources(spec);wrappers<-list()
  for(b in spec$models)if(!is.null(b$model$adapter)&&length(b$model$adapter$function_code))wrappers[[b$model$adapter$wrapper_hash]]<-b$model$adapter$function_code
  save_ll<-isTRUE(spec$verification_outputs$log_lik)
  c("// Generic structure/observation assembler. No research Stan program is dispatched.",
    "// External resources are hash-pinned and remain separately licensed.",
    "functions {",vapply(resources,function(r)paste0("#include ",r$cache_name),character(1)),unlist(wrappers,use.names=FALSE),oi$functions,"}",
    "data {",paste0("int<lower=",if(is.null(si$min_subjects))1L else si$min_subjects,"> N;"),"int<lower=1> J;","int<lower=1> K;","int<lower=0> S;","int<lower=1> O;","int<lower=0,upper=1> mp_use_reduce_sum;","int<lower=1> mp_grainsize;",si$declarations,oi$declarations,"}",
    if(!threaded)c("transformed data {",'if(mp_use_reduce_sum==1) reject("reduction requires a threaded compilation");',"}"),
    "parameters {",si$parameters,"}","transformed parameters {",si$transformed,"}","model {",si$prior,oi$likelihood,"}",
    "generated quantities {",if(save_ll)"vector[O] log_lik;",si$generated,if(save_ll)oi$log_lik,"}")
}
