.mp_sha256 <- function(file) {
  if(!file.exists(file)) .mp_abort("E_RESOURCE",paste("resource missing:",basename(file)))
  executable <- Sys.which("sha256sum")
  if(nzchar(executable)) result <- system2(executable,shQuote(normalizePath(file)),stdout=TRUE,stderr=TRUE) else {
    executable <- Sys.which("shasum")
    if(!nzchar(executable)) .mp_abort("E_HASH_TOOL","external-resource pinning needs sha256sum or shasum in PATH")
    result <- system2(executable,c("-a","256",shQuote(normalizePath(file))),stdout=TRUE,stderr=TRUE)
  }
  hash <- strsplit(result[1L],"[[:space:]]+")[[1L]][1L]
  if(!grepl("^[a-fA-F0-9]{64}$",hash)) .mp_abort("E_HASH_TOOL","SHA-256 command failed")
  tolower(hash)
}
stan_resource <- function(file, expected_sha256, exports, license, source) {
  .mp_scalar_string(file,"resource file"); .mp_scalar_string(expected_sha256,"expected_sha256")
  .mp_scalar_string(license,"resource license"); .mp_scalar_string(source,"resource source")
  if(!grepl("^[a-fA-F0-9]{64}$",expected_sha256)) .mp_abort("E_RESOURCE_HASH","expected SHA-256 must be explicit")
  if(!is.character(exports)||!length(exports)||anyNA(exports)||any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",exports))||anyDuplicated(exports))
    .mp_abort("E_STAN_SYMBOL","resource exports must be distinct plain Stan identifiers")
  hash <- .mp_sha256(file)
  if(hash!=tolower(expected_sha256)) .mp_abort("E_RESOURCE_HASH",paste("resource hash mismatch:",basename(file)))
  if(any(grepl("^[[:space:]]*#include",readLines(file,warn=FALSE))))
    .mp_abort("E_RESOURCE_DEPENDENCY","nested includes are unsupported; every external function resource must be explicitly pinned")
  structure(list(file=normalizePath(file),sha256=hash,exports=exports,license=license,source=source,
    cache_name=paste0("external_",hash,".stanfunctions")),class="measurement_stan_resource")
}
stan_constant <- function(value, type, units, provenance) {
  type<-match.arg(type,c("real","int","vector","matrix"));.mp_scalar_string(units,"constant units");.mp_scalar_string(provenance,"constant provenance")
  if(!is.numeric(value)||!length(value)||any(!is.finite(value))) .mp_abort("E_CONSTANT","constants must contain finite numeric values")
  if(type%in%c("real","int")&&length(value)!=1L) .mp_abort("E_CONSTANT","scalar constant must have one value")
  if(type=="int"&&(value!=floor(value)||abs(value)>.Machine$integer.max)) .mp_abort("E_CONSTANT","integer constant outside Stan integer range")
  if(type=="vector"&&!is.null(dim(value))) .mp_abort("E_CONSTANT","vector constant cannot be a matrix")
  if(type=="matrix"&&(!is.matrix(value)||any(dim(value)>200L))) .mp_abort("E_CONSTANT","matrix constant must be a matrix with dimensions at most 200")
  list(value=value,type=type,units=units,provenance=provenance)
}
register_stan_adapter <- function(model, resources = list(), function_code = character(), function_exports = character()) {
  if(!inherits(model,"observation_model")) .mp_abort("E_MODEL","expected observation_model")
  if(any(!vapply(resources,inherits,logical(1),"measurement_stan_resource"))) .mp_abort("E_RESOURCE","all resources must be pinned stan_resource objects")
  function_code <- .mp_validate_function_code(function_code)
  if(length(function_code)&&(!length(function_exports)||anyDuplicated(function_exports)||any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",function_exports))))
    .mp_abort("E_STAN_SYMBOL","wrapper functions require declared plain-identifier exports")
  roles<-vapply(model$parameters,function(p) {
    if(length(p$levels)!=1L) .mp_abort("E_ADAPTER","adapter parameters need one executable default level, not a level menu")
    .bh_parameter_role(p)
  },character(1))
  primary<-names(roles)[roles=="predictor"]
  nuisance<-names(roles)[roles=="observation_scale"]
  if(length(primary)+length(nuisance)!=length(model$parameters))
    .mp_abort("E_ADAPTER","every adapter parameter needs a predictor or observation-scale role")
  if(length(nuisance)&&any(!vapply(model$parameters[nuisance],function(p)p$support=="positive"&&p$receives=="theta",logical(1))))
    .mp_abort("E_ADAPTER","task nuisance parameters must be positive SDs received on theta scale")
  if(!model$likelihood$normalized) .mp_abort("E_LIKELIHOOD","executable adapters require a declared normalized observation density")
  for(i in seq_along(model$likelihood$arguments)) {
    a<-model$likelihood$arguments[i];type<-model$likelihood$argument_types[i]
    expected<-if(a%in%names(model$data_schema)) {
      f<-model$data_schema[[a]];integer<-f$type%in%c("integer","nonnegative_integer","binary")
      paste0(if(integer)"int"else"real",if(f$width>1L)"_array"else"")
    } else if(a%in%names(model$parameters)) "real" else model$constants[[a]]$type
    if(!identical(type,expected)) .mp_abort("E_ARGUMENT_TYPE",paste("typed density argument differs from its binding:",a))
  }
  if(!all(c(primary,nuisance)%in%model$likelihood$arguments)) .mp_abort("E_PARAMETER_DEPENDENCY","every declared parameter, including nuisance, must be consumed by the density")
  model$adapter<-list(primary=primary,nuisance=nuisance,resources=resources,function_code=function_code,
    function_exports=function_exports,wrapper_hash=.mp_hash(function_code),version="typed_row_adapter_v1",
    independence_unit="one explicit observation row; arrays remain one joint observation",
    parameter_binding="same task for every declared parameter; cross-task references are not implemented")
  model$likelihood["source"]<-list(NULL) # External pinned bytes are never embedded in exported model objects.
  model$backend<-"registered_stan"
  .mp_validate_adapter(model)
  model
}
.mp_validate_adapter <- function(model) {
  if(is.null(model$adapter)) .mp_abort("E_ADAPTER","no executable adapter registered")
  if(!identical(model$adapter$version,"typed_row_adapter_v1") ||
     !(is.null(model$adapter$mode)||identical(model$adapter$mode,"routed")) ||
     !identical(model$adapter$independence_unit,"one explicit observation row; arrays remain one joint observation"))
    .mp_abort("E_ADAPTER","unsupported adapter version, mode or independence unit")
  binding<-if(identical(model$adapter$mode,"routed"))"explicit per-observation formal argument to domain target or fixed predictor"else"same task for every declared parameter; cross-task references are not implemented"
  if(!identical(model$adapter$parameter_binding,binding)) .mp_abort("E_ADAPTER","unsupported adapter parameter-binding contract")
  .mp_validate_density_arguments(model)
  roles<-vapply(model$parameters,function(p) {
    if(length(p$levels)!=1L) .mp_abort("E_ADAPTER","adapter parameters require one default level")
    args<-p[c("support","predictor","receives","levels","units","description","predictor_units")]
    if(!is.null(p$role))args$role<-p$role
    verified<-do.call(parameter_spec,args)
    if(!identical(verified,p)) .mp_abort("E_PARAMETER","typed parameter metadata changed; construct and register the intended parameter contract explicitly")
    role<-.bh_parameter_role(p)
    if(!role%in%c("predictor","observation_scale")) .mp_abort("E_PARAMETER_ROLE","unknown scientific parameter role")
    if(role=="observation_scale"&&(p$support!="positive"||p$receives!="theta"||p$levels=="subject_task")) .mp_abort("E_PARAMETER_ROLE","observation-scale contract is inconsistent")
    role
  },character(1))
  if(!identical(model$adapter$primary,names(roles)[roles=="predictor"])||!identical(model$adapter$nuisance,names(roles)[roles=="observation_scale"]))
    .mp_abort("E_PARAMETER_ROLE","adapter role indices no longer match the scientific parameter contracts")
  if(!is.null(model$reference_parameter_hash)&&!identical(model$reference_parameter_hash,.mp_hash(model$parameters))) .mp_abort("E_PARAMETER_SCALE","locked reference parameter contract was altered; construct an explicitly different custom model")
  .mp_validate_routes(model)
  code<-.mp_validate_function_code(model$adapter$function_code)
  if(!identical(.mp_hash(code),model$adapter$wrapper_hash)) .mp_abort("E_ADAPTER_HASH","wrapper code changed; register a new adapter explicitly")
  for(constant in model$constants)stan_constant(constant$value,constant$type,constant$units,constant$provenance)
  available<-c(unlist(lapply(model$adapter$resources,function(r)r$exports)),model$adapter$function_exports)
  dependencies<-model$likelihood$dependencies
  if(typeof(dependencies)!="character"||anyNA(dependencies)||anyDuplicated(dependencies)||
     any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",dependencies))||
     !all(dependencies%in%c(available,"normal_lpdf","binomial_logit_lpmf","lognormal_lpdf")))
    .mp_abort("E_STAN_DEPENDENCY","declared dependencies must resolve to registered exports or the supported builtin row-density symbols")
  if(!model$likelihood$symbol%in%c(available,"normal_lpdf","binomial_logit_lpmf","lognormal_lpdf"))
    .mp_abort("E_STAN_SYMBOL","density symbol is not exported by a pinned resource")
  for(r in model$adapter$resources) {
    if(.mp_sha256(r$file)!=r$sha256) .mp_abort("E_RESOURCE_HASH",paste("external source changed:",basename(r$file)))
    if(!identical(r$cache_name,paste0("external_",r$sha256,".stanfunctions"))) .mp_abort("E_RESOURCE_HASH","external cache filename must be derived from its verified content hash")
  }
  invisible(TRUE)
}
.mp_model_info <- function(model) {
  if(!is.null(model$adapter)) return(list(primary=model$adapter$primary,nuisance=model$adapter$nuisance,scale="predictor",family_id=0L))
  .mp_family_info(model$name)
}
.mp_registry_mode <- function(spec) inherits(spec,"bhmm_spec") || identical(spec$structure$type,"component_covariance") || any(vapply(spec$models,function(b)b$model$backend=="registered_stan",logical(1))) || identical(spec$priors$kind,"parameter_specific")
.mp_adapter_resources <- function(spec) {
  out<-list();symbols<-list()
  for(b in spec$models) if(!is.null(b$model$adapter)) {
    .mp_validate_adapter(b$model)
    for(r in b$model$adapter$resources) {
      for(symbol in r$exports) {
        if(!is.null(symbols[[symbol]])&&!identical(symbols[[symbol]],r$sha256)) .mp_abort("E_SYMBOL_COLLISION",paste("external symbol collision:",symbol))
        symbols[[symbol]]<-r$sha256
      }
      out[[r$sha256]]<-r
    }
    for(symbol in b$model$adapter$function_exports) {
      hash<-b$model$adapter$wrapper_hash
      if(!is.null(symbols[[symbol]])&&!identical(symbols[[symbol]],hash)) .mp_abort("E_SYMBOL_COLLISION",paste("wrapper symbol collision:",symbol))
      symbols[[symbol]]<-hash
    }
  }
  out
}
model_resource_manifest <- function(spec) {
  resources<-.mp_adapter_resources(spec)
  if(!length(resources)) return(data.frame())
  do.call(rbind,lapply(resources,function(r)data.frame(resource=basename(r$file),sha256=r$sha256,license=r$license,
    source=r$source,exports=paste(r$exports,collapse=", "),stringsAsFactors=FALSE)))
}

.mp_validate_function_code <- function(code) {
  if(!is.character(code)||anyNA(code)||any(grepl("#include",code,fixed=TRUE)))
    .mp_abort("E_ADAPTER","wrapper code must contain no include directives")
  code<-unlist(strsplit(code,"\n",fixed=TRUE),use.names=FALSE)
  if(!length(code))return(character())
  text<-paste(code,collapse="\n")
  # Conservative brace-depth gate keeps the wrapper inside functions{}.
  text<-gsub('"(?:\\\\.|[^"\\\\])*"','""',text,perl=TRUE)
  text<-gsub("(?s)/\\*.*?\\*/","",text,perl=TRUE)
  text<-gsub("//[^\n]*","",text,perl=TRUE)
  braces<-regmatches(text,gregexpr("[{}]",text))[[1L]]
  depth<-cumsum(ifelse(braces=="{",1L,-1L))
  if(!length(depth)||any(depth<0L)||tail(depth,1L)!=0L) .mp_abort("E_ADAPTER_SCOPE","wrapper must contain balanced function definitions only")
  code
}

.mp_validate_density_arguments <- function(model) {
  .mp_names(model$data_schema,"data_schema");.mp_names(model$parameters,"parameters")
  for(field in model$data_schema) {
    checked<-do.call(data_field,field[c("type","role","units","lower","upper","width")])
    if(!identical(checked,field)) .mp_abort("E_SCHEMA","registered field metadata changed")
  }
  density<-model$likelihood;args<-density$arguments;types<-density$argument_types
  if(typeof(args)!="character"||!length(args)||anyNA(args)||anyDuplicated(args)||
     any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",args))||typeof(types)!="character"||anyNA(types)||length(args)!=length(types)||
     !all(types%in%c("int","real","int_array","real_array","vector","matrix")))
    .mp_abort("E_ARGUMENT_TYPE","density requires distinct plain arguments with explicit supported types")
  if(!all(args%in%c(names(model$data_schema),names(model$parameters),names(model$constants))))
    .mp_abort("E_PARAMETER_DEPENDENCY","density contains an unbound argument")
  if(!isTRUE(density$normalized)) .mp_abort("E_LIKELIHOOD","executable adapters require a declared normalized density")
  if(anyDuplicated(model$parameter_dependencies)||!setequal(model$parameter_dependencies,names(model$parameters))||
     !all(names(model$parameters)%in%args)) .mp_abort("E_PARAMETER_DEPENDENCY","every declared parameter must have one explicit dependency and be consumed by the density")
  for(i in seq_along(args)) {
    a<-args[i]
    expected<-if(a%in%names(model$data_schema)) {
      f<-model$data_schema[[a]]
      paste0(if(f$type%in%c("integer","nonnegative_integer","binary"))"int"else"real",if(f$width>1L)"_array"else"")
    }else if(a%in%names(model$parameters))"real"else model$constants[[a]]$type
    if(!identical(types[i],expected)) .mp_abort("E_ARGUMENT_TYPE",paste("density argument differs from its binding:",a))
  }
  exports<-model$adapter$function_exports
  if(typeof(exports)!="character"||anyNA(exports)||anyDuplicated(exports)||any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",exports)))
    .mp_abort("E_STAN_SYMBOL","wrapper exports require distinct plain identifiers")
  if(any(!vapply(model$adapter$resources,inherits,logical(1),"measurement_stan_resource"))) .mp_abort("E_RESOURCE","adapter resources must be explicitly pinned")
  invisible(TRUE)
}
