density_route <- function(selector_value, symbol, arguments, argument_types, parameter_arguments) {
  selector_value<-.mp_integer(selector_value,"selector_value")
  .mp_scalar_string(symbol,"route symbol")
  if(!is.character(arguments)||!length(arguments)||anyNA(arguments)||any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",arguments))||!grepl("^[A-Za-z][A-Za-z0-9_]*_(lpdf|lpmf)$",symbol)||anyDuplicated(arguments)||length(arguments)!=length(argument_types))
    .mp_abort("E_ROUTE","invalid typed density route")
  .mp_names(parameter_arguments,"route parameter arguments")
  if(any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",names(parameter_arguments)))) .mp_abort("E_ROUTE","formal parameter aliases must be plain identifiers")
  if(!all(names(parameter_arguments)%in%arguments)||!all(argument_types%in%c("int","real","int_array","real_array","vector","matrix")))
    .mp_abort("E_ROUTE","route parameter arguments/types are invalid")
  list(selector_value=selector_value,symbol=symbol,arguments=arguments,argument_types=argument_types,parameter_arguments=parameter_arguments)
}
parameter_binding_map <- function(domains, bindings) {
  if(!is.data.frame(domains)||!all(c("parameter","task")%in%names(domains))) .mp_abort("E_PARAMETER_DOMAIN","domains require parameter/task columns")
  domains<-domains[c("parameter","task")]
  for(n in names(domains))domains[[n]]<-.mp_key_component(domains[[n]],paste("domain",n))
  if(anyDuplicated(paste(domains$parameter,domains$task,sep="::"))) .mp_abort("E_PARAMETER_DOMAIN","duplicate parameter-domain key")
  required<-c("observation_id","route","argument","kind","target_parameter","target_task","fixed_value","fixed_scale")
  if(!is.data.frame(bindings)||!all(required%in%names(bindings))) .mp_abort("E_PARAMETER_BINDING",paste("bindings require",paste(required,collapse="/")))
  bindings<-bindings[required]
  for(n in c("observation_id","route","argument"))bindings[[n]]<-.mp_key_component(bindings[[n]],paste("binding",n))
  if(any(!bindings$kind%in%c("target","fixed"))) .mp_abort("E_PARAMETER_BINDING","binding kind must be target or fixed")
  if(anyDuplicated(paste(bindings$observation_id,bindings$route,bindings$argument,sep="::"))) .mp_abort("E_PARAMETER_BINDING","duplicate observation/route/argument binding")
  target<-bindings$kind=="target"
  if(any(target)) {
    for(n in c("target_parameter","target_task"))bindings[[n]][target]<-.mp_key_component(bindings[[n]][target],n)
    key<-paste(bindings$target_parameter[target],bindings$target_task[target],sep="::")
    if(!all(key%in%paste(domains$parameter,domains$task,sep="::"))) .mp_abort("E_PARAMETER_BINDING","binding references unknown domain key")
    if(any(!is.na(bindings$fixed_value[target]))||any(!is.na(bindings$fixed_scale[target]))) .mp_abort("E_PARAMETER_BINDING","target bindings cannot also contain fixed values/scales")
  }
  if(any(!target)) {
    if(any(!is.finite(bindings$fixed_value[!target]))||any(!bindings$fixed_scale[!target]%in%c("identity","log","logit"))||
       any(!is.na(bindings$target_parameter[!target]))||any(!is.na(bindings$target_task[!target])))
      .mp_abort("E_PARAMETER_BINDING","fixed bindings require only a finite predictor value and explicit predictor scale")
  }
  structure(list(domains=domains,bindings=bindings,version="per_observation_argument_v1"),class="measurement_parameter_bindings")
}
register_routed_adapter <- function(model, selector, routes, resources=list(), function_code=character(), function_exports=character()) {
  .mp_scalar_string(selector,"route selector");.mp_names(routes,"routes")
  if(!selector%in%names(model$data_schema)||!model$data_schema[[selector]]$type%in%c("integer","nonnegative_integer")) .mp_abort("E_ROUTE","selector must bind an integer schema field")
  if(anyDuplicated(vapply(routes,function(r)r$selector_value,integer(1)))) .mp_abort("E_ROUTE","route selector values must be distinct")
  for(r in routes) {
    density_route(r$selector_value,r$symbol,r$arguments,r$argument_types,r$parameter_arguments)
    if(!all(unlist(r$parameter_arguments)%in%names(model$parameters))) .mp_abort("E_ROUTE","route argument refers to unknown scientific parameter")
    if(!all(setdiff(r$arguments,names(r$parameter_arguments))%in%c(names(model$data_schema),names(model$constants)))) .mp_abort("E_ROUTE","unbound route data argument")
  }
  # Reuse registration/resource validation with a complete aggregate parameter contract.
  model<-register_stan_adapter(model,resources,function_code,function_exports)
  model$adapter$mode<-"routed";model$adapter$selector<-selector;model$adapter$routes<-routes
  model$adapter$parameter_binding<-"explicit per-observation formal argument to domain target or fixed predictor"
  .mp_validate_routes(model)
  model
}
.mp_validate_routes <- function(model) {
  if(!identical(model$adapter$mode,"routed"))return(invisible(TRUE))
  .mp_names(model$adapter$routes,"routes")
  selector<-model$adapter$selector
  if(!is.character(selector)||length(selector)!=1L||is.na(selector)||!selector%in%names(model$data_schema)||
     !model$data_schema[[selector]]$type%in%c("integer","nonnegative_integer")||model$data_schema[[selector]]$width!=1L)
    .mp_abort("E_ROUTE","route selector must be one declared scalar integer field")
  selectors<-vapply(model$adapter$routes,function(r).mp_integer(r$selector_value,"selector_value"),integer(1))
  if(anyDuplicated(selectors)) .mp_abort("E_ROUTE","route selector values must be distinct")
  consumed<-unlist(lapply(model$adapter$routes,function(r)r$parameter_arguments),use.names=FALSE)
  if(!setequal(consumed,names(model$parameters))) .mp_abort("E_PARAMETER_DEPENDENCY","routes must consume every declared scientific parameter")
  symbols<-c(unlist(lapply(model$adapter$resources,function(x)x$exports)),model$adapter$function_exports)
  for(r in model$adapter$routes) {
    density_route(r$selector_value,r$symbol,r$arguments,r$argument_types,r$parameter_arguments)
    if(!all(unlist(r$parameter_arguments)%in%names(model$parameters))) .mp_abort("E_ROUTE","unknown route scientific parameter")
    if(!r$symbol%in%symbols) .mp_abort("E_STAN_SYMBOL","route density symbol not exported by pinned resource/wrapper")
    for(i in seq_along(r$arguments)) {
      a<-r$arguments[i]
      expected<-if(a%in%names(r$parameter_arguments))"real"else if(a%in%names(model$data_schema)) {
        f<-model$data_schema[[a]];paste0(if(f$type%in%c("integer","nonnegative_integer","binary"))"int"else"real",if(f$width>1L)"_array"else"")
      }else model$constants[[a]]$type
      if(!identical(r$argument_types[i],expected)) .mp_abort("E_ARGUMENT_TYPE",paste("route argument has wrong type",a))
    }
  }
  invisible(TRUE)
}
.mp_resolve_bindings <- function(block, data, block_name, require_coverage=TRUE) {
  map<-block$parameter_bindings;model<-block$model
  if(!inherits(map,"measurement_parameter_bindings")) .mp_abort("E_PARAMETER_BINDING","routed adapters require an explicit parameter_binding_map")
  parameter_binding_map(map$domains,map$bindings);.mp_validate_routes(model)
  if(!is.null(model$validate_bindings)&&!isTRUE(model$validate_bindings(block$data,map))) .mp_abort("E_REFERENCE_BINDING","reference binding validation failed")
  if(!all(map$domains$parameter%in%names(model$parameters))) .mp_abort("E_PARAMETER_DOMAIN","domain has unknown parameter")
  if(!all(map$bindings$observation_id%in%block$data$.observation_id)) .mp_abort("E_PARAMETER_BINDING","binding observation key is absent from the bound data")
  if(!nrow(data)) {
    if(require_coverage&&nrow(map$domains)) .mp_abort("E_PARAMETER_COVERAGE","empty consumed routed block cannot cover its declared targets")
    return(.mp_empty_parameter_bindings())
  }
  out<-list()
  for(o in seq_len(nrow(data))) {
    value<-data[[model$adapter$selector]][o]
    route_names<-names(Filter(function(r)r$selector_value==value,model$adapter$routes))
    if(length(route_names)!=1L) .mp_abort("E_ROUTE","observation selector has no unique density route")
    rn<-route_names[[1L]];route<-model$adapter$routes[[rn]]
    rows<-map$bindings[map$bindings$observation_id==data$.observation_id[o],,drop=FALSE]
    if(any(rows$route!=rn)||!setequal(rows$argument,names(route$parameter_arguments))||nrow(rows)!=length(route$parameter_arguments))
      .mp_abort("E_PARAMETER_BINDING",paste("missing/extra/wrong-route argument for observation",data$.observation_id[o]))
    for(a in names(route$parameter_arguments)) {
      row<-rows[rows$argument==a,,drop=FALSE];parameter<-route$parameter_arguments[[a]];description<-model$parameters[[parameter]]
      if(row$kind=="target"&&row$target_parameter!=parameter) .mp_abort("E_PARAMETER_BINDING",paste("wrong parameter family for argument",a))
      if(row$kind=="fixed"&&(row$fixed_scale!=description$predictor||description$receives!="eta")) .mp_abort("E_PARAMETER_SCALE","fixed route values must be on the declared received predictor scale")
      row$target_key<-if(row$kind=="target")targets(block_name,row$target_parameter,row$target_task)else NA_character_
      row$person_key<-data$.person_key[o];row$row_index<-o;out[[length(out)+1L]]<-row
    }
  }
  result<-do.call(rbind,out);rownames(result)<-NULL
  expected<-paste(block_name,map$domains$parameter,map$domains$task,sep="::")
  if(require_coverage&&!setequal(unique(result$target_key[result$kind=="target"]),expected)) .mp_abort("E_PARAMETER_COVERAGE","every explicit domain target requires observed likelihood information")
  result
}
.mp_empty_parameter_bindings <- function()data.frame(observation_id=character(),route=character(),argument=character(),kind=character(),
  target_parameter=character(),target_task=character(),fixed_value=numeric(),fixed_scale=character(),target_key=character(),person_key=character(),row_index=integer(),stringsAsFactors=FALSE)
parameter_binding_table <- function(spec) {
  level_aware<-inherits(spec,"bhmm_spec")
  if(level_aware)spec<-.bh_normalize_spec(spec)
  rows<-list()
  for(b in names(spec$models)) if(identical(spec$models[[b]]$model$adapter$mode,"routed")&&nrow(spec$data_map$blocks[[b]]))
    rows[[b]]<-.mp_resolve_bindings(spec$models[[b]],spec$data_map$blocks[[b]],b,require_coverage=!level_aware)
  out<-if(length(rows))do.call(rbind,rows)else if(level_aware).mp_empty_parameter_bindings()else data.frame()
  if(level_aware)attr(out,"scope")<-"original route/formal-argument bindings for consumed rows; parameter_roles and observation_operands show effective task/fixed overrides and retained domains"
  out
}
