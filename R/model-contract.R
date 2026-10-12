data_field <- function(type, role = "auxiliary", units = NULL,
                       lower = -Inf, upper = Inf, width = 1L) {
  type <- match.arg(type, c("real", "positive_real", "integer", "nonnegative_integer", "binary"))
  role <- match.arg(role, c("response", "auxiliary"))
  width <- .mp_integer(width,"field width"); if(width>100L) .mp_abort("E_SCHEMA","row arrays are limited to width 100")
  if (!is.null(units)) .mp_scalar_string(units, "field units")
  if (length(lower) != 1L || length(upper) != 1L || is.na(lower) || is.na(upper) || lower > upper)
    .mp_abort("E_SCHEMA", "field bounds are invalid")
  structure(list(type = type, role = role, units = units, lower = lower,
                 upper = upper, width = width, dimensions = if(width==1L) "one value per observation" else paste(width,"values per observation")), class = "measurement_data_field")
}
response_schema <- function(..., units = list(), response = NULL) {
  fields <- list(...); .mp_names(fields, "schema")
  if (is.null(response)) response <- names(fields)[1L]
  if (!length(response) || !all(response %in% names(fields)))
    .mp_abort("E_SCHEMA", "response must name schema fields")
  for (n in names(fields)) {
    if (is.character(fields[[n]])) fields[[n]] <- data_field(fields[[n]], units = units[[n]])
    if (!inherits(fields[[n]], "measurement_data_field"))
      .mp_abort("E_SCHEMA", paste("invalid field", n))
    fields[[n]]$role <- if (n %in% response) "response" else "auxiliary"
  }
  fields
}
parameter_spec <- function(support, predictor, receives, levels, units, description, predictor_units = units, role = NULL) {
  support <- match.arg(support, c("real", "positive", "unit_interval"))
  predictor <- match.arg(predictor, c("identity", "log", "logit"))
  receives <- match.arg(receives, c("eta", "theta"))
  if (typeof(levels)!="character"||is.object(levels)||anyNA(levels)||!length(levels) || !all(levels %in% c("subject_task", "task", "fixed")))
    .mp_abort("E_PARAMETER", "unsupported parameter level")
  .mp_scalar_string(units, "parameter units"); .mp_scalar_string(description, "description"); .mp_scalar_string(predictor_units,"predictor units")
  expected <- c(real = "identity", positive = "log", unit_interval = "logit")[[support]]
  if (predictor != expected) .mp_abort("E_PARAMETER_SCALE", "P0 supports only identity/real, log/positive, and logit/unit_interval")
  if(!is.null(role)) {
    if(typeof(role)!="character"||is.object(role)||length(role)!=1L||is.na(role)||!role%in%c("predictor","observation_scale"))
      .mp_abort("E_PARAMETER_ROLE","role must be exactly predictor or observation_scale")
    if(length(levels)!=1L) .mp_abort("E_PARAMETER_LEVEL","an explicit role needs one executable default level")
    if(role=="observation_scale"&&(support!="positive"||receives!="theta"||levels=="subject_task"))
      .mp_abort("E_PARAMETER_ROLE","observation_scale requires positive support received on theta and a task/fixed default")
  }
  out<-list(support = support, predictor = predictor, receives = receives,
       levels = levels, units = units, predictor_units = predictor_units, description = description,
       transform_owner = "adapter", prior_scale = "must be declared by structure")
  if(!is.null(role))out$role<-role
  out
}
model_capabilities <- function(fit = FALSE, simulate = FALSE, posterior_predict = FALSE,
                               posterior_epred = FALSE, log_lik = FALSE, reduce_sum = FALSE,
                               status = "unvalidated") {
  status <- match.arg(status, c("unvalidated", "smoke_checked", "validated"))
  values <- list(fit = fit, simulate = simulate, posterior_predict = posterior_predict,
                 posterior_epred = posterior_epred, log_lik = log_lik, reduce_sum = reduce_sum)
  if (any(!vapply(values, function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1))))
    .mp_abort("E_CAPABILITY", "capability flags must be single TRUE/FALSE values")
  data.frame(capability = names(values), available = unlist(values, use.names = FALSE),
             status = ifelse(unlist(values, use.names = FALSE), status, "unsupported"), stringsAsFactors = FALSE)
}
stan_density <- function(file = NULL, symbol, arguments, argument_types,
                         normalized, approximation, builtin = FALSE,
                         exports = symbol, dependencies = character(), license = "unknown") {
  .mp_scalar_string(symbol, "Stan density symbol")
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*_(lpdf|lpmf)$", symbol))
    .mp_abort("E_STAN_SYMBOL", "density symbol must be a Stan lpdf/lpmf identifier")
  if (!is.character(arguments) || !length(arguments) || anyDuplicated(arguments) ||
      length(argument_types) != length(arguments) ||
      !all(argument_types %in% c("int", "real", "int_array", "real_array", "vector", "matrix")))
    .mp_abort("E_LIKELIHOOD", "supply distinct arguments and one int/real type per argument")
  if (!is.logical(normalized) || length(normalized) != 1L || is.na(normalized))
    .mp_abort("E_LIKELIHOOD", "normalized must be explicitly TRUE/FALSE")
  .mp_scalar_string(approximation, "approximation"); .mp_scalar_string(license, "license")
  code <- NULL; hash <- NULL
  if (!builtin) {
    if (is.null(file) || !file.exists(file)) .mp_abort("E_RESOURCE", "Stan function file does not exist")
    code <- readLines(file, warn = FALSE); hash <- unname(tools::md5sum(file))
  }
  list(symbol = symbol, arguments = arguments, argument_types = argument_types,
       normalized = normalized, approximation = approximation, builtin = builtin,
       source = code, source_file = if(!is.null(file))normalizePath(file)else NULL, resource_hash = hash, exports = exports, dependencies = dependencies,
       license = license, censoring = "none", truncation = "none")
}
new_observation_model <- function(name, version, data_schema, parameters, likelihood,
                                  capabilities = model_capabilities(),
                                  parameter_dependencies = names(parameters),
                                  validate_data = NULL, prepare_data = NULL,
                                  check_statistics = character(), provenance = list(license = "unknown"), methods = list(), constants = list()) {
  .mp_scalar_string(name, "model name"); .mp_scalar_string(version, "model version")
  .mp_names(data_schema, "data_schema"); .mp_names(parameters, "parameters")
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", c(names(data_schema), names(parameters)))) ||
      length(intersect(names(data_schema), names(parameters))))
    .mp_abort("E_SCHEMA_NAME", "schema/parameter names must be disjoint plain identifiers; internal dot-prefixed keys are reserved")
  if(length(constants)) {
    .mp_names(constants,"constants")
    if(any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",names(constants)))||length(intersect(names(constants),c(names(data_schema),names(parameters)))))
      .mp_abort("E_SCHEMA_NAME","constant names must be disjoint plain identifiers")
    for(constant in constants)stan_constant(constant$value,constant$type,constant$units,constant$provenance)
  }
  if (any(!vapply(data_schema, inherits, logical(1), "measurement_data_field")))
    .mp_abort("E_SCHEMA", "every schema field must be a data_field")
  if (!any(vapply(data_schema, function(x) x$role == "response", logical(1))))
    .mp_abort("E_SCHEMA", "schema requires a response field")
  if (any(!vapply(parameters, function(x) all(c("support", "predictor", "receives", "levels", "units", "description") %in% names(x)), logical(1))))
    .mp_abort("E_PARAMETER", "every parameter requires a parameter_spec")
  if (is.null(likelihood$symbol) || !all(likelihood$arguments %in% c(names(data_schema), names(parameters), names(constants))))
    .mp_abort("E_LIKELIHOOD", "likelihood arguments must bind schema fields or named parameters")
  if (!all(parameter_dependencies %in% names(parameters)))
    .mp_abort("E_PARAMETER_DEPENDENCY", "unknown parameter dependency")
  if (!identical(sort(capabilities$capability), sort(model_capabilities()$capability)))
    .mp_abort("E_CAPABILITY", "complete capability table required")
  for(cap in c("simulate","posterior_predict","posterior_epred")) {
    available <- capabilities$available[capabilities$capability==cap]
    if(isTRUE(available) && !is.function(methods[[cap]])) .mp_abort("E_CAPABILITY",paste("method not registered for",cap))
  }
  # Capture new callbacks without source-debug environments before any prepared
  # fingerprint. Compilation then stabilizes JIT representation; neither step
  # invokes callbacks or freezes their genuine external dependencies.
  for (callback in list(validate_data, prepare_data)) if (!is.null(callback) && !is.function(callback))
    .mp_abort("E_CALLBACK", "validation/preparation callbacks must be functions")
  if(!is.null(validate_data))validate_data<-.mp_stable_callback(validate_data)
  if(!is.null(prepare_data))prepare_data<-.mp_stable_callback(prepare_data)
  for(i in seq_along(methods))if(is.function(methods[[i]]))
    methods[[i]]<-.mp_stable_callback(methods[[i]])
  structure(list(name = name, version = version, provenance = provenance,
                 data_schema = data_schema, parameters = parameters,
                 parameter_dependencies = parameter_dependencies, likelihood = likelihood,
                 validate_data = validate_data, prepare_data = prepare_data,
                 capabilities = capabilities, check_statistics = check_statistics,
                 backend = "contract_only", methods = methods, constants = constants), class = "observation_model")
}
print.observation_model <- function(x, ...) {
  cat("Observation model:", x$name, "v", x$version, "| backend:", x$backend, "\n")
  print(x$capabilities, row.names = FALSE); invisible(x)
}

# Copy only recognized function/syntax debug metadata.
# Preserve executable values, lexical environments, pairlist types and meaningful attributes.
# The fourth slot of a parsed function definition is source metadata; a literal
# srcref-class value elsewhere remains ordinary data.
.mp_callback_source_copy <- function(callback) {
    if (is.primitive(callback)) 
        return(callback)
    debug <- c("srcref", "wholeSrcref", "srcfile")
    syntax <- function(part) {
        if (!is.call(part) && !is.pairlist(part) && !is.expression(part)) 
            return(part)
        pair <- is.pairlist(part)
        for (name in debug) attr(part, name) <- NULL
        if (is.call(part) && length(part) == 4L && identical(part[[1L]], as.name("function")) && 
            inherits(part[[4L]], "srcref")) 
            part[4L] <- list(NULL)
        for (i in seq_along(part)) part[i] <- list(syntax(part[[i]]))
        if (pair) 
            as.pairlist(part)
        else part
    }
    out <- callback
    attrs <- attributes(out)
    attrs[intersect(names(attrs), debug)] <- NULL
    formals(out) <- syntax(formals(out))
    body(out) <- syntax(body(out))
    attributes(out) <- attrs
    out
}

# Normalize before compiling: assigning body/formals discards existing bytecode.
# This is a new-callback capture boundary. Historical objects and seals are untouched.
# The lifecycle check does not freeze genuine global or external dependencies.
.mp_stable_callback <- function(callback) {
    if (!is.function(callback)) 
        .mp_abort("E_CALLBACK", "callback must be a function")
    normalized <- .mp_callback_source_copy(callback)
    compiled <- if (is.primitive(normalized)) 
        normalized
    else compiler::cmpfun(normalized)
    copy <- unserialize(serialize(compiled, NULL))
    if (!identical(.mp_hash(compiled), .mp_hash(copy))) 
        .mp_abort("E_CALLBACK_LIFECYCLE", "this callback's captured environment is not serialization-stable; use a self-contained top-level validator or explicitly rebuild with a supported callback. Its environment and fingerprint were not rewritten")
    compiled
}

