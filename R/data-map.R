observe <- function(model, data, subject, task, response = NULL, fields = list(),
                    units, observation_id = NULL, source_id = NULL, parameter_bindings = NULL, parameters = NULL) {
  if (!inherits(model, "observation_model")) .mp_abort("E_MODEL", "model must be an observation_model")
  if (!is.data.frame(data) || !nrow(data)) .mp_abort("E_DATA", "data must be a nonempty data.frame")
  if(is.null(names(data)) || anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data)))
    .mp_abort("E_DATA_COLUMN", "observation data must have distinct nonempty column names")
  .mp_scalar_string(subject, "subject column"); .mp_scalar_string(task, "task column")
  if (missing(units)) .mp_abort("E_UNITS", "explicit observation units are required")
  .mp_scalar_string(units, "observation units")
  if(!is.null(model$required_units)&&!units%in%model$required_units) .mp_abort("E_UNITS",paste("model requires units:",paste(model$required_units,collapse=", ")))
  if (!all(c(subject, task) %in% names(data))) .mp_abort("E_DATA_COLUMN", "subject/task column missing")
  fields <- as.list(fields)
  response_fields <- names(Filter(function(x) x$role == "response", model$data_schema))
  if (!is.null(response)) {
    if (length(response_fields) != 1L) .mp_abort("E_RESPONSE", "bind multi-field responses with fields")
    if (response_fields %in% names(fields)) .mp_abort("E_RESPONSE", "response is mapped twice")
    fields[[response_fields]] <- response
  }
  .mp_names(fields, "fields mapping")
  if (!all(names(model$data_schema) %in% names(fields))) {
    absent <- setdiff(names(model$data_schema), names(fields))
    code <- if ("trials" %in% absent) "E_TRIALS_MISSING" else "E_FIELD_MISSING"
    .mp_abort(code, paste("bind schema fields:", paste(absent, collapse = ", ")))
  }
  if (!setequal(names(fields), names(model$data_schema))) .mp_abort("E_FIELD_MAPPING", "unknown schema field in fields mapping")
  if (any(!vapply(fields, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))) ||
      !all(unlist(fields) %in% names(data))) .mp_abort("E_DATA_COLUMN", "each field must bind one existing column")
  local_id <- .mp_key_component(data[[subject]], "subject ID")
  task_id <- .mp_key_component(data[[task]], "task ID")
  if (is.null(observation_id)) {
    ids <- paste(local_id, task_id, sep = "::")
    if (anyDuplicated(ids)) .mp_abort("E_OBSERVATION_KEY", "repeated subject/task rows require an explicit unique observation_id column")
    key_origin <- "subject/task (unique single observations)"
  } else {
    if (!observation_id %in% names(data)) .mp_abort("E_DATA_COLUMN", "observation_id column missing")
    ids <- .mp_key_component(data[[observation_id]], "observation ID")
    key_origin <- observation_id
  }
  if (anyDuplicated(ids)) .mp_abort("E_DUPLICATE_OBSERVATION", "observation IDs must be unique within a block")
  if (!is.null(source_id)) .mp_scalar_string(source_id, "source_id")
  mapped <- data.frame(.local_id = local_id, .task = task_id, .observation_id = ids,
                        stringsAsFactors = FALSE)
  for (field in names(fields)) mapped[[field]] <- data[[fields[[field]]]]
  .mp_validate_fields(model, mapped, allow_missing_response = TRUE)
  if (!is.null(model$prepare_data))
    .mp_abort("E_PREPARE_UNSUPPORTED", "custom preparation callbacks are registered but not executed in P0; bind explicitly prepared data")
  if (!is.null(model$validate_data)) {
    result <- model$validate_data(mapped)
    if (!isTRUE(result)) .mp_abort("E_CUSTOM_VALIDATION", "validate_data must return TRUE after a successful check")
  }
  captured_levels<-.bh_capture_parameter_levels(model,parameters)
  binomial_fields<-.bh_binomial_row_fields(model)
  block<-structure(list(model = model, data = mapped, units = units, mapping = fields,
                 subject_column = subject, task_column = task, observation_key_origin = key_origin,
                 source_id = source_id, parameter_bindings = parameter_bindings, transformations = "none; no automatic standardization",
                 data_diagnostics = list(zero_trial_observations = if (!is.null(binomial_fields)) ids[!is.na(mapped[[binomial_fields$trials]]) & mapped[[binomial_fields$trials]] == 0] else character())),
            class = "observation_block")
  if(!is.null(captured_levels))block$parameter_levels<-captured_levels
  block
}
.mp_validate_fields <- function(model, data, allow_missing_response) {
  for (n in names(model$data_schema)) {
    field <- model$data_schema[[n]]; x <- data[[n]]
    width<-if(is.null(field$width))1L else field$width
    if(width==1L&&!is.null(dim(x)) || width>1L&&(!is.matrix(x)||nrow(x)!=nrow(data)||ncol(x)!=width))
      .mp_abort("E_FIELD_DIMENSION",paste("field",n,"has wrong per-observation dimensions"))
    if (!is.numeric(x)) .mp_abort("E_FIELD_TYPE", paste("field", n, "must be numeric, not a factor or character"))
    allowed_missing <- allow_missing_response && field$role == "response"
    if (!allowed_missing && anyNA(x)) .mp_abort(if(field$role=="response")"E_MISSING_RESPONSE"else"E_MISSING_AUXILIARY", paste("missing values in required field", n))
    observed <- !is.na(x)
    bad <- observed & (!is.finite(x) | x < field$lower | x > field$upper)
    if (field$type == "positive_real") bad <- bad | (observed & x <= 0)
    if (field$type %in% c("integer", "nonnegative_integer", "binary")) bad <- bad | (observed & (x != floor(x) | abs(x) > .Machine$integer.max))
    if (field$type == "nonnegative_integer") bad <- bad | (observed & x < 0)
    if (field$type == "binary") bad <- bad | (observed & !(x %in% c(0, 1)))
    bad[is.na(bad)] <- FALSE
    if (any(bad)) {
      row<-if(is.matrix(bad))which(bad,arr.ind=TRUE)[1L,1L]else which(bad)[1L]
      .mp_abort("E_FIELD_SUPPORT",paste("field",n,"out of support at observation",data$.observation_id[row]))
    }
  }
  binomial_fields<-.bh_binomial_row_fields(model)
  if(!is.null(binomial_fields)) {
    y<-data[[binomial_fields$response]];n<-data[[binomial_fields$trials]]
    if(any(y<0|n<0|y>n,na.rm=TRUE)) .mp_abort("E_BINOMIAL_SUPPORT","binomial successes/trials must be nonnegative, with successes no greater than trials")
  }
  invisible(TRUE)
}
shared_subjects <- function(namespace) {
  .mp_scalar_string(namespace, "namespace"); .mp_key_component(namespace, "namespace")
  structure(list(type = "shared", namespace = namespace), class = "measurement_subjects")
}
subject_crosswalk <- function(data, namespace = "crosswalk") {
  if (!is.data.frame(data) || !all(c("model", "local_id", "person_id") %in% names(data)))
    .mp_abort("E_CROSSWALK", "crosswalk requires model/local_id/person_id columns")
  .mp_scalar_string(namespace, "namespace"); .mp_key_component(namespace, "namespace")
  data <- data[c("model", "local_id", "person_id")]
  for (n in names(data)) data[[n]] <- .mp_key_component(data[[n]], paste("crosswalk", n))
  if (anyDuplicated(paste(data$model, data$local_id, sep = "::")))
    .mp_abort("E_CROSSWALK", "each model/local_id must have exactly one person_id")
  if (anyDuplicated(paste(data$model, data$person_id, sep = "::")))
    .mp_abort("E_CROSSWALK", "P0 requires one local_id per model/person_id")
  structure(list(type = "crosswalk", namespace = namespace, data = data), class = "measurement_subjects")
}
missing_policy <- function(subjects = "union", response = "error") {
  if (!identical(subjects, "union")) .mp_abort("E_MISSING_POLICY", "P0 supports subject union only")
  list(subjects = subjects, response = match.arg(response, c("error", "observed_only")))
}
.mp_map_data <- function(models, subjects, missing, allow_empty_blocks=FALSE) {
  records <- list(); exclusions <- list(); source_keys <- character()
  for (name in names(models)) {
    block <- models[[name]]; d <- block$data
    for(key in c(".local_id",".task")) .mp_key_component(d[[key]],key)
    if(anyNA(d$.observation_id)||any(!nzchar(as.character(d$.observation_id)))) .mp_abort("E_OBSERVATION_KEY","observation keys cannot be missing or empty")
    if(anyDuplicated(d$.observation_id)) .mp_abort("E_DUPLICATE_OBSERVATION","bound data contains duplicate observation keys")
    .mp_validate_fields(block$model, d, allow_missing_response = TRUE)
    response_fields <- names(Filter(function(x) x$role == "response", block$model$data_schema))
    absent <- Reduce(`|`, lapply(d[response_fields], function(x)if(is.matrix(x))rowSums(is.na(x))>0 else is.na(x)))
    if (any(absent) && missing$response == "error")
      .mp_abort("E_MISSING_RESPONSE", paste("block", name, "has missing responses; explicitly choose observed_only to exclude them"))
    if (subjects$type == "shared") person <- d$.local_id else {
      cw <- subjects$data; index <- match(paste(name, d$.local_id, sep = "::"), paste(cw$model, cw$local_id, sep = "::"))
      if (anyNA(index)) .mp_abort("E_CROSSWALK", paste("unmapped local subject in block", name))
      person <- cw$person_id[index]
    }
    d$.person_key <- paste(subjects$namespace, person, sep = "::")
    d$.block <- name
    exclusions[[name]] <- d[absent, c(".block", ".observation_id", ".person_key"), drop = FALSE]
    d <- d[!absent, , drop = FALSE]
    if (!nrow(d)&&!allow_empty_blocks) .mp_abort("E_EMPTY_BLOCK", paste("no observed responses in block", name))
    .mp_validate_fields(block$model,d,allow_missing_response=FALSE)
    if(nrow(d)&&!is.null(block$model$validate_data)&&!isTRUE(block$model$validate_data(d))) .mp_abort("E_CUSTOM_VALIDATION",paste("mapped-data validation failed for",name))
    if (!is.null(block$source_id)) {
      keys <- paste(block$source_id, d$.observation_id, sep = "::")
      if (any(keys %in% source_keys)) .mp_abort("E_DOUBLE_USE", "same source_id/observation_id occurs in multiple likelihood blocks")
      source_keys <- c(source_keys, keys)
    }
    # Canonical sorting prevents row-order-based identity or index changes.
    d <- d[order(d$.person_key, d$.task, d$.observation_id), , drop = FALSE]
    rownames(d) <- NULL; records[[name]] <- d
  }
  people <- sort(unique(unlist(lapply(records, function(d) d$.person_key))))
  if(!length(people)) .mp_abort("E_EMPTY_DATA","no consumed observation rows remain in any block after the explicit missing-response policy")
  for (name in names(records)) records[[name]]$.person_index <- match(records[[name]]$.person_key, people)
  coverage <- matrix(vapply(records, function(d) people %in% d$.person_key, logical(length(people))),
                     nrow = length(people), ncol = length(records))
  dimnames(coverage) <- list(people, names(records))
  overlap <- crossprod(coverage * 1L)
  task_coverage <- do.call(rbind, lapply(records, function(d) unique(d[c(".person_key", ".block", ".task")])))
  list(blocks = records, subject_keys = people, coverage = coverage, overlap = overlap,
       task_coverage = task_coverage, excluded = do.call(rbind, exclusions),
       missing_assumption = if (missing$response == "observed_only") "Observed likelihood only; no imputation or MNAR correction" else "No missing responses allowed")
}
