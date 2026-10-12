source(file.path("helpers", "native-guard.R"))
# Bounded R-only numerical-message contract. Every producer is a labelled double.
# Optional retained-workflow evidence: BMEAS_NUMERICAL_MESSAGE_REAL_FIT points to
# the unchanged genuine-fit.rds; only its saved plain output fields are read.
source_root <- Sys.getenv('MEASUREMENTPKG_SOURCE')
source_mode <- nzchar(source_root)
if (source_mode) {
  for (file in list.files(file.path(source_root, 'R'), full.names=TRUE)) source(file)
} else library(bmeas)
private <- function(name) if (source_mode) get(name, .GlobalEnv) else getFromNamespace(name, 'bmeas')
public <- function(name) if (source_mode) get(name, .GlobalEnv) else getExportedValue('bmeas', name)

local({
  passed <- 0L
  ok <- function(value, label) {
    if (!isTRUE(value)) stop(label, call.=FALSE)
    passed <<- passed + 1L
    cat('PASS', label, '\n')
  }
  capture_messages <- private('.bh_capture_numerical_messages')
  project <- function(record) private('.bh_report_numerical')(
    list(availability='retained', record=record))
  rng <- function() if (exists('.Random.seed', .GlobalEnv, inherits=FALSE))
    get('.Random.seed', .GlobalEnv, inherits=FALSE) else NULL
  original_rng <- rng()
  on.exit({
    if (is.null(original_rng)) {
      if (exists('.Random.seed', .GlobalEnv, inherits=FALSE)) rm('.Random.seed', envir=.GlobalEnv)
    } else assign('.Random.seed', original_rng, envir=.GlobalEnv)
  }, add=TRUE)
  set.seed(2606)

  # Isolate the callback environment from changing test counters, so serialized
  # fixtures are stable and do not capture this test's entire local environment.
  callback_state <- new.env(parent=baseenv())
  callback_state$calls <- character()
  callback <- eval(quote(function(...) {
    calls <<- c(calls, 'saved_object_callback')
    stop('FORBIDDEN_SAVED_OBJECT_CALLBACK', call.=FALSE)
  }), callback_state)
  callback_names <- paste0(c('as.character.', 'as.list.', 'as.numeric.',
    'print.', 'format.', '[.', '[[.', '$.', 'length.', 'is.na.',
    'conditionMessage.'), 'numerical_message_test_trap')
  callback_originals <- lapply(callback_names, function(name) {
    if (exists(name, .GlobalEnv, inherits=FALSE))
      list(present=TRUE, value=get(name, .GlobalEnv, inherits=FALSE)) else list(present=FALSE)
  })
  for (name in callback_names) assign(name, callback, envir=.GlobalEnv)
  on.exit({
    for (i in seq_along(callback_names)) {
      if (callback_originals[[i]]$present)
        assign(callback_names[i], callback_originals[[i]]$value, envir=.GlobalEnv)
      else rm(list=callback_names[i], envir=.GlobalEnv)
    }
  }, add=TRUE)
  trapped <- function(x) structure(x, class='numerical_message_test_trap')

  # Each producer attempt makes precisely the one existing output call.
  attempt <- function(fun, label) {
    calls <- 0L
    backend <- list(output=function() {calls <<- calls + 1L; fun()})
    before_rng <- rng()
    result <- capture_messages(backend)
    ok(identical(calls, 1L), paste(label, 'calls output exactly once'))
    ok(identical(rng(), before_rng), paste(label, 'preserves RNG'))
    ok(identical(result$count_unit, 'captured_text_line_occurrence') &&
      identical(result$coverage, 'incomplete') &&
      identical(result$total_failed_evaluations, NA_integer_) &&
      identical(result$category_counts_may_overlap, TRUE),
      paste(label, 'retains occurrence unit incomplete coverage and unknown total'))
    result
  }
  success <- function(result, expected, structural, exception, label) {
    ok(identical(result$output_capture$status, 'succeeded') &&
      identical(result$observed_failure_messages, as.integer(length(expected))) &&
      identical(unname(result$messages), expected) &&
      identical(result$category_counts$structural_numerical, as.integer(structural)) &&
      identical(result$category_counts$stan_exception, as.integer(exception)), label)
  }
  generic <- 'Exception: normal_lpdf: Scale parameter is 0, but must be positive'
  structural <- c('correlation chart', 'correlation basis', 'cholesky_decompose',
    'share logit', 'share normalization', 'active share', 'derived share',
    'derived residual', 'component SD cannot be represented',
    'effective loading cannot be represented', 'required task')
  result <- attempt(function() structural, 'structural-only output')
  success(result, structural, length(structural), 0L, 'every unchanged specialized pattern is recognized')
  modern <- attempt(function() generic, 'generic-only output')
  success(modern, generic, 0L, 1L, 'generic literal Stan Exception is recognized')
  invalid_utf8 <- rawToChar(as.raw(c(0xc3,0x28)))
  Encoding(invalid_utf8) <- 'UTF-8'
  valid_unicode <- 'Exception: 中文 λ'
  local({
    old_warn<-options(warn=2);on.exit(options(old_warn),add=TRUE)
    for(value in list(invalid_utf8,c(generic,invalid_utf8),list(generic,invalid_utf8))) {
      bad_encoding<-attempt(function()value,'malformed encoded output')
      ok(bad_encoding$output_capture$status=='unusable_output' &&
        is.na(bad_encoding$observed_failure_messages),
        'invalid UTF-8 is unavailable before regex processing even when warnings are errors')
    }
    unicode_result<-attempt(function()valid_unicode,'valid Unicode output')
    success(unicode_result,valid_unicode,0L,1L,'valid Unicode remains exact without replacement or conversion')
    latin1<-rawToChar(c(charToRaw('Exception: '),as.raw(0xe9)))
    Encoding(latin1)<-'latin1'
    bytes_text<-rawToChar(c(charToRaw('Exception: '),as.raw(c(0xc3,0x28))))
    Encoding(bytes_text)<-'bytes'
    for(value in list(latin1,bytes_text)) {
      encoded_result<-attempt(function()value,'valid declared text encoding')
      success(encoded_result,value,0L,1L,'valid Latin-1 and bytes-labelled text remain supported without lossy replacement')
      ok(project(encoded_result)$message_text$availability=='retained',
        'recognized text retains a valid declared encoding for passive inspection')
    }
  })
  overlap_text <- 'Exception: correlation chart precision loss'
  overlap <- attempt(function() overlap_text, 'overlapping-category output')
  success(overlap, overlap_text, 1L, 1L, 'overlapping categories count one union occurrence')
  doubled_token <- 'Exception: Exception: normal_lpdf zero scale'
  result <- attempt(function() c(doubled_token, doubled_token), 'repeated lines and tokens')
  success(result, rep(doubled_token, 2L), 0L, 2L,
    'two equal lines stay two occurrences and two tokens on a line stay one')
  unrelated <- c('NaN Inf scale warning rejected proposal', 'exception: lowercase',
    'EXCEPTION: uppercase', 'Exception without colon', 'Sampling completed')
  result <- attempt(function() unrelated, 'unrelated output')
  success(result, character(), 0L, 0L, 'no broad warning nonfinite scale or rejection heuristic is added')
  for (separator in c('\r\n', '\n', '\r')) {
    result <- attempt(function() paste(c(generic, overlap_text, generic), collapse=separator),
      paste('embedded separator', encodeString(separator, quote='"')))
    success(result, c(generic, overlap_text, generic), 1L, 3L,
      'CRLF LF and CR each split into captured line occurrences')
  }
  result <- attempt(function() {cat(generic, '\n', sep=''); generic}, 'printed/returned duplication')
  success(result, rep(generic, 2L), 0L, 2L, 'equal printed and returned lines remain two captured occurrences')
  result <- attempt(function() {
    cat(paste(c(generic, overlap_text, generic), collapse='\r'), '\n', sep=''); NULL
  }, 'printed embedded separators')
  success(result, c(generic, overlap_text, generic), 1L, 3L,
    'printed text receives the same separator normalization as returned text')
  result <- attempt(function() list(chain_a=c(generic, generic), chain_b=overlap_text),
    'plain one-level returned list')
  success(result, c(generic, generic, overlap_text), 1L, 3L,
    'plain per-chain character vectors retain order and repeated occurrences')
  for (empty in list(NULL, character(), list(), list(character()))) {
    result <- attempt(function() empty, 'successful empty output')
    success(result, character(), 0L, 0L, 'successful empty capture is a retained zero')
  }
  failed <- attempt(function() {cat(generic, '\n'); stop('ordinary output failure')},
    'ordinary output error after partial printing')
  ok(identical(failed$output_capture$status, 'failed') &&
    identical(failed$observed_failure_messages, NA_integer_) &&
    !length(failed$messages) && all(is.na(unlist(failed$category_counts))),
    'capture errors retain unavailable counts even after partial printed output')
  malformed_returns <- list(classed_text=trapped(generic), dimensioned_text=matrix(generic, 1L),
    classed_list=trapped(list(generic)), dimensioned_list=structure(list(generic), dim=c(1L,1L)),
    nested_list=list(list(generic)), missing_text=c(generic, NA_character_),
    missing_list=list(generic, NA_character_), numeric=1, factor=factor(generic),
    executable=function() callback(), reference=new.env(parent=emptyenv()),
    custom_attribute=structure(generic, auxiliary='not plain text'))
  for (name in names(malformed_returns)) {
    result <- attempt(function() {cat(generic, '\n'); malformed_returns[[name]]},
      paste('unusable', name))
    ok(identical(result$output_capture$status, 'unusable_output') &&
      identical(result$observed_failure_messages, NA_integer_) &&
      !length(result$messages) && all(is.na(unlist(result$category_counts))),
      paste(name, 'refuses the whole captured count without manufacturing zero'))
  }
  interrupt_calls <- 0L
  interrupted <- tryCatch(capture_messages(list(output=function() {
    interrupt_calls <<- interrupt_calls + 1L
    signalCondition(structure(list(message='test user interrupt', call=NULL),
      class=c('interrupt', 'condition')))
    character()
  })), interrupt=identity)
  ok(inherits(interrupted, 'interrupt') && identical(interrupt_calls, 1L),
    'user interrupt propagates through capture instead of becoming unavailable evidence')
  ok(!length(callback_state$calls), 'all producer shape rejections avoid saved-object callbacks')

  # Cache creation uses the existing scalar-cache flow and a tiny backend double.
  # There is no compiler, sampler, original-fit method, or genuine draw acquisition.
  data <- data.frame(person=1:4, x1=1:4, x2=c(2,1,4,3), x3=c(3,1,2,4))
  spec <- public('bmeas')(public('bmf')(G=~x1+x2+x3), data, subject='person', fit=FALSE)
  retained_plan <- list(posterior_propriety=spec$checks$posterior_propriety,
    gaussian_diagnostics=spec$checks$gaussian_diagnostics,
    identification=spec$checks$identification)
  make_cache <- function(output, summary_error=FALSE, hmc_error=FALSE) {
    calls <- 0L
    backend <- list(output=function() {calls <<- calls + 1L; output()},
      summary=function(variables, ...) {
        if (summary_error) stop('labelled summary failure')
        data.frame(variable=c('alpha[1]', 'loading[1]', 'obs_sd[1]', 'z_raw[1,1]'),
          mean=0, median=0, sd=1, q5=-1, q95=1, rhat=1.5,
          ess_bulk=5, ess_tail=5, mcse_mean=.5)
      }, diagnostic_summary=function(quiet) {
        if (hmc_error) stop('labelled HMC failure')
        list(num_divergent=0L, num_max_treedepth=0L, ebfmi=1)
      }, draws=function(variables, format)
        matrix(rep(1, 2L), 2L, 1L, dimnames=list(NULL, 'L_component[1,1]')))
    cache <- private('.bh_cache_scalar_fit')(backend, spec, retained_plan)
    ok(identical(calls, 1L), 'the full scalar-cache flow calls output exactly once')
    cache
  }
  cache_empty <- make_cache(function() character())
  cache_message <- make_cache(function() generic)
  cache_error <- make_cache(function() stop('labelled capture failure'))
  cache_unusable <- make_cache(function() NA_character_)
  ok(identical(cache_empty$computation$status, 'computed') &&
    identical(cache_empty$numerical_evaluations$observed_failure_messages, 0L),
    'successful empty capture preserves existing computed status')
  ok(identical(cache_message$computation$status, 'warning') &&
    grepl('recognized numerical-message lines', cache_message$computation$reason, fixed=TRUE),
    'recognized generic exception produces an explicit message warning')
  for (cache in list(cache_error, cache_unusable)) ok(
    identical(cache$computation$status, 'warning') &&
      grepl('capture unavailable', cache$computation$reason, fixed=TRUE) &&
      is.na(cache$numerical_evaluations$observed_failure_messages),
    'unavailable capture produces a distinguishable incomplete-evidence warning')
  unaffected <- setdiff(names(cache_empty), c('computation', 'numerical_evaluations'))
  for (cache in list(cache_message, cache_error, cache_unusable)) ok(
    identical(cache[unaffected], cache_empty[unaffected]),
    'message evidence alone preserves HMC inference ID propriety conditioning and predictive evidence')
  for (output in list(function() generic, function() stop('capture failed'), function() NA_character_)) {
    for (failure in c('summary', 'hmc', 'both')) {
      cache <- make_cache(output, failure %in% c('summary','both'), failure %in% c('hmc','both'))
      ok(identical(cache$computation$status, 'failed'),
        paste(failure, 'failure has higher precedence than any numerical-message warning'))
    }
  }

  # Optional metadata must fail locally without dispatch, repair, or new evidence.
  always_unknown <- function(x, label) ok(
    identical(x$count_unit, 'captured_text_line_occurrence') &&
      identical(x$coverage, 'incomplete') && identical(x$category_counts_may_overlap, TRUE) &&
      identical(x$total_failed_evaluations$availability, 'unknown') &&
      identical(x$total_failed_evaluations$value, NA_real_), label)
  for (n in c(0L, 3L)) {
    x <- project(list(observed_failure_messages=n, messages=rep(generic, n)))
    ok(identical(x$recognized_lines$value, n) && x$recognized_lines$availability=='retained' &&
      x$provenance=='unavailable_in_historical_schema' && x$output_capture$status=='unknown' &&
      x$recognition$availability=='unavailable_in_cache',
      paste('legacy count', n, 'retains value without inventing modern capture or recognition'))
    always_unknown(x, 'legacy count never becomes a complete evaluation count')
  }
  for (record in list(NULL, list(), list(messages=character()),
      list(observed_failure_messages=NA_integer_))) {
    x <- project(record)
    ok(is.na(x$recognized_lines$value) && x$output_capture$status=='unknown',
      'absent legacy count and provenance remain unavailable')
    always_unknown(x, 'missing evidence retains unknown total and incomplete coverage')
  }
  for (domain in list(NULL, list(availability='unavailable_in_cache', record=NULL))) {
    x <- private('.bh_report_numerical')(domain)
    ok(is.na(x$recognized_lines$value) && x$recognized_lines$availability=='unavailable_in_cache',
      'absent optional numerical domain does not invent a zero')
  }
  x <- project(modern)
  ok(x$recognized_lines$value==1L && x$output_capture$status=='succeeded' &&
    x$recognition$availability=='retained' && x$category_counts$values$stan_exception==1L,
    'valid versioned generic count exposes retained capture and recognition provenance')
  x <- project(overlap)
  ok(x$recognized_lines$value==1L && sum(unlist(x$category_counts$values))==2L &&
    !length(x$issues), 'overlapping category counts are valid without being added to the union')
  x <- project(failed)
  ok(x$recognized_lines$availability=='unavailable_capture' &&
    x$category_counts$availability=='unavailable_capture' && is.na(x$recognized_lines$value),
    'versioned capture failure projects unavailable counts rather than zero')

  variants <- list()
  add_variant <- function(name, field, value, unavailable, keep_count=TRUE) {
    record <- modern
    record[field] <- list(value)
    variants[[name]] <<- list(record=record, unavailable=unavailable, keep_count=keep_count)
  }
  for (version in list(NULL, NA_character_, c('one','two'), trapped('bmeas_numerical_messages_v1'),
      'unknown_schema', matrix('bmeas_numerical_messages_v1',1L))) {
    add_variant(paste0('schema_', length(variants)+1L), 'schema_version', version,
      c('output_capture','recognition','category_counts'))
  }
  for (capture in list(NULL, 'succeeded', list(status='succeeded'),
      list(status=NA_character_,reason='saved'), list(status=c('succeeded','failed'),reason='saved'),
      list(status='succeeded',reason=trapped('saved')), list(status='invented',reason='saved'),
      structure(list('succeeded','failed','saved'),names=c('status','status','reason')),
      trapped(list(status='succeeded',reason='saved')))) {
    add_variant(paste0('capture_', length(variants)+1L), 'output_capture', capture, 'output_capture')
  }
  for (categories in list(NULL, c(structural_numerical=0L,stan_exception=1L),
      list(structural_numerical=0L), list(structural_numerical=0L,stan_exception=-1L),
      list(structural_numerical=0L,stan_exception=.5),
      list(structural_numerical=0L,stan_exception=trapped(1L)),
      structure(list(0L,1L),names=c('stan_exception','stan_exception')),
      trapped(list(structural_numerical=0L,stan_exception=1L)))) {
    add_variant(paste0('categories_', length(variants)+1L), 'category_counts', categories, 'category_counts')
  }
  for (count in list(-1L, .5, Inf, NaN, '1', c(1L,1L), trapped(1L), matrix(1L,1L))) {
    add_variant(paste0('count_', length(variants)+1L), 'observed_failure_messages', count,
      'recognized_lines', FALSE)
  }
  for (text in list(NA_character_, list(generic), trapped(generic), matrix(generic,1L))) {
    add_variant(paste0('text_', length(variants)+1L), 'messages', text, 'message_text')
  }
  add_variant('missing_rule', 'recognition_rule', NULL, 'recognition')
  add_variant('classed_rule', 'recognition_rule', trapped('structural_or_literal_Stan_Exception_v1'), 'recognition')
  add_variant('wrong_unit', 'count_unit', 'failed_proposals', c('recognition','category_counts'))
  add_variant('disjoint_categories', 'category_counts_may_overlap', FALSE, c('recognition','category_counts'))
  for (name in names(variants)) {
    variant <- variants[[name]]
    before <- serialize(variant$record, NULL, version=2)
    x <- project(variant$record)
    ok(all(vapply(x[variant$unavailable], function(node)
      startsWith(node$availability, 'unavailable'), logical(1))),
      paste(name, 'marks only invalid or absent optional evidence unavailable'))
    if (variant$keep_count) ok(identical(x$recognized_lines$value, 1L),
      paste(name, 'preserves the valid retained count'))
    else ok(is.na(x$recognized_lines$value), paste(name, 'does not repair a malformed count'))
    ok(identical(before, serialize(variant$record, NULL, version=2)),
      paste(name, 'leaves original metadata bytes unchanged'))
    always_unknown(x, paste(name, 'cannot establish complete failed-evaluation counts'))
  }
  for (record in list(trapped(modern), structure(modern,dim=c(length(modern),1L)),
      c(modern,list(observed_failure_messages=99L)))) {
    x <- project(record)
    ok(x$recognized_lines$availability=='unavailable_invalid_metadata' &&
      is.na(x$recognized_lines$value), 'classed dimensioned and duplicate-named records refuse ambiguous counts')
  }
  duplicate_domain <- structure(list('retained',modern,'unavailable_in_cache'),
    names=c('availability','record','availability'))
  x <- private('.bh_report_numerical')(duplicate_domain)
  ok(x$recognized_lines$availability=='unavailable_invalid_metadata',
    'duplicate domain names cannot silently select one availability')

  contradictions <- list()
  bad <- modern; bad$observed_failure_messages <- 2L; contradictions$message_count <- bad
  bad <- modern; bad$category_counts$stan_exception <- 2L; contradictions$category_above_union <- bad
  bad <- modern; bad$category_counts$stan_exception <- 0L; contradictions$category_below_union <- bad
  bad <- modern; bad$output_capture$status <- 'failed'; contradictions$failed_with_count <- bad
  bad <- modern; bad$output_capture$status <- 'unusable_output'; contradictions$unusable_with_count <- bad
  bad <- modern; bad$observed_failure_messages <- NULL; contradictions$success_without_count <- bad
  bad <- modern; bad$output_capture <- list(status='succeeded');bad$category_counts$stan_exception<-7L
  contradictions$malformed_capture_with_inconsistent_categories <- bad
  bad <- modern;bad$output_capture<-NULL;bad$messages<-character()
  contradictions$missing_capture_with_inconsistent_message_count <- bad
  for (name in names(contradictions)) {
    x <- project(contradictions[[name]])
    ok(x$recognized_lines$availability=='unavailable_invalid_metadata' &&
      is.na(x$recognized_lines$value) && length(x$issues)>0L,
      paste(name, 'makes contradictory count locally unavailable without reconciliation'))
    always_unknown(x, paste(name, 'keeps failed-evaluation totals unknown'))
  }
  bad <- modern; bad$coverage <- 'complete'; bad$total_failed_evaluations <- 14L
  x <- project(bad)
  ok(x$recognized_lines$value==1L && length(x$issues)>0L,
    'unsupported completeness claim is annotated without discarding the retained line count')
  always_unknown(x, 'even explicit contradictory completeness metadata cannot establish a total')
  local({
    old_warn<-options(warn=2);on.exit(options(old_warn),add=TRUE)
    bad<-modern;bad$messages<-invalid_utf8
    projected<-project(bad)
    ok(projected$recognized_lines$value==1L &&
      projected$message_text$availability=='unavailable_invalid_metadata',
      'invalid message encoding remains local and preserves a valid retained count')
    bad<-modern;bad$output_capture$reason<-invalid_utf8
    projected<-project(bad)
    ok(projected$recognized_lines$value==1L &&
      projected$output_capture$availability=='unavailable_invalid_metadata',
      'invalid capture-reason encoding remains local and preserves a valid retained count')
    bad<-modern;names(bad)[1L]<-invalid_utf8
    projected<-project(bad)
    ok(projected$recognized_lines$availability=='unavailable_invalid_metadata',
      'invalid record-name encoding does not masquerade as absent metadata')
    bad_domain<-list(availability='retained',record=modern);names(bad_domain)[1L]<-invalid_utf8
    projected<-private('.bh_report_numerical')(bad_domain)
    ok(projected$recognized_lines$availability=='unavailable_invalid_metadata',
      'invalid domain-name encoding is locally unavailable before field lookup')
  })
  ok(!length(callback_state$calls), 'optional report validation invokes no saved-object methods')

  # Terminal traps guard every public/reporting path against fresh acquisition.
  with_report_traps <- function(expr) {
    originals <- list(); route_calls <- character()
    install <- function(env, name) {
      if (!exists(name, env, inherits=FALSE)) return(invisible(NULL))
      value <- get(name, env, inherits=FALSE)
      if (!is.function(value)) return(invisible(NULL))
      locked <- bindingIsLocked(name, env)
      originals[[length(originals)+1L]] <<- list(env=env,name=name,value=value,locked=locked)
      if (locked) unlockBinding(name, env)
      terminal <- local({route <- name; function(...) {
        route_calls <<- c(route_calls, route)
        stop(paste('FORBIDDEN_REPORT_ROUTE', route), call.=FALSE)
      }})
      assign(name, terminal, env)
      if (locked) lockBinding(name, env)
    }
    on.exit({
      for (record in rev(originals)) {
        if (bindingIsLocked(record$name,record$env)) unlockBinding(record$name,record$env)
        assign(record$name,record$value,record$env)
        if (record$locked) lockBinding(record$name,record$env)
      }
    }, add=TRUE)
    env <- if (source_mode) .GlobalEnv else asNamespace('bmeas')
    for (name in c('bmeas','bhmm','check_model','standata','stancode','stan_data',
      'make_stan_code','prepare_measurement','.bh_prepare','.bh_validate_spec',
      '.mp_validate_spec','.mp_hash','.mp_sha256','.bh_compile_scalar','.bh_fit_scalar',
      '.bh_cache_scalar_fit','.bh_capture_numerical_messages','.bh_numeric_cache',
      '.bh_reference_report_cache','.bh_report_availability_cache','simulate_m3_responses'))
      install(env,name)
    for (name in c('readRDS','readLines','readBin','readChar','scan','load','saveRDS',
      'save','writeLines','writeBin','file','gzfile','bzfile','xzfile','url','system',
      'system2','dyn.load','loadNamespace')) install(baseenv(),name)
    for (name in c('read.table','read.csv','read.csv2')) install(asNamespace('utils'),name)
    value <- force(expr)
    if (length(route_calls)) stop(paste('Reporting touched',paste(route_calls,collapse=', ')))
    value
  }
  terminal_backend <- list(output=callback, summary=callback, diagnostic_summary=callback,
    draws=callback, output_files=callback, sample=callback)
  report_spec <- spec
  for (name in names(report_spec$models)) {
    report_spec$models[[name]]$model$validate_data <- callback
    report_spec$models[[name]]$model$informative_rows <- callback
  }
  make_fit <- function(record) {
    cache <- cache_empty
    cache$numerical_evaluations <- record
    # Deliberately historical status: passive reporting must not reassess it.
    cache$computation <- list(status='computed',reason='unchanged original producer record')
    cache$posterior_normalizability <- list(status='retained_fixture',reason='independent evidence')
    structure(list(spec=report_spec,cache=cache,backend=terminal_backend,
      run=list(chains=2L,iter_warmup=10L,iter_sampling=10L),
      provenance=list(execution_profile=list(used='labelled_cached_evidence'))),
      class=c('bmeas_fit','bhmm_fit'))
  }
  read_reports <- function(fit) {
    checks <- public('fit_checks')(fit)
    saved_summary <- summary(fit)
    list(checks=checks, summary=saved_summary,
      printed=capture.output(print(fit)),
      summary_printed=capture.output(print(saved_summary)),
      checks_printed=capture.output(print(checks)))
  }
  report_records <- c(list(legacy_zero=list(observed_failure_messages=0L,messages=character()),
    legacy_positive=list(observed_failure_messages=2L,messages=rep(generic,2L)),
    modern=modern, missing=NULL), contradictions,
    lapply(variants,function(x)x$record))
  for (name in names(report_records)) {
    fit <- make_fit(report_records[[name]])
    bytes <- serialize(fit,NULL,version=2); before_rng <- rng()
    report <- with_report_traps(read_reports(fit))
    ok(identical(report$checks$domains$numerical_evaluations$record,
      fit$cache$numerical_evaluations) &&
      identical(report$checks$domains$computation$record,fit$cache$computation) &&
      identical(report$checks$sampling$parameters,fit$cache$parameter_summary) &&
      identical(report$checks$sampling$chains$record,fit$cache$hmc) &&
      identical(report$checks$domains$identification$record,fit$cache$identification) &&
      identical(report$checks$domains$posterior_propriety$record,fit$cache$posterior_propriety) &&
      identical(report$checks$domains$posterior_normalizability$record,fit$cache$posterior_normalizability) &&
      identical(report$checks$predictive$assessment$record,fit$cache$predictive_fit),
      paste(name, 'keeps independent evidence and the original producer record unchanged'))
    ok(identical(bytes,serialize(fit,NULL,version=2)) && identical(before_rng,rng()),
      paste(name, 'public fit summary and report reads preserve bytes and RNG'))
    ok(identical(report$summary$checks,report$checks) &&
      any(grepl('Recognized cached numerical-message lines:',report$summary_printed,fixed=TRUE)) &&
      any(grepl('total failed evaluations: unknown',report$checks_printed,fixed=TRUE)) &&
      any(grepl('Numerical evaluation evidence: incomplete',report$printed,fixed=TRUE)),
      paste(name, 'all public printed views use the bounded numerical-evidence wording'))
    always_unknown(report$checks$domains$numerical_evaluations$interpretation,
      paste(name,'public report has unknown total and incomplete coverage'))
  }
  fit <- make_fit(list(observed_failure_messages=0L,messages=character()))
  report <- with_report_traps(read_reports(fit))
  ok(identical(names(report$checks),c('version','context','sampling','domains','predictive')) &&
    !is.null(report$checks$domains$numerical_evaluations$interpretation),
    'the additive numerical interpretation preserves the existing five top-level report views')
  duplicate_fit <- make_fit(modern)
  duplicate_fit$cache <- c(duplicate_fit$cache,list(numerical_evaluations=failed))
  duplicate_bytes <- serialize(duplicate_fit,NULL,version=2)
  duplicate_report <- with_report_traps(public('fit_checks')(duplicate_fit))
  duplicate_domain <- duplicate_report$domains$numerical_evaluations
  ok(duplicate_domain$availability=='unavailable_invalid_metadata' &&
    is.null(duplicate_domain$record) &&
    is.na(duplicate_domain$interpretation$recognized_lines$value) &&
    identical(duplicate_report$sampling$parameters,duplicate_fit$cache$parameter_summary) &&
    identical(duplicate_report$sampling$chains$record,duplicate_fit$cache$hmc) &&
    identical(duplicate_report$domains$computation$record,duplicate_fit$cache$computation) &&
    identical(duplicate_bytes,serialize(duplicate_fit,NULL,version=2)),
    'duplicate outer numerical cache keys remain locally unavailable without selecting a record or erasing valid metrics')
  detached <- fit; detached$backend <- NULL
  detached <- unserialize(serialize(detached,NULL,version=2))
  detached_bytes <- serialize(detached,NULL,version=2)
  ok(identical(report,with_report_traps(read_reports(detached))) &&
    identical(detached_bytes,serialize(detached,NULL,version=2)),
    'serialized detached fit reports without its backend and preserves historical zero provenance')
  saved_summary <- unserialize(serialize(report$summary,NULL,version=2))
  historical_summary <- saved_summary
  historical_summary[c('checks','context','blocks','overlap','excluded_responses')] <- NULL
  saved_checks <- unserialize(serialize(report$checks,NULL,version=2))
  old_checks <- saved_checks; old_checks$domains$numerical_evaluations$interpretation <- NULL
  for (saved in list(saved_summary,historical_summary,saved_checks,old_checks)) {
    before <- serialize(saved,NULL,version=2)
    printed <- with_report_traps(capture.output(print(saved)))
    ok(any(grepl('Recognized cached numerical-message lines: 0',printed,fixed=TRUE)) &&
      any(grepl('total failed evaluations: unknown',printed,fixed=TRUE)) &&
      identical(before,serialize(saved,NULL,version=2)),
      'saved summary and report shapes print a legacy zero passively without rewriting bytes')
  }
  duplicate_report<-saved_checks
  duplicate_report$domains<-c(duplicate_report$domains,
    list(numerical_evaluations=list(availability='retained',record=modern)))
  duplicate_bytes<-serialize(duplicate_report,NULL,version=2)
  duplicate_print<-with_report_traps(capture.output(print(duplicate_report)))
  ok(any(grepl('Recognized cached numerical-message lines: unavailable | unavailable_invalid_metadata',
    duplicate_print,fixed=TRUE)) &&
    identical(duplicate_bytes,serialize(duplicate_report,NULL,version=2)),
    'saved report duplicate numerical domains never select the first count and preserve original bytes')
  legacy_ppc <- list(table=data.frame(statistic='retained',value=1),metadata=list(source='legacy'))
  legacy_fit <- structure(list(cache=list(ppc=legacy_ppc),backend=terminal_backend),
    class=c('measurement_fit','bhmm_fit'))
  legacy_expected <- legacy_ppc
  legacy_expected$metadata$replicate_statistics_status <- 'unavailable_legacy_run'
  ok(identical(with_report_traps(public('fit_checks')(legacy_fit)),legacy_expected),
    'legacy measurement_fit retains its existing PPC-only dispatch')
  rm('.Random.seed',envir=.GlobalEnv)
  invisible(with_report_traps(read_reports(detached)))
  ok(is.null(rng()), 'passive reporting never initializes an absent RNG state')
  set.seed(2606)
  ok(!length(callback_state$calls), 'public saved and detached reporting touches no backend or model callbacks')

  # Explicit opt-in to the pinned historical fixture. No original method is called.
  real_path <- Sys.getenv('BMEAS_NUMERICAL_MESSAGE_REAL_FIT')
  real_fit_checked <- FALSE
  if (nzchar(real_path)) {
    before_md5 <- unname(tools::md5sum(real_path))
    ok(identical(before_md5,'aa24f2bcc1310f8548a685bc9b5df2c1'),
      'optional real-fit fixture matches the retained genuine-fit MD5')
    before_rng <- rng()
    real_fit <- readRDS(real_path)
    real_cache <- .subset2(unclass(real_fit),'cache')
    cache_bytes <- serialize(real_cache,NULL,version=2)
    plain_env_get <- function(env,name) {
      if (!is.environment(env) || !exists(name,env,inherits=FALSE) || bindingIsActive(name,env))
        stop(paste('Retained non-active data binding unavailable:',name),call.=FALSE)
      get(name,env,inherits=FALSE)
    }
    backend <- .subset2(unclass(real_fit),'backend')
    runset <- plain_env_get(backend,'runset')
    procs <- plain_env_get(runset,'procs')
    enclosed <- plain_env_get(procs,'.__enclos_env__')
    private_fields <- plain_env_get(enclosed,'private')
    retained_output <- plain_env_get(private_fields,'proc_output_')
    plain_character <- function(x) typeof(x)=='character' && !is.object(x) &&
      is.null(dim(x)) && !anyNA(x)
    ok(typeof(retained_output)=='list' && !is.object(retained_output) &&
      length(retained_output)==2L && all(vapply(retained_output,plain_character,logical(1))) &&
      identical(unname(lengths(retained_output)),c(56L,61L)),
      'the original saved proc_output_ contains only the two retained plain character vectors')
    output_bytes <- serialize(retained_output,NULL,version=2)
    lines <- unlist(retained_output,use.names=FALSE)
    old_pattern <- paste(structural,collapse='|')
    exception_lines <- lines[grepl('Exception:',lines,fixed=TRUE)]
    ok(sum(grepl(old_pattern,lines))==0L && length(exception_lines)==1L &&
      grepl('Scale parameter is 0, but must be positive',exception_lines,fixed=TRUE) &&
      lengths(regmatches(exception_lines,gregexpr('Exception:',exception_lines,fixed=TRUE)))==2L,
      'retained actual text proves old filter zero and one zero-scale line with two Exception tokens')
    actual <- attempt(function() retained_output, 'labelled output-only retained-text double')
    success(actual,exception_lines,0L,1L,
      'the new recognizer counts the genuine retained generic exception exactly once')
    ok(identical(real_cache$numerical_evaluations$observed_failure_messages,0L),
      'the original producer cache remains its historical recognized zero')
    # Public reporting uses only a detached copy; no original backend method runs.
    real_detached <- real_fit; real_detached$backend <- NULL
    reported <- with_report_traps(public('fit_checks')(real_detached))
    actual_interpretation <- reported$domains$numerical_evaluations$interpretation
    ok(identical(actual_interpretation$recognized_lines$value,0L) &&
      actual_interpretation$recognition$availability=='unavailable_in_cache' &&
      actual_interpretation$output_capture$status=='unknown' &&
      identical(reported$domains$computation$record,real_cache$computation),
      'actual historical reporting does not import the separate retained-text audit result')
    always_unknown(actual_interpretation,'actual retained workflow keeps incomplete evidence and unknown total')
    ok(identical(cache_bytes,serialize(.subset2(unclass(real_fit),'cache'),NULL,version=2)) &&
      identical(output_bytes,serialize(plain_env_get(private_fields,'proc_output_'),NULL,version=2)) &&
      identical(before_md5,unname(tools::md5sum(real_path))) && identical(before_rng,rng()),
      'actual fit file cache retained output and RNG remain unchanged')
    real_fit_checked <- TRUE
  } else cat('SKIP retained actual-fit evidence: BMEAS_NUMERICAL_MESSAGE_REAL_FIT is unset\n')
  cat('NUMERICAL_MESSAGE_REAL_FIT_CHECKED=',tolower(as.character(real_fit_checked)),'\n',sep='')
  cat('NUMERICAL_MESSAGE_CHECKS_PASSED=',passed,'\n',sep='')
  cat('Frontend numerical-message checks:',passed,
    'passed. No compiler sampler PPC or original-fit backend method executed.\n')
})
