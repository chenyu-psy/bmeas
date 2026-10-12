source(file.path("helpers", "native-guard.R"))
# Cached reporting contract only: deterministic retained tables, no posterior draws.
# Run from sources with MEASUREMENTPKG_SOURCE or against an installed namespace.
source_root <- Sys.getenv('MEASUREMENTPKG_SOURCE')
source_mode <- nzchar(source_root)
if (source_mode) {
  source_root <- normalizePath(source_root)
  for (file in list.files(file.path(source_root, 'R'), full.names=TRUE)) source(file)
} else library(bmeas)
private <- function(name) if (source_mode) get(name, .GlobalEnv) else getFromNamespace(name, 'bmeas')
public <- function(name) if (source_mode) get(name, .GlobalEnv) else getExportedValue('bmeas', name)
passed <- 0L
ok <- function(value, label) {
  if (!isTRUE(value)) stop(label, call.=FALSE)
  passed <<- passed + 1L
  cat('PASS', label, '\n')
}
err <- function(expr, code, label=code) {
  result <- tryCatch({force(expr); NULL}, error=identity)
  ok(inherits(result, 'measurement_error') && identical(result$code, code), label)
  invisible(result)
}

# Every reporting read runs behind terminal doubles. Binding restoration also
# works on an assertion failure. Backend and callback doubles fail before work.
with_report_traps <- function(expr) {
  trapped <- character()
  terminal_before <- report_terminal_calls
  originals <- list()
  install <- function(env, name) {
    if (!exists(name, env, inherits=FALSE)) return(invisible(NULL))
    value <- get(name, env, inherits=FALSE)
    if (!is.function(value)) return(invisible(NULL))
    locked <- bindingIsLocked(name, env)
    originals[[length(originals)+1L]] <<- list(env=env, name=name, value=value, locked=locked)
    if (locked) unlockBinding(name, env)
    terminal <- local({route <- name; function(...) {
      trapped <<- c(trapped, route)
      stop(paste('FORBIDDEN_REPORT_ROUTE', route), call.=FALSE)
    }})
    assign(name, terminal, env)
    if (locked) lockBinding(name, env)
  }
  on.exit({
    for (record in rev(originals)) {
      if (bindingIsLocked(record$name, record$env)) unlockBinding(record$name, record$env)
      assign(record$name, record$value, record$env)
      if (record$locked) lockBinding(record$name, record$env)
    }
  }, add=TRUE)
  env <- if (source_mode) .GlobalEnv else asNamespace('bmeas')
  for (name in c('bmeas','bhmm','check_model','model_table','get_prior',
    'standata','stancode','stan_data','make_stan_code','prepare_measurement',
    '.bh_prepare','.bh_validate_spec','.bh_check_spec','.mp_validate_spec',
    '.mp_hash','.mp_sha256','.mp_adapter_resources','.mp_toolchain_fingerprint',
    '.bh_compile_scalar','.bh_fit_scalar','.bh_cache_scalar_fit','.mp_cache_fit',
    '.bh_numeric_cache','.bh_affine_matrix_cache','.bh_reference_report_cache',
    '.bh_report_availability_cache','.bh_execution_profile','.bh_information',
    '.bh_row_information','.mp_informative_data','simulate_m3_responses')) install(env, name)
  for (name in c('readRDS','readLines','readBin','readChar','scan','load',
    'saveRDS','save','writeLines','writeBin','file','gzfile','bzfile','xzfile',
    'url','file.copy','file.create','file.rename','file.remove','unlink',
    'dir.create','system','system2','dyn.load','loadNamespace')) install(baseenv(), name)
  for (name in c('read.table','read.csv','read.csv2','write.table','write.csv',
    'write.csv2')) install(asNamespace('utils'), name)
  value <- force(expr)
  if (length(trapped)) stop(paste('report touched terminal route:', paste(trapped, collapse=', ')))
  if (!identical(report_terminal_calls,terminal_before)) stop('report touched a backend or model callback')
  value
}
read_report <- function(fit) {
  checks <- public('fit_checks')(fit)
  out <- summary(fit)
  list(checks=checks, summary=out,
    parameters=public('parameter_summary')(fit), posterior=public('posterior_summary')(fit),
    printed=capture.output(print(fit)), summary_printed=capture.output(print(out)),
    checks_printed=capture.output(print(checks)))
}
report_terminal_calls <- character()
report_terminal <- function(route) {
  report_terminal_calls <<- c(report_terminal_calls,route)
  stop(paste('FORBIDDEN_REPORT_ROUTE',route),call.=FALSE)
}
terminal_backend <- list(summary=function(...) report_terminal('backend_summary'),
  diagnostic_summary=function(...) report_terminal('backend_diagnostics'),
  draws=function(...) report_terminal('backend_draws'),
  output=function(...) report_terminal('backend_output'),
  output_files=function(...) report_terminal('backend_files'),
  sample=function(...) report_terminal('backend_sample'))

# One mixed, unbalanced, missing-response context also supplies marker aliases.
gaussian_data <- data.frame(person=c('p1','p2','p3','p1','p4'),
  task=c('x1','x2','x3','x2','x1'), y=c(.2,.4,-.1,.3,NA_real_))
count_data <- data.frame(person=c('p1','p2','p5','p2'), task=c('t1','t2','t1','t1'),
  successes=c(1L,0L,0L,1L), trials=c(2L,0L,0L,3L))
blocks <- list(scores=observe(gaussian_model(), gaussian_data, 'person','task','y', units='points'),
  counts=observe(binomial_model(), count_data, 'person','task',
    fields=list(successes='successes',trials='trials'), units='successes'))
mixed <- bmeas(bmf(G=~scores$mu[c('x1','x2','x3')],
  scale=list(G=list(marker=~scores$mu['x1'])),
  loadings=list(equal=list(pair=list(G=~scores$mu[c('x2','x3')]))),
  residual=list(targets=~counts$p[c('t1','t2')])), blocks,
  subjects='shared', missing='observed_only', fit=FALSE)

# Two small public specifications distinguish raw-B and final-R centered laws.
# The complete roster contains an unobserved person; x3 has exact zero support.
centered_data <- data.frame(person=as.character(1:4), x1=c(.1,.5,.7,.2),
  x2=c(.3,.2,.1,.4), x3=c(.8,.2,.6,.1))
centered_formula <- function(raw) bmf(A=~x1,B=~x2,C=~x3,
  loadings=list(fixed=list(list(value=1,cells=list(A=~x1,B=~x2)),
    list(value=0,cells=list(C=~x3)))),
  cor=if(raw) list(default='free',coordinates=list(type='ordered_blocks',
    blocks=list(first=~A,second=~B,third=~C))) else 'free',
  residual=list(targets=~x1+x2),
  person=list(distribution='sample_centered',ids=c('1','2','3','4','unseen')),
  augmentation=list(type='task_cholesky',targets=~x1+x2))
centered_raw <- bmeas(centered_formula(TRUE), centered_data, subject='person',
  prior=set_prior('normal(0,.2)','cor_raw'), fit=FALSE)
centered_final <- bmeas(centered_formula(FALSE), centered_data, subject='person', fit=FALSE)

# Values are deliberately diagnostic fixtures, not simulated posterior evidence.
# Fixed cells stay in their authoritative tables and acquire no uncertainty row.
parameters <- data.frame(variable=c('loading[1]','component_sd[1]',
  'effective_loadings[2,1]','f[1,1]','z_raw[1,1]','e_raw[1,1]','opaque_saved_name'),
  parameter_key=c(mixed$tables$loading_primitives$parameter[1L],
    mixed$tables$component_scales$parameter[1L], 'derived:effective:x2',
    'scaled:score:p1','computational:z_raw[1,1]','computational:e_raw[1,1]',
    'opaque::key::that::must::not::be::parsed'),
  parameter_role=c('loading','component_sd','derived_effective_loading',
    'scaled_component_person_score','latent_person_coordinate',
    'latent_person_coordinate','computational'),
  mean=c(.4,.8,.32,0,0,0,1), median=c(.4,.8,.3,0,0,0,1),
  sd=c(.1,.2,.12,1,1,1,.3), q10=c(.2,.5,.1,-1,-1,-1,.6),
  q90=c(.6,1.1,.5,1,1,1,1.4), mcse_mean=c(.02,.03,.02,.1,.2,.2,.04),
  rhat=c(1.04,Inf,NA_real_,NaN,1.08,Inf,1.08),
  ess_bulk=c(700,9,NA_real_,NaN,5,5,Inf),
  ess_tail=c(800,4,NA_real_,NaN,4,9,-Inf), stringsAsFactors=FALSE)
parameters$loading_aliases <- c(mixed$tables$loading_primitives$members[1L], rep(NA_character_,6L))
parameters$correlation_alias_cells <- I(rep(list(data.frame()), nrow(parameters)))
marker_map <- private('.bh_marker_summary_map')(mixed)
mapped <- match(parameters$variable,marker_map$variable)
parameters$parameter_key[!is.na(mapped)] <- marker_map$parameter_key[mapped[!is.na(mapped)]]
hmc <- list(num_divergent=c(chain_C=0,chain_A=2,chain_missing=NA_real_),
  num_max_treedepth=c(1,0), ebfmi=c(chain_D=.6,chain_B=.2,chain_A=.2,chain_bad=NaN),
  chain_id=c('chain_C','chain_A','chain_missing'))
make_fit <- function(spec, tab=parameters, hmc_record=hmc) {
  cache <- list(parameter_summary=tab,hmc=hmc_record,
    computation=list(status='warning',reason='retained deterministic diagnostic fixture'),
    identification=spec$checks$identification,
    posterior_propriety=spec$checks$posterior_propriety,
    posterior_normalizability=spec$checks$posterior_normalizability,
    gaussian_diagnostics=spec$checks$gaussian_diagnostics,
    initialization=spec$checks$initialization,
    inference=list(status='not_assessed',reason='bounded engineering evidence only'),
    predictive_fit=list(status='unsupported',reason='new frontend PPC has not been qualified; no simulation ran'),
    prior_sensitivity=list(status='not_assessed',reason='no sensitivity result retained'),
    admissibility=list(status='warning',reason='retained stored-matrix export screen only',
      threshold=1e-6,draws=data.frame(draw=1L,layer='component',matrix_export_warning=TRUE)),
    numerical_evaluations=list(observed_failure_messages=2L,
      messages=c('retained numerical message one','retained numerical message two'),
      count_scope='observed console lines, not failed density evaluations'),
    optional_reports=list(status='unavailable_values',unavailable_values=1L,
      reason='retained optional matrix export warning'))
  for(name in c('parameter_roles','fixed_parameters','loading_cells','loading_primitives',
    'component_scales','components')) cache[[name]] <- spec$tables[[name]]
  # A callback trap is intentionally present in the saved model. Reporting must
  # not revalidate/reseal an embedded historical specification to inspect it.
  for(name in names(spec$models)) {
    spec$models[[name]]$model$validate_data <- function(...) report_terminal('model_callback')
    spec$models[[name]]$model$informative_rows <- function(...) report_terminal('information_callback')
  }
  structure(list(spec=spec,cache=cache,backend=terminal_backend,
    run=run_config(chains=2L,parallel_chains=1L,threads_per_chain=1L,
      iter_warmup=15L,iter_sampling=25L,seed=2506L,cpu_budget=1L,
      max_observations=20L,max_subjects=5L,max_saved_draws=50L),
    provenance=list(kind='smoke',execution_profile=list(used='deterministic_cache_fixture'),
      exploratory_rank_override=TRUE,output_files='FORBIDDEN_SAVED_POSTERIOR.csv')),
    class=c('bmeas_fit','bhmm_fit'))
}
fit <- make_fit(mixed)
fit$cache$derived_marker_estimands <- marker_map
before <- serialize(fit,NULL,version=2)
set.seed(2506)
rng_before <- .Random.seed
report <- with_report_traps(read_report(fit))
checks <- report$checks
ok(inherits(checks,'bmeas_fit_checks') && identical(names(checks),
  c('version','context','sampling','domains','predictive')), 'public fit_checks has the five stable views')
ok(identical(checks$version,'bmeas_cached_fit_checks_v1'), 'report schema version is explicit')
ok(identical(report$parameters,parameters) && identical(report$parameters,report$posterior) &&
  identical(checks$sampling$parameters,parameters), 'all public parameter accessors return the exact cached table')
ok(identical(report$summary$checks,checks) && identical(report$summary$context,checks$context),
  'summary and fit_checks share the same structured projection')
ok(identical(class(report$summary),c('summary.bmeas_fit','summary.bhmm_fit')) &&
  identical(report$summary$cache,fit$cache) && identical(report$summary$run,fit$run),
  'summary retains its old classes and complete cache/run fields')
for(name in c('components','component_groups','parameter_roles','fixed_parameters','task_target_domain',
  'loading_cells','loading_primitives','component_scales','parameter_residuals','targets',
  'subjects','observations','person_law','augmentation','correlation_coordinates',
  'execution_profile','exploratory_rank_override'))
  ok(name %in% names(report$summary), paste('summary preserves field',name))

ex <- checks$sampling$extrema
ok(identical(ex$rhat$value,Inf) && identical(ex$rhat$rows$row_index,c(2L,6L)) &&
  identical(ex$rhat$rows$parameter_key,parameters$parameter_key[c(2L,6L)]),
  'positive-infinite Rhat and every tied scientific/latent key survive in stored order')
ok(ex$rhat$coverage$rows==7L && ex$rhat$coverage$finite==3L &&
  ex$rhat$coverage$missing==1L && ex$rhat$coverage$nan==1L &&
  ex$rhat$coverage$positive_infinity==2L, 'Rhat coverage distinguishes missing NaN and recorded infinity')
ok(ex$ess_bulk$value==5 && identical(ex$ess_bulk$rows$row_index,c(5L,6L)) &&
  isTRUE(ex$ess_bulk$finite_only), 'bulk ESS extrema retain bad latent rows and every tie')
ok(ex$ess_tail$value==4 && identical(ex$ess_tail$rows$row_index,c(2L,5L)) &&
  ex$ess_tail$coverage$negative_infinity==1L, 'tail ESS is finite-only with nonfinite coverage retained')
ok(all(vapply(ex,function(x)identical(x$threshold$status,'not_recorded'),logical(1))),
  'absent thresholds are not replaced by an unstated convergence policy')
ok(checks$sampling$parameters$ess_bulk[1L]==700 && checks$sampling$parameters$ess_tail[1L]==800,
  'ESS values are not clipped to configured draw counts')
ok(identical(checks$sampling$parameters$loading_aliases,parameters$loading_aliases) &&
  identical(checks$sampling$parameters$parameter_role,parameters$parameter_role),
  'aliases derived estimands and computational roles remain unchanged')
ok(all(c('q10','q90','mcse_mean') %in% names(checks$sampling$parameters)) &&
  !any(grepl('95%|90% equal-tail',report$checks_printed)),
  'arbitrary saved intervals and mean MCSE acquire no invented interval definition')
ok(identical(checks$sampling$interval_columns,c('q10','q90')) &&
  identical(checks$sampling$mcse_column,'mcse_mean'),
  'report paths identify each retained interval column and posterior-mean MCSE')

chains <- checks$sampling$chains
ok(identical(chains$record,hmc), 'complete raw HMC cache remains available')
ok(identical(vapply(chains$metrics,function(x)nrow(x$entries),integer(1)),
  c(num_divergent=3L,num_max_treedepth=2L,ebfmi=4L)), 'unequal HMC vectors are never recycled')
ok(identical(chains$metrics$num_divergent$entries$chain_id,hmc$chain_id) &&
  all(is.na(chains$metrics$num_max_treedepth$entries$chain_id)),
  'recorded chain identities persist and absent chain identities stay unavailable')
ok(chains$metrics$num_divergent$observed_total==2 &&
  !chains$metrics$num_divergent$complete_for_retained_entries &&
  chains$metrics$num_max_treedepth$observed_total==1,
  'divergence and depth counts remain separate without filling missing counts with zero')
ok(chains$metrics$ebfmi$minimum$value==.2 &&
  identical(chains$metrics$ebfmi$minimum$entries$stored_row_index,c(2L,3L)),
  'minimum E-BFMI retains every tied chain and its stored index')

ctx <- checks$context
block <- ctx$blocks[match(c('scores','counts'),ctx$blocks$block),,drop=FALSE]
ok(identical(as.integer(block$consumed_rows),c(4L,4L)) &&
  identical(as.integer(block$observed_people),c(3L,3L)),
  'unbalanced blocks report consumed likelihood rows and observed people separately')
ok(identical(as.integer(block$informative_rows),c(4L,2L)) &&
  identical(as.integer(block$excluded_rows),c(1L,0L)),
  'zero-trial consumption is distinct from informative and excluded observations')
ok(identical(ctx$overlap$record,mixed$data_map$overlap) &&
  ctx$subjects$record$joint_subject_count==4L, 'saved overlap is retained as observed coverage')
ok(identical(block$response_units,c('points','successes')) &&
  identical(ctx$targets$record,mixed$tables$targets), 'response units and predictor scales preserve their saved meaning')
ok(identical(ctx$parameter_roles$record,mixed$tables$parameter_roles) &&
  identical(ctx$fixed_parameters$record,mixed$tables$fixed_parameters),
  'fixed operands and declared levels have no invented uncertainty rows')
membership <- ctx$membership$record
ok(all(membership$membership[membership$cached_row==7L]=='unavailable_in_cache') &&
  all(is.na(membership$block[membership$cached_row==7L])),
  'opaque display keys cannot invent block membership')
ok(identical(ctx$run$configured$record,fit$run) &&
  ctx$run$observed$availability=='unavailable_in_cache' &&
  ctx$run$summary_method$availability=='unavailable_in_cache',
  'configured chains and iterations do not become observed draws or an assumed estimator')
observed <- fit
observed$cache$report_metadata <- list(observed_run=list(chains=2L,draws_per_chain=c(12L,11L),
  total_draws=23L,source='independently retained count fixture'),
  monitored=c('loading','component_sd'),summary_method=list(method='retained_test_method',version='0'))
observed$cache$diagnostic_thresholds <- list(rhat=list(value=1.3,origin='historical_fixture',
  version='old',applicability='retained historical run',used_for_warning=TRUE))
observed_report <- with_report_traps(public('fit_checks')(observed))
ok(identical(observed_report$context$run$configured$record,fit$run) &&
  identical(observed_report$context$run$observed$record,observed$cache$report_metadata$observed_run),
  'independently retained observed counts remain separate from configured controls')
ok(identical(observed_report$sampling$thresholds$record,observed$cache$diagnostic_thresholds) &&
  identical(observed_report$sampling$extrema$rhat$threshold$record,observed$cache$diagnostic_thresholds$rhat) &&
  observed_report$sampling$extrema$rhat$threshold$status=='recorded_not_reapplied',
  'historical thresholds keep their source applicability and warning record without reassessment')

for(name in c('identification','posterior_propriety','posterior_normalizability',
  'inference','prior_sensitivity','admissibility','numerical_evaluations','optional_reports','initialization')) {
  expected <- fit$cache[[name]]
  if(!is.null(expected)) ok(identical(checks$domains[[name]]$record,expected) &&
    checks$domains[[name]]$availability=='retained', paste('saved evidence is not upgraded:',name))
}
ok(identical(checks$predictive$assessment$record,fit$cache$predictive_fit) &&
  checks$predictive$results$availability=='unavailable_in_cache',
  'unsupported PPC and absent predictive results stay distinct from sampler diagnostics')
ok(!any(c('good_fit','model_passed','loo','waic','dic') %in% names(checks)),
  'report introduces neither a global fit verdict nor extra model criteria')
ok(identical(before,serialize(fit,NULL,version=2)) && identical(rng_before,.Random.seed),
  'first public reads leave fit/spec/cache bytes and RNG unchanged')
ok(identical(report,with_report_traps(read_report(fit))) &&
  identical(before,serialize(fit,NULL,version=2)) && identical(rng_before,.Random.seed),
  'repeat public reads are deterministic and byte-preserving')
rm('.Random.seed',envir=.GlobalEnv)
invisible(with_report_traps(read_report(fit)))
ok(!exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE), 'cache reads do not initialize an absent RNG state')
assign('.Random.seed',rng_before,envir=.GlobalEnv)

# Focused pre-report summary-shape regression. This is a synthetic historical
# shape, not an archived artifact: the new additive fields are deliberately
# removed before an in-memory serialization round trip and public S3 printing.
historical_summary <- report$summary
historical_summary[c('checks','context','blocks','overlap','excluded_responses')] <- NULL
historical_summary <- unserialize(serialize(historical_summary,NULL,version=2))
historical_bytes <- serialize(historical_summary,NULL,version=2)
historical_class <- class(historical_summary)
historical_rng <- .Random.seed
historical_print <- with_report_traps({
  lines <- capture.output(returned <- withVisible(print(historical_summary)))
  list(lines=lines,returned=returned)
})
ok(any(grepl('additional model/data context was not retained',historical_print$lines,fixed=TRUE)) &&
  any(grepl('Maximum recorded Rhat : Inf',historical_print$lines,fixed=TRUE)) &&
  identical(historical_print$returned$value,historical_summary) && !historical_print$returned$visible &&
  identical(class(historical_summary),historical_class) &&
  identical(serialize(historical_summary,NULL,version=2),historical_bytes) &&
  identical(.Random.seed,historical_rng),
  'public printing accepts the pre-report summary shape without mutating classes bytes or RNG')

# The default fit, summary and checks printers must retain the observed warning
# count and its limited scope. A missing count must not be inferred from messages.
unknown_count <- fit
unknown_count$cache$numerical_evaluations$observed_failure_messages <- NULL
unknown_bytes <- serialize(unknown_count,NULL,version=2)
warning_rng <- .Random.seed
unknown_report <- with_report_traps(read_report(unknown_count))
printed_fields <- c('printed','summary_printed','checks_printed')
known_line <- 'Recognized cached numerical-message lines: 2 | retained'
unknown_line <- 'Recognized cached numerical-message lines: unavailable | unavailable_in_cache'
ok(all(vapply(report[printed_fields],function(lines)any(grepl(known_line,lines,fixed=TRUE)),logical(1))) &&
  any(grepl(known_line,historical_print$lines,fixed=TRUE)) &&
  all(vapply(unknown_report[printed_fields],function(lines)any(grepl(unknown_line,lines,fixed=TRUE)),logical(1))) &&
  identical(serialize(unknown_count,NULL,version=2),unknown_bytes) &&
  identical(.Random.seed,warning_rng),
  'public printers preserve recognized cached line counts and leave absent counts unknown')
ok(all(vapply(report[printed_fields],function(lines)any(grepl(
  'Numerical evaluation evidence: incomplete | total failed evaluations: unknown',lines,fixed=TRUE)),logical(1))),
  'recognized message counts never become a total failed-evaluation count')

# Focused optional-context regressions from historical-cache review. Invalid
# metadata affects its projection only; usable diagnostic tables stay readable.
optional_context_read <- function(object) {
  bytes <- serialize(object,NULL,version=2); rng <- .Random.seed
  out <- with_report_traps(read_report(object))
  values <- function(x)lapply(x$sampling$extrema,function(metric)metric[c('value','coverage')])
  list(report=out,stable=identical(serialize(object,NULL,version=2),bytes) &&
    identical(.Random.seed,rng) && identical(out$parameters,object$cache$parameter_summary) &&
    identical(out$posterior,object$cache$parameter_summary) &&
    identical(out$checks$sampling$parameters,object$cache$parameter_summary) &&
    identical(values(out$checks),values(checks)))
}

# Optional annotations cannot invalidate the actual cached quantile columns.
# These four bounded reproductions also enter the detached cold-reader fixture.
metadata_cases <- list(numeric=42,
  missing_column=list(summary_method=list(method='retained_method',version='old'),
    interval_columns=c('q10','not_a_saved_column')),
  matrix=list(interval_columns=matrix(c('q10','q90'),ncol=1L)),
  list=list(interval_columns=list('q10','q90')))
metadata_fits <- list()
for(name in names(metadata_cases)) {
  malformed_metadata <- fit
  malformed_metadata$cache$report_metadata <- metadata_cases[[name]]
  malformed_read <- optional_context_read(malformed_metadata)
  sampling <- malformed_read$report$checks$sampling
  provenance_ok <- if(name=='numeric')
    identical(sampling$summary_provenance$availability,'unavailable_invalid_metadata') else
    if(name=='missing_column') identical(sampling$summary_provenance$record,
      metadata_cases[[name]]$summary_method) else TRUE
  ok(malformed_read$stable && provenance_ok &&
    sampling$interval_provenance$availability=='unavailable_invalid_metadata' &&
    identical(sampling$interval_columns,c('q10','q90')) &&
    is.character(sampling$interval_provenance$reason) && nzchar(sampling$interval_provenance$reason),
    paste(name,'optional report annotations stay invalid while cached quantiles and metrics remain intact'))
  metadata_fits[[paste0('metadata_',name)]] <- malformed_metadata
}

invalid_units <- fit
invalid_units$spec$models$scores$units <- 1
invalid_units$spec$tables$information <- as.list(invalid_units$spec$tables$information)
invalid_units_read <- optional_context_read(invalid_units)
invalid_context <- invalid_units_read$report$checks$context
unit_rows <- invalid_context$block_availability$field=='response_units' &
  invalid_context$block_availability$block=='scores'
ok(invalid_units_read$stable &&
  is.na(invalid_context$blocks$response_units[invalid_context$blocks$block=='scores']) &&
  invalid_context$blocks$response_units[invalid_context$blocks$block=='counts']=='successes' &&
  all(invalid_context$block_availability$availability[unit_rows]=='unavailable_in_cache') &&
  all(grepl('invalid saved metadata',invalid_context$block_availability$reason[unit_rows],fixed=TRUE)) &&
  invalid_context$information$availability=='unavailable_in_cache' &&
  all(is.na(invalid_context$blocks$informative_rows)) &&
  invalid_context$consumed_rows$availability=='retained' && invalid_context$consumed_rows$record==8L &&
  all(grepl('invalid saved metadata',invalid_context$issues$reason,fixed=TRUE)),
  'numeric units and a malformed information table make only affected context unavailable')

integer_flags <- fit
integer_flags$spec$tables$information$informative <- as.integer(integer_flags$spec$tables$information$informative)
integer_flags$spec$tables$person_roster$observed <- as.integer(integer_flags$spec$tables$person_roster$observed)
integer_flags_read <- optional_context_read(integer_flags)
invalid_context <- integer_flags_read$report$checks$context
roster_fields <- invalid_context$subjects$fields[c('observed_roster_people','unobserved_roster_people')]
ok(integer_flags_read$stable && all(is.na(invalid_context$blocks$informative_rows)) &&
  all(vapply(roster_fields,function(field)identical(field$availability,'unavailable_in_cache') &&
    grepl('invalid saved metadata',field$reason,fixed=TRUE),logical(1))) &&
  invalid_context$subjects$record$joint_subject_count==4L &&
  invalid_context$subjects$record$roster_count==4L && invalid_context$consumed_rows$record==8L &&
  any(grepl('information',invalid_context$issues$source,fixed=TRUE)) &&
  any(grepl('person_roster',invalid_context$issues$source,fixed=TRUE)),
  'integer information and roster flags are not coerced into trustworthy context counts')

# Exact optional-shape regressions: a matrix is not one plain flag per row,
# even when its first column could produce plausible counts after indexing.
matrix_information <- fit
flag <- matrix_information$spec$tables$information$informative
matrix_information$spec$tables$information$informative <- cbind(first=flag,second=!flag)
matrix_information_read <- optional_context_read(matrix_information)
invalid_context <- matrix_information_read$report$checks$context
ok(matrix_information_read$stable && all(is.na(invalid_context$blocks$informative_rows)) &&
  invalid_context$consumed_rows$record==8L &&
  identical(invalid_context$information$record,matrix_information$spec$tables$information) &&
  any(grepl('information',invalid_context$issues$source,fixed=TRUE)) &&
  any(grepl('one plain logical value per saved row',invalid_context$issues$reason,fixed=TRUE)),
  'matrix information flags leave informative counts unavailable without flattening saved rows')

matrix_roster <- fit
flag <- matrix_roster$spec$tables$person_roster$observed
matrix_roster$spec$tables$person_roster$observed <- cbind(first=flag,second=!flag)
matrix_roster_read <- optional_context_read(matrix_roster)
invalid_context <- matrix_roster_read$report$checks$context
roster_fields <- invalid_context$subjects$fields[c('observed_roster_people','unobserved_roster_people')]
ok(matrix_roster_read$stable &&
  all(vapply(roster_fields,function(field)identical(field$availability,'unavailable_in_cache') &&
    grepl('invalid saved metadata',field$reason,fixed=TRUE),logical(1))) &&
  invalid_context$subjects$record$roster_count==4L &&
  invalid_context$subjects$record$observed_people==4L &&
  identical(invalid_context$subjects$record$person_roster,matrix_roster$spec$tables$person_roster) &&
  any(grepl('person_roster',invalid_context$issues$source,fixed=TRUE)),
  'matrix roster flags cannot create observed or unobserved roster counts')

matrix_support <- fit
flag <- matrix_support$spec$tables$task_target_domain$has_symbolic_support
matrix_support$spec$tables$task_target_domain$has_symbolic_support <- cbind(first=flag,second=!flag)
matrix_support_read <- optional_context_read(matrix_support)
invalid_context <- matrix_support_read$report$checks$context
ok(matrix_support_read$stable && all(is.na(invalid_context$blocks$active_person_effect_targets)) &&
  identical(as.integer(invalid_context$blocks$declared_subject_task_targets),c(3L,2L)) &&
  identical(invalid_context$task_target_domain$record,matrix_support$spec$tables$task_target_domain) &&
  any(grepl('task_target_domain',invalid_context$issues$source,fixed=TRUE)) &&
  any(grepl('unavailable symbolically active person-effect targets',
    matrix_support_read$report$summary_printed,fixed=TRUE)),
  'matrix symbolic-support flags are rejected before target indexing can drop dimensions')

matrix_units <- fit
matrix_units$spec$models$scores$units <- matrix('points',nrow=1L,ncol=1L)
matrix_units_read <- optional_context_read(matrix_units)
invalid_context <- matrix_units_read$report$checks$context
unit_rows <- invalid_context$block_availability$field=='response_units' &
  invalid_context$block_availability$block=='scores'
ok(matrix_units_read$stable &&
  is.na(invalid_context$blocks$response_units[invalid_context$blocks$block=='scores']) &&
  invalid_context$blocks$response_units[invalid_context$blocks$block=='counts']=='successes' &&
  all(invalid_context$block_availability$availability[unit_rows]=='unavailable_in_cache') &&
  all(grepl('invalid saved metadata',invalid_context$block_availability$reason[unit_rows],fixed=TRUE)) &&
  invalid_context$consumed_rows$record==8L,
  'a one-by-one character units matrix is not silently coerced to a scalar string')
matrix_context_fits <- list(matrix_information=matrix_information,matrix_roster=matrix_roster,
  matrix_support=matrix_support,matrix_units=matrix_units)

for(key_shape in c('matrix','list')) {
  invalid_keys <- fit
  invalid_keys$cache$parameter_summary$parameter_key <- if(key_shape=='matrix')
    I(matrix(parameters$parameter_key,ncol=1L)) else I(as.list(parameters$parameter_key))
  invalid_keys_read <- optional_context_read(invalid_keys)
  invalid_context <- invalid_keys_read$report$checks$context
  ok(invalid_keys_read$stable && invalid_context$membership$availability=='unavailable_in_cache' &&
    grepl('invalid saved metadata',invalid_context$membership$reason,fixed=TRUE) &&
    any(grepl('membership',invalid_context$issues$source,fixed=TRUE)) &&
    all(is.na(invalid_keys_read$report$checks$sampling$extrema$rhat$rows$parameter_key)),
    paste(key_shape,'parameter keys leave membership unavailable while preserving raw tables and metrics'))
}

missing_context <- fit; missing_context$spec <- NULL
missing_context_read <- optional_context_read(missing_context)
missing_report <- missing_context_read$report
ok(missing_context_read$stable &&
  missing_report$checks$context$subjects$availability=='unavailable_in_cache' &&
  missing_report$checks$context$consumed_rows$availability=='unavailable_in_cache' &&
  missing_report$checks$context$person_law$availability=='unavailable_in_cache' &&
  identical(class(missing_report$summary),class(report$summary)) &&
  identical(names(missing_report$summary),names(report$summary)) &&
  all(vapply(missing_report[c('printed','summary_printed')],function(lines)
    grepl('unavailable subject-domain IDs | unavailable likelihood rows',lines[1L],fixed=TRUE) &&
    any(grepl('person law: unavailable in cache',lines,fixed=TRUE)) &&
    !any(grepl('0 people|0 subject-domain IDs|0 likelihood rows|person law: iid',lines)),logical(1))),
  'missing historical fit context prints unknown people rows and person law without changing summary fields')
ok(report$checks$context$consumed_rows$availability=='retained' &&
  report$checks$context$consumed_rows$record==8L &&
  all(vapply(report[c('printed','summary_printed')],function(lines)
    grepl('4 subject-domain IDs | 8 likelihood rows',lines[1L],fixed=TRUE) &&
    any(grepl('person law: iid',lines,fixed=TRUE)),logical(1))),
  'complete saved context still prints its known subject row and person-law values')

# Synthetic reporting-only shape: an explicitly retained empty domain is
# evidence of zero counts; deleting its blocks record makes row coverage unknown.
empty_context <- fit
empty_context$spec <- list(models=list(),data_map=list(blocks=list(),subject_keys=character()),
  tables=list(person_roster=mixed$tables$person_roster[FALSE,,drop=FALSE]))
empty_context_read <- optional_context_read(empty_context)
absent_blocks <- empty_context; absent_blocks$spec$data_map$blocks <- NULL
absent_blocks_read <- optional_context_read(absent_blocks)
empty_saved <- empty_context_read$report$checks$context
absent_saved <- absent_blocks_read$report$checks$context
ok(empty_context_read$stable && absent_blocks_read$stable &&
  empty_saved$consumed_rows$availability=='retained' && empty_saved$consumed_rows$record==0L &&
  empty_saved$subjects$fields$observed_people$availability=='retained' &&
  empty_saved$subjects$record$observed_people==0L &&
  empty_saved$subjects$record$joint_subject_count==0L && empty_saved$subjects$record$roster_count==0L &&
  absent_saved$consumed_rows$availability=='unavailable_in_cache' &&
  absent_saved$subjects$fields$observed_people$availability=='unavailable_in_cache',
  'explicitly saved empty blocks and roster establish zeros while absent blocks leave counts unavailable')

# Historical cache boundaries are small projections, never repaired cache writes.
one <- fit; one$run$chains <- 1L; one$cache$parameter_summary$rhat <- rep(1.7,7L)
one_report <- with_report_traps(public('fit_checks')(one))
ok(one_report$sampling$extrema$rhat$value==1.7,
  'configured one-chain metadata does not erase an actually retained Rhat')
one$cache$parameter_summary$rhat[] <- NA_real_
one$cache$parameter_summary$ess_bulk[] <- NaN
one$cache$parameter_summary$ess_tail[] <- NA_real_
one_report <- with_report_traps(public('fit_checks')(one))
ok(is.na(one_report$sampling$extrema$rhat$value) &&
  nrow(one_report$sampling$extrema$rhat$rows)==0L &&
  one_report$sampling$extrema$rhat$status!='computed',
  'one-chain all-NA Rhat has no accidental infinity or favorable computed status')
ok(is.na(one_report$sampling$extrema$ess_bulk$value) &&
  one_report$sampling$extrema$ess_bulk$coverage$nan==7L &&
  is.na(one_report$sampling$extrema$ess_tail$value),
  'all-NaN and all-NA ESS remain different unavailable extrema')
empty <- fit; empty$cache$parameter_summary <- parameters[FALSE,,drop=FALSE]
empty_report <- with_report_traps(public('fit_checks')(empty))
ok(all(vapply(empty_report$sampling$extrema,function(x)is.na(x$value)&&nrow(x$rows)==0L,logical(1))),
  'an empty monitored set has no accidental extreme')
absent <- fit; absent$cache <- list(); absent$run <- list(); absent$provenance <- list()
absent_report <- with_report_traps(read_report(absent))
ok(is.null(absent_report$parameters) && absent_report$checks$sampling$chains$availability=='unavailable_in_cache',
  'missing historical parameter and HMC caches remain unavailable')
ok(absent_report$checks$domains$identification$availability=='unavailable_in_cache' &&
  absent_report$checks$predictive$assessment$availability=='unavailable_in_cache',
  'missing historical evidence is not inferred to have never been attempted')
partial <- fit; partial$cache$parameter_summary$rhat <- NULL
partial$cache$hmc <- list(num_divergent=NA_real_,ebfmi=c(NA_real_,NaN))
partial_report <- with_report_traps(public('fit_checks')(partial))
ok(partial_report$sampling$extrema$rhat$availability=='unavailable_in_cache' &&
  is.na(partial_report$sampling$chains$metrics$ebfmi$minimum$value) &&
  is.na(partial_report$sampling$chains$metrics$num_divergent$observed_total) &&
  is.na(partial_report$sampling$chains$metrics$num_max_treedepth$observed_total),
  'absent metrics and all-unusable chain summaries are explicit')
failed <- absent; failed$cache$computation <- list(status='failed',reason='retained collection failure; operation not recorded')
failed_report <- with_report_traps(public('fit_checks')(failed))
ok(identical(failed_report$domains$computation$record,failed$cache$computation),
  'recorded collection failure is preserved without inventing which operation failed')
for(name in c('fit_checks','parameter_summary')) {
  e <- with_report_traps(err(public(name)(mixed),'E_FIT',paste(name,'rejects a prepared context')))
  ok(grepl('check_model',conditionMessage(e),fixed=TRUE),'prepared-context error directs readers to check_model')
}
bad <- fit; bad$cache$parameter_summary <- list(rhat=1)
invisible(with_report_traps(err(public('fit_checks')(bad),'E_FIT_REPORT','malformed required table fails with typed reporting error')))

# Existing scientific coordinates are copied from authoritative saved tables.
centered_fixture <- function(spec) {
  map <- private('.bh_reference_summary_map')(spec)
  selected <- match(unique(map$parameter_role),map$parameter_role)
  tab <- map[selected,c('variable','parameter_key','parameter_role'),drop=FALSE]
  for(field in c('target','person_id','basis_index'))
    tab[[paste0('reference_',field)]] <- map[[field]][selected]
  tab$mean <- 0; tab$sd <- 1; tab$q10 <- -1; tab$q90 <- 1
  tab$mcse_mean <- .1; tab$rhat <- 1.2; tab$ess_bulk <- 5; tab$ess_tail <- 4
  out <- make_fit(spec,tab)
  out$cache$reference_law <- private('.bh_reference_cache_metadata')(spec,map)
  if(!nrow(spec$tables$raw_correlation_coordinates)) {
    correlation_map <- private('.bh_affine_summary_map')(spec)
    out$cache$correlation_constraints <- private('.bh_affine_cache_metadata')(spec,correlation_map)
  }
  out
}
centered_fits <- list(raw=centered_fixture(centered_raw),final=centered_fixture(centered_final))
for(name in names(centered_fits)) {
  current <- centered_fits[[name]]; bytes <- serialize(current,NULL,version=2)
  out <- with_report_traps(read_report(current)); context <- out$checks$context
  ok(context$subjects$record$roster_count==5L && context$subjects$record$observed_roster_people==4L &&
    context$subjects$record$unobserved_roster_people==1L,paste(name,'complete centered roster keeps the unseen member'))
  ok(context$blocks$declared_subject_task_targets==3L && context$blocks$active_person_effect_targets==2L,
    paste(name,'structural-zero target stays declared without person variation'))
  ok(identical(context$person_law$record,current$spec$tables$person_law) &&
    identical(context$augmentation$record,current$spec$tables$augmentation) &&
    identical(context$correlation_coordinates$record,current$spec$tables$correlation_coordinate_system),
    paste(name,'person law white coordinates and correlation space retain exact meaning'))
  ok(identical(bytes,serialize(current,NULL,version=2)),paste(name,'report does not reseal historical context'))
  ok(identical(context$estimands$record$reference_law$record,current$cache$reference_law),
    paste(name,'retains raw/final priors and white-axis versus person-score meanings'))
}

# Reuse the pinned public M3 row contract when its separately licensed source is
# supplied. Preparation is R-only; every read below traps its validation callback.
m3_resource <- Sys.getenv('MEASUREMENTPKG_M3_RESOURCE')
if(nzchar(m3_resource)&&file.exists(m3_resource)) {
  joint_data <- data.frame(person=as.character(1:4),task='CS',obs=paste0('joint',1:4))
  joint_data$counts <- I(matrix(0L,4L,10L)); joint_data$counts[1L,1L] <- 2L
  joint_data$counts[3L,1L] <- NA_integer_; joint_data$counts[4L,] <- NA_integer_
  joint_data$options <- I(matrix(1L,4L,10L))
  joint <- observe(m3_cs_model(m3_resource),joint_data,'person','task',
    fields=list(counts='counts',options='options'),units='count',observation_id='obs')
  cognitive <- bmeas(bmf(residual=list(targets=~memory$a['CS']+memory$c['CS']+
    memory$ra['CS']+memory$rc['CS'])),list(memory=joint),missing='observed_only',fit=FALSE)
  tab <- parameters[1L,,drop=FALSE]
  tab$variable <- 'alpha[1]'; tab$parameter_key <- paste0('Intercept:',cognitive$tables$targets$key[1L])
  tab$parameter_role <- 'Intercept'; tab$loading_aliases <- NA_character_
  cognitive_fit <- make_fit(cognitive,tab)
  cognitive_report <- with_report_traps(read_report(cognitive_fit))
  block <- cognitive_report$checks$context$blocks
  ok(block$consumed_rows==2L && block$informative_rows==1L && block$excluded_rows==2L,
    'joint cognitive responses count likelihood rows and retain zero-total uninformative rows')
  ok(identical(cognitive_report$checks$context$partial_response_exclusions$record,
    cognitive$tables$partial_response_exclusions),
    'partial joint responses retain the saved whole-row exclusion policy')
} else cat('SKIP pinned cognitive row check: MEASUREMENTPKG_M3_RESOURCE not provided\n')

# Ordinary exported helpers are namespace-qualified in installed compatibility
# checks. Only bmeas-leading current objects claim S3 ownership across load order.
legacy_ppc <- list(table=data.frame(statistic_id='old_stat',observed=2),metadata=list(replications=4L))
legacy <- structure(list(cache=list(parameter_summary=parameters,ppc=legacy_ppc)),class='measurement_fit')
expected_ppc <- legacy_ppc; expected_ppc$metadata$replicate_statistics_status <- 'unavailable_legacy_run'
legacy_bytes <- serialize(legacy,NULL,version=2)
ok(identical(public('fit_checks')(legacy),expected_ppc) &&
  identical(public('parameter_summary')(legacy),parameters) &&
  identical(public('posterior_summary')(legacy),parameters), 'measurement_fit preserves historical PPC and parameter helper outputs')
legacy$cache$ppc <- NULL
ok(is.null(public('fit_checks')(legacy)), 'measurement_fit without PPC still returns NULL')
legacy$cache$ppc <- legacy_ppc
ok(identical(legacy_bytes,serialize(legacy,NULL,version=2)), 'legacy report inspection preserves the object')
compat <- fit; class(compat) <- 'bhmm_fit'
ok(identical(with_report_traps(public('fit_checks')(compat)),checks) &&
  identical(with_report_traps(public('parameter_summary')(compat)),parameters),
  'compatible bhmm_fit objects receive the same explicit exported-helper projection')
for(name in c('correlation_draws','individual_parameter_summary','ppc_plan','assess_measurement_fit')) {
  error <- with_report_traps(tryCatch(public(name)(fit),error=identity))
  ok(inherits(error,'measurement_error'),paste(name,'does not perform draw or simulation work without its own explicit request'))
}

# A separate vanilla process loads detached fixtures before installing the same
# terminal routes. File use here is test harness transport, never a report read.
local({
  directory <- tempfile('cached-fit-report-'); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  fixture_path <- file.path(directory,'fixtures.rds')
  result_path <- file.path(directory,'result.rds')
  runner <- file.path(directory,'cold.R')
  fixtures <- c(list(mixed=fit,missing=absent),centered_fits,metadata_fits,matrix_context_fits)
  for(i in seq_along(fixtures)) fixtures[[i]]$backend <- NULL
  expected <- lapply(fixtures,function(x)with_report_traps(read_report(x)))
  saveRDS(list(fixtures=fixtures,expected=expected),fixture_path,version=2)
  setup <- if(source_mode) c(paste0('source_root <- ',deparse(source_root)),
    'source_mode <- TRUE',"for(file in list.files(file.path(source_root,'R'),full.names=TRUE)) source(file)") else
    c(paste0('.libPaths(',paste(deparse(.libPaths()),collapse=''),')'),'source_mode <- FALSE','library(bmeas)')
  functions <- unlist(lapply(c('private','public','report_terminal','with_report_traps','read_report'),function(name)
    c(paste0(name,' <- '),deparse(get(name,.GlobalEnv)))),use.names=FALSE)
  writeLines(c(setup,functions,
    'report_terminal_calls <- character()',
    'arguments <- commandArgs(TRUE); saved <- readRDS(arguments[1L])',
    'input_hash <- unname(tools::md5sum(arguments[1L]))',
    'before <- lapply(saved$fixtures,serialize,connection=NULL,version=2)',
    'set.seed(2506); rng <- .Random.seed',
    'actual <- lapply(saved$fixtures,function(x)with_report_traps(read_report(x)))',
    'stopifnot(identical(actual,saved$expected),identical(rng,.Random.seed))',
    'stopifnot(identical(lapply(saved$fixtures,serialize,connection=NULL,version=2),before))',
    'stopifnot(identical(unname(tools::md5sum(arguments[1L])),input_hash))',
    'saveRDS(TRUE,arguments[2L],version=2)'),runner)
  output <- system2(file.path(R.home('bin'),'Rscript'),
    c('--vanilla',shQuote(runner),shQuote(fixture_path),shQuote(result_path)),stdout=TRUE,stderr=TRUE)
  status <- attr(output,'status')
  if(!is.null(status)&&status!=0L) stop(paste(output,collapse='\n'))
  ok(file.exists(result_path)&&isTRUE(readRDS(result_path)),
    'cold detached reads match warm values and preserve serialized bytes and RNG')
})

# Archived-package load-order compatibility is reviewed separately; installing
# bmeas does not require the historical bhmm package.
cat('Frontend cached fit checks:',passed,'passed; no backend, draws, compilation, sampling, PPC or LOO executed.\n')
