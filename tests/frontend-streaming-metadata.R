source(file.path("helpers", "native-guard.R"))
# Handwritten passive engineering records, not fitted posterior evidence.
# These controls perform no source acquisition, fitting or simulation.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,"R"),full.names=TRUE))source(f)else library(bmeas)
private <- function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
passed <- 0L
ok <- function(value,label) {if(!isTRUE(value))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
local({
  env <- if(source_mode).GlobalEnv else asNamespace("bmeas")
  targets <- c(".bh_posterior_ingest_csv",".bh_ppc_ingest_csv",".bh_individual_ingest_csv",
    ".bh_ppc_validate_saved_spec",".bh_ppc_simulate_responses")
  old <- lapply(targets,function(name)list(value=get(name,env),locked=bindingIsLocked(name,env)))
  names(old)<-targets
  on.exit(for(name in targets){assign(name,old[[name]]$value,env);if(old[[name]]$locked)lockBinding(name,env)},add=TRUE)
  calls<-0L;trap<-function(...){calls<<-calls+1L;stop("passive terminal forbidden")}
  for(name in targets){if(old[[name]]$locked)unlockBinding(name,env);assign(name,trap,env)}
  set.seed(746);rng<-.Random.seed;cwd<-getwd()
  allocation<-data.frame(chain_id=c(2L,5L,9L,12L),source_rows=rep(32L,4),
    selected_rows=c(2L,2L,2L,1L),omitted_rows=c(30L,30L,30L,31L))
  keys<-data.frame(chain_id=c(2L,5L,9L,12L,2L,5L,9L),saved_row=c(1L,1L,1L,16L,32L,32L,32L))
  receipt<-list(version=1L,status="acquired",reader=list(package="bmeas",function_name="private_streaming_csv"),
    source_rows=128L,selected_rows=7L,selected_required_values=21L,
    selection=list(method="balanced",allocations=allocation))
  ppc<-list(metadata=list(level="population",law="iid_common_plus_parameter_residual",
    requested_replications=7L,completed_replications=7L,source_qualification="handwritten passive engineering record",
    posterior_selection=receipt$selection,source_receipt=receipt,draw_keys=keys),
    table=data.frame(statistic_id="mean",domain="marginal",level="population",unit="response",
      observed=0,replicated_median=0,lower90=-1,upper90=1,finite_replications=7L,
      requested_replications=7L,outside_interval=FALSE))
  backend_trap<-function(...)stop("handwritten passive backend forbidden")
  environment(backend_trap)<-baseenv()
  fit<-structure(list(cache=list(ppc=ppc,predictive_fit=list(status="computed",level="population"),
    computation=list(status="warning",reason="handwritten existing evidence"),
    inference=list(status="not_assessed",reason="engineering only"),
    parameter_summary=data.frame(variable="alpha[1]",mean=0,sd=1,rhat=1.2,ess_bulk=10,ess_tail=9)),
    provenance=list(kind="handwritten_passive_engineering_record"),
    backend=list(draws=backend_trap,metadata=backend_trap)),class=c("bmeas_fit","bhmm_fit"))
  baseline<-fit_checks(fit)
  ok(identical(baseline$predictive$cached$posterior_source$availability,"retained"),"valid balanced passive source retained")
  ok(length(capture.output(print(summary(fit))))>0,"valid passive public summary prints")
  malformed<-list()
  z<-ppc;z$metadata["source_receipt"]<-alist(value=);malformed$missing_receipt<-z
  z<-ppc;z$metadata["posterior_selection"]<-alist(value=);malformed$missing_selection<-z
  z<-ppc;z$metadata["draw_keys"]<-alist(value=);malformed$missing_keys<-z
  z<-ppc;attr(z$metadata$source_receipt$selection$allocations,"row.names")<-seq_len(5L)
  z$metadata$posterior_selection<-z$metadata$source_receipt$selection;malformed$forged_allocation_rows<-z
  z<-ppc;z$metadata$requested_replications<-8L;malformed$request_count<-z
  z<-ppc;z$metadata$completed_replications<-8L;malformed$completed_count<-z
  z<-ppc;z$table$requested_replications<-8L;malformed$table_count<-z
  z<-ppc;z$metadata$source_receipt$selected_required_values<-20L;malformed$fractional_width<-z
  z<-ppc;z$metadata$plan<-list(required_columns=4L);malformed$planned_width<-z
  z<-ppc;names(z$metadata$source_receipt)[1L]<-"version_unrelated";malformed$receipt_prefix<-z
  z<-ppc;names(z$metadata$source_receipt$reader)[2L]<-"function_name_unrelated";malformed$reader_prefix<-z
  z<-ppc;names(z$metadata$source_receipt$selection)[1L]<-"method_unrelated"
  z$metadata$posterior_selection<-z$metadata$source_receipt$selection;malformed$method_prefix<-z
  z<-ppc;names(z$metadata$source_receipt$selection)[2L]<-"allocations_unrelated"
  z$metadata$posterior_selection<-z$metadata$source_receipt$selection;malformed$allocation_prefix<-z
  for(label in names(malformed)) {
    value<-fit;value$cache$ppc<-malformed[[label]];bytes<-serialize(value,NULL)
    r<-fit_checks(value)
    ok(identical(r$predictive$cached$posterior_source$availability,"unavailable_invalid_metadata")&&
      identical(r$predictive$cached$table$record,malformed[[label]]$table)&&
      identical(r$sampling,baseline$sampling)&&identical(r$domains,baseline$domains),paste("local public source refusal",label))
    ok(length(capture.output(print(summary(value))))>0&&identical(serialize(value,NULL),bytes),paste("passive print and input bytes",label))
  }
  print_ppc<-private(".bh_print_report_ppc")
  projected<-baseline$predictive$cached
  callback_hits<-0L;active<-new.env(parent=emptyenv())
  makeActiveBinding("source_rows",function(){callback_hits<<-callback_hits+1L;128L},active)
  active$selected_rows<-7L
  bad_reports<-list()
  z<-projected;z$posterior_source$record<-active;bad_reports$active_record<-z
  z<-projected;z$posterior_source<-active;bad_reports$active_node<-z
  z<-projected;z$posterior_source$record$source_rows<-127L;bad_reports$changed_display_count<-z
  z<-projected;z["posterior_source"]<-alist(value=);bad_reports$missing_display_node<-z
  z<-projected;z$posterior_source$record$selected_required_values<-20L
  z$metadata$record$source_receipt<-z$posterior_source$record;bad_reports$changed_both_receipts<-z
  z<-projected;z$replications$requested$record<-8L
  z$replications$completed$record<-8L;bad_reports$changed_displayed_counts<-z
  for(label in names(bad_reports)) {
    text<-capture.output(print_ppc(bad_reports[[label]]))
    ok(any(grepl("Streaming source selection: unavailable_invalid_metadata",text,fixed=TRUE))&&
      any(grepl("Cached discrepancies:",text,fixed=TRUE))&&callback_hits==0L,
      paste("direct saved-report source display remains local",label))
  }
  zero<-fit;zero$cache$ppc$metadata$source_receipt$selected_required_values<-0L
  zero$cache$ppc$metadata$plan<-list(required_columns=0L)
  ok(identical(fit_checks(zero)$predictive$cached$posterior_source$availability,"retained"),"zero required columns preserve validated state identities")
  expected<-private(".bh_posterior_expected_chains")
  ordinary<-list(run=list(chains=4L),execution_receipt=list(version="bmeas_execution_receipt_v1",sampling=list(configured_chains=4L)))
  normal<-expected(ordinary)
  ok(identical(normal$value,4)&&length(normal$source)==2L,"both exact expected-chain sources reconcile")
  substitutions<-list(run=list(run_unrelated=list(chains=7L)),
    chains=list(run=list(chains_unrelated=7L)),
    receipt=list(execution_receipt_unrelated=list(version="bmeas_execution_receipt_v1",sampling=list(configured_chains=7L))),
    version=list(execution_receipt=list(version_unrelated="bmeas_execution_receipt_v1",sampling=list(configured_chains=7L))),
    sampling=list(execution_receipt=list(version="bmeas_execution_receipt_v1",sampling_unrelated=list(configured_chains=7L))),
    configured=list(execution_receipt=list(version="bmeas_execution_receipt_v1",sampling=list(configured_chains_unrelated=7L))))
  for(label in names(substitutions)) {
    x<-expected(substitutions[[label]])
    ok(is.null(x$value)&&identical(x$availability,"unavailable_in_saved_context"),paste("prefix field does not invent chains",label))
  }
  missing<-ordinary;missing$run["chains"]<-alist(value=)
  e<-tryCatch(expected(missing),error=identity)
  ok(inherits(e,"measurement_error")&&identical(e$code,"E_PPC_SOURCE_IDENTITY"),"missing exact chain value refuses with typed identity error")
  ok(calls==0L&&identical(rng,.Random.seed)&&identical(cwd,getwd()),"all passive/context guards preserve RNG/cwd with zero terminals")
})

local({
  error_code<-function(expr) {e<-tryCatch(force(expr),error=identity);if(inherits(e,"measurement_error"))e$code else NULL}
  expect_bad<-function(x,label)ok(identical(error_code(invisible(capture.output(print(x)))),"E_INDIVIDUAL_RESULT"),label)
  env<-if(source_mode).GlobalEnv else asNamespace("bmeas")
  trap_names<-c(".bh_posterior_ingest_csv",".bh_individual_ingest_csv",".bh_posterior_policy",
    ".bh_posterior_consumer_policy",".bh_ppc_validate_saved_spec",".bh_individual_parameter_summary",
    ".bh_individual_plan",".bh_individual_summarize")
  saved<-lapply(trap_names,function(name) {
    present<-exists(name,env,inherits=FALSE)
    list(present=present,value=if(present)get(name,env),locked=present&&bindingIsLocked(name,env))
  });names(saved)<-trap_names
  calls<-0L
  for(name in trap_names) {
    if(saved[[name]]$locked)unlockBinding(name,env)
    assign(name,function(...) {calls<<-calls+1L;stop("forbidden active work from passive metadata")},env)
  }
  on.exit(for(name in trap_names) {
    if(saved[[name]]$present)assign(name,saved[[name]]$value,env)else rm(list=name,envir=env)
    if(saved[[name]]$locked)lockBinding(name,env)
  },add=TRUE)
  old_warn<-getOption("warn");options(warn=2);on.exit(options(warn=old_warn),add=TRUE)
  keys<-data.frame(chain_id=c(3L,7L),saved_row=c(2L,2L))
  selection<-list(version="bmeas_individual_selection_v1",method="balanced",
    requested_states=list(method="balanced",draws=2L),full_source_rows=8L,selected_rows=2L,omitted_rows=6L,
    per_chain=data.frame(chain_id=c(3L,7L),source_rows=c(4L,4L),selected_rows=c(1L,1L),omitted_rows=c(3L,3L)))
  table<-data.frame(person_key=c("one","two"),parameter_key="handwritten",scale="natural",
    mean=c(3,4),median=c(3,4),sd=c(1,1),lower90=c(2,3),upper90=c(4,5),status="available",n_draws=2L)
  attr(table,"metadata")<-list(version="bmeas_individual_parameter_summary_v2",status="computed",
    requested_scales="natural",selection=selection)
  attr(table,"work")<-list(logical_parameter_cells=2L,retained_source_rows=8L,selected_source_rows=2L)
  attr(table,"draw_keys")<-keys
  class(table)<-c("bmeas_individual_parameter_summary","data.frame")
  checked<-private(".bh_individual_result_view")(table)
  ok(isTRUE(checked$selection_request_executed),"valid balanced request is fulfilled by its exact selected count")
  ok(length(capture.output(print(table)))>0,"valid handwritten v2 result remains printable")
  for(field in names(selection)) {
    bad<-table;s<-selection;names(s)[names(s)==field]<-paste0(field,"_unrelated")
    attr(bad,"metadata")$selection<-s
    expect_bad(bad,paste("selection requires exact field",field))
    s<-selection;s[field]<-alist(value=);attr(bad,"metadata")$selection<-s
    expect_bad(bad,paste("selection missing sentinel refuses for",field))
  }
  requests<-list(environment=new.env(parent=emptyenv()),contradictory_count=list(method="balanced",draws=7L),
    all_string="all",wrong_method=list(method="other",draws=2L),
    extra_field=list(method="balanced",draws=2L,extra=TRUE),
    method_prefix=list(method_unrelated="balanced",draws=2L),draws_prefix=list(method="balanced",draws_unrelated=2L),
    missing_count=list(method="balanced",draws=NA_real_),infinite_count=list(method="balanced",draws=Inf),
    one_state=list(method="balanced",draws=1L),fractional_count=list(method="balanced",draws=2.5),
    classed_count=list(method="balanced",draws=structure(2,class="Date")),
    vector_count=list(method="balanced",draws=c(2,2)),dimensioned_count=list(method="balanced",draws=matrix(2)),
    unevaluated_expression=list(method="balanced",draws=quote(stop("must not execute"))),
    classed_request=structure(list(method="balanced",draws=2L),class="tripwire"))
  for(label in names(requests)) {
    bad<-table;attr(bad,"metadata")$selection$requested_states<-requests[[label]]
    expect_bad(bad,paste("saved request refuses",label))
  }
  for(field in c("method","draws")) {
    bad<-table;request<-selection$requested_states;request[field]<-alist(value=)
    attr(bad,"metadata")$selection$requested_states<-request
    expect_bad(bad,paste("saved request missing sentinel refuses for",field))
  }
  all<-table;a<-selection;a$method<-"all";a$requested_states<-"all";a$selected_rows<-8L;a$omitted_rows<-0L
  a$per_chain$selected_rows<-c(4L,4L);a$per_chain$omitted_rows<-c(0L,0L)
  attr(all,"metadata")$selection<-a;attr(all,"work")$selected_source_rows<-8L;all$n_draws<-c(8L,8L)
  attr(all,"draw_keys")<-data.frame(chain_id=rep(c(3L,7L),4),saved_row=rep(1:4,each=2))
  ok(length(capture.output(print(all)))>0,"valid all-state request remains printable with exact full keys")
  one<-all;o<-a;o$full_source_rows<-o$selected_rows<-1L
  o$per_chain<-data.frame(chain_id=3L,source_rows=1L,selected_rows=1L,omitted_rows=0L)
  attr(one,"metadata")$selection<-o;one$n_draws[]<-1L
  attr(one,"work")$retained_source_rows<-attr(one,"work")$selected_source_rows<-1L
  attr(one,"draw_keys")<-data.frame(chain_id=3L,saved_row=1L)
  expect_bad(one,"nonempty all-state results require at least two selected states")
  bad<-all;attr(bad,"metadata")$selection$requested_states<-list(method="balanced",draws=8L)
  expect_bad(bad,"all-state method cannot substitute a balanced request")
  for(field in c("version","status","requested_scales","selection")) {
    bad<-table;m<-attr(bad,"metadata");names(m)[names(m)==field]<-paste0(field,"_unrelated");attr(bad,"metadata")<-m
    expect_bad(bad,paste("outer metadata requires exact field",field))
    m<-attr(table,"metadata");m[field]<-alist(value=);attr(bad,"metadata")<-m
    expect_bad(bad,paste("outer metadata missing sentinel refuses for",field))
  }
  for(field in c("logical_parameter_cells","retained_source_rows","selected_source_rows")) {
    bad<-table;w<-attr(bad,"work");names(w)[names(w)==field]<-paste0(field,"_unrelated");attr(bad,"work")<-w
    expect_bad(bad,paste("outer work requires exact field",field))
    w<-attr(table,"work");w[field]<-alist(value=);attr(bad,"work")<-w
    expect_bad(bad,paste("outer work missing sentinel refuses for",field))
  }
  optional<-table;attr(optional,"metadata")$unrelated<-new.env(parent=emptyenv())
  attr(optional,"metadata")$selection$unrelated<-new.env(parent=emptyenv())
  attr(optional,"metadata")$source_qualification_unrelated<-new.env(parent=emptyenv())
  attr(optional,"source_receipt")<-new.env(parent=emptyenv())
  ok(length(capture.output(print(optional)))>0,"unrelated optional annotations cannot replace or erase valid v2 metadata")
  legacy<-table;attr(legacy,"metadata")<-list(version="bmeas_individual_parameter_summary_v1",status="computed",
    requested_scales="natural",selection=new.env(parent=emptyenv()),
    version_unrelated="bmeas_individual_parameter_summary_v2")
  attr(legacy,"work")<-list(logical_parameter_cells=2L,retained_source_rows=2L)
  before<-serialize(legacy,NULL);printed<-capture.output(print(legacy))
  ok(!any(grepl("Selected empirical",printed,fixed=TRUE))&&identical(before,serialize(legacy,NULL)),
    "valid v1 behavior and bytes remain unchanged despite unrelated v2 annotations")
  empty<-structure(lapply(unclass(table),function(column)column[FALSE]),
    class=c("bmeas_individual_parameter_summary","data.frame"),row.names=integer())
  e<-selection;e$requested_states<-list(method="balanced",draws=7L)
  e$full_source_rows<-e$selected_rows<-e$omitted_rows<-0L;e$per_chain<-selection$per_chain[FALSE,,drop=FALSE]
  attr(empty,"metadata")<-list(version="bmeas_individual_parameter_summary_v2",
    status="not_applicable_no_predictor_parameters",requested_scales="natural",selection=e)
  attr(empty,"work")<-list(logical_parameter_cells=0L,retained_source_rows=0L,selected_source_rows=0L)
  attr(empty,"draw_keys")<-keys[FALSE,,drop=FALSE]
  attr(empty,"source_receipt")<-list(status="not_needed",reason="No predictor parameters; no acquisition")
  before<-serialize(empty,NULL);view<-private(".bh_individual_result_view")(empty);printed<-capture.output(print(empty))
  ok(identical(view$selection_request_executed,FALSE)&&any(grepl("Requested balanced states: 7",printed,fixed=TRUE))&&
    any(grepl("request unexecuted",printed,fixed=TRUE))&&identical(before,serialize(empty,NULL)),
    "genuine zero-predictor exception preserves the seven-state request explicitly as unexecuted")
  empty_all<-empty;attr(empty_all,"metadata")$selection$method<-"all";attr(empty_all,"metadata")$selection$requested_states<-"all"
  ok(length(capture.output(print(empty_all)))>0,"genuine no-predictor all-state result retains its no-acquisition behavior")
  bad<-structure(lapply(unclass(table),function(column)column[1L]),
    class=c("bmeas_individual_parameter_summary","data.frame"),row.names=1L)
  for(name in c("metadata","work","draw_keys","source_receipt"))attr(bad,name)<-attr(empty,name,exact=TRUE)
  bad$n_draws<-0L
  expect_bad(bad,"empty-request exception requires an actually zero-row result")
  bad<-empty;attr(bad,"work")$logical_parameter_cells<-1L
  expect_bad(bad,"empty-request exception requires zero logical cells")
  bad<-empty;attr(bad,"metadata")$status<-"computed"
  expect_bad(bad,"empty-request exception requires not-applicable status")
  bad<-empty;attr(bad,"source_receipt")<-NULL
  expect_bad(bad,"empty-request exception requires its no-acquisition receipt")
  bad<-empty;attr(bad,"source_receipt")<-list(status="acquired")
  expect_bad(bad,"empty-request exception rejects an acquisition receipt")
  bad<-empty;attr(bad,"source_receipt")<-list(status_unrelated="not_needed")
  expect_bad(bad,"empty-request exception requires exact no-acquisition receipt status")
  bad<-empty;attr(bad,"source_receipt")<-alist(status=)
  expect_bad(bad,"empty-request exception rejects a missing receipt-status sentinel")
  bad<-empty;attr(bad,"draw_keys")<-keys[1L,,drop=FALSE]
  expect_bad(bad,"empty-request exception requires empty keys")
  bad<-empty;attr(bad,"metadata")$selection$per_chain<-selection$per_chain[1L,,drop=FALSE]
  expect_bad(bad,"empty-request exception requires empty per-chain allocations")
  bad<-empty;attr(bad,"metadata")$selection$requested_states$draws<-0L
  expect_bad(bad,"an unexecuted request still needs a valid positive selected-state request")
  ok(calls==0L,"all metadata controls remain passive with source policy plan statistic and controller traps installed")
})

cat("PASS",passed,"focused streaming metadata controls\n")
