source(file.path("helpers", "native-guard.R"))
# Synthetic retained-cache controls only. No source ingestion, real posterior,
# forward simulation, compilation or inference validation is performed here.
source_root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode<-nzchar(source_root)
if(source_mode) {
  source_root<-normalizePath(source_root)
  for(file in list.files(file.path(source_root,"R"),full.names=TRUE))source(file)
}else library(bmeas)
private<-function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
passed<-0L
ok<-function(value,label) {
  if(!isTRUE(value))stop(label,call.=FALSE)
  passed<<-passed+1L;cat("PASS",label,"\n")
}
terminal_calls<-character()
terminal<-function(...) {
  terminal_calls<<-c(terminal_calls,"backend_or_callback")
  stop("forbidden backend or callback")
}
with_passive_traps<-function(expr) {
  originals<-list();calls_before<-terminal_calls
  install<-function(env,name) {
    if(!exists(name,env,inherits=FALSE)||!is.function(get(name,env,inherits=FALSE)))return(invisible(NULL))
    locked<-bindingIsLocked(name,env)
    originals[[length(originals)+1L]]<<-list(env=env,name=name,locked=locked,value=get(name,env,inherits=FALSE))
    if(locked)unlockBinding(name,env)
    assign(name,local({route<-name;function(...) {
      terminal_calls<<-c(terminal_calls,route);stop(paste("FORBIDDEN_REPORT_ROUTE",route))
    }}),env)
    if(locked)lockBinding(name,env)
  }
  on.exit(for(entry in rev(originals)) {
    if(bindingIsLocked(entry$name,entry$env))unlockBinding(entry$name,entry$env)
    assign(entry$name,entry$value,entry$env)
    if(entry$locked)lockBinding(entry$name,entry$env)
  },add=TRUE)
  env<-if(source_mode).GlobalEnv else asNamespace("bmeas")
  for(name in c(ls(env,all.names=TRUE,pattern="^\\.bh_ppc_"),".bh_validate_fingerprint",".bh_prepare",
    ".bh_fit_scalar",".bh_compile_scalar",".bh_numeric_cache",".mp_hash",".mp_sha256"))install(env,name)
  for(name in c("readRDS","readLines","readBin","readChar","scan","load","saveRDS",
    "writeLines","writeBin","file","gzfile","url","file.copy","file.create","unlink",
    "dir.create","system","system2","dyn.load","loadNamespace","set.seed","RNGkind",
    "sample","sample.int"))install(baseenv(),name)
  for(name in c("rnorm","runif","rbinom","rlnorm","quantile","cov"))install(asNamespace("stats"),name)
  value<-force(expr)
  if(!identical(terminal_calls,calls_before))stop("a report reached a forbidden route")
  value
}
read_all<-function(fit) {
  checks<-fit_checks(fit);s<-summary(fit)
  list(checks=checks,summary=s,printed=capture.output(print(fit)),
    checks_printed=capture.output(print(checks)),summary_printed=capture.output(print(s)),
    parameters=parameter_summary(fit),posterior=posterior_summary(fit))
}
blocked<-tryCatch(with_passive_traps(private(".bh_ppc_assess")()),error=identity)
ok(inherits(blocked,"error")&&grepl("FORBIDDEN_REPORT_ROUTE .bh_ppc_assess",conditionMessage(blocked),fixed=TRUE),
  "positive control intercepts the explicit assessor before any work")
dat<-data.frame(person=c("p1","p2","p3"),x1=c(0,1,2),x2=c(2,1,0),x3=c(1,2,1))
spec<-bmeas(bmf(A=~x1+x2+x3),dat,subject="person",fit=FALSE)
for(name in names(spec$models)) {
  spec$models[[name]]$model$validate_data<-terminal
  spec$models[[name]]$model$informative_rows<-terminal
}
parameters<-data.frame(variable="alpha[1]",parameter_key="synthetic_alpha",
  parameter_role="primitive",rhat=1.02,ess_bulk=8,ess_tail=6,mcse_mean=.1)
fit<-structure(list(spec=spec,cache=list(parameter_summary=parameters,
  hmc=list(num_divergent=1,num_max_treedepth=0,ebfmi=.2),
  computation=list(status="warning",reason="synthetic cache control"),
  inference=list(status="not_assessed"),identification=spec$checks$identification,
  posterior_normalizability=spec$checks$posterior_normalizability,
  posterior_propriety=spec$checks$posterior_propriety,
  predictive_fit=list(status="not_computed"),measurement_structure_fit=list(status="not_assessed")),
  backend=list(draws=terminal,metadata=terminal,summary=terminal,diagnostic_summary=terminal,
    output_files=terminal,sample=terminal),provenance=list(kind="synthetic_cache_control")),
  class=c("bmeas_fit","bhmm_fit"))
tab<-data.frame(statistic_id=c("mean_1","mean_2","pair_1_2"),
  domain=c("marginal","marginal","measurement_structure"),level="population",
  unit=c("points","seconds","points * seconds"),observed=c(1,4,-1),
  replicated_median=c(1.2,3,.1),lower90=c(.8,2,NA),upper90=c(1.5,3.5,NA),
  finite_replications=c(4L,4L,1L),requested_replications=4L,
  outside_interval=c(FALSE,TRUE,NA))
coverage<-list(planned=3L,evaluable=2L,complete=2L,unavailable=1L,status="partial")
unavailable<-list(planned=1L,evaluable=0L,complete=0L,unavailable=1L,status="unavailable")
empty<-list(planned=0L,evaluable=0L,complete=0L,unavailable=0L,status="unavailable")
ppc<-list(table=tab,replicate_statistics=matrix(c(.8,1,1.4,1.5,2,3,3,3.5,.1,NA,NA,NA),4,3,
  dimnames=list(NULL,tab$statistic_id)),coverage=coverage,measurement_structure=unavailable,
  conditional_association=empty,quantile_type=7L,interpretation="synthetic coarse intervals",
  metadata=list(level="population",law="iid_common_plus_parameter_residual",
    requested_replications=4L,completed_replications=4L,
    source_qualification="synthetic_cache_control",
    source_receipt=list(owned_scratch_path="/synthetic-private-path/must-not-print")))
initial<-with_passive_traps(read_all(fit))
ok(identical(names(initial$checks$predictive),c("assessment","results","scope"))&&
  !any(grepl("Cached PPC:",initial$printed,fixed=TRUE)),"absent PPC preserves the historical projection and printing shape")
population<-fit;population$cache$ppc<-ppc
population$cache$predictive_fit<-list(status="warning",level="population")
before<-serialize(population,NULL,version=2);set.seed(804);seed_before<-.Random.seed;kind_before<-RNGkind()
reported<-with_passive_traps(read_all(population));projection<-reported$checks$predictive$cached
ok(identical(projection$table$record,tab)&&identical(projection$coverage$record,coverage)&&
  projection$status=="warning"&&projection$replications$requested$record==4L&&
  projection$replications$completed$record==4L,"cached statistics units coverage and replication counts are retained exactly")
ok(identical(reported$checks$predictive$results$record,ppc)&&
  identical(reported$summary$cache,population$cache)&&
  identical(class(reported$summary),c("summary.bmeas_fit","summary.bhmm_fit")),"original PPC evidence cache and public summary classes remain unchanged")
ok(identical(reported$checks$sampling,initial$checks$sampling)&&
  identical(reported$checks$domains$identification,initial$checks$domains$identification)&&
  identical(reported$checks$domains$posterior_propriety,initial$checks$domains$posterior_propriety),
  "predictive presentation preserves independent sampler identification and propriety records")
ok(identical(before,serialize(population,NULL,version=2))&&identical(seed_before,.Random.seed)&&
  identical(kind_before,RNGkind()),"all report entry points preserve fit bytes and full caller RNG")
ok(identical(reported,with_passive_traps(read_all(population))),"repeat reads are deterministic")
all_lines<-unlist(reported[c("printed","checks_printed","summary_printed")])
ok(any(grepl("replicated median:",all_lines,fixed=TRUE))&&
  any(grepl("finite/requested:",all_lines,fixed=TRUE))&&any(grepl("coarse|Coarse",all_lines))&&
  any(grepl("Saved source qualification: synthetic_cache_control",all_lines,fixed=TRUE))&&
  !any(grepl("synthetic-private-path",all_lines,fixed=TRUE)),"public printers show descriptive comparisons without exposing source scratch paths")
centered<-population;centered$cache$ppc$metadata$law<-"same_N_sample_centered_task_cholesky"
centered_report<-with_passive_traps(read_all(centered))
ok(any(grepl("same_N_sample_centered_task_cholesky",centered_report$printed,fixed=TRUE)),
  "centered same-roster law is displayed without iid relabeling")
conditional<-population;conditional$cache$ppc$metadata$level<-"conditional"
conditional$cache$ppc$metadata$law<-"conditional_response"
conditional$cache$ppc$table$level<-"conditional"
conditional$cache$ppc$table$domain[3]<-"conditional_association"
conditional$cache$measurement_structure_fit<-list(status="not_assessed",level="conditional")
conditional_report<-with_passive_traps(read_all(conditional))
ok(any(grepl("Population measurement structure: unassessed; conditional response checks only",
  conditional_report$printed,fixed=TRUE))&&
  conditional_report$checks$domains$measurement_structure_fit$record$status=="not_assessed",
  "conditional presentation never promotes paired summaries to population structure evidence")
failed<-population;failed$cache$ppc<-list(table=NULL,replicate_statistics=NULL,
  metadata=c(ppc$metadata,list(failure=list(phase="forward_simulation",code="E_PREDICTION_SUPPORT",
    reason="/synthetic-private-path/failure"))))
failed$cache$predictive_fit<-list(status="failed",level="population")
failed_report<-with_passive_traps(read_all(failed))
ok(failed_report$checks$predictive$cached$status=="failed"&&
  is.null(failed_report$checks$predictive$cached$table$record)&&
  !any(grepl("replicated median:|synthetic-private-path",failed_report$printed)),
  "failed attempts have no stale successful comparison or printed receipt paths")
uncomputed<-fit;uncomputed$cache$ppc<-list(metadata=list(level="population"))
ok(with_passive_traps(fit_checks(uncomputed))$predictive$cached$status=="uncomputed",
  "explicit uncomputed and failed statuses remain distinct")
for(status in c("not_run","not_assessed","warning","computed","failed")) {
  saved_status<-population;saved_status$cache$predictive_fit$status<-status
  result<-with_passive_traps(read_all(saved_status))
  expected_status<-if(status=="not_run")"uncomputed"else status
  ok(result$checks$predictive$cached$status==expected_status&&
    identical(result$checks$predictive$assessment$record,saved_status$cache$predictive_fit),
    paste("saved assessment status remains explicit:",status))
}
historical<-population;historical$cache$predictive_fit<-NULL
ok(with_passive_traps(fit_checks(historical))$predictive$cached$status=="retained",
  "historical table presence records retained evidence without inferring computation status")
bad_cases<-list(container_numeric=function(x)42,
  container_class=function(x)structure(x,class="trap_object"),
  container_duplicate=function(x)c(x,x["table"]),
  metadata_numeric=function(x){x$metadata<-42;x},
  metadata_class=function(x){x$metadata<-structure(x$metadata,class="trap_object");x},
  metadata_duplicate=function(x){x$metadata<-c(x$metadata,x$metadata["level"]);x},
  level_list=function(x){x$metadata$level<-list("population");x},
  count_class=function(x){x$metadata$requested_replications<-structure(4L,class="trap_object");x},
  table_list_column=function(x){x$table$unit<-I(as.list(x$table$unit));x},
  table_duplicate=function(x){names(x$table)[2]<-"statistic_id";x},
  table_class=function(x){class(x$table)<-c("trap_object","data.frame");x},
  coverage_function=function(x){x$coverage<-terminal;x},
  coverage_class=function(x){x$coverage$evaluable<-structure(2,class="trap_object");x},
  qualification_class=function(x){x$metadata$source_qualification<-structure("synthetic",class="trap_object");x},
  quantile_environment=function(x){x$quantile_type<-new.env(parent=emptyenv());x})
`$.trap_object`<-`[.trap_object`<-`format.trap_object`<-`as.character.trap_object`<-terminal
for(name in names(bad_cases)) {
  altered<-population;altered$cache$ppc<-bad_cases[[name]](ppc)
  snapshot<-serialize(altered,NULL,version=2)
  output<-with_passive_traps(read_all(altered))
  ok(identical(output$checks$sampling,reported$checks$sampling)&&
    identical(output$checks$domains,reported$checks$domains)&&
    identical(snapshot,serialize(altered,NULL,version=2)),paste("optional PPC defect stays local:",name))
}
bad_meta<-population;bad_meta$cache$ppc$metadata<-42
bad_meta_report<-with_passive_traps(fit_checks(bad_meta))$predictive$cached
ok(bad_meta_report$metadata$availability=="unavailable_invalid_metadata"&&
  bad_meta_report$level$availability=="unavailable_invalid_metadata"&&
  identical(bad_meta_report$table$record,tab),"invalid metadata does not discard an independently valid PPC table")
conditional_bad_meta<-conditional;conditional_bad_meta$cache$ppc$metadata<-42
conditional_bad_report<-with_passive_traps(read_all(conditional_bad_meta))
ok(any(grepl("Population measurement structure: unassessed; conditional response checks only",
  conditional_bad_report$printed,fixed=TRUE)),"conditional table evidence prevents a population claim even with invalid optional metadata")
no_rows<-population;no_rows$cache$ppc$table<-tab[FALSE,,drop=FALSE]
no_rows$cache$ppc$coverage<-empty;no_rows$cache$ppc$measurement_structure<-empty
empty_report<-with_passive_traps(read_all(no_rows))
ok(empty_report$checks$predictive$cached$coverage$record$status=="unavailable"&&
  any(grepl("evaluable: 0 of 0",empty_report$printed,fixed=TRUE)),"empty discrepancy sets retain unavailable evidence and never pass vacuously")
# Use the controller's exact aggregate/result/status shape, with no simulated
# response or posterior input, for empty and wholly unavailable statistic sets.
empty_observed<-private(".bh_ppc_stats_empty")()
empty_observed$estimate<-numeric();empty_observed$status<-character()
attr(empty_observed,"level")<-"population"
unavailable_observed<-data.frame(statistic_id="unavailable_pair",domain="measurement_structure",
  level="population",unit="points * seconds",estimate=NA_real_,status="unavailable_no_overlap")
aggregate_fixtures<-list()
for(name in c("empty","all_unavailable")) {
  observed<-if(name=="empty")empty_observed else unavailable_observed
  reps<-matrix(NA_real_,4L,nrow(observed),dimnames=list(NULL,observed$statistic_id))
  aggregate<-private(".bh_ppc_aggregate")(observed,reps)
  aggregate$metadata<-ppc$metadata
  statuses<-private(".bh_ppc_current_status")(aggregate,"population")
  fixture<-population;fixture$cache$ppc<-aggregate
  fixture$cache$predictive_fit<-statuses$predictive_fit
  fixture$cache$measurement_structure_fit<-statuses$measurement_structure_fit
  result<-with_passive_traps(read_all(fixture))
  ok(result$checks$predictive$cached$status=="not_assessed"&&
    result$checks$predictive$cached$coverage$record$evaluable==0L&&
    identical(result$checks$predictive$cached$table$record,aggregate$table)&&
    any(grepl("Cached PPC: not_assessed",result$printed,fixed=TRUE))&&
    (name=="empty"||any(grepl("unavailable_pair",result$printed,fixed=TRUE))),
    paste("actual aggregate shape preserves unassessed evidence:",name))
  aggregate_fixtures[[name]]<-fixture
}
rm(".Random.seed",envir=.GlobalEnv)
invisible(with_passive_traps(read_all(population)))
ok(!exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE),"passive PPC reads do not initialize absent RNG state")
# The cold-process transport is test harness work outside report traps.
local({
  directory<-tempfile("ppc-presentation-");dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  input<-file.path(directory,"fixture.rds");output<-file.path(directory,"result.rds")
  runner<-file.path(directory,"cold.R")
  fixtures<-c(list(population,centered,conditional,failed,bad_meta,conditional_bad_meta,no_rows),aggregate_fixtures)
  for(i in seq_along(fixtures))fixtures[[i]]$backend<-NULL
  expected<-lapply(fixtures,function(x)with_passive_traps(read_all(x)))
  saveRDS(list(fixtures=fixtures,expected=expected),input,version=2)
  setup<-if(source_mode)c(paste0("source_root <- ",deparse(source_root)),"source_mode <- TRUE",
    "for(file in list.files(file.path(source_root,'R'),full.names=TRUE))source(file)")else
    c(paste0(".libPaths(",paste(deparse(.libPaths()),collapse=""),")"),"source_mode <- FALSE","library(bmeas)")
  helpers<-unlist(lapply(c("private","terminal","with_passive_traps","read_all"),function(name)
    c(paste0(name," <- "),deparse(get(name,.GlobalEnv)))),use.names=FALSE)
  writeLines(c(setup,helpers,"terminal_calls <- character()",
    "args <- commandArgs(TRUE); saved <- readRDS(args[1L])",
    "before <- lapply(saved$fixtures,serialize,connection=NULL,version=2)",
    "set.seed(804); old_seed <- .Random.seed; old_kind <- RNGkind(); old_cwd <- getwd()",
    "actual <- lapply(saved$fixtures,function(x)with_passive_traps(read_all(x)))",
    "stopifnot(identical(actual,saved$expected),identical(old_seed,.Random.seed),identical(old_kind,RNGkind()),identical(old_cwd,getwd()))",
    "stopifnot(identical(lapply(saved$fixtures,serialize,connection=NULL,version=2),before))",
    "saveRDS(TRUE,args[2L],version=2)"),runner)
  log<-system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(runner),shQuote(input),shQuote(output)),stdout=TRUE,stderr=TRUE)
  if(!is.null(attr(log,"status"))&&attr(log,"status")!=0L)stop(paste(log,collapse="\n"))
  ok(file.exists(output)&&isTRUE(readRDS(output)),"cold detached PPC reports match warm reports without changing bytes RNG or working directory")
})
cat("Frontend predictive presentation:",passed,"passed; synthetic cache controls only, no posterior validation.\n")
