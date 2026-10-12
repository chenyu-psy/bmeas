source(file.path("helpers", "native-guard.R"))
# Pure consumer argument guards; no source files, acquisition or simulation.
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(root)
if (source_mode) for (f in list.files(file.path(root,"R"),full.names=TRUE)) source(f) else library(bmeas)
private <- function(name) if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
passed <- 0L
ok <- function(value,label) { stopifnot(isTRUE(value)); passed <<- passed+1L; cat("PASS",label,"\n") }
local({
policy <- list(reader="streaming", max_source_bytes=1048576, max_read_bytes=7340032,
 max_materialized_values=1000000, max_record_bytes=1024, max_columns=100,
 max_source_rows=1000, max_chains=4, max_temp_bytes=2097152, max_scan_values=2000)
set.seed(532); rng <- .Random.seed; oldcwd <- getwd(); calls <- 0L
env <- if(source_mode).GlobalEnv else asNamespace("bmeas")
reader_name <- ".bh_posterior_ingest_csv"; old <- get(reader_name,env); locked <- bindingIsLocked(reader_name,env)
if(locked)unlockBinding(reader_name,env)
on.exit({assign(reader_name,old,env);if(locked)lockBinding(reader_name,env)},add=TRUE)
assign(reader_name,function(...) { calls <<- calls+1L; stop("acquisition forbidden") },env)
for (name in names(policy)) {
  bad <- policy; bad[name] <- alist(value=)
  e <- tryCatch(private(".bh_posterior_consumer_policy")(bad,character()),error=identity)
  ok(inherits(e,"measurement_error") && identical(e$code,"E_PPC_SOURCE_ARGUMENT"),paste("typed nested field refusal",name))
  e <- tryCatch(diagnostic_budget(4,20,80,posterior=bad),error=identity)
  ok(inherits(e,"measurement_error") && identical(e$code,"E_PPC_SOURCE_ARGUMENT"),paste("public budget rejects nested missing",name))
}
ok(identical(private(".bh_posterior_consumer_policy")(policy,"eta[1,1]"),private(".bh_posterior_policy")(policy,"eta[1,1]")),"ordinary normalized policy exact")
ok(identical(diagnostic_budget(4,20,80),diagnostic_budget(4,20,80,posterior=NULL)),"legacy NULL payload exact")
ok(calls==0L && identical(rng,.Random.seed) && identical(oldcwd,getwd()),"no acquisition or RNG/cwd change")
})

local({
  env<-if(source_mode).GlobalEnv else asNamespace("bmeas")
  names_<-c(".bh_posterior_ingest_csv",".bh_individual_ingest_csv",".bh_ppc_validate_saved_spec")
  old<-lapply(names_,function(name)list(value=get(name,env),locked=bindingIsLocked(name,env)))
  names(old)<-names_
  on.exit(for(name in names_) {
    assign(name,old[[name]]$value,env)
    if(old[[name]]$locked)lockBinding(name,env)
  },add=TRUE)
  calls<-0L
  trap<-function(...) {calls<<-calls+1L;stop("unexpected source/backend/spec access")}
  for(name in names_) {
    if(old[[name]]$locked)unlockBinding(name,env)
    assign(name,trap,env)
  }
  old_warn<-getOption("warn");options(warn=2)
  on.exit(options(warn=old_warn),add=TRUE)
  fit<-structure(list(backend=list(draws=trap,metadata=trap)),class="bhmm_fit")
  good<-list(posterior=list(reader="streaming",max_source_bytes=1000,max_read_bytes=7000,
    max_materialized_values=1000000),max_parameter_cells=10,max_summary_rows=30,
    max_transformation_terms=1000,max_declared_observation_rows=20,max_consumed_observation_rows=20,
    max_operand_context_rows=50,max_context_terms=10000)
  for(field in names(good)) {
    bad<-good;bad[field]<-alist(value=)
    e<-tryCatch(individual_parameter_summary(fit,scales="predictor",budget=bad),error=identity)
    ok(inherits(e,"measurement_error")&&identical(e$code,"E_INDIVIDUAL_BUDGET")&&
      grepl(field,conditionMessage(e),fixed=TRUE),paste("typed public streaming missing-field refusal:",field))
  }
  for(field in c("method","draws")) {
    states<-list(method="balanced",draws=4L);states[field]<-alist(value=)
    e<-tryCatch(individual_parameter_summary(fit,scales="predictor",budget=good,states=states),error=identity)
    ok(inherits(e,"measurement_error")&&identical(e$code,"E_INDIVIDUAL_ARGUMENT")&&
      grepl(field,conditionMessage(e),fixed=TRUE),paste("typed public streaming states missing-field refusal:",field))
  }
  old_budget<-list(max_source_bytes=10485760L,max_posterior_rows=100L,max_draw_values=50000L,max_parameter_cells=1000L)
  ok(identical(private(".bh_individual_budget")(old_budget),old_budget),
    "legacy four-field valid budget representation remains exact")
  ok(identical(private(".bh_individual_budget")(good),good)&&
    identical(private(".bh_individual_states")(list(method="balanced",draws=4L),TRUE),list(method="balanced",draws=4)),
    "ordinary explicit streaming values still resolve through the admitted pure boundary")
  ok(calls==0L,"all focused guards run before source provider or saved-spec callbacks")
})
cat("PASS",passed,"focused posterior consumer policy guards\n")
