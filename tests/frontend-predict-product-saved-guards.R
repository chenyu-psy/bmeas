source(file.path("helpers","native-guard.R"))
root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root))for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
source(file.path("helpers","residual-product-fixtures.R"))
source(file.path("helpers","product-archive-fixtures.R"))
(function() {
 private<-product_archive_private;ns<-if(nzchar(root)).GlobalEnv else asNamespace("bmeas")
 passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
 err<-function(action,codes=NULL) {e<-tryCatch(action(),error=identity);if(!inherits(e,"measurement_error")||(!is.null(codes)&&!e$code%in%codes)){print(e);stop("wrong refusal")};ok(TRUE,e$code);e}
 work<-tempfile("product-ppc-guards-");dir.create(work);on.exit(unlink(work,recursive=TRUE),add=TRUE)
 context<-list(max_context_bytes=2^25,max_context_nodes=4000000,max_depth=64)
 posterior<-list(reader="streaming",max_source_bytes=2^21,max_read_bytes=7*2^21,
   max_materialized_values=10000000,max_record_bytes=2^18,max_columns=4000,
   max_source_rows=20,max_chains=2,max_temp_bytes=2*2^21,max_scan_values=100000)
 budget<-diagnostic_budget(4,100,1000,posterior=posterior)
 archive_budget<-list(max_members=16,max_member_bytes=2^25,max_total_bytes=2^27,
   max_context_bytes=2^25,max_read_bytes=2^30,max_temp_bytes=2^28,posterior=posterior,context=context)
 specs<-product_archive_specs();s<-specs$common_product
 f<-product_archive_fit(s,file.path(work,"source"));p<-portable_fit(f$fit,context)
 # Complete saved matrices for the other layer are engineering fixtures only.
 dual<-specs$dual_product;dualrow<-product_archive_rows(dual,1L)[1,]
 assessed<-assess_measurement_fit(p,level="population",draws=4,seed=66,budget=budget)
 stopifnot(!is.null(assessed$cache$ppc$table));result<-assessed$cache$ppc
 initial<-serialize(p,NULL,version=3);immutable<-serialize(p$immutable,NULL,version=3)
 validated<-private(".bh_pf_active")(p,"assess_measurement_fit")
 malformed<-list(
   profile=function(x){x$immutable$context_profile<-"bmeas_postprocess_context_v1";x},
   family=function(x){x$immutable$scientific_context$models[[1]]$model$likelihood$symbol<-"lognormal_lpdf";x},
   person=function(x){x$immutable$scientific_context$tables$person_law$distribution<-"sample_centered";x},
   operands=function(x){x$immutable$scientific_context$tables$observation_operands$eta_index[1]<-99999L;x},
   roster=function(x){x$immutable$scientific_context$tables$person_roster$person_index[1]<-99999L;x},
   capability=function(x){x$immutable$capabilities$basic_ppc<-"enabled";x},
   callback=function(x){x$immutable$scientific_context$models[[1]]$model$callback<-function(...)stop("CALLBACK");x},
   binding=function(x){x$immutable$protected_evidence$original_provenance$model_fingerprint<-"changed";x})
 set.seed(19271);rng<-.Random.seed;kind<-RNGkind()
 for(n in names(malformed)) {
   bad<-malformed[[n]](p)
   err(function()ppc_plan(bad,4,budget,"population"))
   err(function()assess_measurement_fit(bad,level="population",draws=4,seed=66,budget=budget))
 }
 # Structurally valid byte edits can be planned but may never claim a verified
 # digest; active reading rejects the unsealed edit before acquiring any CSV.
 bad<-p;bad$immutable$retained_artifacts$saved_source_text<-paste0(bad$immutable$retained_artifacts$saved_source_text,"\n// changed")
 err(function()assess_measurement_fit(bad,level="population",draws=4,seed=66,budget=budget))
 samebytes<-p;text<-samebytes$immutable$retained_artifacts$saved_source_text
 substr(text,1,1)<-if(substr(text,1,1)=="?")"!"else"?"
 samebytes$immutable$retained_artifacts$saved_source_text<-text
 refused<-err(function()assess_measurement_fit(samebytes,level="population",draws=4,seed=66,budget=budget),"E_PORTABLE_IDENTITY")
 ok(identical(refused$context_validation$phase,"immutable_encoding_and_digest"),"same-size immutable byte edit reaches and fails the digest gate")
 for(field in c("operation","context_profile","phase","status")) {
   receipt<-validated$receipt;receipt[[field]]<-switch(field,operation="component_draws",context_profile="bmeas_postprocess_context_v1",phase="immutable_schema_and_binding",status="not_verified")
   err(function()private(".bh_ppc_assess")(validated$view,"ppc","population",4,66,budget,context_validation=receipt))
 }
 err(function()private(".bh_ppc_assess")(validated$view,"ppc","population",4,66,budget))
 err(function()assess_measurement_fit(p,level="population",draws=4,seed=66,budget=budget,simulator=list()),"E_CHECK_UNSUPPORTED")
 err(function()refresh_diagnostics(p,character(),list()),"E_PORTABLE_CAPABILITY")
 # Current-reader results are strictly bound, including failed attempts.
 changes<-list(
   capability=function(x){x$metadata$reader_execution$execution<-"arbitrary";x},
   plan_capability=function(x){x$metadata$plan$source$reader_execution<-private(".bh_m3cs_reader_capability")();x},
   missing_capability=function(x){x$metadata$reader_execution<-NULL;x},
   missing_both_capabilities=function(x){x$metadata$reader_execution<-NULL;x$metadata$plan$source$reader_execution<-NULL;x},
   law=function(x){x$metadata$law<-"same_N_sample_centered_task_cholesky";x},
   level=function(x){x$metadata$level<-"conditional";x},
   family=function(x){x$metadata$version<-"bmeas_m3cs_ppc_result_v2";x},
   plan_law=function(x){x$metadata$plan$law<-"same_N_sample_centered_task_cholesky";x},
   plan_identity=function(x){x$metadata$plan$model_identity$specification_fingerprint<-"changed";x},
   source_identity=function(x){x$metadata$source_receipt$bound_model_identity$specification_fingerprint<-"changed";x},
   portable_identity=function(x){x$metadata$source_receipt$portable_bound_identity$digest<-paste(rep("0",32),collapse="");x},
   missing_portable_identity=function(x){x$metadata$source_receipt$portable_bound_identity<-NULL;x},
   receipt_profile=function(x){x$metadata$source_receipt$context_validation$context_profile<-"bmeas_postprocess_context_v1";x},
   receipt_operation=function(x){x$metadata$source_receipt$context_validation$operation<-"component_draws";x},
   inference=function(x){x$metadata$upstream$inference$status<-"qualified";x},
   table_level=function(x){x$table$level[]<-"conditional";x})
 for(n in names(changes)) {
   bad<-changes[[n]](result);err(function()private(".bh_pf_ppc")(bad,"cache.ppc",s$tables))
   fit<-assessed;fit$cache$ppc<-bad
   err(function()assess_measurement_fit(fit,level="population",draws=4,seed=66,budget=budget))
 }
 # Saved classes cannot execute dispatch while capability/plan consistency is checked.
 hits<-0L;access<-function(...){hits<<-hits+1L;stop("HOSTILE DISPATCH")}
 assign("$.bmeas_ppc_plan",access,.GlobalEnv);on.exit(if(exists("$.bmeas_ppc_plan",envir=.GlobalEnv,inherits=FALSE))rm("$.bmeas_ppc_plan",envir=.GlobalEnv),add=TRUE)
 private(".bh_pf_ppc")(result,"cache.ppc",s$tables)
 rm("$.bmeas_ppc_plan",envir=.GlobalEnv)
 assign("Ops.product_hostile",access,.GlobalEnv);on.exit(rm("Ops.product_hostile",envir=.GlobalEnv),add=TRUE)
 bad<-result;class(bad$table$level)<-"product_hostile";err(function()private(".bh_pf_ppc")(bad,"cache.ppc",s$tables))
 ok(hits==0L,"malformed cached plan/table never dispatch saved accessors or Ops")
 # Exact predictive work allowance is admitted; every underfunded work limit refuses.
 plan<-ppc_plan(p,4,budget,"population");exact<-budget
 exact$max_observations<-plan$observations_per_rep;exact$max_simulated_values<-plan$simulated_values
 exact$max_latent_values<-plan$latent_values;exact$max_statistics<-plan$statistic_counts$statistics
 exact$max_pair_terms<-plan$statistic_counts$pair_terms;exact$max_work_values<-sum(unlist(plan$allocation_accounting))
 exact$posterior$max_source_bytes<-sum(file.info(f$paths)$size)
 exact$posterior$max_read_bytes<-7*exact$posterior$max_source_bytes;exact$posterior$max_temp_bytes<-2*exact$posterior$max_source_bytes
 out<-assess_measurement_fit(p,level="population",draws=4,seed=66,budget=exact)
 ok(!is.null(out$cache$ppc$table),"exact predictive and source-byte/read/temp allowances admit unchanged arithmetic")
 for(n in c("max_observations","max_simulated_values","max_latent_values","max_statistics","max_pair_terms","max_work_values")) {
   low<-exact;low[[n]]<-low[[n]]-1
   err(function()ppc_plan(p,4,low,"population"),"E_DIAGNOSTIC_BUDGET")
 }
 low<-exact;low$posterior$max_source_bytes<-low$posterior$max_source_bytes-1
 out<-assess_measurement_fit(p,level="population",draws=4,seed=66,budget=low)
 ok(is.null(out$cache$ppc$table),"insufficient source allowance retains explicit failed attempt")
 private(".bh_pf_ppc")(out$cache$ppc,"cache.ppc",s$tables)
 # Forward negatives reject missing/nonfinite values, malformed factors,
 # residual map scales and unrepresentable arithmetic without any repair.
 for(spec in list(s,dual)) {
   forward<-private(".bh_ppc_forward_plan")(spec,"population",unclass(plan)$budget)
   row<-if(identical(spec,dual))dualrow else f$chain_rows[[1]][1,]
   faults<-list(missing=function(x)x[-match(forward$required_variables[1],names(x))],
     nonfinite=function(x){x[forward$required_variables[1]]<-Inf;x})
   if(forward$geometry$K) {
     faults$upper<-function(x){x["L_component[1,2]"]<-.1;x}
     faults$diagonal<-function(x){x["L_component[1,1]"]<-0;x}
     faults$gram<-function(x){x["L_component[1,1]"]<-1.1;x}
     faults$overflow<-function(x){x[grepl("^effective_loadings",names(x))]<-.Machine$double.xmax;x}
   }
   if(forward$geometry$U) {
     faults$residual_factor<-function(x){x["L_parameter[1,1]"]<-1.1;x}
     faults$residual_scale<-function(x){x["D_parameter[1,1]"]<-0;x}
   }
   if("obs_sd[1]"%in%names(row))faults$obs_scale<-function(x){x["obs_sd[1]"]<--1;x}
   for(n in names(faults))err(function()private(".bh_ppc_with_rng")(66,function()private(".bh_ppc_simulate_responses")(forward,faults[[n]](row))),c("E_PPC_DRAW","E_PPC_NUMERICAL"))
 }
 conditional<-private(".bh_ppc_forward_plan")(s,"conditional",unclass(plan)$budget)
 for(value in list(NA_real_,Inf)) {
   row<-f$chain_rows[[1]][1,];row[conditional$required_variables[1]]<-value
   err(function()private(".bh_ppc_with_rng")(66,function()private(".bh_ppc_simulate_responses")(conditional,row)),"E_PPC_DRAW")
 }
 # Whole-source selected and unselected required values must be validated.
 original<-readBin(f$paths[1],"raw",n=file.info(f$paths[1])$size)
 badrows<-f$chain_rows[[1]];badrows[2,"alpha[1]"]<-Inf
 product_archive_write_csv(f$paths[1],badrows,1L)
 out<-assess_measurement_fit(p,level="population",draws=4,seed=66,budget=budget)
 ok(is.null(out$cache$ppc$table),"nonfinite required value in an unselected source row is refused")
 writeBin(original,f$paths[1])
 badrows<-f$chain_rows[[1]][,colnames(f$chain_rows[[1]])!="alpha[1]",drop=FALSE]
 product_archive_write_csv(f$paths[1],badrows,1L)
 out<-assess_measurement_fit(p,level="population",draws=4,seed=66,budget=budget)
 ok(is.null(out$cache$ppc$table),"missing source columns are never reconstructed")
 writeBin(original,f$paths[1])
 # Protected improper evidence survives editing the mutable report cache.
 adversefit<-f$fit;adversefit$cache$posterior_propriety$status<-"proven_improper"
 adverse<-portable_fit(adversefit,context);adverse$cache$posterior_propriety$status<-"not_assessed"
 err(function()ppc_plan(adverse,4,budget,"population"),"E_PPC_POSTERIOR")
 err(function()assess_measurement_fit(adverse,level="population",draws=4,seed=66,budget=budget),"E_PPC_POSTERIOR")
 archive<-archive_fit(p,file.path(work,"archive"),archive_budget)
 err(function()ppc_plan(archive,4,diagnostic_budget(4,100,1000),"population"))
 members<-list.files(archive$root,recursive=TRUE,full.names=TRUE);csv<-members[grepl("[.]csv$",members)][1]
 raw<-readBin(csv,"raw",n=file.info(csv)$size);cat("\n# changed\n",file=csv,append=TRUE)
 err(function()ppc_plan(archive,4,budget,"population"));err(function()assess_measurement_fit(archive,level="population",draws=4,seed=66,budget=budget))
 writeBin(raw,csv)
 manifest<-file.path(archive$root,"manifest.rds")
 if(file.exists(manifest)){raw<-readBin(manifest,"raw",n=file.info(manifest)$size);cat("changed",file=manifest,append=TRUE);err(function()ppc_plan(archive,4,budget,"population"));writeBin(raw,manifest)}
 ok(identical(rng,.Random.seed)&&identical(kind,RNGkind()),"successful and failed attempts preserve caller RNG")
 rm(".Random.seed",envir=.GlobalEnv)
 absent<-assess_measurement_fit(p,level="conditional",draws=4,seed=66,budget=budget)
 ok(!is.null(absent$cache$ppc$table)&&!exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE),"successful saved reading restores absence of caller RNG state")
 absent<-assess_measurement_fit(p,level="population",draws=4,seed=66,budget=low)
 ok(is.null(absent$cache$ppc$table)&&!exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE),"failed saved reading restores absence of caller RNG state")
 assign(".Random.seed",rng,envir=.GlobalEnv)
 ok(identical(initial,serialize(p,NULL,version=3))&&identical(immutable,serialize(assessed$immutable,NULL,version=3)),"all adversarial work preserves original capsule identity and producer evidence")
 cat("Saved product PPC guard assertions:",passed,"\n")
})()
