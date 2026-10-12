source(file.path("helpers", "native-guard.R"))
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root)) for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE)) source(f) else library(bmeas)
resource <- Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
if(!nzchar(resource)||!file.exists(resource)) {
  cat("SKIP saved M3-CS: external pinned resource absent\n")
} else (function() {
ns <- if(nzchar(root)) .GlobalEnv else asNamespace("bmeas")
for(n in c(".bh_ppc_forward_plan",".bh_ppc_generate_eta",".mp_hash",".mp_sha256",".bh_m3cs_reader_capability",".bh_m3cs_joint_reader_capability",".bh_m3cs_kind",".bh_m3cs_spec"))
  assign(n,get(n,ns),envir=environment())
replace <- function(n,v) { locked<-bindingIsLocked(n,ns);if(locked)unlockBinding(n,ns)
  assign(n,v,ns);if(locked)lockBinding(n,ns) }
guarded <- c("compile_m3cs_simulator", ".bh_compile_scalar", ".bh_fit_scalar", ".bh_backend_ir",
  ".bh_materialize_init", ".mp_toolchain_fingerprint", ".bh_provider_runtime_info",
  ".bh_provider_preflight", ".bh_provider_sample", "make_stan_code", "standata", "stancode",
  ".bh_ppc_ingest_csv", ".bh_covariance_probe", ".bh_gaussian_identification")
guarded <- guarded[vapply(guarded,exists,logical(1),envir=ns,inherits=FALSE)]
old <- mget(guarded,ns)
for(n in guarded)replace(n,local({label<-n;function(...)stop(paste("FORBIDDEN",label))}))
on.exit(for(n in guarded)replace(n,old[[n]]),add=TRUE)
source(file.path("helpers","m3cs-saved-fixture.R"),local=TRUE)
passed<-0L
ok<-function(x,label) {stopifnot(isTRUE(x));passed<<-passed+1L;cat("PASS",label,"\n")}
refuses<-function(action,code=NULL) {e<-tryCatch(action(),error=identity)
  stopifnot(inherits(e,"error"));if(!is.null(code))stopifnot(identical(e$code,code));invisible(e)}
plain<-function(x) {if(is.environment(x)||is.function(x)||typeof(x)=="externalptr")return(FALSE)
  if(is.list(x))return(all(vapply(x,plain,FALSE)));TRUE}
# Classifier negatives only: these are not saved end-to-end family fixtures.
ok(!.bh_m3cs_kind(m3_model(resource)),"genuine full routed M3 fails narrow classifier")
ok(!.bh_m3cs_kind(ezdm_model()),"genuine EZDM fails narrow classifier")
sdm_resource<-Sys.getenv("MEASUREMENTPKG_SDM_RESOURCE")
if(nzchar(sdm_resource)&&file.exists(sdm_resource))
  ok(!.bh_m3cs_kind(sdm_reference_model(sdm_resource)),"genuine SDM fails narrow classifier") else
  cat("SKIP SDM classifier: external pinned resource absent\n")
owned<-tempfile("saved-m3cs-");dir.create(owned)
on.exit(unlink(owned,recursive=TRUE),add=TRUE)
context<-list(max_context_bytes=20*1024^2,max_context_nodes=2000000,max_depth=64)
posterior<-list(reader="streaming",max_source_bytes=100000,max_read_bytes=700000,
  max_materialized_values=5000000,max_record_bytes=16384,max_columns=300,max_source_rows=20,
  max_chains=2,max_temp_bytes=200000,max_scan_values=10000)
budget<-diagnostic_budget(4,100,4000,max_statistics=600,max_pair_terms=20000,posterior=posterior)
people<-c("z","a","b","unobserved");tasks<-c("t3","t1","t2")
d<-expand.grid(person=people[1:3],task=tasks,stringsAsFactors=FALSE)
d$counts<-I(matrix(0L,nrow(d),10L));d$counts[,1]<-2L;d$counts[,10]<-1L
# Unequal totals and active options test exact row/cell retention.
d$counts[1,]<-0L;d$options<-I(matrix(1L,nrow(d),10L));d$options[,10]<-3L
calls<-list()
# The numerical law below is a deliberately artificial deterministic-seed
# controller double. It does not implement or qualify the external CS logits.
make_simulator<-function(spec,exe) {
  code<-make_m3cs_simulator_code(spec,budget=budget)
  model<-list(exe_file=function()exe,sample=function(data,chains,parallel_chains,iter_warmup,
    iter_sampling,fixed_param,seed,output_dir,refresh,sig_figs,show_messages) {
    stopifnot(chains==1,parallel_chains==1,iter_warmup==0,iter_sampling==1,isTRUE(fixed_param),
      identical(Sys.getenv("STAN_NUM_THREADS"),"1"))
    calls[[length(calls)+1L]]<<-list(data=data,seed=seed)
    p<-matrix(0,data$O,10L);y<-matrix(0L,data$O,10L);active<-c(1:4,10)
    for(o in seq_len(data$O)) {
      logits<-c(data$arguments[o,],0)+log(data$options[o,active]);prob<-exp(logits-max(logits));prob<-prob/sum(prob)
      p[o,active]<-prob;y[o,active]<-as.vector(rmultinom(1,data$totals[o],prob))
    }
    file<-file.path(output_dir,"double.csv")
    writeLines(c("# num_threads = 1","# algorithm = fixed_param","# num_warmup = 0","# num_samples = 1"),file)
    list(return_codes=function()0L,output_files=function()file,draws=function(variables,format) {
      value<-if(variables=="probabilities")p else y;idx<-expand.grid(o=seq_len(data$O),k=1:10)
      matrix(as.numeric(value),1,dimnames=list(NULL,paste0(variables,"[",idx$o,",",idx$k,"]")))
    })
  })
  structure(list(model=model,code=code,code_hash=.mp_hash(code),
    resource_hash=spec$models$cs$model$adapter$resources[[1L]]$sha256,
    executable_hash=.mp_sha256(exe),toolchain=list(kind="R-only deterministic engineering double"),
    version="bmeas_m3cs_rng_v2"),class="bmeas_m3cs_simulator")
}
write_csv<-function(paths,spec) {
  vars<-unique(c(.bh_ppc_forward_plan(spec,"population",budget)$required_variables,
    .bh_ppc_forward_plan(spec,"conditional",budget)$required_variables,"lp__"))
  for(i in seq_along(paths)) {
    x<-setNames(rep(0,length(vars)),vars)
    x[grepl("^task_row_scale",vars)]<-.35
    for(j in 1:12)for(base in c("L_component","L_parameter","L_task_work","D_parameter")) {
      key<-paste0(base,"[",j,",",j,"]");if(key%in%vars)x[key]<-1
    }
    rows<-vapply(1:4,function(k) {y<-x;y[grepl("^eta|^alpha",vars)]<-.1*k+i/100;paste(y,collapse=",")},"")
    writeLines(c("# ENGINEERING ONLY; NOT POSTERIOR DRAWS", "# stan_version_major = 2",
      "# stan_version_minor = 40", "# stan_version_patch = 0", "# model = scalar_components_model",
      paste0("# id = ",i),"# method = sample (Default)","# num_samples = 4","# num_warmup = 0",
      "# save_warmup = false (Default)","# thin = 1","# algorithm = hmc (Default)",
      "# engine = nuts (Default)","# metric = diag_e (Default)","# max_depth = 4","# sig_figs = 17",
      paste(gsub("\\]","",gsub("[\\[,]",".",vars)),collapse=","),rows,
      "# Elapsed Time: 0 seconds (Warm-up)","# 0 seconds (Sampling)","# 0 seconds (Total)"),paths[i])
  }
}
cache<-function(fit)if(inherits(fit,"bmeas_fit_archive"))fit$report_snapshot$portable$cache else fit$cache
run<-function(fit,level,sim,b=budget) {
  calls<<-list();out<-assess_measurement_fit(fit,level=level,draws=4,seed=472,budget=b,simulator=sim)
  list(fit=out,calls=calls,result=cache(out)$ppc)
}

legacy<-readRDS(file.path("fixtures-archive","legacy-joint-m3cs-v033.rds"))
legacy_bytes<-serialize(legacy,NULL)
get(".bh_pf_immutable_schema",ns)(unclass(legacy),get(".bh_pf_policy",ns)(context))
get(".bh_pf_active",ns)(legacy,"component_draws",archive_inspection=TRUE)
ok(identical(legacy$immutable$context_profile,"bmeas_postprocess_context_v1"),"actual .33 legacy producer profile remains unchanged")
refuses(function()ppc_plan(legacy,4,budget,"population"),"E_PORTABLE_CAPABILITY")
refuses(function()assess_measurement_fit(legacy,level="population",draws=4,seed=1,budget=budget),"E_PORTABLE_CAPABILITY")
ok(identical(serialize(legacy,NULL),legacy_bytes),"legacy refusal preserves original bytes and seals")
m<-m3_cs_model(resource)
b<-observe(m,d,"person","task",fields=list(counts="counts",options="options"),units="count",parameters=list(c="task",ra=list(fixed=.3),rc=list(fixed=.7)))
spec<-with_checks(function()bmeas(bmf(G=~left$a[tasks]+right$a[tasks]),list(left=b,right=b),subjects="shared",fit=FALSE))
fit<-portable_fit(cd_shell(spec,c("ABSENT-new-chain1.csv","ABSENT-new-chain2.csv")),context)
callback_hits<-0L
hostile<-function(...){callback_hits<<-callback_hits+1L;stop("FORBIDDEN saved S3 callback")}
for(n in c("$.joint_hostile","[.joint_hostile","[[.joint_hostile"))assign(n,hostile,.GlobalEnv)
on.exit(rm(list=c("$.joint_hostile","[.joint_hostile","[[.joint_hostile"),envir=.GlobalEnv),add=TRUE)
for(field in c("max_observations","max_simulated_values","max_statistics","max_pair_terms","max_latent_values","max_work_values")) {
 small<-budget;small[[field]]<-1L
 refuses(function()ppc_plan(fit,4,small,"population"),"E_DIAGNOSTIC_BUDGET")
}
ok(TRUE,"all combined predictive budgets refuse without truncation")
# Positive complete-design admissions, using public declarations only.
for(variant in c("crosswalk","observed_only","empty_right")) {
 data_right<-d
 if(variant=="crosswalk")data_right$person<-paste0("other_",data_right$person)
 if(variant=="observed_only")data_right$counts[1,1]<-NA_integer_
 if(variant=="empty_right")data_right$counts[,1]<-NA_integer_
 right<-observe(m,data_right,"person","task",fields=list(counts="counts",options="options"),units="count",parameters=list(c="task",ra=list(fixed=.3),rc=list(fixed=.7)))
 subjects<-if(variant=="crosswalk")subject_crosswalk(rbind(
  data.frame(model="left",local_id=people[1:3],person_id=people[1:3]),
  data.frame(model="right",local_id=paste0("other_",people[1:3]),person_id=people[1:3])))else "shared"
 positive<-with_checks(function()bmeas(bmf(G=~left$a[tasks]+right$a[tasks]),list(left=b,right=right),subjects=subjects,
   missing=if(variant=="crosswalk")"error"else"observed_only",fit=FALSE))
 exported<-portable_fit(cd_shell(positive,c("ABSENT-positive1.csv","ABSENT-positive2.csv")),context)
 plan<-ppc_plan(exported,4,budget,"population")
 ok(identical(exported$immutable$context_profile,"bmeas_joint_m3cs_context_v1")&&
    plan$observations_per_rep==sum(vapply(positive$data_map$blocks,nrow,integer(1))),paste(variant,"complete saved design admission"))
}
mutations<-list(
 hostile_block=function(x){class(x$immutable$scientific_context$models$right)<-"joint_hostile";x},
 hostile_rows=function(x){class(x$immutable$scientific_context$data_map$blocks$right)<-c("joint_hostile","data.frame");x},
 hostile_column=function(x){class(x$immutable$scientific_context$tables$observation_operands$block)<-"joint_hostile";x},
 missing_block=function(x){x$immutable$scientific_context$data_map$blocks$right<-NULL;x},
 wrong_order=function(x){x$immutable$scientific_context$data_map$blocks<-rev(x$immutable$scientific_context$data_map$blocks);x},
 swapped_people=function(x){x$immutable$scientific_context$data_map$blocks$right$.person_key<-rev(x$immutable$scientific_context$data_map$blocks$right$.person_key);x},
 wrong_block=function(x){x$immutable$scientific_context$data_map$blocks$right$.block[]<-"left";x},
 missing_operand=function(x){x$immutable$scientific_context$tables$observation_operands<-x$immutable$scientific_context$tables$observation_operands[-1,];x},
 duplicate_operand=function(x){x$immutable$scientific_context$tables$observation_operands$observation_id[2]<-x$immutable$scientific_context$tables$observation_operands$observation_id[1];x},
 changed_count=function(x){x$immutable$scientific_context$data_map$blocks$right$counts[1,1]<-99L;x},
 bad_roster=function(x){x$immutable$scientific_context$tables$person_roster$person_index[1]<-1000L;x},
 bad_signature=function(x){x$immutable$scientific_context$models$right$model$likelihood$symbol<-"arbitrary";x},
 callable=function(x){x$immutable$scientific_context$models$right$model$validate_data<-function(...)stop("callback");x})
hits<-0L
trip<-function(...){hits<<-hits+1L;stop("FORBIDDEN resource/source/backend access")}
extra<-c(".mp_sha256",".bh_posterior_ingest_csv",".bh_posterior_read",".bh_ppc_ingest_csv",".bh_m3cs_run")
extra<-extra[vapply(extra,exists,logical(1),envir=ns,inherits=FALSE)]
previous<-mget(extra,ns);for(n in extra)replace(n,trip)
tryCatch({
 for(label in names(mutations)) {
  bad<-mutations[[label]](fit)
  refuses(function()ppc_plan(bad,4,budget,"population"))
  refuses(function()assess_measurement_fit(bad,level="population",draws=4,seed=1,budget=budget))
  ok(hits==0L&&callback_hits==0L,paste(label,"refuses before source/resource/backend calls"))
 }
},finally=for(n in extra)replace(n,previous[[n]]))
cat("Saved joint M3-CS profile guards:",passed,"passed. No native or posterior work.\n")
})()
