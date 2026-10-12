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
    # Task-level targets have exactly zero person-effect maps. Only varying
    # target rows receive the nonzero synthetic common-factor loading.
    varying<-spec$tables$targets$target_index[spec$tables$targets$level=="subject_task"]
    for(j in varying)x[grep(paste0("^effective_loadings\\[",j,","),vars)]<-.45
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
for(law in c("iid","sample_centered","all_fixed")) {
  directory<-file.path(owned,law);dir.create(directory)
  original_resource<-file.path(directory,"original.stanfunctions");file.copy(resource,original_resource)
  m<-m3_cs_model(original_resource)
  parameters<-if(law=="all_fixed")list(a=list(fixed=.2,scale="predictor"),c=list(fixed=-.5,scale="predictor"),ra=list(fixed=.3),rc=list(fixed=.7)) else
    list(c="task",ra=list(fixed=.3),rc=list(fixed=.7))
  block<-function(data)observe(m,data,"person","task",fields=list(counts="counts",options="options"),units="count",parameters=parameters)
  other<-d[c(8,2,6,5,1,9,3),];rownames(other)<-NULL
  other$counts[2,1]<-5L
  formula<-if(law=="sample_centered")bmf(residual=list(targets=~cs$a[tasks]+other$a[tasks]),
    person=list(distribution="sample_centered",ids=people),
    augmentation=list(type="task_cholesky",targets=~cs$a[c("t2","t1","t3")]+other$a[c("t3","t2","t1")])) else if(law=="iid")
      bmf(G=~cs$a[tasks]+other$a[tasks],person="iid") else bmf()
  spec<-with_checks(function()bmeas(formula,list(cs=block(d),other=block(other)),subjects="shared",fit=FALSE))
  paths<-file.path(directory,paste0("source-",1:2,".csv"));write_csv(paths,spec)
  live<-cd_shell(spec,paths)
  fit<-portable_fit(live,context)
  ok(identical(fit$immutable$context_profile,"bmeas_joint_m3cs_context_v1"),"new export explicitly admits a joint producer profile")
  exe<-file.path(directory,"NOT-AN-EXECUTABLE.txt");writeLines("R-only engineering simulator binding",exe)
  simulator<-make_simulator(spec,exe)
  initial_fit<-serialize(fit,NULL);initial_live<-serialize(live[setdiff(names(live),"backend")],NULL)
  detached<-fit$immutable$scientific_context
  refuses(function()make_m3cs_simulator_code(detached),"E_PORTABLE_UNSUPPORTED")
  refuses(function()m3cs_simulator_data(detached,matrix(0,1,1)),"E_PORTABLE_UNSUPPORTED")
  refuses(function()m3cs_probabilities(detached,matrix(0,1,1),simulator),"E_PORTABLE_UNSUPPORTED")
  archive<-archive_fit(fit,file.path(directory,"archive"))
  adverse_live<-live;adverse_live$cache$posterior_propriety$status<-"proven_improper"
  adverse<-archive_fit(portable_fit(adverse_live,context),file.path(directory,"adverse"))
  population<-.bh_ppc_forward_plan(spec,"population",budget)
  if(law=="sample_centered")ok(population$geometry$N==4L&&identical(population$roster$person_id,people),"joint full unobserved-person roster")
  baselines<-list()
  set.seed(931);rng<-.Random.seed;previous_threads<-Sys.getenv("STAN_NUM_THREADS",unset=NA_character_)
  for(level in c("population","conditional")) {
    lp<-ppc_plan(live,4,budget,level);pp<-ppc_plan(fit,4,budget,level)
    ok(identical(lp$statistics,pp$statistics)&&identical(lp$law,pp$law)&&
      identical(lp$geometry,pp$geometry)&&identical(lp$roster,pp$roster)&&
      identical(lp$required_variables,pp$required_variables)&&identical(lp$latent_values,pp$latent_values)&&
      lp$simulated_values==4*(nrow(d)+nrow(other))*10&&identical(lp$simulated_values,pp$simulated_values),paste(law,level,"live/portable ten-cell plan parity"))
    ok(identical(pp$source$reader_execution,.bh_m3cs_joint_reader_capability())&&plain(pp$source$reader_execution),"plain reader-owned capability metadata")
    expected<-run(live,level,simulator);portable<-run(fit,level,simulator)
    ok(!is.null(expected$result$table)&&identical(expected$result$table,portable$result$table)&&
      identical(expected$result$replicate_statistics,portable$result$replicate_statistics)&&
      identical(expected$calls,portable$calls),paste(law,level,"deterministic controller parity"))
    ok(identical(expected$result$metadata$draw_keys,portable$result$metadata$draw_keys),"portable retains exact aligned posterior chain/iteration keys")
    ok(length(portable$calls)==4L&&all(vapply(portable$calls,function(z)z$data$O==nrow(d)+nrow(other),logical(1))),"one stacked rng_v2 call per replicate")
    ok(identical(expected$result$metadata$stan_seeds,portable$result$metadata$stan_seeds)&&
      !is.null(portable$result$metadata$reader_execution),"seed replay and current reader execution receipt")
    ok(identical(portable$fit$immutable,fit$immutable)&&identical(portable$fit$identity,fit$identity)&&
      identical(cache(portable$fit)$computation,fit$cache$computation)&&
      identical(cache(portable$fit)$posterior_propriety,fit$cache$posterior_propriety),"cache update preserves identity and adverse history")
    for(call in portable$calls)stopifnot(identical(call$data$options,unname(do.call(rbind,lapply(spec$data_map$blocks,`[[`,"options")))),
      identical(as.numeric(call$data$totals),as.numeric(unlist(lapply(spec$data_map$blocks,function(z)rowSums(z$counts)),use.names=FALSE))),
      all(call$data$arguments[,3]==qlogis(.3)),all(call$data$arguments[,4]==qlogis(.7)))
    ok(portable$result$metadata$actual_work$simulated_values==4*(nrow(d)+nrow(other))*10&&
      portable$result$metadata$actual_work$latent_values==pp$latent_values,"actual work retains ten cells and full latent roster")
    if(level=="conditional")ok(identical(cache(portable$fit)$measurement_structure_fit$status,"not_assessed"),"conditional PPC does not claim population structure assessment")
    baselines[[level]]<-expected
  }
  ok(identical(serialize(live[setdiff(names(live),"backend")],NULL),initial_live)&&identical(serialize(fit,NULL),initial_fit),"caller saved fit fields remain byte-identical")
  # Relocation uses an explicitly supplied current binding, never a rewritten saved declaration.
  relocated_resource<-file.path(directory,"current.stanfunctions");file.copy(original_resource,relocated_resource)
  simulator$resource_file<-relocated_resource
  stopifnot(file.rename(archive$root,file.path(directory,"moved")));archive$root<-file.path(directory,"moved")
  unlink(c(paths,original_resource))
  files<-list.files(archive$root,recursive=TRUE,full.names=TRUE);hashes<-tools::md5sum(files)
  bytes<-serialize(archive,NULL)
  for(level in c("population","conditional")) {
    plan<-ppc_plan(archive,4,budget,level)
    ok(!is.null(plan$source$reader_execution)&&identical(plan$source$archive_context$status,"reopened_and_verified"),"relocated passive plan needs no historical resource")
    result<-run(archive,level,simulator);expected<-baselines[[level]]
    ok(identical(result$result$table,expected$result$table)&&
      identical(result$result$replicate_statistics,expected$result$replicate_statistics)&&
      identical(result$calls,expected$calls),paste(law,level,"relocated archive parity"))
    ok(identical(result$fit$report_snapshot$portable$immutable,fit$immutable)&&
      identical(result$result$metadata$simulator$current_resource_binding$file,relocated_resource),
      "current resource receipt is separate from immutable historical declaration")
  }
  ok(identical(.Random.seed,rng)&&identical(Sys.getenv("STAN_NUM_THREADS",unset=NA_character_),previous_threads),"caller RNG and thread setting preserved")
  ok(identical(serialize(archive,NULL),bytes)&&identical(tools::md5sum(files),hashes),"archive handle and owned bytes unchanged")
  refuses(function()assess_measurement_fit(archive,level="population",draws=4,seed=1,budget=budget),"E_SIMULATOR_REQUIRED")
  refuses(function()ppc_plan(adverse,4,budget,"population"))
  refuses(function()run(adverse,"population",simulator))
  small<-budget;small$max_simulated_values<-4*(nrow(d)+nrow(other))*10-1L
  refuses(function()ppc_plan(archive,4,small,"conditional"),"E_DIAGNOSTIC_BUDGET")
  small<-budget;small$posterior$max_source_bytes<-1
  failed<-run(archive,"population",simulator,small)
  ok(identical(cache(failed$fit)$predictive_fit$status,"failed")&&length(failed$calls)==0L,"source budget fails before simulator execution")
  exe_bytes<-readBin(exe,"raw",n=file.info(exe)$size)
  cat("altered inert executable bytes",file=exe,append=TRUE);calls<-list()
  tryCatch(refuses(function()run(archive,"population",simulator),"E_SIMULATOR_VERSION"),
    finally=writeBin(exe_bytes,exe))
  ok(length(calls)==0L&&identical(.mp_sha256(exe),simulator$executable_hash)&&
    identical(serialize(archive,NULL),bytes),"actual executable byte change refused before sampling without capsule mutation")
  for(field in c("version","code","code_hash","resource_hash","executable_hash")) {
    bad<-simulator;bad[[field]]<-"incompatible";calls<-list()
    refuses(function()run(archive,"population",bad));stopifnot(length(calls)==0L)
  }
  missing<-simulator;missing$resource_file<-file.path(directory,"missing-resource")
  refuses(function()run(archive,"population",missing))
  changed<-file.path(directory,"changed.stanfunctions");writeLines("changed",changed)
  wrong<-simulator;wrong$resource_file<-changed
  refuses(function()run(archive,"population",wrong))
  if(law=="iid") {
    sample_hits<-0L;broken<-simulator;original_sample<-simulator$model$sample
    broken$model$sample<-function(...) {
      sample_hits<<-sample_hits+1L
      if(sample_hits==2L)stop("Synthetic simulator failure; no native process")
      original_sample(...)
    }
    failed<-run(archive,"population",broken)
    ok(identical(cache(failed$fit)$predictive_fit$status,"failed")&&
      identical(failed$result$metadata$completed_replications,1L)&&
      is.null(failed$result$table)&&is.null(failed$result$replicate_statistics)&&
      identical(.Random.seed,rng)&&identical(failed$fit$report_snapshot$portable$immutable,fit$immutable),
      "simulator failure retains honest partial receipt and restores RNG")
    csv<-file.path(archive$root,"draws/000001.csv");csv_bytes<-readBin(csv,"raw",n=file.info(csv)$size)
    cat("corrupt",file=csv,append=TRUE)
    tryCatch({calls<-list();refuses(function()run(archive,"population",simulator));stopifnot(length(calls)==0L)},
      finally=writeBin(csv_bytes,csv))
    original_read<-get(".bh_archive_read",ns)
    replace(".bh_archive_read",function(...) {
      out<-original_read(...);cat("late mutation",file=file.path(out$handle$root,"draws/000001.csv"),append=TRUE);out
    })
    late<-tryCatch(run(archive,"population",simulator),finally={replace(".bh_archive_read",original_read);writeBin(csv_bytes,csv)})
    ok(identical(cache(late$fit)$predictive_fit$status,"failed")&&length(late$calls)==0L&&
      identical(.Random.seed,rng)&&identical(tools::md5sum(files),hashes),
      "archive byte freshness and post-reopen mutation pin enforced before simulation")
  }
  cat("PASS adverse evidence, budgets and explicit source/resource/executable binding refusals\n")
}
cat("Saved joint M3-CS R-only engineering controls:",passed,"passed. No native compilation, execution or HMC.\n")
})()
