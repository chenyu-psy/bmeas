# Narrow built-in CS contract. Planning compares saved values only and never
# reads the external resource or executes a model callback.
# Classification is passive inspection, including before the sealed-spec check.
# Strip container classes locally before every accessor/subset/lapply; never
# mutate or reseal the original object. Non-list containers cannot match.
.bh_m3cs_plain <- function(x) {
  if(typeof(x)!="list"||isS4(x))return(list())
  unclass(x)
}
.bh_m3cs_signature <- function(model) {
  m<-.bh_m3cs_plain(model);adapter<-.bh_m3cs_plain(m$adapter)
  list(schema=lapply(.bh_m3cs_plain(m$data_schema),function(f).bh_m3cs_plain(f)[c("type","role","lower","upper","width")]),
    parameters=lapply(.bh_m3cs_plain(m$parameters),function(p).bh_m3cs_plain(p)[c("support","predictor","receives","levels","units","predictor_units")]),
    density=.bh_m3cs_plain(m$likelihood)[c("symbol","arguments","argument_types","normalized","censoring","truncation")],
    constants=m$constants,backend=m$backend,
    adapter=adapter[c("version","mode","selector","routes","independence_unit","parameter_binding","function_code","wrapper_hash")],
    exports=adapter$function_exports,
    resources=lapply(.bh_m3cs_plain(adapter$resources),function(r).bh_m3cs_plain(r)[c("sha256","exports","cache_name")]))
}
.bh_m3cs_canonical_signature <- function() list(schema = list(counts = list(type = "nonnegative_integer", 
    role = "response", lower = -Inf, upper = Inf, width = 10L), 
    options = list(type = "nonnegative_integer", role = "auxiliary", 
        lower = -Inf, upper = Inf, width = 10L)), parameters = list(
    a = list(support = "positive", predictor = "log", receives = "eta", 
        levels = "subject_task", units = "activation", predictor_units = "log_activation"), 
    c = list(support = "positive", predictor = "log", receives = "eta", 
        levels = "subject_task", units = "activation", predictor_units = "log_activation"), 
    ra = list(support = "unit_interval", predictor = "logit", 
        receives = "eta", levels = "subject_task", units = "probability", 
        predictor_units = "log_odds"), rc = list(support = "unit_interval", 
        predictor = "logit", receives = "eta", levels = "subject_task", 
        units = "probability", predictor_units = "log_odds")), 
    density = list(symbol = "measurement_m3_cs_lpmf", arguments = c("counts", 
    "options", "a", "c", "ra", "rc"), argument_types = c("int_array", 
    "int_array", "real", "real", "real", "real"), normalized = TRUE, 
        censoring = "none", truncation = "none"), constants = list(), 
    backend = "registered_stan", adapter = structure(list("typed_row_adapter_v1", 
        NULL, NULL, NULL, "one explicit observation row; arrays remain one joint observation", 
        "same task for every declared parameter; cross-task references are not implemented", 
        c("real measurement_m3_cs_lpmf(array[] int counts, array[] int options, real a, real c, real ra, real rc) {", 
        "  array[5] int y = {counts[1],counts[2],counts[3],counts[4],counts[10]};", 
        "  vector[10] log_options;", "  for(k in 1:10) log_options[k] = options[k]>0 ? log(options[k]) : 0;", 
        "  return multinomial_logit_lpmf(y | cs_logits(a,c,ra,rc,log_options));", 
        "}"), "d5b7dd069d872aa66e0239be94cbbc2a"), names = c("version", 
    NA, NA, NA, "independence_unit", "parameter_binding", "function_code", 
    "wrapper_hash")), exports = "measurement_m3_cs_lpmf", resources = list(
        list(sha256 = "4b070861489b188e63575c4e0a8dae18cd54d01afd8f1d56a804714061fdc4d1", 
            exports = c("ac_task", "shifted_activation", "shifted_zero_activation", 
            "cs_logits", "mu_no_update_logits", "mu_update_logits", 
            "partial_m3_cs", "partial_m3_mu_no_update", "partial_m3_mu_update"
            ), cache_name = "external_4b070861489b188e63575c4e0a8dae18cd54d01afd8f1d56a804714061fdc4d1.stanfunctions")))
.bh_m3cs_kind <- function(model) {
  if(typeof(model)!="list")return(FALSE)
  actual <- .bh_m3cs_signature(model)
  expected <- .bh_m3cs_canonical_signature()
  if (identical(actual, expected)) return(TRUE)
  # The identical wrapper has this saveRDS(version=2) hash in the verified
  # R 4.5.2 Mac runtime. Compare a second exact passive signature only: no
  # hashing, callback execution, saved-value mutation or historical resealing.
  expected$adapter$wrapper_hash <- "db7e78458a367eb1b1a150c843a0e015"
  identical(actual, expected)
}
.bh_m3cs_spec <- function(spec) {
  if(typeof(spec)!="list"||isS4(spec))return(FALSE)
  s<-.bh_m3cs_plain(spec);models<-.bh_m3cs_plain(s$models)
  length(models)==1L&&.bh_m3cs_kind(.bh_m3cs_plain(models[[1L]])$model)
}
# The historical single-block reader predicate above is intentionally unchanged.
# Joint saved execution additionally requires the distinct joint producer
# profile and current validation receipt; no detached context becomes live.
.bh_m3cs_joint_spec <- function(spec) {
  if(typeof(spec)!="list"||isS4(spec))return(FALSE)
  models<-.bh_m3cs_plain(.bh_m3cs_plain(spec)$models)
  length(models)==2L&&all(vapply(models,function(b)
    .bh_m3cs_kind(.bh_m3cs_plain(b)$model),logical(1)))
}
.bh_m3cs_predictive_spec <- function(spec)
  .bh_m3cs_spec(spec)||.bh_m3cs_joint_spec(spec)
.bh_m3cs_counts <- function(counts,options,n,totals=NULL) {
  for(x in list(counts,options))if(!is.matrix(x)||!typeof(x)%in%c("integer","double")||
    !identical(dim(x),c(as.integer(n),10L))||any(!is.finite(x))||any(x<0|x!=floor(x)|x>.Machine$integer.max))
    .mp_abort("E_PREDICTION_SUPPORT","CS responses/options require nonnegative integer O-by-10 matrices")
  if(any(counts[,5:9,drop=FALSE]!=0)||any(options[,c(1:4,10),drop=FALSE]<=0)||
     any(rowSums(counts)>.Machine$integer.max)||(!is.null(totals)&&any(rowSums(counts)!=totals)))
    .mp_abort("E_PREDICTION_SUPPORT","CS inactive cells, positive active options or fixed totals differ")
  invisible(TRUE)
}
# Reader-owned capability, deliberately separate from the immutable producer
# profile. It grants no compilation, refitting or historical origin claim.
.bh_m3cs_reader_capability <- function() list(
  version="bmeas_saved_m3cs_reader_v1",family="single_pinned_m3cs",
  levels=c("population","conditional"),simulator_version="bmeas_m3cs_rng_v2",
  execution="explicit_simulator_only",resource_binding="current_path_and_pinned_sha256",
  producer_profile="unchanged",historical_paths_and_seals="unchanged",
  qualification="R_only_candidate_native_saved_context_not_qualified")
.bh_m3cs_reader_capability_check <- function(x) {
  if(!identical(x,.bh_m3cs_reader_capability()))
    .mp_abort("E_PORTABLE_UNSUPPORTED","unsupported saved M3-CS reader execution capability")
  invisible(TRUE)
}
.bh_m3cs_contract <- function(spec,budget=NULL,reader=NULL,check_resource=TRUE) {
  if(is.null(budget))budget<-.bh_ppc_budget_defaults()
  detached<-identical(.bh_pf_class(spec),"bmeas_portable_context")
  if(detached&&.bh_m3cs_joint_spec(spec)&&
     (is.null(reader)||!identical(.bh_pf_get(reader,"context_profile"),"bmeas_joint_m3cs_context_v1")))
    .mp_abort("E_PORTABLE_UNSUPPORTED","two-block M3-CS prediction requires a live fit; saved two-block execution is not supported")
  if(detached&&!is.null(reader)) {
    .bh_pf_context_receipt_schema(reader)
    if(!identical(reader$operation,"assess_measurement_fit"))
      .mp_abort("E_PORTABLE_IDENTITY","M3-CS requires the current assessment validation receipt")
    .bh_pf_plain_tree(unclass(spec),"spec",limit=reader$validation_policy$max_depth)
    .bh_pf_spec(spec,reader$validation_policy,detached=TRUE)
  }else {
    .bh_pf_refuse_execution(spec,"M3-CS simulation")
    if(!inherits(spec,"bhmm_spec")||!.bh_m3cs_predictive_spec(spec))
      .mp_abort("E_CHECK_UNSUPPORTED","requires one or exactly two pinned main-API M3-CS blocks")
    .bh_ppc_validate_saved_spec(spec)
  }
  if(!.bh_m3cs_predictive_spec(spec))
    .mp_abort("E_CHECK_UNSUPPORTED","requires one or exactly two pinned main-API M3-CS blocks")
  .bh_ppc_forward_plan(spec,"conditional",budget,saved_joint=detached&&!is.null(reader))
  resource<-spec$models[[1L]]$model$adapter$resources[[1L]]
  if(check_resource)for(block in spec$models) {
    r<-block$model$adapter$resources[[1L]]
    if(!identical(.mp_sha256(r$file),r$sha256))
      .mp_abort("E_RESOURCE_HASH","M3-CS resource changed")
  }
  resource
}
make_m3cs_simulator_code <- function(spec,file=NULL,budget=NULL) {
  resource<-.bh_m3cs_contract(spec,budget)
  code<-.bh_m3cs_code(resource)
  if(!is.null(file)){.mp_scalar_string(file,"simulator destination");writeLines(code,file)}
  code
}
# This text generator is pure and shared by live and saved readers. Resource
# verification belongs to the caller, after all detached/map/support checks.
.bh_m3cs_code <- function(resource) {
  code<-paste(c("// Main-API M3-CS simulator; original external logits are unchanged.",
    "functions {",paste0("#include ",resource$cache_name),"}",
    "data { int<lower=1> O; array[O,10] int<lower=0> options;",
    "array[O] int<lower=0> totals; matrix[O,4] arguments; int<lower=0,upper=1> draw_response; }",
    "generated quantities { matrix[O,10] probabilities=rep_matrix(0.0,O,10);",
    "array[O,10] int counts_rep=rep_array(0,O,10);",
    "for(o in 1:O) { array[5] int active={1,2,3,4,10}; vector[10] log_options; vector[5] p;",
    "for(k in 1:10)log_options[k]=options[o,k]>0 ? log(options[o,k]) : 0;",
    "p=softmax(cs_logits(arguments[o,1],arguments[o,2],arguments[o,3],arguments[o,4],log_options));",
    "for(k in 1:5)probabilities[o,active[k]]=p[k];",
    "if(draw_response==1) { array[5] int y=multinomial_rng(p,totals[o]); for(k in 1:5)counts_rep[o,active[k]]=y[k]; }",
    "} }"),collapse="\n")
  code
}
compile_m3cs_simulator <- function(spec,cache_dir,budget=NULL) {
  .bh_m3cs_contract(spec,budget);.mp_scalar_string(cache_dir,"cache_dir")
  if(!requireNamespace("cmdstanr",quietly=TRUE)) .mp_abort("E_BACKEND_MISSING","installed CmdStanR/CmdStan required")
  code<-make_m3cs_simulator_code(spec,budget=budget);resource<-spec$models[[1L]]$model$adapter$resources[[1L]]
  options<-list(stan_threads=FALSE,O=0);stanc<-.mp_stanc_options()
  identity<-list(code=code,resource=resource,toolchain=.mp_toolchain_fingerprint(options,stanc),version="bmeas_m3cs_rng_v2")
  directory<-file.path(cache_dir,.mp_hash(identity));dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  file<-file.path(directory,"m3cs_simulator.stan");changed<-.mp_sync_stan_source(code,file)
  target<-file.path(directory,resource$cache_name)
  if(!file.exists(target)||.mp_sha256(target)!=resource$sha256){
    if(!file.copy(resource$file,target,overwrite=TRUE)||.mp_sha256(target)!=resource$sha256) .mp_abort("E_RESOURCE_HASH","simulator resource copy failed")
    changed<-TRUE
  }
  manifest<-file.path(directory,"manifest.rds")
  force<-changed||!file.exists(manifest)||!identical(readRDS(manifest),identity)
  model<-cmdstanr::cmdstan_model(file,cpp_options=options,stanc_options=stanc,force_recompile=force)
  saveRDS(identity,manifest)
  structure(list(model=model,code=code,code_hash=.mp_hash(code),resource_hash=resource$sha256,resource_file=resource$file,
    executable_hash=.mp_sha256(model$exe_file()),toolchain=identity$toolchain,version=identity$version),class="bmeas_m3cs_simulator")
}
.bh_m3cs_simulator_record <- function(simulator) {
  if(typeof(simulator)!="list"||isS4(simulator)||
     !identical(.bh_pf_class(simulator),"bmeas_m3cs_simulator"))
    .mp_abort("E_SIMULATOR_REQUIRED","explicit compile_m3cs_simulator result required; assessment never compiles")
  .bh_pf_record(simulator,"M3-CS simulator",
    c("model","code","code_hash","resource_hash","resource_file","executable_hash","toolchain","version"),
    c("model","code","code_hash","resource_hash","executable_hash","toolchain","version"),"bmeas_m3cs_simulator")
}
.bh_m3cs_simulator_check <- function(spec,simulator,budget=NULL,reader=NULL) {
  # Schema/operand/support admission precedes even inspecting the explicitly
  # supplied executable capability. This is separate from resource binding.
  resource<-.bh_m3cs_contract(spec,budget,reader,check_resource=FALSE)
  simulator<-.bh_m3cs_simulator_record(simulator)
  code<-.bh_m3cs_code(resource)
  if(!identical(simulator$version,"bmeas_m3cs_rng_v2")||!identical(simulator$code,code)||
     !identical(simulator$code_hash,.mp_hash(code))||!identical(simulator$resource_hash,resource$sha256))
    .mp_abort("E_SIMULATOR_VERSION","M3-CS source/resource binding differs")
  current<-if(is.null(simulator$resource_file))resource$file else simulator$resource_file
  .bh_pf_string(current,"current M3-CS resource file")
  if(!identical(.mp_sha256(current),resource$sha256)) .mp_abort("E_RESOURCE_HASH","M3-CS current resource binding changed")
  .bh_pf_string(simulator$executable_hash,"M3-CS executable hash")
  if(!grepl("^[0-9a-f]{64}$",simulator$executable_hash))
    .mp_abort("E_SIMULATOR_VERSION","M3-CS executable digest is malformed")
  if(!identical(simulator$executable_hash,.mp_sha256(simulator$model$exe_file())))
    .mp_abort("E_SIMULATOR_VERSION","M3-CS executable binding differs")
  invisible(TRUE)
}
# Direct data inspection adds a map only for the new joint live route; the
# private Stan payload and every historical single-block return stay unchanged.
m3cs_simulator_data <- function(spec,eta,draw_response=TRUE,budget=NULL) {
  data<-.bh_m3cs_data(spec,eta,draw_response,budget)
  if(.bh_m3cs_joint_spec(spec))attr(data,"row_map")<-.bh_m3cs_row_map(spec)
  data
}
.bh_m3cs_row_map <- function(spec) {
  offset<-0L
  rows<-lapply(names(spec$data_map$blocks),function(b) {
    d<-spec$data_map$blocks[[b]];n<-nrow(d);at<-offset+seq_len(n);offset<<-offset+n
    data.frame(simulator_row=at,block=rep(b,n),block_row=seq_len(n),
      observation_id=d$.observation_id,person_key=d$.person_key,
      person_index=d$.person_index,task=d$.task,stringsAsFactors=FALSE)
  })
  out<-do.call(rbind,rows);rownames(out)<-NULL;out
}
.bh_m3cs_split <- function(spec,values) {
  offset<-0L
  lapply(spec$data_map$blocks,function(d) {
    n<-nrow(d);at<-offset+seq_len(n);offset<<-offset+n
    x<-values[at,,drop=FALSE];rownames(x)<-d$.observation_id;x
  })
}
.bh_m3cs_data <- function(spec,eta,draw_response=TRUE,budget=NULL,reader=NULL) {
  .bh_m3cs_contract(spec,budget,reader,check_resource=is.null(reader))
  forward<-.bh_ppc_forward_plan(spec,"conditional",if(is.null(budget)).bh_ppc_budget_defaults()else budget,
    saved_joint=!is.null(reader)&&identical(.bh_pf_get(reader,"context_profile"),"bmeas_joint_m3cs_context_v1"))
  if(!is.matrix(eta)||!is.numeric(eta)||!identical(dim(eta),c(forward$geometry$N,forward$geometry$J)))
    .mp_abort("E_PPC_DRAW","eta must retain the full roster-by-target shape; required cells must be finite")
  if(!is.logical(draw_response)||length(draw_response)!=1L||is.na(draw_response)) .mp_abort("E_ARGUMENT","draw_response must be a scalar logical")
  if(length(forward$blocks)==1L) {
  block<-forward$blocks[[1L]];d<-block$rows
  if(!nrow(d)) .mp_abort("E_PPC_NO_OBSERVATIONS","no consumed CS rows")
  arguments<-matrix(0,nrow(d),4L,dimnames=list(d$.observation_id,c("a","c","ra","rc")))
  for(a in names(block$operands))arguments[,a]<-.bh_ppc_forward_operand(block$operands[[a]],eta,numeric())
  return(list(O=nrow(d),options=unname(d$options),totals=array(as.integer(rowSums(d$counts)),dim=nrow(d)),
    arguments=unname(arguments),draw_response=as.integer(draw_response)))
  }
  # One concatenated response call, with one shared full-roster eta. Never
  # regenerate latent people per block. Each row gets a separate kernel RNG draw.
  pieces<-lapply(forward$blocks,function(block) {
    d<-block$rows
    arguments<-matrix(0,nrow(d),4L,dimnames=list(d$.observation_id,c("a","c","ra","rc")))
    for(a in names(block$operands))arguments[,a]<-.bh_ppc_forward_operand(block$operands[[a]],eta,numeric())
    list(arguments=arguments,options=d$options,totals=as.integer(rowSums(d$counts)))
  })
  O<-sum(vapply(pieces,function(x)nrow(x$arguments),integer(1)))
  if(!O) .mp_abort("E_PPC_NO_OBSERVATIONS","no consumed CS rows")
  list(O=O,options=unname(do.call(rbind,lapply(pieces,`[[`,"options"))),
    totals=array(unlist(lapply(pieces,`[[`,"totals"),use.names=FALSE),dim=O),
    arguments=unname(do.call(rbind,lapply(pieces,`[[`,"arguments"))),draw_response=as.integer(draw_response))
}
.bh_m3cs_run <- function(spec,eta,simulator,seed,draw_response=TRUE,budget=NULL,reader=NULL) {
  .bh_m3cs_simulator_check(spec,simulator,budget,reader)
  simulator<-.bh_m3cs_simulator_record(simulator)
  data<-.bh_m3cs_data(spec,eta,draw_response,budget,reader);seed<-.bh_ppc_plain_integer(seed,"seed")
  .bh_ppc_with_rng(seed,function() {
    previous_threads<-Sys.getenv("STAN_NUM_THREADS",unset=NA_character_)
    on.exit(if(is.na(previous_threads))Sys.unsetenv("STAN_NUM_THREADS")else Sys.setenv(STAN_NUM_THREADS=previous_threads),add=TRUE)
    Sys.setenv(STAN_NUM_THREADS="1")
    directory<-tempfile("bmeas-m3cs-");dir.create(directory);on.exit(unlink(directory,recursive=TRUE),add=TRUE)
    output<-simulator$model$sample(data=data,chains=1,parallel_chains=1,iter_warmup=0,iter_sampling=1,
      fixed_param=TRUE,seed=seed,output_dir=directory,refresh=0,sig_figs=17,show_messages=FALSE)
    if(any(output$return_codes()!=0L)) .mp_abort("E_PREDICTION_FAILED","M3-CS fixed_param execution failed")
    headers<-readLines(output$output_files()[1L],warn=FALSE)
    for(pattern in c("num_threads = 1([[:space:]]|$)","algorithm = fixed_param","num_warmup = 0([[:space:]]|$)","num_samples = 1([[:space:]]|$)"))
      if(!any(grepl(paste0("^#[[:space:]]+",pattern),headers))) .mp_abort("E_PREDICTION_FAILED","fixed_param execution receipt does not match the one-draw contract")
    extract<-function(name) {
      x<-output$draws(variables=name,format="draws_matrix")
      if(!is.matrix(x)||nrow(x)!=1L) .mp_abort("E_PREDICTION_SUPPORT","simulator output is not one matrix row")
      idx<-expand.grid(o=seq_len(data$O),k=1:10)
      keys<-paste0(name,"[",idx$o,",",idx$k,"]")
      if(any(!keys%in%colnames(x))) .mp_abort("E_PREDICTION_SUPPORT","simulator output has missing category cells")
      matrix(as.numeric(x[1L,keys]),data$O,10L)
    }
    p<-extract("probabilities");y<-if(draw_response)extract("counts_rep")else NULL
    if(any(!is.finite(p))||any(p<0)||any(abs(rowSums(p)-1)>1e-10)||any(p[,5:9,drop=FALSE]!=0))
      .mp_abort("E_PREDICTION_SUPPORT","invalid CS probability support")
    if(draw_response){.bh_m3cs_counts(y,data$options,data$O,as.vector(data$totals));if(any(y[p==0]!=0)) .mp_abort("E_PREDICTION_SUPPORT","counts in zero-probability cells")}
    ids<-unlist(lapply(spec$data_map$blocks,function(d)d$.observation_id),use.names=FALSE)
    dimnames(p)<-list(ids,paste0("category",1:10));if(draw_response)dimnames(y)<-dimnames(p)
    list(probabilities=p,counts=y,seed=seed,execution=list(algorithm="fixed_param",warmup=0L,draws=1L,parameters=0L,threads=1L))
  })
}
m3cs_probabilities <- function(spec,eta,simulator,budget=NULL) {
  p<-.bh_m3cs_run(spec,eta,simulator,1L,FALSE,budget)$probabilities
  if(.bh_m3cs_joint_spec(spec)).bh_m3cs_split(spec,p)else p
}
.bh_m3cs_simulate <- function(spec,forward,draw,simulator,seed,budget=NULL,reader=NULL) {
  eta<-.bh_ppc_generate_eta(forward,draw)
  result<-if(is.null(reader)).bh_m3cs_run(spec,eta,simulator,seed,TRUE,budget)else .bh_m3cs_run(spec,eta,simulator,seed,TRUE,budget,reader)
  if(length(forward$blocks)==1L)return(stats::setNames(list(result$counts),names(forward$blocks)))
  offset<-0L
  lapply(forward$blocks,function(block) {
    n<-nrow(block$rows);at<-offset+seq_len(n);offset<<-offset+n
    y<-result$counts[at,,drop=FALSE]
    rownames(y)<-block$rows$.observation_id
    y
  })
}
