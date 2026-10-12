.mp_prediction_rng_preflight <- function() {
  kind<-RNGkind()
  if(identical(kind[2L],"Box-Muller")||any(kind=="user-supplied"))
    .mp_abort("E_RNG_KIND","Box-Muller/user-supplied RNG kinds have hidden state that cannot be safely restored; choose a supported kind explicitly before prediction")
  invisible(TRUE)
}
# Full-reference M3 prediction calls external, hash-pinned probability functions.
# No translation or redistribution of the reference probability implementation.
.mp_m3_ppc_supported <- function(spec) {
  identical(spec$structure$type,"component_covariance") &&
    identical(spec$structure$reference_name,"csmu_exp1_memory_gpc_v1") &&
    length(spec$models)==1L && identical(spec$models[[1L]]$model$name,"m3_reference_full") &&
    identical(spec$models[[1L]]$model$version,"csmu_exp1_full_v1")
}
.mp_m3_prediction_contract <- function(spec) {
  check_measurement(spec)
  if(!.mp_m3_ppc_supported(spec)) .mp_abort("E_CHECK_UNSUPPORTED","M3 prediction requires the exact full Exp1 M3/component reference spec; SDM and other structures remain unsupported")
  model<-spec$models[[1L]]$model
  reference<-m3_model(model$adapter$resources[[1L]]$file)
  for(field in c("routes","wrapper_hash","selector"))if(!identical(model$adapter[[field]],reference$adapter[[field]]))
    .mp_abort("E_SIMULATOR_VERSION","the density routes/wrapper differ from the pinned M3 probability simulator")
  if(!identical(model$reference_parameter_hash,reference$reference_parameter_hash)) .mp_abort("E_SIMULATOR_VERSION","M3 received parameter scales differ")
  N<-length(spec$data_map$subject_keys);O<-nrow(spec$data_map$blocks[[1L]])
  b<-reference_run_budget("csmu_exp1_memory_gpc_v1")
  if(N>b$max_subjects||O>b$max_observations||length(spec$structure$keys)!=b$max_parameters||spec$structure$common_total!=b$max_components)
    .mp_abort("E_DIAGNOSTIC_BUDGET","M3 prediction retains the bounded N<=8,O<=72,J27,K15 reference scope")
  invisible(TRUE)
}
make_m3_simulator_code <- function(spec, file=NULL) {
  .mp_m3_prediction_contract(spec)
  resource<-spec$models[[1L]]$model$adapter$resources[[1L]]
  code<-c("// Original prediction wrapper; reference logits remain external and hash-pinned.",
    "functions {",paste0("#include ",resource$cache_name),"}",
    "data {","int<lower=1,upper=72> O;","array[O] int<lower=1,upper=3> route;",
    "array[O,10] int<lower=0> options;","array[O] int<lower=0> totals;","matrix[O,6] arguments;",
    "int<lower=0,upper=1> draw_response;","}","generated quantities {",
    "matrix[O,10] probabilities=rep_matrix(0.0,O,10);","array[O,10] int counts_rep=rep_array(0,O,10);",
    "for(o in 1:O) {","vector[10] log_options;",
    "for(k in 1:10)log_options[k]=options[o,k]>0 ? log(options[o,k]) : 0;",
    "if(route[o]==1) {","array[5] int active={1,2,3,4,10};",
    "vector[5] p=softmax(cs_logits(arguments[o,1],arguments[o,2],arguments[o,3],arguments[o,4],log_options));",
    "for(k in 1:5)probabilities[o,active[k]]=p[k];",
    "if(draw_response==1) { array[5] int y=multinomial_rng(p,totals[o]); for(k in 1:5)counts_rep[o,active[k]]=y[k]; }","}",
    "else if(route[o]==2) {","array[5] int active={5,6,7,9,10};",
    "vector[5] p=softmax(mu_no_update_logits(arguments[o,1],arguments[o,2],arguments[o,3],arguments[o,4],log_options));",
    "for(k in 1:5)probabilities[o,active[k]]=p[k];",
    "if(draw_response==1) { array[5] int y=multinomial_rng(p,totals[o]); for(k in 1:5)counts_rep[o,active[k]]=y[k]; }","}",
    "else {","array[6] int active={5,6,7,8,9,10};",
    "vector[6] p=softmax(mu_update_logits(arguments[o,1],arguments[o,2],arguments[o,3],arguments[o,4],arguments[o,5],arguments[o,6],log_options));",
    "for(k in 1:6)probabilities[o,active[k]]=p[k];",
    "if(draw_response==1) { array[6] int y=multinomial_rng(p,totals[o]); for(k in 1:6)counts_rep[o,active[k]]=y[k]; }","}","}","}")
  if(!is.null(file)){.mp_scalar_string(file,"simulator destination");writeLines(code,file)}
  paste(code,collapse="\n")
}
compile_m3_simulator <- function(spec, cache_dir) {
  .mp_m3_prediction_contract(spec);.mp_scalar_string(cache_dir,"cache_dir")
  if(!requireNamespace("cmdstanr",quietly=TRUE)) .mp_abort("E_BACKEND_MISSING","CmdStanR/CmdStan required for explicit M3 simulation")
  code<-make_m3_simulator_code(spec);resource<-spec$models[[1L]]$model$adapter$resources[[1L]]
  options<-list(stan_threads=FALSE,O=0);stanc<-.mp_stanc_options()
  identity<-list(code=code,resource=resource,toolchain=.mp_toolchain_fingerprint(options,stanc),version="m3_probability_rng_v1")
  directory<-file.path(cache_dir,.mp_hash(identity));dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  file<-file.path(directory,"m3_simulator.stan");changed<-.mp_sync_stan_source(code,file)
  target<-file.path(directory,resource$cache_name)
  if(!file.exists(target)||.mp_sha256(target)!=resource$sha256){if(!file.copy(resource$file,target,overwrite=TRUE)||.mp_sha256(target)!=resource$sha256) .mp_abort("E_RESOURCE_HASH","simulator resource copy failed");changed<-TRUE}
  manifest<-file.path(directory,"manifest.rds");force<-changed||!file.exists(manifest)||!identical(readRDS(manifest),identity)
  model<-cmdstanr::cmdstan_model(file,cpp_options=options,stanc_options=stanc,force_recompile=force)
  saveRDS(identity,manifest)
  structure(list(model=model,code=code,code_hash=.mp_hash(code),resource_hash=resource$sha256,
    executable_hash=.mp_sha256(model$exe_file()),toolchain=identity$toolchain,version=identity$version),class="measurement_m3_simulator")
}
m3_simulator_data <- function(spec, eta, draw_response=TRUE) {
  .mp_m3_prediction_contract(spec)
  N<-length(spec$data_map$subject_keys);J<-length(spec$structure$keys)
  if(!is.matrix(eta)||!is.numeric(eta)||!identical(dim(eta),c(as.integer(N),as.integer(J)))||any(!is.finite(eta))) .mp_abort("E_COMPONENT_STATE","eta must be a finite N-by-J predictor matrix in structure key order")
  if(!is.logical(draw_response)||length(draw_response)!=1L||is.na(draw_response)) .mp_abort("E_ARGUMENT","draw_response must be a scalar logical")
  b<-names(spec$models)[1L];block<-spec$models[[1L]];d<-spec$data_map$blocks[[1L]]
  bindings<-.mp_resolve_bindings(block,d,b);arguments<-matrix(0,nrow(d),6)
  formal<-list(CS=c("a","c","ra","rc"),MU_no=c("a1","a2","c1","ra_fixed"),MU_yes=c("a1","a2","c2","c1","ra_fixed","rc"))
  for(o in seq_len(nrow(d))) {
    rn<-names(formal)[d$route[o]];rows<-bindings[bindings$observation_id==d$.observation_id[o],,drop=FALSE]
    rows<-rows[match(formal[[rn]],rows$argument),,drop=FALSE]
    if(anyNA(rows$argument)) .mp_abort("E_PARAMETER_BINDING","M3 simulator formal binding missing")
    for(a in seq_len(nrow(rows)))arguments[o,a]<-if(rows$kind[a]=="fixed")rows$fixed_value[a]else eta[d$.person_index[o],match(rows$target_key[a],spec$structure$keys)]
  }
  totals<-rowSums(d$counts)
  if(any(totals>.Machine$integer.max)) .mp_abort("E_PREDICTION_SUPPORT","row total exceeds Stan multinomial integer support")
  list(O=nrow(d),route=array(as.integer(d$route),dim=nrow(d)),options=d$options,
       totals=array(as.integer(totals),dim=nrow(d)),arguments=arguments,draw_response=as.integer(draw_response))
}
.mp_run_m3_simulator <- function(spec,eta,simulator,seed,draw_response) {
  .bh_pf_refuse_execution(spec,"M3 simulation or probabilities")
  .mp_prediction_rng_preflight()
  data<-m3_simulator_data(spec,eta,draw_response);seed<-.mp_integer(seed,"seed")
  if(!inherits(simulator,"measurement_m3_simulator")) .mp_abort("E_SIMULATOR_REQUIRED","explicit compile_m3_simulator result required; PPC never compiles implicitly")
  code<-make_m3_simulator_code(spec);resource<-spec$models[[1L]]$model$adapter$resources[[1L]]
  if(!identical(simulator$code_hash,.mp_hash(code))||!identical(simulator$resource_hash,resource$sha256)||!identical(simulator$executable_hash,.mp_sha256(simulator$model$exe_file()))) .mp_abort("E_SIMULATOR_VERSION","simulator code/resource/executable identity differs")
  had_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE);old_kind<-RNGkind()
  if(had_seed)old_seed<-get(".Random.seed",envir=.GlobalEnv)
  on.exit({do.call(RNGkind,as.list(old_kind));if(had_seed)assign(".Random.seed",old_seed,envir=.GlobalEnv)else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)},add=TRUE)
  previous_threads<-Sys.getenv("STAN_NUM_THREADS",unset=NA_character_)
  on.exit(if(is.na(previous_threads))Sys.unsetenv("STAN_NUM_THREADS")else Sys.setenv(STAN_NUM_THREADS=previous_threads),add=TRUE)
  Sys.setenv(STAN_NUM_THREADS="1")
  directory<-tempfile("measurement-m3-predict-");dir.create(directory);on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  output<-simulator$model$sample(data=data,chains=1,parallel_chains=1,iter_warmup=0,iter_sampling=1,
    fixed_param=TRUE,seed=seed,output_dir=directory,refresh=0,sig_figs=17,show_messages=FALSE)
  if(any(output$return_codes()!=0L)) .mp_abort("E_PREDICTION_FAILED","fixed-parameter M3 simulator failed")
  headers<-readLines(output$output_files()[1L],warn=FALSE)
  if(!any(grepl("^#[[:space:]]+num_threads = 1([[:space:]]|$)",headers))||!any(grepl("^#[[:space:]]+algorithm = fixed_param",headers))||!any(grepl("^#[[:space:]]+num_warmup = 0",headers))||!any(grepl("^#[[:space:]]+num_samples = 1([[:space:]]|$)",headers))) .mp_abort("E_PREDICTION_FAILED","simulator did not execute exactly one fixed-parameter draw with zero warmup")
  execution<-list(algorithm="fixed_param",warmup=0L,draws=1L,parameters=0L,threads=1L,header=headers[grepl("^#[[:space:]]+(algorithm|num_warmup|num_samples|num_threads) =",headers)])
  extract<-function(name){x<-as.matrix(output$draws(variables=name,format="draws_matrix"));class(x)<-c("matrix","array");v<-stats::setNames(as.numeric(x[1,,drop=FALSE]),colnames(x));idx<-expand.grid(o=seq_len(data$O),k=1:10);matrix(v[paste0(name,"[",idx$o,",",idx$k,"]")],data$O,10)}
  p<-extract("probabilities");y<-if(draw_response)extract("counts_rep")else NULL
  active<-list(c(1:4,10),c(5:7,9:10),5:10)
  if(any(!is.finite(p))||any(p<0)||max(abs(rowSums(p)-1))>1e-10) .mp_abort("E_PREDICTION_SUPPORT","nonfinite/non-normalized M3 probabilities")
  for(o in seq_len(data$O))if(any(p[o,-active[[data$route[o]]]]!=0)) .mp_abort("E_PREDICTION_SUPPORT","inactive M3 categories have positive probability")
  if(draw_response&&(any(!is.finite(y))||any(y<0)||any(y!=floor(y))||any(rowSums(y)!=as.vector(data$totals))||any(y[p==0]!=0))) .mp_abort("E_PREDICTION_SUPPORT","M3 replicate violates count support or fixed totals")
  list(probabilities=p,counts=y,seed=seed,execution=execution,method=if(draw_response)"external reference logits plus multinomial_rng"else"probabilities only; no responses generated")
}
m3_probabilities <- function(spec,eta,simulator) .mp_run_m3_simulator(spec,eta,simulator,1L,FALSE)$probabilities
simulate_m3_responses <- function(spec,eta,simulator,seed) .mp_run_m3_simulator(spec,eta,simulator,seed,TRUE)
.mp_m3_ppc_plan <- function(fit,draws,budget) {
  .mp_m3_prediction_contract(fit$spec)
  if(!inherits(budget,"measurement_diagnostic_budget")) .mp_abort("E_DIAGNOSTIC_BUDGET","explicit diagnostic_budget required")
  diagnostic_budget(budget$max_draws,budget$max_observations,budget$max_simulated_values)
  draws<-.mp_integer(draws,"draws",2L);O<-nrow(fit$spec$data_map$blocks[[1L]])
  if(draws>10L||draws>budget$max_draws||O>budget$max_observations||draws*O*10L>budget$max_simulated_values) .mp_abort("E_DIAGNOSTIC_BUDGET","M3 PPC requires<=10 reps and an explicit budget for every10-category response cell")
  list(replications=draws,observations_per_rep=O,categories_per_row=10L,simulated_values=draws*O*10L,
    allocation="one group and one response matrix at a time; aggregate discrepancies only",
    level="population",population="new centered same-N group with the original fixed design, not iid independent new people",screening_only=TRUE,
    covariance_statistics="36 correct-response-rate cross-task pairs at most; all active categories receive marginal mean/SD checks",
    simulator="explicit separately compiled pinned M3 logits/multinomial RNG; no implicit compilation",
    memory_limit="no process-memory hard cap; dimensions/reference scope and replicate/output counts are bounded")
}
.mp_m3_ppc_statistics <- function(spec,counts=NULL) {
  d<-spec$data_map$blocks[[1L]];b<-names(spec$models)[1L]
  if(is.null(counts))counts<-d$counts
  totals<-rowSums(d$counts);active<-list(c(1:4,10),c(5:7,9:10),5:10)
  if(!is.matrix(counts)||!identical(dim(counts),dim(d$counts))||any(!is.finite(counts))||any(counts<0)||any(counts!=floor(counts))||any(rowSums(counts)!=totals)) .mp_abort("E_PREDICTION_SUPPORT","counts must retain original dimensions and totals")
  for(o in seq_len(nrow(d)))if(any(counts[o,-active[[d$route[o]]]]!=0)) .mp_abort("E_PREDICTION_SUPPORT","inactive categories must remain zero")
  rows<-list();correct<-list()
  add<-function(domain,target,metric,value,n,kind,n_rows=NA_integer_) {
    rows[[length(rows)+1L]]<<-data.frame(domain=domain,model=b,level="population",target=target,metric=metric,estimate=value,
      unit=if(domain=="measurement_structure")"probability^2"else"probability",n_units=n,unit_type=kind,n_rows=n_rows,
      status=if(is.finite(value))"computed"else"not_assessed",stringsAsFactors=FALSE)
  }
  # Observation cells are validated route/content pairs, independent of arbitrary
  # observation_task display labels or the27 latent-parameter task keys.
  for(route in 1:3)for(content in 1:3) {
    ii<-which(d$route==route & d$content==content);informative<-ii[totals[ii]>0]
    label<-paste(b,c("CS","MU_no","MU_yes")[route],paste0("content",content),sep="::")
    people<-d$.person_key[informative];den<-tapply(totals[informative],people,sum)
    for(category in active[[route]]) {
      num<-tapply(counts[informative,category],people,sum);rate<-num/den
      target<-paste(label,paste0("category",category),sep="::")
      add("marginal",target,"person_category_rate_mean",if(length(rate))mean(rate)else NA_real_,length(rate),"equally weighted people with positive task total",length(informative))
      add("marginal",target,"person_category_rate_sd",if(length(rate)>=2L)stats::sd(rate)else NA_real_,length(rate),"equally weighted people with positive task total",length(informative))
      if(category==if(route==1L)1L else 5L)correct[[label]]<-rate
    }
  }
  for(pair in utils::combn(names(correct),2L,simplify=FALSE)) {
    x<-correct[[pair[1L]]];y<-correct[[pair[2L]]];common<-intersect(names(x),names(y))
    add("measurement_structure",paste(pair,collapse=" | "),"paired_person_correct_rate_covariance",
      if(length(common)>=2L)stats::cov(x[common],y[common])else NA_real_,length(common),"paired positive-total people; means recomputed on each intersection")
  }
  do.call(rbind,rows)
}
.mp_assess_m3_ppc <- function(fit,draws,seed,budget,simulator) {
  .mp_prediction_rng_preflight()
  plan<-.mp_m3_ppc_plan(fit,draws,budget);seed<-.mp_integer(seed,"seed")
  if(!inherits(simulator,"measurement_m3_simulator")) .mp_abort("E_SIMULATOR_REQUIRED","compile_m3_simulator must be called explicitly before M3 PPC")
  had_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE);old_kind<-RNGkind()
  if(had_seed)old_seed<-get(".Random.seed",envir=.GlobalEnv)
  on.exit({do.call(RNGkind,as.list(old_kind));if(had_seed)assign(".Random.seed",old_seed,envir=.GlobalEnv)else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)},add=TRUE)
  RNGkind("Mersenne-Twister","Inversion","Rejection");set.seed(seed)
  stan_seeds<-sample.int(.Machine$integer.max,draws)
  result<-tryCatch({
    variables<-setdiff(structure_ir(fit$spec)$monitored,c("z_task",grep("^Omega_component",structure_ir(fit$spec)$monitored,value=TRUE)))
    posterior<-as.matrix(fit$backend$draws(variables=variables,format="draws_matrix"));class(posterior)<-c("matrix","array")
    if(nrow(posterior)<draws) .mp_abort("E_DIAGNOSTIC_BUDGET","insufficient retained posterior rows for requested replications")
    selection<-as.integer(seq(1L,nrow(posterior),length.out=draws));observed<-.mp_m3_ppc_statistics(fit$spec)
    reps<-matrix(NA_real_,nrow(observed),draws)
    for(r in seq_len(draws)) {
      draw<-stats::setNames(as.numeric(posterior[selection[r],,drop=FALSE]),colnames(posterior))
      group<-.mp_component_population(fit$spec,draw)
      simulated<-simulate_m3_responses(fit$spec,group$evaluated$eta,simulator,stan_seeds[r])
      statistic<-.mp_m3_ppc_statistics(fit$spec,simulated$counts)
      if(!identical(statistic$target,observed$target)||!identical(statistic$metric,observed$metric)) .mp_abort("E_PREDICTION_SUPPORT","replicate statistic keys differ")
      reps[,r]<-statistic$estimate
    }
    table<-observed;table$observed<-observed$estimate;table$estimate<-NULL
    summarize<-function(fun)apply(reps,1L,function(x)if(all(is.finite(x)))fun(x)else NA_real_)
    table$replicated_median<-summarize(stats::median);table$lower90<-summarize(function(x)unname(stats::quantile(x,.05)));table$upper90<-summarize(function(x)unname(stats::quantile(x,.95)))
    table$finite_replications<-rowSums(is.finite(reps));table$replications<-draws
    table$status<-ifelse(is.finite(table$observed)&table$finite_replications==draws,"computed","not_assessed")
    table$outside90<-ifelse(table$status=="computed",table$observed<table$lower90|table$observed>table$upper90,NA)
    table$reason<-ifelse(table$status=="computed","Mechanical descriptive comparison only;<=10 reps and inadequate upstream smoke fit","Insufficient positive-total people or finite replications")
    retained<-.mp_replicate_aggregate_cache(table,reps)
    list(table=retained$table,replicate_statistics=retained$replicate_statistics,metadata=list(replicate_statistics_status="retained",replicate_statistics_layout="rows=replications; columns=table statistic_id",seed=seed,stan_seeds=stan_seeds,rng_policy="one continuing R stream after distinct Stan seeds; simulator preserves caller R RNG",rng_kind=c("Mersenne-Twister","Inversion","Rejection"),posterior_row_indices=selection,replications=draws,level="population",screening_only=TRUE,
      method="New same-N sum-to-zero group; fixed observed route/content/options/totals; original logits and multinomial RNG; streamed aggregates",
      population_covariance="marginal Sigma_predictor; between-person -Sigma_predictor/(N-1); expected sample covariance N/(N-1)*Sigma_predictor",
      zero_trial_rows=sum(rowSums(fit$spec$data_map$blocks[[1L]]$counts)==0),category_covariance="only correct-rate cross-task pairs; compositional category covariance singularity is expected",
      interval="90% empirical replication interval; no calibrated test, fit verdict or inference claim",
      monte_carlo_uncertainty="<=10 dependent poor-smoke posterior rows; intervals and tail comparisons are unstable",
      upstream=list(computation=fit$cache$computation,hmc=fit$cache$hmc,inference=fit$cache$inference,run=fit$run),
      simulator=list(version=simulator$version,execution=simulated$execution,code_hash=simulator$code_hash,resource_hash=simulator$resource_hash,executable_hash=simulator$executable_hash),
      data_hash=fit$spec$provenance$data_hash,model_hash=fit$spec$provenance$model_hash,plan=plan))
  },error=identity)
  if(inherits(result,"error")) {
    fit$cache$predictive_fit<-.mp_status("failed",conditionMessage(result));fit$cache$measurement_structure_fit<-.mp_status("failed","This predictive attempt failed; no covariance assessment is retained")
    fit$cache$ppc<-list(table=NULL,metadata=list(seed=seed,plan=plan,error=conditionMessage(result)))
  } else {
    fit$cache$ppc<-result
    fit$cache$predictive_fit<-.mp_status(if(all(result$table$status=="computed"))"computed"else"warning",paste(draws,"group-generative observed-data screening reps from inadequate smoke draws; no evidence of model validity"))
    complete<-all(result$table$status[result$table$domain=="measurement_structure"]=="computed")
    fit$cache$measurement_structure_fit<-.mp_status(if(complete)"computed"else"warning","Descriptive paired-person correct-rate covariance checks only; no mechanical pass/fail or fit verdict")
  }
  fit
}
.mp_component_population <- function(spec,draw,innovations=NULL) {
  s<-spec$structure;J<-length(s$keys);N<-length(spec$data_map$subject_keys)
  if(is.null(innovations))innovations<-matrix(stats::rnorm(J*N),J,N)
  if(!is.matrix(innovations)||!identical(dim(innovations),c(as.integer(J),as.integer(N)))||any(!is.finite(innovations))) .mp_abort("E_COMPONENT_STATE","population innovations must be a finite J-by-N matrix")
  z<-sweep(innovations,1,rowMeans(innovations),"-")*sqrt(N/(N-1))
  vector<-function(name,n)unname(draw[paste0(name,"[",seq_len(n),"]")])
  matrix_value<-function(name,nr,nc){ij<-expand.grid(i=seq_len(nr),j=seq_len(nc));matrix(draw[paste0(name,"[",ij$i,",",ij$j,"]")],nr,nc)}
  state<-list(mu=vector("mu",J),tau=vector("tau",J),z_task=z,families=list(),cross=list());col0<-0L
  for(i in seq_along(s$families)) {
    name<-names(s$families)[i];f<-s$families[[i]];prefix<-paste0("f",i,"_")
    state$families[[name]]<-list(base=vector(paste0(prefix,"base"),3),paradigm=matrix_value(paste0(prefix,"par"),3,ncol(f$paradigm)),content=matrix_value(paste0(prefix,"con"),3,ncol(f$content)),interaction=matrix_value(paste0(prefix,"int"),3,ncol(f$interaction)))
    if(i>1L)state$cross[[name]]<-matrix_value(paste0(prefix,"cross"),s$common_dimensions[i],col0)
    col0<-col0+s$common_dimensions[i]
  }
  list(state=state,evaluated=evaluate_component_structure(s,state))
}
