source(file.path("helpers", "native-guard.R"))
# Mixed-family contracts and PPC mechanics; no Stan compilation/sampling.
if(Sys.getenv("MEASUREMENTPKG_SOURCE")!="") for(f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"),"R"),pattern="[.]R$",full.names=TRUE))source(f) else library(bmeas)
passed<-0L
ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat("PASS:",label,"\n")}
err<-function(expr,code){r<-tryCatch({force(expr);NULL},error=identity);ok(inherits(r,"measurement_error")&&identical(r$code,code),code)}
internal<-function(name)if(exists(name))get(name)else getFromNamespace(name,"bmeas")
spec<-demo_mixed_spec(); d<-stan_data(spec); map<-stan_symbol_map(spec); keys<-parameter_keys(spec)
ok(d$N==8 && d$O==48 && d$J==6 && d$K==2 && d$S==4 && d$O_cont==32 && d$O_bin==16,"one executable mixed-family Stan data representation")
ok(all(keys$units[keys$parameter=="p"]=="probability")&&all(keys$predictor_units[keys$parameter=="p"]=="log_odds"),"binomial parameter and eta units are distinct")
ok(all(keys$units[keys$parameter%in%c("meanlog","sdlog")]=="log(response/second)"),"lognormal parameter units are log-response units")
ok(sum(is.na(map$observation_sd))==2 && sum(grepl("sdlog",map$observation_sd_key),na.rm=TRUE)==2,"binomial has no free scale; lognormal sdlog has stable keys")
pt<-prior_table(spec)
ok(sum(pt$prior_scale=="logit")==4 && sum(pt$prior_scale=="log_response")==4,"priors resolve on explicit link scales")
code<-as.character(make_stan_code(spec))
ok(grepl("binomial_logit_lpmf",code)&&grepl("lognormal_lpdf",code)&&grepl("normal_lpdf",code),"all three likelihoods share one Stan model")
ok(grepl("Sigma_predictor",code)&&!grepl("Sigma_observed",code),"mixed covariance is never mislabeled raw observed covariance")
ok(sum(d$trials==0)==1 && sum(d$successes[d$trials==0])==0,"zero-trial rows preserved without invented responses")
bad<-spec;bad$priors<-prior_preset("gaussian_cfa_standardized_v1");err(check_measurement(bad),"E_PRIOR_SCALE")
bad<-spec;bad$priors$blocks$accuracy$predictor_scale<-"response";err(check_measurement(bad),"E_PRIOR_SCALE")
bad<-spec;bad$priors$blocks$time$units<-"millisecond";err(check_measurement(bad),"E_PRIOR_UNITS")
bad<-spec;bad$priors$blocks$accuracy$observation_sd<-half_normal(1);err(check_measurement(bad),"E_PRIOR_COVERAGE")
bad<-spec;bad$priors$blocks$time$observation_sd<-NULL;err(check_measurement(bad),"E_PRIOR_COVERAGE")
bad<-spec;bad$priors$blocks$time<-NULL;err(check_measurement(bad),"E_PRIOR_COVERAGE")
bad<-spec;bad$structure$nuisance<-task_parameters("sigma");err(check_measurement(bad),"E_PARAMETER_COVERAGE")
bad<-spec;bad$data_map$blocks$accuracy$trials[]<-0;bad$data_map$blocks$accuracy$successes[]<-0;err(check_measurement(bad),"E_UNINFORMATIVE_PARAMETER")
counts<-data.frame(person="a",task="b",y=0,n=.Machine$integer.max+1)
err(observe(binomial_model(),counts,"person","task",fields=list(successes="y",trials="n"),units="count"),"E_FIELD_SUPPORT")
# Registration does not imply formal statistical validation.
ok(all(vapply(spec$models,function(b)all(b$model$capabilities$status[b$model$capabilities$available]=="unvalidated"),logical(1))),"implemented methods remain statistically unvalidated")
run<-run_config(1,1,1,20,20,1,1,48,8,20)
fit<-structure(list(spec=spec,run=run,execution_plan=execution_plan(spec,run)),class="measurement_fit")
budget<-diagnostic_budget(10,48,480)
ok(ppc_plan(fit,10,budget)$simulated_values==480,"PPC preflight exposes bounded simulated values")
err(ppc_plan(fit,11,budget),"E_DIAGNOSTIC_BUDGET")
larger_budget<-diagnostic_budget(21,48,1008)
ok(larger_budget$max_draws==21L,"explicit larger budget is admitted by the shared constructor")
err(ppc_plan(fit,21,larger_budget),"E_DIAGNOSTIC_BUDGET")
badfit<-fit;badfit$spec$models$time$model$capabilities$available[3]<-FALSE;err(ppc_plan(badfit,10,budget),"E_CAPABILITY")
statistics<-internal(".mp_ppc_statistics"); observed<-statistics(spec)
ok(sum(observed$domain=="measurement_structure")==15,"PPC includes all paired-person cross-indicator covariances")
r<-observed[observed$target=="accuracy::p::t1" & observed$metric=="zero_success_rows",]
ok(nrow(r)==1 && r$n_units==7,"zero-trial rows excluded from binomial rate/extreme summaries")
ok(all(observed$n_units[observed$domain=="measurement_structure"]%in%c(7,8)),"cross-task covariance uses paired people, not pooled trial counts")
ok(sum(observed$metric%in%c("log_mean","log_sd"))==4,"lognormal PPC includes log-scale mean and dispersion")
# Explicit deterministic test doubles inspect mapping, never supply a user-facing fit.
populate<-internal(".mp_population_responses")
test_spec<-spec
# Capture shared eta inputs while the real registered methods generate valid responses.
seen<-new.env(parent=emptyenv())
for(b in names(test_spec$models)) {
  original<-test_spec$models[[b]]$model$methods$simulate
  test_spec$models[[b]]$model$methods$posterior_predict<-local({name<-b;fn<-original;function(eta,observation_sd=NULL,trials=NULL){seen[[name]]<-eta;fn(eta,observation_sd,trials)}})
}
draw<-c(stats::setNames(rep(0,6),map$intercept),stats::setNames(rep(1,6),map$loading),
        stats::setNames(rep(.5,4),map$observation_sd[!is.na(map$observation_sd)]),
        stats::setNames(c(1,.3,.3,1),c("Omega[1,1]","Omega[2,1]","Omega[1,2]","Omega[2,2]")))
set.seed(101); invisible(populate(test_spec,draw))
ok(identical(seen$scores,seen$accuracy)&&identical(seen$scores,seen$time),"fresh person factor is shared across all three blocks")
bad<-spec;bad$models$time$model$methods$posterior_predict<-function(eta,...) rep(0,length(eta))
err(populate(bad,draw),"E_FIELD_SUPPORT")
if(requireNamespace("posterior",quietly=TRUE)) {
  mock_draws <- posterior::as_draws_matrix(matrix(rep(draw,2L),nrow=2L,byrow=TRUE,dimnames=list(NULL,names(draw))))
  mock_fit <- fit; mock_fit$backend <- list(draws=function(...)mock_draws); mock_fit$cache <- list()
  checked <- assess_measurement_fit(mock_fit,draws=2,seed=201,budget=diagnostic_budget(2,48,96))
  ok(checked$cache$predictive_fit$status=="computed","PPC handles actual posterior draws_matrix row slicing")
  aggregate_draws<-checked$cache$ppc$replicate_statistics;ct<-checked$cache$ppc$table
  ok(identical(dim(aggregate_draws),c(2L,nrow(ct)))&&identical(colnames(aggregate_draws),ct$statistic_id),"basic PPC retains only keyed aggregate replication matrix")
  ok(identical(unname(apply(aggregate_draws,2,median)),ct$replicated_median)&&identical(unname(apply(aggregate_draws,2,function(x)quantile(x,.05))),ct$lower90)&&identical(unname(apply(aggregate_draws,2,function(x)quantile(x,.95))),ct$upper90),"basic cached intervals reconstruct exactly from retained aggregates")
  broken <- checked;broken$spec$models$time$model$methods$posterior_predict<-function(eta,...)rep(0,length(eta))
  failed <- assess_measurement_fit(broken,draws=2,seed=201,budget=diagnostic_budget(2,48,96))
  ok(failed$cache$predictive_fit$status=="failed" && is.null(failed$cache$ppc$table),"out-of-support replications become failed PPC, never computed")
  ok(failed$cache$measurement_structure_fit$status=="failed"&&is.null(failed$cache$ppc$replicate_statistics),"failed basic retry clears previous covariance status and aggregate samples")
}
cat("TOTAL",passed,"mixed checks passed; no compilation or sampling performed\n")
