source(file.path("helpers", "native-guard.R"))
# Public controller integration with explicit simulator/acquisition doubles only.
# No native execution and no posterior-origin evidence.
source('frontend-predict-m3cs.R')
if(exists('s'))local({
  oldcheck<-private('.bh_m3cs_simulator_check');oldrun<-private('.bh_m3cs_run');oldingest<-private('.bh_ppc_ingest_csv')
  on.exit({.bmeas_test_set('.bh_m3cs_simulator_check',oldcheck);.bmeas_test_set('.bh_m3cs_run',oldrun);.bmeas_test_set('.bh_ppc_ingest_csv',oldingest)})
  .bmeas_test_set('.bh_m3cs_simulator_check',function(spec,simulator,budget=NULL)invisible(TRUE))
  seen<-list()
  .bmeas_test_set('.bh_m3cs_run',function(spec,eta,simulator,seed,draw_response=TRUE,budget=NULL){
    seen[[length(seen)+1L]]<<-m3cs_simulator_data(spec,eta)
    list(counts=spec$data_map$blocks[[1L]]$counts)
  })
  .bmeas_test_set('.bh_ppc_ingest_csv',function(paths,variables,budget,draws,expected_model){
    value<-setNames(rep(.1,length(variables)),variables)
    list(draws=matrix(rep(value,each=draws),draws,dimnames=list(NULL,variables)),
      draw_keys=data.frame(chain=rep(1L,draws),iteration=seq_len(draws)),
      receipt=list(actual_posterior_rows=draws,all_row_shape=c(draws,length(variables)),materialized_required_values=draws*length(variables)))
  })
  sim<-structure(list(version='engineering_double',code_hash='not_native',resource_hash='not_native',executable_hash='not_native',toolchain=list()),class='bmeas_m3cs_simulator')
  set.seed(17);rng<-.Random.seed
  result<-assess_measurement_fit(fit,level='conditional',draws=2,seed=73,budget=budget,simulator=sim)
  if(result$cache$predictive_fit$status!='computed')print(result$cache$ppc$metadata$failure)
  stopifnot(result$cache$predictive_fit$status=='computed',result$cache$measurement_structure_fit$status=='not_assessed',
    length(seen)==2,identical(rng,.Random.seed),ncol(result$cache$ppc$replicate_statistics)==105,
    result$cache$ppc$metadata$actual_work$simulated_values==180,length(result$cache$ppc$metadata$stan_seeds)==2)
  private('.bh_pf_ppc')(result$cache$ppc,'ppc')
  stopifnot(is.list(fit_checks(result)))
  cat('PASS public conditional controller, RNG, ten-cell work and portable saved evidence with explicit R-only doubles\n')
})
