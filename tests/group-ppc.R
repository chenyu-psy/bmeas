source(file.path("helpers", "native-guard.R"))
# Synthetic mechanics only. No Stan compilation, MCMC or real simulator calls.
if(Sys.getenv('MEASUREMENTPKG_SOURCE')!='')for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),pattern='[.]R$',full.names=TRUE))source(f)else library(bmeas)
private<-function(name)if(Sys.getenv('MEASUREMENTPKG_SOURCE')!='')get(name)else getFromNamespace(name,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS:',label,'\n')}
err<-function(expr,code){x<-tryCatch({force(expr);NULL},error=identity);ok(inherits(x,'measurement_error')&&identical(x$code,code),code)}
basic<-demo_measurement_spec();err(make_m3_simulator_code(basic),'E_CHECK_UNSUPPORTED')
file<-Sys.getenv('MEASUREMENTPKG_M3_RESOURCE')
if(!nzchar(file))cat('SKIP external full-M3 group prediction tests: no MEASUREMENTPKG_M3_RESOURCE\n')else {
  d<-expand.grid(content=1:3,route=1:3,person=c('synthetic_a','synthetic_b'),stringsAsFactors=FALSE)
  d$observation_id<-sprintf('synthetic_%03d',seq_len(nrow(d)));d$task<-'arbitrary_reused_label'
  d$counts<-d$options<-matrix(0L,nrow(d),10);active<-list(c(1:4,10),c(5:7,9:10),5:10)
  for(o in seq_len(nrow(d))){d$counts[o,active[[d$route[o]]]]<-1L;d$options[o,active[[d$route[o]]]]<-1L}
  map<-m3_reference_bindings(d);b<-observe(m3_model(file),d,'person','task',observation_id='observation_id',fields=list(counts='counts',options='options',route='route',content='content'),units='count',parameter_bindings=map)
  spec<-measurement(list(memory=b),shared_subjects('synthetic'),reference_structure('csmu_exp1_memory_gpc_v1',map$domains,'memory'),reference_priors('csmu_exp1_memory_gpc_v1',map$domains,'memory'))
  fake<-structure(list(spec=spec,cache=list(computation=list(status='warning',reason='synthetic test double'),hmc=list(num_divergent=1),inference=list(status='not_assessed')),run=list(kind='synthetic test double')),class='measurement_fit')
  budget<-diagnostic_budget(10,18,1800);plan<-ppc_plan(fake,2,budget)
  ok(plan$simulated_values==360L&&plan$level=='population','category cells included in explicit budget')
  err(ppc_plan(fake,11,diagnostic_budget(20,18,4000)),'E_DIAGNOSTIC_BUDGET')
  err(ppc_plan(fake,2,diagnostic_budget(2,18,359)),'E_DIAGNOSTIC_BUDGET')
  err(assess_measurement_fit(fake,level='conditional',draws=2,seed=1,budget=budget),'E_CHECK_UNSUPPORTED')
  err(assess_measurement_fit(fake,draws=2,seed=1,budget=budget),'E_SIMULATOR_REQUIRED')
  eta<-matrix(seq_len(54)/50,2,27);data<-m3_simulator_data(spec,eta)
  mapped<-spec$data_map$blocks[[1L]];cs<-which(mapped$route==1&mapped$content==1)
  rc<-match(targets('memory','rc','CS_rc_color'),spec$structure$keys)
  ok(identical(data$arguments[cs,4],eta[cbind(mapped$.person_index[cs],rc)]),'CS fourth argument is correct CS rc, never ra or MU rc')
  no<-which(mapped$route==2&mapped$content==1);yes<-which(mapped$route==3&mapped$content==1)
  ok(all(data$arguments[no,4]==100)&&all(data$arguments[yes,5]==100),'fixed MU ra is received eta100')
  ok(identical(data$arguments[no,1:2],data$arguments[yes,1:2]),'MU branches share both old/new activation targets')
  cold<-match(targets('memory','c','MU_ori_color'),spec$structure$keys);cnew<-match(targets('memory','c','MU_new_color'),spec$structure$keys)
  ok(identical(data$arguments[yes,3],eta[cbind(mapped$.person_index[yes],cnew)])&&identical(data$arguments[yes,4],eta[cbind(mapped$.person_index[yes],cold)]),'MU update c_new precedes c_old')
  err(m3_simulator_data(spec,matrix(NA_real_,2,27)),'E_COMPONENT_STATE')
  err(m3_simulator_data(spec,eta,draw_response=NA),'E_ARGUMENT')
  code<-make_m3_simulator_code(spec)
  ok(grepl('multinomial_rng(p,totals[o])',code,fixed=TRUE)&&!grepl('parameters {',code,fixed=TRUE),'inspectable external-logit simulator has no fitted parameters')
  stat<-private('.mp_m3_ppc_statistics')(spec)
  ok(nrow(stat)==132&&sum(stat$domain=='measurement_structure')==36,'48active-category mean/SDs plus36 named correct-rate pairs')
  ok(all(stat$n_units==2),'rates use global people despite reused task label')
  renamed<-spec;renamed$data_map$blocks[[1L]]$.task<-paste0('renamed',seq_len(18))
  ok(identical(stat,private('.mp_m3_ppc_statistics')(renamed)),'statistics keys use route/content, not display labels')
  zero<-spec;zero$data_map$blocks[[1L]]$counts[mapped$.person_index==2,]<-0L
  zs<-private('.mp_m3_ppc_statistics')(zero)
  ok(all(zs$n_units==1)&&all(zs$status[zs$metric!='person_category_rate_mean']=='not_assessed'),'one positive-total person cannot yield SD/covariance')
  zero$data_map$blocks[[1L]]$counts[,]<-0L;zs<-private('.mp_m3_ppc_statistics')(zero)
  ok(all(zs$n_units==0)&&all(zs$status=='not_assessed'),'zero-total cells are never zero-filled rates')
  bad<-mapped$counts;bad[1,1]<-bad[1,1]+1;err(private('.mp_m3_ppc_statistics')(spec,bad),'E_PREDICTION_SUPPORT')
  init<-structure_ir(spec)$init;draw<-numeric()
  for(name in names(init)){v<-init[[name]];if(!length(v))next;symbols<-if(is.matrix(v)){ij<-expand.grid(i=seq_len(nrow(v)),j=seq_len(ncol(v)));paste0(name,'[',ij$i,',',ij$j,']')}else paste0(name,'[',seq_along(v),']');draw<-c(draw,stats::setNames(as.numeric(v),symbols))}
  E<-matrix(seq_len(54)/40,27,2);group<-private('.mp_component_population')(spec,draw,E)
  ok(max(abs(rowSums(group$state$z_task)))<1e-12&&max(abs(group$state$z_task-sweep(E,1,rowMeans(E),'-')*sqrt(2)))<1e-12,'fresh centered innovations use sqrt(N/(N-1)), without sample-SD normalization')
  changed<-draw;changed[grepl('^z_task',names(changed))]<-1000
  ok(identical(group,private('.mp_component_population')(spec,changed,E)),'fitted person latent values are ignored')
  H<-diag(2)-matrix(1/2,2,2);V<-2*tcrossprod(H)
  ok(all(diag(V)==1)&&V[1,2]==-1,'same-N centered group has unit marginal variance and negative between-person dependence')
  # Mocked response generator proves cache/RNG plumbing only; no real inference.
  fake$backend<-list(draws=function(...){x<-rbind(draw,draw);if(requireNamespace('posterior',quietly=TRUE))posterior::as_draws_matrix(x)else x})
  fn<-private('.mp_assess_m3_ppc');env<-new.env(parent=environment(fn));environment(fn)<-env;groups<-list()
  env$simulate_m3_responses<-function(spec,eta,simulator,seed){groups[[length(groups)+1L]]<<-eta;list(counts=spec$data_map$blocks[[1L]]$counts,execution=list(algorithm='synthetic_mock'))}
  dummy<-structure(list(version='synthetic mock'),class='measurement_m3_simulator')
  set.seed(973);saved<-.Random.seed;kind<-RNGkind();result<-fn(fake,2,17,budget,dummy)
  ok(identical(saved,.Random.seed)&&identical(kind,RNGkind()),'master RNG state and kind restored')
  ok(length(groups)==2&&!identical(groups[[1]],groups[[2]]),'identical hyperparameter rows still generate fresh shared-person groups')
  ok(nrow(result$cache$ppc$table)==132&&all(result$cache$ppc$table$finite_replications==2),'aggregate-only completed cache')
  ok(identical(fake$cache$hmc,result$cache$hmc)&&identical(fake$cache$inference,result$cache$inference),'upstream warning and unassessed inference preserved')
  agg<-result$cache$ppc$replicate_statistics;tab<-result$cache$ppc$table
  ok(identical(dim(agg),c(2L,132L))&&identical(colnames(agg),tab$statistic_id),'small aggregate matrix is keyed to summary table')
  ok(identical(unname(apply(agg,2,median)),tab$replicated_median)&&identical(unname(apply(agg,2,function(x)quantile(x,.05))),tab$lower90)&&identical(unname(apply(agg,2,function(x)quantile(x,.95))),tab$upper90),'all cached intervals reconstruct exactly from replicate aggregates')
  legacy<-result;legacy$cache$ppc$replicate_statistics<-NULL
  ok(fit_checks(legacy)$metadata$replicate_statistics_status=='unavailable_legacy_run','earlier aggregate samples are unavailable, never silently regenerated')

  first<-result$cache$ppc;result2<-fn(fake,2,17,budget,dummy);ok(identical(first,result2$cache$ppc),'same master seed replays complete mock PPC')
  env$simulate_m3_responses<-function(...)stop('synthetic response failure')
  failed<-fn(result,2,17,budget,dummy)
  ok(is.null(failed$cache$ppc$table)&&failed$cache$predictive_fit$status=='failed'&&failed$cache$measurement_structure_fit$status=='failed','failed repeat invalidates prior table and both predictive statuses')
  ok(identical(saved,.Random.seed)&&identical(kind,RNGkind()),'RNG restored on failed attempt')
  RNGkind("Mersenne-Twister","Box-Muller","Rejection");set.seed(124);invisible(rnorm(1));expected<-rnorm(1)
  set.seed(124);invisible(rnorm(1));err(fn(fake,2,17,budget,dummy),'E_RNG_KIND');ok(identical(rnorm(1),expected),'Box-Muller hidden cached normal preserved by preflight rejection')
  set.seed(124);invisible(rnorm(1));err(m3_probabilities(spec,eta,dummy),'E_RNG_KIND');ok(identical(rnorm(1),expected),'probability helper rejects before disturbing Box-Muller hidden state')
  do.call(RNGkind,as.list(kind));assign('.Random.seed',saved,envir=.GlobalEnv)

}
cat('TOTAL',passed,'group PPC checks passed; no compilation or Stan execution\n')
