source(file.path('helpers','native-guard.R'))
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
source(file.path('helpers','residual-product-fixtures.R'))
local({
 passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
 err<-function(expr,code){e<-tryCatch(suppressWarnings(force(expr)),error=identity);if(!inherits(e,'measurement_error')||!e$code%in%code){print(e);stop(code)};ok(TRUE,e$code)}
 policy<-list(max_output_bytes=10485760,cache='deferred',cpu_budget=1,max_subjects=4,max_observations=100,max_saved_draws=10,max_targets=8,max_latent_coordinates=16,max_coefficient_primitives=32)
 specs<-residual_product_fixtures();calls<-0L;env<-if(source_mode).GlobalEnv else asNamespace('bmeas');original<-get('.bh_fit_scalar',env);locked<-bindingIsLocked('.bh_fit_scalar',env)
 replace<-function(value){if(bindingIsLocked('.bh_fit_scalar',env))unlockBinding('.bh_fit_scalar',env);assign('.bh_fit_scalar',value,env);if(locked)lockBinding('.bh_fit_scalar',env)}
 on.exit(replace(original),add=TRUE);replace(function(...){calls<<-calls+1L;private('.mp_abort')('E_TERMINAL_TEST','terminal fitter double')})
 for(name in names(specs)) {
  s<-specs[[name]];at<-calls
  ok(private('.bh_affine_product_runtime_supported')(s),'canonical iid residual product eligible across common modes')
  ok(check_model(s)$backend$evidence_version=='bmeas_iid_gaussian_residual_dual_product_fixed_states_v1','new residual/dual evidence separate from common-only and historical modes')
  for(explore in c(FALSE,TRUE))err(bmeas(s,chains=1,iter=20,warmup=10,execution='qualified',exploratory=explore,resources=policy),'E_EXECUTION_PROFILE')
  err(bmeas(s,chains=1,iter=20,warmup=10,execution='experimental',resources=policy),'E_TERMINAL_TEST');ok(calls==at+1L,'only explicit experimental reaches fitter once')
  err(bmeas(s,chains=1,iter=20,warmup=10,execution='experimental',resources=list(max_output_bytes=1,cache='deferred')),'E_OUTPUT_BUDGET')
  bad<-s;bad$tables$parameter_residuals$target[1]<-'altered';err(bmeas(bad,chains=1,iter=20,warmup=10,execution='experimental',resources=policy),'E_SPEC_CHANGED')
  weak<-bmeas(s$formula,s$models,subjects='shared',fit=FALSE,prior=c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(.7)','parameter_sd'),set_prior('lkj(.8)','cor',layer='parameter')))
  err(bmeas(weak,chains=1,iter=20,warmup=10,execution='experimental',resources=policy),'E_NUMERICAL_PRIOR_UNQUALIFIED')
  map<-private('.bh_affine_summary_map')(s);pr<-s$tables$correlation_primitives;pm<-map[map$layer=='parameter',]
  ok(identical(pm$computational_variable,c('parameter_correlation_raw[1]','parameter_correlation_raw[2]'))&&!anyDuplicated(map$parameter_key),'residual labels keep unique ordered raw coordinates and keys')
  ok(identical(vapply(pm$alias_cells,nrow,integer(1)),c(2L,1L)),'shared disconnected residual label has two aliases and one coordinate')
  meta<-private('.bh_affine_cache_metadata')(s,map);ok(meta$version=='iid_gaussian_residual_dual_product_live_cache_v1'&&grepl('saved-value product profile supports durable postprocessing',meta$runtime_scope,fixed=TRUE),'live residual product metadata describes bounded saved-value support')
  for(field in c('validate_data','validate_bindings','informative_rows')) {bad<-s;bad$models[[1]]$model[[field]]<-function(...)stop('must never run');ok(!private('.bh_affine_product_runtime_supported')(bad),'custom callbacks cannot inherit product runtime')}
  bad<-s;bad$models[[1]]$model$likelihood$symbol<-'other_density';ok(!private('.bh_affine_product_runtime_supported')(bad),'noncanonical density remains closed')
 }
 err(bmeas(specs$dual_product,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_RUN_BUDGET')
 base<-specs$residual_only
 for(kind in c('disabled_fit','unnormalized')) {
  blocks<-base$models
  if(kind=='disabled_fit')blocks[[1]]$model$capabilities$available[blocks[[1]]$model$capabilities$capability=='fit']<-FALSE else blocks[[1]]$model$likelihood$normalized<-FALSE
  bad<-bmeas(base$formula,blocks,subjects='shared',fit=FALSE,prior=base$prior)
  expected<-if(kind=='disabled_fit')'E_CAPABILITY'else'E_OBSERVATION_CONTRACT_UNSUPPORTED'
  for(execution in c('qualified','experimental'))err(bmeas(bad,chains=1,iter=20,warmup=10,execution=execution,resources=policy),expected)
 }
 bad<-base;bad$models[[1]]$model<-ezdm_model();ok(!private('.bh_affine_product_runtime_supported')(bad),'canonical cognitive likelihood cannot inherit Gaussian product eligibility')
 d<-data.frame(person=rep(as.character(1:4),2));for(j in 1:6)d[[paste0('x',j)]]<-sin(seq_len(8)*j/7)+seq_len(8)/9
 residual<-list(targets=~x1+x2+x3+x4+x5+x6,cor=list(default='zero',free=list(~x1:x3,~x4:x6),fixed=list(list(pairs=~x1:x2,value=.2),list(pairs=~x4:x5,value=.3))))
 centered<-bmeas(bmf(residual=residual,person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3+x4+x5+x6)),d,subject='person',fit=FALSE)
 raw<-bmeas(bmf(A=~x1+x2+x3,B=~x4+x5+x6,loadings=list(fixed=list(list(value=1,cells=list(A=~x1+x2+x3,B=~x4+x5+x6)))),cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(first=~A,second=~B))),residual=residual),d,subject='person',fit=FALSE,prior=set_prior('normal(0,.2)','cor_raw',block='second'))
 for(bad in list(centered,raw)) {
  ok(!private('.bh_affine_product_runtime_supported')(bad),'residual product centered/task/raw mode remains closed')
  for(execution in c('qualified','experimental'))for(explore in c(FALSE,TRUE))err(bmeas(bad,chains=1,iter=20,warmup=10,execution=execution,exploratory=explore,resources=policy),'E_FRONTEND_BACKEND_UNSUPPORTED')
  err(private('.bh_cache_scalar_fit')(list(),bad,list()),'E_REFERENCE_CACHE_UNAVAILABLE')
 }
 ok(calls==5L,'all other refusals precede fitter')
 cat('PASSED',passed,'residual/dual product runtime controls; no compilation or sampling\n')
})
