source(file.path('helpers','native-guard.R'))
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
local({
 passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
 err<-function(expr,code){e<-tryCatch(suppressWarnings(force(expr)),error=identity);if(!inherits(e,'measurement_error')||!e$code%in%code){print(e);stop(code)};ok(TRUE,e$code)}
 d<-data.frame(person=rep(1:4,2));for(j in 1:6)d[[paste0('x',j)]]<-sin(seq_len(8)*j/7)
 cor<-list(default='zero',free=list(~A:C,~D:F),fixed=list(list(pairs=~A:B,value=.2),list(pairs=~D:E,value=.3)))
 f<-bmf(A=~x1,B=~x2,C=~x3,D=~x4,E=~x5,F=~x6,cor=cor)
 s<-bmeas(f,d,subject='person',fit=FALSE)
 calls<-0L;env<-if(source_mode).GlobalEnv else asNamespace('bmeas');original<-get('.bh_fit_scalar',env);locked<-bindingIsLocked('.bh_fit_scalar',env)
 replace<-function(value){if(bindingIsLocked('.bh_fit_scalar',env))unlockBinding('.bh_fit_scalar',env);assign('.bh_fit_scalar',value,env);if(locked)lockBinding('.bh_fit_scalar',env)}
 on.exit(replace(original),add=TRUE)
 replace(function(...){calls<<-calls+1L;private('.mp_abort')('E_TERMINAL_TEST','terminal fitter double')})
 for(explore in c(FALSE,TRUE))err(bmeas(s,chains=1,iter=20,warmup=10,execution='qualified',exploratory=explore,resources='development'),'E_EXECUTION_PROFILE')
 err(bmeas(s,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_TERMINAL_TEST')
 ok(calls==1L,'explicit experimental request reaches exactly one terminal fitter')
 err(bmeas(s,chains=1,iter=20,warmup=10,execution='experimental',resources=list(max_output_bytes=1,cache='deferred')),'E_OUTPUT_BUDGET')
 altered<-s;altered$tables$components$component[1]<-'tampered'
 err(bmeas(altered,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_SPEC_CHANGED')
 weak<-bmeas(f,d,subject='person',fit=FALSE,prior=set_prior('lkj(.8)','cor'))
 err(bmeas(weak,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_NUMERICAL_PRIOR_UNQUALIFIED')
 residual<-list(targets=~x1+x2+x3+x4+x5+x6,cor=list(default='zero',free=list(~x1:x3,~x4:x6),fixed=list(list(pairs=~x1:x2,value=.2),list(pairs=~x4:x5,value=.3))))
 r<-bmeas(bmf(residual=residual),d,subject='person',fit=FALSE)
 dual<-bmeas(bmf(A=~x1,B=~x2,C=~x3,D=~x4,E=~x5,F=~x6,cor=cor,residual=residual),d,subject='person',fit=FALSE)
 centered<-bmeas(bmf(A=~x1,B=~x2,C=~x3,D=~x4,E=~x5,F=~x6,cor=cor,residual=list(targets=~x1+x2+x3+x4+x5+x6),person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3+x4+x5+x6)),d,subject='person',fit=FALSE)
 for(other in list(centered)) {
  ok(!private('.bh_affine_runtime_supported')(other),'unqualified product conjunction stays closed')
  err(private('.bh_affine_runtime_gate')(other),'E_FRONTEND_BACKEND_UNSUPPORTED')
  for(execution in c('qualified','experimental'))for(explore in c(FALSE,TRUE))err(bmeas(other,chains=1,iter=20,warmup=10,execution=execution,exploratory=explore,resources='development'),c('E_FRONTEND_BACKEND_UNSUPPORTED','E_IDENTIFICATION'))
 }
 # Preserve prerequisite failures rather than overwriting them with the product profile.
 model<-gaussian_model();model$capabilities$available[model$capabilities$capability=='fit']<-FALSE
 long<-do.call(rbind,lapply(1:6,function(j)data.frame(person=d$person,task=paste0('x',j),y=d[[paste0('x',j)]])))
 long$observation<-seq_len(nrow(long))
 block<-observe(model,long,'person','task','y',units='points',observation_id='observation')
 fd<-bmf(A=~score$mu['x1'],B=~score$mu['x2'],C=~score$mu['x3'],D=~score$mu['x4'],E=~score$mu['x5'],F=~score$mu['x6'],cor=cor)
 disabled<-bmeas(fd,list(score=block),fit=FALSE)
 ok(check_model(disabled)$backend$status=='fit_capability_unavailable'&&!private('.bh_affine_product_runtime_supported')(disabled),'disabled-fit Gaussian cannot inherit product execution')
 for(execution in c('qualified','experimental'))err(bmeas(disabled,chains=1,iter=20,warmup=10,execution=execution,resources='development'),'E_CAPABILITY')
 historical<-s;historical$schema_version<-'historical_unsupported_schema'
 ok(private('.bh_checks')(historical)$backend$status=='prepared_schema_requires_rebuild','schema failure cannot be overwritten by product status')
 unsupported_model<-gaussian_model();unsupported_model$likelihood$normalized<-FALSE
 unsupported_block<-observe(unsupported_model,long,'person','task','y',units='points',observation_id='observation')
 bad_contract<-bmeas(fd,list(score=unsupported_block),fit=FALSE)
 ok(check_model(bad_contract)$backend$status=='observation_contract_unsupported','unsupported observation contract cannot be overwritten by product status')
 for(execution in c('qualified','experimental'))err(bmeas(bad_contract,chains=1,iter=20,warmup=10,execution=execution,resources='development'),'E_OBSERVATION_CONTRACT_UNSUPPORTED')
 callback_calls<-0L;custom_callback<-s;custom_callback$models[[1]]$model$validate_data<-function(...) {callback_calls<<-callback_calls+1L;stop('must not run')}
 ok(!private('.bh_affine_product_runtime_supported')(custom_callback)&&callback_calls==0L,'custom callbacks are classified without invocation and cannot inherit product execution')
 ok(calls==1L,'all refusals precede fitter')
 # A renamed canonical family is still recognized; a changed density contract is not.
 altered_model<-s;altered_model$models[[1]]$model$name<-'cosmetic Gaussian name'
 ok(private('.bh_affine_product_runtime_supported')(altered_model),'Gaussian eligibility does not depend on display name')
 altered_model$models[[1]]$model$likelihood$symbol<-'not_a_canonical_gaussian_lpdf'
 ok(!private('.bh_affine_product_runtime_supported')(altered_model),'custom density cannot inherit the Gaussian product profile')
 map<-private('.bh_affine_summary_map')(s)
 ok(identical(map$computational_variable,c('correlation_raw[1]','correlation_raw[2]'))&&all(map$computational_mapping=='unique_label_product_interval_coordinate'),'unique product coordinates retain direct scientific mapping')
 metadata<-private('.bh_affine_cache_metadata')(s,map)
 ok(metadata$version=='iid_gaussian_common_product_live_cache_v1'&&grepl('saved-value product profile supports durable postprocessing',metadata$runtime_scope,fixed=TRUE),'live metadata discloses the bounded saved-value profile')
 # Product export/hostile-storage contracts have their own versioned archive suite.
 cat('PASSED',passed,'product activation controls; no compilation or sampling\n')
})
