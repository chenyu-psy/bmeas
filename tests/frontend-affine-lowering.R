source(file.path("helpers", "native-guard.R"))
# Bounded source/data/initialization gate; no compiler or model calls.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
fixed<-function(pair,value)list(pairs=pair,value=value)
d<-data.frame(person=rep(1:4,2));for(j in 1:6)d[[paste0('x',j)]]<-sin(seq_len(8)*j/7)+seq_len(8)/9
f<-bmf(A=~x1+x2,B=~x3+x4,C=~x5+x6,cor=list(default='free',fixed=list(fixed(~A:C,.9),fixed(~B:C,.9))))
s<-bmeas(f,d,subject='person',fit=FALSE);x<-standata(s);g<-check_model(s);code<-as.character(stancode(s))
ok(x$E==1L&&x$component_cor_mode==2L&&max(abs(x$component_cor_bounds[1:4]-c(.62,1,.81,.19)))<1e-10,'one unique computational coordinate and full positive interval')
ok(abs(g$initialization$correlation_scientific$component-.81)<1e-10,'raw zero initializes actual scientific midpoint')
ok(grepl('unique free marginal labels',code,fixed=TRUE)&&grepl('bmeas_checked_affine_cholesky',code,fixed=TRUE),'source exposes unique-label measure and actual matrix certificate')
ok(g$backend$status=='experimental_affine_correlations','new affine runtime requires explicit experimental execution')
for(exploratory in c(FALSE,TRUE))err(bmeas(s,chains=1,iter=20,warmup=10,execution='qualified',exploratory=exploratory,resources='development'),'E_EXECUTION_PROFILE')
ok(private('.bh_affine_runtime_supported')(s),'actual iid/final-marginal mode meets the bounded affine capability predicate')
fixed_spec<-bmeas(bmf(A=~x1+x2,B=~x3+x4,cor=list(default='zero',fixed=list(fixed(~A:B,-.3)))),d,subject='person',fit=FALSE)
x<-standata(fixed_spec);i<-check_model(fixed_spec)$initialization
ok(x$E==0L&&x$component_cor_mode==1L&&!any(get_prior(fixed_spec)$class=='cor'),'fixed nonidentity matrix has no correlation unknown or density role')
ok(i$joint_correlation_prior_kernel==0&&i$correlation_chart_log_jacobian==0,'fixed matrix contributes no determinant kernel or Jacobian')
ok(!'component_fixed_factor'%in%names(x),'factor is derived from authoritative fixed matrix, not separately supplied data')
multi<-bmeas(bmf(A=~x1+x2,B=~x3+x4,C=~x5+x6,cor=list(default='free',fixed=list(fixed(~A:B,.2)))),d,subject='person',fit=FALSE)
err(standata(multi),'E_CORRELATION_DIMENSION_UNSUPPORTED');err(stancode(multi),'E_CORRELATION_DIMENSION_UNSUPPORTED')
for(prefix in c('bmeas_','bhmm_')) {
 symbol<-paste0(prefix,'owned_lpdf')
 m<-register_stan_adapter(new_observation_model('reserved_test','1',response_schema(y='real'),
  list(location=parameter_spec('real','identity','theta','subject_task','points','Location'),noise=parameter_spec('positive','log','theta','task','points','Noise')),
  stan_density(symbol=symbol,arguments=c('y','location','noise'),argument_types=rep('real',3),normalized=TRUE,approximation='none',builtin=TRUE,license='MIT'),model_capabilities(fit=TRUE)),
  function_code=paste0('real ',symbol,'(real y, real location, real noise) { return normal_lpdf(y | location,noise); }'),function_exports=symbol)
 dat<-data.frame(person=1:4,task='t',y=c(.1,.5,-.2,.7));b<-observe(m,dat,'person','task','y',units='points')
 err(bmeas(bmf(G=~custom$location['t']),list(custom=b),fit=FALSE),'E_SYMBOL_COLLISION')
}
cat('PASSED',passed,'affine lowering controls\n')
