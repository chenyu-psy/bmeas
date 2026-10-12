source(file.path("helpers", "native-guard.R"))
# Fixed/equal final marginal correlations: public R/IR controls, no native calls.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
if(!source_mode)for(n in c('.bh_covariance_layout','.bh_covariance_state'))assign(n,getFromNamespace(n,'bmeas'))
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch(force(x),error=identity);if(!inherits(e,'measurement_error')||!identical(e$code,code)){print(e);stop(code)};ok(TRUE,code)}
known<-data.frame(person=1:4,task='x1',y=c(.2,.5,-.7,1.1))
unknown<-data.frame(person=rep(1:4,2),task=rep(c('x2','x3'),each=4),y=sin(1:8)+1:8/9)
blocks<-list(known=observe(gaussian_model(),known,'person','task','y',units='score',parameters=list(sigma=list(fixed=1))),
 unknown=observe(gaussian_model(),unknown,'person','task','y',units='score'))
f<-bhmf(residual=list(targets=~known$mu['x1']+unknown$mu[c('x2','x3')],cor=list(default='zero',equal=list(e=list(
 ~known$mu['x1']:unknown$mu['x2'],~known$mu['x1']:unknown$mu['x3'],~unknown$mu['x2']:unknown$mu['x3'])))))
s<-bhmm(f,blocks,subjects='shared',fit=FALSE);i<-check_model(s)$identification
ok(i$status=='local_rank_full_not_global_proof'&&i$free_covariance_parameters==6L&&all(i$probe_ranks==6L),'one known-noise target identifies the shared-rho triangle without repeats')
dom<-model_table(s,'correlation_domains')$parameter
ok(max(abs(c(dom$lower,dom$upper)-c(-.5,1)))<1e-12,'three-way shared rho exact equicorrelation interval')
ok(all(grepl('unique',get_prior(s)$coordinate_measure[get_prior(s)$class=='cor'])),'prior table directly exposes the unique-label measure')
tab<-model_table(s,'correlation_constraint_rules')
ok(nrow(tab)==3L&&all(tab$layer=='parameter')&&all(vapply(tab$captured_selector,function(x)all(c('expression_text','endpoints','indices','capture_mode')%in%names(x)),logical(1))),'qualified residual selectors retain static capture and resolved endpoints')
# The ill-conditioned helper witness is also a prepared-model numerical boundary.
p<-1-1.9*2^-28
d<-data.frame(person=rep(1:4,2),x1=sin(1:8),x2=cos(1:8),x3=1:8/9)
f<-bhmf(A=~x1,B=~x2,C=~x3,cor=list(default='zero',free=list(~A:B),fixed=list(list(pairs=~A:C,value=p))))
z<-bhmm(f,d,subject='person',fit=FALSE);g<-check_model(z);dom<-model_table(z,'correlation_domains')$component
ok(dom$status=='feasible'&&dom$interval_status=='not_assessed'&&g$identification$status=='not_assessed','unresolved endpoint precision preserves declaration with honest rank deferral')
v<-rep(.5,sum(.bh_covariance_layout(z)$counts));v[.bh_covariance_layout(z)$indices$component_cor]<-0.00011897942333204787
err(.bh_covariance_state(z,v),'E_CORRELATION_NUMERIC')
err(stancode(z),'E_CORRELATION_NUMERIC')
# The triangle's repeated determinant boundary is handled on its exact graph.
for(eta in c(.4,.6)){
 q<-bhmm(s$formula,blocks,subjects='shared',prior=set_prior(paste0('lkj(',eta,')'),'cor',layer='parameter'),fit=FALSE)
 ok(check_model(q)$correlation_prior_geometry$parameter$status==if(eta>.5)'proven_proper_prior'else'proven_improper_prior','equicorrelation triangle prior boundary multiplicity is explicit')
}
cat('PASSED',passed,'public boundary checks\n')
