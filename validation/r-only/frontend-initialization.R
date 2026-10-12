source(file.path("helpers", "native-guard.R"))
# Deterministic prior/support/operand starts; no compilation or sampling.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
d<-data.frame(person=rep(1:4,2),x1=sin(1:8*.5),x2=cos(1:8*.7)+1:8/5,x3=sin(1:8*.3)+1:8/7)
model<-function(value)bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1,value=value)))
prepare<-function(value,prior=NULL)bhmm(model(value),d,subject='person',prior=prior,fit=FALSE)
for(value in c(1,-2,1e200,1e-200,1e300,1e-300)) {
 s<-prepare(value);checks<-check_model(s);initial<-private('.bh_backend_ir')(s)$init;pd<-private('.bh_prior_data')(s)
 ok(checks$initialization$status=='screened','reciprocal marker units have a finite supported initialization')
 ok(identical(as.vector(initial$component_sd),unname(pd$component_sd_scale))&&identical(dim(initial$component_sd),1L)&&all(initial$loading_free==0),'resolved SD prior and signed beta center determine marked starts with declared vector rank')
 ok(all(is.finite(stats::dnorm(initial$component_sd,0,pd$component_sd_scale,log=TRUE)))&&all(is.finite(stats::dnorm(initial$loading_free,0,pd$loading_scale,log=TRUE))),'independent Normal log densities are finite in both rescaling directions')
 ok(is.finite(value*initial$component_sd)&&value*initial$component_sd!=0,'required effective marker loading remains represented')
 if(abs(value)!=1&&abs(value)!=2)ok(is.list(private('.bh_execution_gate')(s)),'extreme initial representation does not imply an additional HMC claim')
}
s<-prepare(1e200,set_prior('half_normal(1e-199)','component_sd',component='G'))
ok(private('.bh_backend_ir')(s)$init$component_sd==1e-199,'explicit resolved component prior overrides default initializer scale')
u<-bhmm(bhmf(G=~x1+x2+x3),d,subject='person',fit=FALSE);init<-private('.bh_backend_ir')(u)$init
ok(all(init$loading_anchor==.5)&&all(init$loading_free==.5)&&all(init$obs_sd==1),'finite legacy unit-SD starts are preserved')
tiny<-bhmm(bhmf(G=~x1+x2+x3),d,subject='person',prior=set_prior('normal(0,1e-200)','loading'),fit=FALSE)
i<-private('.bh_backend_ir')(tiny)$init
ok(i$loading_anchor==1e-200&&all(i$loading_free==0),'unit-SD positive and signed loading fallback uses resolved priors')
small_sigma<-prepare(1,set_prior('half_normal(1e-200)','sigma'))
ok(all(private('.bh_backend_ir')(small_sigma)$init$obs_sd==1e-200),'observation SD also receives finite prior-scale fallback')
small_sd<-prepare(1,set_prior('half_normal(1e-200)','component_sd'))
ok(check_model(small_sd)$initialization$status=='screened','optional underflowed covariance export does not reject a finite effective-loading start')
for(prior in list(set_prior('normal(0,1e-320)','loading'),set_prior('half_student_t(4,0,1e-200)','component_sd'))) {
 s<-prepare(1,prior);before<-serialize(s,NULL);checks<-check_model(s)
 ok(checks$initialization$status=='numerically_unavailable'&&length(checks$initialization$reasons)>0L,'unsupported initial arithmetic remains explicit and inspectable')
 ok(length(stancode(s))>0L&&is.list(standata(s))&&identical(before,serialize(s,NULL)),'initialization failure does not change priors/data or block code inspection')
 err(bhmm(s,chains=1,iter=20,warmup=10,execution='experimental',exploratory=TRUE,resources='development'),'E_INITIALIZATION_NUMERICAL')
}
d4<-d;d4$x4<-sin(1:8*.9)+1:8/9
aggregate<-bhmm(bhmf(G=~x1+x2+x3+x4),d4,subject='person',prior=set_prior('half_normal(1e-154)','sigma'),fit=FALSE)
a<-check_model(aggregate)$initialization
ok(a$status=='numerically_unavailable'&&any(grepl('aggregate primitive prior',a$reasons)),'individually finite priors cannot hide a nonfinite summed initial target')
ok(is.list(standata(aggregate))&&is.character(stancode(aggregate)),'aggregate numeric failure remains inspectable')
err(bhmm(aggregate,chains=1,iter=20,warmup=10,execution='experimental',exploratory=TRUE,resources='development'),'E_INITIALIZATION_NUMERICAL')
# The final check is an execution-policy call; it precedes any backend call.
block<-observe(gaussian_model(),data.frame(person=1:3,task='x',y=c(.1,.6,1.4)),subject='person',task='task',fields=list(y='y'),units='points',parameters=list(mu='task',sigma=list(fixed=1)))
s<-bhmm(bhmf(),list(scores=block),fit=FALSE);i<-private('.bh_backend_ir')(s)$init
ok(identical(names(i),'alpha')&&check_model(s)$initialization$status=='screened','task-only alpha has no invented scale or person coordinates')
block<-observe(gaussian_model(),data.frame(person=1:3,task='x',y=c(.1,.6,1.4)),subject='person',task='task',fields=list(y='y'),units='points',parameters=list(mu=list(fixed=0),sigma=list(fixed=1)))
s<-bhmm(bhmf(),list(scores=block),fit=FALSE)
ok(length(private('.bh_backend_ir')(s)$init)==0L,'fully fixed likelihood retains a truly empty initializer')
err(bhmm(s,chains=1,iter=20,warmup=10,resources='development'),'E_NO_FREE_PARAMETERS')
cat('Passed',passed,'initialization checks\n')
