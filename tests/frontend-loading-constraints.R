source(file.path("helpers", "native-guard.R"))
# Exact cell/class maps, priors and diagnostics only; no native backend.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
d<-data.frame(person=rep(1:4,2),x1=sin(1:8*.5),x2=cos(1:8*.7)+1:8/5,x3=sin(1:8*.3)+1:8/7)
prepare<-function(m,prior=NULL)bhmm(m,d,subject='person',prior=prior,fit=FALSE)
fixed<-function(value,cells)list(value=value,cells=cells)
m<-bhmf(A=~x1+x2+x3,loadings=list(equal=list(pair=list(A=~x1+x2)),fixed=list(fixed(-.5,list(A=~x3)))))
s<-prepare(m);cells<-model_table(s,'loading_cells');primitives<-model_table(s,'loading_primitives')
ok(nrow(primitives)==1L&&sum(cells$state=='free')==2L&&sum(cells$state=='fixed')==1L,'one primitive for two aliases plus one fixed cell')
ok(primitives$sign=='free'&&model_table(s,'orientation')$pinned,'fixed nonzero loading pins the whole factor without extra anchor')
state<-private('.bh_covariance_state')(s,private('.bh_covariance_probe')(s,1))
ok(state$Lambda[1,1]==state$Lambda[2,1]&&state$Lambda[3,1]==-.5,'affine covariance map assigns exact equality and fixed negative value')
p<-get_prior(s);proposal<-model_table(s,'loading_prior_calibration')
ok(sum(p$class=='loading')==1L&&p$calibration_scale[p$class=='loading']==max(proposal$proposed_scale),'one inspectable max-member calibration prior')
ok(length(unique(model_table(s,'loading_prior_aliases')$parameter))==1L,'all aliases expose the same unique prior')
dat<-standata(s)
ok(dat$L==1L&&dat$LC==3L&&identical(as.integer(dat$loading_cell_primitive),c(1L,1L,0L)), 'lowering distinguishes one free primitive from three ordinary cells')
if(!private('.bh_coefficient_generator_qualified')()) {
 err(bhmm(s,chains=1,iter=20,warmup=10,execution='experimental',exploratory=TRUE,resources='development'),'E_FRONTEND_BACKEND_UNSUPPORTED')
}else local({
 old<-private('.bh_fit_scalar')
 replace<-function(f)if(source_mode)assign('.bh_fit_scalar',f,.GlobalEnv)else assignInNamespace('.bh_fit_scalar',f,'bmeas')
 on.exit(replace(old));replace(function(...)private('.mp_abort')('TEST_BACKEND_REACHED','terminal cache/sampler double only'))
 err(bhmm(s,chains=1,iter=20,warmup=10,execution='experimental',exploratory=TRUE,resources='development'),'TEST_BACKEND_REACHED')
})
# Crossed alias classes couple sign gauges; only one anchor is legitimate.
crossed<-prepare(bhmf(A=~x1+x2,B=~x1+x2,cor='independent',loadings=list(equal=list(a=list(A=~x1,B=~x2),b=list(A=~x2,B=~x1)))))
p<-model_table(crossed,'loading_primitives');ok(sum(p$sign=='positive_anchor')==1L&&sum(p$sign=='free')==1L,'cross-factor equality receives one common sign gauge')
v<-private('.bh_covariance_probe')(crossed,1);v[private('.bh_covariance_layout')(crossed)$indices$loading]<-c(.8,-.4)
L<-private('.bh_covariance_state')(crossed,v)$Lambda
ok(identical(unname(L),matrix(c(.8,-.4,-.4,.8),2,2)),'opposite-sign crossed state remains legal')
other<-v;other[private('.bh_covariance_layout')(crossed)$indices$loading]<-c(.4,-.8)
ok(max(abs(private('.bh_covariance_state')(crossed,v)$Sigma_predictor-private('.bh_covariance_state')(crossed,other)$Sigma_predictor))<1e-14&&check_model(crossed)$identification$status=='proven_unidentified','crossed classes retain a known global permutation alias despite the one correct sign gauge')
err(stancode(crossed),'E_IDENTIFICATION')
err(bhmm(crossed,chains=1,iter=20,warmup=10,execution='experimental',exploratory=TRUE,resources='development'),'E_IDENTIFICATION')
crossed_cor<-prepare(bhmf(A=~x1+x2,B=~x1+x2,cor='free',loadings=list(equal=list(a=list(A=~x1,B=~x2),b=list(A=~x2,B=~x1)))))
v<-private('.bh_covariance_probe')(crossed_cor,1);layout<-private('.bh_covariance_layout')(crossed_cor)
v[layout$indices$loading]<-c(.8,-.4);v[layout$indices$component_cor]<-.35;other<-v;other[layout$indices$loading]<-c(.4,-.8)
ok(max(abs(private('.bh_covariance_state')(crossed_cor,v)$Sigma_predictor-private('.bh_covariance_state')(crossed_cor,other)$Sigma_predictor))<1e-14&&check_model(crossed_cor)$identification$status=='proven_unidentified','crossed permutation certificate also preserves the two-coordinate free-rho matrix')
pinned<-prepare(bhmf(A=~x1+x2+x3,B=~x1+x2,cor='free',loadings=list(equal=list(a=list(A=~x1,B=~x2),b=list(A=~x2,B=~x1)),fixed=list(fixed(.6,list(A=~x3))))))
ok(is.null(check_model(pinned)$identification$loading_permutation_nonidentification),'an asymmetric fixed extra row does not inherit the mirror-pattern certificate')
# Transitive fixed propagation and rule order have one scientific map.
a<-list(first=list(A=~x1+x2),second=list(A=~x2+x3))
for(eq in list(a,rev(a))) {
 x<-prepare(bhmf(A=~x1+x2+x3,loadings=list(equal=eq,fixed=list(fixed(.4,list(A=~x1))))))
 ok(nrow(model_table(x,'loading_primitives'))==0L&&all(model_table(x,'loading_cells')$fixed_value==.4),'transitive equality propagates a single fixed constant')
 ok(all(private('.bh_covariance_state')(x,private('.bh_covariance_probe')(x,1))$Lambda==.4),'all-fixed map has no dummy coefficient')
}
err(prepare(bhmf(A=~x1+x2,loadings=list(equal=list(e=list(A=~x1+x2)),fixed=list(fixed(1,list(A=~x1)),fixed(2,list(A=~x2)))))), 'E_LOADING_CONFLICT')
err(bhmf(A=~x1,loadings=list(equal=list(e=list(A=~x1+x1)))),'E_DUPLICATE_LOADING')
err(prepare(bhmf(A=~x1+x2,loadings=list(equal=list(e=list(A=~x1))))),'E_LOADING_CONSTRAINT')
err(prepare(bhmf(A=~x1+x2,loadings=list(fixed=list(fixed(0,list(A=~x3)))))),'E_LOADING_COVERAGE')
zero<-prepare(bhmf(A=~x1+x2+x3,loadings=list(fixed=list(fixed(-0,list(A=~x1+x2+x3))))))
ok(nrow(model_table(zero,'loading_primitives'))==0L&&nrow(model_table(zero,'loadings'))==0L&&all(model_table(zero,'target_coverage')$mean_only),'fixed-zero cells preserve declarations with no effective support')
ok(check_model(zero)$identification$status=='local_rank_full_not_global_proof','unused independent latent score is not an unidentified population coefficient')
err(prepare(bhmf(A=~x1+x2+x3,loadings=list(fixed=list(fixed(0,list(A=~x1+x2+x3))))),set_prior('normal(0,1)','loading')),'E_PRIOR_SELECTOR')
# Role-first prior matching and cross-alias conflict policy.
f<-bhmf(A=~x1+x2,B=~x1+x2,cor='independent',loadings=list(fixed=list(fixed(0,list(A=~x1)))))
x<-prepare(f,set_prior('normal(0,.3)','loading',target=~x1))
ok(sum(get_prior(x)$prior=='normal(0,.3)')==1L,'target-only loading prior selects remaining free B cell')
err(prepare(f,set_prior('normal(0,.3)','loading',component='A',target=~x1)),'E_PRIOR_SELECTOR')
f<-bhmf(A=~x1+x2,B=~x1+x2,cor='independent',loadings=list(equal=list(e=list(A=~x1,B=~x1))))
conflict<-c(set_prior('normal(0,1)','loading',component='A'),set_prior('normal(0,2)','loading',component='B'),set_prior('normal(0,.3)','loading',component='A',target=~x1))
err(prepare(f,conflict),'E_PRIOR_CONFLICT')
idempotent_a<-set_prior('normal(0,1)','loading',component='A');idempotent_b<-set_prior('normal(0,1.0)','loading',component='B')
x<-prepare(f,c(idempotent_a,idempotent_b));y<-prepare(f,c(idempotent_b,idempotent_a))
ok(identical(get_prior(x),get_prior(y))&&all(vapply(get_prior(x)$prior[get_prior(x)$class=='loading'],function(p)identical(private('.bh_prior_distribution')(p)$values,c(0,1)),logical(1))),'identical maximal densities are order-independent and counted once')
err(prepare(f,c(set_prior('normal(0,1)','loading'),set_prior('normal(0,2)','loading'))),'E_PRIOR_CONFLICT')
# Basis and owned expressions remain explicit derived cells, including zero rows.
Q<-matrix(c(1,-1,0),3,1,dimnames=list(c('x1','x2','x3'),'q'))
basis<-component(~x1+x2+x3,Q,loading='shared')
f<-bhmf(P=basis,cor='independent')
b<-prepare(f,set_prior('half_normal(1)','amplitude',component='P'))
ok(nrow(model_table(b,'loading_cells'))==3L&&!model_table(b,'loading_cells')$effective_support[3],'zero basis cell remains inspectable and derived')
err(prepare(bhmf(P=basis,loadings=list(fixed=list(fixed(0,list(P=~x3)))))), 'E_LOADING_DERIVED')
# Exact basis alias scope: nonzero fixed at active Q, zero fixed at active Q,
# and nonzero fixed at zero Q have distinct proof outcomes.
for(q in list(c(1,-1,1),c(0,1,-1)))for(value in c(0,.5)) {
 Q<-matrix(q,3,1,dimnames=list(c('x1','x2','x3'),'q'))
 x<-prepare(bhmf(G=~x1+x2+x3,P=component(~x1+x2+x3,Q,loading='shared'),cor='free',loadings=list(fixed=list(fixed(value,list(G=~x1))))),set_prior('half_normal(1)','amplitude',component='P'))
 id<-check_model(x)$identification
 if(q[1]!=0&&value==0)ok(is.null(id$basis_nonidentification)&&all(id$probe_ranks==7L),'fixed zero at active basis row removes shear and ordinary sign alias')else ok(id$status=='proven_unidentified'&&!is.null(id$basis_nonidentification),'resolved fixed/basis map retains the applicable exact alias witness')
}
# Fixed nonzero latent effects block all-scale collapse but permit repeat collapse.
constant<-data.frame(person=1:3,x1=1,x2=c(.2,.6,-.3),x3=c(.9,1.4,.2))
f<-bhmf(A=~x1+x2+x3,loadings=list(fixed=list(fixed(.5,list(A=~x1)))))
x<-bhmm(f,constant,subject='person',fit=FALSE)
ok(check_model(x)$posterior_propriety$status=='no_scoped_collapse_detected','fixed nonzero loading blocks the joint-to-zero collapse path')
y<-bhmm(f,rbind(constant,constant),subject='person',fit=FALSE)
ok(check_model(y)$posterior_propriety$status=='proven_improper'&&any(grepl('exact_within_person',check_model(y)$posterior_propriety$findings$sufficient_paths)),'fixed nonzero person variation retains exact-repeat sigma collapse')
# Safe captured selectors and public persistence.
tasks<-c('x1','x2');m<-bhmf(A=~x1+x2+x3,loadings=list(equal=list(pair=list(A=~scores$mu[tasks]))))
# Explicit qualified loading selectors can bind the same canonical wide keys.
tasks<-'x3';x<-prepare(m);before<-model_table(x,'loading_cells')
for(i in 1:3){x<-unserialize(serialize(x,NULL));ok(identical(model_table(x,'loading_cells'),before),'captured constraint selectors persist without their original variable')}
ok(binomial_model()$parameters$p$predictor_units=='log_odds'&&gaussian_model()$parameters$sigma$predictor_units=='log(response/unit)','builtin raw predictor-unit labels reflect their actual input scale')
long<-data.frame(person=1:4,task='t',y=c(.2,.8,.3,1))
block<-function(units,model=gaussian_model())observe(model,long,'person','task','y',units=units)
formula<-bhmf(A=~a$mu['t']+b$mu['t'],loadings=list(equal=list(tie=list(A=~a$mu['t']+b$mu['t']))))
err(bhmm(formula,list(a=block('points'),b=block('milliseconds')),subjects='shared',fit=FALSE),'E_LOADING_UNITS')
formula<-bhmf(A=~a$mu['t']+b$mu['t'],loadings=list(equal=list(tie=list(A=~a$mu['t']+b$mu['t'])),fixed=list(fixed(.4,list(A=~a$mu['t'])))))
all_fixed<-bhmm(formula,list(a=block('points'),b=block('milliseconds')),subjects='shared',fit=FALSE)
ok(nrow(model_table(all_fixed,'loading_primitives'))==0L&&length(unique(model_table(all_fixed,'loading_cells')$units))==2L,'all-fixed class keeps per-cell units without a shared-prior fiction')
cm<-register_stan_adapter(new_observation_model('custom_normal','1',response_schema(y='real'),
 list(mu=parameter_spec('real','identity','theta','subject_task','points','Location'),sigma=parameter_spec('positive','log','theta','task','points','Noise')),
 stan_density(symbol='constraint_normal_lpdf',arguments=c('y','mu','sigma'),argument_types=rep('real',3),normalized=TRUE,builtin=TRUE,approximation='none'),model_capabilities(fit=TRUE)),
 function_code='real constraint_normal_lpdf(real y, real mu, real sigma) { return normal_lpdf(y | mu, sigma); }',function_exports='constraint_normal_lpdf')
formula<-bhmf(A=~a$mu['t']+b$mu['t'],loadings=list(equal=list(tie=list(A=~a$mu['t']+b$mu['t']))))
missing<-bhmm(formula,list(a=block('points'),b=block('points',cm)),subjects='shared',fit=FALSE)
ok(get_prior(missing)$status[get_prior(missing)$class=='loading']=='missing','one uncalibrated member leaves the whole class prior missing')
specified<-bhmm(formula,list(a=block('points'),b=block('points',cm)),subjects='shared',prior=set_prior('normal(0,.8)','loading',target=~b$mu['t']),fit=FALSE)
ok(get_prior(specified)$prior[get_prior(specified)$class=='loading']=='normal(0,.8)'&&nrow(model_table(specified,'loading_prior_aliases'))==2L,'one alias can explicitly set the unique class prior without multiplication')
err(bhmf(A=~x1,loadings=list(fixed=NULL)),'E_LOADING_CONSTRAINT')
nonvarying<-observe(gaussian_model(),long,'person','task','y',units='points',parameters=list(mu='task'))
err(bhmm(bhmf(A=~a$mu['t'],loadings=list(fixed=list(fixed(0,list(A=~a$mu['t']))))),list(a=nonvarying),fit=FALSE),'E_PARAMETER_LEVEL_CONFLICT')
# Known nonzero latent variance is a constant moment unless another primitive
# or free marginal edge appears in its exact covariance expression.
short<-data.frame(person=rep(1:3,each=2),task=rep(c('x1','x2'),3),y=c(.2,.9,.6,1.4,-.3,.2))
g<-observe(gaussian_model(),short,'person','task','y',units='points')
h<-observe(gaussian_model(),data.frame(person=1:3,task='known',y=c(.3,.7,-.2)),'person','task','y',units='points',parameters=list(sigma=list(fixed=1)))
known<-bhmm(bhmf(G=~g$mu[c('x1','x2')],Known=~h$mu['known'],cor='independent',loadings=list(fixed=list(fixed(.7,list(Known=~h$mu['known']))))),list(g=g,h=h),subjects='shared',fit=FALSE)
id<-check_model(known)$identification
ok(id$status=='proven_unidentified'&&id$observable_covariance_moments==3L&&id$free_covariance_parameters==4L,'independent fixed latent covariance cannot dilute an existing dimension proof')
k<-observe(gaussian_model(),data.frame(person=1:3,task='other',y=c(.1,.9,-.4)),'person','task','y',units='points',parameters=list(sigma=list(fixed=.8)))
edge<-bhmm(bhmf(A=~h$mu['known'],B=~k$mu['other'],cor='free',loadings=list(fixed=list(fixed(.7,list(A=~h$mu['known'])),fixed(.5,list(B=~k$mu['other']))))),list(h=h,k=k),subjects='shared',fit=FALSE)
id<-check_model(edge)$identification
ok(id$observable_covariance_moments==1L&&id$free_covariance_parameters==1L&&all(id$probe_ranks==1L),'fixed loadings still expose a genuinely free marginal correlation moment')
direct<-prepare(bhmf(residual=list(targets=~x1+x2+x3,cor='free')))
ok(nrow(model_table(direct,'components'))==0L&&nrow(model_table(direct,'loading_cells'))==0L&&all(check_model(direct)$identification$probe_ranks==9L),'K0 direct covariance retains its empty ordinary map')
task_only_block<-observe(gaussian_model(),long,'person','task','y',units='points',parameters=list(mu='task',sigma=list(fixed=1)))
task_only<-bhmm(bhmf(),list(a=task_only_block),fit=FALSE)
ok(nrow(model_table(task_only,'loading_cells'))==0L&&sum(get_prior(task_only)$parameter_count)==1L,'empty component map still allows a free task mean')
Q<-matrix(0,3,1,dimnames=list(c('x1','x2','x3'),'q'))
zero_basis<-prepare(bhmf(P=component(~x1+x2+x3,Q,loading='shared')),set_prior('half_normal(1)','amplitude',component='P'))
ok(check_model(zero_basis)$identification$status=='proven_unidentified'&&nrow(model_table(zero_basis,'loading_cells'))==3L,'all-zero basis reports its unused amplitude through a valid inspectable map')
canonical_rules<-list(set_prior('normal(0,1)','loading'),set_prior('normal(0,1e0)','loading'),set_prior('normal(-0,1.0)','loading'))
for(order in list(c(1,2,3),c(1,3,2),c(2,1,3),c(2,3,1),c(3,1,2),c(3,2,1))) {
 x<-prepare(bhmf(A=~x1+x2+x3),do.call(c,canonical_rules[order]))
 ok(all(get_prior(x)$prior[get_prior(x)$class=='loading']=='normal(0,1)'),'canonical coalesced prior text is invariant to negative-zero spelling and rule order')
}
captured<-model_table(s,'loading_constraint_rules')
ok('captured_selector'%in%names(captured)&&all(vapply(captured$captured_selector,is.list,logical(1))),'resolved constraint rules retain their captured selector values')
conflict_error<-tryCatch(prepare(bhmf(A=~x1+x2+x3),c(set_prior('normal(0,1)','loading'),set_prior('normal(0,2)','loading'))),error=identity)
ok(grepl('rule #1',conditionMessage(conflict_error),fixed=TRUE)&&grepl('rule #2',conditionMessage(conflict_error),fixed=TRUE),'prior conflict identifies every competing rule')
cat(passed,'loading constraint R/IR checks passed; no compiler or sampler invoked\n')
