source(file.path("helpers", "native-guard.R"))
# Centered/raw/task R/IR and bounded runtime controls. Terminal doubles only; no compilation or sampling.
source_mode<-nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))
if(source_mode)for(f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"),"R"),full.names=TRUE))source(f)else library(bmeas)
if(!source_mode)for(n in c(".bh_centering_basis",".bh_centering_covariance",".bh_covariance_layout",".bh_covariance_probe",".bh_covariance_state",".bh_raw_correlation_inverse",".bh_raw_correlation_state",".bh_task_state"))assign(n,getFromNamespace(n,"bmeas"),envir=.GlobalEnv)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
replace<-function(n,f)if(source_mode)assign(n,f,.GlobalEnv)else assignInNamespace(n,f,'bmeas')
count<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);count<<-count+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);if(!inherits(e,'error'))stop('no error: ',code);ok(identical(e$code,code),paste(code,conditionMessage(e)))}
d<-data.frame(person=rep(as.character(1:4),2),x1=sin(1:8*.5),x2=cos(1:8*.7)+1:8/5,x3=sin(1:8*.3)+1:8/7,x4=cos(1:8*.9))
ordinary<-bhmm(bhmf(G=~x1+x2+x3),d,subject='person',fit=FALSE)
ok(check_model(ordinary)$backend$status=='deterministic_gaussian_qualified','default iid profile stays qualified in its bounded scope')
ok(nrow(standata(ordinary)$basis_multiplier)==3L,'default code/data lowering remains available')
# Noncontiguous partition; all scientific loadings are known constants.
m<-bhmf(A=~x1,B=~x2,C=~x3,D=~x4,
 loadings=list(fixed=list(list(value=1,cells=list(A=~x1,B=~x2,C=~x3,D=~x4)))),
 cor=list(default='free',zero=list(~A+C),coordinates=list(type='ordered_blocks',blocks=list(first=~C+A,second=~D,third=~B))))
p<-c(set_prior('normal(0,.2)','cor_raw',block='second'),set_prior('normal(0,.14)','cor_raw',block='third'))
s<-bhmm(m,d,subject='person',prior=p,fit=FALSE)
ok(identical(model_table(s,'correlation_coordinate_system')$ordered_global_indices,c(3L,1L,4L,2L)),'noncontiguous order retained')
raw<-model_table(s,'raw_correlation_coordinates');ok(nrow(raw)==5L&&identical(raw$block,c('second','second','third','third','third')),'five raw cells in column-major block order')
ok(sum(get_prior(s)$class=='cor_raw')==5L&&!any(get_prior(s)$class=='cor'),'unique raw priors replace common final-R prior')
x<-c(.2,-.3,.4,-.15,.1);R<-.bh_raw_correlation_state(s,x)$R
ok(abs(R[1,3])<1e-14&&max(abs(unname(.bh_raw_correlation_inverse(s,R))-x))<1e-13,'raw forward/inverse uses block/global permutation')
ok(check_model(s)$identification$status=='local_rank_full_not_global_proof','raw primitive diagnostic maps B to R')
ok(grepl('correlation_B',as.character(stancode(s)),fixed=TRUE),'raw coordinate source is inspectable without native qualification')
missing<-bhmm(m,d,subject='person',fit=FALSE);ok(sum(get_prior(missing)$status=='missing')==5L,'raw missing priors remain inspectable')
err(bhmm(m,d,subject='person',prior=c(p,set_prior('normal(0,1)','cor_raw',block='first')),fit=FALSE),'E_PRIOR_SELECTOR')
err(bhmm(m,d,subject='person',prior=set_prior('lkj(2)','cor'),fit=FALSE),'E_PRIOR_COORDINATES')
err(set_prior('normal(1,1)','cor_raw'),'E_PRIOR')
# Centered direct predictor covariance, explicit unobserved roster member.
roster<-c('4','2','1','3','unseen')
cm<-bhmf(residual=list(targets=~x1+x2),person=list(distribution='sample_centered',ids=roster),augmentation=list(type='task_cholesky',targets=~x2+x1))
cs<-bhmm(cm,d[,1:3],subject='person',fit=FALSE)
a<-model_table(cs,'augmentation');ok(a$task_white_coordinates==8L&&a$sampled_common_coordinates==0L&&a$sampled_parameter_coordinates==0L,'task augmentation replaces both person blocks')
ok(identical(cs$data_map$subject_keys,paste0('study::',roster))&&sum(model_table(cs,'person_roster')$observed)==4L,'explicit roster retains order and unobserved person')
ok(sum(get_prior(cs)$class=='structural_latent')==1L&&get_prior(cs)$parameter_count[get_prior(cs)$class=='structural_latent']==8L,'one centered-white prior role counts N minus one')
H<-.bh_centering_covariance(cs);ok(max(abs(rowSums(H)))<1e-14&&all(diag(H)==1)&&qr(H)$rank==4L,'centered covariance law')
ok(check_model(cs)$identification$status=='not_assessed'&&check_model(cs)$posterior_propriety$status=='not_assessed','centered active law does not inherit iid noise proofs')
ok(identical(model_table(unserialize(serialize(cs,NULL))),model_table(cs)),'captured reference context persistence')
err(bhmf(person=list(distribution='sample_centered',ids='1'),augmentation=list(type='task_cholesky',targets=~0)),'E_PERSON_ROSTER')
excluded<-d[,1:3];excluded$x1[excluded$person=='4']<-NA;excluded$x2[excluded$person=='4']<-NA
ce<-bhmm(cm,excluded,subject='person',missing='observed_only',fit=FALSE)
ok(nrow(model_table(ce,'person_roster'))==5L&&sum(model_table(ce,'person_roster')$observed)==3L,'exclusion retains declared roster')
unknown<-excluded;unknown$person[unknown$person=='4']<-'outside'
err(bhmm(cm,unknown,subject='person',missing='observed_only',fit=FALSE),'E_PERSON_ROSTER')
# Empty active domain preserves free mean/noise checks. iid N1 never evaluates N/(N-1).
long<-data.frame(person=c('1','2','3'),task='t',y=0,obs=paste0('o',1:3))
b<-observe(gaussian_model(),long,'person','task','y',units='points',observation_id='obs',parameters=list(mu='task'))
np<-c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(1)','sigma'))
nm<-bhmf(cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list())),person=list(distribution='sample_centered',ids=c('1','2','3')),augmentation=list(type='task_cholesky',targets=~0))
ns<-bhmm(nm,list(score=b),prior=np,fit=FALSE)
ok(check_model(ns)$posterior_propriety$status=='proven_improper'&&model_table(ns,'augmentation')$task_white_coordinates==0L,'empty centered domain preserves Gaussian collapse proof')
one<-observe(gaussian_model(),long[1,,drop=FALSE],'person','task','y',units='points',parameters=list(mu='task',sigma=list(fixed=1)))
one_s<-bhmm(bhmf(),list(score=one),prior=set_prior('normal(0,1)','Intercept'),fit=FALSE)
ok(is.na(model_table(one_s,'augmentation')$raw_coordinate_variance)&&model_table(one_s,'person_law')$roster_size==1L,'iid N1 has no centered correction')
cat('CORE',count,'checks passed\n')
count<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);count<<-count+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'error')&&identical(e$code,code),code)}
person<-list(distribution='sample_centered',ids=c('p2','p1','p3'));aug<-list(type='task_cholesky')
err(bhmf(G=~x1,person=person),'E_REFERENCE_COMBINATION')
err(bhmf(G=~x1,augmentation=aug),'E_REFERENCE_COMBINATION')
for(ids in list(c('p','p'),c('p',NA),1:3,structure(c('p1','p2'),class='special'),structure(c('p1','p2'),hidden=new.env())))err(bhmf(person=list(distribution='sample_centered',ids=ids),augmentation=aug),'E_PERSON_ROSTER')
err(bhmf(A=~x1,B=~x2,cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(both=~A+B)))),'E_CORRELATION_COORDINATE_MASK')
err(bhmf(A=~x1,B=~x2,cor=list(default='zero',coordinates=list(type='ordered_blocks',blocks=list(one=~A,two=~B)))),'E_CORRELATION_COORDINATE_MASK')
err(bhmf(A=~x1,B=~x2,cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(one=~A,two=~A)))),'E_CORRELATION_BLOCKS')
err(bhmf(A=~x1,cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list()))),'E_CORRELATION_BLOCKS')
err(bhmf(cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(empty=~0)))),'E_CORRELATION_SYNTAX')
err(set_prior('normal(0,1)','cor_raw',target=~x1),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','cor_raw',component='G'),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','cor_raw',layer='parameter'),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','loading',block='a'),'E_PRIOR_SELECTOR')
d<-data.frame(person=c('p1','p2','p3'),x1=c(.1,.2,.4),x2=c(.4,-.2,.8),x3=c(.2,.6,-.3))
err(bhmm(bhmf(G=~x1+x2,person=person,augmentation=aug),d[,1:3],subject='person',fit=FALSE),'E_TASK_AUGMENTATION_PD')
err(bhmf(residual=list(targets=~x1),person=person,augmentation=list(type='task_cholesky',targets=~0+x1)),'E_FORMULA')
zm<-bhmf(G=~x1+x2,loadings=list(fixed=list(list(value=0,cells=list(G=~x2)))),residual=list(targets=~x1),person=person,augmentation=list(type='task_cholesky',targets=~x1+x2))
err(bhmm(zm,d[,1:3],subject='person',fit=FALSE),'E_AUGMENTATION_TARGET')
# One block has no raw coordinates or raw prior, with no fabricated component.
one<-bhmf(A=~x1,cor=list(default='zero',coordinates=list(type='ordered_blocks',blocks=list(all=~A))))
os<-bhmm(one,d[,1:2],subject='person',fit=FALSE)
ok(nrow(model_table(os,'raw_correlation_coordinates'))==0L&&!any(get_prior(os)$class=='cor_raw'),'single identity block has no raw primitive')
err(bhmm(one,d[,1:2],subject='person',prior=set_prior('normal(0,1)','cor_raw'),fit=FALSE),'E_PRIOR_SELECTOR')
# Crosswalk roster uses public global IDs, not local or namespace strings.
long<-data.frame(local=c('l1','l2','l3'),task='t',y=c(.1,.4,.2))
b<-observe(gaussian_model(),long,'local','task','y',units='points')
cw<-data.frame(model='score',local_id=long$local,person_id=c('p1','p2','p3'))
m<-bhmf(residual=list(targets=~score$mu['t']),person=person,augmentation=aug)
s<-bhmm(m,list(score=b),subjects=subject_crosswalk(cw,'study; special'),fit=FALSE)
ok(identical(model_table(s,'person_roster')$person_id,c('p2','p1','p3'))&&identical(s$data_map$blocks$score$.person_index,c(2L,1L,3L)),'crosswalk public roster/order are separate from namespace')
# Block-specific prior refinement, all rules first.
rm<-bhmf(A=~x1,B=~x2,C=~x3,cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(a=~A,b=~B,c=~C))))
rules<-c(set_prior('normal(0,.7)','cor_raw'),set_prior('normal(0,.2)','cor_raw',block='b'))
a<-bhmm(rm,d,subject='person',prior=rules,fit=FALSE);b<-bhmm(rm,d,subject='person',prior=c(set_prior('normal(0,.2)','cor_raw',block='b'),set_prior('normal(0,.7)','cor_raw')),fit=FALSE)
ok(identical(get_prior(a),get_prior(b)),'raw block prior resolution order independent')
pr<-get_prior(a);ok(all(pr$prior[pr$class=='cor_raw'&pr$block=='b']=='normal(0,.2)')&&all(pr$prior[pr$class=='cor_raw'&pr$block=='c']=='normal(0,.7)'),'explicit block prior refines fallback')
# This residual-only centered/final-marginal mode requires explicit experimental
# execution. Public dispatch ends at the fitter double, before any backend work.
local({
 old_fit<-private('.bh_fit_scalar');old_compile<-private('.bh_compile_scalar')
 fit_calls<-compile_calls<-0L;plans<-list();before<-serialize(s,NULL,version=2)
 on.exit({replace('.bh_fit_scalar',old_fit);replace('.bh_compile_scalar',old_compile)},add=TRUE)
 abort<-private('.mp_abort')
 replace('.bh_fit_scalar',function(spec,plan,...) {
  fit_calls<<-fit_calls+1L;plans[[fit_calls]]<<-plan
  abort('E_TERMINAL_FITTER','expected terminal fitter boundary')
 })
 replace('.bh_compile_scalar',function(...) {compile_calls<<-compile_calls+1L;abort('E_TERMINAL_COMPILER','unexpected compiler boundary')})
 ok(check_model(s)$backend$status=='experimental_centered_task_final_marginal','crosswalk residual-only task mode has its bounded final-marginal profile')
 for(execution in c('qualified','experimental'))for(exploratory in c(FALSE,TRUE)) {
  warnings<-character();before_calls<-fit_calls
  e<-tryCatch(withCallingHandlers(bhmm(s,chains=1,iter=20,warmup=10,execution=execution,exploratory=exploratory,resources='development'),
    warning=function(w){warnings<<-c(warnings,w$code);invokeRestart('muffleWarning')}),error=identity)
  expected<-if(execution=='experimental')'E_TERMINAL_FITTER'else'E_EXECUTION_PROFILE'
  ok(inherits(e,'measurement_error')&&identical(e$code,expected),paste('crosswalk',execution,exploratory,'returns',expected))
  ok(fit_calls==before_calls+as.integer(execution=='experimental'),paste('crosswalk',execution,exploratory,'reaches the fitter only with explicit experimental opt-in'))
  if(execution=='experimental') {
   plan<-plans[[fit_calls]]
   ok('W_EXPERIMENTAL_EXECUTION'%in%warnings&&plan$execution_profile$used=='experimental_centered_task_final_marginal'&&identical(plan$exploratory,exploratory)&&!plan$exploratory_rank_override&&plan$identification$status=='not_assessed','crosswalk execution preserves warnings and unassessed identification without inventing a rank override')
  }
 }
 ok(fit_calls==2L&&compile_calls==0L&&identical(before,serialize(s,NULL,version=2)),'crosswalk controls preserve sealed bytes and stop at exactly two terminal fitter calls')
})
cat('GUARDS',count,'checks passed\n')
count<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);count<<-count+1L;cat('PASS',label,'\n')}
d<-data.frame(person=as.character(1:3),x1=c(.1,.5,-.2),x2=c(-.3,.4,.7),x3=c(.8,-.4,.2))
owner<-list(type='variance_share',targets=~x1+x2+x3,components=~G,residual=TRUE,shares=~1)
m<-bhmf(G=~x1+x2+x3,decomposition=list(all=owner),person=list(distribution='sample_centered',ids=as.character(1:3)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3))
s<-bhmm(m,d,subject='person',fit=FALSE);v<-.bh_covariance_probe(s,1);ix<-.bh_covariance_layout(s)$indices
v[ix$total_sd]<-c(.7,1.2,.9);v[ix$share]<- -qlogis(.36)/sqrt(2)
white<-sqrt(3/2)*rbind(c(.4,-.3),c(-.7,.5),c(.2,.8));alpha<-c(.1,-.2,.3)
a<-.bh_task_state(s,v,alpha,white)
S0<-matrix(.36,3,3)+diag(.64,3)
ok(max(abs(a$working_covariance-S0))<1e-14,'normalized covariance built from direct owned maps')
full<-.bh_covariance_state(s,v)$Sigma_predictor;Z<-white%*%t(.bh_centering_basis(3))
ok(max(abs(t(t(chol(full))%*%Z)-a$delta))<1e-13,'row-scaled task map equals direct positive Cholesky at moderate states')
ok(max(abs(colSums(a$delta)))<1e-14&&max(abs(tcrossprod(.bh_centering_basis(3))-diag(3)+matrix(1/3,3,3)))<1e-14,'exact roster centering and isometric basis')
v[ix$total_sd]<-1e-200;tiny<-.bh_task_state(s,v,alpha,white)
ok(all(is.finite(tiny$delta))&&sum(tiny$delta!=0)==9L,'tiny owned scales retain nine representable task deviations')
ok(all(is.na(diag(tiny$Sigma_predictor)))&&!any(diag(tiny$Sigma_predictor_available)),'underflowed covariance exports are unavailable rather than zero')
ok(all(tiny$effective_loadings!=0)&&max(abs(tiny$working_covariance-S0))<1e-14,'normalized covariance never divides damaged full maps')
# Only symbolic active targets get task whites; a numeric free-loading zero remains active.
zm<-bhmf(G=~x1+x2+x3,loadings=list(fixed=list(list(value=0,cells=list(G=~x3)),list(value=1,cells=list(G=~x1)))),residual=list(targets=~x1+x2),person=list(distribution='sample_centered',ids=as.character(1:3)),augmentation=list(type='task_cholesky',targets=~x2+x1))
zs<-bhmm(zm,d,subject='person',fit=FALSE);zv<-.bh_covariance_probe(zs,1);zv[.bh_covariance_layout(zs)$indices$loading]<-0
z<-.bh_task_state(zs,zv,alpha,matrix(.3,2,2))
ok(length(z$task_target)==2L&&all(z$eta[,3]==alpha[3]),'exact zero row stays mean-only with its free mean')
ok(all(model_table(zs,'task_target_domain')$has_symbolic_support==c(TRUE,TRUE,FALSE)),'symbolic support ignores the actual zero free-loading value')
# K0 and I0 dimensions are genuine empties.
km<-bhmf(residual=list(targets=~x1+x2),person=list(distribution='sample_centered',ids=as.character(1:3)),augmentation=list(type='task_cholesky'))
ks<-bhmm(km,d[,1:3],subject='person',fit=FALSE);kv<-.bh_covariance_probe(ks,1)
k<-.bh_task_state(ks,kv,alpha[1:2],matrix(.2,2,2))
ok(identical(dim(k$effective_loadings),c(2L,0L))&&identical(dim(k$delta),c(3L,2L)),'K0 direct residual task factor')
long<-data.frame(person=as.character(1:3),task='t',y=0)
b<-observe(gaussian_model(),long,'person','task','y',units='points',parameters=list(mu='task'))
im<-bhmf(person=list(distribution='sample_centered',ids=as.character(1:3)),augmentation=list(type='task_cholesky',targets=~0))
is<-bhmm(im,list(score=b),prior=c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(1)','sigma')),fit=FALSE)
iv<-.bh_covariance_probe(is,1);i<-.bh_task_state(is,iv,.4)
ok(identical(dim(i$z_task),c(0L,3L))&&identical(dim(i$eta),c(3L,1L))&&all(i$eta==.4),'I0 retains means without dummy task whites')
ok(check_model(is)$posterior_propriety$status=='proven_improper','I0 covariance helper does not erase mean/noise collapse proof')
# Mixed owned/unowned rows and correlated parameter residuals use d=(tau,tau,1).
owner2<-list(type='variance_share',targets=~x1+x2,components=~G,residual=TRUE,shares=~1)
mm<-bhmf(G=~x1+x2+x3,decomposition=list(left=owner2),residual=list(targets=~x3,cor='free'),person=list(distribution='sample_centered',ids=as.character(1:3)),augmentation=list(type='task_cholesky'))
ms<-bhmm(mm,d,subject='person',fit=FALSE);mv<-.bh_covariance_probe(ms,2);mi<-.bh_covariance_layout(ms)$indices
mv[mi$total_sd]<-c(.7,1.2);mv[mi$loading]<- -.4;mv[mi$parameter_cor]<-c(.1,-.15,.2)
mt<-.bh_task_state(ms,mv,alpha,white);mf<-.bh_covariance_state(ms,mv)
ok(identical(mt$working_row_scale,c(.7,1.2,1)),'unowned row scale remains exactly one')
ok(max(abs(mt$Sigma_predictor-mf$Sigma_predictor))<1e-13&&max(abs(mt$delta-t(t(chol(mf$Sigma_predictor))%*%Z)))<1e-13,'mixed owned/unowned correlated-residual Gram path preserves complete covariance and deviations')
cat('TASK',count,'checks passed\n')

if(!source_mode)for(n in c(".bh_cache_scalar_fit"))assign(n,getFromNamespace(n,"bmeas"),envir=.GlobalEnv)
count<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);count<<-count+1L;cat('PASS',label,'\n')}
d<-data.frame(person=as.character(1:3),x1=c(.2,-.1,.4),x2=c(.5,.1,.2),x3=c(.6,.2,-.4))
s<-bhmm(bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1,value=1))),d,subject='person',fit=FALSE)
text<-paste(capture.output(print(s)),collapse=' ')
ok(grepl('Person law: iid',text,fixed=TRUE)&&grepl('1 explicit free component SDs',text,fixed=TRUE),'marker print separates standardized variance and free component scale')
cm<-bhmf(residual=list(targets=~x1+x2),person=list(distribution='sample_centered',ids=as.character(1:3)),augmentation=list(type='task_cholesky'))
cs<-bhmm(cm,d[,1:3],subject='person',fit=FALSE);text<-paste(capture.output(print(cs)),collapse=' ')
ok(grepl('sample_centered',text,fixed=TRUE)&&grepl('4 task-white basis coordinates',text,fixed=TRUE)&&!grepl('iid people',text,fixed=TRUE),'centered printer reports actual person law and task-white count')
ok(grepl('Posterior normalizability:',text,fixed=TRUE)&&grepl('likelihood identification:',text,fixed=TRUE)&&grepl('Execution qualification:',text,fixed=TRUE),'three diagnostic dimensions print separately')
long<-data.frame(person=as.character(1:3),task='t',y=0)
b<-observe(gaussian_model(),long,'person','task','y',units='points',parameters=list(mu='task'))
is<-bhmm(bhmf(person=list(distribution='sample_centered',ids=as.character(1:3)),augmentation=list(type='task_cholesky',targets=~0)),list(score=b),prior=c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(1)','sigma')),fit=FALSE)
p<-check_model(is)$posterior_propriety
ok(p$status=='proven_improper'&&p$person_law_certificate=='no_active_person_effect'&&grepl('centered roster is retained',p$scope,fixed=TRUE),'empty-I proof reports its actual eligibility certificate')
rm<-bhmf(A=~x1,B=~x2,cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(a=~A,b=~B))))
rs<-bhmm(rm,d[,1:3],subject='person',prior=set_prior('normal(0,.2)','cor_raw',block='b'),fit=FALSE)
r<-model_table(rs,'correlations');ok(r$estimand=='derived_final_marginal_correlation'&&is.na(r$primitive_parameter),'raw-family final R rows are explicitly derived')
ok(!any(model_table(rs,'parameters')$parameter==r$parameter)&&sum(get_prior(rs)$class=='cor_raw')==1L,'derived R entry has no duplicate primitive or prior')
# Inspect the real cache helper's requests and role assignment through an R-only
# backend double. Optional diagnostic draws terminate at explicit traps.
local({
 before<-serialize(cs,NULL,version=2);requests<-draw_requests<-list()
 backend<-list(summary=function(variables,...) {
  requests[[length(requests)+1L]]<<-variables
  columns<-unlist(lapply(variables,function(v) {
   if(v=='R_parameter')paste0(v,'[',rep(1:2,2),',',rep(1:2,each=2),']')
   else if(v%in%c('alpha','obs_sd','parameter_sd','parameter_row_sd'))paste0(v,'[',1:2,']')else v
  }),use.names=FALSE)
  means<-rep(0,length(columns));means[columns%in%c('R_parameter[1,1]','R_parameter[2,2]')]<-1
  data.frame(variable=columns,mean=means,median=means,sd=.1,q5=means-.2,q95=means+.2,rhat=1.5,ess_bulk=5,ess_tail=4,mcse_mean=.1)
 },draws=function(variables,...) {
  draw_requests[[length(draw_requests)+1L]]<<-variables
  stop('expected terminal diagnostic draw trap')
 },diagnostic_summary=function(...)list(num_divergent=0L,num_max_treedepth=0L,ebfmi=.4),output=function()character())
 cache<-private('.bh_cache_scalar_fit')(backend,cs,list());tab<-cache$parameter_summary
 expected<-private('.bh_reference_summary_map')(cs)
 ok(length(requests)==2L&&all(expected$variable%in%requests[[1L]])&&identical(requests[[2L]],'R_parameter'),'actual final-marginal cache requests task coordinates once and an expanded residual matrix separately')
 requested<-unlist(c(requests,draw_requests),use.names=FALSE)
 ok(!any(grepl('^(z_raw|e_raw|z|f)(\\[|$)|^correlation_B',requested)),'actual centered cache never requests absent iid/common-person score variables')
 ok(sum(tab$parameter_role=='task_white_basis_coordinate')==4L&&sum(tab$parameter_role=='derived_centered_task_white_score')==6L&&sum(tab$parameter_role=='derived_task_deviation')==6L,'actual cache keeps four sampled white axes distinct from six centered whites and six deviations')
 at<-match(expected$variable,tab$variable)
 ok(!anyNA(at)&&identical(tab$parameter_key[at],expected$parameter_key)&&identical(tab$parameter_role[at],expected$parameter_role)&&all(is.na(tab$reference_person_id[tab$parameter_role=='task_white_basis_coordinate'])),'cache assigns semantic task roles without invented basis-axis person IDs')
 ok(cache$expanded_correlation_matrices$status=='available'&&all(cache$expanded_correlation_matrices$entries$role=='fixed_correlation_constant')&&all(is.na(cache$expanded_correlation_matrices$entries$sd)),'identity residual matrix has fixed values without fabricated posterior uncertainty')
 ok(length(draw_requests)==2L&&cache$admissibility$status=='not_assessed'&&cache$optional_reports$status=='not_assessed'&&cache$inference$status=='not_assessed','terminal diagnostic traps retain explicit unassessed evidence without promoting the double to inference')
 ok(identical(cache$reference_law$person_roster,cs$tables$person_roster)&&identical(before,serialize(cs,NULL,version=2)),'actual reference cache preserves the roster and sealed scientific context')
})
cat('REPORTING',count,'checks passed\n')

zero<-bhmm(bhmf(G=~x1,loadings=list(fixed=list(list(value=0,cells=list(G=~x1)))),person=list(distribution="sample_centered",ids=as.character(1:3)),augmentation=list(type="task_cholesky",targets=~0)),d[,1:2],subject="person",fit=FALSE)
text<-paste(capture.output(print(zero)),collapse=" ")
ok(grepl("1 declared subject-task targets; 0 symbolically active person-effect targets",text,fixed=TRUE),"structural zero print preserves declared role while reporting empty active support")

if(!source_mode)for(n in c(".bh_centering_basis",".bh_covariance_layout",".bh_covariance_probe",".bh_covariance_state",".bh_raw_correlation_state",".bh_task_state"))assign(n,getFromNamespace(n,"bmeas"),envir=.GlobalEnv)
count<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);count<<-count+1L;cat('PASS',label,'\n')}
d<-data.frame(person=as.character(1:4),x1=c(.2,.6,.3,-.1),x2=c(.4,.8,-.2,.1),x3=c(.7,.2,.3,-.4),x4=c(.1,-.3,.5,.2))
m<-bhmf(A=~x1,B=~x2,C=~x3,D=~x4,loadings=list(fixed=list(list(value=1,cells=list(A=~x1,B=~x2,C=~x3,D=~x4)))),
 residual=list(targets=~x1+x2+x3+x4),cor=list(default='free',zero=list(~A+C),coordinates=list(type='ordered_blocks',blocks=list(first=~C+A,second=~D,third=~B))),
 person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x3+x1+x4+x2))
s<-bhmm(m,d,subject='person',prior=set_prior('normal(0,.2)','cor_raw'),fit=FALSE);ix<-.bh_covariance_layout(s)$indices
v<-.bh_covariance_probe(s,1);v[ix$component_cor]<-c(.3,-.2,.15,.4,-.1);white<-matrix(.2*sin(1:12),4,3)
a<-.bh_task_state(s,v,rep(0,4),white);r<-.bh_raw_correlation_state(s,v[ix$component_cor],global_cholesky=FALSE)
ok(is.null(r$L_global)&&max(abs(tcrossprod(r$F_global)-r$R))<1e-14,'raw task path uses a square factor without an eager global Cholesky')
ok(identical(r$square_factor_column_ids,s$tables$coordinates$id[c(3,1,4,2)]),'noncontiguous whitening column order is explicit')
old_route<-function(v){state<-.bh_covariance_state(s,v,covariance=FALSE);C<-state$effective_loadings%*%t(chol(state$R_component));U<-state$D_parameter%*%t(chol(state$R_parameter));S<-tcrossprod(C)+tcrossprod(U);at<-s$tables$augmentation$target_index;delta<-matrix(0,4,4);delta[,at]<-t(t(chol(S[at,at]))%*%(white%*%t(.bh_centering_basis(4))));delta}
ok(max(abs(a$delta-old_route(v)))<1e-13,'moderate task values agree with the previous global-Cholesky route')
for(j in ix$component_cor){plus<-minus<-v;plus[j]<-plus[j]+1e-5;minus[j]<-minus[j]-1e-5;new<-(.bh_task_state(s,plus,rep(0,4),white)$delta-.bh_task_state(s,minus,rep(0,4),white)$delta)/2e-5;old<-(old_route(plus)-old_route(minus))/2e-5;ok(max(abs(new-old))<1e-9,'moderate keyed task derivatives agree across factor conventions')}
# Rounded R loses numerical PD, while its square factor and residual-supported task path remain usable.
large<-bhmf(A=~x1,B=~x2,loadings=list(fixed=list(list(value=1,cells=list(A=~x1,B=~x2)))),residual=list(targets=~x1+x2),
 cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(a=~A,b=~B))),person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky'))
ls<-bhmm(large,d[,1:3],subject='person',prior=set_prior('normal(0,.2)','cor_raw'),fit=FALSE);lv<-.bh_covariance_probe(ls,1);li<-.bh_covariance_layout(ls)$indices;lv[li$component_cor]<-1e10;lv[li$parameter_sd]<-.5
lr<-.bh_raw_correlation_state(ls,1e10,global_cholesky=FALSE)
ok(inherits(try(chol(lr$R),silent=TRUE),'try-error')&&all(is.finite(lr$F_global)),'rounded common R is not mislabeled as numerically PD')
lt<-.bh_task_state(ls,lv,c(0,0),matrix(.2*cos(1:6),2,3))
ok(all(is.finite(lt$delta))&&all(diag(lt$working_cholesky)>0)&&grepl('unqualified',lt$stored_common_R_scope,fixed=TRUE),'task target remains representable with explicit reporting limits')
old<-tryCatch(.bh_raw_correlation_state(ls,1e10),error=identity);ok(old$code=='E_STRUCTURE_STATE','explicit global-Cholesky request retains its numerical boundary')
# K0 remains a true empty square factor.
km<-bhmf(residual=list(targets=~x1),cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list())),person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky'))
ks<-bhmm(km,d[,1:2],subject='person',fit=FALSE);kv<-.bh_covariance_probe(ks,1);kt<-.bh_task_state(ks,kv,0,matrix(.2,1,3))
ok(identical(dim(kt$working_common_factor),c(1L,0L))&&identical(dim(.bh_raw_correlation_state(ks,numeric(),FALSE)$F_global),c(0L,0L)),'K0 factor and task dimensions remain exact')
cat('RAW TASK FACTOR',count,'checks passed\n')
