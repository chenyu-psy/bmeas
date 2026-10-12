source(file.path("helpers", "native-guard.R"))
# R-only centered/task/final-marginal composition and detached lifecycle controls.
# Direct cache and public fitting lifecycle use terminal and provider-metadata doubles.
# No stanc, compilation, native model, installation or sampling is used.
source_root<-Sys.getenv('MEASUREMENTPKG_SOURCE');source_mode<-nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
replace<-function(n,f)if(source_mode)assign(n,f,.GlobalEnv)else assignInNamespace(n,f,'bmeas')
local({
# Fail closed even if a regression accidentally admits an early public call.
original_toolchain<-private('.mp_toolchain_fingerprint');native_calls<-0L
replace('.mp_toolchain_fingerprint',function(...) {native_calls<<-native_calls+1L;stop('unexpected native toolchain boundary')})
on.exit(replace('.mp_toolchain_fingerprint',original_toolchain),add=TRUE)
if(requireNamespace('cmdstanr',quietly=TRUE)) {
 backend_ns<-asNamespace('cmdstanr');original_model<-get('cmdstan_model',backend_ns)
 unlockBinding('cmdstan_model',backend_ns)
 assign('cmdstan_model',function(...) {native_calls<<-native_calls+1L;stop('unexpected native model boundary')},backend_ns)
 lockBinding('cmdstan_model',backend_ns)
 on.exit({unlockBinding('cmdstan_model',backend_ns);assign('cmdstan_model',original_model,backend_ns);lockBinding('cmdstan_model',backend_ns)},add=TRUE)
}
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code,label=code){e<-tryCatch(suppressWarnings(force(expr)),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),label);invisible(e)}
people<-as.character(1:4);person<-list(distribution='sample_centered',ids=people)
d<-data.frame(person=people,x1=c(.2,.6,.1,-.4),x2=c(.8,.4,-.5,.1),x3=c(-.3,.2,.7,.5),x4=c(.6,-.1,.3,.9))
all4<-list(~x1:x2,~x1:x3,~x1:x4,~x2:x3,~x2:x4,~x3:x4)
fixed<-function(pair,value)list(pairs=pair,value=value)
# Resolve priors through the public table so each fixture is a complete public context.
prepare<-function(m,data=d,subject='person') {
 first<-bmeas(m,data,subject=subject,fit=FALSE);p<-get_prior(first);rules<-list()
 densities<-c(Intercept='normal(0,1)',loading='normal(0,.8)',component_sd='half_normal(.9)',amplitude='half_normal(.8)',parameter_sd='half_normal(.7)',sigma='half_normal(1)',total_sd='half_student_t(3,0,2.5)',share='normal(.15,.8)',cor_raw='normal(0,.25)')
 for(cl in names(densities))if(any(p$class==cl))rules[[length(rules)+1L]]<-set_prior(densities[[cl]],cl)
 for(layer in c('component','parameter'))if(any(p$class=='cor'&p$layer==layer))rules[[length(rules)+1L]]<-set_prior(if(layer=='component')'lkj(2.5)'else'lkj(3.5)','cor',layer=layer)
 bmeas(m,data,subject=subject,prior=if(length(rules))do.call(c,rules)else NULL,fit=FALSE)
}
common<-list(default='free',zero=list(~A1+A2,~B1+B2,~A1:B2))
compact<-function(residual='independent',correlation=common,person_law=person,aug=list(type='task_cholesky',targets=~x1+x2+x3+x4))prepare(bmf(
 A1=~x1,A2=~x2,B1=~x3,B2=~x4,
 loadings=list(fixed=list(list(value=1,cells=list(A1=~x1,A2=~x2,B1=~x3,B2=~x4)))),
 cor=correlation,residual=list(targets=~x1+x2+x3+x4,cor=residual),person=person_law,augmentation=aug))
fixtures<-list(independent=compact(),ordinary_residual=compact(list(default='free',zero=all4[-1L])),shared_residual=compact(list(default='zero',equal=list(shared=all4))),
 fixed_positive=compact(list(default='zero',fixed=lapply(all4,fixed,value=.2))),
 fixed_negative=compact(list(default='zero',fixed=lapply(all4,fixed,value=-.2))))
# S17: two separate owners, three signed scalar parts each, six target SDs.
Q<-cbind(G=1,P=c(1,-1,1),C=c(1,1,-1));args<-owners<-list()
for(owner in c('a','c')) {
 keys<-paste0(owner,1:3);formula<-as.formula(paste('~',paste(keys,collapse='+')))
 args[[paste0(owner,'_G')]]<-formula
 for(part in c('P','C'))args[[paste0(owner,'_',part)]]<-component(formula,matrix(Q[,part],3,1,dimnames=list(keys,'pattern')),'decomposition')
 owners[[owner]]<-list(type='variance_share',targets=formula,components=as.formula(paste('~',paste(paste0(owner,c('_G','_P','_C')),collapse='+'))),residual=TRUE,shares=~1)
}
args$cor<-list(default='free',zero=list(~a_G+a_P+a_C,~c_G+c_P+c_C,~a_P:c_C,~a_C:c_P));args$decomposition<-owners
args$person<-person;args$augmentation<-list(type='task_cholesky',targets=~a1+a2+a3+c1+c2+c3)
s17_data<-data.frame(person=people)
for(j in 1:6)s17_data[[c(paste0('a',1:3),paste0('c',1:3))[j]]]<-sin((1:4)*(.4+.11*j))+.1*j*cos((1:4)*.2)
fixtures$s17<-prepare(do.call(bmf,args),s17_data)
fixtures$no_common<-prepare(bmf(residual=list(targets=~x1+x2),person=person,augmentation=list(type='task_cholesky',targets=~x1+x2)),d[c('person','x1','x2')])
fixtures$no_free_edge<-compact(correlation='independent')
fixtures$one_common_edge<-prepare(bmf(A=~x1,B=~x2,loadings=list(fixed=list(list(value=1,cells=list(A=~x1,B=~x2)))),cor='free',residual=list(targets=~x1+x2),person=person,augmentation=list(type='task_cholesky',targets=~x1+x2)),d[c('person','x1','x2')])
fixtures$singleton<-prepare(bmf(G=~x1,loadings=list(fixed=list(list(value=1,cells=list(G=~x1)))),residual=list(targets=~x1),person=list(distribution='sample_centered',ids=people[1:2]),augmentation=list(type='task_cholesky',targets=~x1)),d[1:2,c('person','x1')])
long<-data.frame(person=people[1:2],task='only',y=c(.1,.4),observation=c('o1','o2'))
empty_model<-bmf(person=list(distribution='sample_centered',ids=people[1:2]),augmentation=list(type='task_cholesky',targets=~0))
fixtures$no_task_white<-prepare(empty_model,list(score=observe(gaussian_model(),long,'person','task','y',units='points',observation_id='observation',parameters=list(mu='task',sigma=list(fixed=1)))),NULL)
fixtures$fully_fixed<-prepare(empty_model,list(score=observe(gaussian_model(),long,'person','task','y',units='points',observation_id='observation',parameters=list(mu=list(fixed=.2),sigma=list(fixed=1)))),NULL)

# Scan actual emitted declarations, independent of the output-estimation formulas.
output_declarations<-function(code,data) {
 lines<-strsplit(as.character(code),'\n',fixed=TRUE)[[1L]];records<-list()
 for(block in c('parameters','transformed parameters','generated quantities')) {
  start<-which(lines==paste0(block,' {'))+1L;stopifnot(length(start)==1L)
  for(i in seq.int(start,length(lines))) {
   line<-trimws(lines[i]);if(!grepl('^(array\\[|vector[<\\[]|matrix[<\\[]|real\\b)',line))break
   lhs<-trimws(strsplit(gsub('<[^>]+>','',line),'=',fixed=TRUE)[[1L]][1L]);lhs<-sub(';.*$','',lhs)
   variable<-sub('.*[[:space:]]([[:alnum:]_]+)$','\\1',lhs)
   dims<-regmatches(lhs,gregexpr('\\[[^]]+\\]',lhs))[[1L]]
   expressions<-unlist(strsplit(gsub('\\[|\\]','',dims),',',fixed=TRUE))
   count<-if(length(expressions))prod(vapply(expressions,function(x)as.numeric(eval(parse(text=x),envir=list2env(data,parent=baseenv()))),numeric(1)))else 1
   records[[length(records)+1L]]<-data.frame(block=switch(block,parameters='parameters','transformed parameters'='transformed','generated quantities'='generated'),variable=variable,elements=count)
  }
 }
 do.call(rbind,records)
}
# Default CmdStan sampler columns are separate from model declarations.
sampler_columns<-c('lp__','accept_stat__','stepsize__','treedepth__','n_leapfrog__','divergent__','energy__')
emitted_records<-list();emitted_totals<-list()
for(name in names(fixtures)) {
 s<-fixtures[[name]];before<-serialize(s,NULL,version=2);x<-standata(s);checks<-check_model(s);p<-get_prior(s)
 ok(private('.bh_centered_final_marginal_mode')(s)&&private('.bh_reference_reporting_supported')(s)&&private('.bh_reference_runtime_supported')(s),paste(name,'has bounded reporting and runtime capability'))
 ok(checks$backend$status=='experimental_centered_task_final_marginal',paste(name,'public capability requires explicit experimental execution'))
 ok(!any(p$class=='cor_raw')&&nrow(s$tables$raw_correlation_coordinates)==0L,paste(name,'has no raw-B primitive or prior'))
 map<-private('.bh_affine_summary_map')(s);primitives<-s$tables$correlation_primitives
 ok(nrow(map)==nrow(primitives)&&!anyDuplicated(map$parameter_key)&&setequal(map$parameter_key,primitives$parameter),paste(name,'one marginal summary per authoritative primitive'))
 for(layer in c('component','parameter')) {
  plan<-private('.bh_affine_layer_plan')(s,layer);m<-map[map$layer==layer,,drop=FALSE]
  ok(all(m$parameter_role=='unique_marginal_correlation')&&all(m$computational_mapping==if(plan$mode==2L)'one_label_interval_coordinate'else'MGS_vector_map_no_scalar_association'),paste(name,layer,'scientific rhos retain their chart-map role'))
  if(plan$mode==0L)ok(all(is.na(m$computational_variable)),paste(name,layer,'MGS vector has no scalar rho association'))
  if(plan$mode==2L)ok(nrow(m)==1L&&m$computational_variable=='parameter_correlation_raw[1]'&&nrow(m$alias_cells[[1L]])==6L,paste(name,'shared residual has one interval coordinate and six aliases'))
  if(plan$mode==1L)ok(nrow(m)==0L&&!any(p$class=='cor'&p$layer==layer),paste(name,'fixed layer has no correlation unknown or prior'))
 }
 ini<-private('.bh_materialize_init')(s,checks$initialization)
 ci<-checks$initialization$correlation_initialization
 ok(identical(checks$initialization$recipe_version,'centered_final_marginal_role_scale_v1')&&identical(ci$layer,c('component','parameter'))&&identical(as.integer(ci$unique_parameters),as.integer(c(x$E,x$EU))),paste(name,'initializer records each final correlation layer once'))
 ok(all(is.finite(ci$joint_prior_kernel))&&all(is.finite(ci$chart_log_jacobian))&&sum(ci$joint_prior_kernel)==checks$initialization$joint_correlation_prior_kernel&&sum(ci$chart_log_jacobian)==checks$initialization$correlation_chart_log_jacobian,paste(name,'one layer kernel and chart Jacobian contribute once'))
 empty<-ci$unique_parameters==0L
 ok(all(ci$joint_prior_kernel[empty]==0)&&all(ci$chart_log_jacobian[empty]==0)&&all(lengths(ci$computational_values[empty])==0L),paste(name,'fixed or identity layers add no prior coordinate or Jacobian'))
 ok(checks$initialization$status=='screened'&&!any(c('correlation_B','z_raw','e_raw')%in%names(ini)),paste(name,'initialization has no raw/common-person primitives'))
 for(variable in c('correlation_raw','parameter_correlation_raw','alpha','obs_sd','parameter_sd'))if(variable%in%names(ini)&&length(ini[[variable]])==1L)ok(identical(dim(ini[[variable]]),1L),paste(name,variable,'singleton initial vector retains rank'))
 ok(('correlation_raw'%in%names(ini))==(x$E>0L)&&('parameter_correlation_raw'%in%names(ini))==(x$EU>0L)&&('z_task'%in%names(ini))==(x$JT>0L),paste(name,'zero dimensions allocate no dummy initializer'))
 if(x$JT)ok(identical(dim(ini$z_task),c(x$JT,x$N)),paste(name,'task initializer follows target then roster dimensions'))
 for(log_lik in c(FALSE,TRUE)) {
  code<-stancode(s,log_lik=log_lik);declared<-output_declarations(code,x);inventory<-private('.bh_reference_output_inventory')(s,log_lik)
  key<-function(t)paste(t$block,t$variable,sep=':');at<-match(key(inventory),key(declared))
  ok(!anyNA(at)&&setequal(key(inventory),key(declared))&&all(inventory$elements==declared$elements[at]),paste(name,log_lik,'actual declarations match every inventory field'))
  size<-private('.bh_reference_output_reals')(s,log_lik)
  record_key<-paste(name,log_lik,sep=':');emitted_records[[record_key]]<-list(declared=declared,inventory=inventory)
  emitted_totals[[record_key]]<-data.frame(fixture=name,log_lik=log_lik,K=x$K,E=x$E,U=x$U,EU=x$EU,JT=x$JT,N=x$N,O=x$O,parameters=sum(declared$elements[declared$block=='parameters']),transformed=sum(declared$elements[declared$block=='transformed']),generated=sum(declared$elements[declared$block=='generated']),sampler_allowance=66L,total=sum(declared$elements)+66L)
  ok(size$total==sum(declared$elements)+66&&size$sampled_white_coordinates==x$JT*(x$N-1)&&size$constrained_white_values==x$JT*x$N,paste(name,log_lik,'actual output count and distinct white dimensions agree'))
  ok(sum(declared$elements[declared$variable=='correlation_raw'])==x$E&&sum(declared$elements[declared$variable=='correlation_map'])==(x$K+1L)*x$K,paste(name,log_lik,'common MGS parameter and chart output are counted'))
  ok(!grepl('correlation_B|z_raw|e_raw',as.character(code))&&grepl('common_task_factor=effective_loadings_work*L_component;',as.character(code),fixed=TRUE)&&!grepl('cholesky_decompose(R_component)',as.character(code),fixed=TRUE),paste(name,log_lik,'source uses direct common factor without raw scores or rounded refactor'))
 }
 ok(private('.bh_reference_output_reals')(s,TRUE)$total-private('.bh_reference_output_reals')(s,FALSE)$total==x$O,paste(name,'optional log likelihood adds exactly O outputs'))
 ok(isTRUE(private('.bh_affine_runtime_gate')(s)),paste(name,'private runtime gate admits the bounded final-marginal mode'))
 if(sum(p$parameter_count)) {
  for(exploratory in c(FALSE,TRUE))err(bmeas(s,chains=1,iter=20,warmup=10,exploratory=exploratory,resources='development'),'E_EXECUTION_PROFILE',paste(name,exploratory,'qualified execution cannot bypass the experimental requirement'))
 }else {
  for(execution in c('qualified','experimental'))err(bmeas(s,chains=1,iter=20,warmup=10,execution=execution,exploratory=TRUE,resources='development'),'E_NO_FREE_PARAMETERS',paste(name,execution,'P0 public sampling stays refused'))
  err(private('.bh_fit_scalar')(s,list(),tempfile(),tempfile()),'E_NO_FREE_PARAMETERS',paste(name,'P0 private sampling stays refused'))
 }
 ok(identical(before,serialize(s,NULL,version=2)),paste(name,'all inspection and refusals preserve sealed bytes'))
}

# Public S17 structure and a scoped exact population inverse, not a generic ID claim.
s<-fixtures$s17;t<-model_table(s);l<-private('.bh_covariance_layout')(s);ix<-l$indices;free<-t$correlations[t$correlations$state=='free',,drop=FALSE];p<-get_prior(s)
ok(nrow(t$targets)==6L&&nrow(t$components)==6L&&nrow(t$parameter_residuals)==6L&&nrow(free)==7L,'S17 has six coordinates and seven common free entries')
ok(all(t$correlations$state[t$correlations$row==2&t$correlations$col==6]=='zero')&&all(t$correlations$state[t$correlations$row==3&t$correlations$col==5]=='zero'),'S17 retains the two selected cross zeros')
ok(sum(p$class=='share')==6L&&sum(p$class=='total_sd')==6L&&!any(p$class%in%c('loading','amplitude','parameter_sd','component_sd','cor_raw')),'S17 ownership removes redundant primitive scales and loadings')
ok(sum(p$class=='cor')==1L&&p$parameter_count[p$class=='cor']==7L,'S17 has one joint common prior on seven final rhos')
v<-private('.bh_covariance_probe')(s,1);v[ix$total_sd]<-c(.7,.9,1.1,.8,1,1.2);v[ix$share]<-c(.23,-.35,.17,-.27,.31,-.11);v[ix$observation_variance]<-c(.2,.3,.4,.25,.35,.45)
C<-rbind(c(.1,.05,-.04),c(.08,.12,0),c(.03,0,-.07));R<-rbind(cbind(diag(3),C),cbind(t(C),diag(3)))
v[ix$component_cor]<-R[cbind(free$row,free$col)];state<-private('.bh_covariance_state')(s,v);S<-state$Sigma_predictor
V<-kronecker(private('.bh_centering_covariance')(s),S)+kronecker(diag(4),diag(v[ix$observation_variance]));rec<--3*V[1:6,7:12]
tau<-sqrt(diag(rec));Sn<-rec/outer(tau,tau);M<-rbind(Q[1,]*Q[2,],Q[1,]*Q[3,],Q[2,]*Q[3,]);coefs<-numeric();loading_blocks<-weights<-list()
for(k in 1:2) {
 idx<-(k-1)*3+1:3;wc<-solve(M,c(Sn[idx[1],idx[2]],Sn[idx[1],idx[3]],Sn[idx[2],idx[3]]));w<-c(wc,1-sum(wc));stratum<-t$share_strata[[k]]
 coefs<-c(coefs,as.numeric(solve(crossprod(stratum$C),crossprod(stratum$C,log(w)-mean(log(w))))));loading_blocks[[k]]<-Q%*%diag(sqrt(wc));weights[[k]]<-w
}
cross<-solve(loading_blocks[[1]],Sn[1:3,4:6])%*%solve(t(loading_blocks[[2]]));noise<-diag(V[1:6,1:6])-diag(rec)
ok(max(abs(c(tau-v[ix$total_sd],coefs-v[ix$share],as.numeric(cross-C),noise-v[ix$observation_variance])))<1e-12&&all(unlist(weights)>0),'S17 population inverse recovers six SDs six share coefficients seven rhos and six noises')
ok(check_model(s)$identification$status=='not_assessed','S17 fixture inverse is not a name-based generic identification certificate')
mask<-private('.bh_mask')(s$tables$correlations,seq_len(6))=='free';raw<-private('.bh_correlation_inverse')(R,mask);chart<-private('.bh_correlation_chart')(raw,mask)
ok(max(abs(chart$R-R))<1e-12&&max(abs(raw-v[ix$component_cor]))>1e-4,'S17 MGS computational coordinates differ from the scientific marginal vector')

for(name in c('independent','shared_residual','fixed_positive','fixed_negative')) {
 s<-fixtures[[name]];ix<-private('.bh_covariance_layout')(s)$indices;v<-private('.bh_covariance_probe')(s,1)
 v[ix$component_cor]<-c(.18,-.11,.23);v[ix$parameter_sd]<-c(.6,.8,1,1.2);v[ix$observation_variance]<-c(.25,.36,.49,.64)
 if(length(ix$parameter_cor))v[ix$parameter_cor]<-.15
 state<-private('.bh_task_state')(s,v,rep(0,4),matrix(.1,4,3));S<-state$Sigma_predictor
 V<-kronecker(private('.bh_centering_covariance')(s),S)+kronecker(diag(4),diag(v[ix$observation_variance]));rec<--3*V[1:4,5:8];u<-sqrt(diag(rec)-1)
 Ru<-state$R_parameter;if(name=='shared_residual'){rho<-rec[1,2]/(u[1]*u[2]);Ru<-matrix(rho,4,4);diag(Ru)<-1;ok(abs(rho-.15)<1e-12,'known common zero identifies the shared residual rho')}
 Rc<-rec-Ru*outer(u,u);noise<-diag(V[1:4,1:4])-diag(rec)
 ok(max(abs(c(as.numeric(Rc-state$R_component),u-v[ix$parameter_sd],noise-v[ix$observation_variance])))<1e-12,paste(name,'compact population inverse separates common residual and noise'))
}

# Retain the scoped free-G plus positive-amplitude basis nonidentification certificate.
Qnegative<-matrix(c(-1,0,1),3,1,dimnames=list(c('x1','x2','x3'),'linear'))
negative<-prepare(bmf(G=~x1+x2+x3,P=component(~x1+x2+x3,Qnegative,'shared'),cor='free',residual=list(targets=~x1+x2+x3),person=person,augmentation=list(type='task_cholesky',targets=~x1+x2+x3)),d[c('person','x1','x2','x3')])
ix<-private('.bh_covariance_layout')(negative)$indices;v<-private('.bh_covariance_probe')(negative,1);v[ix$loading]<-c(.8,.7,.9);v[ix$amplitude]<-.6;v[ix$component_cor]<-.2
gnew<-.35;hnew<-.6*sqrt(1-.2^2)/sqrt(1-gnew^2);other<-v;other[ix$loading]<-v[ix$loading]+(.2*.6-gnew*hnew)*Qnegative[,1];other[ix$amplitude]<-hnew;other[ix$component_cor]<-gnew
a<-private('.bh_task_state')(negative,v,rep(0,3),matrix(.1,3,3));b<-private('.bh_task_state')(negative,other,rep(0,3),matrix(.1,3,3))
ok(max(abs(a$Sigma_predictor-b$Sigma_predictor))<1e-12&&max(abs(a$delta-b$delta))<1e-12&&max(abs(v-other))>.01&&all(other[ix$loading]>0),'G/P shear preserves centered covariance and same task-white predictors with positive anchors')
ok(check_model(negative)$identification$status=='proven_unidentified','in-profile G/P structural negative remains conclusive');err(stancode(negative),'E_IDENTIFICATION')

# These public declarations must not inherit this bounded execution mode.
common_affine<-compact(correlation=list(default='zero',fixed=list(fixed(~A1:A2,.2))))
common_tied<-compact(correlation=list(default='zero',equal=list(tied=list(~A1:A2,~B1:B2))))
iid<-compact(person_law='iid',aug=NULL)
raw_control<-compact(correlation=list(default='free',zero=list(~A1+A2,~B1+B2),coordinates=list(type='ordered_blocks',blocks=list(first=~A1+A2,second=~B1+B2))))
raw_iid<-prepare(bmf(A=~x1+x2,B=~x3+x4,loadings=list(fixed=list(list(value=1,cells=list(A=~x1+x2,B=~x3+x4)))),cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(first=~A,second=~B)))))
err(compact(aug=NULL),'E_REFERENCE_COMBINATION','sample-centered common-plus-residual augmentation remains refused')
err(compact(person_law='iid'),'E_REFERENCE_COMBINATION','iid task augmentation remains refused')
for(s in list(common_affine,common_tied,iid,raw_control))ok(!private('.bh_centered_final_marginal_mode')(s),'fixed/tied common, iid and raw controls are outside the bounded final-marginal mode')
ok(private('.bh_reference_runtime_supported')(raw_control),'qualified raw/common runtime predicate stays separate')
for(s in list(common_affine,common_tied)) {
 err(private('.bh_affine_runtime_gate')(s),'E_FRONTEND_BACKEND_UNSUPPORTED','valid fixed/tied common combination has explicit unsupported runtime error')
 err(private('.bh_reference_output_inventory')(s),'E_REFERENCE_OUTPUT_SCOPE','fixed/tied common cannot claim this final-R output inventory')
}
multilabel<-compact(list(default='zero',equal=list(first=all4[1:2],second=all4[3:4])))
ok(!private('.bh_centered_final_marginal_mode')(multilabel),'multilabel affine residual stays outside the bounded final-marginal mode')
err(private('.bh_affine_lowering_plan')(multilabel),'E_CORRELATION_DIMENSION_UNSUPPORTED','multilabel residual has explicit unsupported-chart error')
noop<-compact(correlation=list(default='free',zero=list(~A1+A2,~B1+B2,~A1:B2),fixed=list(fixed(~A1:A2,0))))
ok(private('.bh_centered_final_marginal_mode')(noop)&&standata(noop)$E==3L,'explicit redundant fixed zero preserves effective ordinary graph route')
old_shape<-fixtures$independent;old_shape$tables$correlation_primitives<-NULL;old_bytes<-serialize(old_shape,NULL,version=2)
ok(identical(private('.bh_reference_output_reals')(fixtures$independent,TRUE),private('.bh_reference_output_reals')(old_shape,TRUE))&&identical(old_bytes,serialize(old_shape,NULL,version=2)),'older ordinary table shape keeps exact output counts without resealing')
# Private shape controls only: this copy is not resealed or presented as a public historical context.
ok(!private('.bh_final_correlation_metadata_available')(old_shape)&&!private('.bh_reference_runtime_supported')(old_shape),'missing historical primitive maps have an explicit reporting and runtime boundary')
for(fn in c('.bh_affine_summary_map','.bh_affine_matrix_cache','.bh_affine_cache_metadata','.bh_reference_cache_metadata','.bh_cache_scalar_fit','.bh_affine_runtime_gate','.bh_compile_scalar','.bh_fit_scalar'))
 err(do.call(private(fn),list(spec=old_shape)),'E_CORRELATION_REPORT_REBUILD',paste(fn,'requires reporting rebuild before backend access'))
ok(identical(old_bytes,serialize(old_shape,NULL,version=2)),'reporting refusal does not reseal or mutate old-shaped input')

overlap<-compact('free');ok(check_model(overlap)$identification$status=='proven_unidentified','overlapping full residual and common graph is not promoted to identified');err(stancode(overlap),'E_IDENTIFICATION')

name_values<-function(field,x) {
 if(!length(x))return(numeric())
 labels<-if(is.matrix(x))paste0(field,'[',rep(seq_len(nrow(x)),ncol(x)),',',rep(seq_len(ncol(x)),each=nrow(x)),']')else paste0(field,'[',seq_along(x),']')
 stats::setNames(as.numeric(x),labels)
}
make_backend<-function(s) {
 J<-nrow(s$tables$targets);K<-nrow(s$tables$components);U<-nrow(s$tables$parameter_residuals);N<-length(s$data_map$subject_keys);JT<-length(s$tables$augmentation$target)
 ix<-private('.bh_covariance_layout')(s)$indices
 draws<-do.call(rbind,lapply(1:3,function(i) {
  v<-private('.bh_covariance_probe')(s,i);st<-private('.bh_covariance_state')(s,v,covariance=FALSE);white<-matrix(.1*sin(seq_len(JT*(N-1))+i),JT,N-1)
  state<-private('.bh_task_state')(s,v,rep(.1*i,J),white)
  common_raw<-if(private('.bh_raw_cor')(s))v[ix$component_cor]else if(K)private('.bh_correlation_inverse')(st$R_component,private('.bh_mask')(s$tables$correlations,seq_len(K))=='free')else numeric()
  pp<-private('.bh_affine_layer_plan')(s,'parameter')
  parameter_raw<-if(pp$mode==2L)private('.bh_cor_interval_inverse')(v[ix$parameter_cor],pp$domain)else if(pp$mode==0L&&U)private('.bh_correlation_inverse')(st$R_parameter,private('.bh_mask')(s$tables$parameter_correlations,seq_len(U))=='free')else numeric()
  shares<-if(private('.bh_has_shares')(s))vapply(seq_len(nrow(s$tables$share_parts)),function(k){p<-s$tables$share_parts[k,];st$shares[[p$stratum_index]][p$target,p$part]},numeric(1))else numeric()
  fields<-list(alpha=rep(.1*i,J),loading=v[ix$loading],component_sd=v[ix$component_sd],amplitude=v[ix$amplitude],total_sd=v[ix$total_sd],share_beta=v[ix$share],share_weight=shares,
   obs_sd=sqrt(v[ix$observation_variance]),parameter_sd=v[ix$parameter_sd],parameter_row_sd=st$parameter_sd,
   correlation_raw=common_raw,parameter_correlation_raw=parameter_raw,L_component=if(K)t(chol(st$R_component))else matrix(numeric(),0,0),R_component=st$R_component,
   L_parameter=if(U)t(chol(st$R_parameter))else matrix(numeric(),0,0),R_parameter=st$R_parameter,
   z_task=state$z_task,task_white_unconstrained=white,task_delta=state$delta,effective_loadings=state$effective_loadings,Lambda=state$Lambda,D_parameter=state$D_parameter,
   component_covariance=state$component_covariance,component_covariance_available=state$component_covariance_available,component_covariance_log_abs=state$component_covariance_log_abs,
   log_component_row_sd=log(state$component_sd),Sigma_predictor_log_abs=state$Sigma_predictor_log_abs,
   Lambda_available=state$Lambda_available,effective_loadings_available=state$effective_loadings_available,D_parameter_available=state$D_parameter_available,
   Sigma_common_available=matrix(1,J,J),Sigma_parameter_available=matrix(1,J,J),Sigma_predictor_available=state$Sigma_predictor_available)
  if(private('.bh_raw_cor')(s)){fields$correlation_raw<-NULL;fields$correlation_B<-common_raw;fields$L_component<-NULL;fields$F_component<-st$F_component}
  unlist(lapply(names(fields),function(n)name_values(n,fields[[n]])),use.names=TRUE)
 }))
 if(is.null(dim(draws)))draws<-matrix(numeric(),3,0)
 calls<-new.env(parent=emptyenv());calls$summary<-calls$draws<-0L;calls$monitored<-NULL
 select<-function(variables)unique(unlist(lapply(variables,function(v)if(v%in%colnames(draws))v else colnames(draws)[startsWith(colnames(draws),paste0(v,'['))]),use.names=FALSE))
 backend<-list(return_codes=function()0L,output_files=function()mock_output,draws=function(variables,format){calls$draws<-calls$draws+1L;draws[,select(variables),drop=FALSE]},diagnostic_summary=function(quiet)list(num_divergent=0,num_max_treedepth=0,ebfmi=.4),output=function()character())
 backend$summary<-function(variables,...) {
  calls$summary<-calls$summary+1L;if(is.null(calls$monitored))calls$monitored<-variables
  columns<-select(variables)
  if(!length(columns))return(data.frame(variable=character(),mean=numeric(),median=numeric(),sd=numeric(),q5=numeric(),q95=numeric(),rhat=numeric(),ess_bulk=numeric(),ess_tail=numeric(),mcse_mean=numeric()))
  do.call(rbind,lapply(columns,function(v){x<-draws[,v];data.frame(variable=v,mean=mean(x),median=median(x),sd=sd(x),q5=unname(quantile(x,.05)),q95=unname(quantile(x,.95)),rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.5)}))
 }
 list(backend=backend,calls=calls,draws=draws)
}
artifacts<-Sys.getenv('MEASUREMENTPKG_TEST_ARTIFACTS')
if(!nzchar(artifacts))artifacts<-tempfile('centered-final-cache-')
dir.create(artifacts,recursive=TRUE,showWarnings=FALSE);artifacts<-normalizePath(artifacts)
# Only file metadata is used for this explicit backend double's size receipt.
mock_output<-file.path(artifacts,'backend-metadata-only.txt')
writeLines('metadata-only backend double; not posterior data',mock_output)
saveRDS(emitted_records,file.path(artifacts,'emitted-declaration-inventories.rds'),version=2)
utils::write.table(do.call(rbind,emitted_totals),file.path(artifacts,'emitted-output-counts.tsv'),sep='\t',row.names=FALSE,quote=FALSE)
cold_fits<-list()
original_provider_runtime<-private('.bh_provider_runtime_info')
provider_fixture<-list(available=TRUE,cmdstanr_version='0.9.0',cmdstan_version='2.40.0',os_type='unix',wsl=FALSE)
local({
 # The external compiler and provider metadata discovery use explicit doubles.
 # Production provider preflight, model admission, output budgets, initialization,
 # fitting dispatch and cache construction stay real. This fixture does not
 # qualify the host provider or execute any native compiler or sampler.
 original_compile<-private('.bh_compile_scalar');calls<-new.env(parent=emptyenv())
 calls$compile<-calls$sample<-calls$runtime<-0L;calls$args<-calls$compile_args<-NULL
 on.exit(replace('.bh_provider_runtime_info',original_provider_runtime),add=TRUE)
 replace('.bh_provider_runtime_info',function() {calls$runtime<-calls$runtime+1L;provider_fixture})
 on.exit(replace('.bh_compile_scalar',original_compile),add=TRUE)
 replace('.bh_compile_scalar',function(spec,cache_dir,save_log_lik=FALSE) {
  calls$compile<-calls$compile+1L;calls$compile_args<-list(spec=spec,save_log_lik=save_log_lik)
  list(model=list(sample=function(...) {calls$sample<-calls$sample+1L;calls$args<-list(...);b$backend}),
    identity=list(code=as.character(stancode(spec,log_lik=save_log_lik)),toolchain=list(test='terminal compiler double')),
    cache_key='mock-final-marginal',cache_hit=FALSE,executable_sha256='no-native-executable')
 })
 before_calls<-c(calls$compile,calls$sample)
 for(name in c('common_affine','common_tied','raw_iid','multilabel','negative','overlap')) {
  s<-get(name);before_spec<-serialize(s,NULL,version=2)
  code<-if(name%in%c('negative','overlap'))'E_IDENTIFICATION'else if(name=='multilabel')'E_CORRELATION_DIMENSION_UNSUPPORTED'else'E_FRONTEND_BACKEND_UNSUPPORTED'
  for(execution in c('qualified','experimental'))for(exploratory in c(FALSE,TRUE))
   err(bmeas(s,chains=1,iter=20,warmup=10,execution=execution,exploratory=exploratory,resources='development'),code,paste(name,execution,exploratory,'public flags preserve the refusal boundary'))
  if(name%in%c('common_affine','common_tied','raw_iid','multilabel'))err(private('.bh_cache_scalar_fit')(NULL,s,list()),'E_REFERENCE_CACHE_UNAVAILABLE',paste(name,'actual cache refuses unsupported reference combinations before backend access'))
  ok(identical(before_spec,serialize(s,NULL,version=2)),paste(name,'public refusals preserve sealed bytes'))
 }
 ok(identical(before_calls,c(calls$compile,calls$sample)),'unsupported modes and structural negatives never contact the compiler or sample double')
 for(name in names(fixtures)) {
  s<-fixtures[[name]];before_spec<-serialize(s,NULL,version=2);x<-standata(s);checks<-check_model(s);b<-make_backend(s)
  plan<-list(posterior_propriety=checks$posterior_propriety,posterior_normalizability=checks$posterior_normalizability,gaussian_diagnostics=checks$gaussian_diagnostics,identification=checks$identification,
   execution_profile=list(requested='experimental',used=checks$backend$status),backend_qualification=checks$backend)
  ok(isTRUE(private('.bh_affine_runtime_gate')(s)),paste(name,'unmodified runtime gate admits direct cache and fit lifecycle'))
  cache<-private('.bh_cache_scalar_fit')(b$backend,s,plan);tab<-cache$parameter_summary;expanded<-cache$expanded_correlation_matrices$entries;primitives<-s$tables$correlation_primitives
  ok(!is.null(tab)&&cache$computation$status=='computed'&&cache$expanded_correlation_matrices$status==if(x$K+x$U)'available'else'not_applicable',paste(name,'actual cache succeeds from deterministic backend double'))
  if(x$K+x$U==0L)ok(b$calls$summary==1L&&nrow(expanded)==0L,paste(name,'empty matrix report never requests an empty variable set from backend'))
  marginal<-tab[tab$parameter_role=='unique_marginal_correlation',,drop=FALSE]
  ok(nrow(marginal)==nrow(primitives)&&!anyDuplicated(marginal$parameter_key)&&setequal(marginal$parameter_key,primitives$parameter),paste(name,'cache monitors each unique final rho once'))
  ok(!any(grepl('^(correlation_raw|parameter_correlation_raw|correlation_B|z_raw|e_raw|z|f)\\[',tab$variable))&&!any(tab$parameter_role%in%c('cor_raw','derived_marginal_component_correlation_entry','latent_person_coordinate')),paste(name,'cache does not mislabel MGS or task whites as scientific/common-person primitives'))
  cells<-s$tables$correlation_affine_cells
  if(nrow(expanded)) {
   free<-expanded$role=='expanded_correlation_alias';at<-match(expanded$parameter_key[free],primitives$parameter)
   ok(!anyNA(at)&&all(expanded$layer[free]==primitives$layer[at]),paste(name,'expanded aliases join authoritative primitive keys within their own layer'))
   constants<-expanded$role=='fixed_correlation_constant'
   ok(all(is.na(expanded$parameter_key[constants]))&&all(is.na(expanded$sd[constants]))&&all(is.na(expanded$rhat[constants]))&&all(expanded$mean[constants]==expanded$fixed_value[constants]),paste(name,'fixed zeros and diagonals retain values without invented uncertainty'))
   ok(sum(free)==2L*sum(cells$state=='free'),paste(name,'full symmetric alias cells do not multiply primitive summaries'))
  }
  ok(sum(tab$parameter_role=='task_white_basis_coordinate')==x$JT*(x$N-1L)&&sum(tab$parameter_role=='derived_centered_task_white_score')==x$JT*x$N&&sum(tab$parameter_role=='derived_task_deviation')==x$JT*x$N,paste(name,'task basis constrained whites and deviations retain distinct roles and counts'))
  reference<-private('.bh_reference_summary_map')(s);sel<-tab$parameter_role=='task_white_basis_coordinate'
  ok(identical(tab$parameter_key[sel],reference$parameter_key[match(tab$variable[sel],reference$variable)])&&all(is.na(tab$reference_person_id[sel])),paste(name,'white basis axes have semantic keys without invented person IDs'))
  ok(identical(cache$reference_law$person_roster,s$tables$person_roster)&&identical(cache$reference_law$augmentation,s$tables$augmentation),paste(name,'cache preserves exact roster and task ordering'))
  ok(grepl('joint determinant density',cache$reference_law$prior_measure,fixed=TRUE)&&grepl('Common R free entries are scientific primitives',cache$reference_law$prior_measure,fixed=TRUE)&&grepl('explicit experimental centered/task ordinary final-marginal common MGS',cache$correlation_constraints$runtime_scope,fixed=TRUE),paste(name,'cache records the final-R measure and bounded experimental runtime'))
  ok(cache$inference$status=='not_assessed'&&all(tab$rhat==2)&&identical(cache$posterior_normalizability,checks$posterior_normalizability)&&(x$JT==0L||cache$posterior_normalizability$status=='not_assessed'),paste(name,'poor diagnostics and scoped normalizability remain distinct from unassessed inference'))
  if(x$K+x$U>0L) {
   reason<-cache$admissibility$reason
   ok(grepl('ordinary MGS layers use the factor product',reason,fixed=TRUE)&&grepl('affine residual layers inspect their stored matrix',reason,fixed=TRUE),paste(name,'actual conditioning cache distinguishes factor-product MGS screening from stored affine residual screening'))
   ok(!grepl('rounded stored component R checked separately',reason,fixed=TRUE),paste(name,'final-marginal conditioning cache makes no raw-only stored-component claim'))
  }
  fit<-structure(list(spec=s,cache=cache,run=list(),backend=NULL,provenance=list(execution_profile=plan$execution_profile)),class=c('bmeas_fit','bhmm_fit'))
  if(sum(get_prior(s)$parameter_count)) {
   for(log_lik in c(FALSE,TRUE)) {
    before_calls<-c(calls$compile,calls$sample);warnings<-character()
    fit<-withCallingHandlers(bmeas(s,chains=1,iter=20,warmup=10,cores=1,threads=1,seed=63,
      execution='experimental',exploratory=TRUE,save_log_lik=log_lik,output_dir=tempfile('mock-final-output-'),resources='development'),
      warning=function(w){warnings<<-c(warnings,w$code);invokeRestart('muffleWarning')})
    ok(identical(c(calls$compile,calls$sample),before_calls+1L)&&inherits(fit,'bmeas_fit'),paste(name,log_lik,'public fitting reaches exactly one compiler and sample double'))
    declared<-emitted_records[[paste(name,log_lik,sep=':')]]$declared
    expected_bytes<-40*(sum(declared$elements)+length(sampler_columns))*fit$run$chains*fit$run$iter_sampling
    ok(fit$provenance$estimated_csv_bytes==expected_bytes&&identical(calls$compile_args$save_log_lik,log_lik)&&identical(fit$code,as.character(stancode(s,log_lik=log_lik))),paste(name,log_lik,'actual fitter estimate counts every emitted field and optional likelihood'))
    init<-calls$args$init();initial<-checks$initialization
    ok(identical(init,private('.bh_materialize_init')(s,initial))&&identical(fit$cache$initialization,fit$provenance$initialization)&&identical(fit$provenance$initialization$constrained_init_hash,private('.mp_hash')(init)),paste(name,log_lik,'actual sample boundary and detached provenance share the exact rank-safe initializer'))
    ok(fit$cache$initialization$recipe_version=='centered_final_marginal_role_scale_v1'&&fit$cache$initialization$materialization_version=='Stan_declared_parameter_ranks_v1'&&identical(fit$cache$initialization$correlation_initialization,initial$correlation_initialization),paste(name,log_lik,'fitter persists the complete once-only final-rho initialization ledger'))
    ok(calls$args$data$JT==x$JT&&calls$args$data$N==x$N&&calls$args$threads_per_chain==1L&&calls$args$iter_warmup==10L&&calls$args$iter_sampling==10L&&calls$args$seed==63L,paste(name,log_lik,'actual sample boundary receives declared dimensions and explicit run controls'))
    ok('W_EXPERIMENTAL_EXECUTION'%in%warnings&&fit$provenance$execution_profile$used=='experimental_centered_task_final_marginal'&&identical(fit$cache$backend_qualification,checks$backend)&&isTRUE(fit$provenance$exploratory_requested)&&identical(fit$provenance$exploratory_rank_override,checks$identification$status=='possible_nonidentification'),paste(name,log_lik,'experimental warning and existing identification policy survive fitting'))
    ok(identical(fit$cache$parameter_summary,tab)&&fit$cache$inference$status=='not_assessed'&&fit$cache$predictive_fit$status=='not_run',paste(name,log_lik,'actual fit uses the direct final-R cache without promoting mock completion to inference'))
    fit$backend<-NULL
   }
   # A partial or edited plan cannot bypass revalidation. Keep the independent
   # emitted-column boundary at ten configured mock states with a caller policy.
   budget_plan<-plan;budget_plan$run<-fit$run;budget_plan$adapt_delta<-.8
   before_calls<-c(calls$compile,calls$sample)
   err(private('.bh_fit_scalar')(s,budget_plan,tempfile(),tempfile(),TRUE),'E_RESOURCE_PLAN_CHANGED',paste(name,'partial legacy-style plan is refused before backend access'))
   ok(identical(before_calls,c(calls$compile,calls$sample)),paste(name,'partial plan never contacts compiler or sample double'))
   per_draw<-40*(sum(emitted_records[[paste(name,TRUE,sep=':')]]$declared$elements)+length(sampler_columns))
   expected_bytes<-per_draw*10L
   make_budget_plan<-function(limit)execution_plan(s,chains=1,iter=20,warmup=10,
     resources=list(max_output_bytes=limit,cache='deferred'),save_log_lik=TRUE,
     execution='experimental',exploratory=TRUE)
   budget_plan<-make_budget_plan(expected_bytes)
   before_runtime<-calls$runtime
   near<-suppressWarnings(private('.bh_fit_scalar')(s,budget_plan,tempfile(),tempfile('mock-budget-'),TRUE))
   ok(calls$runtime==before_runtime+1L&&identical(near$provenance$provider$runtime,provider_fixture)&&near$provenance$provider$status=='reviewed_deferred_scope',paste(name,'real deferred preflight records the explicit provider metadata double once'))
   ok(near$provenance$estimated_csv_bytes==expected_bytes&&near$provenance$estimated_csv_bytes==budget_plan$resource_policy$limits$max_output_bytes,paste(name,'independently counted emitted CSV bytes fit the exact output allowance'))
   budget_plan<-make_budget_plan(expected_bytes-1L);before_calls<-c(calls$compile,calls$sample)
   err(private('.bh_fit_scalar')(s,budget_plan,tempfile(),tempfile(),TRUE),'E_OUTPUT_BUDGET',paste(name,'output allowance one byte below the emitted estimate is refused'))
   ok(identical(before_calls,c(calls$compile,calls$sample)),paste(name,'over-budget fit refuses before compiler or backend access'))
  }
  before_calls<-c(b$calls$summary,b$calls$draws);bytes<-serialize(fit,NULL,version=2)
  invisible(capture.output(print(summary(fit))))
  ok(identical(posterior_summary(fit),tab)&&identical(before_calls,c(b$calls$summary,b$calls$draws))&&identical(bytes,serialize(fit,NULL,version=2))&&identical(before_spec,serialize(s,NULL,version=2)),paste(name,'detached reporting changes neither backend contacts nor fit/spec bytes'))
  cold_fits[[name]]<<-fit
 }
 # Effective ordinary graphs and existing raw profiles share the actual lifecycle
 # without acquiring each other's scientific coordinates or inference claims.
 for(name in c('noop','raw_control')) {
  s<-get(name);before_spec<-serialize(s,NULL,version=2);b<-make_backend(s);before_calls<-c(calls$compile,calls$sample)
  fit<-suppressWarnings(bmeas(s,chains=1,iter=20,warmup=10,execution='experimental',output_dir=tempfile('mock-effective-'),resources='development'))
  expected<-if(name=='noop')'experimental_centered_task_final_marginal'else'experimental_centered_task_raw'
  declared<-output_declarations(stancode(s),standata(s));initial<-private('.bh_initialization')(s)
  ok(identical(c(calls$compile,calls$sample),before_calls+1L)&&fit$provenance$execution_profile$used==expected&&!fit$provenance$exploratory_requested&&!fit$provenance$exploratory_rank_override,paste(name,'public experimental admission preserves existing identification policy without requiring exploration'))
  ok(fit$provenance$estimated_csv_bytes==40*(sum(declared$elements)+length(sampler_columns))*10L&&identical(fit$provenance$initialization$constrained_init_hash,private('.mp_hash')(calls$args$init()))&&identical(fit$provenance$initialization$recipe_version,initial$recipe_version),paste(name,'actual fitter preserves emitted output count and its own initializer provenance'))
  raw<-name=='raw_control'
  ok(('correlation_B'%in%names(calls$args$init()))==raw&&('correlation_raw'%in%names(calls$args$init()))==!raw&&identical(before_spec,serialize(s,NULL,version=2)),paste(name,'actual sample boundary preserves distinct common primitives and sealed context'))
 }
})
ok(identical(private('.bh_provider_runtime_info'),original_provider_runtime),'provider discovery binding is restored after the scoped lifecycle double')
ok(all(vapply(fixtures,private('.bh_reference_runtime_supported'),logical(1))),'runtime support stays genuine after all compiler doubles are restored')
# A genuine qualified raw-common cache still reports derived common R and raw priors.
s<-raw_control;b<-make_backend(s);checks<-check_model(s);plan<-list(posterior_propriety=checks$posterior_propriety,posterior_normalizability=checks$posterior_normalizability,gaussian_diagnostics=checks$gaussian_diagnostics,identification=checks$identification,execution_profile=list(requested='experimental',used=checks$backend$status),backend_qualification=checks$backend)
raw_cache<-private('.bh_cache_scalar_fit')(b$backend,s,plan);tab<-raw_cache$parameter_summary
ok(sum(tab$parameter_role=='cor_raw')==nrow(s$tables$raw_correlation_coordinates)&&all(tab$parameter_role[grepl('^R_component\\[',tab$variable)]=='derived_marginal_component_correlation_entry'),'old raw-common cache retains raw primitives and derived common R roles')
ok(grepl('Raw B cells carry their specified Normal densities once',raw_cache$reference_law$prior_measure,fixed=TRUE),'old raw-common cache retains its distinct Normal prior measure')
ok(grepl('rounded stored component R checked separately',raw_cache$admissibility$reason,fixed=TRUE),'old raw-common cache retains its separate stored-component screening description')
# Persist the detached objects, then read them in a fresh, independent R process.
cache_path<-file.path(artifacts,'detached-final-marginal-fits.rds');saveRDS(cold_fits,cache_path,version=2)
cold_script<-file.path(artifacts,'cold-cache-reader.R')
writeLines(c(
 "source_root<-Sys.getenv('MEASUREMENTPKG_SOURCE');source_mode<-nzchar(source_root)",
 "if(source_mode)for(f in list.files(file.path(source_root,'R'),full.names=TRUE))source(f)else library(bmeas)",
 "private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')",
 "for(n in c('.bh_compile_scalar','.bh_fit_scalar','.bh_cache_scalar_fit')){trap<-function(...)stop('cold summaries must not contact compiler, fitter or cache builder');if(source_mode)assign(n,trap,.GlobalEnv)else assignInNamespace(n,trap,'bmeas')}",
 "fits<-readRDS(commandArgs(TRUE)[1]);bytes<-serialize(fits,NULL,version=2)",
 "for(name in names(fits)){fit<-fits[[name]];stopifnot(is.null(fit$backend),identical(posterior_summary(fit),fit$cache$parameter_summary));invisible(capture.output(print(summary(fit))));private('.bh_validate_fingerprint')(fit$spec);stopifnot(private('.bh_reference_runtime_supported')(fit$spec),fit$cache$execution_profile$used=='experimental_centered_task_final_marginal');if(length(fit$run))stopifnot(identical(fit$cache$initialization,fit$provenance$initialization),fit$provenance$initialization$recipe_version=='centered_final_marginal_role_scale_v1');cat('COLD PASS',name,'detached final-R roles, measure, fingerprint, runtime and init provenance retained\\n')}",
 "stopifnot(identical(bytes,serialize(fits,NULL,version=2)));cat('COLD COMPLETE',length(fits),'independent-process cache controls\\n')"),cold_script)
cold_log<-file.path(artifacts,'cold-cache-reader.log')
status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(cold_script),shQuote(cache_path)),stdout=cold_log,stderr=cold_log,env=c('OMP_NUM_THREADS=1','OPENBLAS_NUM_THREADS=1','MKL_NUM_THREADS=1'))
ok(identical(status,0L)&&sum(grepl('^COLD PASS ',readLines(cold_log)))==length(cold_fits),'independent cold R process reads all detached caches with unchanged bytes and final-R provenance')
ok(native_calls==0L,'terminal native toolchain and model guards receive no calls')
cat('PASSED',passed,'centered final-marginal R-only controls; compiler/sample and provider-metadata doubles; no native compiler, model or sampling calls\n')
cat('Cold persistence artifacts:',artifacts,'\n')

})
