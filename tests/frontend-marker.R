source(file.path("helpers", "native-guard.R"))
# Marker declarations and actual effective covariance maps; no native backend.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
d<-data.frame(person=rep(1:4,2),x1=sin(1:8*.5),x2=cos(1:8*.7)+1:8/5,x3=sin(1:8*.3)+1:8/7)
prepare<-function(m,prior=NULL,data=d)bhmm(m,data,subject='person',prior=prior,fit=FALSE)
state<-function(s,v=NULL)private('.bh_covariance_state')(s,if(is.null(v))private('.bh_covariance_probe')(s,1)else v)
layout<-function(s)private('.bh_covariance_layout')(s)
marked<-function(value=1)bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1,value=value)))
s<-prepare(marked());sc<-model_table(s,'component_scales');p<-get_prior(s)
ok(nrow(model_table(s,'loading_primitives'))==2L&&sum(p$class=='component_sd')==1L,'marker removes one beta primitive and adds one component SD')
ok(all(model_table(s,'loading_primitives')$sign=='free')&&model_table(s,'orientation')$pinned,'positive marker pins orientation without another positive beta anchor')
ok(sc$state=='free'&&sc$marker_value==1&&sc$primitive_index==1L,'explicit marker role and unique SD index')
ok(is.na(model_table(s,'components')$variance)&&model_table(s,'components')$standardized_variance==1,'scientific free variance differs from standardized unit variance')
st<-state(s);ok(st$Lambda[1,1]==1&&max(abs(st$effective_loadings-st$Lambda*st$component_sd))<1e-15,'declared Lambda and effective beta times SD stay distinct')
ok(max(abs(st$Sigma_predictor-st$effective_loadings%*%st$R_component%*%t(st$effective_loadings)))<1e-15,'effective covariance map')
ok(all(check_model(s)$identification$probe_ranks==6L),'marker Gaussian covariance has expected full local rank')
ok(check_model(s)$backend$status=='deterministic_gaussian_qualified','new scale generator retains bounded fixed-state qualification')
data<-standata(s);code<-as.character(stancode(s))
ok(data$CS==1L&&data$L==2L&&data$A==0L&&data$F==2L&&data$component_sd_index==1L,'lowered dimensions distinguish scale primitive from fixed marker beta')
ok(grepl('transpose(z)*transpose(effective_loadings)',code,fixed=TRUE),'native predictors use effective loadings and standardized scores')
ok(is.list(private('.bh_execution_gate')(s)), 'qualified marker declaration passes pre-backend policy without sampling')
negative<-prepare(marked(-2));ok(state(negative)$Lambda[1,1]==-2&&all(model_table(negative,'loading_primitives')$sign=='free'),'negative marker retains sign and signed remaining beta')
omitted<-prepare(bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1))))
ok(identical(model_table(omitted),model_table(s)),'omitted marker value is exactly explicit one')
ordinary<-prepare(bhmf(G=~x1+x2+x3));empty<-prepare(bhmf(G=~x1+x2+x3,scale=list()))
ok(identical(model_table(ordinary),model_table(empty))&&all(state(empty)$component_sd==1),'NULL and empty scale lists preserve unit component model')
ok(identical(state(ordinary)$Lambda,state(ordinary)$effective_loadings)&&!any(get_prior(ordinary)$class=='component_sd'),'unit path has no dummy SD prior')
for(value in list(NULL,0,-0,Inf,NA_real_,c(1,2),1+1i,structure(1,class='fake')))
 err(bhmf(G=~x1,scale=list(G=list(marker=~x1,value=value))),'E_MARKER_SCALE')
err(bhmf(G=~x1,scale=list(H=list(marker=~x1))),'E_MARKER_SCALE')
err(bhmf(G=~x1+x2,scale=list(G=list(marker=~x1+x2))),'E_MARKER_SCALE')
err(bhmf(G=~x1,scale=list(G=list(marker=~x1,extra=1))),'E_MARKER_SCALE')
err(prepare(bhmf(G=~x1+x2,scale=list(G=list(marker=~x3)))),'E_LOADING_COVERAGE')
err(prepare(bhmf(G=~x1+x2+x3,loadings=list(fixed=list(list(value=2,cells=list(G=~x1)))),scale=list(G=list(marker=~x1)))),'E_LOADING_CONFLICT')
fixed<-prepare(bhmf(G=~x1+x2+x3,loadings=list(fixed=list(list(value=2,cells=list(G=~x1+x2+x3)))),scale=list(G=list(marker=~x1,value=2))))
ok(nrow(model_table(fixed,'loading_primitives'))==0L&&sum(get_prior(fixed)$class=='component_sd')==1L,'all fixed beta still has one uncertain population SD')
ok(all(state(fixed)$Lambda==2)&&all(state(fixed)$effective_loadings!=2),'fixed declared beta has variable effective loading')
err(prepare(marked(),set_prior('normal(0,1)','loading',target=~x1)),'E_PRIOR_SELECTOR')
err(set_prior('half_normal(1)','component_sd',target=~x1),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','component_sd',component='G'),'E_PRIOR')
err(prepare(bhmf(G=~x1+x2+x3),set_prior('half_normal(1)','component_sd',component='G')),'E_PRIOR_SELECTOR')
explicit<-prepare(marked(),set_prior('half_student_t(4,0,.6)','component_sd',component='G'))
ok(get_prior(explicit)$prior[get_prior(explicit)$class=='component_sd']=='half_student_t(4,0,.6)','explicit positive SD prior matches one role')
ok(identical(get_prior(explicit)$prior[get_prior(explicit)$class=='loading'],p$prior[p$class=='loading']),'user SD prior does not change beta-default calibration denominator')
# Marker propagation through one class does not silently mark its other columns.
shared_class<-list(equal=list(marker_pair=list(G=~x1,H=~x2)))
mixed<-prepare(bhmf(G=~x1+x2,H=~x2+x3,cor='free',loadings=shared_class,scale=list(G=list(marker=~x1))))
ok(sum(get_prior(mixed)$class=='component_sd')==1L&&identical(model_table(mixed,'component_scales')$state,c('free','fixed')),'only explicitly marked component receives a scale')
ok(all(model_table(mixed,'ordinary_loading_map')$fixed_value[c(1,3)]==1),'one marker fixes both aliases without duplicating SDs')
both<-prepare(bhmf(G=~x1+x2,H=~x2+x3,cor='free',loadings=shared_class,scale=list(G=list(marker=~x1),H=list(marker=~x2))))
ok(sum(get_prior(both)$class=='component_sd')==2L&&all(check_model(both)$identification$probe_ranks==8L),'asymmetric shared marker class has separate SDs and full local rank')
err(prepare(bhmf(G=~x1+x2,H=~x2+x3,loadings=shared_class,scale=list(G=list(marker=~x1),H=list(marker=~x2,value=2)))),'E_LOADING_CONFLICT')
# The symmetric marked two-column pattern retains a known global alias.
crossed<-prepare(bhmf(G=~x1+x2,H=~x1+x2,cor='free',loadings=list(equal=list(offdiag=list(G=~x2,H=~x1))),scale=list(G=list(marker=~x1,value=-1),H=list(marker=~x2,value=2))))
v<-private('.bh_covariance_probe')(crossed,1);ix<-layout(crossed)$indices
v[ix$loading]<-.4;v[ix$component_sd]<-c(.8,.6);v[ix$component_cor]<-.27
w<-v;w[ix$loading]<- -2/.4;w[ix$component_sd]<-c(.4*.6,.2*.8);w[ix$component_cor]<- -.27
ok(max(abs(state(crossed,v)$Sigma_predictor-state(crossed,w)$Sigma_predictor))<1e-13,'distinct signed marker permutation preserves exact covariance')
ok(check_model(crossed)$identification$status=='proven_unidentified'&&!is.null(check_model(crossed)$identification$marker_nonidentification),'crossed marker global alias proof distinct from local rank')
err(stancode(crossed),'E_IDENTIFICATION')
# Basis and share-derived columns cannot acquire a second scale.
Q<-matrix(c(1,-1,1),3,1,dimnames=list(c('x1','x2','x3'),'q'))
err(prepare(bhmf(P=component(~x1+x2+x3,Q,loading='shared'),scale=list(P=list(marker=~x1)))),'E_MARKER_DERIVED')
owner<-list(type='variance_share',targets=~x1+x2,components=~G,residual=TRUE,shares=~1)
err(prepare(bhmf(G=~x1+x2+x3,decomposition=list(a=owner),scale=list(G=list(marker=~x3)))),'E_MARKER_DERIVED')
shear<-prepare(bhmf(G=~x1+x2+x3,P=component(~x1+x2+x3,Q,loading='shared'),cor='free',scale=list(G=list(marker=~x1))),set_prior('half_normal(1)','amplitude'))
ok(check_model(shear)$identification$status=='proven_unidentified'&&!is.null(check_model(shear)$identification$marker_nonidentification)&&is.null(check_model(shear)$identification$basis_nonidentification),'marked scale reopens continuous shear without using old unit-SD discrete proof')
# Default proposals transform coherently under a positive marker-value change.
base<-prepare(marked(.5));other<-prepare(marked(3.5));bp<-get_prior(base);op<-get_prior(other)
ok(abs(op$calibration_scale[op$class=='component_sd']*7-bp$calibration_scale[bp$class=='component_sd'])<1e-13,'marker SD default rescales by reciprocal marker ratio')
ok(max(abs(op$calibration_scale[op$class=='loading']/7-bp$calibration_scale[bp$class=='loading']))<1e-13,'nonmarker beta defaults scale by marker ratio')
huge<-prepare(marked(1e-320));hp<-get_prior(huge)
ok(hp$status[hp$class=='component_sd']=='missing'&&all(hp$status[hp$class=='loading']=='specified'),'overflowing SD default does not erase separately representable beta ratios')
repaired<-prepare(marked(1e-320),c(set_prior('half_normal(1)','component_sd'),set_prior('normal(0,1)','loading')))
ok(check_model(repaired)$priors$status=='complete'&&model_table(repaired,'component_sd_calibration')$status=='unrepresentable_default_ratio','valid explicit priors replace invalid defaults while provenance remains')
v<-private('.bh_covariance_probe')(s,1);v[layout(s)$indices$component_sd]<-1e308;v[layout(s)$indices$loading]<-2
err(state(s,v),'E_COMPONENT_SCALE_NUMERICAL')
# Fixed beta does not prevent shrinking its free component SD to zero.
constant<-data.frame(person=1:3,x1=1,x2=c(.2,.6,-.3),x3=c(.9,1.4,.2))
collapse<-prepare(marked(),data=constant)
ok(check_model(collapse)$posterior_propriety$status=='proven_improper'&&check_model(collapse)$posterior_propriety$findings$independently_shrinking_coefficients[1]==1L,'constant marker target has one shrinking SD, not a fixed-effect blocker')
repeats<-prepare(marked(),data=rbind(constant,constant))
ok(any(grepl('exact_within_person',check_model(repeats)$posterior_propriety$findings$sufficient_paths)),'marked component retains exact-repeat observation-noise collapse proof')
# Captured selectors and tables survive caller mutation and RDS.
selector<-'x1';m<-bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~scores$mu[selector])))
selector<-'x3';copy<-prepare(m)
ok(model_table(copy,'component_scales')$marker_target==targets('scores','mu','x1'),'marker selectors are captured before binding')
captured_tables<-model_table(copy)
for(i in 1:3){copy<-unserialize(serialize(copy,NULL));ok(identical(model_table(copy),captured_tables),'marker context and selector persist with no environment dependency')}
for(c in c(1e-300,1e-100,1e-10,1e10,1e100,1e300)) {
 z<-prepare(marked(c))
 ok(all(check_model(z)$identification$probe_ranks==6L),'equivalent marker units preserve full diagnostic rank')
}
# Unrepresentable derived exports cannot erase a representable effective map.
z<-prepare(marked(1e300));zs<-state(z)
ok(all(is.finite(zs$effective_loadings))&&all(is.finite(zs$Sigma_predictor))&&zs$component_covariance_status=='unrepresentable_export'&&is.na(zs$component_covariance[1,1]),'underflowed SD-squared export is unavailable while effective covariance remains valid')
# Genuine K0 and zero-effect branches still have typed empty scale tables.
k0<-prepare(bhmf(residual=list(targets=~x1+x2+x3)))
ok(nrow(model_table(k0,'component_scales'))==0L&&identical(dim(state(k0)$effective_loadings),c(3L,0L)),'common-empty residual model has no dummy scale or loading')
# A registered custom density with no calibration leaves every needed default missing.
cm<-register_stan_adapter(new_observation_model('marker_normal','1',response_schema(y='real'),
 list(mu=parameter_spec('real','identity','theta','subject_task','points','Location'),sigma=parameter_spec('positive','log','theta','task','points','Noise')),
 stan_density(symbol='marker_normal_lpdf',arguments=c('y','mu','sigma'),argument_types=rep('real',3),normalized=TRUE,approximation='none',builtin=TRUE,license='MIT'),model_capabilities(fit=TRUE)),
 function_code='real marker_normal_lpdf(real y, real mu, real sigma) { return normal_lpdf(y | mu, sigma); }',function_exports='marker_normal_lpdf')
ld<-data.frame(person=rep(1:4,3),task=rep(c('x1','x2','x3'),each=4),y=sin(1:12*.3))
block<-observe(cm,ld,'person','task','y',units='points')
formula<-bhmf(G=~a$mu[c('x1','x2','x3')],scale=list(G=list(marker=~a$mu['x1'])))
unknown<-bhmm(formula,data=list(a=block),fit=FALSE)
ok(check_model(unknown)$priors$status=='incomplete'&&all(get_prior(unknown)$status[get_prior(unknown)$class%in%c('loading','component_sd')]=='missing'),'unknown calibration does not invent a unit-scale prior')
explicit<-c(set_prior('normal(0,1)','Intercept'),set_prior('normal(0,1)','loading'),set_prior('half_normal(1)','sigma'),set_prior('half_normal(.7)','component_sd'))
resolved<-bhmm(formula,data=list(a=block),prior=explicit,fit=FALSE)
ok(check_model(resolved)$priors$status=='complete'&&model_table(resolved,'component_sd_calibration')$status=='missing_marker_calibration','explicit custom priors resolve roles while missing default evidence stays visible')
overflow_sum<-prepare(bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1)),residual=list(targets=~x1+x2+x3)))
v<-private('.bh_covariance_probe')(overflow_sum,1);ix<-layout(overflow_sum)$indices
v[ix$component_sd]<-1e154;v[ix$parameter_sd]<-1e154
err(state(overflow_sum,v),'E_STRUCTURE_STATE')
named_marker<-prepare(bhmf(components=stats::setNames(list(~x1+x2+x3),'Memory; Speed'),scale=stats::setNames(list(list(marker=~x1)),'Memory; Speed')))
nt<-model_table(named_marker,'component_scales')
ok(nt$affected_class_members==nt$marker_cell,'delimiter-containing names use structured marker class membership')
misleading_label<-prepare(bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1)),loadings=list(equal=stats::setNames(list(list(G=~x2+x3)),'foo; marker:G'))))
ml<-model_table(misleading_label,'component_scales')
ok(ml$affected_class_members==ml$marker_cell,'literal equality label cannot impersonate a marker provenance record')
wide_scales<-data.frame(person=rep(1:4,2),x1=sin(1:8*.5)*1e-100,x2=(cos(1:8*.7)+1:8/5)*1e100,x3=sin(1:8*.3)+1:8/7)
proposal<-prepare(marked(1e200),data=wide_scales);cal<-model_table(proposal,'loading_prior_calibration');bad<-cal[cal$target==targets('scores','mu','x2'),]
ok(bad$proposal_status=='unrepresentable_beta_ratio'&&is.infinite(bad$proposed_scale)&&is.finite(bad$original_target_scale)&&is.finite(bad$marker_sd_proposal),'unrepresentable beta proposal retains both finite operands and explicit failure reason')
ok(nzchar(bad$observations)&&bad$original_source=='data_calibrated_MAD'&&bad$policy=='marker_scaled_singleton','failed transformed default retains response lineage and truthful policy')
repaired<-prepare(marked(1e200),set_prior('normal(0,1)','loading',target=~x2),data=wide_scales)
ok(check_model(repaired)$priors$status=='complete'&&model_table(repaired,'loading_prior_calibration')$proposal_status[1]=='unrepresentable_beta_ratio','beta-only explicit prior repairs missing default while failed proposal remains inspectable')
tiny_sd<-prepare(bhmf(G=~x1+x2+x3,loadings=list(fixed=list(list(value=1,cells=list(G=~x1+x2+x3)))),scale=list(G=list(marker=~x1))))
v<-private('.bh_covariance_probe')(tiny_sd,1);v[layout(tiny_sd)$indices$component_sd]<-1e-200
err(state(tiny_sd,v),'E_STRUCTURE_STATE')
v<-private('.bh_covariance_probe')(k0,1);v[layout(k0)$indices$parameter_sd]<-1e-200
err(state(k0,v),'E_STRUCTURE_STATE')
cat('Passed',passed,'marker R/IR checks\n')
