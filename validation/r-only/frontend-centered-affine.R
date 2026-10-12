source(file.path("helpers", "native-guard.R"))
# R-only composition reporting. No compiler, backend execution or sampling.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(suppressWarnings(force(expr)),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
replace<-function(n,f)if(source_mode)assign(n,f,.GlobalEnv)else assignInNamespace(n,f,'bmeas')
fixed<-function(pair,value)list(pairs=pair,value=value)
d<-data.frame(person=as.character(1:4),x1=c(.2,.6,.1,-.4),x2=c(.8,.4,-.5,.1),x3=c(-.3,.2,.7,.5),x4=c(.6,-.1,.3,.9))
all4<-list(~x1:x2,~x1:x3,~x1:x4,~x2:x3,~x2:x4,~x3:x4)
correlations<-list(shared6=list(default='zero',equal=list(shared=all4)),
 shared2=list(default='zero',equal=list(shared=list(~x1:x2,~x3:x4))),
 fixed_positive=list(default='zero',fixed=lapply(all4,fixed,value=.2)),
 fixed_negative=list(default='zero',fixed=lapply(all4,fixed,value=-.2)))
prepare<-function(correlation,free=TRUE) bmeas(bmf(A=~x1+x2,B=~x3+x4,
 loadings=list(fixed=list(list(value=1,cells=list(A=~x1+x2,B=~x3+x4)))),
 cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(first=~A,second=~B))),
 residual=list(targets=~x1+x2+x3+x4,cor=correlation),
 person=list(distribution='sample_centered',ids=as.character(1:4)),
 augmentation=list(type='task_cholesky',targets=~x1+x2+x3+x4)),d,subject='person',fit=FALSE,
 prior=c(set_prior('normal(0,.2)','cor_raw',block='second'),if(free)set_prior('lkj(2)','cor',layer='parameter')))
fixtures<-lapply(names(correlations),function(n)prepare(correlations[[n]],startsWith(n,'shared')));names(fixtures)<-names(correlations)
fixtures$permuted<-bmeas(bmf(A=~x1,B=~x2,C=~x3,
 loadings=list(fixed=list(list(value=1,cells=list(A=~x1,B=~x2,C=~x3)))),
 cor=list(default='free',zero=list(~A+C),coordinates=list(type='ordered_blocks',blocks=list(first=~C+A,second=~B))),
 residual=list(targets=~x1+x2+x3,cor=list(default='zero',equal=list(shared=list(~x1:x2,~x1:x3,~x2:x3)))),
 person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3)),
 d,subject='person',fit=FALSE,prior=c(set_prior('normal(0,.2)','cor_raw',block='second'),set_prior('lkj(2)','cor',layer='parameter')))
fixtures$zero_common<-bmeas(bmf(
 cor=list(default='zero',coordinates=list(type='ordered_blocks',blocks=list())),
 residual=list(targets=~x1+x2+x3,cor=list(default='zero',equal=list(shared=list(~x1:x2,~x1:x3,~x2:x3)))),
 person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3)),
 d,subject='person',fit=FALSE,prior=set_prior('lkj(2)','cor',layer='parameter'))
# Count declarations from the actual emitted program, independently of the R inventory.
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
name_values<-function(field,x) {
 if(!length(x))return(numeric())
 labels<-if(is.matrix(x))paste0(field,'[',rep(seq_len(nrow(x)),ncol(x)),',',rep(seq_len(ncol(x)),each=nrow(x)),']')else paste0(field,'[',seq_along(x),']')
 stats::setNames(as.numeric(x),labels)
}
make_backend<-function(s,extreme=FALSE) {
 J<-nrow(s$tables$targets);K<-nrow(s$tables$components);U<-nrow(s$tables$parameter_residuals);N<-length(s$data_map$subject_keys)
 ix<-private('.bh_covariance_layout')(s)$indices;JT<-length(s$tables$augmentation$target)
 draws<-do.call(rbind,lapply(1:3,function(i) {
  v<-private('.bh_covariance_probe')(s,i);if(extreme)v[ix$component_cor]<-1e10
  st<-private('.bh_covariance_state')(s,v,covariance=FALSE)
  white<-matrix(.1*sin(seq_len(JT*(N-1))+i),JT,N-1)
  t<-private('.bh_task_state')(s,v,rep(.1*i,J),white)
  f<-list(alpha=rep(.1*i,J),loading=v[ix$loading],component_sd=v[ix$component_sd],obs_sd=sqrt(v[ix$observation_variance]),parameter_sd=v[ix$parameter_sd],parameter_row_sd=st$parameter_sd,
   correlation_B=v[ix$component_cor],F_component=st$F_component,R_component=st$R_component,R_parameter=st$R_parameter,L_parameter=t(chol(st$R_parameter)),
   z_task=t$z_task,task_white_unconstrained=white,task_delta=t$delta,effective_loadings=t$effective_loadings,Lambda=t$Lambda,D_parameter=t$D_parameter,
   component_covariance=t$component_covariance,component_covariance_available=t$component_covariance_available,component_covariance_log_abs=t$component_covariance_log_abs,
   log_component_row_sd=log(t$component_sd),Sigma_predictor_log_abs=t$Sigma_predictor_log_abs,
   Lambda_available=t$Lambda_available,effective_loadings_available=t$effective_loadings_available,D_parameter_available=t$D_parameter_available,
   Sigma_common_available=matrix(1,J,J),Sigma_parameter_available=matrix(1,J,J),Sigma_predictor_available=t$Sigma_predictor_available)
  unlist(lapply(names(f),function(n)name_values(n,f[[n]])),use.names=TRUE)
 }))
 calls<-new.env();calls$summary<-calls$draws<-0L;calls$monitored<-NULL
 select<-function(variables)unique(unlist(lapply(variables,function(v)if(v%in%colnames(draws))v else colnames(draws)[startsWith(colnames(draws),paste0(v,'['))]),use.names=FALSE))
 backend<-list(draws=function(variables,format){calls$draws<-calls$draws+1L;draws[,select(variables),drop=FALSE]},diagnostic_summary=function(quiet)list(num_divergent=0,num_max_treedepth=0,ebfmi=.4),output=function()character())
 backend$summary<-function(variables,...) {
  calls$summary<-calls$summary+1L;if(is.null(calls$monitored))calls$monitored<-variables
  do.call(rbind,lapply(select(variables),function(v){x<-draws[,v];data.frame(variable=v,mean=mean(x),median=median(x),sd=sd(x),q5=unname(quantile(x,.05)),q95=unname(quantile(x,.95)),rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.5)}))
 }
 list(backend=backend,calls=calls,draws=draws)
}
local({
 terminal<-0L;fit_calls<-0L;old_compile<-private('.bh_compile_scalar');old_fitter<-private('.bh_fit_scalar')
 on.exit({replace('.bh_compile_scalar',old_compile);replace('.bh_fit_scalar',old_fitter)})
 replace('.bh_compile_scalar',function(...){terminal<<-terminal+1L;stop('unexpected compiler')})
 replace('.bh_fit_scalar',function(...){fit_calls<<-fit_calls+1L;private('.mp_abort')('E_TERMINAL_TEST','fitter trap')})
 for(name in names(fixtures)) {
  s<-fixtures[[name]];x<-standata(s);code<-stancode(s,log_lik=TRUE)
  checks<-check_model(s);p<-get_prior(s);map<-private('.bh_affine_summary_map')(s);reference<-private('.bh_reference_summary_map')(s)
  ok(checks$backend$status=='experimental_centered_task_raw_affine_residual'&&private('.bh_affine_runtime_supported')(s),'combined execution requires explicit experimental profile')
  ok(all(map$layer=='parameter')&&nrow(map)==x$EU,'only residual final labels enter affine primitive summaries')
  ok(sum(reference$parameter_role=='cor_raw')==x$RB&&sum(reference$parameter_role=='derived_marginal_component_correlation_entry')==sum(s$tables$correlations$state=='free'),'raw primitives and derived common R retain distinct roles')
  ok(all(map$computational_mapping=='one_label_interval_coordinate')&&all(map$computational_variable=='parameter_correlation_raw[1]'),'one-label residual keeps its single z link')
  ok(sum(p$class=='loading')==0L&&sum(p$class=='cor'&p$layer=='parameter')==as.integer(x$EU>0),'fixed loadings and fixed residual layer add no prior coordinate')
  if(x$EU)ok(nrow(map$alias_cells[[1L]])==sum(s$tables$parameter_correlations$state=='free'),'alias multiplicity changes cells without multiplying rho')
  ini<-checks$initialization;ci<-ini$correlation_initialization
  ok(nrow(ci)==1L&&ci$layer=='parameter'&&ci$unique_parameters==x$EU,'initial density/interval ledger excludes raw common determinant prior')
  if(!x$EU)ok(ci$joint_prior_kernel==0&&ci$chart_log_jacobian==0,'fixed R_u has zero correlation kernel and Jacobian')
  else ok(is.finite(ci$joint_prior_kernel)&&is.finite(ci$chart_log_jacobian),'resolved midpoint has a finite once-only kernel and Jacobian')
  declared<-output_declarations(code,x);inventory<-private('.bh_reference_output_inventory')(s,TRUE)
  key<-function(t)paste(t$block,t$variable,sep=':');at<-match(key(inventory),key(declared))
  ok(!anyNA(at)&&setequal(key(inventory),key(declared))&&all(inventory$elements==declared$elements[at]),'all emitted parameter/transformed/generated dimensions match inventory')
  size<-private('.bh_reference_output_reals')(s,TRUE)
  ok(size$total==sum(declared$elements)+66&&size$sampled_white_coordinates==x$JT*(x$N-1)&&size$constrained_white_values==x$JT*x$N,'output estimate separates free white axes and constrained outputs')
  init<-private('.bh_materialize_init')(s,ini)
  if(x$EU==1L)ok(identical(dim(init$parameter_correlation_raw),1L),'singleton residual initial vector keeps rank one')
  else ok(!'parameter_correlation_raw'%in%names(init),'zero residual coordinates are omitted from initialization')
  b<-make_backend(s);plan<-list(posterior_propriety=checks$posterior_propriety,posterior_normalizability=checks$posterior_normalizability,gaussian_diagnostics=checks$gaussian_diagnostics,identification=checks$identification,execution_profile=list(used='R_cache_double_only'),backend_qualification=checks$backend)
  err(bmeas(s,chains=1,iter=20,warmup=10,resources='development'),'E_EXECUTION_PROFILE')
  err(bmeas(s,chains=1,iter=20,warmup=10,execution='experimental',exploratory=TRUE,resources='development'),'E_TERMINAL_TEST')
  # Actual cache construction reads only this deterministic backend double.
  cache<-private('.bh_cache_scalar_fit')(b$backend,s,plan)
  ok(grepl('explicit experimental',cache$correlation_constraints$runtime_scope,fixed=TRUE),'cached qualification records explicit experimental scope')
  tab<-cache$parameter_summary;expanded<-cache$expanded_correlation_matrices$entries
  ok(sum(tab$parameter_role=='cor_raw')==x$RB&&sum(tab$parameter_role=='unique_marginal_correlation')==x$EU,'actual cache retains raw B and one residual primitive separately')
  common<-tab[grepl('^R_component\\[',tab$variable),,drop=FALSE]
  ok(nrow(common)==sum(s$tables$correlations$state=='free')&&all(common$parameter_role=='derived_marginal_component_correlation_entry'),'common posterior correlation rows are derived')
  freecommon<-expanded$layer=='component'&expanded$role!='fixed_correlation_constant'
  ok(all(expanded$role[freecommon]=='derived_common_correlation')&&all(is.na(expanded$parameter_key[freecommon])),'expanded raw-common cells have no invented marginal primitive link')
  fixed<-expanded$role=='fixed_correlation_constant'
  ok(all(is.na(expanded$sd[fixed]))&&all(is.na(expanded$rhat[fixed])),'fixed matrix cells have no invented uncertainty diagnostics')
  ok(!any(grepl('^(z_raw|e_raw|z|f)\\[',tab$variable))&&sum(tab$parameter_role=='task_white_basis_coordinate')==x$JT*(x$N-1),'task whites are not common/residual person scores')
  ok(identical(cache$reference_law$person_roster,s$tables$person_roster)&&identical(cache$reference_law$augmentation,s$tables$augmentation),'roster and task order survive cache construction')
  ok(cache$inference$status=='not_assessed'&&all(tab$rhat==2),'poor diagnostics remain visible without an inference claim')
  fit<-structure(list(spec=s,cache=cache,run=list(),backend=NULL,provenance=list(execution_profile=plan$execution_profile)),class=c('bmeas_fit','bhmm_fit'))
  copy<-unserialize(serialize(fit,NULL,version=2));before<-c(b$calls$summary,b$calls$draws);bytes<-serialize(copy,NULL,version=2)
  invisible(capture.output(print(summary(copy))))
  ok(identical(posterior_summary(copy),tab)&&identical(before,c(b$calls$summary,b$calls$draws))&&identical(bytes,serialize(copy,NULL,version=2)),'detached reporting preserves bytes without backend contact')
 }
 ok(terminal==0L&&fit_calls==length(fixtures),'only the intended terminal fitter doubles are reached')
})
# Exact population inverse for the scoped centered Gaussian control.
s<-fixtures$shared6;l<-private('.bh_covariance_layout')(s);v<-numeric(length(l$names))
v[l$indices$component_cor]<-.4;v[l$indices$parameter_sd]<-c(.6,.8,1,1.2);v[l$indices$parameter_cor]<-.2;v[l$indices$observation_variance]<-c(.25,.36,.49,.64)
st<-private('.bh_covariance_state')(s,v);S<-st$Sigma_predictor;H<-private('.bh_centering_covariance')(s)
V<-kronecker(H,S)+kronecker(diag(4),diag(v[l$indices$observation_variance]));recovered<- -3*V[1:4,5:8]
u<-sqrt(diag(recovered)-1);rho<-(recovered[1,2]-1)/(u[1]*u[2]);r<-recovered[1,3]-rho*u[1]*u[3]
b<-r/sqrt(1-r*r);noise<-diag(V[1:4,1:4])-diag(recovered)
ok(max(abs(c(u-v[l$indices$parameter_sd],rho-.2,b-.4,noise-v[l$indices$observation_variance])))<1e-12,'known centered population covariance has a unique positive-interior inverse')
c<-1.01;other<-v;other[l$indices$parameter_sd]<-c*v[l$indices$parameter_sd];other[l$indices$parameter_cor]<-.2/c^2
other[l$indices$observation_variance]<-v[l$indices$observation_variance]+(1-c^2)*v[l$indices$parameter_sd]^2
S2<-private('.bh_covariance_state')(s,other)$Sigma_predictor
ok(max(abs(S+diag(v[l$indices$observation_variance])-S2-diag(other[l$indices$observation_variance])))<1e-12&&max(abs(S-S2))>1e-3,'free-rho iid alias does not preserve centered cross-person covariance')
v[l$indices$component_cor]<-1e10
state<-private('.bh_task_state')(s,v,rep(0,4),matrix(.1,4,3))
ok(all(is.finite(state$delta))&&inherits(try(chol(state$R_component),silent=TRUE),'try-error'),'direct common factor remains usable when rounded common R cannot be Cholesky factored')
ok(check_model(s)$identification$status=='not_assessed','scoped inverse is not promoted to a generic centered identification claim')
# The retained zero/free reference schema did not require affine role tables.
# This is an internal table-shape control, never a resealed public context.
ordinary<-prepare('free');old_shape<-ordinary;old_shape$tables$correlation_primitives<-NULL
bytes<-serialize(old_shape,NULL,version=2)
ok(identical(private('.bh_reference_output_reals')(ordinary,TRUE),private('.bh_reference_output_reals')(old_shape,TRUE))&&
 identical(bytes,serialize(old_shape,NULL,version=2)),'ordinary reference output counts retain the accepted older table shape without mutation')
# A genuine in-profile negative: free G plus one free-amplitude basis column.
Q<-matrix(c(-1,0,1),3,1,dimnames=list(c('x1','x2','x3'),'linear'))
negative<-bmeas(bmf(G=~x1+x2+x3,P=component(~x1+x2+x3,Q,'shared'),
 cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(first=~G,second=~P))),
 residual=list(targets=~x1+x2+x3,cor=list(default='zero',equal=list(shared=list(~x1:x2,~x1:x3,~x2:x3)))),
 person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3)),
 d,subject='person',fit=FALSE,prior=c(set_prior('normal(0,.2)','cor_raw',block='second'),set_prior('lkj(2)','cor',layer='parameter'),set_prior('half_normal(1)','amplitude',component='P')))
layout<-private('.bh_covariance_layout')(negative);v<-private('.bh_covariance_probe')(negative,1)
v[layout$indices$loading]<-c(.8,.7,.9);v[layout$indices$amplitude]<-.6;v[layout$indices$component_cor]<-.2
gamma<-.2/sqrt(1+.2^2);gnew<-.35;hnew<-.6*sqrt(1-gamma^2)/sqrt(1-gnew^2);other<-v
other[layout$indices$loading]<-v[layout$indices$loading]+(gamma*.6-gnew*hnew)*Q[,1]
other[layout$indices$amplitude]<-hnew;other[layout$indices$component_cor]<-gnew/sqrt(1-gnew^2)
a<-private('.bh_task_state')(negative,v,rep(0,3),matrix(.1,3,3));b<-private('.bh_task_state')(negative,other,rep(0,3),matrix(.1,3,3))
ok(max(abs(a$Sigma_predictor-b$Sigma_predictor))<1e-12&&max(abs(a$delta-b$delta))<1e-12&&max(abs(v-other))>.01,'isolated G/basis shear preserves the centered law and same task-white predictors')
ok(check_model(negative)$identification$status=='proven_unidentified','in-profile structural alias remains a hard identification negative')
err(stancode(negative),'E_IDENTIFICATION')
iid<-bmeas(bmf(A=~x1,B=~x2,C=~x3,D=~x4,
 loadings=list(fixed=list(list(value=1,cells=list(A=~x1,B=~x2,C=~x3,D=~x4)))),
 cor=list(default='zero',equal=list(shared=list(~A:B,~C:D)))),d,subject='person',fit=FALSE)
tab<-iid$tables$correlations;at<-which(tab$state=='free')
expected<-tab[at,c('row','col','component1','component2'),drop=FALSE];names(expected)[3:4]<-c('endpoint1','endpoint2')
ok(identical(private('.bh_affine_summary_map')(iid)$alias_cells[[1L]],expected),'retained iid alias table preserves exact values and integer row-name storage')
cat('PASSED',passed,'centered affine lifecycle controls; terminal doubles only\n')
