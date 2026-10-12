source(file.path("helpers", "native-guard.R"))
# Coherent R draw doubles exercise reporting only, with no backend execution.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
fixed<-function(pair,value)list(pairs=pair,value=value)
d<-data.frame(person=rep(1:4,2));for(j in 1:6)d[[paste0('x',j)]]<-sin(seq_len(8)*j/7)+seq_len(8)/9
forms<-list(
 fixed=bmf(A=~x1+x2,B=~x3+x4,cor=list(default='zero',fixed=list(fixed(~A:B,.3)))),
 tied=bmf(A=~x1,B=~x2,C=~x3,D=~x4,cor=list(default='zero',equal=list(shared=list(~A:B,~C:D)))),
 residual=bmf(residual=list(targets=~x1+x2+x3,cor=list(default='zero',fixed=list(fixed(~x1:x2,-.2),fixed(~x1:x3,-.2),fixed(~x2:x3,-.2))))),
 both=bmf(A=~x1,B=~x2,C=~x3,cor=list(default='zero',equal=list(shared=list(~A:B,~A:C,~B:C))),residual=list(targets=~x4+x5+x6,cor=list(default='zero',equal=list(shared=list(~x4:x5,~x4:x6,~x5:x6))))),
 marked=bmf(G=~x1+x2,H=~x2+x3,cor=list(default='zero',fixed=list(fixed(~G:H,.3))),scale=list(G=list(marker=~x1),H=list(marker=~x2))),
 common_mgs=bmf(A=~x1,B=~x2,C=~x3,D=~x4,cor='free',residual=list(targets=~x5+x6,cor=list(default='zero',fixed=list(fixed(~x5:x6,.2))))),
 residual_mgs=bmf(A=~x1,B=~x2,cor=list(default='zero',fixed=list(fixed(~A:B,.3))),residual=list(targets=~x3+x4+x5+x6,cor='free')),
 positive=bmf(A=~x1+x2,B=~x3+x4,C=~x5+x6,cor=list(default='free',fixed=list(fixed(~A:C,.9),fixed(~B:C,.9)))))
name_values<-function(field,x) {
 if(!length(x))return(numeric())
 labels<-if(is.matrix(x))paste0(field,'[',rep(seq_len(nrow(x)),ncol(x)),',',rep(seq_len(ncol(x)),each=nrow(x)),']')else paste0(field,'[',seq_along(x),']')
 stats::setNames(as.numeric(x),labels)
}
for(name in names(forms)) {
 s<-bmeas(forms[[name]],d,subject='person',fit=FALSE)
 J<-nrow(s$tables$targets);K<-nrow(s$tables$components);U<-nrow(s$tables$parameter_residuals);N<-length(s$data_map$subject_keys)
 ix<-private('.bh_covariance_layout')(s)$indices
 rows<-lapply(c(1L,2L,3L,1L),function(i) {
  values<-private('.bh_covariance_probe')(s,i);st<-private('.bh_covariance_state')(s,values)
  L<-if(K)t(chol(st$R_component))else matrix(numeric(),0,0);Lu<-if(U)t(chol(st$R_parameter))else matrix(numeric(),0,0)
  raw<-matrix(.2*sin(seq_len(K*N)+i),K,N);eraw<-matrix(.2*cos(seq_len(U*N)+i),U,N);z<-L%*%raw
  f<-sweep(z,1,st$component_sd,`*`)
  fields<-list(alpha=rep(.1*i,J),loading=values[ix$loading],component_sd=values[ix$component_sd],obs_sd=sqrt(values[ix$observation_variance]),parameter_sd=values[ix$parameter_sd],parameter_row_sd=st$parameter_sd,
   L_component=L,R_component=st$R_component,L_parameter=Lu,R_parameter=st$R_parameter,z_raw=raw,e_raw=eraw,z=z,f=f,effective_loadings=st$effective_loadings,component_covariance=st$component_covariance,
   log_component_row_sd=st$log_component_sd,component_covariance_log_abs=st$component_covariance_log_abs,f_log_abs=sweep(log(abs(z)),1,st$log_component_sd,`+`),
   Sigma_common_available=matrix(1,J,J),Sigma_parameter_available=matrix(1,J,J),Sigma_predictor_available=matrix(1,J,J),component_covariance_available=matrix(as.integer(!st$component_covariance_unavailable),K,K),f_available=matrix(1,K,N))
  unlist(lapply(names(fields),function(n)name_values(n,fields[[n]])),use.names=TRUE)
 })
 draws<-do.call(rbind,rows);calls<-new.env();calls$summary<-calls$draws<-0L;calls$monitored<-NULL
 select<-function(variables)unique(unlist(lapply(variables,function(v)if(v%in%colnames(draws))v else colnames(draws)[startsWith(colnames(draws),paste0(v,'['))]),use.names=FALSE))
 backend<-list(draws=function(variables,format){calls$draws<-calls$draws+1L;draws[,select(variables),drop=FALSE]},diagnostic_summary=function(quiet)list(num_divergent=0,num_max_treedepth=0,ebfmi=.4),output=function()character())
 backend$summary<-function(variables,...) {
  calls$summary<-calls$summary+1L;if(is.null(calls$monitored))calls$monitored<-variables
  do.call(rbind,lapply(select(variables),function(v){x<-draws[,v];data.frame(variable=v,mean=mean(x),median=median(x),sd=sd(x),q5=unname(quantile(x,.05)),q95=unname(quantile(x,.95)),rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.5)}))
 }
 checks<-check_model(s);plan<-list(posterior_propriety=checks$posterior_propriety,gaussian_diagnostics=checks$gaussian_diagnostics,identification=checks$identification,execution_profile=list(used='cache_double_only'),backend_qualification=checks$backend)
 cache<-private('.bh_cache_scalar_fit')(backend,s,plan);tab<-cache$parameter_summary;pr<-s$tables$correlation_primitives
 corr<-tab[tab$parameter_role=='unique_marginal_correlation',,drop=FALSE]
 ok(nrow(corr)==nrow(pr)&&!anyDuplicated(corr$parameter_key),'one posterior row per unique marginal label')
 ok(sum(grepl('^R_component\\[|^R_parameter\\[',calls$monitored))==nrow(pr)&&!any(c('R_component','R_parameter')%in%calls$monitored),'fixed and repeated matrix entries are excluded from primitive monitors')
 if(nrow(pr))ok(all(vapply(corr$correlation_alias_cells,nrow,integer(1))==pr$members[match(corr$parameter_key,pr$parameter)]),'structured alias membership is retained')
 map<-cache$correlation_constraints$primitive_summary_map
 if(name%in%c('common_mgs','residual_mgs')) {
  layer<-if(name=='common_mgs')'component'else'parameter';at<-map$layer==layer
  ok(sum(at)==6L&&all(is.na(map$computational_variable[at]))&&all(map$computational_mapping[at]=='MGS_vector_map_no_scalar_association'),'MGS marginal summaries have no false scalar raw association')
  h<-1e-6;zero<-numeric(6);up<-down<-zero;up[4]<-h;down[4]<- -h
  state<-private('.bh_affine_layer_state')
  g4<-(state(s,layer,up)$R[1,4]-state(s,layer,down)$R[1,4])/(2*h)
  up<-down<-zero;up[3]<-h;down[3]<- -h
  g3<-(state(s,layer,up)$R[1,4]-state(s,layer,down)$R[1,4])/(2*h)
  ok(abs(g4-1)<1e-8&&abs(g3)<1e-8,'MGS packing follows row blocks, not pair-table order')
  q<-seq_len(6)*.04
  grad<-vapply(1:6,function(j){a<-b<-q;a[j]<-a[j]+h;b[j]<-b[j]-h;(state(s,layer,a)$R[1,4]-state(s,layer,b)$R[1,4])/(2*h)},numeric(1))
  ok(sum(abs(grad)>1e-7)>1L,'a generic MGS rho depends jointly on several raw coordinates')
 }else if(nrow(map))ok(all(map$computational_mapping=='one_label_interval_coordinate')&&!anyNA(map$computational_variable),'one-label chart alone retains its scalar z association')
 ok(identical(cache$correlation_constraints$primitives,pr)&&identical(cache$correlation_constraints$prior_geometry,s$tables$correlation_prior_geometry),'unique prior measure and scientific metadata persist')
 expanded<-cache$expanded_correlation_matrices;constants<-expanded$entries[expanded$entries$role=='fixed_correlation_constant',,drop=FALSE]
 ok(expanded$status=='available'&&nrow(expanded$entries)==K*K+U*U,'expanded matrix summaries retain all cells separately')
 ok(all(is.na(constants$rhat))&&all(is.na(constants$sd))&&all(constants$mean==constants$fixed_value),'fixed constants have no invented uncertainty diagnostics')
 ok(all(tab$rhat==2)&&all(tab$ess_bulk==5)&&cache$inference$status=='not_assessed','poor diagnostics do not become an inference claim')
 if(name=='marked')ok(any(tab$variable=='component_covariance[2,1]'&tab$parameter_role=='derived_component_covariance')&&tab$sd[tab$variable=='component_covariance[2,1]']>0,'fixed nonzero rho still has uncertain scaled covariance')
 if(name=='both')ok(setequal(corr$correlation_layer,c('component','parameter'))&&length(unique(corr$parameter_key))==2L,'same display label never aliases two correlation layers')
 fit<-structure(list(spec=s,cache=cache,run=list(),backend=NULL,provenance=list(execution_profile=plan$execution_profile)),class=c('bmeas_fit','bhmm_fit'))
 copy<-unserialize(serialize(fit,NULL,version=2));before<-c(calls$summary,calls$draws);bytes<-serialize(copy,NULL,version=2);text<-capture.output(print(summary(copy)))
 ok(identical(posterior_summary(copy),tab)&&identical(before,c(calls$summary,calls$draws))&&identical(bytes,serialize(copy,NULL,version=2)),'detached summaries preserve bytes and never contact a backend')
 ok(any(grepl('unique free marginal-correlation labels',text,fixed=TRUE)),'readable summary separates labels from expanded cells')
}
cat('PASSED',passed,'affine cache controls; no compilation or sampling\n')
