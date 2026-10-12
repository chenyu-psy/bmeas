source(file.path("helpers", "native-guard.R"))
# Coherent R draw doubles exercise reporting only, with no backend execution.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
source(file.path('helpers','residual-product-fixtures.R'))
specs<-residual_product_fixtures()
name_values<-function(field,x) {
 if(!length(x))return(numeric())
 labels<-if(is.matrix(x))paste0(field,'[',rep(seq_len(nrow(x)),ncol(x)),',',rep(seq_len(ncol(x)),each=nrow(x)),']')else paste0(field,'[',seq_along(x),']')
 stats::setNames(as.numeric(x),labels)
}
for(name in names(specs)) {
 s<-specs[[name]]
 J<-nrow(s$tables$targets);K<-nrow(s$tables$components);U<-nrow(s$tables$parameter_residuals);N<-length(s$data_map$subject_keys)
 ix<-private('.bh_covariance_layout')(s)$indices
 rows<-lapply(c(1L,2L,3L,1L),function(i) {
  values<-rep(.4,sum(private('.bh_covariance_layout')(s)$counts));values[ix$observation_variance]<-1
  values[ix$component_cor]<-private('.bh_affine_layer_state')(s,'component',rep(.1*i,length(ix$component_cor)))$scientific_values
  values[ix$parameter_cor]<-private('.bh_affine_layer_state')(s,'parameter',rep(-.15*i,length(ix$parameter_cor)))$scientific_values
  st<-private('.bh_covariance_state')(s,values)
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
 for(layer in c('component','parameter')) {
  at<-map$layer==layer;mode<-private('.bh_affine_layer_plan')(s,layer)$mode
  if(mode==0L&&any(at))ok(all(is.na(map$computational_variable[at]))&&all(map$computational_mapping[at]=='MGS_vector_map_no_scalar_association'),'MGS retains vector mapping without false scalar association')
  if(mode%in%c(2L,3L)&&any(at))ok(!anyNA(map$computational_variable[at])&&!anyDuplicated(map$computational_variable[at]),'each affine label retains its own scalar raw mapping')
 }
 if(name=='dual_product')ok(length(unique(corr$parameter_key))==4L&&setequal(corr$correlation_layer,c('component','parameter')),'same shared label text never aliases across common and residual layers')
 ok(identical(cache$correlation_constraints$primitives,pr)&&identical(cache$correlation_constraints$prior_geometry,s$tables$correlation_prior_geometry),'unique prior measure and scientific metadata persist')
 expanded<-cache$expanded_correlation_matrices;constants<-expanded$entries[expanded$entries$role=='fixed_correlation_constant',,drop=FALSE]
 ok(expanded$status=='available'&&nrow(expanded$entries)==K*K+U*U,'expanded matrix summaries retain all cells separately')
 ok(all(is.na(constants$rhat))&&all(is.na(constants$sd))&&all(constants$mean==constants$fixed_value),'fixed constants have no invented uncertainty diagnostics')
 ok(all(tab$rhat==2)&&all(tab$ess_bulk==5)&&cache$inference$status=='not_assessed','poor diagnostics do not become an inference claim')
 fit<-structure(list(spec=s,cache=cache,run=list(),backend=NULL,provenance=list(execution_profile=plan$execution_profile)),class=c('bmeas_fit','bhmm_fit'))
 copy<-unserialize(serialize(fit,NULL,version=2));before<-c(calls$summary,calls$draws);bytes<-serialize(copy,NULL,version=2);text<-capture.output(print(summary(copy)))
 ok(identical(posterior_summary(copy),tab)&&identical(before,c(calls$summary,calls$draws))&&identical(bytes,serialize(copy,NULL,version=2)),'detached summaries preserve bytes and never contact a backend')
 ok(any(grepl('unique free marginal-correlation labels',text,fixed=TRUE)),'readable summary separates labels from expanded cells')
}
cat('PASSED',passed,'residual/dual product live cache controls; no compilation or sampling\n')
