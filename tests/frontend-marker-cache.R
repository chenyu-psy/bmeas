source(file.path("helpers", "native-guard.R"))
# Coherent deterministic draw doubles exercise cache semantics, not inference.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
d<-data.frame(person=rep(1:4,2),x1=sin(1:8*.5),x2=cos(1:8*.7)+1:8/5,x3=sin(1:8*.3)+1:8/7)
forms<-list(
 single=bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1,value=1.2)),loadings=list(equal=list(pair=list(G=~x2+x3)))),
 mixed=bhmf(G=~x1+x2,H=~x2+x3,cor='independent',scale=list(G=list(marker=~x1))),
 both=bhmf(G=~x1+x2,H=~x2+x3,cor='free',scale=list(G=list(marker=~x1),H=list(marker=~x2)),loadings=list(equal=list(markers=list(G=~x1,H=~x2)))),
 fixed=bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1)),loadings=list(fixed=list(list(value=1,cells=list(G=~x1)),list(value=-.4,cells=list(G=~x2)),list(value=.7,cells=list(G=~x3))))),
 unit=bhmf(G=~x1+x2+x3),
 direct=bhmf(residual=list(targets=~x1+x2+x3,cor='free')),
 extreme=bhmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1,value=1e300))))
name_values<-function(field,x) {
 if(!length(x))return(numeric())
 labels<-if(is.matrix(x))paste0(field,'[',rep(seq_len(nrow(x)),ncol(x)),',',rep(seq_len(ncol(x)),each=nrow(x)),']')else paste0(field,'[',seq_along(x),']')
 stats::setNames(as.numeric(x),labels)
}
for(name in names(forms)) {
 s<-bhmm(forms[[name]],d,subject='person',fit=FALSE);J<-nrow(s$tables$targets);K<-nrow(s$tables$components);U<-nrow(s$tables$parameter_residuals);N<-length(s$data_map$subject_keys)
 layout<-private('.bh_covariance_layout')(s);ix<-layout$indices
 rows<-lapply(1:4,function(i) {
  values<-private('.bh_covariance_probe')(s,1)
  if(length(ix$loading))values[ix$loading]<-c(.2,.8,.3,.6)[i]*if(name=='extreme')1e300 else 1
  if(length(ix$component_sd))values[ix$component_sd]<-(c(.5,.7,.9,.4)[i]+.05*(seq_along(ix$component_sd)-1))*if(name=='extreme')1e-300 else 1
  st<-private('.bh_covariance_state')(s,values)
  L<-if(K)t(chol(st$R_component))else matrix(numeric(),0,0);Lu<-if(U)t(chol(st$R_parameter))else matrix(numeric(),0,0)
  raw<-matrix(.2*sin(seq_len(K*N)+i),K,N);if(K)raw[,1]<-0
  if(name=='extreme')raw[,2]<-1e-100
  eraw<-matrix(.2*cos(seq_len(U*N)+i),U,N);z<-L%*%raw
  f<-sweep(z,1,st$component_sd,function(a,b)a*b);available<-is.finite(f)&(z==0|f!=0);f[!available]<-NA_real_
  fields<-list(alpha=rep(.1*i,J),loading=values[ix$loading],component_sd=values[ix$component_sd],obs_sd=sqrt(values[ix$observation_variance]),parameter_sd=values[ix$parameter_sd],parameter_row_sd=st$parameter_sd,
   L_component=L,R_component=st$R_component,L_parameter=Lu,R_parameter=st$R_parameter,z_raw=raw,e_raw=eraw,z=z,f=f,effective_loadings=st$effective_loadings,component_covariance=st$component_covariance,
   log_component_row_sd=st$log_component_sd,component_covariance_log_abs=st$component_covariance_log_abs,f_log_abs=sweep(log(abs(z)),1,st$log_component_sd,function(a,b)a+b),
   Sigma_common_available=matrix(1,J,J),Sigma_parameter_available=matrix(1,J,J),Sigma_predictor_available=matrix(1,J,J),component_covariance_available=matrix(as.integer(!st$component_covariance_unavailable),K,K),f_available=matrix(as.integer(available),K,N))
  unlist(lapply(names(fields),function(n)name_values(n,fields[[n]])),use.names=TRUE)
 })
 draws<-do.call(rbind,rows);calls<-new.env();calls$summary<-calls$draws<-0L
 select<-function(variables)unique(unlist(lapply(variables,function(v)if(v%in%colnames(draws))v else colnames(draws)[startsWith(colnames(draws),paste0(v,'['))]),use.names=FALSE))
 backend<-list(draws=function(variables,format){calls$draws<-calls$draws+1L;draws[,select(variables),drop=FALSE]},diagnostic_summary=function(quiet)list(num_divergent=0,num_max_treedepth=0,ebfmi=.4),output=function()character())
 backend$summary<-function(variables,...) {
  calls$summary<-calls$summary+1L
  do.call(rbind,lapply(select(variables),function(v){x<-draws[,v];bad<-anyNA(x);data.frame(variable=v,mean=if(bad)NA_real_ else mean(x),median=if(bad)NA_real_ else stats::median(x),sd=if(bad)NA_real_ else stats::sd(x),q5=if(bad)NA_real_ else unname(stats::quantile(x,.05)),q95=if(bad)NA_real_ else unname(stats::quantile(x,.95)),rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.5)}))
 }
 checks<-check_model(s);plan<-list(posterior_propriety=checks$posterior_propriety,gaussian_diagnostics=checks$gaussian_diagnostics,identification=checks$identification,execution_profile=list(used='cache_double_only',support=checks$backend$support),backend_qualification=checks$backend)
 cache<-private('.bh_cache_scalar_fit')(backend,s,plan);tab<-cache$parameter_summary
 ok(sum(tab$parameter_role=='component_sd')==nrow(private('.bh_free_component_sd')(s)),'one cached posterior row per component-SD primitive')
 ok(sum(tab$parameter_role=='loading')==nrow(private('.bh_free_loadings')(s)),'declared beta aliases retain one posterior primitive')
 ok(identical(cache$component_scales,s$tables$component_scales)&&identical(cache$components,s$tables$components),'scientific and standardized variance metadata persist')
 ok(all(tab$rhat==2)&&all(tab$ess_bulk==5)&&cache$inference$status=='not_assessed','poor diagnostics are retained without an inference claim')
 if(private('.bh_has_marker')(s)) {
  m<-private('.bh_free_component_sd')(s)[1,];j<-match(m$marker_target,s$tables$targets$key);v<-paste0('effective_loadings[',j,',',m$component_index,']')
  ok(tab$mean[tab$variable==v]==mean(draws[,v])&&tab$parameter_role[tab$variable==v]=='derived_effective_loading','fixed declared marker has a separate genuinely derived loading summary')
  ok(sum(tab$parameter_role=='standardized_component_person_score')==K*N&&sum(tab$parameter_role=='scaled_component_person_score')==K*N,'standardized z and scaled f remain distinct estimands')
 }
 if(name=='single') {
  v<-'effective_loadings[2,1]';product_means<-mean(draws[,'loading[1]'])*mean(draws[,'component_sd[1]'])
  ok(abs(tab$mean[tab$variable==v]-product_means)>1e-3,'derived mean uses per-draw products, not the product of primitive means')
 }
 if(name=='extreme') {
  ok(cache$optional_reports$status=='unavailable_values'&&cache$computation$status=='warning','unavailable optional reports are explicit warnings rather than target failure')
  ok(nrow(cache$optional_reports$flags)==4L&&anyNA(tab$mean[tab$parameter_role=='scaled_component_person_score']),'unavailable score summaries do not silently drop draws')
 }else ok(cache$optional_reports$status=='available'&&cache$computation$status=='computed','legitimate negative-infinite log metadata does not invalidate available reports')
 if(K)ok(any(is.infinite(cache$optional_reports$log_magnitudes)&cache$optional_reports$log_magnitudes<0),'exact zero-score log magnitude is retained as negative infinity')
 run<-run_config(chains=1,parallel_chains=1,threads_per_chain=1,iter_warmup=10,iter_sampling=10,seed=731,cpu_budget=1,max_observations=24,max_subjects=4,max_saved_draws=10)
 fit<-structure(list(spec=s,cache=cache,run=run,provenance=list(execution_profile=plan$execution_profile),backend=NULL),class='bhmm_fit')
 copy<-unserialize(serialize(fit,NULL));before<-c(calls$summary,calls$draws);printed<-capture.output(print(summary(copy)))
 ok(identical(posterior_summary(copy),tab)&&identical(before,c(calls$summary,calls$draws)),'detached summary and serialized cache never access a backend')
 if(private('.bh_has_marker')(s))ok(any(grepl('declared beta',printed,fixed=TRUE))&&any(grepl('population component SDs',printed,fixed=TRUE)),'summary describes fixed beta and population scales explicitly')
}
cat('Passed',passed,'marker cache controls; no compilation or sampling\n')
