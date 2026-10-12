source(file.path("helpers", "native-guard.R"))
# Cache summaries distinguish unique primitives, aliases and fixed cells.
# Direct cache doubles only; no compiler or sampler is invoked.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
d<-data.frame(person=1:4,x1=c(.2,.8,.3,1),x2=c(.7,.6,-.3,.1),x3=c(-.2,.6,.9,.4))
forms<-list(alias=bhmf(G=~x1+x2+x3,loadings=list(equal=list(pair=list(G=~x1+x2)))),
 fixed=bhmf(G=~x1+x2+x3,loadings=list(fixed=list(list(value=.5,cells=list(G=~x1+x2+x3))))),
 zero=bhmf(G=~x1+x2+x3,loadings=list(fixed=list(list(value=0,cells=list(G=~x1+x2+x3))))))
for(name in names(forms)) {
 spec<-bhmm(forms[[name]],d,subject='person',fit=FALSE)
 tab<-model_table(spec,'loading_primitives');cells<-model_table(spec,'loading_cells')
 calls<-new.env();calls$summary<-calls$draws<-0L
 variables<-c(paste0('alpha[',1:3,']'),if(nrow(tab))paste0('loading[',seq_len(nrow(tab)),']'),paste0('obs_sd[',1:3,']'))
 backend<-list(
  diagnostic_summary=function(quiet)list(num_divergent=1,num_max_treedepth=2,ebfmi=.2),output=function()character(),
  draws=function(variables,format){calls$draws<-calls$draws+1L;matrix(1,1,1,dimnames=list(NULL,'L_component[1,1]'))})
 # Capture the exact synthetic variable names without shadowing the formal argument.
 names_for_summary<-variables
 backend$summary<-function(variables,...) {calls$summary<-calls$summary+1L;data.frame(variable=names_for_summary,mean=0,median=0,sd=1,q5=-1,q95=1,rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.5)}
 checks<-check_model(spec)
 plan<-list(posterior_propriety=checks$posterior_propriety,gaussian_diagnostics=checks$gaussian_diagnostics,identification=checks$identification,
  execution_profile=list(used='cache_double_only',support=checks$backend$support),backend_qualification=checks$backend)
 cache<-private('.bh_cache_scalar_fit')(backend,spec,plan)
 ok(identical(cache$loading_cells,cells)&&identical(cache$loading_primitives,tab),'cache preserves the affine map and unique primitive identities')
 ok(sum(cache$parameter_summary$parameter_role=='loading')==nrow(tab),'posterior summary has one row per free loading primitive')
 ok(identical(cache$parameter_summary$loading_aliases[cache$parameter_summary$parameter_role=='loading'],tab$members),'uncertain loading rows expose their complete alias member list')
 ok(!any(cache$parameter_summary$parameter_key%in%cells$cell_id),'aliases and fixed cells are not fabricated posterior unknowns')
 run<-run_config(chains=1,parallel_chains=1,threads_per_chain=1,iter_warmup=10,iter_sampling=10,seed=731,cpu_budget=1,max_observations=12,max_subjects=4,max_saved_draws=10)
 fit<-structure(list(spec=spec,cache=cache,run=run,provenance=list(execution_profile=plan$execution_profile),backend=NULL),class='bhmm_fit')
 copy<-unserialize(serialize(fit,NULL));before<-c(calls$summary,calls$draws)
 printed<-capture.output(print(summary(copy)))
 ok(any(grepl('unique coefficients',printed,fixed=TRUE))&&identical(before,c(calls$summary,calls$draws)),'detached summary explains unique coefficients without backend access')
 ok(identical(posterior_summary(copy),cache$parameter_summary)&&all(posterior_summary(copy)$rhat==2),'poor cached diagnostics and unique parameter rows survive reload')
}
cat(passed,'affine loading cache checks passed; no fitting or native calls\n')
