source(file.path("helpers", "native-guard.R"))
# Canonical cognitive qualification gates and public lifecycle doubles only.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch({force(x);NULL},error=identity);ok(inherits(e,'measurement_error')&&e$code==code,code)}
# Capability flags must gate even a familiar built-in distribution.
g<-gaussian_model();g$capabilities$available[g$capabilities$capability=='fit']<-FALSE
d<-demo_gaussian_cfa();b<-observe(g,d,'person','item','value_z',units='z')
f<-bhmf(A=~scores$mu[c('v1','v2','v3')],B=~scores$mu[c('s1','s2','s3')])
s<-bhmm(f,list(scores=b),fit=FALSE)
ok(check_model(s)$backend$status=='fit_capability_unavailable','disabled fit capability stays visible in prepared context')
err(stancode(s),'E_CAPABILITY');err(bhmm(s,chains=1,iter=20,warmup=10,resources='development'),'E_CAPABILITY')
m3<-Sys.getenv('MEASUREMENTPKG_M3_RESOURCE');sdm<-Sys.getenv('MEASUREMENTPKG_SDM_RESOURCE')
if(nzchar(m3)&&nzchar(sdm)&&file.exists(m3)&&file.exists(sdm)) {
  legacy<-demo_cognitive_wiring_spec(m3,sdm)
  f<-bhmf(residual=list(targets=~memory$a['CS']+memory$c['CS']+memory$ra['CS']+memory$rc['CS']+report$log_c['CR']+report$log_kappa['CR'],cor='free'))
  # Explicit engineering priors; no automatic universal cognitive calibration.
  missing<-bhmm(f,legacy$models,subjects='shared',fit=FALSE)
  ok(check_model(missing)$priors$status=='incomplete','custom cognitive priors remain explicit and inspectable')
  err(stancode(missing),'E_PRIOR_INCOMPLETE')
  p<-c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(.25)','parameter_sd'),set_prior('lkj(2.25)','cor',layer='parameter'))
  spec<-bhmm(f,legacy$models,subjects='shared',prior=p,fit=FALSE)
  checks<-check_model(spec)
  ok(checks$backend$status=='deterministic_cognitive_direct_qualified'&&checks$identification$status=='not_assessed','numerical cognitive qualification does not claim identification')
  data<-standata(spec)
  ok(data$K==0L&&data$U==6L&&data$S==0L&&data$O==24L,'no invented factors or Gaussian observation noise in actual cognitive binding')
  ok(all(model_table(spec,'targets')$receives=='eta')&&identical(model_table(spec,'targets')$predictor,c('log','log','logit','logit','identity','identity')),'original predictor links are applied only at the specified boundary')
  reversed<-bhmm(f,rev(legacy$models),subjects='shared',prior=p,fit=FALSE)
  ok(check_model(reversed)$backend$status=='deterministic_cognitive_direct_qualified','canonical scope does not depend on block ordering')
  renamed_blocks<-legacy$models;names(renamed_blocks)<-c('memory2','report2')
  renamed<-bhmm(bhmf(residual=list(targets=~report2$log_kappa['CR']+memory2$rc['CS']+memory2$ra['CS']+memory2$c['CS']+memory2$a['CS']+report2$log_c['CR'],cor='free')),renamed_blocks,subjects='shared',prior=p,fit=FALSE)
  ok(check_model(renamed)$backend$status=='deterministic_cognitive_direct_qualified','block labels and target order are not scientific restrictions')
  masked<-bhmm(bhmf(residual=list(targets=~memory$a['CS']+memory$c['CS']+memory$ra['CS']+memory$rc['CS']+report$log_c['CR']+report$log_kappa['CR'],cor=list(default='free',zero=list(~memory$a['CS']+memory$c['CS'])))),legacy$models,subjects='shared',prior=p,fit=FALSE)
  ok(check_model(masked)$backend$status=='experimental_cognitive_components'&&is.character(stancode(masked)),'canonical mask variation remains explicitly experimental without a demo-graph restriction')
  err(bhmm(masked,chains=1,iter=20,warmup=10,resources='development'),'E_EXECUTION_PROFILE')
  fake_blocks<-legacy$models;fake_blocks$report$model$likelihood$approximation<-'different declared algorithm'
  fake<-bhmm(f,fake_blocks,subjects='shared',prior=p,fit=FALSE)
  ok(check_model(fake)$backend$status=='experimental_typed_composition'&&check_model(fake)$backend$qualification$status=='exact_composition_unqualified','model name alone cannot grant canonical-reference qualification')
  local({
    old<-private('.bh_compile_scalar');replace<-function(value)if(source_mode)assign('.bh_compile_scalar',value,.GlobalEnv)else assignInNamespace('.bh_compile_scalar',value,'bmeas')
    on.exit(replace(old));events<-character();args<-NULL
    # The size receipt sees a real metadata file, never synthetic posterior CSV evidence.
    mock_output<-tempfile('backend-metadata-',fileext='.txt')
    writeLines('metadata-only backend double; not posterior data',mock_output)
    on.exit(unlink(mock_output),add=TRUE)
    backend<-list(return_codes=function()rep(0L,args$chains),output_files=function()mock_output,output=function()character(),
      diagnostic_summary=function(quiet)list(num_divergent=0,num_max_treedepth=0,ebfmi=1),
      summary=function(variables,...)data.frame(variable=c('alpha[1]',if('parameter_sd'%in%variables)'parameter_sd[1]'else'loading[1]'),mean=0,median=0,sd=1,q5=-1,q95=1,rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.4),
      draws=function(variables,format){n<-if(variables=='L_parameter')6L else 3L;matrix(as.vector(diag(n)),1,n*n,dimnames=list(NULL,paste0(variables,'[',rep(seq_len(n),n),',',rep(seq_len(n),each=n),']')))} )
    replace(function(spec,cache_dir,save_log_lik){events<<-c(events,'compile_double');list(
      model=list(sample=function(...){events<<-c(events,'sample_double');args<<-list(...);backend}),identity=list(code='explicit_backend_double',toolchain=list(stanc_options=list(O0=TRUE))),
      cache_key='mock',cache_hit=TRUE,executable_sha256='mock')})
    fit<-withCallingHandlers(bhmm(spec,chains=1,iter=20,warmup=10,output_dir=tempdir(),resources='development'),warning=function(w){
      if(inherits(w,'measurement_warning')&&w$code=='W_IDENTIFICATION_UNASSESSED')events<<-c(events,'identification_warning')
      invokeRestart('muffleWarning')})
    ok(identical(events,c('identification_warning','compile_double','sample_double')),'unassessed cognitive identification warning precedes backend work')
    ok(args$data$S==0L&&!any(c('obs_sd','z_raw','loading_anchor')%in%names(args$init())),'public cognitive dispatch retains the no-nuisance/no-common initialization contract')
    fit$backend<-NULL;out<-capture.output(print(summary(fit)))
    ok(fit$cache$identification$status=='not_assessed'&&fit$cache$inference$status=='not_assessed'&&any(grepl('0 common component groups, 0 scalar coordinates, 0 shared amplitudes; 6 parameter residuals;',out,fixed=TRUE)),'cognitive cached summary is honest and backend-free')
    neutral<-bhmf(Shared=~memory$a['CS']+memory$c['CS']+memory$ra['CS']+memory$rc['CS']+report$log_c['CR']+report$log_kappa['CR'],
      SubsetA=~memory$a['CS']+memory$c['CS']+memory$ra['CS'],SubsetB=~memory$a['CS']+report$log_c['CR']+report$log_kappa['CR'],cor='independent')
    pc<-c(set_prior('normal(0,1)','Intercept'),set_prior('normal(0,.25)','loading'))
    common<-bhmm(neutral,legacy$models,subjects='shared',prior=pc,fit=FALSE)
    ok(check_model(common)$backend$status=='experimental_cognitive_common_only','canonical common-only structures expose their experimental profile')
    before<-events;err(bhmm(common,chains=1,iter=20,warmup=10,output_dir=tempdir(),resources='development'),'E_EXECUTION_PROFILE')
    ok(identical(events,before),'default blocks experimental execution before any backend call')
    events<-character()
    experimental_fit<-withCallingHandlers(bhmm(common,chains=1,iter=20,warmup=10,output_dir=tempdir(),execution='experimental',resources='development'),warning=function(w){events<<-c(events,w$code);invokeRestart('muffleWarning')})
    ok(identical(events,c('W_EXPERIMENTAL_EXECUTION','W_IDENTIFICATION_UNASSESSED','compile_double','sample_double')),'explicit experimental request warns before backend work')
    ok(experimental_fit$provenance$execution_profile$used=='experimental_cognitive_common_only'&&experimental_fit$cache$identification$status=='not_assessed','actual profile and unknown cognitive identification persist in cache')
    experimental_fit$backend<-NULL
    ok(any(grepl('experimental_cognitive_common_only',capture.output(print(summary(experimental_fit))),fixed=TRUE)),'backend-free summary shows the experimental profile')
    one_common<-bhmm(bhmf(DifferentName=~memory$a['CS']+memory$c['CS']+memory$ra['CS']+memory$rc['CS']+report$log_c['CR']+report$log_kappa['CR']),legacy$models,subjects='shared',prior=pc,fit=FALSE)
    ok(check_model(one_common)$backend$status=='experimental_cognitive_common_only','no demo graph or component names hardcoded as scientific validity')
    all_targets<-~memory$a['CS']+memory$c['CS']+memory$ra['CS']+memory$rc['CS']+report$log_c['CR']+report$log_kappa['CR']
    nonidentified<-bhmm(bhmf(A=all_targets,B=all_targets,cor='independent'),legacy$models,subjects='shared',prior=pc,fit=FALSE)
    before<-events;err(bhmm(nonidentified,chains=1,iter=20,warmup=10,execution='experimental',exploratory=TRUE,resources='development'),'E_IDENTIFICATION')
    ok(identical(events,before),'neither profile nor exploratory flag bypasses proven nonidentification')
    Q<-matrix(c(1,-1),2,1,dimnames=list(c('memory::a::CS','memory::c::CS'),'difference'))
    with_basis<-bhmm(bhmf(Shared=all_targets,FixedPattern=component(~memory$a['CS']+memory$c['CS'],Q,loading='shared'),cor='independent'),
      legacy$models,subjects='shared',prior=c(pc,set_prior('half_normal(.25)','amplitude')),fit=FALSE)
    ok(check_model(with_basis)$backend$status=='experimental_cognitive_common_only'&&check_model(with_basis)$identification$status=='not_assessed','cognitive shared basis remains explicit experimental and identification-unassessed')
    before<-events;err(bhmm(with_basis,chains=1,iter=20,warmup=10,output_dir=tempdir(),resources='development'),'E_EXECUTION_PROFILE')
    ok(identical(events,before),'basis profile cannot silently start backend work')
    events<-character()
    basis_fit<-withCallingHandlers(bhmm(with_basis,chains=1,iter=20,warmup=10,output_dir=tempdir(),execution='experimental',resources='development'),warning=function(w){events<<-c(events,w$code);invokeRestart('muffleWarning')})
    ok(identical(events,c('W_EXPERIMENTAL_EXECUTION','W_IDENTIFICATION_UNASSESSED','compile_double','sample_double')),'basis experimental warning precedes lifecycle double')
    ok(args$data$B==1L&&args$data$L==6L&&args$data$K==2L&&'amplitude'%in%names(args$init()),'one amplitude reaches actual native parameter role without derived-cell duplication')
    ok(basis_fit$cache$identification$status=='not_assessed'&&basis_fit$cache$inference$status=='not_assessed','experimental basis metadata does not become inferred scientific validity')
    shares<-bhmm(bhmf(AnotherName=all_targets,decomposition=list(joint=list(type='variance_share',targets=all_targets,components=~AnotherName,residual=TRUE,shares=~1))),
      legacy$models,subjects='shared',prior=c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(.25)','total_sd'),set_prior('normal(0,.5)','share')),fit=FALSE)
    ok(check_model(shares)$backend$status=='experimental_cognitive_components'&&check_model(shares)$identification$status=='not_assessed','canonical variance-share composition is experimental with unknown cognitive identification')
    before<-events;err(bhmm(shares,chains=1,iter=20,warmup=10,output_dir=tempdir(),resources='development'),'E_EXECUTION_PROFILE')
    ok(identical(events,before),'default refuses share experimental execution before backend')
    events<-character()
    share_fit<-withCallingHandlers(bhmm(shares,chains=1,iter=20,warmup=10,output_dir=tempdir(),execution='experimental',resources='development'),warning=function(w){events<<-c(events,w$code);invokeRestart('muffleWarning')})
    ok(identical(events,c('W_EXPERIMENTAL_EXECUTION','W_IDENTIFICATION_UNASSESSED','compile_double','sample_double')),'explicit share profile warns before lifecycle double')
    ok(args$data$T==6L&&args$data$H==1L&&args$data$U==6L&&args$data$UF==0L&&args$data$S==0L,'cognitive share roles reach the actual sampler interface without extra observation/free-u noise')
    ok(all(c('total_sd','share_beta','z_raw','e_raw')%in%names(args$init()))&&!any(c('parameter_sd','loading_anchor','loading_free','obs_sd')%in%names(args$init())),'only real share primitive/latent coordinates are initialized')
    share_fit$backend<-NULL
    ok(any(grepl('6 target total SDs and 1 unique logit-contrast coefficients',capture.output(print(summary(share_fit))),fixed=TRUE)),'backend-free share summary exposes distinct primitive counts')
    ok(share_fit$cache$backend_qualification$evidence_version=='bmeas_affine_correlation_fixed_states_v1'&&share_fit$cache$inference$status=='not_assessed','fixed-state evidence version does not become inference validity')
    requested<-withCallingHandlers(bhmm(spec,chains=1,iter=20,warmup=10,output_dir=tempdir(),execution='experimental',resources='development'),warning=function(w)invokeRestart('muffleWarning'))
    ok(requested$provenance$execution_profile$requested=='experimental'&&requested$provenance$execution_profile$used=='qualified','experimental request alone does not relabel a qualified direct execution')

  })
}else cat('SKIP canonical cognitive tests: original external resources required\n')
cat('Frontend cognitive checks:',passed,'passed; no native compilation or posterior sampling.\n')
