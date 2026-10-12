source(file.path("helpers", "native-guard.R"))
# Public execution routing is tested only through terminal doubles, never a backend.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
local({
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(suppressWarnings(force(expr)),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
d<-data.frame(person=1:4);for(j in 1:4)d[[paste0('x',j)]]<-sin(seq_len(4)*j/7)+seq_len(4)/9
s<-bmeas(bmf(A=~x1+x2,B=~x3+x4,cor=list(default='zero',fixed=list(list(pairs=~A:B,value=.3)))),d,subject='person',fit=FALSE)
ok(check_model(s)$backend$status=='experimental_affine_correlations','fixed affine Gaussian needs explicit experimental execution')
err(bmeas(s,chains=1,iter=20,warmup=10,resources='development'),'E_EXECUTION_PROFILE')
calls<-new.env();calls$fitter<-0L
env<-if(source_mode).GlobalEnv else asNamespace('bmeas');old_fitter<-get('.bh_fit_scalar',env);locked<-bindingIsLocked('.bh_fit_scalar',env)
on.exit({if(bindingIsLocked('.bh_fit_scalar',env))unlockBinding('.bh_fit_scalar',env);assign('.bh_fit_scalar',old_fitter,env);if(locked)lockBinding('.bh_fit_scalar',env)},add=TRUE)
if(locked)unlockBinding('.bh_fit_scalar',env)
abort<-private('.mp_abort');assign('.bh_fit_scalar',function(...){calls$fitter<-calls$fitter+1L;abort('E_TERMINAL_TEST','active fitter trap')},env);if(locked)lockBinding('.bh_fit_scalar',env)
err(bmeas(s,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_TERMINAL_TEST')
ok(calls$fitter==1L,'valid affine public route reaches exactly one terminal mock')
dd<-data.frame(person=1:4,task='t',y=c(.1,.4,-.3,.9));bb<-observe(gaussian_model(),dd,'person','task','y',units='points',parameters=list(mu=list(fixed=.2),sigma=list(fixed=1)));fixed_spec<-bmeas(bmf(),list(score=bb),fit=FALSE)
err(bmeas(fixed_spec,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_NO_FREE_PARAMETERS')
d<-data.frame(person=rep(as.character(1:4),2));for(j in 1:4)d[[paste0('x',j)]]<-sin(seq_len(8)*j/7)+cos(seq_len(8)/3+j)
ref<-bmeas(bmf(A=~x1+x2,B=~x3+x4,
 cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(first=~A,second=~B))),
 residual=list(targets=~x1+x2+x3+x4,cor=list(default='zero',equal=list(shared=list(~x1:x2,~x1:x3,~x1:x4,~x2:x3,~x2:x4,~x3:x4)))),
 person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3+x4)),d,subject='person',fit=FALSE,
 prior=c(set_prior('normal(0,.2)','cor_raw',block='second'),set_prior('lkj(3.5)','cor',layer='parameter')))
ok(check_model(ref)$backend$status=='experimental_centered_task_raw_affine_residual','centered task raw common plus bounded residual has its own experimental profile')
err(bmeas(ref,chains=1,iter=20,warmup=10,resources='development'),'E_EXECUTION_PROFILE')
err(bmeas(ref,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_TERMINAL_TEST')
ok(isTRUE(private('.bh_affine_runtime_gate')(ref))&&calls$fitter==2L,'qualified scientific mode reaches one additional terminal double')
unsupported<-bmeas(bmf(A=~x1+x2,B=~x3+x4,
 cor=list(default='zero',fixed=list(list(pairs=~A:B,value=.3))),
 residual=list(targets=~x1+x2+x3+x4),
 person=list(distribution='sample_centered',ids=as.character(1:4)),augmentation=list(type='task_cholesky',targets=~x1+x2+x3+x4)),d,subject='person',fit=FALSE)
ok(check_model(unsupported)$backend$status=='affine_combination_not_runtime_qualified','centered final-marginal common mode remains source-only')
for(execution in c('qualified','experimental'))for(exploratory in c(FALSE,TRUE))err(bmeas(unsupported,chains=1,iter=20,warmup=10,execution=execution,exploratory=exploratory,resources='development'),'E_FRONTEND_BACKEND_UNSUPPORTED')
err(private('.bh_affine_runtime_gate')(unsupported),'E_FRONTEND_BACKEND_UNSUPPORTED')
err(private('.bh_cache_scalar_fit')(list(),unsupported,list()),'E_REFERENCE_CACHE_UNAVAILABLE')
ok(calls$fitter==2L,'all other reference-affine routes stop before terminal fitter')
raw_iid<-bmeas(bmf(A=~x1+x2,B=~x3+x4,
 loadings=list(fixed=list(list(value=1,cells=list(A=~x1+x2,B=~x3+x4)))),
 cor=list(default='free',coordinates=list(type='ordered_blocks',blocks=list(first=~A,second=~B))),
 residual=list(targets=~x1+x2+x3+x4,cor=list(default='zero',equal=list(shared=list(~x1:x2,~x3:x4))))),
 d,subject='person',fit=FALSE,prior=c(set_prior('normal(0,.2)','cor_raw',block='second'),set_prior('lkj(2)','cor',layer='parameter')))
ok(check_model(raw_iid)$backend$status=='affine_combination_not_runtime_qualified'&&check_model(raw_iid)$backend$evidence_version=='reference_affine_combination_unqualified','raw iid mode inherits no centered runtime evidence')
for(execution in c('qualified','experimental'))for(exploratory in c(FALSE,TRUE))err(bmeas(raw_iid,chains=1,iter=20,warmup=10,execution=execution,exploratory=exploratory,resources='development'),'E_FRONTEND_BACKEND_UNSUPPORTED')
err(private('.bh_affine_runtime_gate')(raw_iid),'E_FRONTEND_BACKEND_UNSUPPORTED')
ok(calls$fitter==2L,'raw iid mode also stops before terminal fitter')

d<-data.frame(person=1:2);for(j in 1:12)d[[paste0('x',j)]]<-c(j/10,j/10+.5+j/100)
f<-bmf(A=~x1+x2+x3,B=~x4+x5+x6,C=~x7+x8+x9,D=~x10+x11+x12,cor=list(default='zero',equal=list(shared=list(~A:B,~C:D))))
weak<-bmeas(f,d,subject='person',prior=set_prior('lkj(.6)','cor'),fit=FALSE)
ok(check_model(weak)$correlation_prior_geometry$component$status=='proven_proper_prior','eta .6 matching-pair prior is mathematically proper')
err(bmeas(weak,chains=1,iter=20,warmup=10,execution='experimental',resources='development'),'E_NUMERICAL_PRIOR_UNQUALIFIED')
err(private('.bh_affine_runtime_gate')(weak),'E_NUMERICAL_PRIOR_UNQUALIFIED')
ok(is.character(stancode(weak,log_lik=TRUE)),'prior normalizability and source capability are separate from runtime restriction')
local({
 # Cache flag normalization is R-only; native toolchain discovery is separate.
 old_fingerprint<-get('.mp_toolchain_fingerprint',env)
 fingerprint_locked<-bindingIsLocked('.mp_toolchain_fingerprint',env)
 on.exit({
  if(bindingIsLocked('.mp_toolchain_fingerprint',env))unlockBinding('.mp_toolchain_fingerprint',env)
  assign('.mp_toolchain_fingerprint',old_fingerprint,env)
  if(fingerprint_locked)lockBinding('.mp_toolchain_fingerprint',env)
 },add=TRUE)
 fingerprint_calls<-0L
 if(fingerprint_locked)unlockBinding('.mp_toolchain_fingerprint',env)
 assign('.mp_toolchain_fingerprint',function(cpp_options,stanc_options) {
  fingerprint_calls<<-fingerprint_calls+1L
  list(environment=Sys.getenv(c('CC','CXX','CPPFLAGS','CXXFLAGS','LDFLAGS')),
       cpp_options=cpp_options,stanc_options=stanc_options,fake='R-only compiler identity fixture')
 },env)
 if(fingerprint_locked)lockBinding('.mp_toolchain_fingerprint',env)
 old<-Sys.getenv(c('CPPFLAGS','CXXFLAGS'),unset=NA_character_)
 on.exit(for(n in names(old))if(is.na(old[[n]]))Sys.unsetenv(n)else do.call(Sys.setenv,stats::setNames(list(old[[n]]),n)),add=TRUE)
 Sys.setenv(CPPFLAGS='-DNOTE="keep -O2 inside" -O3 -O2',CXXFLAGS='-Og -O2 -g')
 a<-private('.bh_compile_identity')(s,TRUE);clean<-private('.bh_reference_compiler_environment')();do.call(Sys.setenv,as.list(clean));b<-private('.bh_compile_identity')(s,TRUE)
 ok(identical(a,b)&&a$version=='bmeas-affine-correlation-labels-v1','affine cache identity uses the scoped cleaned compiler context')
 ok(grepl('keep -O2 inside',a$toolchain$environment[['CPPFLAGS']],fixed=TRUE)&&!grepl('-O3',a$toolchain$environment[['CPPFLAGS']],fixed=TRUE),'quoted macro is retained while actual optimization options are removed')
 ok(fingerprint_calls==2L&&identical(a$toolchain$cpp_options,list(stan_threads=TRUE,O=0))&&identical(a$toolchain$stanc_options,list(O0=TRUE)),'both identities use explicit R-only fingerprints with original compiler options')
})
cat('PASSED',passed,'affine runtime controls; terminal traps only\n')

})
