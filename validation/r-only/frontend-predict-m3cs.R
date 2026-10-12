source(file.path("helpers","native-guard.R"))
root<-Sys.getenv("MEASUREMENTPKG_SOURCE");source_mode<-nzchar(root)
if(source_mode)for(f in list.files(file.path(root,"R"),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,"bmeas")
for(n in c('.bh_ppc_forward_plan','.bh_ppc_generate_eta','.bh_ppc_forward_names','.bh_ppc_with_rng','.bh_m3cs_kind','.bh_pf_ppc_plan'))assign(n,private(n))
resource<-Sys.getenv('MEASUREMENTPKG_M3_RESOURCE')
if(!nzchar(resource)||!file.exists(resource)){cat('SKIP external M3-CS resource absent\n')}else {
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch(force(x),error=identity);ok(inherits(e,'measurement_error')&&identical(e$code,code),code)}
m<-m3_cs_model(resource);tasks<-c('t3','t1','t2');people<-c('z','a','b','unobserved')
d<-expand.grid(person=people[1:3],task=tasks,stringsAsFactors=FALSE)
d$counts<-I(matrix(0L,nrow(d),10L));d$counts[,1]<-2L;d$options<-I(matrix(1L,nrow(d),10L));d$options[,10]<-3L
block<-function(parameters)observe(m,d,'person','task',fields=list(counts='counts',options='options'),units='count',parameters=parameters)
b<-block(list(c='task',ra=list(fixed=.3),rc=list(fixed=.7)))
s<-bmeas(bmf(residual=list(targets=~cs$a[tasks])),list(cs=b),fit=FALSE)
make_fit<-function(spec)structure(list(spec=spec,code='SYNTHETIC ENGINEERING ONLY',cache=list(),
 provenance=list(model_fingerprint=spec$fingerprint,kind='synthetic_engineering_state_not_posterior',output_files='missing.csv')),class=c('bmeas_fit','bhmm_fit'))
fit<-make_fit(s);budget<-diagnostic_budget(2,100,2000)
p<-.bh_ppc_forward_plan(s,'conditional');draw<-setNames(seq_along(p$required_variables)/100,p$required_variables)
eta<-.bh_ppc_generate_eta(p,draw);dat<-m3cs_simulator_data(s,eta)
bo<-p$blocks$cs$operands
for(a in c('a','c'))ok(identical(as.numeric(dat$arguments[,match(a,c('a','c','ra','rc'))]),as.numeric(draw[paste0('eta[',bo[[a]]$person_index,',',bo[[a]]$eta_index,']')])),paste(a,'aligned conditional operands'))
ok(all(dat$arguments[,3]==qlogis(.3))&&all(dat$arguments[,4]==qlogis(.7)),'natural fixed values transformed once to received predictors')
ok(identical(dat$options,unname(d$options))&&all(dat$totals==2),'options and totals unchanged')
plan<-ppc_plan(fit,2,budget,level='conditional');ok(nrow(plan$statistics)==105&&plan$simulated_values==180,'105 statistics and ten-cell budget')
.bh_pf_ppc_plan(plan,'test');ok(TRUE,'portable passive plan schema')
err(ppc_plan(fit,2,diagnostic_budget(2,100,100),level='population'),'E_DIAGNOSTIC_BUDGET')
err(assess_measurement_fit(fit,level='population',draws=2,seed=1,budget=budget),'E_SIMULATOR_REQUIRED')
fixed<-bmeas(bmf(),list(cs=block(list(a=list(fixed=.2,scale='predictor'),c=list(fixed=-.5,scale='predictor'),ra=list(fixed=.3),rc=list(fixed=.7)))),fit=FALSE)
fp<-.bh_ppc_forward_plan(fixed,'population');ok(length(fp$required_variables)==0&&fp$latent_values_per_draw==0,'all-fixed no posterior variables or latent generation')
fe<-.bh_ppc_generate_eta(fp,numeric());fd<-m3cs_simulator_data(fixed,fe);ok(all(fd$arguments[,1]==.2)&&all(fd$arguments[,2]==-.5),'all-fixed operands despite empty eta')
centered<-bmeas(bmf(residual=list(targets=~cs$a[tasks]),person=list(distribution='sample_centered',ids=people),augmentation=list(type='task_cholesky',targets=~cs$a[c('t2','t1','t3')])),list(cs=b),fit=FALSE)
cp<-.bh_ppc_forward_plan(centered,'population');ok(cp$geometry$N==4&&cp$latent_values_per_draw==9,'full centered roster includes unobserved person')
cd<-setNames(rep(0,length(cp$required_variables)),cp$required_variables);J<-cp$geometry$J;JT<-cp$geometry$JT
cd[paste0('alpha[',1:J,']')]<-seq_len(J)/10
cd[paste0('L_task_work[',1:JT,',',1:JT,']')]<-1
cd[paste0('task_row_scale[',cp$active_target_index,']')]<-.5
set.seed(773);before<-.Random.seed;ce<-.bh_ppc_with_rng(4,function().bh_ppc_generate_eta(cp,cd))
ok(identical(before,.Random.seed),'population replay preserves caller RNG')
ok(max(abs(colMeans(ce)-cd[paste0('alpha[',1:J,']')]))<1e-14,'centered deviations average zero across full roster')
ip<-.bh_ppc_forward_plan(s,'population');id<-setNames(rep(0,length(ip$required_variables)),ip$required_variables)
id[paste0('alpha[',1:ip$geometry$J,']')]<-.2
for(k in 1:ip$geometry$U){id[paste0('D_parameter[',ip$residual_target_index[k],',',k,']')]<-.3;id[paste0('L_parameter[',k,',',k,']')]<-1}
set.seed(41);ie<-.bh_ppc_generate_eta(ip,id);set.seed(41);noise<-matrix(rnorm(ip$geometry$U*ip$geometry$N),ip$geometry$U,ip$geometry$N)
expected<-matrix(.2,ip$geometry$N,ip$geometry$J);expected[,ip$residual_target_index]<-expected[,ip$residual_target_index]+.3*t(noise)
ok(max(abs(ie-expected))<1e-14,'iid generator exact independent residual oracle')
code<-make_m3cs_simulator_code(s);ok(grepl('cs_logits(arguments[o,1],arguments[o,2],arguments[o,3],arguments[o,4]',code,fixed=TRUE)&&!grepl('mu_update_logits(',code,fixed=TRUE),'generated wrapper calls original CS only')
# Planning must survive a missing external file: no file or callback access.
hidden<-s;hidden$models$cs$model$adapter$resources[[1L]]$file<-'nonexistent-external-resource';hidden_fit<-make_fit(hidden)
ok(nrow(ppc_plan(hidden_fit,2,budget,level='population')$statistics)==105,'planning does not read external resource')
mutated<-s;mutated$models$cs$model$likelihood$symbol<-'impostor'
err(ppc_plan(make_fit(mutated),2,budget,level='population'),'E_PPC_FAMILY')
cat('M3-CS R-only engineering controls:',passed,'passed; native simulator never invoked.\n')
}

# Test-only replacement of package internals; always restored on exit.
.bmeas_test_set <- function(name, value) {
  env <- if(source_mode) .GlobalEnv else asNamespace("bmeas")
  locked <- bindingIsLocked(name, env)
  if(locked) unlockBinding(name, env)
  assign(name, value, env)
  if(locked) lockBinding(name, env)
  invisible(NULL)
}
