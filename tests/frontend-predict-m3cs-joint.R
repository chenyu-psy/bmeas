source(file.path('helpers','native-guard.R'))
local({
test_scope<-environment()
root<-Sys.getenv('MEASUREMENTPKG_SOURCE')
source_mode<-nzchar(root)
if(source_mode) {
 # Source-mode callbacks require the stable global environment. Restore its
 # original bindings after the test rather than retaining source-only helpers.
 saved_globals<-as.list(.GlobalEnv,all.names=TRUE)
 on.exit({
  rm(list=setdiff(ls(.GlobalEnv,all.names=TRUE),names(saved_globals)),envir=.GlobalEnv)
  list2env(saved_globals,.GlobalEnv)
 },add=TRUE)
 for(f in list.files(file.path(root,'R'),full.names=TRUE))source(f)
} else {
 library(bmeas)
 # Only direct test entry points are bound locally. Function environments
 # remain the installed namespace, including all internal dispatch.
 ns<-asNamespace('bmeas')
 for(n in c('.bh_m3cs_contract','.bh_m3cs_joint_spec','.bh_m3cs_run','.bh_m3cs_simulator_check','.bh_m3cs_spec','.bh_m3cs_split','.bh_m3cs_statistics','.bh_m3cs_statistics_plan','.bh_pf_ppc','.bh_pf_ppc_plan','.bh_ppc_forward_plan','.bh_ppc_generate_eta','.bh_ppc_ingest_csv','.bh_ppc_with_rng','.mp_abort'))assign(n,get(n,ns),test_scope)
}
test_env<-if(source_mode).GlobalEnv else asNamespace('bmeas')
assign_test<-function(name,value,envir=test_env) {
 locked<-bindingIsLocked(name,test_env)
 if(locked)unlockBinding(name,test_env)
 on.exit(if(locked)lockBinding(name,test_env))
 base::assign(name,value,test_env)
 if(!source_mode)base::assign(name,value,test_scope)
 invisible(value)
}
resource<-Sys.getenv('MEASUREMENTPKG_M3_RESOURCE')
if(!nzchar(resource)||!file.exists(resource)){cat('SKIP external M3-CS resource absent\n')}else {
ok<-function(x,label){stopifnot(isTRUE(x));cat('PASS',label,'\n')}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);if(!inherits(e,'measurement_error')||!identical(e$code,code))stop(paste('expected',code,'got',conditionMessage(e)));cat('PASS refusal',code,'\n')}
near<-function(x,y)isTRUE(all.equal(x,y,check.attributes=FALSE,tolerance=1e-13))
tasks<-'same';people<-c('p3','p1','p2','unobserved')
make_data<-function(person,x,total) {
 d<-data.frame(person=person,task='same',obs=paste0('row',seq_along(person)))
 d$counts<-I(matrix(0L,length(person),10L));d$counts[,1]<-x;d$counts[,10]<-total-x
 d$options<-I(matrix(1L,length(person),10L));d
}
a<-make_data(c('p1','p1','p2','p3'),c(1,0,2,0),c(1,1,2,0))
b<-make_data(c('p2','p1','p3'),c(0,1,0),c(2,2,0))
m<-m3_cs_model(resource)
block<-function(d)observe(m,d,'person','task',fields=list(counts='counts',options='options'),
 observation_id='obs',units='count',parameters=list(c=list(fixed=.2,scale='predictor'),ra=list(fixed=.3),rc=list(fixed=.7)))
s<-bmeas(bmf(G=~left$a[tasks]+right$a[tasks]),list(left=block(a),right=block(b)),subjects='shared',fit=FALSE)
make_fit<-function(s)structure(list(spec=s,code='SYNTHETIC ENGINEERING DOUBLE ONLY',cache=list(),
 provenance=list(model_fingerprint=s$fingerprint,kind='synthetic_engineering_state_not_posterior',output_files='not-read.csv')),class=c('bmeas_fit','bhmm_fit'))
fit<-make_fit(s);budget<-diagnostic_budget(2,7,140,max_statistics=45,max_pair_terms=150)
plan<-ppc_plan(fit,2,budget,level='population');fp<-.bh_ppc_forward_plan(s,'population',unclass(budget))
ok(!.bh_m3cs_spec(s)&&.bh_m3cs_joint_spec(s),'historical single-block capability remains false for joint live specs')
ok(plan$simulated_values==140&&plan$statistic_counts$statistics==45&&plan$statistic_counts$pair_terms==150,'combined ten-cell and all cross-block pair budgets')
.bh_pf_ppc_plan(plan,'joint_plan')
sp<-.bh_m3cs_statistics_plan(s,2,unclass(budget),'population');observed<-.bh_m3cs_statistics(s,sp)
ok(nrow(sp$pairs)==25&&all(sp$pairs$overlap==2)&&length(unique(sp$groups$target))==10,'duplicate task names remain distinct block/task domains')
l<-which(sp$groups$block=='left'&sp$groups$category_index==1);r<-which(sp$groups$block=='right'&sp$groups$category_index==1)
p<-which(sp$pairs$left==l&sp$pairs$right==r)
ok(near(observed$estimate[which(observed$pair_index==p)],-.125),'pooled repeat rows and asymmetric row order use actual shared-person covariance')
ok(near(.bh_m3cs_statistics(s,sp,list(right=s$data_map$blocks$right$counts,left=s$data_map$blocks$left$counts))$estimate,observed$estimate),'response block names resolve independently of list order')
for(field in c('max_observations','max_simulated_values','max_statistics','max_pair_terms','max_latent_values','max_work_values')) {
 small<-unclass(budget);small[[field]]<-switch(field,max_observations=6L,max_simulated_values=139L,max_statistics=44L,max_pair_terms=149L,max_latent_values=1L,max_work_values=1L)
 class(small)<-'measurement_diagnostic_budget';err(ppc_plan(fit,2,small,level='population'),'E_DIAGNOSTIC_BUDGET')
}
draw<-setNames(rep(0,length(fp$required_variables)),fp$required_variables)
draw[grep('^effective_loadings',names(draw))]<-1;draw['L_component[1,1]']<-1
set.seed(314);eta<-.bh_ppc_generate_eta(fp,draw)
ok(near(eta[,1],eta[,2]),'same common-factor state is shared by both block targets')
dat<-m3cs_simulator_data(s,eta,budget=unclass(budget))
map<-attr(dat,'row_map')
ok(identical(map$block,c(rep('left',4),rep('right',3)))&&identical(map$task,rep('same',7))&&identical(map$person_key,unlist(lapply(s$data_map$blocks,function(d)d$.person_key),use.names=FALSE)), 'public joint data carries explicit block/task/person/observation row map')
ok(dat$O==7&&identical(as.vector(dat$totals),as.integer(c(rowSums(a$counts),rowSums(b$counts)))),'combined call preserves original row totals and block row offsets')
for(j in seq_along(fp$blocks)) {
 at<-if(j==1)1:4 else 5:7;op<-fp$blocks[[j]]$operands$a
 ok(near(dat$arguments[at,1],eta[cbind(op$person_index,op$eta_index)]),'block operands index one shared full-roster eta')
}
cp_spec<-bmeas(bmf(G=~left$a[tasks]+right$a[tasks],residual=list(targets=~left$a[tasks]+right$a[tasks]),person=list(distribution='sample_centered',ids=people),
 augmentation=list(type='task_cholesky',targets=~right$a[tasks]+left$a[tasks])),list(left=block(a),right=block(b)),subjects='shared',fit=FALSE)
cp<-.bh_ppc_forward_plan(cp_spec,'population');cd<-setNames(rep(0,length(cp$required_variables)),cp$required_variables)
cd[grep('^alpha',names(cd))]<-.2;cd[c('L_task_work[1,1]','L_task_work[2,2]')]<-1;cd['L_task_work[2,1]']<-.4
cd[grep('^task_row_scale',names(cd))]<-.7
set.seed(13);ce<-.bh_ppc_generate_eta(cp,cd)
ok(cp$geometry$N==4&&cp$latent_values_per_draw==6&&max(abs(colMeans(ce)-.2))<1e-14,'joint centered law keeps full original roster and joint task Cholesky')
Q<-matrix(0,4,3);for(k in 1:3){Q[1:k,k]<-1/sqrt(k*(k+1));Q[k+1,k]<--k/sqrt(k*(k+1))}
set.seed(13);W<-matrix(rnorm(6,sd=sqrt(4/3)),2,3);expected<-matrix(.2,4,2)
expected[,cp$active_target_index]<-expected[,cp$active_target_index]+t(.7*matrix(c(1,.4,0,1),2)%*%W%*%t(Q))
ok(near(ce,expected),'centered joint law equals independent positive-Helmert oracle')
conditional<-.bh_ppc_forward_plan(s,'conditional');qd<-setNames(seq_along(conditional$required_variables)/10,conditional$required_variables)
qe<-.bh_ppc_generate_eta(conditional,qd);qdat<-m3cs_simulator_data(s,qe)
for(j in seq_along(conditional$blocks)) {
 op<-conditional$blocks[[j]]$operands$a;at<-if(j==1)1:4 else 5:7
 ok(near(qdat$arguments[at,1],qd[paste0('eta[',op$person_index,',',op$eta_index,']')]),'conditional selected state remains aligned across both blocks')
}
# Excluded block retains an empty named domain; offsets cannot shift later rows.
empty_a<-a;empty_a$counts[,]<-NA_integer_
empty<-bmeas(bmf(G=~left$a[tasks]+right$a[tasks]),list(left=block(empty_a),right=block(b)),subjects='shared',missing='observed_only',fit=FALSE)
ep<-.bh_ppc_forward_plan(empty,'conditional');ed<-setNames(rep(.1,length(ep$required_variables)),ep$required_variables)
ee<-.bh_ppc_generate_eta(ep,ed);em<-m3cs_simulator_data(empty,ee)
split<-.bh_m3cs_split(empty,matrix(0,3,10))
ok(em$O==3&&identical(attr(em,'row_map')$block,rep('right',3))&&identical(dim(split$left),c(0L,10L))&&identical(dim(split$right),c(3L,10L)), 'empty consumed block preserves named domain and right-block row offsets')
sb<-diagnostic_budget(2,7,140,max_statistics=45,max_pair_terms=150,posterior=list(reader='streaming',max_source_bytes=100000,max_read_bytes=700000,max_materialized_values=50000))
stream_plan<-ppc_plan(fit,2,sb,level='conditional')
ok(stream_plan$simulated_values==140&&stream_plan$statistic_counts$statistics==45&&identical(stream_plan$source$selection$method,'balanced'), 'streaming plan budgets the entire joint design and balanced joint states')
# Reject saved execution before callbacks/resources; no resealing or live-class spoofing.
detached<-unclass(s);class(detached)<-'bmeas_portable_context'
err(.bh_ppc_forward_plan(detached,'population'),'E_PORTABLE_UNSUPPORTED')
err(.bh_m3cs_contract(detached),'E_PORTABLE_UNSUPPORTED')
changed<-s;changed$models$right$model$likelihood$symbol<-'arbitrary'
err(.bh_ppc_forward_plan(changed,'population'),'E_PPC_FAMILY')
third<-bmeas(bmf(G=~left$a[tasks]+right$a[tasks]+third$a[tasks]),list(left=block(a),right=block(b),third=block(b)),subjects='shared',fit=FALSE)
err(.bh_ppc_forward_plan(third,'population'),'E_PPC_FAMILY')
# Kernel is the identical pinned rng_v2 row kernel: no independent block reseeds.
single<-bmeas(bmf(G=~left$a[tasks]),list(left=block(a)),fit=FALSE)
ok(identical(make_m3cs_simulator_code(s),make_m3cs_simulator_code(single)),'joint response simulation uses byte-identical pinned single-row kernel')
# Exercise the actual R runner/CSV-output contract with a multinomial response
# double. This verifies separate row draws in one RNG transaction, not native
# CS probability accuracy (the emitted CS kernel is checked byte-for-byte above).
local({
 oldcheck<-.bh_m3cs_simulator_check
 on.exit(assign_test('.bh_m3cs_simulator_check',oldcheck,test_env))
 assign_test('.bh_m3cs_simulator_check',function(...)invisible(TRUE),test_env)
 calls<-0L;active<-c(1:4,10);prob<-c(.2,.15,.25,.1,.3)
 sampler<-function(data,chains,parallel_chains,iter_warmup,iter_sampling,fixed_param,seed,output_dir,...) {
  calls<<-calls+1L
  stopifnot(data$O==7,chains==1,parallel_chains==1,iter_warmup==0,iter_sampling==1,isTRUE(fixed_param))
  y<-matrix(0,data$O,10);p<-matrix(0,data$O,10)
  for(i in seq_len(data$O)){p[i,active]<-prob;y[i,active]<-rmultinom(1,data$totals[i],prob)[,1]}
  f<-file.path(output_dir,'double.csv')
  writeLines(c('# num_threads = 1','# algorithm = fixed_param','# num_warmup = 0','# num_samples = 1'),f)
  list(return_codes=function()0L,output_files=function()f,draws=function(variables,format){
   value<-if(variables=='probabilities')p else y;idx<-expand.grid(o=seq_len(data$O),k=1:10)
   matrix(as.vector(value),1,dimnames=list(NULL,paste0(variables,'[',idx$o,',',idx$k,']')))
  })
 }
 sim<-structure(list(model=list(sample=sampler),code='engineering double',code_hash='double',
  resource_hash='double',resource_file=resource,executable_hash='double',toolchain=list(),version='engineering_double'),class='bmeas_m3cs_simulator')
 set.seed(41);saved<-.Random.seed
 got<-.bh_m3cs_run(s,qe,sim,89L,budget=unclass(budget))
 expected<-.bh_ppc_with_rng(89L,function(){y<-matrix(0,7,10);for(i in 1:7)y[i,active]<-rmultinom(1,dat$totals[i],prob)[,1];y})
 ok(calls==1&&near(got$counts,expected)&&identical(saved,.Random.seed),'actual R runner uses one seed/call with separate multinomial row draws, not cloned block noise')
 parts<-m3cs_probabilities(s,qe,sim,budget=unclass(budget))
 ok(identical(names(parts),c('left','right'))&&identical(dim(parts$left),c(4L,10L))&&identical(dim(parts$right),c(3L,10L)), 'public joint probabilities retain block-scoped matrices')
 ok(identical(rownames(got$counts),unlist(lapply(s$data_map$blocks,function(d)d$.observation_id),use.names=FALSE)), 'runner retains stacked observation identity order')
})

local({
 oldcheck<-.bh_m3cs_simulator_check;oldrun<-.bh_m3cs_run;oldingest<-.bh_ppc_ingest_csv;oldeta<-.bh_ppc_generate_eta
 on.exit({assign_test('.bh_m3cs_simulator_check',oldcheck,test_env);assign_test('.bh_m3cs_run',oldrun,test_env);assign_test('.bh_ppc_ingest_csv',oldingest,test_env);assign_test('.bh_ppc_generate_eta',oldeta,test_env)})
 seen<-list();eta_calls<-0L
 assign_test('.bh_ppc_generate_eta',function(plan,draw){eta_calls<<-eta_calls+1L;oldeta(plan,draw)},test_env)
 assign_test('.bh_m3cs_simulator_check',function(...)invisible(TRUE),test_env)
 assign_test('.bh_m3cs_run',function(spec,eta,simulator,seed,draw_response=TRUE,budget=NULL){
  seen[[length(seen)+1L]]<<-list(eta=eta,data=m3cs_simulator_data(spec,eta,budget=budget),seed=seed)
  list(counts=do.call(rbind,lapply(spec$data_map$blocks,function(d)d$counts)))
 },test_env)
 assign_test('.bh_ppc_ingest_csv',function(paths,variables,budget,draws,expected_model){
  value<-setNames(rep(.1,length(variables)),variables)
  list(draws=matrix(rep(value,each=draws),draws,dimnames=list(NULL,variables)),draw_keys=data.frame(chain=rep(1L,draws),iteration=seq_len(draws)),
   receipt=list(actual_posterior_rows=draws,all_row_shape=c(draws,length(variables)),materialized_required_values=draws*length(variables)))
 },test_env)
 sim<-structure(list(version='engineering_double',code_hash='not_native',resource_hash='not_native',executable_hash='not_native',toolchain=list()),class='bmeas_m3cs_simulator')
 set.seed(37);before<-.Random.seed
 out<-assess_measurement_fit(fit,level='conditional',draws=2,seed=53,budget=budget,simulator=sim)
 if(out$cache$predictive_fit$status=='failed')print(out$cache$ppc$metadata$failure)
 ok(out$cache$predictive_fit$status=='computed'&&eta_calls==2&&length(seen)==2&&all(vapply(seen,function(x)x$data$O==7,logical(1))),'controller generates eta once and calls one combined response simulator per replication')
 ok(identical(before,.Random.seed)&&ncol(out$cache$ppc$replicate_statistics)==45&&out$cache$ppc$metadata$actual_work$simulated_values==140,'joint assessment restores RNG and retains full statistics/work')
 .bh_pf_ppc(out$cache$ppc,'joint_ppc');ok(is.list(fit_checks(out)),'passive current result schema/report remains valid without changing saved capability')
 first<-out$cache$ppc$replicate_statistics
 out2<-assess_measurement_fit(fit,level='conditional',draws=2,seed=53,budget=budget,simulator=sim)
 ok(identical(first,out2$cache$ppc$replicate_statistics)&&identical(before,.Random.seed),'deterministic replay preserves caller RNG')
 calls<-0L
 assign_test('.bh_m3cs_run',function(...){calls<<-calls+1L;if(calls==2L).mp_abort('E_PREDICTION_FAILED','engineering injected second replication failure');list(counts=do.call(rbind,lapply(s$data_map$blocks,function(d)d$counts)))},test_env)
 failed<-assess_measurement_fit(out,level='conditional',draws=2,seed=53,budget=budget,simulator=sim)
 ok(failed$cache$predictive_fit$status=='failed'&&failed$cache$ppc$metadata$completed_replications==1L&&is.null(failed$cache$ppc$table)&&is.null(failed$cache$ppc$replicate_statistics)&&identical(before,.Random.seed),'partial failure clears prior evidence atomically and restores RNG')
})
cat('PASS all joint M3-CS R-only engineering controls; no native execution or inferential qualification.\n')

}

})
