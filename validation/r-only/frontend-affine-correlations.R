source(file.path("helpers", "native-guard.R"))
# Fixed/equal final marginal correlations: public R/IR controls, no native calls.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
if(!source_mode)for(n in c('.bh_covariance_layout','.bh_covariance_probe','.bh_covariance_state'))assign(n,getFromNamespace(n,'bmeas'))
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
scientific_data<-function(x){attr(x,'representation_projection')<-NULL;x}
err<-function(x,code){e<-tryCatch(force(x),error=identity);if(!inherits(e,'measurement_error')||!identical(e$code,code)){print(e);stop(paste('expected',code))};ok(TRUE,code)}
d<-data.frame(person=rep(paste0('p',1:4),2))
for(j in 1:12)d[[paste0('x',j)]]<-sin(seq_len(8)*j/7)+j/10+seq_len(8)*j/19
prep<-function(f,data=d,prior=NULL)bhmm(f,data,subject='person',prior=prior,missing='observed_only',fit=FALSE)
fixed<-function(pairs,value)list(pairs=pairs,value=value)
c2<-function(r)list(default='zero',fixed=list(fixed(~A:B,r)))
f2<-function(r)bhmf(A=~x1+x2,B=~x3+x4,cor=c2(r))
for(r in c(.3,-.3)){
 s<-prep(f2(r),d[1:4,]);i<-check_model(s)$identification;p<-get_prior(s)
 ok(i$free_covariance_parameters==8L&&i$observable_covariance_moments==10L&&all(i$probe_ranks==8L),'fixed nonzero cross moments P8/M10/rank8')
 ok(sum(model_table(s,'loading_primitives')$sign=='positive_anchor')==1L,'S14 one simultaneous sign anchor')
 ok(!any(p$class=='cor')&&nrow(model_table(s,'correlation_primitives'))==0L,'fully fixed common layer has no prior or correlation primitive')
 v<-.bh_covariance_probe(s,1);state<-.bh_covariance_state(s,v)
 ok(state$R_component[1,2]==r&&max(abs(state$Sigma_common-state$Lambda%*%matrix(c(1,r,r,1),2)%*%t(state$Lambda)))<1e-14,'fixed signed R enters actual covariance algebra')
 ok(grepl('bmeas_corr_layer',as.character(stancode(s)),fixed=TRUE)&&standata(s)$E==0L,'fixed correlation source and zero-dimensional data are inspectable')
 err(bhmm(s,chains=1,iter=20,warmup=10,execution='qualified',exploratory=TRUE,resources='development'),'E_EXECUTION_PROFILE')
 err(prep(f2(r),prior=set_prior('lkj(2)','cor')),'E_PRIOR_SELECTOR')
}
s15<-function(eta=2)prep(bhmf(A=~x1,B=~x2,C=~x3,D=~x4,
 cor=list(default='zero',equal=list(shared=list(~A:B,~C:D)))),prior=set_prior(paste0('lkj(',eta,')'),'cor'))
s<-s15();s_checks<-check_model(s);p<-get_prior(s);tab<-model_table(s,'correlations');dom<-model_table(s,'correlation_domains')$component
ok(nrow(model_table(s,'correlation_primitives'))==1L&&sum(tab$state=='free')==2L&&sum(p$class=='cor')==1L&&p$parameter_count[p$class=='cor']==1L,'S15 one unique label and one prior count')
ok(sum(model_table(s,'loading_primitives')$sign=='positive_anchor')==3L&&ncol(model_table(s,'orientation_sign_system')$allowed_binary_basis)==3L,'S15 exact three-dimensional sign gauge')
ok(all(is.na(model_table(s,'orientation')$sign_group))&&!anyNA(model_table(s,'components')$orientation),'parity system reports actual basis without fake pairwise groups')
ok(max(abs(c(dom$lower,dom$upper)-c(-1,1)))<1e-12&&dom$dimension==1L,'shared disconnected blocks use one full interval')
v<-.bh_covariance_probe(s,1);v[.bh_covariance_layout(s)$indices$component_cor]<--.35;state<-.bh_covariance_state(s,v)
ok(state$R_component[1,2]==-.35&&state$R_component[3,4]==-.35&&all(state$R_component[cbind(c(1,1,2,2),c(3,4,3,4))]==0),'negative shared label scatters once into both blocks')
for(eta in c(.4,.5,.6,1,2)){
 z<-if(eta==2)s else s15(eta);checks<-if(eta==2)s_checks else check_model(z);g<-checks$correlation_prior_geometry$component
 ok(g$status==if(eta>.5)'proven_proper_prior'else'proven_improper_prior','shared matching determinant prior exact integrability threshold')
 ok(!identical(checks$posterior_propriety$status,'proven_proper'),'prior status never promotes arbitrary posterior propriety')
}
# Constraint order/aliases/fixed propagation and no-op compatibility.
rules<-list(first=list(~A:B,~B:C),second=list(~B:C,~A:C))
for(eq in list(rules,rev(rules))){
 f<-bhmf(A=~x1,B=~x2,C=~x3,cor=list(default='zero',equal=eq,fixed=list(fixed(~A:B,.2))))
 z<-prep(f);t<-model_table(z,'correlations')
 ok(all(t$state=='fixed')&&all(t$fixed_value==.2)&&nrow(model_table(z,'correlation_primitives'))==0L,'transitive fixed propagation removes all correlation primitives')
 ok(all(vapply(t$equality_labels,function(x)setequal(x,c('first','second')),logical(1))),'transitive class retains both label aliases')
}
err(bhmf(A=~x1,B=~x2,C=~x3,cor=list(default='zero',free=list(~A:B),equal=list(e=list(~A:B,~A:C)),fixed=list(fixed(~A:C,.2)))),'E_CORRELATION_CONFLICT')
err(bhmf(A=~x1,B=~x2,cor=list(default='zero',equal=list(e=list(~A:B,~B:A)))),'E_CORRELATION_PAIR')
err(bhmf(A=~x1,B=~x2,cor=list(default='zero',equal=list(e=list()))),'E_CORRELATION_SYNTAX')
base<-prep(bhmf(A=~x1+x2+x3,B=~x4+x5+x6,cor='independent'))
noop<-prep(bhmf(A=~x1+x2+x3,B=~x4+x5+x6,cor=list(default='zero',fixed=list(fixed(~A:B,0)))))
ok(identical(as.character(stancode(base)),as.character(stancode(noop)))&&identical(scientific_data(standata(base)),scientific_data(standata(noop)))&&identical(get_prior(base),get_prior(noop)),'explicit fixed-zero no-op preserves old lowering/data/priors')
empty<-prep(bhmf(A=~x1+x2+x3,B=~x4+x5+x6,cor=list(default='zero',fixed=list(),equal=list())))
ok(identical(stancode(base),stancode(empty))&&identical(standata(base),standata(empty)),'empty extension lists preserve default backend')
# Strict matrix-domain feasibility is separate from numerical uncertainty.
for(r in c(-1,1)){
 z<-prep(f2(r));c<-check_model(z)
 ok(c$correlation_feasibility$status=='proven_infeasible_strict_PD'&&c$identification$status=='not_assessed','boundary PSD does not acquire a strict-domain ID verdict')
 err(stancode(z),'E_CORRELATION_DOMAIN')
}
z<-prep(bhmf(A=~x1,B=~x2,C=~x3,cor=list(default='zero',fixed=list(fixed(~A:B,.9),fixed(~A:C,.9),fixed(~B:C,-.9)))))
ok(check_model(z)$correlation_feasibility$status=='proven_infeasible_strict_PD','fully fixed indefinite matrix certificate')
interval<-prep(bhmf(A=~x1+x2,B=~x3+x4,C=~x5+x6,cor=list(default='free',fixed=list(fixed(~A:C,.9),fixed(~B:C,.9)))))
domain<-model_table(interval,'correlation_domains')$component
ok(domain$status=='feasible'&&domain$interval_status=='available'&&max(abs(c(domain$lower,domain$upper)-c(.62,1)))<1e-10,'full non-prefix domain is (.62,1)')
for(i in 1:3){v<-.bh_covariance_probe(interval,i);ok(v[.bh_covariance_layout(interval)$indices$component_cor]>.62,'rank probes use actual nonzero interval interior')}
# Stable residual covariance can be identified without repeats when rho is fixed.
rc<-function(r)list(default='zero',fixed=list(fixed(~x1:x2,r),fixed(~x1:x3,r),fixed(~x2:x3,r)))
for(r in c(.3,-.2)){
 z<-prep(bhmf(residual=list(targets=~x1+x2+x3,cor=rc(r))),d[1:4,]);i<-check_model(z)$identification
 ok(i$status=='local_rank_full_not_global_proof'&&i$free_covariance_parameters==6L&&all(i$probe_ranks==6L),'known residual triangle/no repeat remains rank6 positive')
 v<-.bh_covariance_probe(z,1);state<-.bh_covariance_state(z,v);S<-state$Sigma_parameter
 recovered<-sqrt(c(S[1,2]*S[1,3]/(r*S[2,3]),S[1,2]*S[2,3]/(r*S[1,3]),S[1,3]*S[2,3]/(r*S[1,2])))
 ok(max(abs(recovered-state$parameter_sd))<1e-12,'known-rho triangle explicit inverse recovers SDs')
}
eqr<-list(default='zero',equal=list(shared=list(~x1:x2,~x1:x3,~x2:x3)))
z<-prep(bhmf(residual=list(targets=~x1+x2+x3,cor=eqr)),d[1:4,]);i<-check_model(z)$identification
ok(i$status=='proven_unidentified'&&i$free_covariance_parameters==7L&&i$observable_covariance_moments==6L,'shared residual rho without repeats P7/M6 negative')
one<-d;one[5:8,c('x2','x3')]<-NA_real_
z<-prep(bhmf(residual=list(targets=~x1+x2+x3,cor=eqr)),one);i<-check_model(z)$identification
ok(i$status=='local_rank_full_not_global_proof'&&all(i$probe_ranks==7L),'one repeated target restores shared-rho generic rank7')
ok(sum(get_prior(z)$class=='cor')==1L&&get_prior(z)$parameter_count[get_prior(z)$class=='cor']==1L,'residual equality has one density coordinate')
# Same textual equality labels in different layers stay independent.
z<-prep(bhmf(A=~x1,B=~x2,C=~x3,
 cor=list(default='zero',equal=list(shared=list(~A:B,~A:C))),
 residual=list(targets=~x1+x2+x3,cor=eqr)))
pr<-model_table(z,'correlation_primitives')
ok(nrow(pr)==2L&&length(unique(pr$parameter))==2L&&setequal(pr$layer,c('component','parameter')),'common and residual label scopes stay distinct')
ok(sum(get_prior(z)$class=='cor')==2L&&all(get_prior(z)$parameter_count[get_prior(z)$class=='cor']==1L),'separate joint prior per layer')
# Fixed correlation plus basis still has a real reflection, with a pure-P positive control.
Q<-matrix(c(1,-1,1),3,1,dimnames=list(paste0('x',1:3),'q'))
cf<-list(default='zero',fixed=list(fixed(~G:P,.2)))
b<-prep(bhmf(G=~x1+x2+x3,P=component(~x1+x2+x3,Q,loading='shared'),cor=cf),prior=set_prior('half_normal(1)','amplitude'))
b_checks<-check_model(b)
ok(b_checks$identification$status=='proven_unidentified'&&!is.null(b_checks$identification$affine_correlation_alias),'fixed-rho basis reflection known global negative')
v<-.bh_covariance_probe(b,1);lay<-.bh_covariance_layout(b);v[lay$indices$loading]<-c(.5,.8,-.3);v[lay$indices$amplitude]<-.6
w<-v;w[lay$indices$loading]<--v[lay$indices$loading]-2*.2*.6*Q[,1]
ok(max(abs(.bh_covariance_state(b,v)$Sigma_predictor-.bh_covariance_state(b,w)$Sigma_predictor))<1e-14,'actual basis state map preserves reflection alias')
err(stancode(b),'E_IDENTIFICATION')
b0<-prep(bhmf(G=~x1+x2+x3,P=component(~x1+x2+x3,Q,loading='shared'),cor=cf,
 loadings=list(fixed=list(list(cells=list(G=~x1),value=0)))),prior=set_prior('half_normal(1)','amplitude'))
b0_checks<-check_model(b0)
ok(is.null(b0_checks$identification$affine_correlation_alias)&&all(b0_checks$identification$probe_ranks==6L),'fixed-zero pure basis row remains rank6 positive')
# Two opposite signed markers, one offdiagonal alias and known rho retain a different global alias.
mk<-prep(bhmf(A=~x1+x2,B=~x1+x2,cor=c2(.3),scale=list(A=list(marker=~x1,value=1),B=list(marker=~x2,value=-1)),
 loadings=list(equal=list(off=list(A=~x2,B=~x1)))))
mk_checks<-check_model(mk)
ok(mk_checks$identification$status=='proven_unidentified'&&!is.null(mk_checks$identification$affine_correlation_alias),'opposite-marker fixed-rho alias is preserved')
err(stancode(mk),'E_IDENTIFICATION')
# Portable sealed inspection and immutable source-level domains.
before<-serialize(s,NULL,version=2);tmp<-tempfile();saveRDS(s,tmp);again<-readRDS(tmp);unlink(tmp)
ok(identical(model_table(s),model_table(again))&&identical(s_checks,check_model(again))&&identical(before,serialize(s,NULL,version=2)),'prepared affine RDS and inspection preserve sealed bytes')
cat('PASSED',passed,'public integration checks\n')
