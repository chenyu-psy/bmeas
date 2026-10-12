source(file.path("helpers", "native-guard.R"))
# Fixed/equal final marginal correlations: public R/IR controls, no native calls.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
if(!source_mode)for(n in c('.bh_cor_domain','.bh_cor_table','.bh_correlation_prior_geometry'))assign(n,getFromNamespace(n,'bmeas'))
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch(force(x),error=identity);if(!inherits(e,'measurement_error')||!identical(e$code,code)){print(e);stop(code)};ok(TRUE,code)}
d<-data.frame(person=rep(1:8,2));for(j in 1:6)d[[paste0('x',j)]]<-sin(seq_len(16)*j/9)+seq_len(16)/7
prep<-function(f,data=d,prior=NULL)bhmm(f,data,subject='person',missing='observed_only',prior=prior,fit=FALSE)
fixed<-function(pairs,value)list(pairs=pairs,value=value)
eq<-list(default='zero',equal=list(e=list(~A:B,~C:D)))
f<-bhmf(A=~x1,B=~x2,C=~x3,D=~x4,cor=eq)
one<-d;one$x1[one$person>4]<-NA;one$x2[one$person<=4]<-NA
s<-prep(f,one);i<-check_model(s)$identification
ok(is.null(i$uninformed_correlations)&&i$free_covariance_parameters==9L,'one observed alias member protects a shared correlation primitive from false no-overlap proof')
none<-one;none$x3[none$person>4]<-NA;none$x4[none$person<=4]<-NA
z<-prep(f,none);i<-check_model(z)$identification
ok(i$status=='proven_unidentified'&&length(unique(i$uninformed_correlations))==1L,'all alias members lacking overlap prove the unique primitive uninformed')
req<-list(default='zero',equal=list(e=list(~x1:x2,~x3:x4)))
rf<-bhmf(residual=list(targets=~x1+x2+x3+x4,cor=req))
i<-check_model(prep(rf,one))$identification
ok(is.null(i$uninformed_parameter_correlations),'residual no-overlap proof also operates on unique classes')
i<-check_model(prep(rf,none))$identification
ok(i$status=='proven_unidentified'&&length(unique(i$uninformed_parameter_correlations))==1L,'residual all-uninformed aliases produce one proof')
owner<-function(targets,components)list(type='variance_share',targets=targets,components=components,residual=TRUE,shares=~1)
f<-bhmf(G=~x1+x2,P=~x1+x2,C=~x3,cor=list(default='free',equal=list(e=list(~G:P,~G:C))),decomposition=list(a=owner(~x1+x2,~G+P)))
ok(all(f$correlations$state[c(1,2)]=='zero')&&all(vapply(f$correlations$constraint_sources[c(1,2)],function(x)any(startsWith(x,'owner_required_zero:')),logical(1))),'owner zero propagates through equality with retained provenance')
err(bhmf(G=~x1+x2,P=~x1+x2,C=~x3,cor=list(default='free',equal=list(e=list(~G:P,~G:C)),free=list(~G:C)),decomposition=list(a=owner(~x1+x2,~G+P))),'E_CORRELATION_CONFLICT')
err(bhmf(G=~x1+x2,P=~x1+x2,C=~x3,cor=list(default='free',equal=list(e=list(~G:P,~G:C)),fixed=list(fixed(~G:C,.2))),decomposition=list(a=owner(~x1+x2,~G+P))),'E_CORRELATION_CONFLICT')
# Raw common mode remains a different primitive law, with residual affine rules independent.
raw<-list(type='ordered_blocks',blocks=list(first=~A,second=~B))
f<-bhmf(A=~x1+x2,B=~x3+x4,cor=list(default='free',coordinates=raw),residual=list(targets=~x1+x2,cor=list(default='zero',fixed=list(fixed(~x1:x2,.2)))))
missing_raw_prior<-prep(f)
err(stancode(missing_raw_prior),'E_PRIOR_INCOMPLETE')
s<-prep(f,prior=set_prior('normal(0,.3)','cor_raw'))
ok(model_table(s,'correlation_coordinate_system')$type=='ordered_blocks'&&nrow(model_table(s,'raw_correlation_coordinates'))==1L,'raw common declaration survives a separate residual fixed correlation')
ok(!any(model_table(s,'correlation_primitives')$layer=='component')&&all(is.na(model_table(s,'correlation_affine_cells')$parameter[model_table(s,'correlation_affine_cells')$layer=='component'])),'derived raw-mode final correlations acquire no duplicate primitive keys')
ok(sum(get_prior(s)$class=='cor_raw')==1L&&!any(get_prior(s)$class=='cor'),'raw-B prior stays independent of fully fixed residual layer')
ok(grepl('bmeas_corr_layer',as.character(stancode(s)),fixed=TRUE)&&standata(s)$EU==0L,'raw common and fixed residual source remain separate coordinate laws')
err(bhmf(A=~x1,B=~x2,cor=list(default='free',coordinates=raw,fixed=list(fixed(~A:B,.2)))),'E_CORRELATION_COORDINATES')
# Generic multilabel affine declarations are inspectable, with actual local-rank deferral.
multi<-prep(bhmf(A=~x1+x2,B=~x3+x4,C=~x5+x6,cor=list(default='free',fixed=list(fixed(~A:B,.2)))))
g<-check_model(multi);dom<-model_table(multi,'correlation_domains')$component
ok(dom$dimension==2L&&dom$status=='feasible'&&dom$affine_chart_scope=='multilabel_inspection_only','multilabel affine map retains exact identity-completion witness')
ok(g$identification$status=='not_assessed'&&grepl('multilabel',g$identification$reason),'unknown chart is not replaced by unsafe zero/independent rank probes')
ok(g$backend$status=='affine_correlation_not_lowered','multilabel model explicitly has no new runtime support')
# Exact zero/free theorem survives the numerical eigensolver size cap.
names<-paste0('C',seq_len(101));tab<-.bh_cor_table(list(default='zero',free=list(~C1:C2),fixed=list(fixed(~C2:C3,0))),names)
dom<-.bh_cor_domain(tab,101L)
ok(dom$status=='feasible'&&dom$method=='exact_identity_interior'&&dom$runtime_scope=='existing_zero_free_MGS_route','large ordinary graph retains exact feasible identity and old MGS route')
mock<-list(tables=list(correlations=tab,parameter_correlations=.bh_cor_table('independent',character()),components=data.frame(component=names),parameter_residuals=data.frame(target=character()),
 priors=data.frame(class='cor',layer='component',prior='lkj(.5)'),correlation_domains=list(component=dom,parameter=.bh_cor_domain(.bh_cor_table('independent',character()),0L))),formula=list())
ok(.bh_correlation_prior_geometry(mock)$component$status=='existing_zero_free_prior_scope','size cap does not erase old zero/free prior scope')
# Layer-domain reporting does not make a stale whole-model capability claim.
label_spec<-prep(bhmf(A=~x1+x2,B=~x3+x4,cor=list(default='zero',fixed=list(fixed(~A:B,.3)))))
label_bytes<-serialize(label_spec,NULL,version=2);label_domain<-model_table(label_spec,'correlation_domains')$component
label_checks<-check_model(label_spec)
ok(grepl('affine_layer_domain_only',label_domain$runtime_scope,fixed=TRUE)&&!grepl('lowering_not_available',label_domain$runtime_scope,fixed=TRUE),'affine domain scope remains neutral about current source/runtime capability')
ok(label_checks$correlation_lowering$status=='bounded_source_available'&&label_checks$backend$status=='experimental_affine_correlations','whole-model capability fields remain authoritative')
ok(is.character(stancode(label_spec)),'fixed-rho source stays available despite a domain-only scope label')
ok(identical(label_bytes,serialize(label_spec,NULL,version=2)),'domain and current capability inspection preserve prepared bytes')
cat('PASSED',passed,'public scope checks\n')
