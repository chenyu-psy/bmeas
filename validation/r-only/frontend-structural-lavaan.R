source(file.path("helpers","native-guard.R"))
root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root))for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
pvt<-function(n)if(nzchar(root))get(n,.GlobalEnv)else getFromNamespace(n,"bmeas")
passed<-0L;fit_count<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
err<-function(expr,code="E_STRUCTURAL_CONTRACT"){
 e<-tryCatch(force(expr),error=identity);ok(inherits(e,"measurement_error")&&identical(e$code,code),paste(code,if(inherits(e,"error"))conditionMessage(e)else"no error"))
}
near<-function(a,b)all(is.finite(c(a,b)))&&max(abs(a-b))<1e-6*(1+max(abs(b)))
make<-function(S,D=1L,role="posterior",ids=paste0("component::",seq_len(nrow(S)))) {
 a<-array(rep(as.vector(S),each=D),c(D,nrow(S),ncol(S)),list(NULL,ids,ids))
 population_draws(a,data.frame(chain_id=rep(7L,D),saved_row=seq_len(D)),
 data.frame(id=ids,scale=rep("fixed",length(ids)),orientation=rep("fixed",length(ids))),role,"m","p","synthetic","fixed covariance fixture","joint_population_covariance_parameters")
}
map<-function(z,p,q,prior=NULL,...) {
 z<-structural_draws(z,p,q,prior,backend="lavaan",...)
 fit_count<<-fit_count+z$posterior$diagnostics$fit_calls+if(is.null(z$prior))0L else z$prior$diagnostics$fit_calls
 if(fit_count>35L)stop("Focused test fit budget exceeded")
 z
}
# Dependency guards execute without invoking fits, including absent optional package.
f<-pvt(".bh_lavaan_dependency");ee<-new.env(parent=environment(f));ee$requireNamespace<-function(...)FALSE;environment(f)<-ee
err(f(),"E_STRUCTURAL_DEPENDENCY");err(pvt(".bh_lavaan_version")("0.7.3"),"E_STRUCTURAL_DEPENDENCY")
bd<-pvt(".bh_lavaan_boundary")
ok(bd(matrix(1e-300),1e-150,1e-8)$status=="checked"&&!bd(matrix(1e-300),1e-150,1e-8)$boundary,"guarded tiny boundary normalization")
ok(bd(matrix(1),1e-200,1e-8)$status=="unrepresentable_boundary_check","overflow in boundary normalization retained as status")
e<-new.env(parent=environment(bd));e$eigen<-function(...)stop("forced eigen failure");environment(bd)<-e
ok(bd(matrix(1),1,1e-8)$status=="lavaan_boundary_check_failed","boundary eigen failure is captured")
if(!requireNamespace("lavaan",quietly=TRUE)||as.character(utils::packageVersion("lavaan"))!="0.7.2") {
 cat("SKIP optional lavaan 0.7.2 fixtures;",passed,"dependency checks passed\n")
} else {
 S<-matrix(c(2,1,1,3),2);post<-make(S);ids<-post$variables$id;p<-ids[1];q<-ids[2]
 set.seed(189);rng<-.Random.seed
 a<-map(post,p,q);b<-structural_draws(post,p,q)
 ok(near(a$posterior$coefficients,b$posterior$coefficients)&&near(a$posterior$residual_covariance,b$posterior$residual_covariance),"scalar saturated analytic parity")
 ok(identical(rng,.Random.seed),"no RNG-state change")
 ok(!is.null(a$mapping_spec$parsed_parameter_table)&&identical(a$mapping_spec$parameter_contract,pvt(".bh_lavaan_parameter_contract")(a$mapping_spec$parsed_parameter_table)),"compiled parameter extraction contract")
 ok(a$estimand_type=="regression_functional"&&a$mapping_spec$lavaan_version=="0.7.2","saturated estimand and pinned version")
 ok(identical(a$mapping_spec$aliases$id,ids)&&identical(a$mapping_spec$aliases$alias,c("x1","y1")),"nonsyntactic stable IDs safely aliased")
 op<-a$mapping_spec$resolved_options
 ok(identical(op$sample.cov.rescale,FALSE)&&op$se=="none"&&identical(op$test,"none")&&op$rstarts==0&&op$optim.attempts==1&&op$optim.method=="nlminb"&&!op$fixed.x&&!op$conditional.x,"resolved immutable no-inference optimizer options")
 ok(a$posterior$draw_status$global_optimum=="not_verified"&&a$posterior$draw_status$local_optima_check=="not_performed","no uniqueness or identification certification")
 a2<-map(post,p,q);ok(identical(a,a2),"exact deterministic repeat")
 st<-map(post,p,q,coefficient_scale="standardized");bst<-structural_draws(post,p,q,coefficient_scale="standardized")
 ok(near(st$posterior$coefficients,bst$posterior$coefficients)&&near(st$posterior$residual_covariance,bst$posterior$residual_covariance),"scalar input-SD standardized parity")
 u<-setNames(c(-2,3),ids);un<-map(post,p,q,units=u);bu<-structural_draws(post,p,q,units=u)
 ok(near(un$posterior$coefficients,bu$posterior$coefficients)&&near(un$posterior$residual_covariance,bu$posterior$residual_covariance),"signed units parity")
 pair<-map(post,p,q,make(S,2L,"prior"));ok(nrow(pair$prior$draw_keys)==2L&&nrow(pair$posterior$draw_keys)==1L&&identical(pair$prior$draw_keys,make(S,2L,"prior")$draw_keys),"unequal draw counts and repeated states retained")
 ok(identical(pair$prior$diagnostics$resolved_options,pair$posterior$diagnostics$resolved_options),"prior/posterior exact options")
 XX<-matrix(c(2,.4,.4,1.5),2);B<-matrix(c(.6,-.2,.3,.7),2);Psi<-matrix(c(1.2,.25,.25,.8),2)
 S4<-rbind(cbind(XX,XX%*%B),cbind(t(B)%*%XX,t(B)%*%XX%*%B+Psi));S4[lower.tri(S4)]<-t(S4)[lower.tri(S4)]
 z<-make(S4);id<-z$variables$id;pp<-id[1:2];qq<-id[3:4]
 m<-map(z,pp,qq);ok(near(m$posterior$coefficients[1,,],B)&&near(m$posterior$residual_covariance[1,,],Psi),"multivariate saturated analytic parity")
 ms<-map(z,pp,qq,coefficient_scale="standardized");bs<-structural_draws(z,pp,qq,coefficient_scale="standardized")
 ok(near(ms$posterior$coefficients,bs$posterior$coefficients)&&near(ms$posterior$residual_covariance,bs$posterior$residual_covariance),"multivariate standardized parity")
 perm<-c(4,2,1,3);zr<-make(S4[perm,perm],ids=id[perm]);mr<-map(zr,pp,qq)
 ok(near(mr$posterior$coefficients,m$posterior$coefficients)&&near(mr$posterior$residual_covariance,m$posterior$residual_covariance),"named reordering parity")
 n50<-map(z,pp,qq,backend_control=list(reference_n=50L));n5000<-map(z,pp,qq,backend_control=list(reference_n=5000L))
 ok(identical(n50$posterior$coefficients,n5000$posterior$coefficients)&&identical(n50$posterior$residual_covariance,n5000$posterior$residual_covariance),"reference-N invariance with covariance rescale disabled")
 proj<-map(z,pp,qq,model="y1 ~ x1 + x2; y2 ~ x2; y1 ~~ 0*y2")
 ok(proj$estimand_type=="projected_path"&&proj$posterior$draw_status$status=="mapped"&&proj$posterior$draw_status$objective>.1&&proj$posterior$draw_status$max_abs_covariance_residual>.1,"overidentified covariance projection explicitly marked")
 ok(proj$posterior$coefficients[1,1,2]==0&&near(proj$posterior$coefficients[1,2,2],.78)&&proj$posterior$residual_covariance[1,1,2]==0,"fixed missing path and residual restrictions retained")
 ps<-map(z,pp,qq,model="y1 ~ x1 + x2; y2 ~ x2",coefficient_scale="standardized")
 ok(near(ps$posterior$coefficients[1,2,2],.78*sqrt(S4[2,2]/S4[4,4])),"projected standardization uses input SD")
 fail<-map(post,p,q,backend_control=list(iter_max=1L,eval_max=2L))
 ok(fail$posterior$draw_status$status%in%c("lavaan_not_converged","lavaan_fit_failed")&&all(is.na(fail$posterior$coefficients))&&identical(fail$posterior$draw_keys,post$draw_keys),"optimizer failure keeps IDs and NA slots")
 ok(summary(fail)$posterior$status=="unavailable_incomplete_mapping","failed mapping blocks summary")
 boundary<-map(post,p,q,backend_control=list(boundary_tol=.9))
 ok(boundary$posterior$draw_status$status=="lavaan_boundary_solution"&&isTRUE(boundary$posterior$draw_status$boundary)&&all(is.na(boundary$posterior$coefficients)),"boundary is never an ordinary mapped draw")
 # All invalid rows and all declared coordinates remain guarded before optimizer.
 bad<-make(S,3L);bad$covariance[1,1,2]<-9;bad$covariance[2,1,1]<-NA_real_;bad$covariance[3,,]<-matrix(c(1,2,2,1),2)
 invalid<-map(bad,p,q)
 ok(identical(invalid$posterior$draw_status$status,c("nonexact_symmetric_covariance","nonfinite_covariance","non_positive_definite"))&&invalid$posterior$diagnostics$fit_calls==0&&all(is.na(invalid$posterior$coefficients)),"invalid draws kept without optimizer calls")
 extra<-make(diag(3));extra$covariance[1,3,3]<--1
 ok(map(extra,extra$variables$id[1],extra$variables$id[2])$posterior$draw_status$status=="non_positive_definite","unused declared coordinate still validated")
 dir<-post;dir$direction_resolved[1]<-FALSE;ok(map(dir,p,q)$posterior$draw_status$status=="unresolved_direction","direction failure retained")
 err(map(post,p,q,make(S,2L,"prior"),backend_control=list(max_fits=2)),"E_STRUCTURAL_BUDGET")
 for(model in c("f =~ y1","y1 ~ y1","y1 ~ a*x1","y1 ~ 1","y1 ~ x1; y1 ~ x1","x1 ~~ x1","y1 ~ x1; y1 ~~ x1","y1 ~ x1; a == 0","y1 ~ x1 #comment","y1 ~ x1*","y1 ~ 0*x1*"))err(map(post,p,q,model=model))
 err(map(post,p,q,backend_control=list(sample_cov_rescale=TRUE)))
 err(structural_draws(post,p,q,model="y1~x1"))
 err(map(post,p,q,backend_control=list(reference_n=1)))
 # Direct reference confirms disabled rescaling is consequential, two fits.
 spec<-a$mapping_spec;SS<-S;dimnames(SS)<-list(c("x1","y1"),c("x1","y1"));spec$options$sample_cov_rescale<-TRUE
 residuals<-numeric(2)
 for(i in 1:2){spec$reference_n<-c(50L,5000L)[i];f<-pvt(".bh_lavaan_fit")(SS,spec);fit_count<-fit_count+1L;pt<-lavaan::parTable(f);residuals[i]<-pt$est[pt$lhs=="y1"&pt$op=="~~"&pt$rhs=="y1"]}
 ok(near(residuals,c(2.5*49/50,2.5*4999/5000)),"rescale-TRUE negative control alters residuals")
 ok(summary(a)$posterior$status=="descriptive_only"&&summary(a)$bayes_factor=="not_implemented","descriptive only; S4 remains absent")
 cat("LAVAAN_FIT_CALLS",fit_count,"\n")
 cat("PASSED",passed,"checks\n")
}
