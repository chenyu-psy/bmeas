source(file.path("helpers", "native-guard.R"))
# Fixed synthetic population matrices only: no sampling, fitting, density or BF.
root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root))for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
pvt<-function(n)if(nzchar(root))get(n,.GlobalEnv)else getFromNamespace(n,"bmeas")
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
err<-function(expr,code="E_STRUCTURAL_CONTRACT"){
 e<-tryCatch(force(expr),error=identity)
 ok(inherits(e,"measurement_error")&&identical(e$code,code),paste(code,if(inherits(e,"error"))conditionMessage(e)else"no error"))
}
near<-function(a,b)max(abs(a-b))<1e-10
make<-function(S,D=3L,role="posterior",ids=paste0("v",seq_len(nrow(S)))) {
 a<-array(rep(as.vector(S),each=D),c(D,nrow(S),ncol(S)),list(NULL,ids,ids))
 population_draws(a,data.frame(chain_id=rep(7L,D),saved_row=seq_len(D)),
   data.frame(id=ids,scale=rep("fixed_synthetic_unit",length(ids)),orientation=rep("fixed_positive_axis",length(ids))),
   role,"synthetic_model_1","synthetic_prior_1","synthetic","fixed algebraic fixture",
   "joint_population_covariance_parameters")
}
S<-matrix(c(2,1,1,3),2)
post<-make(S);prior<-make(S,2L,"prior")
x<-structural_draws(post,"v1","v2",prior)
ok(near(x$posterior$coefficients,.5)&&near(x$posterior$residual_covariance,2.5),"scalar analytic regression")
ok(identical(x$posterior$draw_keys,post$draw_keys)&&identical(x$prior$draw_keys,prior$draw_keys),"unequal independent draw sets preserve IDs")
ok(dim(x$prior$coefficients)[1L]==2L&&dim(x$posterior$coefficients)[1L]==3L,"no row pairing")
ok(identical(x,structural_draws(post,"v1","v2",prior)),"same input exactly same output")
ok(x$posterior$diagnostics$repeated_covariance_rows==2L,"duplicate values retained and disclosed")
ok(grepl("not ESS",x$posterior$diagnostics$independence,fixed=TRUE),"no independence claim")
ok(is.null(structural_draws(post,"v1","v2")$prior),"posterior-only permitted")
ok(summary(x)$posterior$status=="descriptive_only"&&summary(x)$bayes_factor=="not_implemented","descriptive summary no BF")
std<-structural_draws(post,"v1","v2",coefficient_scale="standardized")
ok(near(std$posterior$coefficients,1/sqrt(6))&&near(std$posterior$residual_covariance,2.5/3),"standardized coefficient and residual units")
u<-c(v1=-2,v2=3)
rescaled<-structural_draws(post,"v1","v2",units=u)
ok(near(rescaled$posterior$coefficients,-.75)&&near(rescaled$posterior$residual_covariance,22.5),"fixed signed unit transformation")
rescaled_std<-structural_draws(post,"v1","v2",units=u,coefficient_scale="standardized")
ok(near(rescaled_std$posterior$coefficients,-1/sqrt(6)),"orientation sign propagated to standardized path")

# Historical multivariate fixture formulas reused, now exercised through public API.
A<-matrix(c(1.2,.2,-.1,.1,.9,.25,.3,-.2,1.1),3)
XX<-crossprod(A)+diag(.4,3);B<-matrix(c(.3,-.5,.2,-.1,.4,.6),3,2)
Psi<-matrix(c(.9,.12,.12,.6),2)
# Construct an exactly symmetric positive-definite fixture directly: the
# historical chained products can differ by 1 ULP across BLAS platforms.
XY<-XX%*%B;YY<-crossprod(chol(XX)%*%B)+Psi
S5<-rbind(cbind(XX,XY),cbind(t(XY),YY))
ok(identical(S5,t(S5)),"multivariate fixture is exactly symmetric before mapping")
multi<-make(S5)
m<-structural_draws(multi,c("v1","v2","v3"),c("v4","v5"))
ok(near(m$posterior$coefficients[1,,],B)&&near(m$posterior$residual_covariance[1,,],Psi),"multivariate historical fixture")
rebuilt<-rbind(cbind(XX,XX%*%m$posterior$coefficients[1,,]),cbind(t(m$posterior$coefficients[1,,])%*%XX,
  t(m$posterior$coefficients[1,,])%*%XX%*%m$posterior$coefficients[1,,]+m$posterior$residual_covariance[1,,]))
ok(near(rebuilt,S5),"covariance analytic round trip")
perm<-c(5,2,1,4,3);reordered<-make(S5[perm,perm],ids=paste0("v",perm))
mr<-structural_draws(reordered,c("v1","v2","v3"),c("v4","v5"))
ok(near(mr$posterior$coefficients,m$posterior$coefficients),"stable IDs survive coherent variable reordering")
for(beta in c(-2,-.25,0,.8,2))for(vx in c(.3,1,2))for(ve in c(.2,1.1)) {
 ss<-matrix(c(vx,beta*vx,beta*vx,beta^2*vx+ve),2)
 z<-structural_draws(make(ss,1L),"v1","v2")
 ok(near(z$posterior$coefficients,beta)&&near(z$posterior$residual_covariance,ve),"scalar grid roundtrip")
}

# All bad numeric rows remain and block summaries; no complete-case filtering.
bad<-post;bad$covariance[1,1,2]<-9;bad$covariance[2,1,1]<-NA_real_;bad$covariance[3,,]<-matrix(c(1,2,2,1),2)
z<-structural_draws(bad,"v1","v2")
ok(identical(z$posterior$draw_status$status,c("nonexact_symmetric_covariance","nonfinite_covariance","non_positive_definite")),"per-row failures distinguish cause")
ok(identical(z$posterior$draw_keys,bad$draw_keys)&&all(is.na(z$posterior$coefficients)),"failed IDs and output slots retained")
ok(summary(z)$posterior$status=="unavailable_incomplete_mapping","failed rows never silently omitted from summaries")
bad<-post;bad$available[1,1,2]<-FALSE
ok(structural_draws(bad,"v1","v2")$posterior$draw_status$status[1L]=="unavailable_covariance","availability failure cannot be upgraded")
bad<-post;bad$direction_resolved[1L]<-FALSE
ok(all(structural_draws(bad,"v1","v2")$posterior$draw_status$status=="unresolved_direction"),"unresolved sign is explicit")
nearS<-diag(c(1,1e-12,2));nearS[1,3]<-nearS[3,1]<-.2
z<-structural_draws(make(nearS),c("v1","v2"),"v3")
ok(all(z$posterior$draw_status$status=="mapped_near_singular"),"ill-conditioned predictor block warning retained")
z<-structural_draws(make(matrix(c(1,1-1e-12,1-1e-12,1),2)),"v1","v2")
ok(all(z$posterior$draw_status$status=="mapped_near_singular")&&all(z$posterior$draw_status$rcond==1),"near-singular full covariance warned even with one predictor")
singular<-matrix(1,2,2)
ok(all(structural_draws(make(singular),"v1","v2")$posterior$draw_status$status=="non_positive_definite"),"singular covariance not repaired")
ok(all(structural_draws(post,"v1","v2",units=c(v1=1e300,v2=1))$posterior$draw_status$status=="unrepresentable_unit_transform"),"unit overflow diagnosed")
tiny<-matrix(c(1,1e-300,1e-300,1),2)
ok(all(structural_draws(make(tiny),"v1","v2",units=c(v1=1e-20,v2=1e-20))$posterior$draw_status$status=="unrepresentable_unit_transform"),"unit underflow diagnosed")
extreme<-make(matrix(c(1e300,1e-100,1e-100,1e-300),2))
z<-structural_draws(extreme,"v1","v2",coefficient_scale="standardized")
ok(all(abs(z$posterior$coefficients/1e-100-1)<1e-12),"standardized path survives unstandardized underflow")
ok(all(structural_draws(extreme,"v1","v2")$posterior$draw_status$status=="unrepresentable_mapping"),"nonzero unstandardized underflow is never a false zero")
z<-structural_draws(extreme,"v2","v1")
ok(all(abs(z$posterior$coefficients/1e200-1)<1e-12),"reciprocal extreme path retains representable value")
z<-structural_draws(extreme,"v1","v2",units=c(v1=1e-150,v2=1e150),coefficient_scale="standardized")
ok(all(abs(z$posterior$coefficients/1e-100-1)<1e-12),"extreme reciprocal unit transform preserves standardized path")
scaled_tiny<-make(matrix(c(1e-300,0,0,1e-300),2))
z<-structural_draws(scaled_tiny,"v1","v2",units=c(v1=1e200,v2=1e200))
ok(all(abs(z$posterior$residual_covariance/1e100-1)<1e-12),"unit multiplier product overflow avoided when result representable")

bad<-post;bad$draw_keys[2,]<-bad$draw_keys[1,];err(structural_draws(bad,"v1","v2"))
bad<-post;dimnames(bad$covariance)[[2]]<-rev(bad$variables$id);err(structural_draws(bad,"v1","v2"))
bad<-post;bad$variables$scale[1]<-"";err(structural_draws(bad,"v1","v2"))
bad<-post;bad$variables$orientation<-NULL;err(structural_draws(bad,"v1","v2"))
bad<-post;bad$provenance$joint_scope<-"factor_score_sample_covariance";err(structural_draws(bad,"v1","v2"))
bad<-post;bad$provenance$verification<-"certified";err(structural_draws(bad,"v1","v2"))
bad<-prior;bad$model_id<-"other_model";err(structural_draws(post,"v1","v2",bad),"E_STRUCTURAL_IDENTITY")
bad<-prior;bad$prior_id<-"other_prior";err(structural_draws(post,"v1","v2",bad),"E_STRUCTURAL_IDENTITY")
bad<-prior;bad$variables$orientation[1]<-"opposite";bad$parameterization$variables<-bad$variables
err(structural_draws(post,"v1","v2",bad),"E_STRUCTURAL_IDENTITY")
err(structural_draws(post,"v1","v2",post))
err(structural_draws(prior,"v1","v2"))
err(structural_draws(post,"v1","v1"));err(structural_draws(post,"unknown","v2"))
err(structural_draws(post,"v1","v2",units=c(v2=1,v1=1)))
err(structural_draws(post,"v1","v2",control=list(max_draws=2)),"E_STRUCTURAL_BUDGET")
err(structural_draws(post,"v1","v2",prior,control=list(max_values=25)),"E_STRUCTURAL_BUDGET")
err(structural_draws(post,"v1","v2",control=list(jitter=1e-8)))
err(population_draws(matrix(1,2,2),post$draw_keys,post$variables,"posterior","m","p","synthetic","fixture","joint_population_covariance_parameters"))

# Synthetic extraction-shaped data, explicitly not an actual posterior fit.
bridge<-pvt(".bh_structural_component");cap<-pvt(".bh_structural_control")(list())
coords<-data.frame(index=1:2,id=c("v1","v2"),display=c("A","B"))
sc<-data.frame(coordinate_id=coords$id,state=c("fixed","free"),marker_value=c(NA_real_,-2))
ori<-data.frame(coordinate_id=coords$id,pinned=c(FALSE,TRUE),anchor=c("positive_loading",NA_character_))
native<-structure(list(schema_version="bmeas_component_draws_v1",domain="declared_component_population",
 coordinates=coords,maps=list(component_scales=sc,orientation=ori),prior=list(test="synthetic"),
 covariance=post$covariance,draw_keys=post$draw_keys,available=post$available,
 source=list(bound_context=list(spec_fingerprint="synthetic_spec",fit_fingerprint="synthetic_spec")),
 upstream=list(current_cache=list(statuses=list(inference=list(status="not_assessed"))))),class="bmeas_component_draws")
bound<-bridge(native,cap)
ok(identical(bound$upstream,native)&&identical(bound$parameterization$maps,native$maps),"all original extraction and mapping evidence preserved")
ok(all(bound$direction_resolved)&&bound$distribution=="posterior","ordinary anchor and marker sign bridge")
ok(identical(bound$covariance,native$covariance)&&identical(bound$draw_keys,native$draw_keys),"bridge changes no covariance or keys")
bad<-bound;bad$covariance[1,1,1]<-10;err(structural_draws(bad,"v1","v2"),"E_STRUCTURAL_IDENTITY")
bad<-bound;bad$available[1,1,1]<-FALSE;err(structural_draws(bad,"v1","v2"),"E_STRUCTURAL_IDENTITY")
bad<-bound;bad$parameterization$maps$component_scales$marker_value[2]<-2
err(structural_draws(bad,"v1","v2"),"E_STRUCTURAL_IDENTITY")
bad<-bound;bad$provenance$source_kind<-"external";bad$provenance$verification<-"user_declared_not_verified"
err(structural_draws(bad,"v1","v2"))
bad<-bound;bad$upstream$upstream$current_cache$statuses$posterior_propriety<-list(status="proven_improper")
err(structural_draws(bad,"v1","v2"),"E_COMPONENT_POSTERIOR")
bad<-bound;bad$distribution<-"prior";err(structural_draws(post,"v1","v2",bad))
native$maps$orientation$anchor<-c(NA_character_,NA_character_)
bound<-bridge(native,cap)
ok(!bound$direction_resolved[1]&&bound$direction_resolved[2],"unanchored latent direction remains unresolved")
native$maps$orientation_sign_system<-list(unanchored_binary_basis=matrix(integer(),2,0))
ok(all(bridge(native,cap)$direction_resolved),"combined GF2 anchors resolve directions without direct anchor")
native$maps$orientation_sign_system$unanchored_binary_basis<-matrix(c(1L,0L),2,1)
ok(identical(bridge(native,cap)$direction_resolved,c(FALSE,TRUE)),"GF2 unresolved row not globally upgraded")
native$available[1,1,1]<-FALSE
ok(!bridge(native,cap)$available[1,1,1],"native availability remains authoritative")
native$source$bound_context$fit_fingerprint<-"different";err(bridge(native,cap))
cat("PASSED",passed,"structural contract/mapping checks; synthetic R-only fixtures\n")
