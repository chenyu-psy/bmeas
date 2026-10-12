source(file.path("helpers", "native-guard.R"))
# Fixed-factor diagnostic invariance, without backend or RNG calls.
source_mode<-nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))
if(source_mode)for(f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"),"R"),full.names=TRUE))source(f)else library(bmeas)
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,"measurement_error")&&identical(e$code,code),code)}
local({
 old<-if(source_mode)get(".bh_fit_scalar",.GlobalEnv)else getFromNamespace(".bh_fit_scalar","bmeas")
 replace<-function(f)if(source_mode)assign(".bh_fit_scalar",f,.GlobalEnv)else assignInNamespace(".bh_fit_scalar",f,"bmeas")
 on.exit(replace(old));backend_calls<-0L
 replace(function(...){backend_calls<<-backend_calls+1L;stop("FORBIDDEN_BACKEND")})
d<-data.frame(person=rep(1:3,each=3),task=rep(c("x1","x2","x3"),3),y=c(1,.2,.9,1,.6,1.4,1,-.3,.2))
# Reuse immutable model descriptors; this suite tests factor diagnostics, not
# repeated constructor capture (covered by the dedicated callback tests).
gaussian<-gaussian_model();binomial<-binomial_model()
g<-function(data=d)observe(gaussian,data,"person","task","y",units="points")
coin<-function(person=1L,k=1L,n=2L,eta=0,empty=FALSE,free=FALSE)observe(binomial,
 data.frame(person=person,task="coin",k=if(empty)NA_integer_ else k,n=n),"person","task","k",fields=list(trials="n"),units="count",
 parameters=if(free)list(p="task")else list(p=list(fixed=eta,scale="predictor")))
prior<-c(set_prior("normal(0,1)","Intercept"),set_prior("normal(0,1)","loading"),set_prior("half_normal(1)","sigma"))
gaussian_block<-g()
make<-function(block=gaussian_block,extra=NULL,tasks=c("x1","x2","x3"),reverse=FALSE) {
 blocks<-c(list(gauss=block),if(!is.null(extra))list(extra=extra))
 if(reverse)blocks<-rev(blocks)
 bhmm(bhmf(G=~gauss$mu[tasks]),data=blocks,subjects="shared",missing="observed_only",prior=prior,fit=FALSE)
}
base<-make();proof<-check_model(base)$posterior_propriety;pb<-get_prior(base)
ok(proof$status=="proven_improper","baseline exact-data Gaussian divergence")
for(extra in list(coin(),coin(empty=TRUE),coin(person=99L)))for(reverse in c(FALSE,TRUE)) {
 s<-make(extra=extra,reverse=reverse);ch<-check_model(s)
 ok(ch$posterior_propriety$status=="proven_improper"&&identical(ch$posterior_propriety$findings,proof$findings),"positive/empty fixed factor preserves exact Gaussian proof")
 ps<-get_prior(s)
 ok(identical(ps[ps$scope!="latent_person",],pb[pb$scope!="latent_person",]),"fixed factors add no population prior primitives")
 ok(ch$backend$status=="experimental_typed_composition"&&!ch$gaussian_diagnostics$all_gaussian,"diagnostic scope does not grant native qualification")
 err(bhmm(s,chains=1,iter=20,warmup=10,execution="experimental",resources='development'),"E_POSTERIOR_IMPROPER")
 ok(identical(check_model(unserialize(serialize(s,NULL)))$gaussian_diagnostics,ch$gaussian_diagnostics),"saved context recomputes diagnostic factorization")
}
small<-g(d[d$task!="x3",]);a<-make(small,tasks=c("x1","x2"));b<-make(small,coin(),tasks=c("x1","x2"))
ac<-check_model(a);bc<-check_model(b)
ok(ac$identification$status=="proven_unidentified"&&bc$identification$status=="proven_unidentified","fixed coin preserves Gaussian covariance-dimension proof")
ok(identical(ac$identification$moments,bc$identification$moments),"coin adds no Gaussian response moments")
err(bhmm(b,chains=1,iter=20,warmup=10,execution="experimental",resources='development'),"E_IDENTIFICATION")
fixed_gaussian<-observe(gaussian,data.frame(person=1L,task="f",y=.3),"person","task","y",units="points",parameters=list(mu=list(fixed=0),sigma=list(fixed=1)))
cg<-make(small,fixed_gaussian,tasks=c("x1","x2"));cgc<-check_model(cg)
ok(cgc$identification$status=="proven_unidentified"&&identical(cgc$identification$moments,ac$identification$moments),"fixed Gaussian moments cannot dilute a covariance-dimension proof")
regular<-d;regular$y[regular$task=="x1"]<-c(.1,.8,-.2)
s<-make(g(regular),coin());sd<-standata(s)
ok(sd$b1_O+sd$b2_O==10L&&grepl("binomial_logit_lpmf",as.character(stancode(s)),fixed=TRUE),"fixed coin row remains in generated model")
ok(abs(check_model(s)$gaussian_diagnostics$constant_factors$log_constant-log(.5))<1e-14,"positive factor records exact evaluable log constant")
for(eta in c(1000,-1000,1e308)) {
 s<-make(extra=coin(n=2L,eta=eta));ch<-check_model(s)
 ok(ch$posterior_propriety$status=="proven_improper"&&ch$gaussian_diagnostics$positive_factors_certified,"finite logit positivity does not depend on rounded probabilities")
}
s<-make(extra=coin(k=0L,n=2L,eta=1e308))
ok(grepl("not_representable",check_model(s)$gaussian_diagnostics$constant_factors$numerical_status),"unrepresentable log constant is separately reported")
err(coin(k=3L,n=2L),"E_BINOMIAL_SUPPORT")
err(make(extra=observe(binomial,data.frame(person=1L,task="coin",k=1L,n=2L),"person","task","k",fields=list(trials="n"),units="count",parameters=list(p=list(fixed=0)))),"E_PARAMETER_RECEIVER_UNQUALIFIED")
free<-make(extra=coin(free=TRUE));free_checks<-check_model(free)
ok(free_checks$posterior_propriety$status=="not_assessed"&&!free_checks$gaussian_diagnostics$eligible,"free non-Gaussian means are outside the factorization")
cm<-register_stan_adapter(new_observation_model("opaque","1",response_schema(y="real"),
 list(location=parameter_spec("real","identity","theta","subject_task","points","Location")),
 stan_density(symbol="opaque_lpdf",arguments=c("y","location"),argument_types=c("real","real"),normalized=TRUE,builtin=TRUE,approximation="none"),model_capabilities(fit=TRUE)),
 function_code="real opaque_lpdf(real y, real location) { return normal_lpdf(y | location, 1); }",function_exports="opaque_lpdf")
opaque<-observe(cm,data.frame(person=1L,task="o",y=.2),"person","task","y",units="points",parameters=list(location=list(fixed=0)))
s<-make(extra=opaque);ch<-check_model(s)
ok(ch$posterior_propriety$status=="proven_non_normalizable"&&ch$gaussian_diagnostics$constant_factors$status=="opaque_fixed_factor","opaque constant cannot normalize a certified Gaussian divergence")
ok(is.na(ch$gaussian_diagnostics$constant_factors$log_constant)&&grepl("not evaluated",ch$posterior_propriety$reason,fixed=TRUE),"opaque density validity/value remain explicitly unevaluated")
err(bhmm(s,chains=1,iter=20,warmup=10,execution="experimental",resources='development'),"E_POSTERIOR_NON_NORMALIZABLE")
s<-make(g(regular),opaque);ch<-check_model(s)
ok(ch$posterior_propriety$status=="not_assessed"&&ch$identification$status=="not_assessed","no positive validity claim for an opaque fixed factor")
s<-make(small,opaque,tasks=c("x1","x2"));ch<-check_model(s)
ok(ch$identification$status=="proven_unidentified"&&grepl("not certified",ch$identification$constant_factor_scope,fixed=TRUE),"Gaussian equivalence witness persists without certifying opaque density")
ok(backend_calls==0L,"all failures precede backend/cache dispatch")
})
cat("Constant-factor checks:",passed,"\n")
