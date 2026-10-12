source(file.path("helpers", "native-guard.R"))
library(bmeas)
# One public preparation chain; no compiler or optimizer is called.
local({
  private <- function(n) getFromNamespace(n, "bmeas")
  refuses <- function(expr, code) {
    e <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(e, "measurement_error"), identical(e$code, code))
  }
  d <- data.frame(person=1:4, x1=c(.2,.8,.3,1), x2=c(.7,.6,-.3,.1), x3=c(-.2,.6,.9,.4))
  prior <- set_prior("normal(0,.4)", "loading", target=~x2+x3)
  s <- bmeas(bmf(G=~x1+x2+x3), d, subject="person", prior=prior, fit=FALSE)
  checks <- check_model(s); p <- get_prior(s)
  stopifnot(nrow(model_table(s,"targets"))==3L,
    sum(p$class=="loading" & p$target %in% c("scores::mu::x2","scores::mu::x3"))==2L,
    all(p$prior[p$class=="loading" & p$target %in% c("scores::mu::x2","scores::mu::x3")]=="normal(0,.4)"),
    checks$identification$status=="local_rank_full_not_global_proof",
    is.character(stancode(s)), is.list(standata(s)))
  refuses(bmeas(bmf(G=~x1+x2+x3),d,subject="person",fit=FALSE,
    prior=c(prior,set_prior("normal(0,.2)","loading",target=~x2+x3))),"E_PRIOR_CONFLICT")
  changed <- s; changed$data_map$blocks$scores$y[1] <- 99
  refuses(model_table(changed),"E_SPEC_CHANGED")
  # Tiny fixed basic-family composition protects role/source forwarding.
  model <- gaussian_model()
  g <- observe(model,data.frame(person=1:3,task="g",y=c(.1,.2,.3)),"person","task","y",
    units="points",parameters=list(mu=list(fixed=0),sigma=list(fixed=1)))
  b <- observe(binomial_model(),data.frame(person=1:3,task="b",k=c(1L,0L,2L),n=2L),
    "person","task","k",fields=list(trials="n"),units="count",parameters=list(p=list(fixed=0,scale="predictor")))
  l <- observe(lognormal_model(),data.frame(person=1:3,task="l",y=c(1,2,3)),"person","task","y",
    units="seconds",parameters=list(meanlog=list(fixed=0),sdlog=list(fixed=1)))
  mixed <- bmeas(bmf(),list(g=g,b=b,l=l),subjects="shared",fit=FALSE)
  code <- as.character(stancode(mixed))
  stopifnot(all(vapply(c("normal_lpdf","binomial_logit_lpmf","lognormal_lpdf"),grepl,
    logical(1),x=code,fixed=TRUE)),sum(vapply(mixed$data_map$blocks,nrow,integer(1)))==9L)
  # These refusals must happen before any expensive execution boundary.
  ns <- asNamespace("bmeas"); names <- c(".bh_fit_scalar",".bh_compile_scalar",".mp_toolchain_fingerprint")
  saved <- mget(names,ns); calls <- 0L
  on.exit(for(n in names){unlockBinding(n,ns);assign(n,saved[[n]],ns);lockBinding(n,ns)},add=TRUE)
  for(n in names){unlockBinding(n,ns);assign(n,function(...){calls<<-calls+1L;stop("FORBIDDEN execution")},ns);lockBinding(n,ns)}
  disabled <- gaussian_model()
  disabled$capabilities$available[disabled$capabilities$capability=="fit"] <- FALSE
  blocked <- observe(disabled, data.frame(person=rep(1:3,3),task=rep(c("x1","x2","x3"),each=3),
    y=c(.1,.3,.2,.4,.2,.5,.3,.6,.2)),"person","task","y",units="points")
  unavailable <- bmeas(bmf(G=~scores$mu[c("x1","x2","x3")]),list(scores=blocked),fit=FALSE)
  refuses(bmeas(unavailable,chains=1,iter=10,warmup=5,resources="development"),"E_CAPABILITY")
  plan <- execution_plan(s,chains=1,iter=10,warmup=5,resources=list(max_output_bytes=2^20,cache="deferred"))
  bytes <- plan$output_estimate$estimated_csv_bytes
  exact <- execution_plan(s,chains=1,iter=10,warmup=5,resources=list(max_output_bytes=bytes,cache="deferred"))
  short <- execution_plan(s,chains=1,iter=10,warmup=5,resources=list(max_output_bytes=bytes-1,cache="deferred"))
  refuses(private(".bh_resource_fit_gate")(short),"E_OUTPUT_BUDGET")
  stopifnot(exact$resource_admission$allowed,!short$resource_admission$allowed,!short$execution_allowed,calls==0L)
  cat("PASS public Gaussian/prior preparation, fixed mixed roles, seals, capability and exact resource boundaries\n")
})
