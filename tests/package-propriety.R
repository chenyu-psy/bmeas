source(file.path("helpers", "native-guard.R"))
library(bmeas)
# Sufficient collapse witnesses, not a model-recovery or sampling experiment.
local({
  d <- data.frame(person=1:3,x1=rep(2,3),x2=c(-1,0,1),x3=c(1,-1,0))
  make <- function(data) bmeas(bmf(G=~x1+x2+x3),data,subject="person",fit=FALSE)
  constant <- make(d)
  stopifnot(constant$checks$posterior_propriety$status=="proven_improper")
  near <- d; near$x1[3] <- 2+1e-10
  stopifnot(make(near)$checks$posterior_propriety$status=="no_scoped_collapse_detected")
  for(scale in c(1e-9,1e9)) {
    scaled <- d; scaled$x1 <- scaled$x1*scale
    stopifnot(make(scaled)$checks$posterior_propriety$status=="proven_improper")
  }
  # P>M and invalid calibrated priors remain independent refusal reasons.
  unidentified <- bmeas(bmf(G=~x1+x2+x3,H=~x1+x2+x3,cor="independent"),near,subject="person",fit=FALSE)
  overflow <- d; overflow$x2 <- c(-8e307,0,8e307)
  invalid <- make(overflow)
  stopifnot(invalid$checks$priors$status=="invalid",invalid$checks$posterior_propriety$status=="not_assessed")
  ns <- asNamespace("bmeas"); names <- c(".bh_fit_scalar",".bh_compile_scalar",".mp_toolchain_fingerprint")
  saved <- mget(names,ns); calls <- 0L
  on.exit(for(n in names){unlockBinding(n,ns);assign(n,saved[[n]],ns);lockBinding(n,ns)},add=TRUE)
  for(n in names){unlockBinding(n,ns);assign(n,function(...){calls<<-calls+1L;stop("FORBIDDEN execution")},ns);lockBinding(n,ns)}
  refuses <- function(expr,code){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,"measurement_error"),identical(e$code,code))}
  refuses(bmeas(constant,chains=1,iter=10,warmup=5,execution="experimental",exploratory=TRUE,resources="development"),"E_POSTERIOR_IMPROPER")
  refuses(bmeas(unidentified,chains=1,iter=10,warmup=5,exploratory=TRUE,resources="development"),"E_IDENTIFICATION")
  refuses(standata(invalid),"E_PRIOR_INVALID")
  stopifnot(calls==0L)
  cat("PASS constant/near-constant/scaled collapse and invalid-prior/identification refusals before execution\n")
})
