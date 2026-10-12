source(file.path("helpers", "native-guard.R"))
library(bmeas)
local({
  private <- function(n)getFromNamespace(n,"bmeas")
  d <- data.frame(person=1:4,x1=c(.2,.8,.3,1),x2=c(.7,.6,-.3,.1),x3=c(-.2,.6,.9,.4))
  s <- bmeas(bmf(G=~x1+x2+x3,scale=list(G=list(marker=~x1,value=-2)),
    loadings=list(equal=list(shared=list(G=~x2+x3))),residual=list(targets=~x1)),d,subject="person",fit=FALSE)
  layout <- private(".bh_covariance_layout")(s);v<-private(".bh_covariance_probe")(s,1)
  state <- private(".bh_covariance_state")(s,v)
  p <- get_prior(s)
  stopifnot(length(layout$indices$loading)==1L,sum(p$class=="loading")==1L,
    state$effective_loadings[2,1]==state$effective_loadings[3,1],
    abs(state$effective_loadings[1,1]+2*state$component_sd[1])<1e-12,
    state$Sigma_parameter[1,1]>0,all(state$Sigma_parameter[-1,-1]==0),
    max(abs(state$Sigma_predictor-state$Sigma_common-state$Sigma_parameter))<1e-12)
  Q <- private(".bh_centering_basis")(3L)
  white <- c(.2,-.3);z<-Q%*%white
  stopifnot(identical(dim(Q),c(3L,2L)),max(abs(crossprod(Q)-diag(2)))<1e-12,
    max(abs(colSums(Q)))<1e-12,max(abs(drop(crossprod(Q,z))-white))<1e-12)
  e<-tryCatch(private(".bh_centering_basis")(1L),error=identity)
  stopifnot(inherits(e,"measurement_error"),e$code=="E_PERSON_ROSTER")
  # One public centered task model, with an independent two-person scalar oracle.
  centered_model <- bmf(G=~x1,loadings=list(fixed=list(list(value=1,cells=list(G=~x1)))),
    residual=list(targets=~x1),person=list(distribution="sample_centered",ids=c("1","2")),
    augmentation=list(type="task_cholesky",targets=~x1))
  centered <- bmeas(centered_model,d[1:2,c("person","x1")],subject="person",fit=FALSE)
  ix <- private(".bh_covariance_layout")(centered)$indices
  value <- private(".bh_covariance_probe")(centered,1)
  value[ix$parameter_sd] <- .4;value[ix$observation_variance] <- .09
  result <- private(".bh_task_state")(centered,value,0,matrix(.2,1,1))
  stopifnot(max(abs(result$Sigma_predictor-matrix(1+.4^2,1,1)))<1e-12,
    max(abs(drop(result$delta)-sqrt(1+.4^2)*c(.2,-.2)/sqrt(2)))<1e-12,
    max(abs(private(".bh_centering_covariance")(centered)-matrix(c(1,-1,-1,1),2)))<1e-12,
    is.list(standata(centered)),is.character(stancode(centered)))
  bad <- tryCatch(private(".bh_task_state")(centered,value,0,matrix(c(.2,.3),1,2),"constrained"),error=identity)
  stopifnot(inherits(bad,"measurement_error"),bad$code=="E_STRUCTURE_STATE")
  path <- tempfile(fileext=".rds");on.exit(unlink(path),add=TRUE);saveRDS(centered,path)
  stopifnot(identical(model_table(readRDS(path)),model_table(centered)))
  cat("PASS signed marker, one density per alias, residual covariance, centered basis inverse, public scalar task oracle, transport and refusal\n")
})
