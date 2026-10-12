source(file.path("helpers", "native-guard.R"))
library(bmeas)
# Provider transaction contracts; actual optimizer parity is manual validation.
local({
  private<-function(n)getFromNamespace(n,"bmeas")
  dependency<-private(".bh_lavaan_dependency");env<-new.env(parent=environment(dependency))
  env$requireNamespace<-function(...)FALSE;environment(dependency)<-env
  e<-tryCatch(dependency(),error=identity);stopifnot(e$code=="E_STRUCTURAL_DEPENDENCY")
  e<-tryCatch(private(".bh_lavaan_version")("0.7.3"),error=identity);stopifnot(e$code=="E_STRUCTURAL_DEPENDENCY")
  boundary<-private(".bh_lavaan_boundary")
  stopifnot(!boundary(matrix(1e-300),1e-150,1e-8)$boundary,
    boundary(matrix(1),1e-200,1e-8)$status=="unrepresentable_boundary_check")
  S<-matrix(c(2,1,1,3),2);ids<-c("x","y")
  x<-population_draws(array(S,c(1,2,2),list(NULL,ids,ids)),data.frame(chain_id=1L,saved_row=1L),
    data.frame(id=ids,scale="fixed",orientation="fixed"),"posterior","m","p","synthetic","contract fixture","joint_population_covariance_parameters")
  control<-private(".bh_structural_control")(list());backend<-private(".bh_lavaan_control")(list())
  # Only syntax/options resolution is real; optimizer and extraction terminals are doubles.
  spec<-private(".bh_lavaan_spec")("x","y",NULL,backend,x$variables,c(x=1,y=1),"unstandardized")
  ns<-asNamespace("bmeas");names<-c(".bh_lavaan_fit",".bh_lavaan_extract");saved<-mget(names,ns)
  on.exit(for(n in names){unlockBinding(n,ns);assign(n,saved[[n]],ns);lockBinding(n,ns)},add=TRUE)
  calls<-0L;mode<-"mapped"
  fit<-function(covariance,mapping){calls<<-calls+1L;stopifnot(identical(unname(covariance),S),identical(mapping$options$sample_cov_rescale,FALSE));if(mode=="error")stop("provider failure");list()}
  extract<-function(...)list(B=matrix(.5),Psi=matrix(2.5),implied=S,converged=mode!="not_converged",iterations=1L,objective=0,gradient=0,resolved_options=spec$options)
  for(n in names){unlockBinding(n,ns);assign(n,if(n==names[1])fit else extract,ns);lockBinding(n,ns)}
  run<-function()private(".bh_lavaan_map")(x,"x","y",c(x=1,y=1),"unstandardized",control,spec)
  good<-run();stopifnot(good$coefficients[1,1,1]==.5,good$residual_covariance[1,1,1]==2.5,good$draw_status$status=="mapped")
  mode<-"not_converged";bad<-run();stopifnot(bad$draw_status$status=="lavaan_not_converged",all(is.na(bad$coefficients)))
  mode<-"error";bad<-run();stopifnot(bad$draw_status$status=="lavaan_fit_failed",all(is.na(bad$coefficients)),calls==3L)
  cat("PASS dependency/version, boundary arithmetic and adapter mapped/failed/unavailable slots; zero optimizer calls\n")
})
