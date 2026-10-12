.mp_reference_helmert <- function(n) {
  Q<-matrix(0,n,n-1L)
  for(j in seq_len(n-1L)) {Q[seq_len(j),j]<-1/sqrt(j*(j+1));Q[j+1L,j]<--j/sqrt(j*(j+1))}
  Q
}
reference_structure <- function(name, domains, model) {
  if(!identical(name,"csmu_exp1_memory_gpc_v1")) .mp_abort("E_STRUCTURE_NOT_IMPLEMENTED","unknown reference structure version")
  .mp_scalar_string(model,"reference model block")
  expected<-m3_reference_domain()
  if(!is.data.frame(domains)||!all(c("parameter","task")%in%names(domains))||!setequal(paste(domains$parameter,domains$task),paste(expected$parameter,expected$task))||nrow(domains)!=27L)
    .mp_abort("E_REFERENCE_DOMAIN","Exp1 reference blueprint requires exactly the canonical27-key M3 domain")
  Q3<-.mp_reference_helmert(3L);Q4<-.mp_reference_helmert(4L)
  par<-Q3[rep(1:3,each=3),,drop=FALSE];con<-Q3[rep(1:3,3),,drop=FALSE]
  interaction<-cbind(par[,1]*con[,1],par[,1]*con[,2],par[,2]*con[,1],par[,2]*con[,2])
  families<-list()
  for(i in 1:3) {
    rows<-(i-1L)*9L+1:9
    families[[c("activation","context","retrieval")[i]]]<-list(keys=paste(model,expected$parameter[rows],expected$task[rows],sep="::"),
      paradigm=par,content=con,interaction=interaction,paradigm_loadings=sqrt(3/2)*par,content_loadings=sqrt(3/2)*con)
  }
  s<-component_covariance_structure(families,Q4,name,
    "CSMU Exp1 G/P/C equations; source Stan SHA256 3d780d6bc47d2df829a2d63ab3efe116b779ccacc600f70572570b582ea72629; contrast source SHA256 2a8abbb55230e8ee342f3f9d9eb34352c940cf6cc199fca1cd36fbea4db41c4a")
  s$reference_contract_hash<-.mp_hash(list(s$families,s$share_contrast,s$keys,s$version))
  s$reference_name<-name;s$reference_model<-model;s$reference_dimension_order<-expected
  s
}
reference_priors <- function(name, domains, model) {
  s<-reference_structure(name,domains,model);domain<-s$reference_dimension_order
  mean<-stats::setNames(lapply(domain$parameter,function(p)if(p%in%c("a","c"))normal(1,.4)else logistic(0,1)),s$keys)
  component_prior_set(mean,student_t(3,0,2.5),normal(0,1.5),normal(0,.3),normal(0,.3),normal(0,.15),
    cross=list(context=normal(0,.2),retrieval=normal(0,.14)),source="Explicitly selected exact CSMU Exp1 memory G/P/C reference priors; no CFA/LKJ substitution")
}
csmu_gpc <- function(domains, model, version="csmu_exp1_memory_gpc_v1") {
  if(missing(domains)||missing(model)) .mp_abort("E_PARAMETER_DOMAIN","provide explicit full-M3 domains/model to select the versioned reference structure")
  reference_structure(version,domains,model)
}
