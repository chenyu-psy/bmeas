source(file.path("helpers", "native-guard.R"))
source("frontend-predict-m3cs.R")
# Mutated saved classes must never execute accessors or reseal identities.
local({
  calls<-0L
  for(method in c("$","[","[[","as.list","length","names"))
    assign(paste0(method,".m3_callback"),function(...){calls<<-calls+1L;stop("unexpected saved callback")},.GlobalEnv)
  on.exit(rm(list=paste0(c("$","[","[[","as.list","length","names"),".m3_callback"),envir=.GlobalEnv))
  change<-function(x,path) {
    if(!length(path)){class(x)<-c("m3_callback",class(x));return(x)}
    x[[path[1L]]]<-change(x[[path[1L]]],path[-1L]);x
  }
  basic<-list(models=list(score=list(model=gaussian_model())))
  for(path in list(c("models"),c("models","score"),c("models","score","model"),
      c("models","score","model","data_schema"),c("models","score","model","parameters"),
      c("models","score","model","likelihood"))) {
    bad<-change(basic,path);before<-serialize(bad,NULL)
    stopifnot(!private(".bh_m3cs_spec")(bad),calls==0L,identical(before,serialize(bad,NULL)))
  }
  cat("PASS basic family and nested classifier containers avoid saved callbacks and mutation\n")
  if(exists("s",envir=.GlobalEnv)) {
    paths<-list(c("models","cs"),c("models","cs","model"))
    for(tail in list(c("data_schema"),c("data_schema","counts"),c("parameters"),c("parameters","a"),
        c("likelihood"),c("adapter"),c("adapter","resources"),c("adapter","resources","1")))
      paths[[length(paths)+1L]]<-c("models","cs","model",tail)
    # Resource list is unnamed: use the numeric position for its final element.
    change<-function(x,path) {
      if(!length(path)){class(x)<-c("m3_callback",class(x));return(x)}
      key<-if(path[1L]=="1")1L else path[1L]
      x[[key]]<-change(x[[key]],path[-1L]);x
    }
    for(path in paths) {
      bad<-fit;bad$spec<-change(s,path);before<-serialize(bad,NULL)
      stopifnot(private(".bh_m3cs_spec")(bad$spec),calls==0L)
      planned<-ppc_plan(bad,2,budget,level="conditional")
      got<-tryCatch(assess_measurement_fit(bad,level="conditional",draws=2,seed=1,budget=budget),error=identity)
      stopifnot(inherits(planned,"bmeas_ppc_plan"),inherits(got,"measurement_error"),
        identical(got$code,"E_SPEC_CHANGED"),calls==0L,identical(before,serialize(bad,NULL)))
    }
    cat("PASS ten M3 block/model/nested signature mutations retain passive plans and refuse E_SPEC_CHANGED with zero callbacks/resealing\n")
  }
})
