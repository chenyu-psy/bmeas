source(file.path("helpers", "native-guard.R"))
# No MCMC. Reference-specific probes run only with explicit external resource paths.
if(Sys.getenv("MEASUREMENTPKG_SOURCE")!="")for(f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"),"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
passed<-0L
ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat("PASS:",label,"\n")}
err<-function(expr,code){r<-tryCatch({force(expr);NULL},error=identity);ok(inherits(r,"measurement_error")&&identical(r$code,code),code)}
internal<-function(name)if(exists(name))get(name)else getFromNamespace(name,"bmeas")
# Original local tutorial model demonstrates custom registration without special family dispatch.
root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
example<-if(nzchar(root))file.path(root,"inst/examples/custom-registry.R")else system.file("examples","custom-registry.R",package="bmeas")
code<-readLines(example)
if(nzchar(root))code<-sub('file <- system.file.*','file <- file.path(root,"inst/stan/two_afc_example.stanfunctions")',code)
eval(parse(text=code),envir=environment())
ok(inherits(model,"observation_model")&&model$backend=="registered_stan","user-defined likelihood becomes an executable typed adapter")
ok(is.null(model$likelihood$source),"registered external source bytes are not embedded in the model")
ok(stan_data(spec)$J==3L&&stan_data(spec)$O==6L&&stan_data(spec)$S==0L,"custom scalar model reuses the same hierarchy and key compiler")
ok(grepl("example_two_afc_lpmf",as.character(make_stan_code(spec)),fixed=TRUE),"custom density call is emitted without a hard-coded model-name branch")
err(stan_resource(file,paste(rep("0",64),collapse=""),"example_two_afc_lpmf","MIT","test"),"E_RESOURCE_HASH")
err(register_stan_adapter(model,function_code="real x(){return 0;}\n#include hidden.stanfunctions",function_exports="x"),"E_ADAPTER")
err(register_stan_adapter(model,function_code="} data { int x; } functions {",function_exports="x"),"E_ADAPTER_SCOPE")
bad<-model;bad$adapter$function_code<-"real changed(){return 0;}";err(internal(".mp_validate_adapter")(bad),"E_ADAPTER_HASH")
constant<-stan_constant(matrix(0,2,2),"matrix","unitless","test")
err(new_observation_model("bad","1",response_schema(y="binary"),model$parameters,model$likelihood,
  constants=stats::setNames(list(constant),"x; injected")),"E_SCHEMA_NAME")
err(stan_constant(matrix(0,201,1),"matrix","unitless","test"),"E_CONSTANT")
err(data_field("real",width=101),"E_SCHEMA")
# Fixed-width response validation never flattens a row into independent subjects.
wide_model<-new_observation_model("wide","1",response_schema(y=data_field("nonnegative_integer",width=3)),
  list(eta=parameter_spec("real","identity","eta","subject_task","u","eta")),
  stan_density(symbol="wide_lpmf",arguments=c("y","eta"),argument_types=c("int_array","real"),normalized=TRUE,approximation="none",builtin=TRUE))
d<-data.frame(person=c("a","b"),task="t");d$y<-matrix(1,2,3)
b<-observe(wide_model,d,"person","task",fields=list(y="y"),units="count")
ok(is.matrix(b$data$y)&&identical(dim(b$data$y),c(2L,3L)),"fixed-width data remain one matrix-valued column per observation")
d$y<-matrix(1,2,2);err(observe(wide_model,d,"person","task",fields=list(y="y"),units="count"),"E_FIELD_DIMENSION")
d$y<-matrix(1,2,3);d$y[2,3]<--1;err(observe(wide_model,d,"person","task",fields=list(y="y"),units="count"),"E_FIELD_SUPPORT")
m3<-Sys.getenv("MEASUREMENTPKG_M3_RESOURCE");sdm<-Sys.getenv("MEASUREMENTPKG_SDM_RESOURCE")
if(nzchar(m3)&&nzchar(sdm)) {
  cognitive<-demo_cognitive_wiring_spec(m3,sdm)
  ok(length(cognitive$models)==2&&length(cognitive$data_map$subject_keys)==4&&stan_data(cognitive)$J==6,"two cognitive blocks share four synthetic people and six parameter indicators")
  ok(nrow(model_resource_manifest(cognitive))==2,"external cognitive resources have separate pin/license/source records")
  ok(all(vapply(cognitive$models,function(b)!b$model$capabilities$available[b$model$capabilities$capability=="posterior_predict"],logical(1))),"cognitive simulators/PPC are not invented")
  ok(all(vapply(cognitive$models,function(b)is.null(b$model$likelihood$source),logical(1))),"cognitive objects retain paths/hashes, no external code payload")
  err(m3_cs_model(m3,expected_sha256=paste(rep("0",64),collapse="")),"E_MODEL_VERSION")
  err(sdm_reference_model(sdm,expected_sha256=paste(rep("0",64),collapse="")),"E_MODEL_VERSION")
  md<-cognitive$models$memory$data;md$counts[1,5]<-1
  err(cognitive$models$memory$model$validate_data(md),"E_M3_BRANCH")
  md<-cognitive$models$memory$data;md$options[1,1]<-0
  err(cognitive$models$memory$model$validate_data(md),"E_M3_OPTIONS")
  rd<-cognitive$models$report$data;rd$run[]<-1
  err(cognitive$models$report$model$validate_data(rd),"E_SDM_RUN")
  bad<-cognitive;bad$data_map$blocks$memory$counts[1,5]<-1;err(check_measurement(bad),"E_M3_BRANCH")
  bad<-cognitive;bad$data_map$blocks$report$error[1]<-4;err(check_measurement(bad),"E_FIELD_SUPPORT")
  bad<-cognitive;bad$data_map$blocks$report$error[1]<-.01;err(check_measurement(bad),"E_DATA_CHANGED")
  ok(cognitive$models$report$model$constants$CN$value[1,1]==cos(pi/2)-1,"reference SDM cosine grid is preserved")
  ok(identical(cognitive$checks$identification$status,"not_assessed"),"six jointly observed indicators are not claimed statistically identified")
} else cat("SKIP: reference-specific probes require explicitly provided external resources\n")
cat("TOTAL",passed,"registry checks passed; no compilation or MCMC\n")
