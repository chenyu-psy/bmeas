# Explicit stage selector: compile first; sample only after independent fixed-point approval.
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<4L)stop("Arguments: compile|sample package_root m3_resource sdm_resource [output]")
action<-match.arg(args[1],c("compile","sample"));root<-normalizePath(args[2])
for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)
spec<-demo_cognitive_wiring_spec(args[3],args[4])
out<-if(length(args)>4L)args[5]else file.path(dirname(root),"bhmm-registry-validation")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(action=="sample") {
  frozen<-readRDS(file.path(out,"spec.rds"))
  stopifnot(identical(stan_data(frozen),stan_data(spec)),identical(as.character(make_stan_code(frozen)),as.character(make_stan_code(spec))))
  spec<-frozen # Sample the exact compiled and independently checked scientific specification.
}
run<-run_config(1,1,1,10,10,20261003,1,24,4,10)
saveRDS(spec,file.path(out,"spec.rds"));saveRDS(stan_data(spec),file.path(out,"stan-data.rds"))
utils::write.csv(prior_table(spec),file.path(out,"prior-table.csv"),row.names=FALSE)
utils::write.csv(model_resource_manifest(spec),file.path(out,"external-resource-manifest.csv"),row.names=FALSE)
writeLines(spec$example_warning,file.path(out,"SCOPE.txt"))
if(action=="compile") {
  compiled<-compile_measurement(spec,file.path(out,"compile"))
  writeLines(compiled$model$exe_file(),file.path(out,"executable-path.txt"))
  saveRDS(compiled$toolchain,file.path(out,"toolchain.rds"))
  cat("PASS: registry cognitive adapter compilation; no sampling performed\n")
} else {
  fit<-fit_measurement(spec,run,file.path(out,"compile"),file.path(out,"sampling"))
  stopifnot(all(fit$backend$return_codes()==0L),fit$cache$computation$status!="failed",fit$cache$predictive_fit$status=="unsupported")
  text<-capture.output(print(summary(fit)));copy<-fit;copy$backend<-NULL
  stopifnot(identical(text,capture.output(print(summary(copy)))))
  writeLines(text,file.path(out,"smoke-summary.txt"));saveRDS(fit,file.path(out,"smoke-fit.rds"))
  utils::write.csv(parameter_summary(fit),file.path(out,"parameter-summary.csv"),row.names=FALSE)
  cat(text,sep="\n");cat("\nPASS: joint M3-CS + SDM numerical execution only; no PPC or scientific factor claim\n")
}
