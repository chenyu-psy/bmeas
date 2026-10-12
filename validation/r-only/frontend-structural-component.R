source(file.path("helpers", "native-guard.R"))
# End-to-end saved-reader bridge with synthetic engineering CSV, never a fit.
root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root))for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
local({
 passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat("PASS",label,"\n")}
 d<-data.frame(person=as.character(1:8))
 for(j in 1:6)d[[paste0("x",j)]]<-sin((1:8)*j/4)+j/10
 spec<-bmeas(bmf(A=~x1+x2+x3,B=~x4+x5+x6,scale=list(B=list(marker=~x4,value=-2))),d,subject="person",fit=FALSE)
 path<-tempfile(fileext=".csv");on.exit(unlink(path),add=TRUE)
 vars<-c("R_component.1.1","R_component.2.1","R_component.1.2","R_component.2.2",
   "component_row_sd.1","component_row_sd.2","component_covariance_available.1.1",
   "component_covariance_available.2.1","component_covariance_available.1.2","component_covariance_available.2.2")
 writeLines(c("# ENGINEERING SYNTHETIC FIXTURE ONLY; no posterior inference", "# stan_version_major = 2",
  "# stan_version_minor = 40","# stan_version_patch = 0","# model = scalar_components_model","# id = 7",
  "# method = sample (Default)","# num_samples = 3","# num_warmup = 0","# save_warmup = false (Default)",
  "# thin = 1","# algorithm = hmc (Default)","# engine = nuts (Default)","# metric = diag_e (Default)",
  "# max_depth = 4","# sig_figs = 17",paste(vars,collapse=","),
  "1,0.5,0.5,1,1,2,1,1,1,1","1,0.25,0.25,1,1,4,1,1,1,1","1,0,0,1,1,2,1,0,1,1",
  "# Elapsed Time: 0 seconds (Warm-up)","# 0 seconds (Sampling)","# 0 seconds (Total)"),path)
 backend<-new.env(parent=emptyenv())
 makeActiveBinding("do_not_inspect",function(...)stop("FORBIDDEN backend access"),backend)
 fit<-structure(list(spec=spec,run=structure(list(chains=1L,csv_sig_figs=17L),class="measurement_run_config"),
  backend=backend,cache=list(inference=list(status="not_assessed",reason="Synthetic fixture")),
  provenance=list(output_files=path,model_fingerprint=spec$fingerprint,csv_sig_figs=17L,
    kind="engineering_synthetic_fixture_no_posterior_qualification")),class=c("bmeas_fit","bhmm_fit"))
 original<-component_draws(fit)
 native<-component_population_draws(fit)
 ok(identical(native$upstream,original),"public adapter retains exact component extraction")
 ok(identical(native$covariance,original$covariance)&&identical(native$draw_keys,original$draw_keys),"reader preserves covariance and chain rows")
 ok(all(native$draw_keys$chain_id==7)&&identical(native$draw_keys$saved_row,1:3),"source chain 7 remains authoritative")
 ids<-spec$tables$coordinates$id
 paths<-structural_draws(native,ids[1],ids[2])
 ok(max(abs(paths$posterior$coefficients[1:2,,]-c(1,1)))<1e-12,"same-row variable SD changes respected")
 ok(max(abs(paths$posterior$residual_covariance[1:2,,]-c(3,15)))<1e-12,"marker-scale residual covariance")
 ok(paths$posterior$draw_status$status[3]=="unavailable_covariance","saved unavailable cell not repaired even at numerical zero")
 ok(summary(paths)$posterior$status=="unavailable_incomplete_mapping","incomplete real-reader output not filtered")
 ok(identical(native$upstream$upstream$current_cache$statuses$inference$status,"not_assessed"),"inference evidence not upgraded")
 ok(native$parameterization$maps$component_scales$marker_value[2]== -2,"negative marker declaration preserved")
 ok(all(native$direction_resolved),"positive anchor and negative fixed marker resolve declared axes")
 fit$cache$posterior_propriety<-list(status="proven_improper",reason="Synthetic adverse evidence")
 e<-tryCatch(component_population_draws(fit),error=identity)
 ok(inherits(e,"measurement_error")&&identical(e$code,"E_COMPONENT_POSTERIOR"),"original improper evidence still blocks extraction")
 cat("PASSED",passed,"public component structural bridge checks; synthetic CSV only\n")
})
