# Genuine public declarations and fit lifecycles with terminal R-only doubles.
# Caller sources residual-product-fixtures.R first. No native model is built.
product_archive_private <- function(name) {
  root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
  if(nzchar(root)) get(name,.GlobalEnv) else getFromNamespace(name,"bmeas")
}
product_archive_specs <- function() {
  d <- data.frame(person=rep(1:4,2))
  for(j in 1:6)d[[paste0("x",j)]] <- sin(seq_len(8)*j/7)+seq_len(8)/9
  common <- bmeas(bmf(A=~x1,B=~x2,C=~x3,D=~x4,E=~x5,F=~x6,
    cor=list(default="zero",free=list(~D:F),equal=list(shared=list(~A:C,~B:C)),
      fixed=list(list(pairs=~A:B,value=.2),list(pairs=~D:E,value=.3)))),
    d,subject="person",fit=FALSE)
  c(list(common_product=common),residual_product_fixtures())
}
product_archive_named <- function(name,x) {
  if(!length(x))return(numeric())
  keys <- if(is.matrix(x))paste0(name,"[",rep(seq_len(nrow(x)),ncol(x)),",",
    rep(seq_len(ncol(x)),each=nrow(x)),"]") else paste0(name,"[",seq_along(x),"]")
  stats::setNames(as.numeric(x),keys)
}
product_archive_rows <- function(spec,chain=1L,private=product_archive_private) {
  J <- nrow(spec$tables$targets);K <- nrow(spec$tables$components)
  U <- nrow(spec$tables$parameter_residuals);N <- length(spec$data_map$subject_keys)
  layout <- private(".bh_covariance_layout")(spec);ix <- layout$indices
  rows <- lapply(seq_len(4L),function(i) {
    values <- rep(.4+.02*i,sum(layout$counts));values[ix$observation_variance] <- 1
    cr <- rep(.04*i+.01*chain,length(ix$component_cor))
    ur <- rep(-.06*i-.01*chain,length(ix$parameter_cor))
    values[ix$component_cor] <- private(".bh_affine_layer_state")(spec,"component",cr)$scientific_values
    values[ix$parameter_cor] <- private(".bh_affine_layer_state")(spec,"parameter",ur)$scientific_values
    st <- private(".bh_covariance_state")(spec,values)
    L <- if(K)t(chol(st$R_component))else matrix(numeric(),0L,0L)
    Lu <- if(U)t(chol(st$R_parameter))else matrix(numeric(),0L,0L)
    raw <- matrix(.2*sin(seq_len(K*N)+i+chain/10),K,N)
    eraw <- matrix(.2*cos(seq_len(U*N)+i+chain/10),U,N)
    z <- L%*%raw;e <- Lu%*%eraw
    alpha <- seq_len(J)/10+i/20+chain/100
    eta <- matrix(rep(alpha,each=N),N,J)+t(z)%*%t(st$effective_loadings)+t(e)%*%t(st$D_parameter)
    f <- sweep(z,1,st$component_sd,`*`)
    fields <- list(alpha=alpha,eta=eta,loading=values[ix$loading],component_sd=values[ix$component_sd],
      obs_sd=sqrt(values[ix$observation_variance]),parameter_sd=values[ix$parameter_sd],
      parameter_row_sd=st$parameter_sd,component_row_sd=st$component_sd,
      correlation_raw=cr,parameter_correlation_raw=ur,L_component=L,R_component=st$R_component,
      L_parameter=Lu,R_parameter=st$R_parameter,z_raw=raw,e_raw=eraw,z=z,f=f,
      effective_loadings=st$effective_loadings,D_parameter=st$D_parameter,
      component_covariance=st$component_covariance,log_component_row_sd=st$log_component_sd,
      component_covariance_log_abs=st$component_covariance_log_abs,
      f_log_abs=sweep(log(abs(z)),1,st$log_component_sd,`+`),
      Sigma_common_available=matrix(1,J,J),Sigma_parameter_available=matrix(1,J,J),
      Sigma_predictor_available=matrix(1,J,J),
      component_covariance_available=matrix(as.integer(!st$component_covariance_unavailable),K,K),
      f_available=matrix(1,K,N))
    c(unlist(lapply(names(fields),function(n)product_archive_named(n,fields[[n]])),use.names=TRUE),
      divergent__=0,treedepth__=2,energy__=c(1,2,4,3)[i]+chain/10)
  })
  do.call(rbind,rows)
}
product_archive_write_csv <- function(path,rows,chain,warmup=4L) {
  lines <- apply(rows,1,function(x)paste(sprintf("%.17g",x),collapse=","))
  writeLines(c("# ENGINEERING SYNTHETIC STATES ONLY; NO NATIVE MODEL OR SAMPLING",
    "# stan_version_major = 2","# stan_version_minor = 40","# stan_version_patch = 0",
    "# model = scalar_components_model",paste0("# id = ",chain),"# method = sample (Default)",
    paste0("# num_samples = ",nrow(rows)),paste0("# num_warmup = ",warmup),
    "# save_warmup = false (Default)","# thin = 1","# algorithm = hmc (Default)",
    "# engine = nuts (Default)","# metric = diag_e (Default)","# max_depth = 4","# sig_figs = 17",
    paste(gsub("\\]","",gsub("[\\[,]",".",colnames(rows))),collapse=","),lines,
    "# Elapsed Time: 0 seconds (Warm-up)","# 0 seconds (Sampling)","# 0 seconds (Total)"),path)
}
# Return fit, chain_rows, paths and mutable call counters. Only .bh_compile_scalar
# and provider runtime discovery are replaced, then restored before returning.
product_archive_fit <- function(spec,directory,deferred=FALSE,private=product_archive_private) {
  ns <- if(nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))) .GlobalEnv else asNamespace("bmeas")
  replacements <- c(".bh_compile_scalar",".bh_provider_runtime_info")
  old <- mget(replacements,ns);locked <- vapply(replacements,bindingIsLocked,logical(1),env=ns)
  put <- function(n,x) {if(bindingIsLocked(n,ns))unlockBinding(n,ns);assign(n,x,ns);if(locked[[n]])lockBinding(n,ns)}
  on.exit(for(n in replacements)put(n,old[[n]]),add=TRUE)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  rows <- lapply(1:2,function(chain)product_archive_rows(spec,chain,private))
  draws <- do.call(rbind,rows);calls <- new.env(parent=emptyenv())
  calls$compile <- calls$sample <- calls$draws <- calls$summary <- calls$diagnostics <- calls$output <- 0L
  paths <- character()
  select <- function(variables)unique(unlist(lapply(variables,function(v)
    if(v%in%colnames(draws))v else colnames(draws)[startsWith(colnames(draws),paste0(v,"["))]),use.names=FALSE))
  backend <- list(return_codes=function()c(0L,0L),output_files=function()paths,
    output=function(){calls$output<-calls$output+1L;character()},
    diagnostic_summary=function(quiet){calls$diagnostics<-calls$diagnostics+1L;list(num_divergent=c(0L,0L),num_max_treedepth=c(0L,0L),ebfmi=c(.4,.4))},
    draws=function(variables,format){calls$draws<-calls$draws+1L;draws[,select(variables),drop=FALSE]},
    summary=function(variables,...) {
      calls$summary<-calls$summary+1L
      do.call(rbind,lapply(select(variables),function(v){x<-draws[,v];data.frame(variable=v,
        mean=mean(x),median=median(x),sd=sd(x),q5=unname(quantile(x,.05)),q95=unname(quantile(x,.95)),
        rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.5)}))
    })
  put(".bh_provider_runtime_info",function()list(available=TRUE,os_type="unix",wsl=FALSE,
    cmdstanr_version="0.9.0",cmdstan_version="2.40.0"))
  put(".bh_compile_scalar",function(spec,cache_dir,save_log_lik) {
    calls$compile<-calls$compile+1L
    list(model=list(sample=function(...) {
      calls$sample<-calls$sample+1L;args<-list(...)
      stopifnot(args$chains==2L,args$iter_sampling==4L,args$sig_figs==17L)
      prefix<-if(is.null(args$output_basename))"synthetic"else args$output_basename
      paths<<-file.path(args$output_dir,paste0(prefix,"-",1:2,".csv"))
      for(i in 1:2)product_archive_write_csv(paths[i],rows[[i]],i,args$iter_warmup)
      backend
    }),identity=list(code=as.character(stancode(spec,threads=2L,log_lik=save_log_lik)),
      toolchain=list(cmdstan=character(),cmdstan_path=character(),cmdstanr=character(),R=character(),
        platform=character(),compiler=character(),compiler_version=character(),environment=character(),
        make_local=character(),cpp_options=list(),stanc_options=list())),
      cache_key="synthetic_terminal_double",cache_hit=FALSE,executable_sha256="no_native_executable")
  })
  resources <- if(!deferred)"development"else list(max_output_bytes=2^24,cache="deferred",cpu_budget=1,
    max_subjects=4,max_observations=100,max_saved_draws=8,max_targets=8,
    max_observation_scales=8,max_latent_coordinates=16,max_coefficient_primitives=32)
  fit <- suppressWarnings(bmeas(spec,chains=2,iter=8,warmup=4,cores=1,threads=1,
    control=list(max_treedepth=4,csv_sig_figs=17),execution="experimental",resources=resources,
    output_dir=directory))
  stopifnot(calls$compile==1L,calls$sample==1L,identical(fit$provenance$output_files,paths))
  list(fit=fit,chain_rows=rows,paths=paths,calls=calls)
}
