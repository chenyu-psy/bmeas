source(file.path("helpers", "native-guard.R"))
# Focused planning-review amendments; R only, no generated program or backend.
source_root<-Sys.getenv("MEASUREMENTPKG_SOURCE");source_mode<-nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,"R"),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,"bmeas")
replace<-function(n,value)if(source_mode)assign(n,value,.GlobalEnv)else assignInNamespace(n,value,"bmeas")
local({
  terminal<-c("stancode","standata",".bh_backend_ir",".bh_materialize_init",".bh_compile_scalar",
    ".bh_cache_scalar_fit",".mp_toolchain_fingerprint",".bh_provider_runtime_info",".bh_provider_preflight","compile_measurement","fit_measurement")
  original<-lapply(terminal,private);names(original)<-terminal;calls<-character()
  on.exit(for(n in terminal)replace(n,original[[n]]),add=TRUE)
  for(n in terminal)replace(n,local({name<-n;function(...) {calls<<-c(calls,name);stop(paste("forbidden planning boundary",name))}}))
  passed<-0L;callbacks<-0L
  ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat("PASS",label,"\n")}
  err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,"measurement_error")&&identical(e$code,code),code)}
  method<-"anyNA.resource_names_probe";old_method<-get0(method,.GlobalEnv,inherits=FALSE)
  assign(method,function(...) {callbacks<<-callbacks+1L;stop("names callback")},.GlobalEnv)
  on.exit(if(is.null(old_method))rm(list=method,envir=.GlobalEnv)else assign(method,old_method,.GlobalEnv),add=TRUE)
  policy<-list(max_output_bytes=2^40,cache="deferred")
  bad<-policy;attr(bad,"names")<-structure(names(policy),class="resource_names_probe")
  err(private(".bh_resource_policy")(bad),"E_RESOURCE_POLICY")
  malformed_name<-rawToChar(as.raw(c(0xc3,0x28)));Encoding(malformed_name)<-"UTF-8"
  bad<-policy;names(bad)[1]<-malformed_name
  err(private(".bh_resource_policy")(bad),"E_RESOURCE_POLICY")
  for(bad in list(alist(max_output_bytes=,cache="deferred"),alist(max_output_bytes=4294967296,cache=),
    alist(max_output_bytes=4294967296,cache="deferred",max_targets=)))
    err(private(".bh_resource_policy")(bad),"E_RESOURCE_POLICY")
  d<-data.frame(person=1:4,x1=c(.2,.8,.3,1),x2=c(.7,.6,-.3,.1),x3=c(-.2,.6,.9,.4))
  s<-bmeas(bmf(G=~x1+x2+x3),d,subject="person",fit=FALSE)
  plan<-function(spec=s,...)suppressWarnings(execution_plan(spec,chains=1,iter=10,warmup=5,resources=policy,...))
  ctl<-list(adapt_delta=.9);attr(ctl,"names")<-structure("adapt_delta",class="resource_names_probe")
  err(plan(control=ctl),"E_RUN_CONFIG")
  for(ctl in list(alist(adapt_delta=),alist(max_treedepth=),alist(adapt_delta=.9,max_treedepth=)))err(plan(control=ctl),"E_RUN_CONFIG")
  ok(callbacks==0L,"classed names and empty fields never invoke methods")
  # Fixed predictor aliases count toward resource max_targets, while the
  # unchanged PPC geometry ceiling counts only the free eta target dimension.
  long<-expand.grid(person=1:4,task=paste0("x",1:13),stringsAsFactors=FALSE);long$y<-sin(seq_len(nrow(long))*.3)
  block<-observe(gaussian_model(),long,"person","task","y",units="points",parameters=list(mu=list(fixed=0),sigma=list(fixed=1)))
  fixed<-bmeas(bmf(),list(score=block),fit=FALSE);p<-plan(fixed)
  ok(p$dimensions$logical_targets==13&&p$dimensions$J==0&&p$postprocessing_support$ppc$status=="configured_limits_not_exceeded","PPC free-target cap stays distinct from thirteen logical fixed aliases")
  p<-suppressWarnings(execution_plan(fixed,chains=1,iter=10,warmup=5,resources=c(policy,list(max_targets=12))))
  ok(!p$resource_admission$allowed&&any(vapply(p$resource_admission$findings,function(f)identical(f$quantity,"max_targets"),logical(1))),"resource max_targets still counts fixed aliases")
  # Genuine two-label affine lowering gap, prepared through public syntax.
  d<-data.frame(person=rep(1:8,2));for(j in 1:6)d[[paste0("x",j)]]<-sin(seq_len(16)*j/9)+seq_len(16)/7
  multi<-bmeas(bmf(A=~x1+x2,B=~x3+x4,C=~x5+x6,
    cor=list(default="free",fixed=list(list(pairs=~A:B,value=.2)))),d,subject="person",fit=FALSE)
  before<-serialize(multi,NULL,version=2);p<-plan(multi,execution="experimental")
  ok(p$dimensions$J==6&&p$dimensions$K==3&&p$output_estimate$saved_states==5,"absent lowering retains geometric and configured counts")
  ok(identical(p$output_estimate$status,"unavailable_lowering")&&is.na(p$output_estimate$columns_per_saved_state)&&
    is.na(p$output_estimate$estimated_csv_bytes)&&is.null(p$output_estimate$inventory),"absent lowering does not invent emitted fields or bytes")
  ok(!p$resource_admission$allowed&&!p$execution_allowed&&p$scientific_eligibility$finding$code=="E_CORRELATION_DIMENSION_UNSUPPORTED","resource allowance does not authorize a nonexistent program")
  ok(p$cache_estimate$status=="deferred"&&p$cache_estimate$estimated_draw_values==0&&
    is.na(p$cache_estimate$listed_materialization_values)&&!nrow(p$cache_estimate$phases),"unknown emitted width is safe in deferred cache audit")
  ok(p$provider_metadata$status=="unavailable_lowering"&&p$provider_metadata$headers$status=="unavailable"&&
    is.na(p$provider_metadata$inverse_metric$scalar_slots),"absent lowering cannot fabricate provider metadata allocations")
  text<-capture.output(print(p))
  ok(any(grepl("Output estimate: unavailable",text,fixed=TRUE))&&length(p$postprocessing_support$ppc$estimate_risks)>0,"compact print and consumer estimates preserve unavailable output")
  ok(identical(before,serialize(multi,NULL,version=2)),"unavailable lowering does not reseal specification")
  # A legal raw-iid source route without a runtime profile is a different case.
  raw<-bmeas(bmf(A=~x1+x2,B=~x3+x4,
    cor=list(default="free",coordinates=list(type="ordered_blocks",blocks=list(a=~A,b=~B)))),
    d[c("person","x1","x2","x3","x4")],subject="person",prior=c(set_prior("normal(0,1)","Intercept"),
      set_prior("normal(0,.5)","loading"),set_prior("half_normal(1)","sigma"),set_prior("normal(0,.25)","cor_raw")),fit=FALSE)
  p<-plan(raw,execution="experimental")
  ok(!p$scientific_eligibility$allowed&&p$lowering_support$available&&p$output_estimate$status=="estimated"&&
    is.finite(p$output_estimate$estimated_csv_bytes)&&nrow(p$output_estimate$inventory)>0,"runtime-unqualified legal source keeps meaningful output inventory")
  ok(all(vapply(seq_len(nrow(p$output_estimate$inventory)),function(i)
    prod(p$output_estimate$inventory$dimensions[[i]])==p$output_estimate$inventory$elements[i],logical(1))),"count-only dimension ranks preserve emitted group counts")
  ok(length(calls)==0L&&callbacks==0L,"all generator/backend traps and callback counters untouched")
  cat("PASS",passed,"planning boundary amendment controls\n")
})
