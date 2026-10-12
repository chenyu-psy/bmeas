source(file.path("helpers", "native-guard.R"))
# Bounded R-only resource-policy controls. No native model, backend IR,
# generated source/data, dense person initialization, posterior reader or cache.
source_root<-Sys.getenv("MEASUREMENTPKG_SOURCE");source_mode<-nzchar(source_root)
if(source_mode)for(f in list.files(file.path(source_root,"R"),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,"bmeas")
replace<-function(n,value)if(source_mode)assign(n,value,.GlobalEnv)else assignInNamespace(n,value,"bmeas")
local({
  terminal<-c("stancode","standata",".bh_backend_ir",".bh_materialize_init",".bh_compile_scalar",
    ".bh_cache_scalar_fit",".mp_toolchain_fingerprint",".bh_provider_runtime_info",".bh_provider_preflight","compile_measurement","fit_measurement")
  old<-lapply(terminal,private);names(old)<-terminal;calls<-character()
  on.exit(for(n in terminal)replace(n,old[[n]]),add=TRUE)
  for(n in terminal)replace(n,local({name<-n;function(...) {calls<<-c(calls,name);stop(paste("forbidden planning boundary",name))}}))
  passed<-0L
  ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat("PASS",label,"\n")}
  err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,"measurement_error")&&identical(e$code,code),code)}
  d<-data.frame(person=1:4,x1=c(.2,.8,.3,1),x2=c(.7,.6,-.3,.1),x3=c(-.2,.6,.9,.4))
  s<-bmeas(bmf(G=~x1+x2+x3),d,subject="person",fit=FALSE)
  identity_before<-serialize(s,NULL,version=2)
  policy<-list(max_output_bytes=2^32,cache="deferred")
  p<-execution_plan(s,chains=4,iter=2000,warmup=1000,cores=4,resources=policy)
  ok(p$execution_allowed&&p$resource_admission$status=="admitted","conventional four-chain request admitted with explicit finite policy")
  ok(p$output_estimate$saved_states==4000&&p$configured_transitions==8000&&p$output_estimate$saved_warmup_states==0,"total iteration and configured saved-state meanings")
  ok(p$concurrency$simultaneous_workers==4&&p$run$parallel_chains==4,"four actual parallel workers")
  ok(p$cache_estimate$status=="deferred"&&p$cache_estimate$estimated_draw_values==0,"deferred requests zero posterior materialization")
  ok(p$provider_metadata$provider$runtime_compatibility=="unverified"&&p$provider_metadata$provider$version=="0.9.0"&&
    p$provider_metadata$provider$cmdstan_version=="2.40.0","provider scope is expected metadata without runtime discovery")
  ok(p$provider_metadata$headers$name_entries_all_chains==4*p$output_estimate$columns_per_saved_state&&
    p$provider_metadata$inverse_metric$scalar_slots==4*p$dimensions$total_sampled_parameter_dimension&&
    is.na(p$provider_metadata$full_file_scan$actual_bytes),"provider header and diagonal metric work is separate from unknown actual scan bytes")
  ok(p$provider_metadata$deferred_materialization_status=="deferred_requested"&&
    identical(p$provider_metadata$deferred_posterior_draw_scalar_slots,0),"zero deferred draw slots requires an actual deferred request")
  ok(p$postprocessing_support$ppc$status=="streaming_policy_required"&&p$postprocessing_support$individual$status=="streaming_policy_required"&&
    p$postprocessing_support$ppc$legacy_reader$status=="configured_layout_incompatible"&&
    p$postprocessing_support$individual$legacy_reader$status=="configured_layout_incompatible",
    "legacy reader limits and an explicit unverified streaming policy are distinguished")
  printed<-capture.output(print(p));ok(length(printed)<20&&any(grepl("unobserved",printed,fixed=TRUE)),"plan prints compact configured evidence")
  ok(any(grepl("CmdStanR 0.9.0 / CmdStan 2.40.0",printed,fixed=TRUE))&&
    any(grepl("POSIX",printed,fixed=TRUE))&&any(grepl("runtime: unverified",printed,fixed=TRUE))&&
    any(grepl("full-file constructor metadata scan",printed,fixed=TRUE)),"printed provider scope discloses unverified runtime and full-file metadata scan")
  # Saved display fields cannot dispatch numeric methods or active bindings.
  callback_calls<-0L;method_name<-"format.bmeas_resource_print_probe"
  old_method<-get0(method_name,envir=.GlobalEnv,inherits=FALSE)
  on.exit(if(is.null(old_method))rm(list=method_name,envir=.GlobalEnv)else assign(method_name,old_method,.GlobalEnv),add=TRUE)
  assign(method_name,function(x,...) {callback_calls<<-callback_calls+1L;"unexpected formatting callback"},.GlobalEnv)
  bad_print<-p;bad_print$output_estimate$estimated_csv_bytes<-structure(1,class="bmeas_resource_print_probe")
  e<-tryCatch(capture.output(print(bad_print)),error=identity)
  ok(inherits(e,"measurement_error")&&e$code=="E_RESOURCE_PLAN_PRINT"&&callback_calls==0L,"classed display counts refused before format callbacks")
  active<-new.env(parent=emptyenv());makeActiveBinding("kind",function() {callback_calls<<-callback_calls+1L;"caller_resources"},active)
  bad_print<-p;bad_print$resource_policy<-active
  e<-tryCatch(capture.output(print(bad_print)),error=identity)
  ok(inherits(e,"measurement_error")&&e$code=="E_RESOURCE_PLAN_PRINT"&&callback_calls==0L,"non-list display containers refused before active bindings")
  malformed<-list(p,p);malformed[[1]]$resource_admission$status<-c("admitted","refused")
  malformed[[2]]$resource_policy<-structure(list("caller_resources","development"),names=c("kind","kind"))
  errors<-lapply(malformed,function(value)tryCatch(capture.output(print(value)),error=identity))
  ok(all(vapply(errors,function(e)inherits(e,"measurement_error")&&e$code=="E_RESOURCE_PLAN_PRINT",logical(1))),"ambiguous display containers and nonscalar statuses refused")
  opaque<-p;opaque$scientific_checks<-list(retained_rich_record=structure(list(note="not a printed field"),class="retained_scientific_record"))
  ok(identical(capture.output(print(opaque)),printed)&&callback_calls==0L,"ordinary display parity preserves unprinted richer scientific records")
  bad_print<-p;bad_print$provider_metadata$headers$name_entries_all_chains<-structure(1,class="bmeas_resource_print_probe")
  e<-tryCatch(capture.output(print(bad_print)),error=identity)
  ok(inherits(e,"measurement_error")&&e$code=="E_RESOURCE_PLAN_PRINT"&&callback_calls==0L,"provider display counts refuse formatting callbacks")
  bad_print<-p;bad_print$provider_metadata$provider<-active
  e<-tryCatch(capture.output(print(bad_print)),error=identity)
  ok(inherits(e,"measurement_error")&&e$code=="E_RESOURCE_PLAN_PRINT"&&callback_calls==0L,"provider display refuses active-binding containers")
  earlier<-p;earlier$provider_metadata<-NULL;earlier_print<-capture.output(print(earlier))
  ok(any(grepl("Provider metadata: not retained",earlier_print,fixed=TRUE))&&
    identical(earlier_print[!grepl("^Provider ",earlier_print)],printed[!grepl("^Provider ",printed)]),"earlier plans disclose missing provider metadata without changing other printed evidence")
  q<-execution_plan(s,chains=4,iter=2000,warmup=1000,cores=1,resources=c(policy,list(cpu_budget=1)))
  ok(q$execution_allowed&&q$concurrency$simultaneous_workers==1,"four sequential chains require one worker")
  q<-execution_plan(s,chains=1,iter=10,warmup=5,cores=100,threads=2,resources=c(policy,list(cpu_budget=2)))
  ok(q$execution_allowed&&q$concurrency$simultaneous_workers==2,"idle requested cores are not charged")
  q<-execution_plan(s,chains=4,iter=2000,warmup=1000,cores=4,resources=c(policy,list(cpu_budget=3)))
  err(private(".bh_resource_fit_gate")(q),"E_RUN_BUDGET")
  q<-execution_plan(s,chains=4,iter=2000,warmup=1000)
  ok(q$resource_admission$status=="resource_policy_required"&&q$output_estimate$saved_states==4000,"missing policy remains inspectable without legacy smoke admission")
  ok(q$provider_metadata$deferred_materialization_status=="uncomputed_policy_missing"&&
    is.na(q$provider_metadata$deferred_posterior_draw_scalar_slots),"missing policy does not imply deferred zero-materialization")
  err(bmeas(s,chains=4,iter=2000,warmup=1000),"E_RESOURCE_POLICY_REQUIRED")
  q<-execution_plan(s,chains=1,iter=10,warmup=5,resources="development")
  ok(q$execution_allowed&&q$resource_policy$kind=="development"&&is.null(q$resource_policy$limits$max_cache_draw_values),"development is explicit historical eager compatibility without an invented new quota")
  ok(q$provider_metadata$deferred_materialization_status=="inapplicable_eager"&&
    is.na(q$provider_metadata$deferred_posterior_draw_scalar_slots)&&
    q$provider_metadata$provider$runtime_compatibility=="not_applied_to_requested_mode","development eager does not inherit the deferred provider pin or zero slots")
  q<-execution_plan(s,chains=4,iter=2000,warmup=1000,resources=list(max_output_bytes=2^32,cache="eager",max_cache_draw_values=2^32))
  err(private(".bh_resource_fit_gate")(q),"E_CACHE_ESTIMATE_UNAVAILABLE")
  ok(is.na(q$cache_estimate$estimated_draw_values)&&nrow(q$cache_estimate$phases)>0,"incomplete eager audit never becomes a certified total")
  q<-execution_plan(s,chains=1,iter=10,warmup=5,resources=c(policy,list(max_cache_draw_values=1)))
  ok(q$resource_admission$allowed&&q$cache_estimate$max_cache_draw_values==1,"deferred retains an inapplicable supplied cache allowance")
  for(bad in list(Inf,NA_real_,-1,.5,2^53,structure(1,class="count"),matrix(1,1)))
    err(execution_plan(s,chains=1,iter=10,warmup=5,resources=list(max_output_bytes=bad,cache="deferred")),"E_RESOURCE_POLICY")
  for(bad in list(structure(list(1,"deferred",2),names=c("max_output_bytes","cache","max_output_bytes")),
    c(policy,list(force=TRUE)),structure(policy,hidden=new.env()),list(max_output_bytes=2^32,cache="eager")))
    err(execution_plan(s,chains=1,iter=10,warmup=5,resources=bad),"E_RESOURCE_POLICY")
  err(execution_plan(s,chains=1,iter=10,warmup=5,resources=policy,control=structure(list(),hidden=new.env())),"E_RUN_CONFIG")
  err(private(".bh_count_product")(2^32,2^32),"E_RESOURCE_ESTIMATE")
  err(private(".bh_count_sum")(c(2^53-1,1)),"E_RESOURCE_ESTIMATE")
  ok(private(".bh_count_sum")(c(2^31,1))==2147483649,"checked aggregate exceeds signed-int range safely")
  changed<-p;changed$run$iter_sampling<-1L
  err(private(".bh_revalidate_plan")(s,changed,FALSE),"E_RESOURCE_PLAN_CHANGED")
  changed<-p;changed$provider_metadata$version<-"different_provider_scope"
  err(private(".bh_revalidate_plan")(s,changed,FALSE),"E_RESOURCE_PLAN_CHANGED")
  err(private(".bh_revalidate_plan")(s,list(run=list(chains=4)),FALSE),"E_RESOURCE_PLAN_CHANGED")
  err(private(".bh_revalidate_plan")(s,p,TRUE),"E_RESOURCE_PLAN_CHANGED")
  ceiling<-list(max_output_bytes=p$output_estimate$estimated_csv_bytes,cache="deferred")
  q<-execution_plan(s,chains=4,iter=2000,warmup=1000,cores=4,resources=ceiling)
  ok(q$resource_admission$allowed,"exact estimated output ceiling admitted")
  ceiling$max_output_bytes<-ceiling$max_output_bytes-1
  q<-execution_plan(s,chains=4,iter=2000,warmup=1000,cores=4,resources=ceiling)
  err(private(".bh_resource_fit_gate")(q),"E_OUTPUT_BUDGET")
  # The domain is unique shared people, while complete consumed rows add.
  tasks<-c("x1","x2","x3")
  long<-expand.grid(person=1:4,task=tasks,stringsAsFactors=FALSE);long$y<-sin(seq_len(nrow(long))*.31)
  block<-observe(gaussian_model(),long,"person","task","y",units="points")
  fm<-bmf(A=~a$mu[tasks],B=~b$mu[tasks],cor="independent")
  shared<-bmeas(fm,list(a=block,b=block),subjects="shared",fit=FALSE)
  separate<-bmeas(fm,list(a=block,b=block),subjects="separate",fit=FALSE)
  shape_plan<-function(spec)suppressWarnings(execution_plan(spec,chains=1,iter=10,warmup=5,resources=policy,execution="experimental"))
  a<-shape_plan(shared);b<-shape_plan(separate)
  ok(a$dimensions$N==4&&b$dimensions$N==8&&a$dimensions$O==24&&b$dimensions$O==24,"shared and separate aggregation uses the correct person domains and all block rows")
  ok(a$dimensions$response_values==24&&a$dimensions$schema_values==24,"likelihood rows and fixed-width payload are separately reported")
  fixed<-observe(gaussian_model(),long,"person","task","y",units="points",parameters=list(mu=list(fixed=0),sigma=list(fixed=1)))
  empty<-bmeas(bmf(),list(score=fixed),fit=FALSE);q<-shape_plan(empty)
  ok(q$dimensions$J==0&&q$dimensions$K==0&&q$dimensions$U==0&&q$dimensions$S==0&&q$dimensions$logical_targets==3,"fixed aliases retain their logical domain and true empty free dimensions")
  ok(q$resource_admission$allowed&&!q$scientific_eligibility$allowed&&q$scientific_eligibility$finding$code=="E_NO_FREE_PARAMETERS","resources do not admit zero-parameter sampling")
  q<-execution_plan(s,chains=4,iter=2000,warmup=1000,cores=4,threads=3,grainsize=1001,control=list(max_treedepth=11),resources=policy)
  ok(q$execution_allowed&&q$concurrency$simultaneous_workers==12&&q$run$max_treedepth==11,"ordinary thread/grainsize/depth controls exceed only former development ceilings")
  # A heuristic size concern is distinct from an actual source violation.
  run<-p$run;run$chains<-1L;output<-p$output_estimate;output$saved_states<-20;output$estimated_csv_bytes<-20*1024^2
  consumer<-private(".bh_resource_consumer_support")(p$dimensions,run,output)
  ok(consumer$individual$status=="configured_limits_not_exceeded"&&length(consumer$individual$estimate_risks)==1,"estimated CSV risk is not falsely labelled observed source incompatibility")
  ok(identical(serialize(s,NULL,version=2),identity_before),"planning never reseals or mutates the scientific specification")
  ok(length(calls)==0L,"all terminal planning traps remain untouched")
  cat("PASS",passed,"bounded resource-policy controls\n")
})
