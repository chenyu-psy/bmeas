source(file.path("helpers", "native-guard.R"))
# Generic experimental support and dispatch, with no native backend invocation.
source_mode<-nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))
if(source_mode)for(f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"),"R"),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,"bmeas")
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,"measurement_error")&&identical(e$code,code),code)}
tasks<-c("t1","t2","t3")
d<-expand.grid(person=1:4,task=tasks,stringsAsFactors=FALSE);d$y<-sin(seq_len(nrow(d))*.31)+seq_len(nrow(d))*.09;d$obs<-paste0("o",seq_len(nrow(d)))
block<-function(model=gaussian_model(),data=d,parameters=NULL)observe(model,data,"person","task","y",units="points",observation_id="obs",parameters=parameters)
a<-block();b<-block(data=transform(d,y=y*.6+.1))
shared<-bhmm(bhmf(G=~a$mu[tasks]+b$mu[tasks]),data=list(a=a,b=b),subjects="shared",fit=FALSE)
ok(check_model(shared)$backend$status=="experimental_typed_composition","two Gaussian blocks have explicit experimental profile")
ok(check_model(shared)$backend$support$status=="supported_typed_ir"&&check_model(shared)$backend$qualification$status=="exact_composition_unqualified","support does not imply qualification")
ok(length(shared$data_map$subject_keys)==4L&&standata(shared)$b1_O+standata(shared)$b2_O==24L,"shared people and all likelihood rows retained")
prior<-c(set_prior("normal(0,1)","Intercept"),set_prior("normal(0,.7)","loading"),set_prior("half_normal(1)","observation_sd"))
plain_validator<-function(data)TRUE
custom<-function(name="own_normal",validate_data=plain_validator)register_stan_adapter(new_observation_model(name,"test1",response_schema(y="real"),
  list(location=parameter_spec("real","identity","theta","subject_task","points","Location"),
       noise=parameter_spec("positive","log","theta","task","points","Observation SD")),
  stan_density(symbol="owned_normal_lpdf",arguments=c("y","location","noise"),argument_types=rep("real",3),normalized=TRUE,approximation="none",builtin=TRUE,license="MIT"),
  model_capabilities(fit=TRUE,reduce_sum=TRUE),validate_data=validate_data),
  function_code="real owned_normal_lpdf(real y, real location, real noise) { return normal_lpdf(y | location, noise); }",function_exports="owned_normal_lpdf")
ok(is.primitive(custom(validate_data=is.list)$validate_data),"stable primitive validator retained without compilation")
cm<-custom();cs<-bhmm(bhmf(G=~custom$location[tasks]),data=list(custom=block(cm)),prior=prior,fit=FALSE)
ok(check_model(cs)$backend$support$status=="supported_typed_ir","registered arbitrary typed density supported")
ok(all(model_table(cs,"observation_parameters")$parameter=="noise"),"custom observation role is independent of its name")
ok(grepl("caller_managed",check_model(cs)$backend$support$blocks$custom$callback_dependencies),"custom callback dependency boundary is visible")
for(i in 1:4){stan_data<-standata(cs);stan_code<-stancode(cs);cs<-unserialize(serialize(cs,NULL));ok(identical(standata(cs),stan_data),"cold deterministic callback remains JIT/RDS stable")}
renamed<-gaussian_model();renamed$name<-"renamed response"
renamed_spec<-bhmm(bhmf(G=~score$mu[tasks]),data=list(score=block(renamed)),fit=FALSE)
ordinary<-bhmm(bhmf(G=~score$mu[tasks]),data=list(score=block()),fit=FALSE)
ok(identical(standata(ordinary),standata(renamed_spec))&&identical(as.character(stancode(ordinary)),as.character(stancode(renamed_spec))),"renaming a canonical builtin preserves exact scientific code/data")
spoof<-new_observation_model("gaussian","test1",cm$data_schema,cm$parameters,cm$likelihood,model_capabilities(fit=TRUE))
spoofed<-bhmm(bhmf(G=~custom$location[tasks]),data=list(custom=block(spoof)),prior=prior,fit=FALSE)
ok(check_model(spoofed)$backend$support$status=="unsupported_observation_contract","a familiar name does not register an executable contract")
err(stancode(spoofed),"E_OBSERVATION_CONTRACT_UNSUPPORTED")
# Contract corruption is checked before backend selection, not delegated to Stan.
bad<-cm;bad$likelihood$argument_types[2]<-"int"
err(bhmm(bhmf(G=~custom$location[tasks]),data=list(custom=block(bad)),prior=prior,fit=FALSE),"E_ARGUMENT_TYPE")
bad<-cm;bad$likelihood$arguments<-bad$likelihood$arguments[-3];bad$likelihood$argument_types<-bad$likelihood$argument_types[-3]
err(bhmm(bhmf(G=~custom$location[tasks]),data=list(custom=block(bad)),prior=prior,fit=FALSE),"E_PARAMETER_DEPENDENCY")
for(field in c("version","mode","independence_unit","parameter_binding")) {
 bad<-cm;bad$adapter[[field]]<-"unsupported"
 err(bhmm(bhmf(G=~custom$location[tasks]),data=list(custom=block(bad)),prior=prior,fit=FALSE),"E_ADAPTER")
}

helper_contract<-new_observation_model("helper_density","test1",cm$data_schema,cm$parameters,
 stan_density(symbol="helper_normal_lpdf",arguments=c("y","location","noise"),argument_types=rep("real",3),normalized=TRUE,approximation="none",builtin=TRUE,dependencies="centered_value",license="MIT"),model_capabilities(fit=TRUE))
helper<-register_stan_adapter(helper_contract,function_code=c("real centered_value(real y, real location) { return y-location; }","real helper_normal_lpdf(real y, real location, real noise) { return normal_lpdf(centered_value(y,location) | 0,noise); }"),function_exports=c("centered_value","helper_normal_lpdf"))
helper_spec<-bhmm(bhmf(G=~custom$location[tasks]),data=list(custom=block(helper)),prior=prior,fit=FALSE)
ok(check_model(helper_spec)$backend$support$status=="supported_typed_ir"&&is.character(stancode(helper_spec)),"correctly declared registered helper dependency is supported")
helper_contract$likelihood$dependencies<-"unregistered_helper"
err(register_stan_adapter(helper_contract,function_code=helper$adapter$function_code,function_exports=helper$adapter$function_exports),"E_STAN_DEPENDENCY")
# Name-only binomial tests: canonical constraints survive, custom names gain none.
bi<-binomial_model();bi$name<-"renamed binomial"
db<-data.frame(person=1:3,task="t",successes=c(0,1,2),trials=c(0,2,1))
err(observe(bi,db,"person","task","successes",fields=list(trials="trials"),units="count"),"E_BINOMIAL_SUPPORT")
custom_bin<-custom("binomial");ok(nrow(block(custom_bin)$data)==nrow(d),"registered custom named binomial does not gain builtin trials relation")

canonical_bin<-binomial_model()
registered_bin<-register_stan_adapter(new_observation_model("registered_trials","1",canonical_bin$data_schema,canonical_bin$parameters,canonical_bin$likelihood,model_capabilities(fit=TRUE)))
trial_data<-function(person)data.frame(person=c(person,"bridge"),task="t",successes=c(1L,0L),trials=c(2L,0L))
prepare_trials<-function(model,trial_field="trials") {
 block<-function(person)observe(model,trial_data(person),"person","task","successes",fields=stats::setNames(list("trials"),trial_field),units="count")
 bhmm(bhmf(A=~a$p["t"],B=~b$p["t"],cor="free"),list(a=block("first"),b=block("second")),subjects="shared",prior=c(set_prior("normal(0,1)","Intercept"),set_prior("normal(0,1)","loading")),fit=FALSE)
}
registered_trials<-prepare_trials(registered_bin)
ok(sum(registered_trials$tables$information$informative)==2L&&check_model(registered_trials)$identification$status=="proven_unidentified","bare registered Stan Math binomial keeps zero-trial information rule")
invalid_trials<-trial_data("bad");invalid_trials$successes[2]<-1L
err(observe(registered_bin,invalid_trials,"person","task","successes",fields=list(trials="trials"),units="count"),"E_BINOMIAL_SUPPORT")
renamed_bin<-register_stan_adapter(new_observation_model("renamed_fields","1",response_schema(y="integer",n="integer"),canonical_bin$parameters,
 stan_density(symbol="binomial_logit_lpmf",arguments=c("y","n","p"),argument_types=c("int","int","real"),normalized=TRUE,approximation="none",builtin=TRUE),model_capabilities(fit=TRUE)))
renamed_trials<-prepare_trials(renamed_bin,"n")
ok(sum(renamed_trials$tables$information$informative)==2L&&check_model(renamed_trials)$identification$status=="proven_unidentified","known primitive row operands resolve by argument bindings, not fixed field names")
negative_trials<-trial_data("bad");negative_trials$trials[1]<- -1L
err(observe(renamed_bin,negative_trials,"person","task","successes",fields=list(n="trials"),units="count"),"E_BINOMIAL_SUPPORT")
ok(check_model(registered_trials)$backend$status=="experimental_typed_composition","row information does not create automatic family/evidence qualification")
# Independent person sets do not invent cross-person information.
d2<-transform(d,person=person+4L)
independent<-bhmm(bhmf(A=~a$mu[tasks],B=~b$mu[tasks],cor="independent"),data=list(a=a,b=block(data=d2)),subjects="shared",fit=FALSE)
uninformed<-bhmm(bhmf(A=~a$mu[tasks],B=~b$mu[tasks],cor="free"),data=list(a=a,b=block(data=d2)),subjects="shared",fit=FALSE)
local({
 old<-private(".bh_fit_scalar");calls<-0L
 replace<-function(f)if(source_mode)assign(".bh_fit_scalar",f,.GlobalEnv)else assignInNamespace(".bh_fit_scalar",f,ns="bmeas")
 on.exit(replace(old));replace(function(spec,plan,...) {calls<<-calls+1L;list(spec=spec,plan=plan)})
 run<-function(s,...)bhmm(s,chains=1,iter=20,warmup=10,...,resources='development')
 err(run(shared),"E_EXECUTION_PROFILE");ok(calls==0L,"qualified default stops before backend")
 warnings<-character()
 fit<-withCallingHandlers(run(shared,execution="experimental"),warning=function(w){warnings<<-c(warnings,w$code);invokeRestart("muffleWarning")})
 ok(calls==1L&&"W_EXPERIMENTAL_EXECUTION"%in%warnings,"explicit multiblock experimental request warns then dispatches")
 ok(fit$plan$execution_profile$used=="experimental_typed_composition"&&fit$plan$backend_qualification$qualification$status=="exact_composition_unqualified","caller-specific evidence scope survives dispatch")
 suppressWarnings(run(independent,execution="experimental"));ok(calls==2L,"independent disjoint blocks dispatch")
 err(run(uninformed,execution="experimental",exploratory=TRUE),"E_IDENTIFICATION")
 err(run(spoofed,execution="experimental"),"E_OBSERVATION_CONTRACT_UNSUPPORTED")
 suppressWarnings(run(cs,execution="experimental"));ok(calls==3L,"registered custom dispatches only after prior/type gates")
 err(run(cs),"E_EXECUTION_PROFILE");ok(calls==3L,"a previous successful experimental dispatch does not authorize default execution")
 weak<-bhmm(bhmf(G=~custom$location[tasks]),data=list(custom=block(cm)),fit=FALSE)
 err(run(weak,execution="experimental"),"E_PRIOR_INCOMPLETE")
})

# A serialization-unstable captured factory frame is an explicit capability limit.
unserializable<-function(name="captured_factory",validate_data=function(data)TRUE)custom(name,validate_data)
err(unserializable(),"E_CALLBACK_LIFECYCLE")
# External global dependencies cannot be captured by a function fingerprint.
global_limit<-1
external_validator<-function(data)global_limit>0
externally_managed<-custom("external_dependency",external_validator)
hash_before<-private(".mp_hash")(externally_managed$validate_data);global_limit<-2
ok(identical(hash_before,private(".mp_hash")(externally_managed$validate_data)),"global dependency example documents the hash boundary without claiming it is frozen")
resource<-Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
if(nzchar(resource)) {
 base<-m3_cs_model(resource)
 registered<-register_stan_adapter(new_observation_model("registered_memory","1",base$data_schema,base$parameters,base$likelihood,
   capabilities=base$capabilities,parameter_dependencies=base$parameter_dependencies,validate_data=base$validate_data,constants=base$constants),
   resources=base$adapter$resources,function_code=base$adapter$function_code,function_exports=base$adapter$function_exports)
 make_data<-function(person){x<-data.frame(person=c(person,"bridge"),task="t");x$counts<-matrix(0L,2,10);x$counts[1,1]<-3L;x$options<-matrix(rep(c(1L,3L,1L,3L,0L,0L,0L,0L,0L,10L),each=2),2,10);x}
 prepare<-function(m,task_override=TRUE){b<-function(x)observe(m,x,"person","task",fields=list(counts="counts",options="options"),units="count",parameters=if(task_override)list(c="task",ra=list(fixed=.4),rc=list(fixed=.6))else list(ra=list(fixed=.4),rc=list(fixed=.6)))
  bhmm(bhmf(A=~left$a["t"],B=~right$a["t"],cor="free"),data=list(left=b(make_data("left_only")),right=b(make_data("right_only"))),subjects="shared",prior=c(set_prior("normal(0,1)","Intercept"),set_prior("normal(0,1)","loading")),fit=FALSE)}
 original<-prepare(base);rebuilt<-prepare(registered)
 c_args<-base$parameters$c[c("support","predictor","receives","levels","units","description","predictor_units")];c_args$levels<-"task";c_args$role<-"predictor"
 changed_parameters<-base$parameters;changed_parameters$c<-do.call(parameter_spec,c_args)
 task_default<-register_stan_adapter(new_observation_model("default_task_memory","1",base$data_schema,changed_parameters,base$likelihood,
  capabilities=base$capabilities,validate_data=base$validate_data,constants=base$constants),resources=base$adapter$resources,function_code=base$adapter$function_code,function_exports=base$adapter$function_exports)
 equivalent<-prepare(task_default,task_override=FALSE)
 alternative_parameters<-changed_parameters
 alternative_parameters$a<-parameter_spec("real","identity","eta","subject_task","log_activation","Direct received log-activation",role="predictor")
 alternative_parameters$ra<-parameter_spec("unit_interval","logit","theta","subject_task","probability","Alternative received probability parameterization",role="predictor")
 alternative<-register_stan_adapter(new_observation_model("alternative_received_coordinates","1",base$data_schema,alternative_parameters,base$likelihood,
  capabilities=base$capabilities,validate_data=plain_validator,constants=base$constants),resources=base$adapter$resources,function_code=base$adapter$function_code,function_exports=base$adapter$function_exports)
 alternative_spec<-prepare(alternative,task_override=FALSE)
 ok(sum(alternative_spec$tables$information$informative)==2L&&check_model(alternative_spec)$identification$status=="proven_unidentified","exact zero-count density fact survives valid alternative scalar supports/links/received scales")
 ok(is.na(check_model(alternative_spec)$backend$support$blocks$left$reference_density),"row-information evidence does not confer full scientific reference identity")
 bad_options<-make_data("bad_options");bad_options$options[1,1]<-0L
 bad_block<-observe(alternative,bad_options,"person","task",fields=list(counts="counts",options="options"),units="count",parameters=list(c="task",ra=list(fixed=.4),rc=list(fixed=.6)))
 err(bhmm(bhmf(A=~left$a["t"]),list(left=bad_block),prior=c(set_prior("normal(0,1)","Intercept"),set_prior("normal(0,1)","loading")),fit=FALSE),"E_M3_OPTIONS")

 role_fields<-c("key","role","level","fixed_received")
 ok(identical(model_table(original,"parameter_roles")[role_fields],model_table(equivalent,"parameter_roles")[role_fields]),"task model default and block override have identical effective scientific roles")
 ok(sum(equivalent$tables$information$informative)==2L&&check_model(equivalent)$identification$status=="proven_unidentified","variation-default spelling cannot change the conditional zero-count information rule")

 r<-base$adapter$resources[[1L]]
 annotated_resource<-stan_resource(r$file,r$sha256,r$exports,paste(r$license,"(unchanged; see original source)"),"Same pinned bytes; alternative accurate provenance")
 described_parameters<-lapply(base$parameters,function(p){args<-p[c("support","predictor","receives","levels","units","description","predictor_units")];args$description<-paste(args$description,"Same scientific parameter.");do.call(parameter_spec,args)})
 described_density<-stan_density(symbol=base$likelihood$symbol,arguments=base$likelihood$arguments,argument_types=base$likelihood$argument_types,normalized=TRUE,approximation=base$likelihood$approximation,builtin=TRUE,license="Wrapper MIT; unchanged external research license")
 annotated<-register_stan_adapter(new_observation_model("annotated_memory","readable-version",base$data_schema,described_parameters,described_density,
  capabilities=base$capabilities,validate_data=base$validate_data,constants=base$constants,provenance=list(description="Same executable with descriptive annotations")),resources=list(annotated_resource),function_code=base$adapter$function_code,function_exports=base$adapter$function_exports)
 annotated_spec<-prepare(annotated)
 ok(sum(annotated_spec$tables$information$informative)==2L&&check_model(annotated_spec)$identification$status=="proven_unidentified","descriptive provenance/license/parameter text cannot change scientific information")
 ok(identical(annotated$adapter$resources[[1L]]$source,annotated_resource$source),"scientific matching does not erase recorded provenance")

 external_rebuilt<-register_stan_adapter(new_observation_model("external_reference","1",base$data_schema,base$parameters,base$likelihood,
   capabilities=base$capabilities,parameter_dependencies=base$parameter_dependencies,validate_data=external_validator,constants=base$constants),
   resources=base$adapter$resources,function_code=base$adapter$function_code,function_exports=base$adapter$function_exports)
 external_spec<-prepare(external_rebuilt)
 ok(grepl("caller_managed",check_model(external_spec)$backend$support$blocks$left$callback_dependencies),"known density does not certify a replacement callback's dependencies")

 ok(sum(rebuilt$tables$information$informative)==2L&&sum(original$tables$information$informative)==2L,"public reconstructed M3 retains exact zero-total information rule")
 ok(check_model(rebuilt)$identification$status=="proven_unidentified","zero-count bridge cannot identify a renamed registered M3 correlation")
 ok(check_model(rebuilt)$backend$support$status=="supported_typed_ir","scientific information defect is separate from typed compiler support")
 err(bhmm(rebuilt,execution="experimental",chains=1,iter=20,warmup=10,resources='development'),"E_IDENTIFICATION")
 # Deterministic stand-in for the old release's information-policy function;
 # public construction seals an internally consistent historical-policy context.
 local({
  old_rule<-private(".bh_information_contract")
  replace<-function(f)if(source_mode)assign(".bh_information_contract",f,.GlobalEnv)else assignInNamespace(".bh_information_contract",f,"bmeas")
  on.exit(replace(old_rule))
  replace(function(model)"observed_rows_information_not_qualified")
  historical<-prepare(registered);original_bytes<-serialize(historical,NULL)
  replace(old_rule)
  err(model_table(historical),"E_INFORMATION_POLICY_REBUILD")
  err(check_model(historical),"E_INFORMATION_POLICY_REBUILD")
  err(standata(historical),"E_INFORMATION_POLICY_REBUILD")
  err(stancode(historical),"E_INFORMATION_POLICY_REBUILD")
  err(bhmm(historical,execution="experimental",chains=1,iter=20,warmup=10,resources='development'),"E_INFORMATION_POLICY_REBUILD")
  ok(identical(original_bytes,serialize(historical,NULL)),"old information policy is never silently rewritten or resealed")
 })

}

sdm_resource<-Sys.getenv("MEASUREMENTPKG_SDM_RESOURCE")
if(nzchar(sdm_resource)) {
 base<-sdm_reference_model(sdm_resource)
 constants<-lapply(base$constants,function(k)stan_constant(k$value,k$type,k$units,paste(k$provenance,"Same numeric constant; descriptive annotation.")))
 described<-register_stan_adapter(new_observation_model("described_SDM","test1",base$data_schema,base$parameters,base$likelihood,
   capabilities=base$capabilities,validate_data=base$validate_data,constants=constants),resources=base$adapter$resources,function_code=base$adapter$function_code,function_exports=base$adapter$function_exports)
 # Public support inspection establishes the known density separately from labels.
 sd<-data.frame(person=1:4,task="t",run=1:4,error=c(-.2,.1,.3,-.4))
 sb<-observe(described,sd,"person","task","error",fields=list(run="run"),units="radian")
 ss<-bhmm(bhmf(G=~report$log_c["t"]+report$log_kappa["t"]),list(report=sb),prior=c(set_prior("normal(0,1)","Intercept"),set_prior("normal(0,.5)","loading")),fit=FALSE)
 ok(check_model(ss)$backend$support$blocks$report$reference_density=="sdm_reference","constant provenance leaves known SDM scientific density identity unchanged")
 ok(identical(described$constants,constants),"constant provenance remains recorded in the full model")
}
cat(passed,"generic execution assertions passed; mocked dispatch only\n")
