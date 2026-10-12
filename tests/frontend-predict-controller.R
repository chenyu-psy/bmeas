source(file.path("helpers", "native-guard.R"))
# Invented joint values in HMC-shaped files exercise public plumbing only.
# No posterior evidence, inference validation, model execution, or sampling.
source_root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode<-nzchar(source_root)
if(source_mode) {
  for(file in list.files(file.path(source_root,"R"),full.names=TRUE))source(file)
}else library(bmeas)
private<-function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
.bh_ppc_forward_plan<-private(".bh_ppc_forward_plan")
.bh_ppc_rng_preflight<-private(".bh_ppc_rng_preflight")
.bh_ppc_validate_saved_spec<-private(".bh_ppc_validate_saved_spec")
.bh_ppc_with_rng<-private(".bh_ppc_with_rng")
.bh_validate_fingerprint<-private(".bh_validate_fingerprint")
.mp_hash<-private(".mp_hash")
.bh_ppc_ingest_csv<-private(".bh_ppc_ingest_csv")
.bh_ppc_with_rng<-private(".bh_ppc_with_rng")
if(requireNamespace("cmdstanr",quietly=TRUE)&&identical(as.character(utils::packageVersion("cmdstanr")),"0.9.0")) {
root<-tempfile("bmeas-ppc-controller-")
dir.create(file.path(root,"controller-work/fixtures"),recursive=TRUE)
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
err<-function(expr,code){e<-tryCatch(force(expr),error=identity);ok(inherits(e,"measurement_error")&&identical(e$code,code),paste("refusal",code));invisible(e)}
people<-paste0("person",1:4);tasks<-c("a","b")
d<-expand.grid(person=people[1:3],task=tasks,repeat_id=1:2,stringsAsFactors=FALSE)
d$y<-sin(seq_len(nrow(d)));d$obs<-paste0("row",seq_len(nrow(d)))
obs<-observe(gaussian_model(),d,"person","task","y",units="score",observation_id="obs")
specs<-list(iid=bmeas(bmf(G=~score$mu[tasks],loadings=list(fixed=list(list(value=1,cells=list(G=~score$mu[tasks])))),residual=list(targets=~score$mu[tasks])),data=list(score=obs),fit=FALSE),
  centered=bmeas(bmf(G=~score$mu[tasks],loadings=list(fixed=list(list(value=1,cells=list(G=~score$mu[tasks])))),residual=list(targets=~score$mu[tasks]),person=list(distribution="sample_centered",ids=people),augmentation=list(type="task_cholesky",targets=~score$mu[c("b","a")])),data=list(score=obs),fit=FALSE))
budget<-diagnostic_budget(4,100,400)
make_fit<-function(spec,files=character())structure(list(spec=spec,code=as.character(stancode(spec)),
  run=list(chains=2L,iter_sampling=2L),backend=list(draws=function(...)stop("backend trap"),metadata=function(...)stop("metadata trap")),
  cache=list(parameter_summary=data.frame(variable="alpha[1]",mean=.2,sd=.1,rhat=1.01,ess_bulk=30,ess_tail=20),
    inference=list(status="not_assessed"),posterior_propriety=list(status="not_assessed"),posterior_normalizability=list(status="not_assessed"),
    predictive_fit=list(status="not_run",reason="engineering fixture")),
  provenance=list(kind="synthetic_joint_states_engineering_only_not_posterior_evidence",model_fingerprint=spec$fingerprint,output_files=files)),class=c("bmeas_fit","bhmm_fit"))
make_state<-function(spec,level) {
  p<-.bh_ppc_forward_plan(spec,level);x<-setNames(rep(0,length(p$required_variables)),p$required_variables)
  x[grepl("^alpha\\[",names(x))]<-.2
  x[grepl("^effective_loadings\\[",names(x))]<-1
  x[grepl("^obs_sd\\[",names(x))]<-.4
  x[grepl("^task_row_scale\\[",names(x))]<-.8
  for(base in c("L_component","L_parameter","L_task_work","D_parameter"))for(i in 1:12) {
    key<-paste0(base,"[",i,",",i,"]");if(key%in%names(x))x[key]<-if(base=="D_parameter").5 else 1
  }
  x[grepl("^eta\\[",names(x))]<-seq_len(sum(grepl("^eta\\[",names(x))))/10
  x
}
write_states<-function(label,x) {
  csv_names<-gsub("\\]","",gsub("[\\[,]",".",names(x)))
  vapply(c(7L,2L),function(id){
    path<-file.path(root,"controller-work/fixtures",paste0(label,"-",id,".csv"))
    lines<-c("# ENGINEERING FIXTURE: invented values; not posterior evidence", "# stan_version_major = 2","# stan_version_minor = 35","# stan_version_patch = 0",
      "# model = scalar_components_model",paste0("# id = ",id),"# method = sample","# num_samples = 2","# num_warmup = 0","# save_warmup = 0","# thin = 1","# algorithm = hmc","# engine = nuts","# metric = diag_e","# sig_figs = 18",
      paste(c("lp__","accept_stat__",csv_names),collapse=","),
      rep(paste(c("-1","0.9",format(x,digits=17,scientific=TRUE,trim=TRUE)),collapse=","),2),
      "# Elapsed Time: 0 seconds (Warm-up)","# 0 seconds (Sampling)","# 0 seconds (Total)")
    writeLines(lines,path,useBytes=TRUE);path
  },"")
}
fits<-list()
for(n in names(specs))for(level in c("population","conditional")) {
  label<-paste(n,level,sep="-");spec<-specs[[n]];fit<-make_fit(spec,write_states(label,make_state(spec,level)))
  before<-serialize(fit,NULL);set.seed(789);old<-list(kind=RNGkind(),seed=.Random.seed,cwd=getwd())
  plan<-ppc_plan(fit,4L,budget,level=level)
  ok(identical(before,serialize(fit,NULL))&&identical(.Random.seed,old$seed),paste(label,"passive plan preserves bytes and RNG"))
  out<-assess_measurement_fit(fit,checks="ppc",level=level,draws=4L,seed=23L,budget=budget)
  if(identical(out$cache$predictive_fit$status,"failed"))print(out$cache$ppc$metadata$failure)
  ok(out$cache$predictive_fit$status%in%c("computed","warning"),paste(label,"public assessment produces cached discrepancies"))
  ok(identical(before,serialize(fit,NULL))&&identical(old$seed,.Random.seed)&&identical(old$kind,RNGkind())&&identical(old$cwd,getwd()),paste(label,"fit RNG and cwd unchanged"))
  ok(identical(out$cache$inference,fit$cache$inference)&&identical(out$cache$posterior_normalizability,fit$cache$posterior_normalizability),paste(label,"upstream evidence exact"))
  ok(identical(out$cache$ppc$metadata$source_qualification,fit$provenance$kind)&&!out$cache$ppc$metadata$source_receipt$original_fit_hashes_present,paste(label,"synthetic and read-time receipt scope explicit"))
  ok(identical(out$cache$ppc$metadata$draw_keys$chain_id,c(2L,7L,2L,7L)),paste(label,"recorded chain identity retained"))
  replay<-assess_measurement_fit(fit,checks="ppc",level=level,draws=4L,seed=23L,budget=budget)
  ok(identical(out$cache$ppc$table,replay$cache$ppc$table)&&identical(out$cache$ppc$replicate_statistics,replay$cache$ppc$replicate_statistics),paste(label,"explicit-seed exact response-statistic replay"))
  ok(if(level=="conditional")identical(out$cache$measurement_structure_fit$status,"not_assessed")else out$cache$measurement_structure_fit$status%in%c("computed","warning"),paste(label,"estimand status separated"))
  fits[[label]]<-out
}
fit<-fits[[1]]
err(ppc_plan(fit,4,budget),"E_CHECK_UNSUPPORTED")
err(ppc_plan(fit,4,diagnostic_budget(4,1,4),level="population"),"E_DIAGNOSTIC_BUDGET")
bad<-fit;bad$provenance$model_fingerprint<-"different";err(ppc_plan(bad,4,budget,level="population"),"E_PPC_LAYOUT")
bad<-fit;bad$cache$posterior_propriety$status<-"proven_improper";err(ppc_plan(bad,4,budget,level="population"),"E_PPC_POSTERIOR")
bad<-fit;bad$provenance$output_files<-file.path(root,"controller-work/fixtures/absent.csv")
before<-serialize(bad,NULL);set.seed(434);seed<-.Random.seed
failed<-assess_measurement_fit(bad,checks="ppc",level="population",draws=4,seed=12,budget=budget)
ok(identical(failed$cache$predictive_fit$status,"failed")&&is.null(failed$cache$ppc$table)&&is.null(failed$cache$ppc$replicate_statistics),"missing source invalidates previous current success")
ok(identical(before,serialize(bad,NULL))&&identical(seed,.Random.seed),"failed attempt preserves input and RNG")
rm(.Random.seed,envir=.GlobalEnv)
invisible(assess_measurement_fit(fit,checks="ppc",level="population",draws=4,seed=12,budget=budget))
ok(!exists(".Random.seed",.GlobalEnv,inherits=FALSE),"absent RNG seed restored after acquisition and simulation")
set.seed(188);saved<-.Random.seed;kind<-RNGkind();fitbytes<-serialize(fit,NULL)
trapnames<-c("file","readLines","file.info","file.exists","normalizePath","loadNamespace","requireNamespace","tempfile","saveRDS","check_model","stancode","standata",".bh_validate_fingerprint",".bh_ppc_validate_saved_spec",".mp_hash")
trap_environment<-function(n)if(source_mode).GlobalEnv else if(exists(n,asNamespace("bmeas"),inherits=FALSE))asNamespace("bmeas")else baseenv()
saved_functions<-lapply(trapnames,function(n){e<-trap_environment(n);present<-exists(n,e,inherits=FALSE);list(env=e,present=present,value=if(present)get(n,e)else NULL,locked=present&&bindingIsLocked(n,e))});names(saved_functions)<-trapnames
calls<-0L
for(n in trapnames){s<-saved_functions[[n]];if(s$locked)unlockBinding(n,s$env);assign(n,function(...){calls<<-calls+1L;stop("passive planner reached a terminal")},s$env)}
planning<-tryCatch(ppc_plan(fit,4,budget,level="population"),finally={for(n in trapnames){s<-saved_functions[[n]];if(s$present)assign(n,s$value,s$env)else rm(list=n,envir=s$env);if(s$locked)lockBinding(n,s$env)}})
ok(inherits(planning,"bmeas_ppc_plan")&&calls==0L&&identical(saved,.Random.seed)&&identical(fitbytes,serialize(fit,NULL)),"planning uses no files namespace loader model or backend")
interrupt<-tryCatch(.bh_ppc_with_rng(4,function(){runif(1);stop(structure(list(message="synthetic interrupt"),class=c("interrupt","condition")))}),interrupt=identity)
ok(inherits(interrupt,"interrupt")&&identical(saved,.Random.seed)&&identical(kind,RNGkind()),"interrupt propagates after complete RNG restoration")
oldstate<-.Random.seed;rm(.Random.seed,envir=.GlobalEnv);activecalls<-0L
makeActiveBinding(".Random.seed",function(v){activecalls<<-activecalls+1L;stop("active seed callback")},.GlobalEnv)
active<-tryCatch(.bh_ppc_rng_preflight(),error=identity)
rm(.Random.seed,envir=.GlobalEnv);assign(".Random.seed",oldstate,.GlobalEnv)
ok(identical(active$code,"E_RNG_STATE")&&activecalls==0L,"active RNG binding refuses without callback")
lockBinding(".Random.seed",.GlobalEnv)
locked<-tryCatch(.bh_ppc_rng_preflight(),error=identity)
unlockBinding(".Random.seed",.GlobalEnv)
ok(identical(locked$code,"E_RNG_STATE")&&identical(oldstate,.Random.seed),"locked RNG binding refuses before mutation")
for(state in list(c(-1L,1L),c(10403L,1L),c(999999L,rep(1L,625)))) {
  assign(".Random.seed",state,.GlobalEnv);bad_rng<-tryCatch(.bh_ppc_rng_preflight(),error=identity)
  ok(identical(bad_rng$code,"E_RNG_STATE")&&identical(state,.Random.seed),"malformed RNG header/length refuses without regeneration")
}
assign(".Random.seed",oldstate,.GlobalEnv)
bad<-fit;bad$spec$data_map$blocks$score$y[1]<-99
err(assess_measurement_fit(bad,checks="ppc",level="population",draws=4,seed=12,budget=budget),"E_SPEC_CHANGED")
fixedobs<-observe(gaussian_model(),d,"person","task","y",units="score",observation_id="obs",parameters=list(mu=list(fixed=.1),sigma=list(fixed=.4)))
fixedspec<-bmeas(bmf(),data=list(score=fixedobs),fit=FALSE)
fixedfit<-make_fit(fixedspec,write_states("constant-projection",numeric()))
fixedout<-assess_measurement_fit(fixedfit,checks="ppc",level="conditional",draws=4,seed=12,budget=diagnostic_budget(4,100,400,max_draw_values=0,max_latent_values=0))
ok(fixedout$cache$predictive_fit$status=="computed"&&!fixedout$cache$ppc$metadata$source_receipt$reader$called&&fixedout$cache$ppc$metadata$plan$required_columns==0,"constants-only projection preserves joint keys without requesting all draw columns")
ld<-d;ld$y<-exp(ld$y);bd<-d;bd$trials<-4L;bd$successes<-1L;bd$trials[1]<-0L;bd$successes[1]<-0L
mixedspec<-bmeas(bmf(),data=list(normal=observe(gaussian_model(),d,"person","task","y",units="score",observation_id="obs",parameters=list(mu="task",sigma=list(fixed=.4))),
 positive=observe(lognormal_model(),ld,"person","task","y",units="seconds",observation_id="obs",parameters=list(meanlog="task",sdlog=list(fixed=.3))),
 coin=observe(binomial_model(),bd,"person","task",fields=list(successes="successes",trials="trials"),units="count",observation_id="obs",parameters=list(p="task"))),subjects="shared",fit=FALSE)
mixedfit<-make_fit(mixedspec,write_states("mixed-population",make_state(mixedspec,"population")))
mixedout<-assess_measurement_fit(mixedfit,checks="ppc",level="population",draws=4,seed=44,budget=budget)
ok(mixedout$cache$predictive_fit$status%in%c("computed","warning")&&mixedout$cache$ppc$metadata$plan$observations_per_rep==36L&&all(c("score","seconds","probability")%in%mixedout$cache$ppc$table$unit),"actual-reader mixed canonical response workflow preserves full rows and units")
readbefore<-serialize(mixedout,NULL);set.seed(17);rng<-.Random.seed
checks<-fit_checks(mixedout);text<-capture.output(print(summary(mixedout)))
ok(identical(checks$predictive$cached$table$record,mixedout$cache$ppc$table)&&any(grepl("synthetic_joint_states",text))&&identical(rng,.Random.seed)&&identical(readbefore,serialize(mixedout,NULL)),"public cached display exposes actual table and engineering provenance without mutation")
failing<-mixedout
states<-make_state(mixedspec,"population");keys<-mixedspec$tables$targets
states[paste0("alpha[",keys$target_index[keys$model=="positive"],"]")]<-1000
failing$provenance$output_files<-write_states("numerical-failure",states)
before<-serialize(failing,NULL);oldrng<-.Random.seed
broken<-assess_measurement_fit(failing,checks="ppc",level="population",draws=4,seed=44,budget=budget)
ok(identical(broken$cache$predictive_fit$status,"failed")&&identical(broken$cache$ppc$metadata$failure$code,"E_PPC_NUMERICAL")&&is.null(broken$cache$ppc$table)&&identical(oldrng,.Random.seed)&&identical(before,serialize(failing,NULL)),"numerical forward failure clears stale success and restores caller state")
saveRDS(fits,file.path(root,"controller-work/synthetic-fits.rds"))
cat("TOTAL",passed,"engineering assertions; no posterior validation\n")

fits_for_amendment<-fits
fit<-fits[["iid-population"]]
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
plan<-function(x)ppc_plan(x,4,budget,level="population")
assess<-function(x)assess_measurement_fit(x,checks="ppc",level="population",draws=4,seed=23,budget=budget)
calls<-character()
for(kind in c("fit","spec","block","model","provenance")) {
  cname<-paste0("ppc_callback_",kind)
  assign(paste0("$.",cname),local({tag<-kind;function(x,name){calls<<-c(calls,paste(tag,name));.subset2(unclass(x),name)}}),.GlobalEnv)
  assign(paste0("[.",cname),local({tag<-kind;function(x,...){calls<<-c(calls,paste(tag,"subset"));unclass(x)[...]}}),.GlobalEnv)
  bad<-fit
  if(kind=="fit")class(bad)<-c(cname,class(bad))
  if(kind=="spec")class(bad$spec)<-c(cname,class(bad$spec))
  if(kind=="block")class(bad$spec$models$score)<-c(cname,class(bad$spec$models$score))
  if(kind=="model")class(bad$spec$models$score$model)<-c(cname,class(bad$spec$models$score$model))
  if(kind=="provenance")class(bad$provenance)<-cname
  before<-serialize(bad,NULL);calls<-character();p<-plan(bad)
  ok(inherits(p,"bmeas_ppc_plan")&&!length(calls)&&identical(before,serialize(bad,NULL)),paste(kind,"planner uses plain saved access without callbacks or mutation"))
  calls<-character();ans<-tryCatch(assess(bad),error=identity)
  if(kind%in%c("fit","provenance")) {
    ok(!inherits(ans,"error")&&unclass(ans)$cache$predictive_fit$status=="computed"&&!length(calls)&&identical(class(ans),class(bad))&&identical(before,serialize(bad,NULL)),paste(kind,"assessment preserves classes without accessor callbacks"))
  }else ok(inherits(ans,"measurement_error")&&identical(ans$code,"E_SPEC_CHANGED")&&!length(calls)&&identical(before,serialize(bad,NULL)),paste(kind,"changed sealed class refuses without callbacks or resealing"))
  rm(list=c(paste0("$.",cname),paste0("[.",cname)),envir=.GlobalEnv)
}
duplicates<-list()
duplicates$fit<-structure(c(unclass(fit),list(spec=fit$spec)),class=class(fit))
bad<-fit;bad$spec<-structure(c(unclass(bad$spec),list(fingerprint="conflicting")),class=class(bad$spec));duplicates$spec<-bad
bad<-fit;bad$provenance<-c(bad$provenance,list(output_files="conflicting"));duplicates$provenance<-bad
bad<-fit;bad$cache<-c(bad$cache,list(posterior_propriety=list(status="proven_improper")));duplicates$cache<-bad
for(n in c("posterior_propriety","posterior_normalizability")) {
  bad<-fit;bad$cache[[n]]<-list(status="not_assessed",status=if(n=="posterior_propriety")"proven_improper"else"proven_non_normalizable");duplicates[[n]]<-bad
}
reader_env<-if(source_mode).GlobalEnv else asNamespace("bmeas")
old_reader<-get(".bh_ppc_ingest_csv",reader_env);reader_calls<-0L
set_reader<-function(value)if(source_mode)assign(".bh_ppc_ingest_csv",value,.GlobalEnv)else assignInNamespace(".bh_ppc_ingest_csv",value,ns="bmeas")
set_reader(function(...){reader_calls<<-reader_calls+1L;stop("must not reach acquisition")})
for(n in names(duplicates)) {
  before<-serialize(duplicates[[n]],NULL);got<-tryCatch(assess(duplicates[[n]]),error=identity)
  ok(inherits(got,"measurement_error")&&identical(got$code,"E_PPC_LAYOUT")&&reader_calls==0L&&identical(before,serialize(duplicates[[n]],NULL)),paste("ambiguous",n,"refuses before acquisition"))
}
set_reader(old_reader)
fingerprint_calls<-0L
is.na.ppc_callback_fingerprint<-function(x){fingerprint_calls<<-fingerprint_calls+1L;rep(FALSE,length(unclass(x)))}
for(value in list(structure(fit$spec$fingerprint,class="ppc_callback_fingerprint"),matrix(fit$spec$fingerprint,1,1))) {
  bad<-fit;bad$spec$fingerprint<-value;bad$provenance$model_fingerprint<-value
  before<-serialize(bad,NULL);p<-tryCatch(plan(bad),error=identity);a<-tryCatch(assess(bad),error=identity)
  ok(inherits(p,"measurement_error")&&identical(p$code,"E_PPC_LAYOUT")&&inherits(a,"measurement_error")&&identical(a$code,"E_PPC_LAYOUT")&&fingerprint_calls==0L&&identical(before,serialize(bad,NULL)),"nonplain fingerprint refuses before callbacks, hashing or acquisition")
}
rm(is.na.ppc_callback_fingerprint,envir=.GlobalEnv)
scalar_calls<-0L
for(method in c("length","Ops","as.data.frame","is.na"))assign(paste0(method,".ppc_callback_scalar"),function(...){scalar_calls<<-scalar_calls+1L;stop("consumed scalar callback")},.GlobalEnv)
centered_fixture<-fits_for_amendment[["centered-population"]]
for(n in c("roster_size","target_index","raw_coordinate_variance","units"))for(dimensioned in c(FALSE,TRUE)) {
  bad<-centered_fixture
  original<-switch(n,roster_size=bad$spec$tables$person_law$roster_size,target_index=bad$spec$tables$augmentation$target_index,
    raw_coordinate_variance=bad$spec$tables$augmentation$raw_coordinate_variance,units=bad$spec$models$score$units)
  value<-if(dimensioned)matrix(original,nrow=1L)else structure(original,class="ppc_callback_scalar")
  if(n=="roster_size")bad$spec$tables$person_law$roster_size<-value
  if(n%in%c("target_index","raw_coordinate_variance"))bad$spec$tables$augmentation[[n]]<-value
  if(n=="units")bad$spec$models$score$units<-value
  before<-serialize(bad,NULL);p<-tryCatch(plan(bad),error=identity);a<-tryCatch(assess(bad),error=identity)
  ok(inherits(p,"measurement_error")&&identical(p$code,"E_PPC_LAYOUT")&&inherits(a,"measurement_error")&&identical(a$code,"E_PPC_LAYOUT")&&scalar_calls==0L&&identical(before,serialize(bad,NULL)),paste("nonplain",n,"refuses before helper operations"))
}
for(n in c("person_law","augmentation")) {
  bad<-centered_fixture;record<-bad$spec$tables[[n]];bad$spec$tables[[n]]<-c(record,record[1])
  p<-tryCatch(plan(bad),error=identity)
  ok(inherits(p,"measurement_error")&&identical(p$code,"E_PPC_LAYOUT")&&scalar_calls==0L,paste("ambiguous",n,"container refuses before scalar access"))
}
rm(list=paste0(c("length","Ops","as.data.frame","is.na"),".ppc_callback_scalar"),envir=.GlobalEnv)
old_warn<-getOption("warn");old_kind<-RNGkind();set.seed(137);old_seed<-.Random.seed
uniforms<-c("Wichmann-Hill","Marsaglia-Multicarry","Super-Duper","Mersenne-Twister","Knuth-TAOCP","Knuth-TAOCP-2002","L'Ecuyer-CMRG")
normals<-c("Buggy Kinderman-Ramage","Ahrens-Dieter","Inversion","Kinderman-Ramage")
results<-list();n<-0L
options(warn=2)
for(u in uniforms)for(v in normals)for(s in c("Rounding","Rejection")) {
  suppressWarnings(RNGkind(u,v,s));suppressWarnings(set.seed(111));saved<-.Random.seed;k<-RNGkind();n<-n+1L
  got<-.bh_ppc_with_rng(7,function()runif(1))
  success<-is.finite(got)&&identical(saved,.Random.seed)&&identical(k,RNGkind())
  e<-tryCatch(.bh_ppc_with_rng(7,function(){runif(1);stop(structure(list(message="sentinel failure",code="E_SENTINEL"),class=c("measurement_error","error","condition")))}),error=identity)
  failure<-identical(e$code,"E_SENTINEL")&&identical(conditionMessage(e),"sentinel failure")&&identical(saved,.Random.seed)&&identical(k,RNGkind())
  i<-tryCatch(.bh_ppc_with_rng(7,function(){runif(1);stop(structure(list(message="sentinel interrupt"),class=c("interrupt","condition")))}),interrupt=identity,error=identity)
  interrupt<-inherits(i,"interrupt")&&identical(conditionMessage(i),"sentinel interrupt")&&identical(saved,.Random.seed)&&identical(k,RNGkind())
  results[[n]]<-c(success=success,failure=failure,interrupt=interrupt)
}
mat<-do.call(rbind,results)
for(j in colnames(mat))ok(nrow(mat)==56L&&all(mat[,j]),paste("all 56 supported full RNG kinds restore under warn=2 after",j))
suppressWarnings(RNGkind("Mersenne-Twister","Inversion","Rounding"));set.seed(51);saved<-.Random.seed;k<-RNGkind()
success<-assess(fit)
ok(success$cache$predictive_fit$status=="computed"&&identical(saved,.Random.seed)&&identical(k,RNGkind()),"actual public acquisition and simulation restore warning-emitting kind")
bad<-fit;bad$provenance$output_files<-"absent-engineering-file.csv";failure<-assess(bad)
ok(failure$cache$predictive_fit$status=="failed"&&failure$cache$ppc$metadata$failure$phase=="source_acquisition"&&identical(saved,.Random.seed)&&identical(k,RNGkind()),"public acquisition failure keeps original failure phase and RNG")
set_reader(function(...)stop(structure(list(message="public acquisition interrupt"),class=c("interrupt","condition"))))
i<-tryCatch(assess(fit),interrupt=identity,error=identity);set_reader(old_reader)
ok(inherits(i,"interrupt")&&identical(conditionMessage(i),"public acquisition interrupt")&&identical(saved,.Random.seed)&&identical(k,RNGkind()),"public acquisition interrupt propagates instead of becoming failed fit")
suppressWarnings(do.call(RNGkind,as.list(old_kind)));assign(".Random.seed",old_seed,.GlobalEnv);options(warn=old_warn)
text<-capture.output(print(summary(success)))
ok(any(grepl("Reading this report runs no new predictive simulation",text,fixed=TRUE)),"cached predictive summary footer describes passive reading")
cat("TOTAL",passed,"total controller controls including the amendment and 56 full-kind transactions per outcome; engineering evidence only\n")

unlink(root,recursive=TRUE)
}else cat("SKIP bounded actual-reader controls: CmdStanR 0.9.0 is unavailable\n")
