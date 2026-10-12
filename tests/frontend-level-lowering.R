source(file.path("helpers", "native-guard.R"))
# Operand lowering/zero-coordinate lifecycle checks; no native backend invoked.
source_mode<-nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))
if(source_mode)for(f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"),"R"),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,"bmeas")
passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
tasks<-c("x1","x2","x3")
d<-expand.grid(person=1:4,task=tasks,stringsAsFactors=FALSE);d$y<-sin(seq_len(nrow(d))*.41)
bind<-function(p=NULL,data=d)observe(gaussian_model(),data,"person","task","y",units="points",parameters=p)
prepare<-function(p,m=bhmf())bhmm(m,data=list(scores=bind(p)),fit=FALSE)
cases<-list(ordinary=prepare(NULL,bhmf(G=~scores$mu[tasks])),task=prepare(list(mu="task",sigma=list(fixed=1))),
  fixed_mean=prepare(list(mu=list(fixed=0))),fixed_all=prepare(list(mu=list(fixed=0),sigma=list(fixed=1))))
codes<-lapply(cases,function(s)as.character(stancode(s,threads=2L,log_lik=TRUE)))
ok(all(vapply(codes,function(c)identical(c,codes[[1L]]),logical(1))),"same observation contracts use one body across all parameter levels")
for(name in names(cases)) {
  s<-cases[[name]];dat<-standata(s,threads=2L);ir<-private(".bh_backend_ir")(s,TRUE)
  ok(identical(dat$J,nrow(model_table(s,"targets")))&&identical(dat$S,nrow(model_table(s,"observation_parameters"))),paste(name,"actual free predictor/scale dimensions"))
  ok(length(dat$alpha_location)==dat$J&&length(dat$sd_scale)==dat$S,paste(name,"one prior entry per free coordinate"))
  ok(all(lengths(ir$init)>0L),paste(name,"init omits only empty entries"))
  op<-model_table(s,"observation_operands");rows<-s$data_map$blocks$scores
  for(p in c("mu","sigma")) {
    a<-op[op$argument==p,];a<-a[match(rows$.observation_id,a$observation_id),]
    idx<-dat[[paste0("b1_index_",p)]];fixed<-dat[[paste0("b1_fixed_",p)]]
    expected<-if(p=="mu")a$eta_index else a$observation_scale_index
    expected[is.na(expected)]<-0L
    ok(identical(as.integer(idx),as.integer(expected)),paste(name,p,"exact operand index map"))
    selected<-a$operand=="fixed_received"
    ok(identical(as.numeric(fixed[selected]),as.numeric(a$received_constant[selected])),paste(name,p,"fixed data are already on received scale"))
  }
}
ok(standata(cases$fixed_all)$J==0L&&standata(cases$fixed_all)$S==0L&&length(private(".bh_backend_ir")(cases$fixed_all)$init)==0L,"fully fixed lowering has no invented initialization coordinates")
ok(grepl("if(J>0) for(j in 1:J)",codes[[1]],fixed=TRUE),"empty mean vector cannot enter a 1:0 prior loop")
map<-c(x3=.9,x1=.7,x2=.8)
scaled<-prepare(list(mu="task",sigma=list(fixed=log(map),scale="predictor")))
dat<-standata(scaled);rows<-scaled$data_map$blocks$scores
ok(max(abs(dat$b1_fixed_sigma-unname(map[rows$.task])))<1e-15,"fixed log-sigma converted once before the received branch")

# Zero-row direct blocks lower to no likelihood units while retaining domains.
seen<-d;seen$task<-"seen";seen<-seen[1:4,]
empty<-seen;empty$y<-NA_real_
es<-bhmm(bhmf(),data=list(seen=bind(list(mu="task",sigma=list(fixed=1)),seen),empty=bind(list(mu=list(fixed=.2),sigma=list(fixed=.8)),empty)),subjects="shared",missing="observed_only",fit=FALSE)
ed<-standata(es,threads=2L);ec<-as.character(stancode(es,threads=2L,log_lik=TRUE))
ok(ed$b2_O==0L&&length(ed$b2_index_mu)==0L&&length(ed$b2_fixed_mu)==0L,"empty block operand arrays have zero length")
ok(ed$O==4L&&grepl("if(b2_O>0) for(o in 1:b2_O)",ec,fixed=TRUE),"empty direct block has an explicit zero-row likelihood guard")

# Cache only actual nonempty parameter families and keep level semantics visible.
for(name in c("task","fixed_mean")) {
  s<-cases[[name]];expected<-if(name=="task")"alpha"else"obs_sd"
  backend<-list(summary=function(variables,...) {
    stopifnot(identical(variables,expected))
    data.frame(variable=paste0(expected,"[",1:3,"]"),mean=.4,median=.4,sd=.1,q5=.2,q95=.6,rhat=1,ess_bulk=10,ess_tail=10,mcse_mean=.03)
  },diagnostic_summary=function(quiet=TRUE)data.frame(num_divergent=0,num_max_treedepth=0),output=function()character(),draws=function(...)stop("no correlation draw request is appropriate"))
  plan<-list(identification=check_model(s)$identification,posterior_propriety=check_model(s)$posterior_propriety)
  cache<-private(".bh_cache_scalar_fit")(backend,s,plan)
  ok(!is.null(cache$parameter_summary)&&all(cache$parameter_summary$parameter_role==if(name=="task")"Intercept"else"observation_sd"),paste(name,"cache labels actual free roles"))
  ok(all(cache$parameter_summary$target_variation_level=="task")&&nrow(cache$fixed_parameters)==3L,paste(name,"cache distinguishes task uncertainty from fixed values"))
  ok(cache$admissibility$status=="not_applicable",paste(name,"does not invent person correlation diagnostics"))
  fit<-structure(list(spec=s,backend=NULL,cache=cache,run=list(chains=1,iter_warmup=0,iter_sampling=1,reduction="serial"),provenance=list()),class="bhmm_fit")
  ok(identical(posterior_summary(fit),cache$parameter_summary)&&inherits(summary(fit),"summary.bhmm_fit"),paste(name,"summary is backend-free"))
}
cat(passed,"parameter-level lowering/cache checks passed\n")
