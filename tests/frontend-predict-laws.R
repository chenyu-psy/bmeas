source(file.path("helpers", "native-guard.R"))
# Synthetic engineering controls only: no posterior/model validation, backend,
source_root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode<-nzchar(source_root)
if(source_mode) {
  for(file in list.files(file.path(source_root,"R"),full.names=TRUE))source(file)
}else library(bmeas)
private<-function(name)if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
.bh_builtin_kind<-private(".bh_builtin_kind")
.bh_ppc_forward_plan<-private(".bh_ppc_forward_plan")
.bh_ppc_generate_eta<-private(".bh_ppc_generate_eta")
.bh_ppc_simulate_responses<-private(".bh_ppc_simulate_responses")
.bh_ppc_forward_matrix<-private(".bh_ppc_forward_matrix")
.bh_ppc_forward_names<-private(".bh_ppc_forward_names")
.bh_ppc_forward_plan<-private(".bh_ppc_forward_plan")
.bh_ppc_forward_product<-private(".bh_ppc_forward_product")
.bh_ppc_generate_eta<-private(".bh_ppc_generate_eta")
passed<-0L
ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
err<-function(expr,code){x<-tryCatch(force(expr),error=identity);ok(inherits(x,"measurement_error")&&identical(x$code,code),paste("typed",code));invisible(x)}
near<-function(a,b) isTRUE(all.equal(unname(a),unname(b),tolerance=1e-13))
people<-paste0("p",1:4);tasks<-c("a","b")
d<-expand.grid(person=people[1:3],task=tasks,repeat_id=1:2,stringsAsFactors=FALSE)
d$y<-sin(seq_len(nrow(d)));d$obs<-paste0("row",seq_len(nrow(d)))
block<-function(par=NULL,data=d,model=gaussian_model()) {
  if(identical(.bh_builtin_kind(model),"binomial"))return(observe(model,data,"person","task",fields=list(successes="successes",trials="trials"),units="units",observation_id="obs",parameters=par))
  observe(model,data,"person","task","y",units="units",observation_id="obs",parameters=par)
}
structure_iid<-bmf(G=~score$mu[tasks],residual=list(targets=~score$mu[tasks]))
iid<-bmeas(structure_iid,data=list(score=block()),fit=FALSE)
before<-serialize(iid,NULL);set.seed(77);rng<-.Random.seed
p<-.bh_ppc_forward_plan(iid,"population")
ok(identical(before,serialize(iid,NULL))&&identical(rng,.Random.seed),"planning is passive and preserves input")
ok(identical(p$geometry,list(N=3L,J=2L,K=1L,U=2L,S=2L,JT=0L)),"actual saved geometry")
draw<-setNames(rep(0,length(p$required_variables)),p$required_variables)
draw[c("alpha[1]","alpha[2]")]<-c(.2,-.4)
draw[c("effective_loadings[1,1]","effective_loadings[2,1]")]<-c(1.2,-.7)
draw[c("D_parameter[1,1]","D_parameter[2,2]")]<-c(.5,.8)
draw["L_component[1,1]"]<-1
draw[c("L_parameter[1,1]","L_parameter[2,1]","L_parameter[2,2]")]<-c(1,.3,sqrt(.91))
draw[c("obs_sd[1]","obs_sd[2]")]<-c(.2,.6)
for(seed in 1:20) {
  set.seed(seed);got<-.bh_ppc_generate_eta(p,draw)
  set.seed(seed);Z<-matrix(rnorm(3),1,3);E<-matrix(rnorm(6),2,3)
  A<-matrix(c(1.2,-.7),2,1);D<-diag(c(.5,.8));Lu<-matrix(c(1,.3,0,sqrt(.91)),2,2)
  expected<-matrix(rep(c(.2,-.4),each=3),3,2)+t(Z)%*%t(A)+t(Lu%*%E)%*%t(D)
  ok(near(got,expected),paste("iid aligned two-factor direct law",seed))
}
set.seed(11);ys<-.bh_ppc_simulate_responses(p,draw)
set.seed(11);eta<-.bh_ppc_generate_eta(p,draw);m<-p$blocks$score$mean_operand
expected<-rnorm(nrow(d),eta[cbind(m$person_index,m$eta_index)],ifelse(iid$data_map$blocks$score$.task=="a",.2,.6))
ok(near(ys$score,expected)&&identical(names(ys$score),iid$data_map$blocks$score$.observation_id),"one shared person state reused across repeated rows in saved order")
centered<-bmeas(bmf(G=~score$mu[tasks],residual=list(targets=~score$mu[tasks]),person=list(distribution="sample_centered",ids=people),augmentation=list(type="task_cholesky",targets=~score$mu[c("b","a")])),data=list(score=block()),fit=FALSE)
cp<-.bh_ppc_forward_plan(centered,"population")
ok(cp$geometry$N==4&&identical(cp$active_target_index,c(2L,1L))&&cp$latent_values_per_draw==6,"centered retains missing person and reversed task order")
cd<-setNames(rep(0,length(cp$required_variables)),cp$required_variables)
cd[c("alpha[1]","alpha[2]")]<-c(.2,-.4)
cd[c("L_task_work[1,1]","L_task_work[2,1]","L_task_work[2,2]")]<-c(1.1,.2,.8)
cd[c("task_row_scale[2]","task_row_scale[1]")]<-c(.7,1.3)
cd[c("obs_sd[1]","obs_sd[2]")]<-c(.2,.6)
Q<-matrix(0,4,3)
for(k in 1:3){Q[1:k,k]<-1/sqrt(k*(k+1));Q[k+1,k]<--k/sqrt(k*(k+1))}
H<-4/3*(diag(4)-matrix(.25,4,4))
ok(near(4/3*tcrossprod(Q),H),"independent positive Helmert covariance is exactly same-N H")
L<-matrix(c(1.1,.2,0,.8),2,2);scale<-c(.7,1.3)
for(seed in 1:20) {
  set.seed(seed);got<-.bh_ppc_generate_eta(cp,cd)
  set.seed(seed);W<-matrix(rnorm(6,sd=sqrt(4/3)),2,3)
  expected<-matrix(rep(c(.2,-.4),each=4),4,2)
  expected[,c(2,1)]<-expected[,c(2,1)]+t(diag(scale)%*%L%*%W%*%t(Q))
  ok(near(got,expected)&&max(abs(colMeans(got)-c(.2,-.4)))<1e-14,paste("centered full-roster law and single scaling",seed))
}
ok(near(kronecker(H,diag(scale)%*%tcrossprod(L)%*%diag(scale)),kronecker(4/3*tcrossprod(Q),tcrossprod(diag(scale)%*%L))),"deterministic centered cross-person/target covariance identity")
conditional<-.bh_ppc_forward_plan(centered,"conditional")
qd<-setNames(seq_along(conditional$required_variables)/10,conditional$required_variables)
set.seed(31);rng<-.Random.seed;qe<-.bh_ppc_generate_eta(conditional,qd)
ok(identical(rng,.Random.seed)&&all(is.na(qe[4,]))&&conditional$latent_values_per_draw==0,"conditional uses saved consumed eta without latent RNG or roster recentering")
ok(all(vapply(conditional$blocks,function(b){x<-b$mean_operand;near(qe[cbind(x$person_index,x$eta_index)],qd[paste0("eta[",x$person_index,",",x$eta_index,"]")])},logical(1))),"conditional exact scalar indices")
task<-bmeas(bmf(),data=list(score=block(list(mu="task",sigma=list(fixed=.4)))),fit=FALSE)
tp<-.bh_ppc_forward_plan(task,"population");td<-c("alpha[1]"=.1,"alpha[2]"=.8)
set.seed(21);rng<-.Random.seed;te<-.bh_ppc_generate_eta(tp,td)
ok(identical(rng,.Random.seed)&&near(te,matrix(rep(c(.1,.8),each=3),3,2))&&tp$latent_values_per_draw==0,"K=U=0 task means request no factor or RNG")
fixed<-bmeas(bmf(),data=list(score=block(list(mu=list(fixed=.2),sigma=list(fixed=.4)))),fit=FALSE)
for(level in c("population","conditional")) {
  fp<-.bh_ppc_forward_plan(fixed,level)
  set.seed(4);fy<-.bh_ppc_simulate_responses(fp,numeric())
  set.seed(4);ok(!length(fp$required_variables)&&near(fy$score,rnorm(nrow(d),.2,.4)),paste("fully fixed empty draw projection",level))
}
ctask<-bmeas(bmf(person=list(distribution="sample_centered",ids=people),augmentation=list(type="task_cholesky",targets=~0)),data=list(score=block(list(mu="task",sigma=list(fixed=.4)))),fit=FALSE)
ctp<-.bh_ppc_forward_plan(ctask,"population")
set.seed(17);rng<-.Random.seed;invisible(.bh_ppc_generate_eta(ctp,td))
ok(identical(rng,.Random.seed)&&ctp$geometry$JT==0&&identical(ctp$required_variables,names(td)),"JT=0 has no white/factor/row-scale request or RNG")
bd<-d;bd$successes<-0;bd$trials<-0
bs<-bmeas(bmf(),data=list(score=block(list(p=list(fixed=1000,scale="received")),bd,binomial_model())),fit=FALSE)
bp<-.bh_ppc_forward_plan(bs,"conditional")
set.seed(91);rng<-.Random.seed;by<-.bh_ppc_simulate_responses(bp,numeric())
ok(all(by$score==0)&&identical(rng,.Random.seed),"zero-trial binomial needs no inverse logit or RNG")
bp$blocks$score$trials[1]<-1
err(.bh_ppc_simulate_responses(bp,numeric()),"E_PPC_NUMERICAL")
ld<-d;ld$y<-exp(d$y)
ls<-bmeas(bmf(),data=list(score=block(list(meanlog=list(fixed=1000),sdlog=list(fixed=1)),ld,lognormal_model())),fit=FALSE)
lp<-.bh_ppc_forward_plan(ls,"population")
err(.bh_ppc_simulate_responses(lp,numeric()),"E_PPC_NUMERICAL")
lp$blocks$score$mean_operand$received_constant[]<--1000
err(.bh_ppc_simulate_responses(lp,numeric()),"E_PPC_NUMERICAL")
bad<-draw;bad["L_component[1,1]"]<-0;err(.bh_ppc_generate_eta(p,bad),"E_PPC_DRAW")
bad<-draw;bad["obs_sd[1]"]<-0;err(.bh_ppc_simulate_responses(p,bad),"E_PPC_DRAW")
bad<-draw;bad["alpha[1]"]<-Inf;err(.bh_ppc_generate_eta(p,bad),"E_PPC_DRAW")
renamed<-iid;renamed$models$score$model$name<-"cosmetic metadata";renamed$models$score$model$methods$posterior_predict<-function(...)stop("must not run")
ok(identical(.bh_ppc_forward_plan(renamed,"population")$required_variables,p$required_variables),"metadata rename and arbitrary posterior callback do not dispatch laws")
bad<-iid;bad$models$score$model$likelihood$symbol<-"custom_density";err(.bh_ppc_forward_plan(bad,"population"),"E_PPC_FAMILY")
bad<-iid;bad$tables$augmentation$type<-"unknown";err(.bh_ppc_forward_plan(bad,"population"),"E_PPC_LAYOUT")
bad<-iid;bad$tables$observation_operands$eta_index[1]<-99;err(.bh_ppc_forward_plan(bad,"population"),"E_PPC_LAYOUT")
bad<-iid;bad$tables$person_roster<-bad$tables$person_roster[rep(1,31),];err(.bh_ppc_forward_plan(bad,"population"),"E_PPC_LAYOUT")
empty_d<-d;empty_d$y[]<-NA_real_
empty<-bmeas(structure_iid,data=list(score=block(data=empty_d),anchor=block(list(mu=list(fixed=.2),sigma=list(fixed=.4)))),subjects="shared",fit=FALSE,missing="observed_only")
ep<-.bh_ppc_forward_plan(empty,"population")
set.seed(23);rng<-.Random.seed;invisible(.bh_ppc_generate_eta(ep,numeric()))
ok(!length(ep$required_variables)&&!nrow(ep$blocks$score$rows)&&identical(rng,.Random.seed),"unconsumed free primitives need no columns or latent RNG for fixed-only projection")
ey<-.bh_ppc_simulate_responses(ep,numeric())
ok(!length(ey$score)&&length(ey$anchor)==nrow(d),"wholly excluded declared block remains present as empty")
bd$trials[]<-4;bd$successes[]<-1
mixed<-bmeas(bmf(G=~score$mu[tasks]+positive$meanlog[tasks]+coin$p[tasks]),data=list(score=block(list(sigma=list(fixed=.2))),positive=block(list(sdlog=list(fixed=.4)),ld,lognormal_model()),coin=block(NULL,bd,binomial_model())),subjects="shared",fit=FALSE)
mp<-.bh_ppc_forward_plan(mixed,"population")
md<-setNames(rep(0,length(mp$required_variables)),mp$required_variables)
md[grepl("^effective_loadings",names(md))]<-1
md["L_component[1,1]"]<-1
set.seed(14);my<-.bh_ppc_simulate_responses(mp,md)
set.seed(14);me<-.bh_ppc_generate_eta(mp,md)
expected<-list()
for(b in names(mp$blocks)) {
  x<-mp$blocks[[b]];o<-x$mean_operand;mu<-me[cbind(o$person_index,o$eta_index)]
  expected[[b]]<-switch(x$family,gaussian=rnorm(nrow(x$rows),mu,.2),lognormal=rlnorm(nrow(x$rows),mu,.4),binomial=rbinom(nrow(x$rows),x$trials,plogis(mu)))
}
ok(all(vapply(names(my),function(b)near(my[[b]],expected[[b]]),logical(1)))&&all(me[,1]==me[,6]),"mixed canonical blocks share a single person latent state and use each RNG kernel")
residual_only<-bmeas(bmf(residual=list(targets=~score$mu[tasks])),data=list(score=block()),fit=FALSE)
rp<-.bh_ppc_forward_plan(residual_only,"population")
rd<-draw[rp$required_variables]
set.seed(72);re<-.bh_ppc_generate_eta(rp,rd)
set.seed(72);expect<-matrix(rep(c(.2,-.4),each=3),3,2)+t(Lu%*%matrix(rnorm(6),2,3))%*%diag(c(.5,.8))
ok(near(re,expect)&&!any(grepl("L_component|effective_loadings",rp$required_variables)),"K=0 requests no common dummy and generates residual law")
set.seed(28);a<-.bh_ppc_generate_eta(p,draw)
set.seed(28);b<-.bh_ppc_generate_eta(p,c(draw,optional_unused=-Inf))
ok(identical(a,b),"unused optional nonfinite posterior export is ignored")
centered_fixed<-bmeas(bmf(person=list(distribution="sample_centered",ids=people),augmentation=list(type="task_cholesky",targets=~0)),data=list(score=block(list(mu=list(fixed=.2),sigma=list(fixed=.4)))),fit=FALSE)
cfp<-.bh_ppc_forward_plan(centered_fixed,"population")
set.seed(80);rng<-.Random.seed;cfe<-.bh_ppc_generate_eta(cfp,numeric())
ok(identical(dim(cfe),c(4L,0L))&&!length(cfp$required_variables)&&identical(rng,.Random.seed),"centered J=JT=0 preserves full roster with empty predictors and no RNG")
cat("PASS total",passed,"synthetic engineering checks; no posterior evidence\n")

ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
err<-function(expr,code,label){x<-tryCatch(force(expr),error=identity);ok(inherits(x,"measurement_error")&&identical(x$code,code),label);invisible(x)}
RNGkind("Mersenne-Twister","Inversion","Rejection")
spec<-bmeas(bmf(G=~x),data.frame(id=c("a","b"),x=c(.2,.3)),subject="id",fit=FALSE)
p<-.bh_ppc_forward_plan(spec,"population")
d<-setNames(rep(1,length(p$required_variables)),p$required_variables);d["alpha[1]"]<-0
d["L_component[1,1]"]<-2
set.seed(1);err(.bh_ppc_generate_eta(p,d),"E_PPC_DRAW","reviewer common L=2 covariance inflation is refused")
d["L_component[1,1]"]<-1
tiny<-.Machine$double.xmin*.Machine$double.eps
d["effective_loadings[1,1]"]<-tiny
set.seed(1);err(.bh_ppc_generate_eta(p,d),"E_PPC_NUMERICAL","reviewer iid nonzero coefficient-score underflow is refused")
for(name in c("L_component","L_parameter")) {
  names1<-.bh_ppc_forward_names(name,1,1)
  err(.bh_ppc_forward_matrix(setNames(2,names1),name,1,1,TRUE),"E_PPC_DRAW",paste(name,"rejects nonunit scalar factor"))
  L<-matrix(c(1,.3,0,sqrt(.91)),2,2)
  rounded<-signif(L,6)
  got<-.bh_ppc_forward_matrix(setNames(as.vector(rounded),.bh_ppc_forward_names(name,2,2)),name,2,2,TRUE)
  ok(identical(got,rounded),paste(name,"accepts six-significant-digit CSV rounding without renormalizing"))
  near<-setNames(sqrt(1+1.9e-5),names1)
  ok(identical(as.numeric(.bh_ppc_forward_matrix(near,name,1,1,TRUE)),as.numeric(near)),paste(name,"admits Gram diagonal inside explicit tolerance"))
  far<-setNames(sqrt(1+2.1e-5),names1)
  err(.bh_ppc_forward_matrix(far,name,1,1,TRUE),"E_PPC_DRAW",paste(name,"rejects Gram diagonal outside explicit tolerance"))
}
ok(identical(.bh_ppc_forward_matrix(c("L_task_work[1,1]"=2),"L_task_work",1,1,TRUE),matrix(2,1,1)),"task covariance factor retains its nonunit role")
mul<-function(x,y).bh_ppc_forward_product(x,y,"focused product")
err(mul(matrix(tiny,1,1),matrix(.1836433,1,1)),"E_PPC_NUMERICAL","scalar-term underflow is detected before matrix multiplication")
err(mul(matrix(c(1,tiny),1,2),matrix(c(1,.1836433),2,1)),"E_PPC_NUMERICAL","a dominated underflowing term still refuses conservatively")
ok(identical(mul(matrix(c(1,-1),1,2),matrix(c(1,1),2,1)),matrix(0,1,1)),"exact cancellation of representable nonzero terms remains allowed")
ok(identical(mul(matrix(0,1,1),matrix(.1836433,1,1)),matrix(0,1,1)),"a declared structural zero product remains allowed")
ok(identical(mul(matrix(tiny,1,1),matrix(1,1,1)),matrix(tiny,1,1)),"a representable nonzero subnormal product remains allowed")
err(mul(matrix(.Machine$double.xmax,1,1),matrix(2,1,1)),"E_PPC_NUMERICAL","nonfinite scalar product is a typed refusal")
rp<-.bh_ppc_forward_plan(bmeas(bmf(residual=list(targets=~x)),data.frame(id=c("a","b"),x=c(.2,.3)),subject="id",fit=FALSE),"population")
rd<-setNames(rep(1,length(rp$required_variables)),rp$required_variables);rd["alpha[1]"]<-0;rd["D_parameter[1,1]"]<-tiny
set.seed(1);err(.bh_ppc_generate_eta(rp,rd),"E_PPC_NUMERICAL","iid residual map underflow uses the same guard")
rd["D_parameter[1,1]"]<-1;rd["L_parameter[1,1]"]<-2
err(.bh_ppc_generate_eta(rp,rd),"E_PPC_DRAW","population residual correlation factor role is enforced")
people<-c("a","b","c")
cp<-.bh_ppc_forward_plan(bmeas(bmf(residual=list(targets=~x),person=list(distribution="sample_centered",ids=people),augmentation=list(type="task_cholesky",targets=~x)),data.frame(id=people,x=c(.2,.3,.4)),subject="id",fit=FALSE),"population")
cd<-setNames(rep(1,length(cp$required_variables)),cp$required_variables);cd["alpha[1]"]<-0;cd["L_task_work[1,1]"]<-tiny
set.seed(1);err(.bh_ppc_generate_eta(cp,cd),"E_PPC_NUMERICAL","centered task factor multiplication underflow uses the same guard")
cd["L_task_work[1,1]"]<-1;cd["task_row_scale[1]"]<-tiny
set.seed(1);err(.bh_ppc_generate_eta(cp,cd),"E_PPC_NUMERICAL","centered row scaling retains its explicit underflow guard")
cat("PASS total",passed,"total synthetic forward-law checks including the numerical amendment\n")
