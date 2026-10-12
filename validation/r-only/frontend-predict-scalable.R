source(file.path('helpers','native-guard.R'))
root <- Sys.getenv('MEASUREMENTPKG_SOURCE')
source_mode <- nzchar(root)
if(source_mode)for(f in list.files(file.path(root,'R'),full.names=TRUE))source(f)else library(bmeas)
ns <- if(source_mode).GlobalEnv else asNamespace('bmeas')
for(n in c('.bh_pf_ppc_plan','.bh_ppc_geometry_admit','.bh_ppc_stats_empty',
           '.bh_ppc_aggregate','.bh_ppc_statistics_plan','.bh_ppc_forward_plan'))
  assign(n,get(n,ns,inherits=FALSE))
set_private <- function(name,value) {
  locked <- bindingIsLocked(name,ns)
  if(locked)unlockBinding(name,ns)
  assign(name,value,ns)
  if(locked)lockBinding(name,ns)
}
# Synthetic metadata only; no posterior acquisition or native execution.
d <- data.frame(id=paste0('p',rep(1:31,length.out=1001)),x=seq_len(1001)/1001)
s <- bmeas(bmf(residual=list(targets=~x)),d,subject='id',fit=FALSE)
f <- structure(list(spec=s,code="SYNTHETIC ENGINEERING ONLY",cache=list(),provenance=list(output_files='absent.csv',model_fingerprint=s$fingerprint)),class=c('bmeas_fit','bhmm_fit'))
b <- diagnostic_budget(21,1001,21021,max_latent_values=30000,max_statistics=300,max_work_values=2000000,
 posterior=list(reader='streaming',max_source_bytes=1048576,max_read_bytes=7340032,max_materialized_values=200000))
p <- ppc_plan(f,21,b,level='population')
stopifnot(p$geometry$N==31,p$observations_per_rep==1001,p$replications==21,p$version=='bmeas_basic_ppc_plan_v3')
.bh_pf_ppc_plan(p,'boundary')
fail <- function(expr)stopifnot(inherits(tryCatch(force(expr),error=identity),'measurement_error'))
small<-b;small$max_work_values<-1L;fail(ppc_plan(f,21,small,level='population'))
fail(.bh_ppc_geometry_admit(.Machine$integer.max,.Machine$integer.max,0,0,0,0,b))
.bh_ppc_geometry_admit(31,13,13,0,13,0,b)
observed<-.bh_ppc_stats_empty(); observed<-observed[rep(NA_integer_,201),];observed$statistic_id<-paste0('s',1:201);observed$estimate<-0;observed$status<-'computed';observed$domain<-'marginal';observed$level<-'population'
reps<-matrix(0,21,201,dimnames=list(NULL,observed$statistic_id));invisible(.bh_ppc_aggregate(observed,reps))
cat('PASS N31 O1001 D21 geometry13 statistics201 explicit planning, v3 validation, low-budget and overflow refusal\n')
# Admission must precede forward scalar-name expansion.
local({
saved_names<-get('.bh_ppc_forward_names',ns,inherits=FALSE)
on.exit(set_private('.bh_ppc_forward_names',saved_names))
set_private('.bh_ppc_forward_names',function(...)stop('FORBIDDEN scalar-name allocation'))
small<-b;small$max_simulated_values<-1L
failure<-tryCatch(ppc_plan(f,21,small,level='population'),error=identity)
stopifnot(inherits(failure,'measurement_error'),failure$code=='E_DIAGNOSTIC_BUDGET')
})
# Complete 18-group plan: 72 marginal plus 153 pair statistics, no omission.
d2<-expand.grid(person=paste0('p',1:16),task=paste0('t',1:18),stringsAsFactors=FALSE);d2$y<-1
bl<-observe(gaussian_model(),d2,'person','task','y',units='points',parameters=list(mu=list(fixed=0),sigma=list(fixed=1)))
s2<-bmeas(bmf(),list(g=bl),fit=FALSE)
bp<-b;bp$max_pair_terms<-60000L
st<-.bh_ppc_statistics_plan(s2,21,bp,'population');stopifnot(nrow(st$statistics)==225,st$counts$pair_terms==53856)
fail(.bh_ppc_statistics_plan(s2,21,b,'population'))
# M3 complete O1001 data preparation is R-only, no simulator compilation/run.
resource<-Sys.getenv('MEASUREMENTPKG_M3_RESOURCE')
if(nzchar(resource)) {
 md<-data.frame(person=paste0('p',rep(1:31,length.out=1001)),task='a')
 md$obs<-paste0('o',seq_len(nrow(md)))
 md$counts<-I(matrix(0L,1001,10));md$counts[,1]<-1L;md$options<-I(matrix(1L,1001,10))
 mb<-observe(m3_cs_model(resource),md,'person','task',fields=list(counts='counts',options='options'),units='count',observation_id='obs',parameters=list(a=list(fixed=.2),c=list(fixed=.3),ra=list(fixed=.4),rc=list(fixed=.5)))
 ms<-bmeas(bmf(),list(cs=mb),fit=FALSE)
 mc<-b;mc$max_simulated_values<-210210L
 mf<-.bh_ppc_forward_plan(ms,'conditional',mc)
 dat<-m3cs_simulator_data(ms,matrix(NA_real_,mf$geometry$N,mf$geometry$J),budget=mc)
 code<-make_m3cs_simulator_code(ms,budget=mc)
 stopifnot(dat$O==1001L,!grepl('upper=1000',code,fixed=TRUE),nrow(dat$options)==1001L)
 cat('PASS M3 O1001 explicit-budget source/data preparation without native execution\n')
}
cat('PASS pre-allocation low-response refusal and225-statistic complete plan\n')

wide<-data.frame(id=paste0('p',1:3)); for(j in 1:13)wide[[paste0('x',j)]]<-c(.1,.2,.3)
sw<-bmeas(bmf(residual=list(targets=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)),wide,subject='id',fit=FALSE)
wp<-.bh_ppc_forward_plan(sw,'population',b)
stopifnot(wp$geometry$J==13,wp$geometry$U==13,wp$geometry$S==13)
cat('PASS full J13/U13/S13 forward plan and53856 paired-person terms with explicit allowance\n')
