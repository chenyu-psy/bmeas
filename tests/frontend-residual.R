source(file.path("helpers", "native-guard.R"))
# Residual covariance is a separate predictor layer, not observation noise.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch({force(x);NULL},error=identity);ok(inherits(e,'measurement_error')&&e$code==code,code)}
d<-data.frame(person=rep(paste0('p',1:4),2))
for(j in 1:8)d[[paste0('x',j)]]<-sin(seq_len(8)*j/7)+j/10
prepare<-function(m,prior=NULL,data=d)bhmm(m,data,subject='person',prior=prior,fit=FALSE)
s11<-prepare(bhmf(G=~x1+x2+x3,residual=list(targets=~x1+x2+x3)))
i<-check_model(s11)$identification
ok(i$status=='local_rank_full_not_global_proof'&&i$free_covariance_parameters==9L&&all(i$probe_ranks==9L),'S11 repeated common plus residual rank9')
ok(nrow(model_table(s11,'loadings'))==3L&&nrow(model_table(s11,'parameter_residuals'))==3L,'common loading and residual SD roles separate')
ok(sum(get_prior(s11)$class=='parameter_sd')==3L&&sum(get_prior(s11)$class=='sigma')==3L,'observation and parameter SD each get one prior')
ok(sum(vapply(s11$data_map$blocks,nrow,integer(1)))==24L,'adding residuals never duplicates observation rows')
ok(check_model(s11)$backend$status=='deterministic_gaussian_qualified','bounded residual Gaussian native gate is distinct from HMC/inference')
no_repeat<-prepare(bhmf(G=~x1+x2+x3,residual=list(targets=~x1+x2+x3)),data=d[1:4,])
ok(check_model(no_repeat)$identification$status=='proven_unidentified'&&check_model(no_repeat)$identification$covariance_df== -3L,'S11 no repeats cannot separate two noise layers')
err(stancode(no_repeat),'E_IDENTIFICATION')
s18<-prepare(bhmf(residual=list(targets=~x1+x2+x3,cor='free')))
ok(nrow(model_table(s18,'components'))==0L&&nrow(model_table(s18,'loadings'))==0L,'S18 direct covariance has no scientific components/loadings')
ok(all(check_model(s18)$identification$probe_ranks==9L)&&sum(get_prior(s18)$class=='loading')==0L,'S18 rank9 with no fake loading prior')
sd<-standata(s18)
ok(sd$K==0L&&sd$L==0L&&sd$A==0L&&sd$U==3L&&sd$EU==3L,'empty common dimensions lower correctly')
ok(length(sd$loading_scale)==0L&&length(sd$parameter_sd_scale)==3L,'empty loading prior and parameter SD prior arrays')
mask<-prepare(bhmf(residual=list(targets=~x1+x2+x3,cor=list(default='free',zero=list(~x1+x2)))))
mk<-model_table(mask,'parameter_correlation_mask')
ok(mk[1,2]=='zero'&&sum(mk[upper.tri(mk)]=='free')==2L,'S18 exact residual marginal-zero mask')
only_one<-prepare(bhmf(residual=list(targets=~x1)))
ok(standata(only_one)$U==1L&&standata(only_one)$EU==0L&&all(check_model(only_one)$identification$probe_ranks==2L),'one target direct repeated covariance and zero-edge branch')
sparse_data<-d;sparse_data[5:8,c('x2','x3')]<-NA_real_
sparse<-bhmm(bhmf(G=~x1+x2+x3,residual=list(targets=~x1)),sparse_data,subject='person',missing='observed_only',fit=FALSE)
ok(all(check_model(sparse)$identification$probe_ranks==7L)&&nrow(model_table(sparse,'parameter_residuals'))==1L,'sparse residual requires repeat only where u exists')
s12form<-bhmf(G=~x1+x2+x3+x4,residual=list(targets=~x1+x2+x3+x4,cor=list(default='zero',free=list(~x1+x2))))
s12<-prepare(s12form)
ok(all(check_model(s12)$identification$probe_ranks==13L),'S12 residual edge plus common factor rank13')
missing_pair<-d;missing_pair$x1[c(1,2,5,6)]<-NA;missing_pair$x2[c(3,4,7,8)]<-NA
negative<-bhmm(s12form,missing_pair,subject='person',missing='observed_only',fit=FALSE)
id<-check_model(negative)$identification
ok(id$status=='proven_unidentified'&&length(id$uninformed_parameter_correlations)==1L,'common factor cannot invent residual-endpoint overlap')
both<-prepare(bhmf(A=~x1+x2+x3+x4,B=~x5+x6+x7+x8,
 residual=list(targets=~x1+x2+x3+x4+x5+x6+x7+x8,cor=list(default='zero',free=list(~x1+x2)))),
 prior=c(set_prior('lkj(2.5)','cor'),set_prior('lkj(3.5)','cor',layer='parameter'),set_prior('half_normal(.7)','parameter_sd')))
ok(all(check_model(both)$identification$probe_ranks==26L),'both nonempty correlation blocks give rank26')
bs<-standata(both);pr<-get_prior(both)
ok(bs$cor_eta==2.5&&bs$parameter_cor_eta==3.5&&bs$E==1L&&bs$EU==1L,'distinct correlation priors lowered to separate layers')
ok(all(bs$parameter_sd_dist==4L)&&all(bs$parameter_sd_scale==.7),'parameter SD half-normal is not a loading Normal prior')
ok(sum(pr$class=='cor')==2L&&sum(pr$parameter_count[pr$class=='structural_latent'])==40L,'one prior per matrix and correct latent multiplicities')
eta_small<-prepare(bhmf(residual=list(targets=~x1+x2+x3,cor='free')),prior=set_prior('lkj(.1)','cor',layer='parameter'))
ok(check_model(eta_small)$numerical_qualification$status=='execution_unqualified_prior'&&standata(eta_small)$parameter_cor_eta==.1,'parameter-layer eta<1 is retained but execution unqualified')
err(prepare(bhmf(residual=list(targets=~x1+x2+x3,cor='free')),prior=set_prior('lkj(3)','cor')),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','parameter_sd'),'E_PRIOR')
err(set_prior('half_normal(1)','parameter_sd',component='A'),'E_PRIOR_SELECTOR')
ok(inherits(bhmf(),'bhmm_formula'),'empty structure is a declaration for explicit task/fixed blocks; unresolved subject targets still fail at binding')
err(bhmf(residual=list(targets=~x1+x1)),'E_DUPLICATE_LOADING')
err(bhmf(residual=list(targets=~x1+x2,cor=list(default='zero',free=list(~x1+x3)))),'E_CORRELATION_TARGET')
err(bhmf(residual=list(targets=~x1+x2,cor=list(default='zero',free=list(~x1+x2),zero=list(~x1+x2)))),'E_CORRELATION_CONFLICT')
err(bhmf(G=~x1+x2+x3,residual=list(targets=~x1+x2+x3),person='sample_centered'),'E_PERSON_LAW')
long<-data.frame(person=rep(1:4,6),task=rep(c('t1','t2','t3'),each=8),y=sin(1:24))
long$observation<-paste0('o',seq_len(nrow(long)))
b<-observe(gaussian_model(),long,'person','task','y',units='score',observation_id='observation')
tasks<-c('t1','t2','t3');f<-bhmf(residual=list(targets=~scores$mu[tasks],cor=list(default='free',zero=list(~scores$mu['t1']+scores$mu['t2']))));tasks<-'changed'
explicit<-bhmm(f,list(scores=b),fit=FALSE)
ok(standata(explicit)$U==3L&&standata(explicit)$EU==2L,'qualified selector capture and residual edge resolution')
err(bhmf(residual=list(targets=~scores$mu[c('t1','t2')],cor=list(default='free',zero=list(~t1+t2)))),'E_CORRELATION_TARGET')
grouped<-bhmf(residual=list(targets=~x1+x2+x3+x4,cor=list(default='free',zero=list(~(x1+x2):(x3+x4)))))
ok(sum(grouped$residual$correlations$state=='zero')==4L,'grouped residual selectors set only four cross edges')
wrapped<-bhmf(A=~x1+x2+x3,B=~x4+x5+x6,C=~x1+x4+x7,D=~x2+x5+x8,cor=list(default='free',zero=list(~((A+B):(C+D)))))
ok(sum(wrapped$correlations$state=='zero')==4L,'outer parentheses preserve common cross-group semantics')
# Independent direct covariance arithmetic, including repeat versus observation variance.
layout<-private('.bh_covariance_layout')(both);v<-c(seq(.4,.75,length.out=8),.2,seq(.3,.65,length.out=8),-.15,rep(.6,8))
mom<-private('.bh_covariance_moments')(both);got<-private('.bh_moment_function')(both,mom,v)
Lam<-matrix(0,8,2);Lam[1:4,1]<-v[1:4];Lam[5:8,2]<-v[5:8];Rc<-matrix(c(1,.2,.2,1),2);Ru<-diag(8);Ru[1,2]<-Ru[2,1]<- -.15
D<-diag(v[10:17]);Sigma<-Lam%*%Rc%*%t(Lam)+D%*%Ru%*%D
expected<-Sigma[cbind(mom$row,mom$col)]+ifelse(mom$kind=='variance',.6,0)
ok(max(abs(got-expected))<1e-12,'two-layer moment map equals independent matrix arithmetic')
cat('Frontend residual checks:',passed,'passed. No native compilation or sampling.\n')
# Cached summaries keep the two covariance spaces separate, including K=0.
local({
  requests<-list()
  backend<-list(output=function()character(),diagnostic_summary=function(quiet)list(num_divergent=0,num_max_treedepth=0),
    summary=function(variables,...){requests$monitored<<-variables;data.frame(variable=c('alpha[1]','parameter_sd[1]','obs_sd[1]','R_parameter[1,2]'),mean=0,median=0,sd=1,q5=-1,q95=1,rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.4)},
    draws=function(variables,format){requests$matrices<<-c(requests$matrices,variables);n<-if(variables=='L_component')2L else 3L
      matrix(as.vector(diag(n)),1L,n*n,dimnames=list(NULL,paste0(variables,'[',rep(seq_len(n),n),',',rep(seq_len(n),each=n),']')))})
  cache<-private('.bh_cache_scalar_fit')(backend,s18,list())
  ok(!any(c('loading','R_component','z_raw')%in%requests$monitored)&&all(c('parameter_sd','R_parameter','e_raw')%in%requests$monitored),'direct cache requests no fake common outputs')
  ok(cache$parameter_summary$parameter_key[2]==model_table(s18,'parameter_residuals')$parameter[1],'parameter SD summary key matches prior key')
  ok(startsWith(cache$parameter_summary$parameter_key[4],'parameter_residual_correlation:'),'residual correlation summary retains scientific layer')
  ok(identical(unique(cache$admissibility$draws$layer),'parameter'),'direct numerical screen assesses only actual correlation layer')
  fake_fit<-structure(list(spec=s18,cache=cache,run=list(chains=1,iter_warmup=10,iter_sampling=10,reduction='serial'),provenance=list(exploratory_rank_override=FALSE)),class='bhmm_fit')
  printed<-capture.output(print(summary(fake_fit)))
  ok(any(grepl('0 common component groups, 0 scalar coordinates, 0 shared amplitudes; 3 parameter residuals;',printed,fixed=TRUE)),'direct printed summary names zero common factors and three residual targets')
  ok(identical(posterior_summary(fake_fit),cache$parameter_summary),'new residual summary remains cache-only with absent backend')
})
cat('Frontend residual checks including caches:',passed,'passed. No native compilation or sampling.\n')

empty_init<-private('.bh_backend_ir')(s18)$init
ok(!any(c('loading_anchor','loading_free','correlation_raw','z_raw')%in%names(empty_init)),'zero-coordinate fixed initialization omits unrepresentable empty matrix JSON')
ok(all(c('alpha','parameter_sd','parameter_correlation_raw','e_raw','obs_sd')%in%names(empty_init)),'direct initialization retains every actual parameter coordinate')
cat('Frontend residual checks including empty initialization:',passed,'passed. No native execution.\n')
