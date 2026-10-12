source(file.path("helpers", "native-guard.R"))
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
count<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);count<<-count+1L}
owner<-function(targets,components,residual=TRUE)list(type='variance_share',targets=targets,components=components,residual=residual,shares=~1)
d<-data.frame(id=rep(1:2,2),x1=c(.1,.4,.2,.6),x2=c(.8,.2,.7,.3),x3=c(.2,.5,.4,.9))
no_owner<-bhmm(bhmf(G=~x1+x2+x3),d,subject='id',fit=FALSE)
only<-bhmm(bhmf(decomposition=list(a=owner(~x1,~0))),d[,1:2],subject='id',fit=FALSE)
shared<-bhmm(bhmf(G=~x1+x2,decomposition=list(a=owner(~x1+x2,~G))),d[,1:3],subject='id',fit=FALSE,
 prior=c(set_prior('normal(.3,.7)','share'),set_prior('half_normal(.6)','total_sd')))
for(s in list(no_owner,only,shared)) {
 data<-standata(s,threads=2);ir<-private('.bh_backend_ir')(s,TRUE)
 ok(data$UF==nrow(private('.bh_free_parameter_sd')(s)),'free residual count distinct from rows')
 ok(all((data$parameter_sd_index>0L)+(data$parameter_share_weight>0L)==1L),'one residual SD expression per row')
 ok(length(ir$init$parameter_sd)==data$UF&&length(ir$init$total_sd)==data$T&&length(ir$init$share_beta)==data$H,'initialization follows unique primitive counts')
 code<-as.character(stancode(s,threads=2,log_lik=TRUE))
 ok(length(gregexpr('share_beta ~ normal',code,fixed=TRUE)[[1L]])==1L,'one beta prior statement')
 ok(!grepl('dirichlet|log\\(share_weight',code),'no simplex-density Jacobian inserted')
 if(requireNamespace('cmdstanr',quietly=TRUE)&&requireNamespace('jsonlite',quietly=TRUE)) {
  file<-tempfile(fileext='.json');cmdstanr::write_stan_json(data,file)
  decoded<-jsonlite::fromJSON(file,simplifyVector=FALSE)
  ok(!'representation_projection'%in%names(decoded),'projection provenance is not serialized as Stan data')
  for(field in c('share_target','share_weight_start','share_weight_count','share_weight_total_sd','share_cell_target','share_cell_component','share_cell_weight','share_cell_multiplier','parameter_sd_index','parameter_share_weight','total_sd_dist','total_sd_scale','total_sd_df','share_beta_location','share_beta_scale'))ok(is.list(decoded[[field]]),paste('array/vector JSON rank',field))
  ok(is.list(decoded$share_design)&&length(decoded$share_design)==data$W,'matrix JSON retains weight rows with zero beta')
  unlink(file)
 }
}
z<-standata(no_owner);ok(all(unlist(z[c('T','H','W','V')])==0),'no-owner lowering explicitly adds zero-coordinate share fields')
ok(isTRUE(attr(z,'representation_projection')$empty_owner),'empty-owner projection provenance visible')
z<-standata(only);ok(z$K==0L&&z$U==1L&&z$UF==0L&&z$T==1L&&z$H==0L&&z$V==0L,'residual-only one-part counts')
z<-standata(shared);ok(z$H==1L&&z$UF==0L&&z$U==2L&&z$A==0L&&z$F==0L,'owned Gaussian share has no phantom free loadings or u SDs')
ok(identical(as.numeric(z$share_beta_location),.3)&&identical(as.numeric(z$share_beta_scale),.7),'explicit beta prior lowers once on original coordinate')
# The emitted linear predictor agrees with the captured X/B/C calculation.
beta<-.23;linear<-as.numeric(z$share_design%*%beta);flat<-numeric(z$W)
for(t in seq_len(z$T)){at<-z$share_weight_start[t]+seq_len(z$share_weight_count[t])-1L;v<-linear[at];flat[at]<-exp(v-max(v))/sum(exp(v-max(v)))}
expected<-private('.bh_share_weights')(shared,beta)[[1]]
ok(max(abs(flat-as.vector(t(expected))))<1e-14,'flattened active-part map equals X/B/C weights')
# Derived rows and independent primitives have separate stable cache identities.
variables<-c('alpha[1]','alpha[2]','obs_sd[1]','obs_sd[2]','total_sd[1]','total_sd[2]','share_beta[1]',paste0('share_weight[',1:4,']'),paste0('parameter_row_sd[',1:2,']'))
backend<-list(summary=function(...)data.frame(variable=variables,mean=0,median=0,sd=1,q5=-1,q95=1,rhat=NA_real_,ess_bulk=2,ess_tail=2,mcse_mean=.1),
 diagnostic_summary=function(...)data.frame(num_divergent=0,num_max_treedepth=0,ebfmi=.5),output=function()character(),draws=function(...)stop('mock matrix draws unavailable'))
cache<-private('.bh_cache_scalar_fit')(backend,shared,list())
keys<-cache$parameter_summary$parameter_key
ok(all(shared$tables$share_targets$parameter%in%keys)&&all(shared$tables$share_coefficients$parameter%in%keys),'cache labels unique total-SD and beta primitives')
ok(sum(grepl('^variance_share:',keys))==4L&&sum(grepl('^derived_parameter_sd:',keys))==2L,'cache labels derived shares/row-SDs without pretending they are free parameters')
ok(!anyNA(keys)&&!anyDuplicated(keys),'cached parameter identities remain distinct')
cat(count,'share lowering/schema/cache checks passed; no native execution\n')
ok(all(cache$parameter_summary$parameter_role[grepl('^share_weight',cache$parameter_summary$variable)]=='derived_variance_share'),'cached weights explicitly have derived roles')
local({
 old<-private('.bh_compile_scalar');replace<-function(f)if(source_mode)assign('.bh_compile_scalar',f,.GlobalEnv)else assignInNamespace('.bh_compile_scalar',f,'bmeas')
 on.exit(replace(old));calls<-new.env();calls$n<-0L;calls$args<-NULL
 # The size receipt sees a real metadata file, never synthetic posterior CSV evidence.
 mock_output<-tempfile('backend-metadata-',fileext='.txt')
 writeLines('metadata-only backend double; not posterior data',mock_output)
 on.exit(unlink(mock_output),add=TRUE)
 mock<-list(return_codes=function()rep(0L,calls$args$chains),output_files=function()mock_output,output=function()character(),diagnostic_summary=function(...)data.frame(num_divergent=0,num_max_treedepth=0,ebfmi=.5),
  draws=function(variables,format){data<-calls$args$data;n<-if(variables=='L_component')data$K else data$U;matrix(as.vector(diag(n)),1,n*n,dimnames=list(NULL,paste0(variables,'[',rep(seq_len(n),n),',',rep(seq_len(n),each=n),']')))},
  summary=function(variables,...){data<-calls$args$data;v<-c(paste0('alpha[',seq_len(data$J),']'),if(data$T)paste0('total_sd[',seq_len(data$T),']'),if(data$H)paste0('share_beta[',seq_len(data$H),']'),if(data$W)paste0('share_weight[',seq_len(data$W),']'),if(data$UF)paste0('parameter_sd[',seq_len(data$UF),']'),if(data$U)paste0('parameter_row_sd[',seq_len(data$U),']'));data.frame(variable=v,mean=0,median=0,sd=1,q5=-1,q95=1,rhat=2,ess_bulk=5,ess_tail=4,mcse_mean=.4)})
 replace(function(spec,cache_dir,save_log_lik)list(model=list(sample=function(...){calls$n<-calls$n+1L;calls$args<-list(...);mock}),identity=list(code='explicit_share_test_double',toolchain=list(stanc_options=list(O0=TRUE))),cache_key='mock',cache_hit=TRUE,executable_sha256='mock'))
 f<-bhmm(only,chains=1,iter=20,warmup=10,output_dir=tempdir(),resources='development')
 ok(calls$args$data$T==1L&&calls$args$data$H==0L&&calls$args$data$UF==0L&&!any(c('share_beta','parameter_sd','z_raw')%in%names(calls$args$init())),'one-part dispatch omits every nonexistent beta/free-u/common coordinate')
 dr<-data.frame(id=rep(1:4,2));for(j in 1:4)dr[[paste0('x',j)]]<-sin((1:8)*j/7)+j/10
 q<-matrix(c(1,-1,1,-1),4,1,dimnames=list(paste0('x',1:4),'q'))
 s<-bhmm(bhmf(G=~x1+x2+x3+x4,P=component(~x1+x2+x3+x4,q,'shared'),residual=list(targets=~x4),
  decomposition=list(a=list(type='variance_share',targets=~x1+x2,components=~G+P,residual=TRUE,shares=~1))),dr,subject='id',prior=set_prior('half_normal(.5)','amplitude'),fit=FALSE)
 f<-bhmm(s,chains=1,iter=20,warmup=10,output_dir=tempdir(),resources='development');data<-calls$args$data;init<-calls$args$init()
 ok(data$U==3L&&data$UF==1L&&sum(data$parameter_sd_index>0L)==1L&&length(init$parameter_sd)==1L,'surviving free u keeps its one primitive after derived rows')
 ok(data$A==0L&&data$F==2L&&data$B==1L&&length(init$loading_free)==2L,'hybrid signed loadings and surviving h use correct unconstrained roles')
 key<-private('.bh_free_parameter_sd')(s)$parameter
 ok(identical(f$cache$parameter_summary$parameter_key[f$cache$parameter_summary$variable=='parameter_sd[1]'],key),'free-u summary key follows its actual residual row rather than first owner row')
 saved<-capture.output(print(summary(f)));f$backend<-NULL
 ok(identical(saved,capture.output(print(summary(f))))&&identical(posterior_summary(f),f$cache$parameter_summary),'share summary and posterior table are backend-free')
 ok(f$cache$backend_qualification$evidence_version=='bmeas_affine_correlation_fixed_states_v1'&&f$cache$inference$status=='not_assessed','cache keeps fixed-state evidence separate from inference')
})
cat(count,'share lowering final lifecycle/cache checks passed; only explicit doubles\n')
