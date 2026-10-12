# Publicly prepared synthetic specs; never invokes a native backend.
residual_product_fixtures <- function() {
  d<-data.frame(person=rep(1:2,each=8),task=rep(paste0('t',1:8),2));d$y<-rep((1:8)/10,2)+rep(c(.125,.375),each=8);d$observation<-seq_len(nrow(d))
  block<-observe(gaussian_model(),d,'person','task','y',units='points',observation_id='observation',parameters=list(sigma=list(fixed=1)))
  target<-function(j)paste0('score$mu["t',j,'"]');f<-function(x)as.formula(paste('~',x));pair<-function(i,j)f(paste(target(i),target(j),sep=':'));fixed<-function(i,j,v)list(pairs=pair(i,j),value=v)
  residual<-list(targets=f(paste(vapply(1:8,target,''),collapse='+')),cor=list(default='zero',equal=list(shared=list(pair(1,2),pair(4,5))),free=list(pair(7,8)),fixed=list(fixed(1,3,.2),fixed(2,3,.3),fixed(4,6,-.3),fixed(5,6,.1))))
  configs<-list(residual_only=list(K=0L,cor='independent'),common_fixed=list(K=2L,cor=list(default='zero',fixed=list(list(pairs=~A:B,value=.25)))),common_mgs=list(K=2L,cor='free'),common_one=list(K=3L,cor=list(default='zero',free=list(~A:B),fixed=list(list(pairs=~A:C,value=.9),list(pairs=~B:C,value=.9)))),dual_product=list(K=8L,cor=list(default='zero',equal=list(shared=list(~A:B,~D:E)),free=list(~G:H),fixed=list(list(pairs=~A:C,value=.2),list(pairs=~B:C,value=.3),list(pairs=~D:F,value=-.3),list(pairs=~E:F,value=.1)))))
  lapply(configs,function(cfg) {
    comps<-setNames(lapply(seq_len(cfg$K),function(j)f(target(j))),LETTERS[seq_len(cfg$K)])
    args<-c(comps,list(cor=cfg$cor,residual=residual));if(cfg$K)args$loadings<-list(fixed=list(list(cells=comps,value=1)))
    prior<-c(set_prior('normal(0,1)','Intercept'),set_prior('half_normal(.7)','parameter_sd'),set_prior('lkj(1.75)','cor',layer='parameter'))
    if(cfg$K>2||identical(cfg$cor,'free'))prior<-c(prior,set_prior('lkj(2.25)','cor'))
    bmeas(do.call(bmf,args),list(score=block),subjects='shared',fit=FALSE,prior=prior)
  })
}
