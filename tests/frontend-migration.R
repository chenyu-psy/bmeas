source(file.path("helpers", "native-guard.R"))
# This schema deliberately requires explicit reconstruction of old contexts.
# Synthetic version-tagged negatives are distinct from genuine archived RDS
# witnesses checked independently; no old fingerprint is silently repaired.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch({force(x);NULL},error=identity);ok(inherits(e,'measurement_error')&&e$code==code,code)}
d<-data.frame(person=rep(1:4,2),x1=sin(1:8),x2=cos(1:8),x3=(1:8)/10)
current<-bhmm(bhmf(G=~x1+x2+x3),d,subject='person',fit=FALSE)
seal<-function(x){x$fingerprint<-NULL;x$fingerprint<-private('.mp_hash')(x);x}
entrypoints<-list(function(x)bhmm(x,fit=FALSE),model_table,get_prior,check_model,standata,stancode,
 function(x)bhmm(x,chains=1,iter=20,warmup=10,execution='experimental',resources='development'))
local({
 old_fit<-private('.bh_fit_scalar');calls<-0L
 replace<-function(f)if(source_mode)assign('.bh_fit_scalar',f,.GlobalEnv)else assignInNamespace('.bh_fit_scalar',f,'bmeas')
 on.exit(replace(old_fit));replace(function(...){calls<<-calls+1L;stop('FORBIDDEN_BACKEND')})
 for(version in c('coefficient-map-spec-v1','variance-share-spec-v1','parameter-level-spec-v1','loading-constraint-spec-v1','unknown-schema')) {
  old<-current;old$schema_version<-version;old<-seal(old);before<-serialize(old,NULL)
  for(f in entrypoints)err(f(old),'E_SCHEMA_VERSION')
  ok(identical(before,serialize(old,NULL)),'old version-tagged context is never mutated or resealed')
 }
 ok(calls==0L,'unsupported old contexts stop before any backend/cache path')
})
changed<-current;changed$data_map$blocks$scores$y[1]<-99
err(model_table(changed),'E_SPEC_CHANGED')
missing<-current;missing$tables$ordinary_loading_map<-NULL;missing<-seal(missing)
err(model_table(missing),'E_SCHEMA_VERSION')
# Captured old scalar formulas can still be explicitly rebuilt with data and
# intended priors. This constructs a fresh context, not a context migration.
oldf<-current$formula[c('components','correlations','component_names','person','scale','residual')]
oldf$residual<-'none';oldf$version<-'scalar-components-v1';class(oldf)<-'bhmm_formula';oldf<-seal(oldf)
prior<-c(set_prior('normal(0,1)','Intercept'),set_prior('normal(0,.7)','loading'),set_prior('half_normal(.8)','sigma'))
original<-serialize(oldf,NULL)
fresh<-suppressWarnings(bhmm(oldf,d,subject='person',prior=prior,fit=FALSE))
ok(fresh$schema_version=='reference-law-spec-v1'&&nrow(model_table(fresh,'loading_primitives'))==3L,'explicit old-formula rebuild creates a complete new primitive map')
ok(identical(original,serialize(oldf,NULL))&&fresh$formula$schema_migration$source_fingerprint==oldf$fingerprint,'formula rebuild records its original captured declaration')
ok(all(get_prior(fresh)$source[get_prior(fresh)$class=='loading']=='user'),'explicit reconstruction uses the supplied intended priors')
for(f in list(model_table,get_prior,check_model))ok(identical(f(fresh),f(unserialize(serialize(fresh,NULL)))),'fresh current-schema inspection persists without a migration')
cat(passed,'schema/rebuild boundary checks passed; no native calls\n')
