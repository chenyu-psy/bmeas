source(file.path("helpers", "native-guard.R"))
# Public identity aliases, captured formulas and sealed objects; no native work.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
ok(identical(bmeas,bhmm)&&identical(bmf,bhmf),'current and transitional entries share exact semantics and defaults')
d<-data.frame(person=1:5,x1=c(.1,.3,.9,-.2,.6),x2=c(.5,-.8,.4,.3,.7),x3=c(.4,.2,.8,.5,-.3))
f<-bmf(G=~x1+x2+x3);s<-bmeas(f,d,subject='person',fit=FALSE)
ok(identical(class(f),c('bmeas_formula','bhmm_formula'))&&identical(class(s),c('bmeas_spec','bhmm_spec')),'new objects have stable distinct S3 dispatch and legacy inheritance')
ok(identical(f,bhmf(G=~x1+x2+x3))&&identical(s,bhmm(f,d,subject='person',fit=FALSE)),'alias constructors preserve complete captured models')
ok(startsWith(capture.output(print(f))[1L],'bmeas measurement declaration')&&startsWith(capture.output(print(s))[1L],'Prepared bmeas component model'),'main public print methods use bmeas branding')
choice<-c('x1','x2','x3');long<-data.frame(person=rep(1:4,3),task=rep(choice,each=4),y=sin(1:12))
a<-observe(gaussian_model(),long,'person','task','y',units='score');ff<-bmf(G=~scores$mu[choice]);choice<-'changed'
z<-bmeas(ff,list(scores=a),fit=FALSE)
ok(setequal(model_table(z,'targets')$task,c('x1','x2','x3')),'formula alias keeps caller selector capture safe and stable')
raw<-serialize(s,NULL,version=2);tmp<-tempfile();saveRDS(s,tmp);again<-readRDS(tmp);unlink(tmp)
ok(identical(model_table(s),model_table(again))&&identical(check_model(s),check_model(again)),'current namespace RDS preserves tables and diagnostics')
ok(identical(raw,serialize(s,NULL,version=2))&&identical(s,bmeas(s,fit=FALSE)),'inspection and reuse do not reseal or reclassify a saved object')
cat('Frontend identity checks:',passed,'passed. No compiler or sampler invoked.\n')
