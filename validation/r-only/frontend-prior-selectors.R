source(file.path("helpers", "native-guard.R"))
# Semantic selector/refinement tests; no native compilation, sampling, or simulation.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch({force(x);NULL},error=identity);ok(inherits(e,'measurement_error')&&e$code==code,code)}
d<-data.frame(person=rep(1:6,2),x1=sin(1:12),x2=cos(1:12),x3=sin((1:12)/3),x4=cos((1:12)/2))
f<-bhmf(G=~x1+x2+x3+x4,residual=list(targets=~x1+x2+x3+x4))
prep<-function(prior,formula=f,data=d)bhmm(formula,data,subject='person',prior=prior,fit=FALSE)
table_values<-function(x){attr(x,'prior_resolution')<-NULL;x<-x[order(x$parameter),c('parameter','class','target','component','layer','prior','source')];rownames(x)<-NULL;x}
prior_values<-function(spec)table_values(get_prior(spec))
public_table_equal<-function(table,spec){attr(table,'prior_resolution')<-NULL;identical(table,get_prior(spec))}
# Rule-order matrices exercise the real resolver on one real prepared context;
# public preparation is retained once per rule family and for all API boundaries.
selector_base<-prep(NULL)
resolve<-function(rules,spec=selector_base)private('.bh_prior_table')(spec,do.call(c,rules))
prior_data<-function(spec)private('.bh_prior_data')(spec)
permutations<-function(x){if(length(x)<2L)return(list(x));unlist(lapply(seq_along(x),function(i)lapply(permutations(x[-i]),function(y)c(x[i],y))),recursive=FALSE)}
grouped<-c(set_prior('normal(0,.4)','loading',target=~x1+x2),set_prior('normal(1,2)','Intercept',target=~x1+x2),
 set_prior('half_normal(.7)','parameter_sd',target=~x1+x2),set_prior('half_normal(.8)','sigma',target=~x1+x2))
expanded<-c(set_prior('normal(0,.4)','loading',target='scores::mu::x1'),set_prior('normal(0,.4)','loading',target='scores::mu::x2'),
 set_prior('normal(1,2)','Intercept',target='scores::mu::x1'),set_prior('normal(1,2)','Intercept',target='scores::mu::x2'),
 set_prior('half_normal(.7)','parameter_sd',target='scores::mu::x1'),set_prior('half_normal(.7)','parameter_sd',target='scores::mu::x2'),
 set_prior('half_normal(.8)','sigma',target='scores::sigma::x1'),set_prior('half_normal(.8)','sigma',target='scores::sigma::x2'))
a<-prep(grouped);b<-prep(expanded)
ok(identical(prior_values(a),prior_values(b)),'group expansion equals explicit per-target prior table')
ok(identical(prior_data(a),prior_data(b))&&identical(stancode(a),stancode(b)),'grouping changes neither numeric prior data nor native source')
ok(!anyDuplicated(model_table(a,'parameters')$parameter)&&nrow(model_table(a,'parameters'))==nrow(model_table(b,'parameters')),'grouping introduces no shared/equal or duplicated unknowns')
rules<-list(set_prior('normal(0,1)','loading'),set_prior('normal(0,.5)','loading',target=~x1+x2+x3+x4),set_prior('normal(0,.2)','loading',target=~x1))
reference<-prep(do.call(c,rules))
ok(public_table_equal(resolve(rules),reference),'public refinement agrees with direct resolution on the same scientific context')
for(order in permutations(1:3))ok(identical(table_values(resolve(rules[order])),prior_values(reference)),'global/group/singleton refinement is order-independent')
x<-get_prior(reference);x<-x[x$class=='loading',]
ok(x$prior[x$target=='scores::mu::x1']=='normal(0,.2)'&&all(x$prior[x$target!='scores::mu::x1']=='normal(0,.5)'),'explicit all-target restriction refines NULL even when sets cover the same model')
component_rules<-list(set_prior('normal(0,1)','loading'),set_prior('normal(0,.3)','loading',component='G'))
for(order in list(1:2,2:1)){s<-prep(do.call(c,component_rules[order]));ok(all(get_prior(s)$prior[get_prior(s)$class=='loading']=='normal(0,.3)'),'single existing component still refines unrestricted component predicate')}
# Sparse overlap has unresolved Gaussian ID, but its prepared prior table stays inspectable.
overlap<-bhmf(G=~x1+x2+x3+x4,S=~x1+x2,cor='independent')
intersection_rules<-list(set_prior('normal(0,1)','loading',component='G'),set_prior('normal(0,.5)','loading',target=~x1),
 set_prior('normal(0,.2)','loading',component='G',target=~x1))
overlap_base<-prep(NULL,formula=overlap)
reference<-prep(do.call(c,intersection_rules),formula=overlap)
ok(public_table_equal(resolve(intersection_rules,overlap_base),reference),'public intersection resolution retains the complete prior table and metadata')
x<-get_prior(reference);g1<-x$class=='loading'&x$component=='G'&x$target=='scores::mu::x1'
ok(sum(g1)==1L&&x$prior[g1]=='normal(0,.2)','intersection singleton receives its explicit literal density')
for(order in permutations(1:3))ok(identical(table_values(resolve(intersection_rules[order],overlap_base)),prior_values(reference)),'third intersection rule resolves all six input orders')
for(order in list(1:2,2:1))err(prep(do.call(c,intersection_rules[order]),formula=overlap),'E_PRIOR_CONFLICT')
nested<-list(set_prior('normal(0,.9)','loading',target=~x1+x2+x3+x4),set_prior('normal(0,.7)','loading',target=~x1+x2+x3),
 set_prior('normal(0,.5)','loading',target=~x1+x2),set_prior('normal(0,.2)','loading',target=~x1))
nested_public<-prep(do.call(c,nested));ref<-prior_values(nested_public)
ok(public_table_equal(resolve(nested),nested_public),'public nested resolution retains the complete prior table and metadata')
expected_nested<-c('scores::mu::x1'='normal(0,.2)','scores::mu::x2'='normal(0,.5)','scores::mu::x3'='normal(0,.7)','scores::mu::x4'='normal(0,.9)')
x<-ref[ref$class=='loading',]
ok(identical(unname(x$prior),unname(expected_nested[x$target])),'nested target densities equal the independent literal refinement map')
for(order in permutations(1:4))ok(identical(table_values(resolve(nested[order])),ref),'nested group/subgroup/singleton precedence across24 orders')
non_nested<-list(set_prior('normal(0,.7)','loading',target=~x1+x2),set_prior('normal(0,.5)','loading',target=~x2+x3),set_prior('normal(0,.2)','loading',target=~x2))
non_nested_public<-prep(do.call(c,non_nested))
ok(public_table_equal(resolve(non_nested),non_nested_public),'public incomparable-group refinement agrees with direct resolution')
for(order in permutations(1:3)){x<-resolve(non_nested[order]);ok(x$prior[x$class=='loading'&x$target=='scores::mu::x2']=='normal(0,.2)','an explicit overlap subset resolves incomparable target groups')}
err(prep(c(non_nested[[1]],non_nested[[2]])),'E_PRIOR_CONFLICT')
err(prep(c(set_prior('normal(0,1)','loading',target='scores::mu::x1'),set_prior('normal(0,.5)','loading',target=~scores$mu['x1']))),'E_PRIOR_CONFLICT')
err(prep(set_prior('normal(0,1)','loading',target=~x1+unknown)),'E_PRIOR_SELECTOR')
err(prep(set_prior('normal(0,1)','loading',component='S',target=~x1+x3),formula=overlap),'E_PRIOR_SELECTOR')
err(prep(set_prior('half_normal(1)','sigma',target=~scores$mu['x1'])),'E_PRIOR_SELECTOR')
err(prep(set_prior('half_normal(1)','sdlog',target=~x1)),'E_PRIOR_SELECTOR')
err(set_prior('lkj(2)','cor',target=~x1+x2),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','Intercept',component='G',target=~x1),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','loading',target=~x1+x1),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','loading',target=x1~x2),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','loading',target=~log(x1)),'E_FORMULA')
err(set_prior('normal(0,1)','loading',target=~scores$mu[paste0('x',1:2)]),'E_SELECTOR')
evaluated<-FALSE
err(set_prior('normal(0,1)','loading',target=~{evaluated<<-TRUE;x1}),'E_FORMULA')
ok(!evaluated,'prior formula language never evaluates arbitrary calls')
# Fully qualified role selectors in explicit blocks, including their own sigma.
long<-data.frame(person=rep(1:6,4),task=rep(c('t1','t2'),each=12),y=sin(1:24),observation=paste0('o',1:24))
block<-observe(gaussian_model(),long,'person','task','y',units='score',observation_id='observation')
tasks<-c('t1','t2');p<-c(set_prior('normal(2,3)','Intercept',target=~scores$mu[tasks]),set_prior('half_normal(.8)','sigma',target=~scores$sigma[tasks]))
m<-bhmf(residual=list(targets=~scores$mu[tasks],cor='free'));tasks<-'changed'
s<-bhmm(m,list(scores=block),prior=p,fit=FALSE)
ok(all(get_prior(s)$prior[get_prior(s)$class=='Intercept']=='normal(2,3)'),'prior task-vector capture happens at set_prior, before later mutation')
ok(all(get_prior(s)$prior[get_prior(s)$class=='sigma']=='half_normal(.8)'),'qualified sigma selector preserves literal observation-noise role')
err(bhmm(m,list(scores=block),prior=set_prior('normal(0,1)','Intercept',target=~t1+t2),fit=FALSE),'E_PRIOR_SELECTOR')
reordered<-long[nrow(long):1,];br<-observe(gaussian_model(),reordered,'person','task','y',units='score',observation_id='observation')
sr<-bhmm(m,list(scores=br),prior=p,fit=FALSE)
ok(identical(prior_values(sr),prior_values(s))&&identical(prior_data(sr),prior_data(s)),'row/subject reorder preserves resolved priors and numeric data')
p_restored<-unserialize(serialize(p,NULL,version=2));restored<-bhmm(m,list(scores=block),prior=p_restored,fit=FALSE)
ok(identical(prior_values(restored),prior_values(s)),'captured prior group survives serialization without retaining its formula environment')
changed<-p;changed[[1]]$target$targets[[1]]$task<-'changed'
err(bhmm(m,list(scores=block),prior=changed,fit=FALSE),'E_PRIOR_SELECTOR')
cat('Grouped prior selector checks:',passed,'passed. No native execution or sampling.\n')

err(bhmm(m,list(scores=block),prior=set_prior('normal(0,1)','Intercept',target=~scores$mu['t1']+scores$sigma['t2']),fit=FALSE),'E_PRIOR_SELECTOR')
long2<-long;long2$y<-2+long2$y*.7
block2<-observe(gaussian_model(),long2,'person','task','y',units='score',observation_id='observation',source_id='distinct_second_measure')
fm<-bhmf(G=~first$mu[c('t1','t2')]+second$mu[c('t1','t2')])
pm<-c(set_prior('normal(0,2)','Intercept',target=~first$mu[c('t1','t2')]+second$mu[c('t1','t2')]),set_prior('half_normal(1)','sigma'))
ms1<-bhmm(fm,list(first=block,second=block2),subjects='shared',prior=pm,fit=FALSE)
ms2<-bhmm(fm,list(second=block2,first=block),subjects='shared',prior=pm,fit=FALSE)
ok(identical(prior_values(ms1),prior_values(ms2)),'block order does not change resolved parameter-role priors')
# Capturing many task parameters never captures callback/formula environments.
ok(!any(vapply(unclass(pm),function(r)is.environment(r$target),logical(1))),'prior selectors retain values rather than formula environments')
cat('Grouped prior selector checks including multi-block roles:',passed,'passed. No native execution.\n')

active_env<-new.env(parent=globalenv());active_called<-FALSE
makeActiveBinding('tasks',function(){active_called<<-TRUE;c('t1','t2')},active_env)
active_formula<-as.formula('~scores$mu[tasks]',env=active_env)
err(set_prior('normal(0,1)','Intercept',target=active_formula),'E_SELECTOR')
err(bhmf(G=active_formula),'E_SELECTOR')
ok(!active_called,'active bindings are rejected before calling their function')
child_env<-new.env(parent=active_env)
err(set_prior('normal(0,1)','Intercept',target=as.formula('~scores$mu[tasks]',env=child_env)),'E_SELECTOR')
ok(!active_called,'inherited active bindings are also rejected without evaluation')
# Standard R lazy arguments are intentionally forced to capture their value.
forced<-FALSE
capture_lazy<-function(tasks)set_prior('normal(0,1)','Intercept',target=~scores$mu[tasks])
lazy<-capture_lazy({forced<<-TRUE;c('t1','t2')})
ok(forced&&inherits(lazy,'bhmm_prior'),'ordinary R promise forcing is distinct from evaluating the selector DSL')
cat('Grouped prior selector checks including binding guards:',passed,'passed. No native execution.\n')

local({
  method_name<-'as.character.bhmm_selector_tripwire';had<-exists(method_name,globalenv(),inherits=FALSE)
  if(had)old<-get(method_name,globalenv())
  called<-FALSE;assign(method_name,function(x,...){called<<-TRUE;unclass(x)},globalenv())
  on.exit(if(had)assign(method_name,old,globalenv())else rm(list=method_name,envir=globalenv()))
  task_values<-structure(c('t1','t2'),class='bhmm_selector_tripwire')
  err(set_prior('normal(0,1)','Intercept',target=~scores$mu[task_values]),'E_SELECTOR')
  err(bhmf(G=~scores$mu[task_values]),'E_SELECTOR')
  ok(!called,'classed character selectors reject before coercion-method dispatch')
})
cat('Grouped prior selector checks including plain-value guard:',passed,'passed. No native execution.\n')

err(set_prior('normal(0,1)','loading',target=structure('scores::mu::x1',class='unexpected_selector_class')),'E_PRIOR_SELECTOR')
err(set_prior('normal(0,1)','loading',component=structure('G',class='unexpected_selector_class')),'E_PRIOR_SELECTOR')
cat('Grouped prior selector final checks:',passed,'passed. No native execution.\n')
