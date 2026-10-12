source(file.path("helpers", "native-guard.R"))
# Semantic selector/refinement tests; no native compilation, sampling, or simulation.
source_mode<-nzchar(Sys.getenv('MEASUREMENTPKG_SOURCE'))
if(source_mode)for(f in list.files(file.path(Sys.getenv('MEASUREMENTPKG_SOURCE'),'R'),full.names=TRUE))source(f)else library(bmeas)
private<-function(n)if(source_mode)get(n,.GlobalEnv)else getFromNamespace(n,'bmeas')
passed<-0L;ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS',label,'\n')}
err<-function(x,code){e<-tryCatch({force(x);NULL},error=identity);ok(inherits(e,'measurement_error')&&e$code==code,code)}
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
