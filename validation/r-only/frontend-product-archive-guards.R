source(file.path('helpers','native-guard.R'))
root<-Sys.getenv('MEASUREMENTPKG_SOURCE')
if(nzchar(root))for(f in list.files(file.path(root,'R'),full.names=TRUE))source(f)else library(bmeas)
source(file.path('helpers','residual-product-fixtures.R'))
source(file.path('helpers','product-archive-fixtures.R'))
local({
 private<-product_archive_private;passed<-0L
 ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat('PASS',label,'\n')}
 err<-function(expr,codes=c('E_PORTABLE_SCHEMA','E_PORTABLE_BINDING','E_PORTABLE_IDENTITY','E_PORTABLE_ACCOUNTING','E_PORTABLE_CAPABILITY')) {
  e<-tryCatch(force(expr),error=identity)
  if(!inherits(e,'measurement_error')||!e$code%in%codes){print(e);stop('unexpected refusal')}
  ok(TRUE,e$code);invisible(e)
 }
 work<-tempfile('product-guards-');dir.create(work);on.exit(unlink(work,recursive=TRUE),add=TRUE)
 spec<-product_archive_specs()$common_product
 fixture<-product_archive_fit(spec,file.path(work,'source'));fit<-fixture$fit
 context<-postprocess_budget('archive_fit')$context
 before<-serialize(fit[setdiff(names(fit),'backend')],NULL,version=3)
 backend<-new.env(parent=emptyenv());makeActiveBinding('forbidden',function(...)stop('BACKEND INSPECTION'),backend);fit$backend<-backend
 p<-portable_fit(fit,context)
 ok(identical(before,serialize(fit[setdiff(names(fit),'backend')],NULL,version=3)),'export does not mutate live values or original seals')
 ok(identical(p$immutable$ancestry$original_spec_fingerprint,spec$fingerprint),'original seal retained verbatim')
 ok(identical(p$immutable$ancestry$legacy_seals,private('.bh_pf_nested_seals')(spec)),'nested ancestry seals retained verbatim')
 original<-serialize(p,NULL,version=3)
 validated<-private('.bh_pf_active')(p,'component_draws')
 ok(identical(original,serialize(p,NULL,version=3))&&identical(validated$receipt$context_profile,p$immutable$context_profile),'validation preserves capsule and identifies new profile')
 posterior<-list(reader="streaming",max_source_bytes=2^21,max_read_bytes=7*2^21,
   max_materialized_values=10000000,max_record_bytes=2^18,max_columns=4000,
   max_source_rows=20,max_chains=2,max_temp_bytes=2*2^21,max_scan_values=100000)
 budget<-diagnostic_budget(4,100,1000,posterior=posterior)
 plan<-ppc_plan(p,4,budget,level="population")
 ok(identical(plan$source$reader_execution,private('.bh_pf_product_reader_capability')()),'separate current-reader capability admits complete product planning')
 assessed<-assess_measurement_fit(p,level="population",draws=4,seed=41,budget=budget)
 ok(!is.null(assessed$cache$ppc$table),'complete product saved Gaussian PPC runs')
 refused<-err(refresh_diagnostics(p,character(),list()),'E_PORTABLE_CAPABILITY')
 ok(identical(refused$context_validation$context_profile,p$immutable$context_profile),'refusal receipt identifies the new product profile')
 # Changing a profile cannot reinterpret a new scientific payload as a legacy capsule.
 changed<-p;changed$immutable$context_profile<-'bmeas_postprocess_context_v1';changed$immutable$capabilities<-private('.bh_pf_capabilities')()
 err(private('.bh_pf_active')(changed,'component_draws'))
 err(ppc_plan(changed,level='population'),'E_PORTABLE_CAPABILITY')
 changed<-p;changed$immutable$capabilities<-private('.bh_pf_capabilities')();err(private('.bh_pf_active')(changed,'component_draws'))
 receipt<-validated$receipt;receipt$context_profile<-'bmeas_postprocess_context_v1'
 err(private('.bh_pf_context_receipt_schema')(receipt,p),'E_PORTABLE_BINDING')
 # Scientific edits are never repaired, resealed or coerced. Test the static
 # record validator directly so digest rejection cannot conceal weak schemas.
 domain<-spec$tables$correlation_domains$component
 mutations<-list(
  extra=function(x){x$unknown<-TRUE;x},
  missing_label=function(x){x$product_domains<-x$product_domains[-1];x},
  duplicate_graph=function(x){x$graph_components[[2]]<-x$graph_components[[1]];x},
  wrong_indices=function(x){x$product_domains[[1]]$indices[1]<-100L;x},
  wrong_midpoint=function(x){x$product_domains[[1]]$midpoint<-x$product_domains[[1]]$midpoint+.01;x},
  wrong_interval=function(x){x$product_domains[[1]]$lower<-x$product_domains[[1]]$lower+.01;x},
  wrong_direction=function(x){x$product_domains[[1]]$direction[1,2]<-.5;x},
  wrong_fixed=function(x){x$constant[1,2]<-.7;x},
  wrong_factor=function(x){x$certificate$factor[1,1]<-.8;x},
  wrong_interior=function(x){x$interior[1]<-.7;x},
  wrong_status=function(x){x$status<-'unknown';x},
  wrong_scope=function(x){x$affine_chart_scope<-'multilabel_inspection_only';x})
 for(name in names(mutations))err(private('.bh_ps_product_domain')(mutations[[name]](domain),paste0('test.',name)))
 for(name in c('correlation_affine_constants','correlation_affine_cells','correlations')) {
  t<-spec$tables
  if(name=='correlation_affine_constants')t[[name]]$component[1,2]<-.4
  if(name=='correlation_affine_cells')t[[name]]$parameter[which(t[[name]]$state=='free')[1]]<-'fake_label'
  if(name=='correlations')t[[name]]$fixed_value[which(t[[name]]$state=='fixed')[1]]<-.4
  err(private('.bh_pf_product_tables')(t,'tables'))
 }
 changed<-fit;changed$spec$tables$correlation_domains$component$product_status<-'unavailable'
 err(portable_fit(changed,context),'E_PORTABLE_ORIGINAL_SEAL')
 changed<-fit;changed$cache$inference$status<-'qualified';err(portable_fit(changed,context))
 changed<-fit;changed$cache$inference<-NULL;err(portable_fit(changed,context))
 changed<-fit;changed$cache$correlation_constraints$domains$component$product_domains[[1]]$upper<-.5;err(portable_fit(changed,context))
 changed<-fit;changed$cache$correlation_constraints$primitive_summary_map$computational_mapping[1]<-'MGS_vector_map_no_scalar_association';err(portable_fit(changed,context))
 changed<-fit;changed$cache$correlation_constraints$primitive_summary_map$alias_cells[[1]]$row[1]<-99L;err(portable_fit(changed,context))
 # Hostile dispatch is rejected by exact classes before any custom method runs.
 hits<-0L;assign('$.hostile_product',function(...){hits<<-hits+1L;stop('HOSTILE DISPATCH')},.GlobalEnv)
 assign('length.hostile_product',function(...){hits<<-hits+1L;stop('HOSTILE DISPATCH')},.GlobalEnv)
 on.exit(rm(list=c('$.hostile_product','length.hostile_product'),envir=.GlobalEnv),add=TRUE)
 changed<-domain;class(changed)<-'hostile_product';err(private('.bh_ps_product_domain')(changed,'hostile'))
 changed<-p;class(changed$immutable$scientific_context$tables$correlation_domains$component)<-'hostile_product'
 err(private('.bh_pf_active')(changed,'component_draws'));ok(hits==0L,'hostile class callbacks never dispatched')
 # Even recognized scientific record classes do not authorize S3 callbacks.
 methods<-paste0('$.',c('bmeas_portable_context','observation_block','observation_model','data.frame'))
 existing<-lapply(methods,function(n)if(exists(n,.GlobalEnv,inherits=FALSE))get(n,.GlobalEnv)else NULL)
 for(n in methods)assign(n,function(...){hits<<-hits+1L;stop('RECOGNIZED CLASS DISPATCH')},.GlobalEnv)
 checked<-tryCatch(private('.bh_pf_product_profile')(p$immutable$scientific_context,p$cache,p$immutable$protected_evidence$original_provenance),error=identity)
 for(i in seq_along(methods))if(is.null(existing[[i]]))rm(list=methods[i],envir=.GlobalEnv)else assign(methods[i],existing[[i]],.GlobalEnv)
 if(inherits(checked,'error'))stop(checked)
 ok(hits==0L,'new profile validation never dispatches recognized scientific class methods')
 # Regression for the independently found pre-tree-walk subscript boundary.
 cases<-list()
 for(field in c('variable','computational_variable','computational_mapping'))for(kind in c('factor','hostile_map')) {
  changed<-fit;value<-changed$cache$correlation_constraints$primitive_summary_map[[field]]
  changed$cache$correlation_constraints$primitive_summary_map[[field]]<-if(kind=='factor')factor(value)else structure(value,class=kind)
  cases[[paste(field,kind)]]<-changed
 }
 subscript_hits<-0L
 with_subscript_guards<-function(action) {
  methods<-c('[.factor','[.hostile_map')
  old<-lapply(methods,function(n)if(exists(n,.GlobalEnv,inherits=FALSE))get(n,.GlobalEnv)else NULL)
  on.exit(for(i in seq_along(methods))if(is.null(old[[i]]))rm(list=methods[i],envir=.GlobalEnv)else assign(methods[i],old[[i]],.GlobalEnv))
  for(n in methods)assign(n,function(...){subscript_hits<<-subscript_hits+1L;stop('HOSTILE MAP SUBSCRIPT')},.GlobalEnv)
  action()
 }
 with_subscript_guards(function()for(changed in cases)err(portable_fit(changed,context)))
 ok(subscript_hits==0L,'factor and unknown-class map columns refuse before subscript dispatch')
 cap<-context;cap$max_context_bytes<-1;err(portable_fit(fit,cap),'E_PORTABLE_BUDGET')
 # Adverse unsealed history is explicitly synthetic; export protects it and
 # later mutable edits cannot make the saved consumer forget it.
 bad<-fit;bad$cache$posterior_propriety<-list(status='proven_improper',reason='synthetic adverse-history guard')
 adverse<-portable_fit(bad,context);adverse$cache$posterior_propriety<-list(status='not_assessed',reason='mutable edit')
 report<-summary(adverse);printed<-capture.output(print(report))
 ok(!is.null(private('.bh_pf_print_history_node')(unclass(report)))&&
    any(grepl('Protected original original_fit_history posterior_propriety : proven_improper',printed,fixed=TRUE))&&
    any(grepl('Protected original original_fit_history inference : not_assessed',printed,fixed=TRUE)),
    'product passive reports visibly preserve protected adverse and unknown inference evidence')
 err(component_draws(adverse),'E_COMPONENT_POSTERIOR')
 err(individual_parameter_summary(adverse,scales="predictor",budget=list()),'E_INDIVIDUAL_POSTERIOR')
 private('.bh_pf_active')(adverse,'component_draws',archive_inspection=TRUE)
 ok(identical(adverse$immutable$protected_evidence$original_fit_history$posterior_propriety$status,'proven_improper'),'adverse evidence stays protected for passive archive inspection')
 legacy_path<-Sys.getenv('BMEAS_OLD_CAPSULE')
 if(nzchar(legacy_path)) {
  legacy<-readRDS(legacy_path);bytes<-serialize(legacy,NULL,version=3)
  private('.bh_pf_active')(legacy,'component_draws',archive_inspection=TRUE)
  ok(identical(legacy$immutable$context_profile,'bmeas_postprocess_context_v1')&&identical(legacy$immutable$capabilities,private('.bh_pf_capabilities')()),'legacy capability record remains exactly unchanged')
  ok(identical(bytes,serialize(legacy,NULL,version=3)),'legacy validation never rewrites existing capsule bytes')
 } else cat('SKIP external legacy capsule: BMEAS_OLD_CAPSULE not supplied; packaged legacy archive tests remain separate\n')
 ok(identical(original,serialize(p,NULL,version=3)),'all refusal probes leave original new capsule bytes unchanged')
 cat('PASSED',passed,'product saved-value guard controls; no native execution\n')
})
