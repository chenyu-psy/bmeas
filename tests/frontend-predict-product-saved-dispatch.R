source(file.path("helpers","native-guard.R"))
root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root))for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
source(file.path("helpers","residual-product-fixtures.R"))
source(file.path("helpers","product-archive-fixtures.R"))
(function() {
 private<-product_archive_private;passed<-0L;hits<-character()
 ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
 work<-tempfile("product-ppc-dispatch-");dir.create(work);on.exit(unlink(work,recursive=TRUE),add=TRUE)
 context<-list(max_context_bytes=2^25,max_context_nodes=4000000,max_depth=64)
 posterior<-list(reader="streaming",max_source_bytes=2^21,max_read_bytes=7*2^21,
   max_materialized_values=10000000,max_record_bytes=2^18,max_columns=4000,
   max_source_rows=20,max_chains=2,max_temp_bytes=2*2^21,max_scan_values=100000)
 budget<-diagnostic_budget(4,100,1000,posterior=posterior)
 archive_budget<-list(max_members=16,max_member_bytes=2^25,max_total_bytes=2^27,
   max_context_bytes=2^25,max_read_bytes=2^30,max_temp_bytes=2^28,posterior=posterior,context=context)
 fixture<-product_archive_fit(product_archive_specs()$common_product,file.path(work,"csv"))
 p<-portable_fit(fixture$fit,context);a<-archive_fit(p,file.path(work,"archive"),archive_budget)
 # All newly reached saved classes have accessor/replacement/subscript traps.
 classes<-c("bmeas_portable_context","bmeas_ppc_plan","product_mutable_hostile")
 methods<-as.vector(outer(c("$.","$<-.","[.","[[."),classes,paste0))
 old<-mget(methods,.GlobalEnv,ifnotfound=rep(list(NULL),length(methods)),inherits=FALSE)
 for(n in methods)assign(n,local({label<-n;function(...){hits<<-c(hits,label);stop(paste("HOSTILE",label))}}),.GlobalEnv)
 on.exit(for(n in methods){if(is.null(old[[n]]))rm(list=n,envir=.GlobalEnv)else assign(n,old[[n]],.GlobalEnv)},add=TRUE)
 for(level in c("population","conditional")) {
   plan<-ppc_plan(p,4,budget,level)
   ok(identical(private(".bh_pf_get")(plan,"level"),level),paste(level,"planning avoids all saved accessors"))
   first<-assess_measurement_fit(p,level=level,draws=4,seed=41,budget=budget)
   c1<-private(".bh_pf_get")(first,"cache")
   ok(!is.null(c1$ppc$table),paste(level,"first assessment avoids all saved accessors"))
   second<-assess_measurement_fit(first,level=level,draws=4,seed=41,budget=budget)
   c2<-private(".bh_pf_get")(second,"cache")
   ok(identical(c1$ppc$replicate_statistics,c2$ppc$replicate_statistics),paste(level,"reassessment avoids all saved accessors"))
   saveRDS(second,file.path(work,"cold.rds"));cold<-readRDS(file.path(work,"cold.rds"))
   private(".bh_pf_ppc")(private(".bh_pf_get")(cold,"cache")$ppc,"cache.ppc",private(".bh_pf_get")(private(".bh_pf_get")(p,"immutable")$scientific_context,"tables"))
   ok(TRUE,paste(level,"cold current-reader schema avoids all saved accessors"))
   recorded<-archive_fit(cold,file.path(work,paste0("assessed-",level)),archive_budget)
   recorded<-read_fit_archive(recorded$root,archive_budget)
   replay<-assess_measurement_fit(recorded,level=level,draws=4,seed=41,budget=budget)
   ok(identical(replay$report_snapshot$portable$cache$ppc$replicate_statistics,c1$ppc$replicate_statistics),
     paste(level,"assessed portable current-reader cache survives archive roundtrip"))
   ap<-ppc_plan(a,4,budget,level)
   ar<-assess_measurement_fit(a,level=level,draws=4,seed=41,budget=budget)
   ok(!is.null(ar$report_snapshot$portable$cache$ppc$table),paste(level,"archive planning and assessment avoid all saved accessors"))
 }
 # Unconsumed mutable reports are never traversed by a predictive route.
 changed<-unclass(p);changed$cache$correlation_constraints<-structure(list(version="unused"),class="product_mutable_hostile")
 class(changed)<-"bmeas_portable_fit"
 out<-assess_measurement_fit(changed,level="conditional",draws=4,seed=41,budget=budget)
 ok(!is.null(private(".bh_pf_get")(out,"cache")$ppc$table),"unconsumed mutable scientific-report cache is not dispatched")
 ok(!length(hits),"zero saved accessor replacement or subscript invocations")
 cat("Saved product dispatch assertions:",passed,"\n")
})()
