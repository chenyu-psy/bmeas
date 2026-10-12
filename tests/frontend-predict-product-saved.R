source(file.path("helpers","native-guard.R"))
root<-Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root))for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
source(file.path("helpers","residual-product-fixtures.R"))
source(file.path("helpers","product-archive-fixtures.R"))
(function() {
 private<-product_archive_private;ns<-if(nzchar(root)).GlobalEnv else asNamespace("bmeas")
 passed<-0L;ok<-function(x,label){if(!isTRUE(x))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
 work<-tempfile("saved-product-ppc-");dir.create(work);on.exit(unlink(work,recursive=TRUE),add=TRUE)
 context<-list(max_context_bytes=2^25,max_context_nodes=4000000,max_depth=64)
 posterior<-list(reader="streaming",max_source_bytes=2^21,max_read_bytes=7*2^21,
   max_materialized_values=10000000,max_record_bytes=2^18,max_columns=4000,
   max_source_rows=20,max_chains=2,max_temp_bytes=2*2^21,max_scan_values=100000)
 budget<-diagnostic_budget(4,100,1000,posterior=posterior)
 archive_budget<-list(max_members=16,max_member_bytes=2^25,max_total_bytes=2^27,
   max_context_bytes=2^25,max_read_bytes=2^30,max_temp_bytes=2^28,posterior=posterior,context=context)
 specs<-product_archive_specs()
 fixtures<-lapply(names(specs),function(n)product_archive_fit(specs[[n]],file.path(work,n,"original"),deferred=n=="dual_product"))
 names(fixtures)<-names(specs)
 forbidden<-c(".bh_compile_scalar",".bh_fit_scalar",".bh_backend_ir",".bh_materialize_init",
   ".mp_toolchain_fingerprint",".bh_provider_runtime_info",".bh_provider_preflight",".bh_provider_sample",
   "make_stan_code","standata","stancode",".bh_checks",".bh_covariance_probe",".bh_gaussian_identification",
   ".bh_covariance_state",".bh_affine_layer_state",".bh_cor_product_chart",".bh_ppc_ingest_csv")
 forbidden<-forbidden[vapply(forbidden,exists,FALSE,envir=ns,inherits=FALSE)]
 old<-mget(forbidden,ns);locked<-vapply(forbidden,bindingIsLocked,FALSE,env=ns);hits<-character()
 put<-function(n,v){if(bindingIsLocked(n,ns))unlockBinding(n,ns);assign(n,v,ns);if(locked[[n]])lockBinding(n,ns)}
 for(n in forbidden)put(n,local({name<-n;function(...){hits<<-c(hits,name);stop(paste("FORBIDDEN",name))}}))
 on.exit(for(n in forbidden)put(n,old[[n]]),add=TRUE)
 # An independent Gaussian implementation reads named saved matrices directly.
 # It neither invokes the forward generator nor factors/repairs correlations.
 reference<-function(spec,draw,level) {
   N<-length(spec$data_map$subject_keys);J<-nrow(spec$tables$targets)
   K<-nrow(spec$tables$components);U<-nrow(spec$tables$parameter_residuals)
   vec<-function(n,k)unname(draw[paste0(n,"[",seq_len(k),"]")])
   mat<-function(n,r,c)matrix(unname(draw[paste0(n,"[",rep(seq_len(r),c),",",rep(seq_len(c),each=r),"]")]),r,c)
   eta<-if(level=="conditional")mat("eta",N,J)else matrix(rep(vec("alpha",J),each=N),N,J)
   if(level=="population") {
     if(K)eta<-eta+t(mat("L_component",K,K)%*%matrix(rnorm(K*N),K,N))%*%t(mat("effective_loadings",J,K))
     if(U)eta<-eta+t(mat("L_parameter",U,U)%*%matrix(rnorm(U*N),U,N))%*%t(mat("D_parameter",J,U))
   }
   out<-list();ops<-spec$tables$observation_operands
   for(b in names(spec$models)) {
     d<-spec$data_map$blocks[[b]];meanop<-ops[ops$block==b&ops$argument=="mu",,drop=FALSE]
     scaleop<-ops[ops$block==b&ops$argument=="sigma",,drop=FALSE]
     meanop<-meanop[match(d$.observation_id,meanop$observation_id),,drop=FALSE]
     scaleop<-scaleop[match(d$.observation_id,scaleop$observation_id),,drop=FALSE]
     mu<-meanop$received_constant;sd<-scaleop$received_constant
     at<-which(meanop$operand!="fixed_received");mu[at]<-eta[cbind(d$.person_index[at],meanop$eta_index[at])]
     at<-which(scaleop$operand!="fixed_received");sd[at]<-draw[paste0("obs_sd[",scaleop$observation_scale_index[at],"]")]
     out[[b]]<-setNames(rnorm(nrow(d),mu,sd),d$.observation_id)
   }
   out
 }
 cache<-function(x)if(inherits(x,"bmeas_fit_archive"))x$report_snapshot$portable$cache else x$cache
 set.seed(96721);rng<-.Random.seed;kind<-RNGkind()
 for(name in names(fixtures)) {
   f<-fixtures[[name]];fit<-f$fit;s<-specs[[name]]
   hostile<-new.env(parent=emptyenv());makeActiveBinding("forbidden",function(...)stop("BACKEND TOUCHED"),hostile);fit$backend<-hostile
   live_before<-serialize(fit[setdiff(names(fit),"backend")],NULL,version=3);calls_before<-as.list(f$calls)
   portable<-portable_fit(fit,context);saveRDS(portable,file.path(work,name,"cold.rds"));cold<-readRDS(file.path(work,name,"cold.rds"))
   original<-serialize(cold,NULL,version=3);immutable<-serialize(cold$immutable,NULL,version=3)
   arc<-archive_fit(cold,file.path(work,name,"archive"),archive_budget)
   results<-list()
   for(level in c("population","conditional")) {
     plan<-ppc_plan(cold,4,budget,level)
     ok(identical(plan$source$reader_execution,private(".bh_pf_product_reader_capability")())&&
       identical(plan$source$portable_context$status,"not_verified_by_this_metadata_call"),paste(name,level,"bounded metadata has current reader but no digest claim"))
     live<-assess_measurement_fit(fit,level=level,draws=4,seed=571,budget=budget)
     saved<-assess_measurement_fit(cold,level=level,draws=4,seed=571,budget=budget)
     lr<-cache(live)$ppc;pr<-cache(saved)$ppc
     if(is.null(pr$table)){print(pr$metadata$failure);stop("saved prediction failed")}
     ok(identical(lr$table,pr$table)&&identical(lr$replicate_statistics,pr$replicate_statistics)&&
       identical(lr$metadata$draw_keys,pr$metadata$draw_keys),paste(name,level,"live/cold portable exact table aggregates and joint keys"))
     keys<-pr$metadata$draw_keys
     ok(identical(keys,data.frame(chain_id=rep(1:2,2),saved_row=rep(c(1L,4L),each=2))),paste(name,level,"balanced joint row selection is explicit"))
     sp<-private(".bh_ppc_statistics_plan")(s,4,unclass(plan)$budget,level)
     if(name=="common_product")ok(anyDuplicated(paste(s$data_map$blocks[[1]]$.person_key,s$data_map$blocks[[1]]$.task))>0L,
       paste(name,level,"repeated observations share retained person/task predictors"))
     if(name=="dual_product")ok(nrow(s$tables$components)>0L&&nrow(s$tables$parameter_residuals)>0L,
       paste(name,level,"both saved correlated dependence layers are present"))
     expected<-private(".bh_ppc_with_rng")(571,function() {
       do.call(rbind,lapply(seq_len(nrow(keys)),function(i) {
         responses<-reference(s,f$chain_rows[[keys$chain_id[i]]][keys$saved_row[i],],level)
         # Each mean is also checked against the independent Gaussian response
         # rows before the package aggregate/statistic comparison below.
         statistics<-private(".bh_ppc_statistics")(s,sp,responses)
         person_means<-list()
         for(b in names(responses))for(task in unique(s$data_map$blocks[[b]]$.task)) {
           d<-s$data_map$blocks[[b]];at<-which(d$.task==task)
           row<-which(statistics$model==b&statistics$metric=="mean"&statistics$target==paste(b,"mu",task,sep="::"))
           if(length(row))stopifnot(identical(statistics$estimate[row],mean(responses[[b]][at])))
           person_means[[paste(b,"mu",task,sep="::")]]<-tapply(responses[[b]][at],d$.person_key[at],mean)
         }
         for(j in which(statistics$metric=="paired_person_covariance")) {
           left<-person_means[[statistics$target1[j]]];right<-person_means[[statistics$target2[j]]]
           overlap<-intersect(names(left),names(right))
           if(length(overlap)>1L)stopifnot(isTRUE(all.equal(statistics$estimate[j],unname(cov(left[overlap],right[overlap])),tolerance=1e-14)))
         }
         statistics$estimate
       }))
     })
     ok(isTRUE(all.equal(unname(pr$replicate_statistics),unname(expected),tolerance=1e-14)),paste(name,level,"independent saved-matrix Gaussian arithmetic agrees"))
     ok(identical(pr$metadata$reader_execution,pr$metadata$plan$source$reader_execution)&&
       identical(pr$metadata$source_receipt$context_validation$status,"verified")&&
       identical(pr$metadata$upstream$inference$status,"not_assessed"),paste(name,level,"result binds reader completed receipt and unqualified inference"))
     private(".bh_pf_ppc")(pr,"cache.ppc",s$tables)
     repeated<-assess_measurement_fit(saved,level=level,draws=4,seed=571,budget=budget)
     ok(identical(repeated$cache$ppc$replicate_statistics,pr$replicate_statistics),paste(name,level,"validated cached result can be reassessed"))
     ok(identical(serialize(saved$immutable,NULL,version=3),immutable)&&identical(serialize(cold,NULL,version=3),original),paste(name,level,"immutable and input capsule bytes unchanged"))
     results[[level]]<-pr
   }
   relocated<-file.path(work,name,"relocated");stopifnot(file.rename(arc$root,relocated));unlink(f$paths)
   reopened<-read_fit_archive(relocated,archive_budget);disk<-list.files(relocated,recursive=TRUE,full.names=TRUE);hashes<-tools::md5sum(disk)
   archive_before<-serialize(reopened,NULL,version=3)
   for(level in c("population","conditional")) {
     ap<-ppc_plan(reopened,4,budget,level)
     ok(identical(ap$source$archive_context$status,"reopened_and_verified"),paste(name,level,"archive planning reopens authoritative bytes"))
     a<-assess_measurement_fit(reopened,level=level,draws=4,seed=571,budget=budget);ar<-cache(a)$ppc
     ok(identical(ar$table,results[[level]]$table)&&identical(ar$replicate_statistics,results[[level]]$replicate_statistics)&&
       identical(ar$metadata$draw_keys,results[[level]]$metadata$draw_keys),paste(name,level,"relocated archive exact parity with deleted original CSVs"))
   }
   ok(identical(hashes,tools::md5sum(disk))&&identical(archive_before,serialize(reopened,NULL,version=3))&&
     identical(live_before,serialize(fit[setdiff(names(fit),"backend")],NULL,version=3))&&identical(calls_before,as.list(f$calls)),paste(name,"all disk inputs and producer evidence preserved without backend calls"))
 }
 ok(identical(rng,.Random.seed)&&identical(kind,RNGkind()),"all successful operations restore caller RNG")
 ok(!length(hits),"no native source chart callback backend or scientific reevaluation")
 cat("Saved product PPC assertions:",passed,"\n")
})()
