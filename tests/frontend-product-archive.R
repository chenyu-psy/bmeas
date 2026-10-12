source(file.path("helpers","native-guard.R"))
# Focused R-only archive evidence. Public declarations and public experimental
# fit lifecycles use terminal compiler/sampler doubles and synthetic CSV rows.
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root))for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE))source(f)else library(bmeas)
source(file.path("helpers","residual-product-fixtures.R"))
source(file.path("helpers","product-archive-fixtures.R"))
(function() {
  ns <- if(nzchar(root)).GlobalEnv else asNamespace("bmeas")
  private <- product_archive_private
  passed <- 0L
  ok <- function(value,label) {if(!isTRUE(value))stop(label);passed<<-passed+1L;cat("PASS",label,"\n")}
  err <- function(action,code) {
    value<-tryCatch(action(),error=identity)
    if(!inherits(value,"measurement_error")||!identical(value$code,code)) {print(value);stop(paste("Expected",code))}
    ok(TRUE,code)
  }
  owned <- tempfile("product-archive-r-only-");dir.create(owned)
  on.exit(unlink(owned,recursive=TRUE),add=TRUE)
  context <- list(max_context_bytes=2^25,max_context_nodes=4000000,max_depth=64)
  posterior <- list(reader="streaming",max_source_bytes=2^21,max_read_bytes=7*2^21,
    max_materialized_values=10000000,max_record_bytes=2^18,max_columns=4000,
    max_source_rows=20,max_chains=2,max_temp_bytes=2*2^21,max_scan_values=100000)
  archive_budget <- list(max_members=16,max_member_bytes=2^25,max_total_bytes=2^27,
    max_context_bytes=2^25,max_read_bytes=2^30,max_temp_bytes=2^28,
    posterior=posterior,context=context)
  component_budget <- list(posterior=posterior,max_components=8,max_output_values=100000,
    max_matrix_work=100000,context=context)
  individual_budget <- list(posterior=posterior,max_parameter_cells=2000,max_summary_rows=6000,
    max_transformation_terms=1000000,max_declared_observation_rows=2000,
    max_consumed_observation_rows=2000,max_operand_context_rows=30000,max_context_terms=100000000)
  predictive_budget <- diagnostic_budget(4,100,1000,posterior=posterior)
  legacy_capabilities <- list(schema_version="bmeas_portable_capabilities_v1",
    profile="retained_declarative_postprocessing_only",
    metadata_interfaces=c("diagnostic_parameters","ppc_plan"),
    passive_interfaces=c("summary","fit_checks","parameter_summary","posterior_summary","print"),
    active_interfaces=c("refresh_diagnostics","individual_parameter_summary","assess_measurement_fit"),
    interface_qualification="existing_consumer_scientific_domains_and_operation_specific_budgets_remain_required",
    population_diagnostics="existing_primitive_derived_monitor_domains_and_exact_alias_interpretation",
    individual_summaries="existing_predictor_roles_scales_fixed_shared_person_and_state_selection_rules",
    basic_ppc="exact_canonical_gaussian_binomial_logit_lognormal_descriptors_and_package_owned_kernels_only",
    unavailable=c("fitting","source_generation","data_regeneration","model_rebuilding","saved_model_methods",
      "callback_simulation","generic_check_model_reevaluation","source_path_rebinding"),
    dispatch_status="stage1_export_only_consumers_closed")
  ok(identical(private(".bh_pf_capabilities")(),legacy_capabilities),"legacy capability record remains byte-for-byte unchanged")
  specs <- product_archive_specs()
  ok(identical(names(specs),c("common_product","residual_only","common_fixed","common_mgs","common_one","dual_product")),
    "six genuine public common/residual/dual product declarations")
  fixtures <- lapply(names(specs),function(name) {
    fixture<-product_archive_fit(specs[[name]],file.path(owned,name,"original"),deferred=name=="dual_product")
    ok(identical(fixture$fit$provenance$execution_profile$requested,"experimental")&&
      identical(fixture$fit$cache$inference$status,"not_assessed"),paste(name,"public experimental lifecycle retains unqualified inference"))
    fixture
  });names(fixtures)<-names(specs)
  # Backend-free reads must not consult any runtime, model generator, scientific
  # reevaluator, compiler, live chart or retained model callback.
  forbidden <- c(".bh_compile_scalar",".bh_fit_scalar",".bh_backend_ir",".bh_materialize_init",
    ".mp_toolchain_fingerprint",".bh_provider_runtime_info",".bh_provider_preflight",".bh_provider_sample",
    "make_stan_code","standata","stancode",".bh_checks",".bh_covariance_probe",".bh_gaussian_identification",
    ".bh_covariance_state",".bh_affine_layer_state",".bh_cor_product_chart",
    ".bh_diagnostic_parameter_metrics",".bh_diagnostic_hmc_metrics",".bh_ppc_ingest_csv",".bh_individual_ingest_csv")
  forbidden<-forbidden[vapply(forbidden,exists,logical(1),envir=ns,inherits=FALSE)]
  originals<-mget(forbidden,ns);locked<-vapply(forbidden,bindingIsLocked,logical(1),env=ns)
  hits<-character()
  put<-function(name,value){if(bindingIsLocked(name,ns))unlockBinding(name,ns);assign(name,value,ns);if(locked[[name]])lockBinding(name,ns)}
  for(name in forbidden)put(name,local({label<-name;function(...) {hits<<-c(hits,label);stop(paste("FORBIDDEN",label))}}))
  on.exit(for(name in forbidden)put(name,originals[[name]]),add=TRUE)
  set.seed(9761);rng <- .Random.seed
  for(name in names(fixtures)) {
    fixture<-fixtures[[name]];fit<-fixture$fit;s<-specs[[name]];K<-nrow(s$tables$components)
    calls_before<-as.list(fixture$calls)
    hostile<-new.env(parent=emptyenv())
    makeActiveBinding("must_not_inspect",function(...) {hits<<-c(hits,"backend");stop("FORBIDDEN backend inspection")},hostile)
    fit$backend<-hostile
    before<-serialize(fit[setdiff(names(fit),"backend")],NULL,version=2)
    source_hashes<-tools::md5sum(fixture$paths)
    live_c<-component_draws(fit,component_budget)
    live_i<-individual_parameter_summary(fit,c("predictor","natural","received"),individual_budget)
    portable<-portable_fit(fit,context)
    ok(identical(portable$immutable$context_profile,"bmeas_iid_gaussian_product_context_v1")&&
      identical(class(portable$immutable$scientific_context),"bmeas_portable_context")&&
      !"backend"%in%names(portable),paste(name,"distinct detached product profile"))
    ok(identical(portable$immutable$capabilities$active_interfaces,c("individual_parameter_summary","component_draws")),
      paste(name,"historical producer active-consumer record remains unchanged"))
    cold<-file.path(owned,name,"portable.rds");saveRDS(portable,cold);restored<-readRDS(cold)
    pbytes<-serialize(restored,NULL,version=2)
    report<-summary(restored);checks<-fit_checks(restored)
    capture.output(print(restored));capture.output(print(report));capture.output(print(checks))
    ok(identical(posterior_summary(restored),posterior_summary(fit))&&
      identical(parameter_summary(restored),parameter_summary(fit)),paste(name,"cold passive cached summaries agree"))
    pc<-component_draws(restored,component_budget)
    pi<-individual_parameter_summary(restored,c("predictor","natural","received"),individual_budget)
    cfields<-c("status","domain","correlation","sd","covariance","original_available","derived_available","available",
      "log_abs_covariance","log_sd","draw_keys","coordinates","maps","prior","matrix_status","precision")
    ok(all(vapply(cfields,function(field)identical(live_c[[field]],pc[[field]]),logical(1))),
      paste(name,"live and portable component values and scientific maps agree"))
    ok(all(vapply(names(pi),function(field)identical(live_i[[field]],pi[[field]]),logical(1)))&&
      identical(attr(live_i,"draw_keys"),attr(pi,"draw_keys")),paste(name,"live and portable individual rows agree"))
    # Independently read the decimal states actually saved by the terminal
    # fixture. Pre-write binary64 states need not survive a 17-digit decimal
    # conversion bit-for-bit on every R platform. Do not use a package reader
    # or consumer to construct this exact saved-matrix oracle.
    saved_rows<-lapply(seq_along(fixture$paths),function(chain) {
      declared<-fixture$chain_rows[[chain]]
      csv<-utils::read.table(fixture$paths[chain],header=TRUE,sep=",",
        comment.char="#",check.names=FALSE,colClasses="character")
      expected_names<-gsub("\\]","",gsub("[\\[,]",".",colnames(declared)))
      ok(identical(names(csv),expected_names)&&nrow(csv)==nrow(declared),
        paste(name,chain,"independent saved decimal columns and rows match declarations"))
      matrix(as.double(as.matrix(csv)),nrow=nrow(csv),
        dimnames=list(NULL,colnames(declared)))
    })
    ordered<-do.call(rbind,lapply(seq_len(4),function(i)do.call(rbind,lapply(saved_rows,function(x)x[i,,drop=FALSE]))))
    ok(identical(attr(pi,"draw_keys"),data.frame(chain_id=rep(1:2,4),saved_row=rep(1:4,each=2))),
      paste(name,"aligned chain and saved-row keys are explicit"))
    predictor<-pi[pi$scale=="predictor",,drop=FALSE]
    expected<-vapply(predictor$source_variable,function(variable)mean(ordered[,variable]),numeric(1))
    ok(isTRUE(all.equal(unname(predictor$mean),unname(expected),tolerance=1e-14)),paste(name,"individual means match saved eta arithmetic"))
    if(K) {
      expected_R<-array(NA_real_,dim=c(8L,K,K))
      for(j in seq_len(K))for(i in seq_len(K))expected_R[,i,j]<-ordered[,paste0("R_component[",i,",",j,"]")]
      ok(isTRUE(all.equal(unname(pc$correlation),expected_R,tolerance=0)),paste(name,"component R is the exact aligned saved matrix"))
    }else ok(identical(pc$status,"not_applicable_no_components"),"residual-only product returns explicit empty component result")
    archive<-archive_fit(restored,file.path(owned,name,"archive"),archive_budget)
    ok(identical(source_hashes,tools::md5sum(fixture$paths)),paste(name,"export and archiving preserve original CSV bytes"))
    relocated<-file.path(owned,name,"relocated")
    ok(file.rename(archive$root,relocated),paste(name,"archive relocates"))
    unlink(fixture$paths)
    reopened<-read_fit_archive(relocated,archive_budget)
    disk<-list.files(relocated,recursive=TRUE,full.names=TRUE);disk_hashes<-tools::md5sum(disk)
    abytes<-serialize(reopened,NULL,version=2)
    ac<-component_draws(reopened,component_budget)
    ai<-individual_parameter_summary(reopened,c("predictor","natural","received"),individual_budget)
    selected<-individual_parameter_summary(reopened,"predictor",individual_budget,states=list(method="balanced",draws=4))
    ok(all(vapply(cfields,function(field)identical(pc[[field]],ac[[field]]),logical(1))),
      paste(name,"relocated archive preserves exact component values and maps"))
    ok(all(vapply(names(ai),function(field)identical(pi[[field]],ai[[field]]),logical(1)))&&
      identical(attr(ai,"draw_keys"),attr(pi,"draw_keys")),paste(name,"relocated archive preserves individual summaries"))
    ok(nrow(attr(selected,"draw_keys"))==4L&&identical(attr(selected,"metadata")$selection$method,"balanced"),
      paste(name,"balanced individual selection retains its stated domain"))
    archived_parameters<-posterior_summary(reopened)
    if(!is.null(archived_parameters)) {
      ok(identical(attr(archived_parameters,"archive_context")$status,"retained_snapshot_not_reverified"),
        paste(name,"passive archive parameter summary labels its snapshot scope"))
      attr(archived_parameters,"archive_context")<-NULL
    }
    ok(identical(archived_parameters,posterior_summary(restored))&&is.list(summary(reopened))&&is.list(fit_checks(reopened)),
      paste(name,"relocated passive summaries preserve retained cache"))
    # The profile gate is common to all six scientific modes. Exercise every
    # portable mode and the archive wrapper once, without redundant disk scans.
    ppc_objects<-if(name=="common_product")list(restored,reopened)else list(restored)
    for(object in ppc_objects)for(level in c("population","conditional")) {
      plan<-ppc_plan(object,4,predictive_budget,level)
      ok(identical(plan$source$reader_execution,private(".bh_pf_product_reader_capability")()),
        paste(name,level,"current reader plan is separate from unchanged producer record"))
      result<-assess_measurement_fit(object,level=level,draws=4,seed=42,budget=predictive_budget)
      cache<-if(inherits(result,"bmeas_fit_archive"))result$report_snapshot$portable$cache else result$cache
      ok(if(inherits(object,"bmeas_fit_archive"))!is.null(cache$ppc$table)else is.null(cache$ppc$table),
        paste(name,level,"archive reads relocated bytes; detached missing original paths cannot read"))
    }
    ok(identical(before,serialize(fit[setdiff(names(fit),"backend")],NULL,version=2))&&
      identical(pbytes,serialize(restored,NULL,version=2))&&identical(abytes,serialize(reopened,NULL,version=2))&&
      identical(disk_hashes,tools::md5sum(disk))&&identical(calls_before,as.list(fixture$calls)),
      paste(name,"all reads preserve fit capsule handle and archive bytes without backend calls"))
  }
  ok(!length(hits),"no compiler backend callback model generation or scientific reevaluation was reached")
  ok(identical(rng,.Random.seed),"saved reads preserve RNG state")
  ok(identical(private(".bh_pf_capabilities")(),legacy_capabilities),"legacy capabilities remain unchanged after all product routes")
  cat("PASSED",passed,"focused product archive assertions; six public fit lifecycles, terminal doubles only, no native/HMC evidence\n")
})()
