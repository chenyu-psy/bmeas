# Private portable post-processing routes. The qualified stage1 exporter and
# codec remain unchanged. A consumer view is never a prepared/live-fit class.
.bh_pf_is_portable <- function(x) inherits(x,"bmeas_portable_fit")
.bh_pf_is_view <- function(x,operation=NULL) {
  identical(.bh_pf_class(x),"bmeas_portable_consumer_view") &&
    (is.null(operation)||identical(.bh_pf_get(x,"portable_operation"),operation))
}
.bh_pf_refuse_execution <- function(x,operation) {
  if(.bh_pf_is_portable(x)||inherits(x,"bmeas_portable_consumer_view")||inherits(x,"bmeas_portable_context"))
    .bh_pf_fail(operation,"portable post-processing objects cannot fit, rebuild, generate source/data, invoke callbacks or use a runtime/backend","E_PORTABLE_UNSUPPORTED")
  invisible(NULL)
}
.bh_pf_ordered <- function(x,path,fields,classes=NULL) {
  value<-.bh_pf_record(x,path,fields,classes=classes)
  if(!identical(names(value),fields)) .bh_pf_fail(path,"record field order differs from its declared schema")
  value
}
.bh_pf_identity <- function(x) {
  v<-.bh_pf_ordered(x,"identity",c("format","algorithm","provider","digest"))
  if(!identical(v$format,"bmeas_portable_context_bytes_v1")||
     !identical(v$algorithm,"md5")||!identical(v$provider,"tools::md5sum"))
    .bh_pf_fail("identity","unsupported canonical identity format/provider")
  .bh_pf_md5(v$digest,"identity.digest")
  v
}
.bh_pf_stored_policy <- function(x) {
  v<-.bh_pf_ordered(x,"validation_policy",c("max_context_bytes","max_context_nodes","max_depth"))
  if(!identical(v,.bh_pf_policy(v)))
    .bh_pf_fail("validation_policy","the protected policy must contain the three original finite plain double limits","E_PORTABLE_BUDGET")
  v
}
.bh_pf_export_receipt_schema <- function(x,policy=NULL) {
  template<-.bh_pf_receipt("")
  v<-.bh_pf_ordered(x,"export_receipt",names(template))
  for(n in names(template)) {
    value<-v[[n]]
    if(n=="native_locale_at_export") .bh_pf_string(value,paste0("export_receipt.",n),empty=TRUE)
    else if(typeof(template[[n]])=="double") {
      if(!.bh_pf_plain(value,"double")) .bh_pf_fail("export_receipt","accounting counters must be plain doubles")
      .bh_pf_number(value,paste0("export_receipt.",n),1,(2^53 - 1),TRUE)
      if(grepl("passes$",n)&&!identical(value,1.0)) .bh_pf_fail("export_receipt","unsupported export traversal count")
    } else if(!identical(value,template[[n]])) .bh_pf_fail("export_receipt",paste("unsupported",n))
  }
  if(v$hash_bytes_written!=v$immutable_bytes||v$hash_read_bytes_reserved!=v$immutable_bytes)
    .bh_pf_fail("export_receipt","immutable hash accounting disagrees")
  if(v$canonical_payload_bytes<=v$immutable_bytes||v$canonical_payload_nodes<=v$immutable_nodes||
     v$canonical_payload_depth<v$immutable_depth+1.0)
    .bh_pf_fail("export_receipt","whole export accounting cannot contain its immutable child","E_PORTABLE_ACCOUNTING")
  if(!is.null(policy)&&(v$canonical_payload_bytes>policy$max_context_bytes||
     v$canonical_payload_nodes>policy$max_context_nodes||v$canonical_payload_depth>policy$max_depth))
    .bh_pf_fail("export_receipt","historical export accounting exceeds its protected admission policy","E_PORTABLE_ACCOUNTING")
  v
}
.bh_pf_envelope <- function(fit) {
  root<-.bh_pf_ordered(fit,"portable_fit",c("schema_version","immutable","cache","identity","export_receipt"),"bmeas_portable_fit")
  if(!identical(root$schema_version,"bmeas_portable_fit_v1")) .bh_pf_fail("portable_fit","unsupported schema version")
  core<-.bh_pf_ordered(root$immutable,"immutable",c("context_profile","scientific_context","retained_artifacts","source_binding","ancestry","protected_evidence","validation_policy","capabilities"))
  .bh_pf_string(core$context_profile,"immutable.context_profile",c("bmeas_postprocess_context_v1","bmeas_iid_gaussian_product_context_v1","bmeas_joint_m3cs_context_v1"))
  policy<-.bh_pf_stored_policy(core$validation_policy)
  if(!identical(core$capabilities,.bh_pf_profile_capabilities(core$context_profile))) .bh_pf_fail("capabilities","unsupported or disabled immutable interface/exclusion profile")
  .bh_pf_identity(root$identity)
  .bh_pf_export_receipt_schema(root$export_receipt,policy)
  root
}

# Export normalized all text into ASCII or marked UTF-8. A canonical digest
# alone intentionally does not distinguish an equivalent Latin-1 encoding;
# the detached storage requirement therefore also has this explicit check.
.bh_pf_stored_text <- function(x) {
  text<-function(v) for(s in unclass(v)) if(!is.na(s)) {
    encoding<-Encoding(s)
    if(encoding=="bytes"||(!identical(encoding,"UTF-8")&&
       !isTRUE(.bh_portable_codec_text_scan(s,FALSE)$ascii)))
      .bh_pf_fail("immutable text","stored non-ASCII text must be explicitly UTF-8","E_PORTABLE_ENCODING")
  }
  if(typeof(x)=="character") text(x)
  attrs<-attributes(x)
  if(length(attrs)) {
    text(names(attrs))
    for(n in names(attrs)) .bh_pf_stored_text(if(n=="row.names"&&identical(.bh_pf_class(x),"data.frame")) .row_names_info(x,0L) else attrs[[n]])
  }
  if(typeof(x)=="list")for(i in seq_along(unclass(x))) .bh_pf_stored_text(.subset2(unclass(x),i))
  invisible(TRUE)
}
.bh_pf_inert_timestamp <- function(x,path) {
  if(is.null(x))return(invisible(TRUE))
  if(typeof(x)!="list") {
    if(!.bh_pf_plain(x,c("integer","double"))) .bh_pf_fail(path,"unsupported detached timestamp")
    return(invisible(TRUE))
  }
  v<-.bh_pf_ordered(x,path,c("representation","epoch","original_class","timezone_present","timezone"))
  if(!identical(v$representation,"bmeas_inert_timestamp_v1")||!identical(v$original_class,c("POSIXct","POSIXt"))||
     !.bh_pf_plain(v$epoch,c("integer","double"))) .bh_pf_fail(path,"malformed inert timestamp")
  .bh_pf_logical(v$timezone_present,paste0(path,".timezone_present"))
  if(!v$timezone_present&&!is.null(v$timezone)) .bh_pf_fail(path,"absent timezone carries a value")
  if(!is.null(v$timezone)) .bh_pf_character(v$timezone,paste0(path,".timezone"))
  invisible(TRUE)
}
.bh_pf_protected_schema <- function(history,policy) {
  fields<-c("original_scientific_checks","original_provenance","original_fit_history","retained_cache_scope")
  h<-.bh_pf_record(history,"protected_evidence",c(fields,"original_execution_receipt","original_resource_outcome"),fields)
  if(!identical(h$retained_cache_scope,"copied_existing_evidence_may_include_posterior_valued_arrays_no_new_acquisition"))
    .bh_pf_fail("protected_evidence","unsupported retained-cache scope")
  .bh_pf_plain_tree(h$original_scientific_checks,c("spec","checks"),limit=policy$max_depth)
  .bh_pf_history_schema(h$original_scientific_checks,"spec.checks")
  for(n in c("provenance","execution_receipt","resource_outcome")) {
    key<-paste0("original_",n)
    if(key%in%names(h)) {
      value<-h[[key]]
      .bh_pf_plain_tree(value,n,limit=policy$max_depth)
      .bh_pf_history_schema(value,n)
      times<-if(n=="provenance")value else if(n=="execution_receipt") .bh_pf_get(value,"sampling") else NULL
      for(t in .bh_pf_intersect(c("started","finished"),names(times))) .bh_pf_inert_timestamp(.bh_pf_get(times,t),paste0(key,".",t))
    }
  }
  ordinary<-c("hmc","computation","numerical_evaluations","admissibility","posterior_propriety","posterior_normalizability",
    "gaussian_diagnostics","execution_profile","backend_qualification","identification","inference","predictive_fit",
    "measurement_structure_fit","initialization","posterior_reporting")
  special<-c("optional_report_history","optional_log_history","expanded_report_history","predictive_attempt_history")
  saved<-.bh_pf_record(h$original_fit_history,"original_fit_history",c(ordinary,special),character())
  for(n in names(saved)) {
    value<-saved[[n]]
    if(n%in%ordinary) {
      .bh_pf_plain_tree(value,c("cache",n),limit=policy$max_depth)
      .bh_pf_history_schema(value,paste0("cache.",n))
    } else {
      allowed<-switch(n,optional_report_history=c("status","reason","unavailable_values","entries","log_metadata_semantics","scope","warning","warnings"),
        optional_log_history=c("status","reason"),expanded_report_history=c("status","reason","scope"),
        predictive_attempt_history=c("version","level","law","requested_replications","completed_replications","failure","replicate_statistics_status","screening_only","source_origin_scope","interpretation"))
      .bh_pf_record(value,paste0("original_fit_history.",n),allowed,character())
      # The retained projections can contain ordinary entry tables, never
      # posterior arrays/classes beyond their original positional cache schema.
      original<-switch(n,optional_report_history="optional_reports",optional_log_history="optional_reports",
        expanded_report_history="expanded_correlation_matrices",predictive_attempt_history="ppc")
      .bh_pf_plain_tree(value,c("cache",original),limit=policy$max_depth)
      .bh_pf_open_metadata(value,paste0("original_fit_history.",n))
    }
  }
  invisible(TRUE)
}
.bh_pf_callback_markers <- function(spec,markers) {
  if(!.bh_pf_plain(markers,"list")) .bh_pf_fail("callback_disposition","expected an unclassed unnamed marker list")
  models<-.bh_pf_get(spec,"models");slots<-c("validate_data","prepare_data","validate_bindings","informative_rows",
    "methods.simulate","methods.posterior_predict","methods.posterior_epred")
  if(length(markers)!=7*length(models)) .bh_pf_fail("callback_disposition","markers do not cover all seven exact slots per block")
  at<-0L
  for(b in names(models))for(i in seq_along(slots)) {
    at<-at+1L;model<-.bh_pf_get(.bh_pf_get(models,b),"model")
    owner<-if(i>4L).bh_pf_get(model,"methods")else model
    key<-if(i>4L)substring(slots[[i]],9L)else slots[[i]]
    present<-key%in%names(owner)
    marker<-.bh_pf_ordered(markers[[at]],"callback_disposition",c("slot","presence","original_type","disposition"))
    if(!identical(marker$slot,paste0("spec.models.",b,".model.",slots[[i]]))||
       !identical(marker$disposition,"execution_only_omitted_not_semantically_replayed_or_equated"))
      .bh_pf_fail("callback_disposition","slot or omission semantics changed")
    .bh_pf_string(marker$presence,"callback_disposition.presence",c("absent","NULL","function"))
    expected<-switch(marker$presence,absent="absent","NULL"="NULL","function"=c("closure","builtin","special"))
    .bh_pf_string(marker$original_type,"callback_disposition.original_type",expected)
    if(identical(marker$presence,"absent")!=!present||(present&&!is.null(.bh_pf_get(owner,key))))
      .bh_pf_fail("callback_disposition","omitted callback slot disagrees with its marker")
  }
  invisible(TRUE)
}
.bh_pf_immutable_schema <- function(root,policy) {
  core<-root$immutable;s<-core$scientific_context
  fields<-c("formula","models","subjects","missing","data_map","parameter_keys","tables","prior","status","backend",
    "response_missing_semantics","observation_key_origin","schema_version","checks","fingerprint")
  s<-.bh_pf_record(s,"scientific_context",c(fields,"schema_migration"),fields,"bmeas_portable_context")
  # Validate genuine detached storage. No live class, seal or producer profile
  # is manufactured, even temporarily, to authorize reader execution.
  .bh_pf_plain_tree(unclass(s),"spec",limit=policy$max_depth)
  .bh_pf_spec(core$scientific_context,policy,detached=TRUE)
  if(!.bh_pf_joint_profile_matches(core$context_profile,s)) .bh_pf_fail("immutable","scientific product domain and context profile disagree")
  a<-.bh_pf_ordered(core$ancestry,"ancestry",c("original_fit_class","original_spec_class","original_spec_fingerprint",
    "original_fit_model_fingerprint","legacy_seals","conversion_profile","callback_disposition","backend_disposition","evidence_scope"))
  if(!identical(a$original_fit_class,c("bmeas_fit","bhmm_fit"))||!identical(a$original_spec_class,c("bmeas_spec","bhmm_spec"))||
     !identical(a$conversion_profile,.bh_pf_conversion_profile(core$context_profile))||
     !identical(a$evidence_scope,"original_spec_seal_and_fit_fingerprint_association_do_not_authenticate_other_historically_unsealed_fit_fields"))
    .bh_pf_fail("ancestry","unsupported original class/profile/evidence claim")
  if(!identical(a$legacy_seals,.bh_pf_nested_seals(s))) .bh_pf_fail("ancestry","nested legacy seal records differ from saved context","E_PORTABLE_BINDING")
  .bh_pf_callback_markers(s,a$callback_disposition)
  backend<-.bh_pf_ordered(a$backend_disposition,"backend_disposition",c("slot","presence","disposition"))
  if(!identical(backend$slot,"fit.backend")||!backend$presence%in%c("present","absent")||
     !identical(backend$disposition,"omitted_wholesale_without_traversal_or_copy")) .bh_pf_fail("backend_disposition","unsupported omission marker")
  .bh_pf_protected_schema(core$protected_evidence,policy)
  h<-core$protected_evidence;p<-h$original_provenance
  .bh_pf_product_profile(s,h$original_fit_history,p)
  if(!identical(s$checks,h$original_scientific_checks)||!identical(s$fingerprint,a$original_spec_fingerprint)||
     !identical(s$fingerprint,a$original_fit_model_fingerprint)||!identical(s$fingerprint,p$model_fingerprint))
    .bh_pf_fail("ancestry","scientific checks or fit/spec identities disagree","E_PORTABLE_BINDING")
  artifacts<-.bh_pf_ordered(core$retained_artifacts,"retained_artifacts",c("saved_source_text","source_text_scope","resource_descriptors","run","original_execution_identities"))
  .bh_pf_character(artifacts$saved_source_text,"retained_artifacts.saved_source_text")
  if(!identical(artifacts$source_text_scope,"retained_text_export_time_integrity_not_original_compiler_file_authentication"))
    .bh_pf_fail("retained_artifacts","unsupported source authentication claim")
  .bh_pf_plain_tree(artifacts$run,"run",limit=policy$max_depth);.bh_pf_history_schema(artifacts$run,"run")
  resources<-list()
  for(n in names(s$models)) {
    adapter<-.bh_pf_get(.bh_pf_get(s$models[[n]],"model"),"adapter")
    if(!is.null(adapter))resources[n]<-list(.bh_pf_take(adapter,c("resources","function_code","function_exports","wrapper_hash")))
  }
  if(!identical(artifacts$resource_descriptors,resources)||!identical(artifacts$original_execution_identities,
     .bh_pf_take(p,c("compile_key","cache_hit","executable_sha256","toolchain","initialization","plan_identity","provider","provider_metadata"))))
    .bh_pf_fail("retained_artifacts","duplicated artifact or execution declarations disagree","E_PORTABLE_BINDING")
  source<-.bh_pf_ordered(core$source_binding,"source_binding",c("original_paths","configured_chain_context","execution_output","origin_evidence"))
  if(!identical(source$original_paths,.bh_pf_take(p,"output_files"))||
     !identical(source$configured_chain_context,.bh_pf_take(artifacts$run,c("chains","iter_warmup","iter_sampling","seed")))||
     !identical(source$execution_output,.bh_pf_get(.bh_pf_get(h,"original_execution_receipt"),"output"))||
     !identical(source$origin_evidence,"original_file_hash_evidence_retained_where_present_no_new_source_authentication_or_rebinding"))
    .bh_pf_fail("source_binding","paths, order, chain context or original output evidence disagree","E_PORTABLE_BINDING")
  output_paths<-.bh_pf_get(source$execution_output,"paths")
  if(!is.null(output_paths)&&!identical(output_paths,p$output_files)) .bh_pf_fail("source_binding","original provenance and execution paths disagree","E_PORTABLE_BINDING")
  expected<-.bh_pf_get(.bh_pf_get(.bh_pf_get(h,"original_execution_receipt"),"sampling"),"configured_chains")
  if(!is.null(expected)&&!identical(as.double(expected),as.double(.bh_pf_get(artifacts$run,"chains"))))
    .bh_pf_fail("source_binding","retained chain counts disagree","E_PORTABLE_BINDING")
  if(!identical(core$capabilities,.bh_pf_profile_capabilities(core$context_profile))) .bh_pf_fail("capabilities","unsupported immutable interface/exclusion profile")
  .bh_pf_original_hash_evidence(core)
  .bh_pf_stored_text(core)
  invisible(TRUE)
}

.bh_pf_context_receipt <- function(operation) list(
  schema_version=if(identical(operation,"component_draws")) "bmeas_component_context_validation_v1" else
    "bmeas_portable_context_validation_v1",operation=operation,status="refused",phase="policy_admission",
  context_profile="bmeas_postprocess_context_v1",identity=NULL,validation_policy=NULL,
  immutable_bytes=0.0,immutable_nodes=0.0,immutable_depth=0.0,count_passes=0.0,schema_validation_passes=0.0,
  immutable_encoding_passes=0.0,hash_bytes_written=0.0,hash_read_bytes_reserved=0.0,hash_file_cleanup="not_created",
  source_binding="not_verified",legacy_seals="ancestry_not_recomputed",
  source_origin_scope="export_time_integrity_and_retained_original_evidence_not_original_posterior_authentication",
  work_scope="immutable_context_only_separate_from_consumer_CSV_budgets_schema_checks_are_additional_no_RSS_or_elapsed_time_bound")
.bh_pf_context_error <- function(error,receipt) {
  if(typeof(error)=="list") {
    attrs<-attributes(error);value<-unclass(error)
    if("receipt"%in%names(value)) {
      nested<-.subset2(value,"receipt")
      if(is.null(nested)||(typeof(nested)=="list"&&!isS4(nested)))
        value["receipt"]<-list(.bh_pf_context_attach(nested,receipt))
    }
    value$context_validation<-receipt;attrs$names<-names(value);attributes(value)<-attrs
    stop(value)
  }
  stop(error)
}
.bh_pf_active <- function(fit,operation,archive_inspection=FALSE) {
  receipt<-.bh_pf_context_receipt(operation)
  tryCatch({
    .bh_pf_string(operation,"operation",c("refresh_diagnostics","individual_parameter_summary","assess_measurement_fit","component_draws"))
    root<-.bh_pf_envelope(fit);policy<-.bh_pf_stored_policy(root$immutable$validation_policy)
    receipt$context_profile<-root$immutable$context_profile
    .bh_pf_product_operation(root,operation);.bh_pf_joint_operation(root,operation)
    receipt$identity<-root$identity;receipt$validation_policy<-policy
    locale<-Sys.getlocale("LC_CTYPE");.bh_portable_codec_capabilities(locale)
    receipt$phase<-"immutable_count"
    counted<-.bh_portable_codec_count(root$immutable,policy,locale)
    receipt$immutable_bytes<-counted$bytes;receipt$immutable_nodes<-counted$nodes;receipt$immutable_depth<-counted$depth
    receipt$count_passes<-1.0;receipt$phase<-"immutable_schema_and_binding"
    .bh_pf_immutable_schema(root,policy)
    for(n in c("bytes","nodes","depth"))if(!identical(counted[[n]],root$export_receipt[[paste0("immutable_",n)]]))
      .bh_pf_fail("export_receipt","retained immutable accounting differs from the present core","E_PORTABLE_ACCOUNTING")
    receipt$schema_validation_passes<-1.0;receipt$source_binding<-"matched_retained_paths_and_context"
    receipt$phase<-"immutable_encoding_and_digest"
    receipt$hash_read_bytes_reserved<-counted$bytes
    receipt$hash_file_cleanup<-"owned_transaction_cleanup_attempted_on_failure"
    hashed<-.bh_pf_owned_hash(function(state) {
      base::suspendInterrupts({state$connection<-file(state$file,open="wb")})
      .bh_portable_codec_write(root$immutable,state$connection,policy,locale)
    },expected_bytes=counted$bytes)
    receipt$immutable_encoding_passes<-1.0;receipt$hash_bytes_written<-hashed$bytes
    receipt$hash_read_bytes_reserved<-hashed$counts$bytes;receipt$hash_file_cleanup<-"owned_file_removed_before_return"
    if(!identical(hashed$counts,counted)) .bh_pf_fail("immutable","encoding counts differ from admitted context","E_PORTABLE_ACCOUNTING")
    if(!identical(hashed$digest,root$identity$digest)) .bh_pf_fail("identity","immutable canonical digest did not match; no reseal is permitted","E_PORTABLE_IDENTITY")
    .bh_portable_codec_locale(locale)
    receipt$status<-"verified";receipt$phase<-"complete"
    # Mutable arrays are outside the immutable allowance. Only a shallow
    # container check and the reached fixed-schema receipt are new here;
    # existing consumer budgets/local validators govern consumed cache data.
    .bh_pf_cache_root(root$cache)
    .bh_pf_cache_receipts(root,operation)
    view<-.bh_pf_projection(root,operation)
    if(!isTRUE(archive_inspection)) .bh_pf_protected_gate(view,operation)
    list(view=view,receipt=receipt)
  },error=function(e).bh_pf_context_error(e,receipt),
    interrupt=function(e).bh_pf_context_error(e,receipt))
}
.bh_pf_after_validation <- function(validated,action) {
  tryCatch(action(),error=function(e).bh_pf_context_error(e,validated$receipt),
    interrupt=function(e).bh_pf_context_error(e,validated$receipt))
}
.bh_pf_projection <- function(root,operation) {
  core<-root$immutable;h<-core$protected_evidence;a<-core$retained_artifacts
  if(!identical(.bh_pf_class(core$scientific_context),"bmeas_portable_context")) .bh_pf_fail("scientific_context","expected the detached context class")
  .bh_pf_record(core$scientific_context,"scientific_context",c("formula","models","subjects","missing","data_map","parameter_keys","tables","prior","status","backend",
    "response_missing_semantics","observation_key_origin","schema_version","schema_migration","checks","fingerprint"),c("tables","data_map","models","fingerprint"),"bmeas_portable_context")
  .bh_pf_record(h,"protected_evidence",c("original_scientific_checks","original_provenance","original_fit_history","retained_cache_scope","original_execution_receipt","original_resource_outcome"),
    c("original_scientific_checks","original_provenance","original_fit_history","retained_cache_scope"))
  .bh_pf_ordered(a,"retained_artifacts",c("saved_source_text","source_text_scope","resource_descriptors","run","original_execution_identities"))
  .bh_pf_cache_root(root$cache)
  view<-list(spec=core$scientific_context,run=a$run,cache=root$cache,code=a$saved_source_text,
    provenance=h$original_provenance,portable_operation=operation,protected_evidence=h,portable_context_profile=core$context_profile)
  if("original_execution_receipt"%in%names(h))view["execution_receipt"]<-list(h$original_execution_receipt)
  if("original_resource_outcome"%in%names(h))view["resource_outcome"]<-list(h$original_resource_outcome)
  structure(view,class="bmeas_portable_consumer_view")
}
.bh_pf_metadata <- function(fit,operation) {
  root<-.bh_pf_envelope(fit);.bh_pf_product_operation(root,operation);.bh_pf_joint_operation(root,operation)
  .bh_pf_projection(root,operation)
}
.bh_pf_protected_gate <- function(view,operation) {
  h<-.bh_pf_named(.bh_pf_get(view,"protected_evidence"),"protected evidence")
  # The metadata planner consumes only these saved statuses. Validate their
  # local plain record/scalar shapes; do not traverse proof bodies or recheck
  # the scientific claim. Active routes already verified the immutable core.
  for(kind in c("original_scientific_checks","original_fit_history")) {
    owner<-.bh_pf_named(.bh_pf_get(h,kind),paste("protected",kind))
    for(n in c("posterior_propriety","posterior_normalizability")) {
      record<-.bh_pf_get(owner,n)
      if(is.null(record))next
      record<-.bh_pf_named(record,paste("protected",kind,n))
      status<-.bh_pf_get(record,"status")
      .bh_pf_string(status,paste("protected",kind,n,"status"))
      if(status%in%c("proven_improper","proven_non_normalizable")) {
        code<-switch(operation,refresh_diagnostics="E_DIAGNOSTIC_POSTERIOR",individual_parameter_summary="E_INDIVIDUAL_POSTERIOR",
          assess_measurement_fit="E_PPC_POSTERIOR",ppc_plan="E_PPC_POSTERIOR",component_draws="E_COMPONENT_POSTERIOR")
        .mp_abort(code,"Protected original evidence proves the target improper or non-normalizable; mutable cache edits cannot remove that evidence")
      }
    }
  }
  invisible(TRUE)
}
.bh_pf_context_attach <- function(receipt,context) {
  if(is.null(context))return(receipt)
  if(is.null(receipt))receipt<-list(status="not_acquired",reason="The posterior source phase did not produce a source receipt")
  if(typeof(receipt)!="list"||isS4(receipt)) .bh_pf_fail("source receipt","expected retained plain condition data")
  attrs<-attributes(receipt);value<-unclass(receipt)
  value$context_validation<-context;attrs$names<-names(value);attributes(value)<-attrs
  value
}
.bh_pf_commit_cache <- function(fit,updated) {
  # R copy-on-modify changes exactly one top-level field, never a core seal.
  answer<-unclass(fit);answer$cache<-.bh_pf_get(updated,"cache")
  attrs<-attributes(fit);attrs$names<-names(answer);attributes(answer)<-attrs
  answer
}
.bh_pf_saved_integrity <- function(fit) {
  root<-.bh_pf_envelope(fit);cache<-root$cache
  list(status="not_verified_by_this_passive_call",identity=root$identity,export_receipt=root$export_receipt,
    saved_active_receipts=list(diagnostics=.bh_pf_saved_receipt_path(cache,c("posterior_reporting","provenance","source","context_validation"),root,"refresh_diagnostics"),
      predictive=.bh_pf_saved_receipt_path(cache,c("ppc","metadata","source_receipt","context_validation"),root,"assess_measurement_fit")),
    producer_dispatch_status=root$immutable$capabilities$dispatch_status,
    runtime_support=.bh_pf_profile_runtime(root$immutable$context_profile),protected_original_evidence=root$immutable$protected_evidence)
}
summary.bmeas_portable_fit <- function(object,...) {
  result<-summary.bhmm_fit(.bh_pf_metadata(object,"summary"),...)
  attrs<-attributes(result);result<-unclass(result)
  result$portable_context<-.bh_pf_saved_integrity(object)
  attrs$names<-names(result);attrs$class<-c("summary.bmeas_portable_fit",attrs$class)
  attributes(result)<-attrs;result
}
print.summary.bmeas_portable_fit <- function(x,...) {
  cat("Portable post-processing fit | saved evidence; integrity not checked by this report\n")
  value<-.bh_pf_summary_report_columns(unclass(x))
  print.summary.bhmm_fit(value,...)
  .bh_pf_print_history(.bh_pf_print_history_node(value))
  invisible(x)
}
print.bmeas_portable_fit <- function(x,...) {
  print.summary.bmeas_portable_fit(summary.bmeas_portable_fit(x),...)
  invisible(x)
}
.bh_pf_print_history <- function(history) {
  if(is.null(history)) {
    cat("Protected original history: unavailable in saved portable report metadata\n")
    return(invisible(NULL))
  }
  for(kind in c("original_scientific_checks","original_fit_history")) {
    for(n in c("computation","numerical_evaluations","posterior_propriety","posterior_normalizability","identification","inference")) {
      status<-tryCatch({
        owner<-.bh_pf_named(.bh_pf_get(history,kind),paste("saved",kind))
        record<-.bh_pf_get(owner,n)
        if(is.null(record))NULL else {
          record<-.bh_pf_named(record,paste("saved",kind,n))
          value<-.bh_pf_get(record,"status")
          if(is.null(value))NULL else {.bh_pf_string(value,paste("saved",kind,n,"status"));value}
        }
      },error=function(e)"unavailable_invalid_saved_status")
      if(!is.null(status))cat("Protected original",kind,n,":",status,"\n")
    }
  }
  invisible(NULL)
}

# Exact inert equivalents of the six fields compared by .bh_builtin_kind.
# They contain no model objects/callbacks and invoke no model constructor.
.bh_pf_basic_kind <- function(model) {
  field<-function(type,role)structure(list(type=type,role=role,units=NULL,lower=-Inf,upper=Inf,width=1L,
    dimensions="one value per observation"),class="measurement_data_field")
  parameter<-function(support,predictor,receives,levels,units,description,predictor_units=units)
    list(support=support,predictor=predictor,receives=receives,levels=levels,units=units,predictor_units=predictor_units,
      description=description,transform_owner="adapter",prior_scale="must be declared by structure")
  density<-function(symbol,arguments,types)list(symbol=symbol,arguments=arguments,argument_types=types,normalized=TRUE,
    approximation="none",builtin=TRUE,source=NULL,source_file=NULL,resource_hash=NULL,exports=symbol,dependencies=character(),
    license="Stan Math",censoring="none",truncation="none")
  fields<-c("data_schema","parameters","likelihood","backend","constants","adapter")
  # List [fields] preserves the absent adapter field as an unnamed NULL, exactly
  # as the live canonical-factory comparison does.
  candidates<-list(
    gaussian=list(data_schema=list(y=field("real","response")),parameters=list(
      mu=parameter("real","identity","theta","subject_task","response","Conditional response mean"),
      sigma=parameter("positive","log","theta","task","response","Observation-level standard deviation","log(response/unit)")),
      likelihood=density("normal_lpdf",c("y","mu","sigma"),c("real","real","real")),backend="gaussian_cfa",constants=list()),
    binomial=list(data_schema=list(successes=field("nonnegative_integer","response"),trials=field("nonnegative_integer","auxiliary")),
      parameters=list(p=parameter("unit_interval","logit","eta","subject_task","probability","Success probability","log_odds")),
      likelihood=density("binomial_logit_lpmf",c("successes","trials","p"),c("int","int","real")),backend="basic_cfa",constants=list()),
    lognormal=list(data_schema=list(y=field("positive_real","response")),parameters=list(
      meanlog=parameter("real","identity","theta","subject_task","log(response/unit)","Log-response location"),
      sdlog=parameter("positive","log","theta","task","log(response/unit)","Log-response standard deviation","log(SD of log response)")),
      likelihood=density("lognormal_lpdf",c("y","meanlog","sdlog"),c("real","real","real")),backend="basic_cfa",constants=list()))
  for(n in names(candidates))if(identical(unclass(model)[fields],candidates[[n]][fields]))return(n)
  NA_character_
}

.bh_pf_context_receipt_schema <- function(x,root=NULL) {
  template<-.bh_pf_context_receipt(if(identical(.bh_pf_get(x,"operation"),"component_draws"))
    "component_draws" else "refresh_diagnostics")
  v<-.bh_pf_ordered(x,"context_validation",names(template))
  .bh_pf_string(v$operation,"context_validation.operation",c("refresh_diagnostics","individual_parameter_summary","assess_measurement_fit","component_draws"))
  .bh_pf_string(v$context_profile,"context_validation.context_profile",c("bmeas_postprocess_context_v1","bmeas_iid_gaussian_product_context_v1","bmeas_joint_m3cs_context_v1"))
  if(!identical(v$schema_version,template$schema_version)||!identical(v$status,"verified")||!identical(v$phase,"complete")||
     !v$context_profile%in%c("bmeas_postprocess_context_v1","bmeas_iid_gaussian_product_context_v1","bmeas_joint_m3cs_context_v1")||!identical(v$source_binding,"matched_retained_paths_and_context")||
     !identical(v$hash_file_cleanup,"owned_file_removed_before_return")) .bh_pf_fail("context_validation","stored successful context receipt is malformed")
  .bh_pf_identity(v$identity);.bh_pf_stored_policy(v$validation_policy)
  counters<-c("immutable_bytes","immutable_nodes","immutable_depth","count_passes","schema_validation_passes",
    "immutable_encoding_passes","hash_bytes_written","hash_read_bytes_reserved")
  for(n in counters) {
    if(!.bh_pf_plain(v[[n]],"double")) .bh_pf_fail("context_validation","accounting fields must be plain doubles")
    .bh_pf_number(v[[n]],paste0("context_validation.",n),1,(2^53 - 1),TRUE)
  }
  for(n in c("count_passes","schema_validation_passes","immutable_encoding_passes"))
    if(!identical(v[[n]],1.0)) .bh_pf_fail("context_validation","unsupported traversal count")
  for(n in c("legacy_seals","source_origin_scope","work_scope"))if(!identical(v[[n]],template[[n]]))
    .bh_pf_fail("context_validation","unsupported scope or origin claim")
  if(v$hash_bytes_written!=v$immutable_bytes||v$hash_read_bytes_reserved!=v$immutable_bytes||
     v$immutable_bytes>v$validation_policy$max_context_bytes||v$immutable_nodes>v$validation_policy$max_context_nodes||
     v$immutable_depth>v$validation_policy$max_depth) .bh_pf_fail("context_validation","stored work exceeds or disagrees with context policy")
  if(!is.null(root)) {
    if(!identical(v$context_profile,root$immutable$context_profile)||!identical(v$identity,root$identity)||!identical(v$validation_policy,root$immutable$validation_policy))
      .bh_pf_fail("context_validation","saved receipt names a different immutable authority","E_PORTABLE_BINDING")
    for(n in c("immutable_bytes","immutable_nodes","immutable_depth"))if(!identical(v[[n]],root$export_receipt[[n]]))
      .bh_pf_fail("context_validation","stored context counts differ from immutable export evidence","E_PORTABLE_BINDING")
  }
  invisible(TRUE)
}
.bh_pf_cache_receipts <- function(root,operation) {
  cache<-root$cache
  if(identical(operation,"assess_measurement_fit")) {
    ppc<-.bh_pf_get(cache,"ppc");m<-.bh_pf_get(ppc,"metadata")
    reader<-.bh_pf_get(m,"reader_execution")
    planned<-.bh_pf_get(.bh_pf_get(.bh_pf_get(m,"plan"),"source"),"reader_execution")
    product_receipt<-identical(.bh_pf_get(.bh_pf_get(.bh_pf_get(m,"source_receipt"),"context_validation"),"context_profile"),
      "bmeas_iid_gaussian_product_context_v1")
    if(!is.null(reader)||!is.null(planned)||product_receipt) {
      .bh_pf_ppc(ppc,"cache.ppc",.bh_pf_get(root$immutable$scientific_context,"tables"))
      if(identical(reader,.bh_pf_product_reader_capability())!=
         identical(root$immutable$context_profile,"bmeas_iid_gaussian_product_context_v1"))
        .bh_pf_fail("cache.ppc","saved reader capability names another producer profile","E_PORTABLE_BINDING")
      if(identical(reader,.bh_pf_product_reader_capability())) {
        identity<-.bh_pf_get(.bh_pf_get(m,"plan"),"model_identity")
        spec<-root$immutable$scientific_context
        provenance<-root$immutable$protected_evidence$original_provenance
        expected<-list(specification_fingerprint=.bh_pf_get(spec,"fingerprint"),
          saved_fit_model_fingerprint=.bh_pf_get(provenance,"model_fingerprint"),
          data_identity=.bh_pf_get(spec,"provenance"))
        if(!identical(identity,expected))
          .bh_pf_fail("cache.ppc","product predictive plan names another scientific context","E_PORTABLE_BINDING")
      }
    }
  }
  source<-switch(operation,
    refresh_diagnostics=.bh_pf_get(.bh_pf_get(.bh_pf_get(cache,"posterior_reporting"),"provenance"),"source"),
    assess_measurement_fit=.bh_pf_get(.bh_pf_get(.bh_pf_get(cache,"ppc"),"metadata"),"source_receipt"),NULL)
  receipt<-.bh_pf_get(source,"context_validation")
  if(!is.null(receipt)) {
    .bh_pf_context_receipt_schema(receipt,root)
    if(!identical(receipt$operation,operation)) .bh_pf_fail("context_validation","saved receipt is in another operation's slot")
  }
  invisible(TRUE)
}
.bh_pf_cache_root <- function(cache) {
  fields<-c("parameter_summary","fixed_parameters","parameter_roles","loading_cells","loading_primitives","loading_orientation","component_scales","components",
    "derived_marker_estimands","optional_reports","hmc","computation","numerical_evaluations","admissibility","posterior_propriety","posterior_normalizability",
    "gaussian_diagnostics","execution_profile","backend_qualification","identification","inference","predictive_fit","correlation_constraints","expanded_correlation_matrices",
    "reference_law","initialization","posterior_reporting","ppc","measurement_structure_fit","report_metadata","diagnostic_thresholds")
  .bh_pf_record(cache,"cache",fields,character())
  invisible(TRUE)
}
.bh_pf_metadata_notice <- function(fit) {
  root<-.bh_pf_envelope(fit)
  list(status="not_verified_by_this_metadata_call",identity=root$identity,
    producer_dispatch_status=root$immutable$capabilities$dispatch_status,
    runtime_support=.bh_pf_profile_runtime(root$immutable$context_profile),
    source_scope="saved_metadata_only_no_digest_file_provider_or_posterior_source_access")
}
.bh_pf_fit_checks <- function(fit) {
  out<-.bh_fit_checks(.bh_pf_metadata(fit,"fit_checks"))
  attrs<-attributes(out);out<-unclass(out)
  out$portable_context<-.bh_pf_saved_integrity(fit)
  attrs$names<-names(out);attrs$class<-c("bmeas_portable_fit_checks",attrs$class);attributes(out)<-attrs;out
}
print.bmeas_portable_fit_checks <- function(x,...) {
  cat("Portable post-processing fit | saved evidence; integrity not checked by this report\n")
  .bh_print_fit_checks(x,...)
  .bh_pf_print_history(.bh_pf_print_history_node(unclass(x)))
  invisible(x)
}
.bh_pf_active_route <- function(view,receipt,operation) {
  portable<-.bh_pf_is_view(view,operation)
  if(portable!=!is.null(receipt)) .bh_pf_fail(operation,"private portable views require the completed operation-specific validation receipt","E_PORTABLE_IDENTITY")
  if(portable) {
    .bh_pf_context_receipt_schema(receipt)
    if(!identical(receipt$operation,operation)||
       !identical(receipt$context_profile,.bh_pf_get(view,"portable_context_profile")))
      .bh_pf_fail(operation,"context receipt belongs to another operation or producer profile","E_PORTABLE_IDENTITY")
    if(identical(operation,"assess_measurement_fit")&&
       (.bh_pf_product_view(view)||.bh_pf_has_product(.bh_pf_get(view,"spec")))) {
      if(!.bh_pf_product_view(view)||!.bh_pf_has_product(.bh_pf_get(view,"spec")))
        .bh_pf_fail(operation,"predictive view lacks the complete product producer profile","E_PORTABLE_CAPABILITY")
      .bh_pf_product_profile(.bh_pf_get(view,"spec"))
      if(!identical(.bh_pf_get(.bh_pf_get(.bh_pf_get(view,"cache"),"inference"),"status"),"not_assessed"))
        .bh_pf_fail(operation,"product predictive reading must retain unassessed inference","E_PORTABLE_CAPABILITY")
    }
  }
  portable
}

# The qualified producer records no original file-content hashes. Do not
# invent a positive provenance status from the reader's separate MD5 argument.
# Unsupported retained evidence stays in the immutable core and refuses active
# consumption; it is neither erased nor relabelled as absent.
.bh_pf_original_hash_evidence <- function(core) {
  h<-core$protected_evidence
  nodes<-list(.bh_pf_get(core$source_binding$execution_output,"original_file_hashes"),
    .bh_pf_get(.bh_pf_get(h,"original_resource_outcome"),"original_file_hashes"))
  for(node in nodes) {
    if(is.null(node)||identical(node,"not_collected"))next
    value<-.bh_pf_record(node,"original_file_hashes",c("status","reason","algorithm","hashes","paths","scope"),c("status","reason"))
    .bh_pf_string(value$reason,"original_file_hashes.reason",empty=TRUE)
    if(!identical(value$status,"not_collected")||any(vapply(c("algorithm","hashes","paths","scope"),
        function(n)!is.null(.bh_pf_get(value,n)),logical(1))))
      .bh_pf_fail("original_file_hashes","present hashes or unknown positive-status evidence have no reviewed producer schema in this portable profile","E_PORTABLE_BINDING")
  }
  invisible(TRUE)
}
.bh_pf_public_input <- function(x,operation) {
  if(inherits(x,"bmeas_portable_consumer_view")||inherits(x,"bmeas_portable_context"))
    .bh_pf_fail(operation,"pass the complete bmeas_portable_fit to a public consumer; an internal view or detached context cannot replace fresh validation","E_PORTABLE_UNSUPPORTED")
  invisible(NULL)
}
.bh_pf_saved_receipt <- function(x,root,operation) {
  if(is.null(x))return(list(availability="not_retained",record=NULL,
    reason="No saved receipt for this active operation"))
  tryCatch({
    .bh_pf_context_receipt_schema(x,root)
    if(!identical(x$operation,operation)) .bh_pf_fail("context_validation","saved receipt belongs to a different operation")
    list(availability="retained_not_reverified",record=x,
      reason="Structurally consistent saved receipt; no integrity verification occurred in this passive call")
  },error=function(e)list(availability="unavailable_invalid_saved_receipt",record=NULL,
    reason="Saved context-verification metadata is malformed or names another immutable authority"))
}

.bh_pf_saved_receipt_path <- function(cache,path,root,operation) {
  tryCatch({
    value<-cache
    for(field in path) {
      if(is.null(value))return(.bh_pf_saved_receipt(NULL,root,operation))
      value<-.bh_pf_named(value,paste0("saved receipt path before ",field))
      value<-.bh_pf_get(value,field)
    }
    .bh_pf_saved_receipt(value,root,operation)
  },error=function(e)list(availability="unavailable_invalid_saved_receipt",record=NULL,
    reason="Saved context-verification metadata has a malformed intermediate container"))
}
.bh_pf_print_history_node <- function(report) {
  # This new optional report node is independently guarded. A malformed node
  # must not erase unrelated valid sampling/fit evidence or call saved methods.
  tryCatch({
    node<-.bh_pf_get(report,"portable_context")
    if(is.null(node))return(NULL)
    value<-.bh_pf_ordered(node,"saved portable report metadata",c("status","identity","export_receipt","saved_active_receipts",
      "producer_dispatch_status","runtime_support","protected_original_evidence"))
    legacy_notice<-identical(value$producer_dispatch_status,"stage1_export_only_consumers_closed")&&
      (identical(value$runtime_support,"portable_postprocessing_v1")||
       identical(value$runtime_support,"portable_postprocessing_v1_runtime_qualification_pending"))
    product_notice<-identical(value$producer_dispatch_status,"experimental_product_saved_value_consumers_only_no_inference_or_recovery_qualification")&&
      identical(value$runtime_support,"portable_iid_gaussian_product_postprocessing_v1")
    joint_notice<-identical(value$producer_dispatch_status,"joint_m3cs_R_only_saved_controller_no_native_or_inference_qualification")&&
      identical(value$runtime_support,"portable_joint_m3cs_postprocessing_v1")
    if(!identical(value$status,"not_verified_by_this_passive_call")||!(legacy_notice||product_notice||joint_notice))
      .bh_pf_fail("saved portable report metadata","unsupported passive status or capability notice")
    .bh_pf_identity(value$identity);.bh_pf_export_receipt_schema(value$export_receipt)
    .bh_pf_ordered(value$saved_active_receipts,"saved active receipt notices",c("diagnostics","predictive"))
    history<-.bh_pf_record(value$protected_original_evidence,"saved protected history",
      c("original_scientific_checks","original_provenance","original_fit_history","retained_cache_scope","original_execution_receipt","original_resource_outcome"),
      c("original_scientific_checks","original_provenance","original_fit_history","retained_cache_scope"))
    history
  },error=function(e)NULL)
}

.bh_pf_passive_record <- function(x,label) {
  tryCatch(.bh_pf_named(x,label),error=function(e)list())
}
.bh_pf_summary_table <- function(x,plain_columns=character()) {
  if(is.null(x))return(NULL)
  tryCatch({
    if(typeof(x)!="list"||!identical(.bh_pf_class(x),"data.frame")) .bh_pf_fail("summary table","expected a plain saved table")
    n<-.bh_pf_names(x)
    .bh_pf_character(n,"summary table names",TRUE,TRUE)
    if(length(n)!=.bh_pf_length(x)) .bh_pf_fail("summary table","ambiguous columns")
    # Only columns read by the legacy plain_column display predicate need this
    # additional guard. Check class before any generic dimension operation;
    # preserve inert alias/list columns that this display never consumes.
    values<-unclass(x)
    for(field in plain_columns[plain_columns%in%n]) {
      value<-.subset2(values,field)
      if(isS4(value)||is.object(value))
        .bh_pf_fail("summary table","a consumed display column has an unsupported saved class")
    }
    .bh_pf_table_shape(x,"summary table")
    x
  },error=function(e)NULL)
}
.bh_pf_summary_metadata <- function(view) {
  # Only fields read directly by the existing summary are projected. This is
  # not a full scientific/cache traversal or a prepared specification.
  object<-unclass(view);spec<-unclass(.bh_pf_get(object,"spec"))
  tables<-.bh_pf_passive_record(.bh_pf_get(spec,"tables"),"summary tables")
  for(n in c("components","component_groups","amplitudes","task_target_domain","share_targets","share_coefficients","parameter_residuals","targets"))
    if(n%in%names(tables))tables[n]<-list(.bh_pf_summary_table(.bh_pf_get(tables,n),.bh_pf_summary_column_names(n)))
  for(n in c("person_law","augmentation","correlation_coordinate_system"))
    if(n%in%names(tables))tables[n]<-list(.bh_pf_passive_record(.bh_pf_get(tables,n),paste("summary",n)))
  spec$tables<-tables
  cache<-.bh_pf_passive_record(.bh_pf_get(object,"cache"),"summary cache")
  for(n in c("parameter_roles","fixed_parameters","loading_cells","loading_primitives","component_scales"))
    if(n%in%names(cache))cache[n]<-list(.bh_pf_summary_table(.bh_pf_get(cache,n),.bh_pf_summary_column_names(n)))
  constraints<-.bh_pf_get(cache,"correlation_constraints")
  if(!is.null(constraints)) {
    constraints<-.bh_pf_passive_record(constraints,"summary correlation constraints")
    constraints["primitives"]<-list(.bh_pf_summary_table(.bh_pf_get(constraints,"primitives")))
    cache$correlation_constraints<-constraints
  }
  object$cache<-cache;object$spec<-spec
  object$provenance<-.bh_pf_passive_record(.bh_pf_get(object,"provenance"),"summary provenance")
  object
}
.bh_pf_summary_count <- function(checks,path) {
  tryCatch({
    value<-checks
    for(field in path)value<-.bh_pf_get(value,field)
    .bh_pf_number(value,"saved summary count",0,(2^53 - 1),TRUE)
    value
  },error=function(e)NA_real_)
}
.bh_pf_legacy_report_input <- function(x,operation,allow_private=FALSE) {
  if(.bh_pf_is_portable(x)||inherits(x,"bmeas_portable_context")||
     (inherits(x,"bmeas_portable_consumer_view")&&!(allow_private&&.bh_pf_is_view(x))))
    .bh_pf_fail(operation,"portable tags require their complete portable public report route","E_PORTABLE_UNSUPPORTED")
  invisible(NULL)
}

.bh_pf_summary_column_names <- function(table) switch(table,
  parameter_roles=c("role","level"),task_target_domain=c("level","has_symbolic_support"),
  loading_cells="state",component_scales="state",character())
.bh_pf_summary_report_columns <- function(report) {
  # Saved summaries can be edited after projection. Apply the same narrow
  # reached-column guard again before delegating their passive display.
  for(n in c("parameter_roles","task_target_domain","loading_cells","component_scales"))
    if(n%in%names(report))report[n]<-list(.bh_pf_summary_table(.bh_pf_get(report,n),.bh_pf_summary_column_names(n)))
  report
}
