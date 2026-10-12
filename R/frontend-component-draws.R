# Aligned component-population extraction. No model, provider or saved callback
# is evaluated. The finite-only shared CSV reader remains unchanged.
.bh_component_fail <- function(message, code="E_COMPONENT_CONTEXT") .mp_abort(code,message)
.bh_component_get <- function(x,name) .bh_pf_get(x,name)
.bh_component_named <- function(x,label,classes=NULL) {
  if(!identical(.bh_pf_class(x),classes))
    .bh_component_fail(paste(label,"has an unsupported class"))
  if(typeof(x)!="list"||length(unclass(x))>256L)
    .bh_component_fail(paste(label,"exceeds the reviewed reached-record field profile"))
  .bh_pf_named(unclass(x),label)
}
.bh_component_count <- function(x,label,lower=1,upper=2^53-1) {
  .bh_pf_number(x,label,lower,upper,TRUE)
  as.double(x)
}
.bh_component_add <- function(...) .bh_posterior_sum(c(...),"component accounting")
.bh_component_mul <- function(a,b) .bh_posterior_mul(a,b,"component accounting")
.bh_component_budget <- function(budget) {
  fields<-c("posterior","max_components","max_output_values","max_matrix_work","context")
  cap<-.bh_pf_record(budget,"component budget",fields)
  cap$max_components<-.bh_component_count(cap$max_components,"max_components",upper=.Machine$integer.max)
  for(n in c("max_output_values","max_matrix_work"))
    cap[[n]]<-.bh_component_count(cap[[n]],n)
  cap$context<-.bh_pf_policy(cap$context)
  .bh_posterior_consumer_policy(cap$posterior,character())
  cap
}

# The metadata phase accounts for canonical storage, both traversals, numeric
# metadata and explicit index/copy workspace. It is not an R heap/RSS bound.
# Codec nodes count atomic cells as well as containers/attributes, so N bounds
# the number of numeric metadata cells. Workspace is conservatively 8N+32H+64
# numeric slots. Schema-local scans fit inside that reservation; their logical
# work is disclosed separately from per-state matrix work.
.bh_component_ledger <- function(policy) {
  state<-new.env(parent=emptyenv())
  state$policy<-policy;state$bytes<-0.0;state$nodes<-0.0;state$depth<-0.0
  state$phases<-list();state$locale<-Sys.getlocale("LC_CTYPE")
  .bh_portable_codec_capabilities(state$locale)
  .bh_component_reserve(state,"accounting_receipt_and_fixed_records",8192.0,1024.0,8.0)
  state
}
.bh_component_reserve <- function(state,phase,bytes,nodes,depth,details=NULL) {
  total_bytes<-.bh_component_add(state$bytes,bytes)
  total_nodes<-.bh_component_add(state$nodes,nodes)
  if(total_bytes>state$policy$max_context_bytes||total_nodes>state$policy$max_context_nodes||
     depth>state$policy$max_depth)
    .bh_component_fail(paste("metadata storage/traversal/workspace allowance exceeded in",phase),"E_COMPONENT_CONTEXT_BUDGET")
  state$bytes<-total_bytes;state$nodes<-total_nodes;state$depth<-max(state$depth,depth)
  state$phases[[phase]]<-list(reserved_bytes=bytes,reserved_nodes=nodes,depth=depth,details=details)
  invisible(NULL)
}
.bh_component_admit <- function(x,state,phase) {
  remaining<-list(max_context_bytes=state$policy$max_context_bytes-state$bytes,
    max_context_nodes=state$policy$max_context_nodes-state$nodes,max_depth=state$policy$max_depth)
  counted<-.bh_portable_codec_count(x,remaining,state$locale)
  slots<-.bh_component_add(.bh_component_mul(8,counted$nodes),.bh_component_mul(32,counted$depth),64)
  bytes<-.bh_component_add(.bh_component_mul(3,counted$bytes),.bh_component_mul(8,slots))
  nodes<-.bh_component_add(.bh_component_mul(4,counted$nodes),.bh_component_mul(32,counted$depth),64)
  .bh_component_reserve(state,phase,bytes,nodes,counted$depth,
    list(canonical_bytes=counted$bytes,canonical_nodes=counted$nodes,
      count_passes=1.0,copy_passes=0.0,copy_passes_reserved=1.0,
      schema_validation_passes_reserved=1.0,numeric_metadata_slots_upper_bound=counted$nodes,
      index_and_copy_workspace_slots=slots,canonical_storage_copies_reserved=3.0))
  invisible(counted)
}
.bh_component_copy <- function(x,state,phase) {
  out<-.bh_portable_codec_copy(x,state$policy,state$locale)
  state$phases[[phase]]$details$copy_passes<-1.0
  out
}
.bh_component_context_receipt <- function(state) list(
  schema_version="bmeas_component_metadata_work_v1",policy=state$policy,
  reserved_bytes=state$bytes,reserved_nodes=state$nodes,maximum_depth=state$depth,
  phases=state$phases,
  scope="Copied annotation data, character statuses, labels, numeric metadata and explicit traversal/index/copy workspace; not total RSS or elapsed time")

.bh_component_status_projection <- function(owner,label) {
  if(is.null(owner))return(list(availability="not_retained",statuses=list(),summary_availability="not_retained"))
  owner<-.bh_component_named(owner,label)
  fields<-c("computation","inference","identification","posterior_propriety","posterior_normalizability",
    "numerical_evaluations","admissibility","optional_reports","posterior_reporting","gaussian_diagnostics",
    "execution_profile","backend_qualification","predictive_fit","structure","correlation_feasibility",
    "correlation_lowering","correlation_prior_geometry","numerical_qualification",
    "optional_report_history","expanded_report_history","predictive_attempt_history")
  selected<-list()
  for(n in fields) {
    node<-.bh_component_get(owner,n)
    if(is.null(node))next
    node<-.bh_component_named(node,paste(label,n))
    selected[[n]]<-.bh_pf_take(node,c("status","reason","availability","summary_availability","summary_available"))
  }
  reporting<-.bh_component_get(owner,"posterior_reporting")
  phases<-.bh_component_get(reporting,"phases")
  if(!is.null(phases)) {
    if(typeof(phases)=="character") {
      phase_names<-names(phases)
      if(length(phases)!=6L||!is.null(.bh_pf_class(phases))||
         any(!names(attributes(phases))%in%"names")||is.null(phase_names)||
         anyNA(phase_names)||anyDuplicated(phase_names)||
         !setequal(phase_names,c("parameter_summary","hmc","conditioning","availability_and_covariance","expanded_summary","numerical_messages")))
        .bh_component_fail("Saved reporting phases require distinct names")
      phase_records<-list()
      for(n in phase_names)phase_records[[n]]<-list(status=unname(phases[[n]]))
      phases<-phase_records
    } else phases<-.bh_component_named(phases,paste(label,"posterior_reporting phases"))
    selected$reporting_phases<-list()
    for(n in c("parameter_summary","hmc","conditioning","availability_and_covariance","expanded_summary","numerical_messages")) {
      node<-.bh_component_get(phases,n)
      if(is.null(node))next
      node<-.bh_component_named(node,paste(label,"reporting phase",n))
      selected$reporting_phases[[n]]<-.bh_pf_take(node,c("status","reason","availability"))
    }
  }
  numerical<-.bh_component_get(owner,"numerical_evaluations")
  capture<-.bh_component_get(numerical,"output_capture")
  if(!is.null(capture)) {
    capture<-.bh_component_named(capture,paste(label,"numerical output capture"))
    selected$numerical_output_capture<-.bh_pf_take(capture,c("status","reason"))
  }
  if(!is.null(numerical))selected$numerical_counts<-.bh_pf_take(numerical,
    c("observed_failure_messages","total_failed_evaluations","coverage"))
  list(availability="retained_selected_fields",statuses=selected,
    summary_availability=if("parameter_summary"%in%names(owner)&&!is.null(.bh_component_get(owner,"parameter_summary")))
      "retained_not_acquired_or_reassessed" else "not_recorded_in_this_owner",
    hmc_availability=if(!is.null(.bh_component_get(owner,"hmc")))"retained_not_acquired_or_reassessed"else"not_recorded_in_this_owner")
}
.bh_component_status_validate <- function(projection) {
  inspect<-function(records) for(n in names(records)) {
    record<-records[[n]]
    if(n=="reporting_phases") {inspect(record);next}
    for(field in names(record)) {
      value<-record[[field]]
      if(field%in%c("observed_failure_messages","total_failed_evaluations"))
        .bh_pf_nullable_numeric(value,paste(n,field)) else
      if(field=="summary_available") .bh_pf_logical(value,paste(n,field)) else
        .bh_pf_string(value,paste(n,field),empty=TRUE)
    }
    status<-.bh_component_get(record,"status")
    if(!is.null(status)&&n%in%c("posterior_propriety","posterior_normalizability")&&
       status%in%c("proven_improper","proven_non_normalizable"))
      .bh_component_fail("Retained original or current evidence proves the posterior improper or non-normalizable","E_COMPONENT_POSTERIOR")
  }
  for(owner in projection)inspect(owner$statuses)
  invisible(TRUE)
}
.bh_component_table_fields <- function() c(
  "components","coordinates","component_groups","basis_rows","basis_multiplier","basis_amplitude_index",
  "component_scales","orientation","orientation_sign_system","loadings","free_loadings","amplitudes",
  "loading_coefficients","loading_map","loading_cells","loading_primitives","loading_constraint_rules",
  "ordinary_loading_map","ordinary_fixed_matrix","correlations","correlation_mask","correlation_affine_cells",
  "correlation_primitives","correlation_constraint_rules","correlation_affine_constants","correlation_domains",
  "correlation_coordinate_system","correlation_blocks","raw_correlation_coordinates","correlation_prior_geometry",
  "required_component_zeros","priors","person_law","augmentation")

.bh_component_plan <- function(fit,cap,ledger,context_validation=NULL) {
  portable<-.bh_pf_active_route(fit,context_validation,"component_draws")
  cls<-if(portable)"bmeas_portable_consumer_view"else c("bmeas_fit","bhmm_fit")
  root<-.bh_component_named(fit,"fit",cls)
  original_spec<-.bh_component_get(root,"spec")
  spec<-.bh_component_named(original_spec,"spec",if(portable)"bmeas_portable_context"else c("bmeas_spec","bhmm_spec"))
  tables<-.bh_component_named(.bh_component_get(spec,"tables"),"spec tables")
  required<-setdiff(.bh_component_table_fields(),c("orientation_sign_system","required_component_zeros"))
  if(any(!required%in%names(tables))) .bh_component_fail("Saved component scientific maps are incomplete")
  coordinates<-.bh_component_get(tables,"coordinates")
  if(!identical(.bh_pf_class(coordinates),"data.frame")||typeof(coordinates)!="list")
    .bh_component_fail("Coordinates must be a plain saved table")
  K<-.bh_component_count(.row_names_info(coordinates,2L),"component count",0,.Machine$integer.max)
  if(K>cap$max_components) .bh_component_fail("Component count exceeds max_components","E_COMPONENT_BUDGET")
  square<-.bh_component_mul(K,K)
  V<-.bh_component_add(.bh_component_mul(2,square),K)
  W<-.bh_component_add(.bh_component_mul(6,square),.bh_component_mul(2,K),5)
  work<-.bh_component_add(.bh_component_mul(square,K),.bh_component_mul(12,square),.bh_component_mul(2,K))
  if(V>.Machine$integer.max||square>if(.Machine$sizeof.pointer==8L)2^52 else .Machine$integer.max)
    .bh_component_fail("Component dimensions exceed supported R representation","E_COMPONENT_CAPABILITY")
  if(K&&(W>cap$max_output_values||work>cap$max_matrix_work))
    .bh_component_fail("Even one component state exceeds its output/work allowance","E_COMPONENT_BUDGET")
  provenance<-.bh_component_named(.bh_component_get(root,"provenance"),"provenance")
  run<-.bh_component_named(.bh_component_get(root,"run"),"run","measurement_run_config")
  upstream<-list(current_cache=.bh_component_status_projection(.bh_component_get(root,"cache"),"cache"),
    original_scientific_checks=.bh_component_status_projection(.bh_component_get(spec,"checks"),"spec checks"),
    original_provenance_checks=.bh_component_status_projection(.bh_component_get(provenance,"scientific_checks"),"provenance scientific checks"))
  if(portable) {
    history<-.bh_component_named(.bh_component_get(root,"protected_evidence"),"protected evidence")
    upstream$protected_scientific_checks<-.bh_component_status_projection(history$original_scientific_checks,"protected scientific checks")
    upstream$protected_fit_history<-.bh_component_status_projection(history$original_fit_history,"protected fit history")
  }
  precision<-list(run=.bh_component_get(run,"csv_sig_figs"),provenance=.bh_component_get(provenance,"csv_sig_figs"))
  prototype<-list(tables=.bh_pf_take(tables,.bh_component_table_fields()),prior=.bh_component_get(spec,"prior"),
    upstream=upstream,source_context=list(paths=.bh_component_get(provenance,"output_files"),
      spec_fingerprint=.bh_component_get(spec,"fingerprint"),fit_fingerprint=.bh_component_get(provenance,"model_fingerprint"),
      expected_chains=.bh_posterior_expected_chains(fit),recorded_precision=precision),
    context_validation=context_validation)
  .bh_component_admit(prototype,ledger,"scientific_annotation")
  for(n in names(prototype$tables)) {
    .bh_pf_plain_tree(prototype$tables[[n]],c("spec","tables",n),limit=cap$context$max_depth)
    .bh_portable_scientific_schema(prototype$tables[[n]],paste0("spec.tables.",n))
  }
  .bh_pf_plain_tree(prototype$prior,c("spec","prior"),limit=cap$context$max_depth)
  .bh_portable_scientific_schema(prototype$prior,"spec.prior")
  .bh_component_status_validate(upstream)
  .bh_component_maps(prototype$tables,K)
  identity<-prototype$source_context
  .bh_pf_string(identity$spec_fingerprint,"spec fingerprint")
  .bh_pf_string(identity$fit_fingerprint,"fit fingerprint")
  if(!identical(identity$spec_fingerprint,identity$fit_fingerprint))
    .bh_component_fail("Saved fit and specification fingerprints disagree","E_COMPONENT_IDENTITY")
  for(value in precision)if(!is.null(value)).bh_component_count(value,"recorded csv_sig_figs",1,18)
  if(!is.null(precision$run)&&!is.null(precision$provenance)&&precision$run!=precision$provenance)
    .bh_component_fail("Recorded run and provenance precision disagree","E_COMPONENT_PRECISION")
  if(K) {
    .bh_pf_character(identity$paths,"output files",TRUE,TRUE)
    if(!length(identity$paths)) .bh_component_fail("No retained posterior paths are available","E_COMPONENT_SOURCE")
    if(!is.null(identity$expected_chains$value)&&length(identity$paths)!=identity$expected_chains$value)
      .bh_component_fail("Recorded file count contradicts the saved chain count","E_COMPONENT_SOURCE")
  }
  # This established legacy seal operation has separately disclosed unknown
  # serialization cost. It is never silently included in CSV/context budgets.
  if(!portable) .bh_diagnostic_refresh_validate_spec(original_spec)
  metadata<-.bh_component_copy(prototype,ledger,"scientific_annotation")
  rules<-.bh_component_rules(metadata$tables,K)
  .bh_component_reserve(ledger,"required_column_names",
    .bh_component_mul(V,192),.bh_component_mul(V,8),4)
  variables<-character(V)
  at<-0L
  for(j in seq_len(K))for(i in seq_len(K)) {at<-at+1L;variables[[at]]<-paste0("R_component[",i,",",j,"]")}
  for(i in seq_len(K)) {at<-at+1L;variables[[at]]<-paste0("component_row_sd[",i,"]")}
  for(j in seq_len(K))for(i in seq_len(K)) {at<-at+1L;variables[[at]]<-paste0("component_covariance_available[",i,",",j,"]")}
  list(K=K,square=square,V=V,W=W,matrix_work=work,variables=variables,metadata=metadata,rules=rules,
    portable=portable,budget=cap)
}

.bh_component_maps <- function(tables,K) {
  coord<-unclass(tables$coordinates);scales<-unclass(tables$component_scales)
  components<-unclass(tables$components);cor<-unclass(tables$correlations)
  if(!identical(as.integer(coord$index),as.integer(seq_len(K)))||!identical(coord$display,components$component)||
     !identical(scales$coordinate_id,coord$id)||!identical(scales$component,coord$display)||
     !identical(as.integer(scales$component_index),as.integer(seq_len(K))))
    .bh_component_fail("Coordinate and scale maps disagree")
  if(!identical(attr(tables$correlation_mask,"dim",exact=TRUE),as.integer(c(K,K)))||
     length(cor$row)!=K*(K-1)/2||any(cor$col>K))
    .bh_component_fail("Correlation maps do not cover the component domain")
  if(length(cor$row)&&(!identical(cor$component1,coord$display[cor$row])||
      !identical(cor$component2,coord$display[cor$col])))
    .bh_component_fail("Correlation endpoints disagree with coordinate IDs")
  cells<-unclass(tables$correlation_affine_cells)
  at<-which(cells$layer=="component")
  if(length(at)!=length(cor$row)||!identical(cells$row[at],cor$row)||!identical(cells$col[at],cor$col)||
     !identical(cells$state[at],cor$state))
    .bh_component_fail("Affine cell map disagrees with complete component edges")
  for(e in seq_along(cor$row)) {
    i<-cor$row[[e]];j<-cor$col[[e]];a<-at[[e]]
    if(.subset(tables$correlation_mask,i,j)!=cor$state[[e]]) .bh_component_fail("Correlation mask disagrees with edge state")
    if(cor$state[[e]]=="zero"&&!isTRUE(cells$fixed_value[[a]]==0)) .bh_component_fail("A symbolic zero has no zero constant")
    if(cor$state[[e]]=="fixed"&&(!is.finite(cells$fixed_value[[a]])||
       !isTRUE(cells$fixed_value[[a]]==cor$fixed_value[[e]]))) .bh_component_fail("Fixed correlation constants disagree")
    if(cor$state[[e]]=="free"&&!identical(tables$correlation_coordinate_system$type,"ordered_blocks")&&
       !identical(cells$parameter[[a]],cor$parameter[[e]])) .bh_component_fail("Equality-label maps disagree")
  }
  invisible(TRUE)
}
.bh_component_rules <- function(tables,K) {
  constants<-matrix(NA_real_,K,K);representative<-matrix(0L,K,K)
  cells<-unclass(tables$correlation_affine_cells);at<-which(cells$layer=="component")
  labels<-cells$parameter[at];first<-match(labels,labels)
  for(e in seq_along(at)) {
    a<-at[[e]];i<-cells$row[[a]];j<-cells$col[[a]]
    if(cells$state[[a]]%in%c("zero","fixed")) constants[i,j]<-constants[j,i]<-cells$fixed_value[[a]]
    else if(!is.na(labels[[e]])) {
      b<-at[[first[[e]]]]
      representative[i,j]<-representative[j,i]<-cells$row[[b]]+(cells$col[[b]]-1L)*K
    }
  }
  list(constants=constants,representative=representative)
}

# Log envelopes keep subnormal decimal bounds present and avoid overflow.
# Zero is a represented zero; this function does not declare a symbolic zero.
.bh_component_log_envelope <- function(x,p) {
  if(x==0)return(-Inf)
  log(0.5)+(floor(log10(abs(x)))-p+1)*log(10)
}
.bh_component_log_add <- function(a,b) {
  if(a== -Inf)return(b)
  if(b== -Inf)return(a)
  high<-max(a,b)
  high+log1p(exp(min(a,b)-high))
}
.bh_component_agrees <- function(x,y,p,K,exact=FALSE) {
  residual<-abs(x-y)
  if(residual==0)return(TRUE)
  if(!is.finite(residual))return(FALSE)
  bound<-.bh_component_log_envelope(x,p)
  if(!exact)bound<-.bh_component_log_add(bound,.bh_component_log_envelope(y,p))
  machine<-log(64)+log(K)+log(.Machine$double.eps)+log(max(1,abs(x),abs(y)))
  log(residual)<=.bh_component_log_add(bound,machine)
}
.bh_component_matrix <- function(R,p,rules,K) {
  diagonal<-symmetry<-constraint<-0.0
  diagonal_ok<-symmetry_ok<-constraint_ok<-TRUE
  exactly_symmetric<-TRUE;range_ok<-TRUE;known<-!is.na(p)
  for(j in seq_len(K))for(i in seq_len(K)) {
    x<-R[i,j]
    if(abs(x)>1)range_ok<-FALSE
    if(x!=R[j,i])exactly_symmetric<-FALSE
    symmetry<-max(symmetry,abs(x-R[j,i]))
    if(known&&! .bh_component_agrees(x,R[j,i],p,K))symmetry_ok<-FALSE
    if(i==j) {
      diagonal<-max(diagonal,abs(x-1))
      if(known&&! .bh_component_agrees(x,1,p,K,TRUE))diagonal_ok<-FALSE
    } else {
      fixed<-rules$constants[i,j];ref<-rules$representative[i,j]
      if(!is.na(fixed)||ref>0L) {
        y<-if(!is.na(fixed))fixed else R[[ref]]
        constraint<-max(constraint,abs(x-y))
        if(known&&! .bh_component_agrees(x,y,p,K,!is.na(fixed)))constraint_ok<-FALSE
      }
    }
  }
  pd<-if(!exactly_symmetric)"not_assessed_nonexact_symmetry"else
    tryCatch({base::chol.default(R,pivot=FALSE);"positive_definite_at_saved_precision"},
      error=function(e)"not_positive_definite_at_saved_precision")
  relation<-function(ok)if(!known)"not_assessed_unknown_precision"else
    if(ok)"consistent_with_rounding_envelope"else"outside_rounding_envelope"
  list(range=if(range_ok)"finite_within_correlation_range"else"outside_correlation_range",
    exact_symmetry=if(exactly_symmetric)"exactly_symmetric"else"not_exactly_symmetric",
    diagonal=relation(diagonal_ok),symmetry=relation(symmetry_ok),constraints=relation(constraint_ok),
    positive_definite=pd,max_diagonal_residual=diagonal,max_symmetry_residual=symmetry,max_constraint_residual=constraint)
}
.bh_component_precision <- function(sources,recorded) {
  src<-unclass(sources);p<-src$sig_figs
  status<-character(length(p))
  for(i in seq_along(p)) {
    if(is.na(p[[i]])||p[[i]]== -1) {p[[i]]<-NA_real_;status[[i]]<-"unknown"}
    else {
      .bh_component_count(p[[i]],"source sig_figs",1,18)
      status[[i]]<-"declared_decimal_precision"
    }
    for(expected in recorded)if(!is.null(expected)&&(is.na(p[[i]])||p[[i]]!=expected))
      .bh_component_fail("Explicit recorded precision contradicts a source header","E_COMPONENT_PRECISION")
  }
  list(chain_id=src$chain_id,sig_figs=p,status=status,
    scope="Per-chain source facts; missing/provider-default precision remains unknown",
    comparison_rule="e_p(0)=0; otherwise log(e_p(x))=log(0.5)+(floor(log10(abs(x)))-p+1)*log(10). Saved pairs use e_p(x)+e_p(y)+64*K*epsilon*max(1,abs(x),abs(y)); exact constants omit e_p(y). Log-domain comparison preserves subnormal envelopes.",
    interpretation="A consistency envelope, not numerical equivalence, scientific accuracy or permission to repair R")
}

.bh_component_rows_admit <- function(D,K,ledger,axis_labels) {
  # Six character status columns, three residual columns, compact row names,
  # array dimension/axis metadata and construction/copy overlap. 96 bytes per
  # status is greater than every literal emitted by .bh_component_matrix.
  labels<-.bh_portable_codec_count(axis_labels,ledger$policy,ledger$locale)
  # Repeated axis occurrences count even when R shares the underlying strings.
  # Six matrix axes and two SD axes need fourteen coordinate-vector occurrences;
  # 16 copies of this three-occurrence prototype cover storage/copy traversal.
  bytes<-.bh_component_add(.bh_component_mul(D,1024),.bh_component_mul(K,256),4096,
    .bh_component_mul(16,labels$bytes))
  nodes<-.bh_component_add(.bh_component_mul(D,64),.bh_component_mul(K,64),1024,
    .bh_component_mul(16,labels$nodes))
  .bh_component_reserve(ledger,"draw_status_and_dimension_metadata",bytes,nodes,8,
    list(draws=D,status_columns=6.0,residual_columns=3.0,status_text_bytes_per_cell_reserved=96.0,
      axis_prototype_bytes=labels$bytes,axis_prototype_nodes=labels$nodes,axis_prototype_repetitions_reserved=16.0,
      scope="Includes construction and copy traversal; numeric residuals also count in W"))
}
.bh_component_result <- function(plan,acquired,ledger,context_validation) {
  K<-plan$K;cap<-plan$budget
  D<-if(is.null(acquired))0.0 else as.double(nrow(acquired$draws))
  if(.bh_component_mul(D,plan$W)>cap$max_output_values||
     .bh_component_mul(D,plan$matrix_work)>cap$max_matrix_work)
    .bh_component_fail("Returned source dimensions exceed admitted work","E_COMPONENT_BUDGET")
  if(.bh_component_mul(D,plan$square)>if(.Machine$sizeof.pointer==8L)2^52 else .Machine$integer.max)
    .bh_component_fail("Output arrays exceed supported R indexing","E_COMPONENT_CAPABILITY")
  source<-if(is.null(acquired))list(status="not_acquired",reason="No declared component coordinates")else acquired$receipt
  C<-if(is.null(acquired))0.0 else length(.bh_component_get(source$sources,"chain_id"))
  .bh_component_reserve(ledger,"precision_projection_workspace",
    .bh_component_add(.bh_component_mul(C,1024),4096),
    .bh_component_add(.bh_component_mul(C,128),512),4)
  precision<-if(is.null(acquired))list(chain_id=numeric(),sig_figs=numeric(),status=character(),scope="No component-source acquisition")else
    .bh_component_precision(source$sources,plan$metadata$source_context$recorded_precision)
  source$context_validation<-context_validation
  source$bound_context<-plan$metadata$source_context
  source$non_csv_work<-if(plan$portable)"Immutable validation uses its stored separate allowance; component annotation work has its own receipt"else
    "Original prepared-spec serialization/seal validation is a separate unknown-cost operation; component annotation work has its own receipt"
  coordinate_ids<-.bh_component_get(plan$metadata$tables$coordinates,"id")
  generated<-list(source=source,precision=precision,axis_labels=list(
    matrix=list(NULL,coordinate_ids,coordinate_ids),sd=list(NULL,coordinate_ids)))
  .bh_component_admit(generated,ledger,"source_and_precision_annotation")
  .bh_pf_plain_tree(generated,c("cache","component_source"),limit=cap$context$max_depth)
  generated<-.bh_component_copy(generated,ledger,"source_and_precision_annotation")
  .bh_component_rows_admit(D,K,ledger,generated$axis_labels)
  # Six matrix arrays and two scale arrays. The acquired D*V projection stays
  # covered by the reader, and 4*D*W reserves assignment/extraction overlap.
  dims<-as.integer(c(D,K,K));scale_dims<-as.integer(c(D,K))
  correlation<-array(0.0,dims,generated$axis_labels$matrix)
  covariance<-array(NaN,dims,generated$axis_labels$matrix)
  log_abs_covariance<-array(-Inf,dims,generated$axis_labels$matrix)
  original_available<-array(FALSE,dims,generated$axis_labels$matrix)
  derived_available<-array(FALSE,dims,generated$axis_labels$matrix)
  available<-array(FALSE,dims,generated$axis_labels$matrix)
  sd<-array(0.0,scale_dims,generated$axis_labels$sd)
  log_sd<-array(0.0,scale_dims,generated$axis_labels$sd)
  status_fields<-c("range","exact_symmetry","diagonal","symmetry","constraints","positive_definite")
  status<-setNames(vector("list",9L),c(status_fields,"max_diagonal_residual","max_symmetry_residual","max_constraint_residual"))
  for(n in names(status))status[[n]]<-if(n%in%status_fields)character(D)else numeric(D)
  for(d in seq_len(D)) {
    for(i in seq_len(K)) {
      value<-acquired$draws[d,plan$square+i]
      if(!is.finite(value)||value<=0) .bh_component_fail("Saved component_row_sd must be finite and strictly positive","E_COMPONENT_SOURCE_SCHEMA")
      sd[d,i]<-value;log_sd[d,i]<-log(value)
    }
    R<-matrix(0.0,K,K)
    for(j in seq_len(K))for(i in seq_len(K)) {
      at<-i+(j-1L)*K;r<-acquired$draws[d,at]
      flag<-acquired$draws[d,plan$square+K+at]
      if(!is.finite(r)||!is.finite(flag)||!flag%in%c(0,1))
        .bh_component_fail("Saved component correlations/availability have an invalid finite/binary schema","E_COMPONENT_SOURCE_SCHEMA")
      correlation[d,i,j]<-r;R[i,j]<-r;original_available[d,i,j]<-flag==1
      if(r==0) {ell<- -Inf;value<-0.0;represented<-TRUE} else {
        ell<-log(abs(r))+log_sd[d,i]+log_sd[d,j]
        value<-sign(r)*exp(ell);represented<-is.finite(value)&&value!=0
      }
      log_abs_covariance[d,i,j]<-ell;derived_available[d,i,j]<-represented
      available[d,i,j]<-flag==1&&represented
      if(available[d,i,j])covariance[d,i,j]<-value
    }
    chain<-.bh_component_get(acquired$draw_keys,"chain_id")[[d]]
    p<-generated$precision$sig_figs[[match(chain,generated$precision$chain_id)]]
    row<-.bh_component_matrix(R,p,plan$rules,K)
    for(n in names(status))status[[n]][[d]]<-row[[n]]
  }
  attributes(status)<-list(names=names(status),class="data.frame",row.names=.set_row_names(D))
  keys<-if(is.null(acquired))structure(list(chain_id=integer(),saved_row=integer()),class="data.frame",row.names=integer())else acquired$draw_keys
  metadata<-plan$metadata
  structure(list(schema_version="bmeas_component_draws_v1",status=if(K)"extracted_all_retained_states"else"not_applicable_no_components",
    domain="declared_component_population",correlation=correlation,sd=sd,covariance=covariance,
    covariance_origin="derived_from_saved_R_and_SD",original_available=original_available,
    derived_available=derived_available,available=available,log_abs_covariance=log_abs_covariance,log_sd=log_sd,
    log_origin="derived_from_saved_R_and_SD_at_retained_precision",draw_keys=keys,
    coordinates=metadata$tables$coordinates,maps=metadata$tables[setdiff(names(metadata$tables),"coordinates")],
    prior=metadata$prior,upstream=metadata$upstream,matrix_status=status,precision=generated$precision,source=generated$source,
    work=list(source_width=plan$V,output_values_per_draw=plan$W,output_values=.bh_component_mul(D,plan$W),
      matrix_work_per_draw=plan$matrix_work,matrix_work=.bh_component_mul(D,plan$matrix_work),
      consumer_numeric_slots=.bh_component_add(.bh_component_mul(.bh_component_mul(4,D),plan$W),
        .bh_component_mul(12,plan$square),.bh_component_mul(8,K)),
      buffer_schedule="4*D*(6*K^2+2*K+5)+12*K^2+8*K, in addition to the unchanged reader formula",
      matrix_work_scope="Declared K^3+12*K^2+2*K units per state; status-only Cholesky, no inverse or regression; not exact CPU instructions"),
    context_receipt=.bh_component_context_receipt(ledger),
    interpretation="All aligned saved component states; no scientific diagnostic upgrade, complete measurement-posterior claim, matrix repair or automatic structural inference"),
    class="bmeas_component_draws")
}
.bh_component_execute <- function(fit,cap,context_validation=NULL,acquisition_paths=NULL,archive_hashes=NULL) {
  ledger<-.bh_component_ledger(cap$context)
  plan<-.bh_component_plan(fit,cap,ledger,context_validation)
  if(!plan$K)return(.bh_component_result(plan,NULL,ledger,context_validation))
  paths <- if(is.null(acquisition_paths)) plan$metadata$source_context$paths else acquisition_paths
  acquired<-.bh_posterior_ingest_csv(paths,plan$variables,cap$posterior,states="all",archive_hashes=archive_hashes,
    expected_model="scalar_components_model",expected_chains=plan$metadata$source_context$expected_chains$value,
    consumer_buffers=list(fixed=c(component_matrix_workspace=.bh_component_add(.bh_component_mul(12,plan$square),.bh_component_mul(8,plan$K))),
      per_selected_row=c(component_result_and_overlap=.bh_component_mul(4,plan$W))),
    consumer_work=list(per_selected_row=c(component_output_values=plan$W,component_matrix_work=plan$matrix_work),
      limits=c(component_output_values=cap$max_output_values,component_matrix_work=cap$max_matrix_work)))
  tryCatch(.bh_component_result(plan,acquired,ledger,context_validation),error=function(e) {
    if(typeof(e)=="list") {e$source_receipt<-acquired$receipt;e$component_context<-.bh_component_context_receipt(ledger)}
    stop(e)
  })
}

#' Extract aligned retained component correlation, scale and covariance draws
#' @export
component_draws <- function(fit,budget=postprocess_budget("component_draws")) {
  if(identical(class(fit),"bmeas_fit_archive")) return(.bh_archive_component(fit,.bh_component_budget(budget)))
  .bh_pf_public_input(fit,"component_draws")
  cap<-.bh_component_budget(budget)
  if(.bh_pf_is_portable(fit)) {
    validated<-.bh_pf_active(fit,"component_draws")
    return(.bh_pf_after_validation(validated,function()
      .bh_component_execute(validated$view,cap,validated$receipt)))
  }
  if(!identical(.bh_pf_class(fit),c("bmeas_fit","bhmm_fit")))
    .bh_component_fail("Expected a current main fit or the reviewed complete portable-fit profile","E_COMPONENT_UNSUPPORTED")
  .bh_component_execute(fit,cap)
}
