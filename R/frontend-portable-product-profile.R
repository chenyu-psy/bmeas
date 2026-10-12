# New saved-value profile. The v1 envelope/codec and legacy capability record
# remain unchanged; a new context and conversion profile identify this domain.
.bh_pf_has_product <- function(spec) {
  t <- .bh_pf_get(spec,"tables")
  d <- if(typeof(t)=="list") .bh_pf_get(t,"correlation_domains") else NULL
  if(typeof(d)!="list") return(FALSE)
  any(vapply(c("component","parameter"),function(layer) {
    x <- .bh_pf_get(d,layer)
    typeof(x)=="list" && "product_status" %in% .bh_pf_names(x)
  },logical(1)))
}
.bh_pf_context_profile <- function(spec) if(.bh_pf_has_product(spec))
  "bmeas_iid_gaussian_product_context_v1" else "bmeas_postprocess_context_v1"
.bh_pf_profile_runtime <- function(profile) if(identical(profile,"bmeas_joint_m3cs_context_v1"))
  "portable_joint_m3cs_postprocessing_v1" else if(identical(profile,"bmeas_iid_gaussian_product_context_v1"))
  "portable_iid_gaussian_product_postprocessing_v1" else "portable_postprocessing_v1"
.bh_pf_conversion_profile <- function(profile) if(identical(profile,"bmeas_joint_m3cs_context_v1"))
  "bmeas_joint_m3cs_export_v1" else if(identical(profile,"bmeas_iid_gaussian_product_context_v1"))
  "bmeas_iid_gaussian_product_export_v1" else "bmeas_portable_fit_v1"
.bh_pf_profile_capabilities <- function(profile) {
  x <- .bh_pf_capabilities()
  if(identical(profile,"bmeas_iid_gaussian_product_context_v1")) {
    x$profile <- "retained_iid_gaussian_product_postprocessing_v1"
    x$metadata_interfaces <- "diagnostic_parameters"
    x$active_interfaces <- c("individual_parameter_summary","component_draws")
    x$basic_ppc <- "not_admitted_product_profile_requires_separate_predictive_qualification"
    x$unavailable <- c(x$unavailable,"refresh_diagnostics","ppc_plan","assess_measurement_fit","ppc")
    x$dispatch_status <- "experimental_product_saved_value_consumers_only_no_inference_or_recovery_qualification"
  }
  if(identical(profile,"bmeas_joint_m3cs_context_v1")) {
    x$profile <- "retained_joint_m3cs_postprocessing_v1"
    x$basic_ppc <- "exactly_two_canonical_pinned_M3CS_blocks_explicit_rng_v2_only"
    x$dispatch_status <- "joint_m3cs_R_only_saved_controller_no_native_or_inference_qualification"
  }
  x
}
.bh_pf_product_profile <- function(spec,cache=NULL,provenance=NULL) {
  if(!.bh_pf_has_product(spec)) return(invisible(TRUE))
  spec <- unclass(spec)
  t <- spec$tables
  if(!identical(t$person_law$distribution,"iid") ||
     !identical(t$augmentation$type,"common_plus_parameter_residual") ||
     identical(t$correlation_coordinate_system$type,"ordered_blocks"))
    .bh_pf_fail("product profile","only iid ordinary common/residual coordinates are admitted","E_PORTABLE_CAPABILITY")
  for(b in spec$models) {
    m <- unclass(.bh_pf_get(b,"model"))
    if(!is.null(m$adapter)||!identical(m$likelihood$symbol,"normal_lpdf")||!isTRUE(m$likelihood$builtin))
      .bh_pf_fail("product profile","only canonical Gaussian descriptors are admitted","E_PORTABLE_CAPABILITY")
    .bh_pf_builtin_descriptor(m,"product profile.model")
    cap <- unclass(m$capabilities)
    flag <- cap$available[cap$capability=="fit"]
    if(length(flag)!=1L||!isTRUE(flag)) .bh_pf_fail("product profile","retained model lacks fit capability","E_PORTABLE_CAPABILITY")
  }
  .bh_pf_product_tables(t,"spec.tables")
  checks <- spec$checks
  if(!identical(checks$backend$status,"experimental_iid_gaussian_affine_product"))
    .bh_pf_fail("product profile","retained backend evidence is outside the experimental product profile","E_PORTABLE_CAPABILITY")
  for(owner in list(checks,cache)) if(!is.null(owner)) {
    inference <- .bh_pf_get(owner,"inference")
    if(!is.null(inference)&&!identical(.bh_pf_get(inference,"status"),"not_assessed"))
      .bh_pf_fail("product profile","product inference must remain explicitly not assessed")
  }
  if(!is.null(provenance)&&!identical(provenance$execution_profile$requested,"experimental"))
    .bh_pf_fail("product profile","saved execution must retain its explicit experimental request")
  if(!is.null(cache)) {
    if(!identical(.bh_pf_get(.bh_pf_get(cache,"inference"),"status"),"not_assessed"))
      .bh_pf_fail("product profile","the saved inference status must be present and not assessed")
    c <- .bh_pf_get(cache,"correlation_constraints")
    if(!is.null(c)) {
      expected <- if(identical(t$correlation_domains$parameter$product_status,"available"))
        "iid_gaussian_residual_dual_product_live_cache_v1" else "iid_gaussian_common_product_live_cache_v1"
      if(!identical(c$version,expected)) .bh_pf_fail("product cache","wrong retained product producer version")
      .bh_pf_product_cache_map(c$primitive_summary_map,t)
      aliases <- c(primitives="correlation_primitives",cells="correlation_affine_cells",domains="correlation_domains",prior_geometry="correlation_prior_geometry")
      for(field in names(aliases)) {
        if(!identical(c[[field]],t[[aliases[[field]]]])) .bh_pf_fail("product cache","scientific cache differs from sealed tables","E_PORTABLE_BINDING")
      }
    }
  }
  invisible(TRUE)
}
.bh_pf_product_operation <- function(root,operation) {
  profile <- root$immutable$context_profile
  product <- .bh_pf_has_product(root$immutable$scientific_context)
  if((identical(profile,"bmeas_iid_gaussian_product_context_v1")||product)&&
     operation%in%c("refresh_diagnostics","ppc_plan","assess_measurement_fit","ppc")) {
    if(identical(operation,"refresh_diagnostics")||!product||
       !identical(profile,"bmeas_iid_gaussian_product_context_v1"))
      .bh_pf_fail(operation,"this operation requires the exact complete product reader profile; refresh remains unavailable","E_PORTABLE_CAPABILITY")
    # Admission here is only a profile route. Before predictive metadata is
    # consumed, the caller counts and validates the complete immutable schema;
    # active assessment additionally verifies its digest and adverse evidence.
  }
  invisible(TRUE)
}

# Current reader execution is deliberately separate from the immutable .33
# producer capability record, historical notices and export-time evidence.
.bh_pf_product_reader_capability <- function() list(
  version="bmeas_saved_iid_gaussian_product_reader_v1",family="canonical_gaussian_only",
  levels=c("population","conditional"),person_law="iid_common_plus_parameter_residual",
  execution="package_owned_canonical_R_Gaussian_kernels",
  alignment="one_joint_posterior_row_and_one_full_roster_eta_per_replicate",
  population="independent_common_and_residual_normals_saved_factors_applied_once",
  conditional="retained_used_eta_and_observation_scales",
  producer_profile="bmeas_iid_gaussian_product_context_v1",
  historical_paths_seals_capabilities_and_no_PPC_notices="unchanged",
  qualification="experimental_R_only_reader_inference_not_assessed_no_generic_or_native_qualification")
.bh_pf_product_view <- function(fit) .bh_pf_is_view(fit)&&
  identical(.bh_pf_get(fit,"portable_context_profile"),"bmeas_iid_gaussian_product_context_v1")
.bh_pf_reader_capability_check <- function(x,plan,path) {
  plan<-unclass(plan) # caller validates the fixed plan schema before this dispatch
  if(identical(x,.bh_pf_product_reader_capability())) {
    if(!identical(plan$version,"bmeas_basic_ppc_plan_v3")||
       !plan$level%in%c("population","conditional")||
       !identical(plan$law,if(plan$level=="population")"iid_common_plus_parameter_residual"else"conditional_response"))
      .bh_pf_fail(path,"product reader capability disagrees with the Gaussian plan family, level or iid law","E_PORTABLE_CAPABILITY")
  } else {
    .bh_m3cs_saved_capability_check(x)
    if(!plan$version%in%c("bmeas_m3cs_ppc_plan_v1","bmeas_m3cs_ppc_plan_v2","bmeas_m3cs_ppc_plan_v3")||
       !plan$level%in%c("population","conditional")||
       (plan$level=="conditional"&&!identical(plan$law,"conditional_response"))||
       (plan$level=="population"&&!plan$law%in%c("iid_common_plus_parameter_residual","same_N_sample_centered_task_cholesky")))
      .bh_pf_fail(path,"M3-CS reader capability disagrees with the plan family, level or law","E_PORTABLE_CAPABILITY")
  }
  invisible(TRUE)
}

# Bind the product producer's primitive/alias display map to sealed values.
# No live lowering plan or chart is invoked, including for the other MGS layer.
.bh_pf_product_cache_map <- function(map,tables) {
  p <- "product cache.primitive_summary_map"
  fields <- c("variable","parameter_key","parameter_role","layer","computational_variable","computational_mapping","alias_cells")
  z <- .bh_pf_table(map,p,fields)
  # History-table shape alone does not admit S3 column methods. Validate
  # every consumed text vector before subsetting, even before cache tree walk.
  for(field in fields[fields!="alias_cells"])
    .bh_pf_character(z[[field]],paste0(p,".",field),missing=field=="computational_variable")
  primitives <- unclass(tables$correlation_primitives)
  cells <- unclass(tables$correlation_affine_cells)
  if(!identical(z$parameter_key,primitives$parameter)||!identical(z$layer,primitives$layer)||
     !identical(z$parameter_role,rep("unique_marginal_correlation",length(primitives$parameter))))
    .bh_pf_fail(p,"primitive display map does not cover the sealed labels in order")
  aliases <- unclass(z$alias_cells)
  if(typeof(aliases)!="list"||length(aliases)!=length(primitives$parameter))
    .bh_pf_fail(p,"primitive alias records do not cover every label")
  for(layer in c("component","parameter")) {
    edge <- unclass(tables[[if(layer=="component")"correlations"else"parameter_correlations"]])
    affine <- any(edge$state=="fixed")||base::anyDuplicated.default(edge$parameter[edge$state=="free"])>0L
    domain <- tables$correlation_domains[[layer]]
    indices <- which(primitives$layer==layer)
    mapping <- if(!affine)"MGS_vector_map_no_scalar_association" else if(identical(domain$product_status,"available"))
      "unique_label_product_interval_coordinate" else "one_label_interval_coordinate"
    for(j in seq_along(indices)) {
      i <- indices[j];at <- which(cells$layer==layer & !is.na(cells$parameter) & cells$parameter==primitives$parameter[i])
      if(!length(at)) .bh_pf_fail(p,"primitive display map has no matching sealed cells")
      variable <- paste0("R_",layer,"[",cells$row[at[1]],",",cells$col[at[1]],"]")
      raw <- if(!affine)NA_character_ else paste0(if(layer=="component")"correlation_raw"else"parameter_correlation_raw",
        "[",if(identical(domain$product_status,"available"))j else 1L,"]")
      if(!identical(z$variable[i],variable)||!identical(z$computational_variable[i],raw)||!identical(z$computational_mapping[i],mapping))
        .bh_pf_fail(p,"primitive scientific/computational association differs from the saved coordinate representation")
      a <- .bh_pf_table(aliases[[i]],paste0(p,".alias_cells"),c("row","col","endpoint1","endpoint2"))
      for(field in names(a))if(!identical(a[[field]],cells[[field]][at]))
        .bh_pf_fail(p,"primitive alias membership differs from the complete sealed label cells")
    }
  }
  invisible(TRUE)
}
