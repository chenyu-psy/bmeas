# New producer admission; old capsules are neither reclassified nor resealed.
# This profile records a complete two-block design. Runtime work still needs
# current context validation, finite budgets, source validation and rng_v2.
.bh_pf_joint_export_profile <- function(spec) {
  if(.bh_pf_has_product(spec)||!.bh_m3cs_joint_spec(spec))return(.bh_pf_context_profile(spec))
  .bh_pf_joint_records(spec)
  "bmeas_joint_m3cs_context_v1"
}
.bh_pf_joint_profile_matches <- function(profile,spec) {
  if(identical(profile,"bmeas_joint_m3cs_context_v1")) {
    .bh_pf_joint_records(spec)
    return(TRUE)
  }
  # Legacy classification remains exactly the historical producer contract.
  identical(profile,.bh_pf_context_profile(spec))
}
.bh_pf_joint_records <- function(spec) {
  if(!.bh_m3cs_joint_spec(spec)||.bh_pf_has_product(spec))
    .bh_pf_fail("joint M3-CS profile","requires exactly two canonical pinned non-product blocks","E_PORTABLE_CAPABILITY")
  s<-unclass(spec);models<-s$models;dm<-s$data_map;t<-s$tables
  if(!identical(names(models),names(dm$blocks)))
    .bh_pf_fail("joint M3-CS profile","complete block names/order are required")
  roster<-t$person_roster;ops<-t$observation_operands
  if(!identical(roster$person_key,dm$subject_keys)||
     !identical(as.integer(roster$person_index),seq_len(nrow(roster))))
    .bh_pf_fail("joint M3-CS profile","full roster identity is inconsistent")
  if(is.null(ops)||!is.data.frame(ops)||any(!ops$block%in%names(models)))
    .bh_pf_fail("joint M3-CS profile","complete block-qualified operands are required")
  for(b in names(models)) {
    d<-dm$blocks[[b]];declared<-models[[b]]$data
    present<-rowSums(is.na(declared$counts))==0L
    if(!setequal(d$.observation_id,declared$.observation_id[present])||
       (!all(present)&&!identical(s$missing$response,"observed_only")))
      .bh_pf_fail("joint M3-CS profile","the complete observed response design must be retained")
    at<-match(d$.observation_id,declared$.observation_id)
    local<-declared$.local_id[at]
    if(identical(s$subjects$type,"shared"))person<-local else {
      cw<-s$subjects$data;cw<-cw[cw$model==b,,drop=FALSE]
      if(anyDuplicated(cw$local_id)) .bh_pf_fail("joint M3-CS profile","ambiguous saved crosswalk")
      person<-cw$person_id[match(local,cw$local_id)]
    }
    expected_people<-if(length(person))paste(s$subjects$namespace,person,sep="::")else character()
    if(anyNA(person)||!identical(d$.person_key,expected_people)||
       !identical(d$.local_id,local)||any(d$.block!=b))
      .bh_pf_fail("joint M3-CS profile","block/local-person/crosswalk identities disagree")
    if(anyNA(at)||anyDuplicated(d$.observation_id)||
       any(!d$.person_index%in%seq_len(nrow(roster)))||
       !identical(d$.person_key,roster$person_key[d$.person_index])||
       !identical(d$.task,declared$.task[at]))
      .bh_pf_fail("joint M3-CS profile","consumed rows, tasks and people must align with declared blocks")
    for(field in c("counts","options"))
      if(!identical(unname(d[[field]]),unname(declared[[field]][at,,drop=FALSE])))
        .bh_pf_fail("joint M3-CS profile","consumed response/options differ from declared rows")
    .bh_m3cs_counts(d$counts,d$options,nrow(d))
    bo<-ops[ops$block==b,,drop=FALSE]
    if(nrow(bo)!=4*nrow(d)||any(!bo$argument%in%c("a","c","ra","rc")))
      .bh_pf_fail("joint M3-CS profile","four complete operands per consumed row are required")
    for(a in c("a","c","ra","rc")) {
      ids<-bo$observation_id[bo$argument==a]
      if(length(ids)!=nrow(d)||anyDuplicated(ids)||!setequal(ids,d$.observation_id))
        .bh_pf_fail("joint M3-CS profile","operand block/observation identity is incomplete or ambiguous")
    }
  }
  invisible(TRUE)
}
.bh_pf_joint_operation <- function(root,operation) {
  if(operation%in%c("ppc_plan","assess_measurement_fit","ppc")&&
     .bh_m3cs_joint_spec(root$immutable$scientific_context)&&
     !identical(root$immutable$context_profile,"bmeas_joint_m3cs_context_v1"))
    .bh_pf_fail(operation,"legacy two-block capsules have no joint predictive producer admission; export a new capsule from the original intact live fit","E_PORTABLE_CAPABILITY")
  invisible(TRUE)
}
.bh_pf_joint_view <- function(fit) .bh_pf_is_view(fit)&&
  identical(.bh_pf_get(fit,"portable_context_profile"),"bmeas_joint_m3cs_context_v1")
.bh_m3cs_joint_reader_capability <- function() list(
  version="bmeas_saved_joint_m3cs_reader_v1",family="two_canonical_pinned_m3cs",
  levels=c("population","conditional"),simulator_version="bmeas_m3cs_rng_v2",
  execution="explicit_simulator_only_one_stacked_call_per_replicate",
  alignment="one_joint_posterior_row_and_one_full_roster_eta_per_replicate",
  producer_profile="bmeas_joint_m3cs_context_v1",historical_paths_and_seals="unchanged",
  qualification="R_only_candidate_native_saved_joint_context_not_qualified")
.bh_m3cs_saved_capability <- function(spec) if(.bh_m3cs_joint_spec(spec))
  .bh_m3cs_joint_reader_capability() else .bh_m3cs_reader_capability()
.bh_m3cs_saved_capability_check <- function(x) {
  if(identical(x,.bh_m3cs_joint_reader_capability()))return(invisible(TRUE))
  .bh_m3cs_reader_capability_check(x)
}
