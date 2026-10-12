# Active archive consumers reopen authoritative bytes on every call. Only the
# mutable report cache may be carried from the caller's in-memory snapshot.
.bh_archive_active <- function(handle,operation) {
  fields <- c("root","manifest_md5","budget","report_snapshot")
  handle <- .bh_pf_record(handle,"archive handle",fields,fields[1:3],"bmeas_fit_archive")
  .bh_pf_string(handle$root,"archive root");.bh_pf_md5(handle$manifest_md5,"archive manifest identity")
  .bh_archive_budget(handle$budget)
  snapshot <- if(is.null(handle$report_snapshot))NULL else
    .bh_archive_report_snapshot(structure(handle,class="bmeas_fit_archive"))
  reopened <- .bh_archive_read(handle$root,handle$budget)
  if(!identical(handle$manifest_md5,reopened$handle$manifest_md5))
    .bh_archive_fail("Archive manifest changed since opening")
  portable <- reopened$portable
  if(!is.null(snapshot)) {
    if(!identical(snapshot$identity,portable$identity)) .bh_archive_fail("Archive snapshot names another capsule")
    .bh_pf_cache_root(snapshot$cache)
    portable$cache <- snapshot$cache
  }
  validated <- .bh_pf_active(portable,operation)
  if(operation=="assess_measurement_fit") {
    models <- .bh_pf_get(.bh_pf_get(validated$view,"spec"),"models")
    if(!.bh_m3cs_predictive_spec(.bh_pf_get(validated$view,"spec"))&&
       any(!vapply(models,function(block).bh_pf_get(.bh_pf_get(block,"model"),"name") %in% c("gaussian","binomial","lognormal"),FALSE)))
      .mp_abort("E_CHECK_UNSUPPORTED","Archive PPC supports canonical basic families or one or exactly two canonical pinned M3-CS blocks")
  }
  posterior <- reopened$manifest$members[reopened$manifest$members$role=="posterior",,drop=FALSE]
  list(handle=reopened$handle,portable=portable,validated=validated,
    paths=vapply(posterior$member,function(x).bh_archive_path(reopened$handle$root,x),""),
    hashes=posterior$digest)
}
.bh_archive_commit <- function(active,updated) {
  portable <- .bh_pf_commit_cache(active$portable,updated)
  .bh_archive_handle(active$handle$root,active$handle$manifest_md5,active$handle$budget,portable)
}
.bh_archive_streaming <- function(posterior) {
  if(is.null(posterior)||!identical(posterior$reader,"streaming"))
    .bh_archive_fail("Archive consumers require an explicit streaming posterior budget; older readers cannot bind relocated bytes")
  invisible(TRUE)
}
.bh_archive_refresh <- function(handle,parameters,budget) {
  cap <- .bh_diagnostic_refresh_budget(budget);.bh_archive_streaming(cap$posterior)
  active <- .bh_archive_active(handle,"refresh_diagnostics");v <- active$validated
  .bh_pf_after_validation(v,function() {
    plan <- .bh_diagnostic_refresh_plan(v$view,parameters,cap)
    .bh_archive_commit(active,.bh_diagnostic_refresh_compute(v$view,plan,v$receipt,active$paths,active$hashes))
  })
}
.bh_archive_individual <- function(handle,scales,budget,states) {
  budget <- .bh_individual_budget(budget);.bh_archive_streaming(budget$posterior)
  active <- .bh_archive_active(handle,"individual_parameter_summary");v <- active$validated
  .bh_pf_after_validation(v,function()
    .bh_individual_parameter_summary(v$view,scales,budget,states,v$receipt,active$paths,active$hashes))
}
.bh_archive_ppc_plan <- function(handle,draws,budget,level) {
  if(is.null(level)) .mp_abort("E_CHECK_UNSUPPORTED","archive predictive planning requires an explicit level")
  cap <- .bh_ppc_budget_resolve(budget);.bh_archive_streaming(cap$values$posterior)
  active <- .bh_archive_active(handle,"assess_measurement_fit");v <- active$validated
  .bh_pf_after_validation(v,function() {
    out <- .bh_ppc_plan(v$view,draws,budget,level)
    attrs <- attributes(out);out <- unclass(out)
    out$source$archive_context <- list(status="reopened_and_verified",manifest_md5=active$handle$manifest_md5,
      source_scope="relocated_archive_members; original paths and fit-time evidence unchanged")
    attributes(out) <- attrs
    out
  })
}
.bh_archive_assess <- function(handle,checks,level,draws,seed,budget,simulator) {
  if(!identical(checks,"ppc"))
    .mp_abort("E_CHECK_UNSUPPORTED","archive assessment requires checks='ppc'")
  cap <- .bh_ppc_budget_resolve(budget);.bh_archive_streaming(cap$values$posterior)
  active <- .bh_archive_active(handle,"assess_measurement_fit");v <- active$validated
  .bh_pf_after_validation(v,function()
    .bh_archive_commit(active,.bh_ppc_assess(v$view,checks,level,draws,seed,budget,simulator,v$receipt,active$paths,active$hashes)))
}
