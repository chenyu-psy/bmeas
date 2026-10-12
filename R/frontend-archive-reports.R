# Passive archive reports use only the capsule captured at explicit open/create.
# No disk verification, refresh, acquisition, or immutable identity resealing.
.bh_archive_handle <- function(root,manifest_md5,budget,portable) {
  structure(list(root=root,manifest_md5=manifest_md5,budget=budget,
    report_snapshot=list(format="bmeas_archive_report_snapshot_v1",
      manifest_md5=manifest_md5,portable_identity=portable$identity,portable=portable)),
    class="bmeas_fit_archive")
}
.bh_archive_report_snapshot <- function(handle) {
  fields <- c("root","manifest_md5","budget","report_snapshot")
  h <- .bh_pf_record(handle,"archive handle",fields,fields[1:3],"bmeas_fit_archive")
  if(is.null(h$report_snapshot)) .bh_archive_fail(
    "Archive handle has no retained report snapshot; explicitly reopen with read_fit_archive(path) before reporting")
  if(!identical(names(h),fields)) .bh_archive_fail("Invalid archive handle field order")
  .bh_pf_string(h$root,"archive root")
  .bh_pf_md5(h$manifest_md5,"archive manifest identity")
  .bh_archive_budget(h$budget)
  s <- .bh_pf_ordered(h$report_snapshot,"archive report snapshot",
    c("format","manifest_md5","portable_identity","portable"))
  if(!identical(s$format,"bmeas_archive_report_snapshot_v1") ||
     !identical(s$manifest_md5,h$manifest_md5)) .bh_archive_fail("Invalid archive report snapshot binding")
  root <- .bh_pf_envelope(s$portable)
  .bh_pf_identity(s$portable_identity)
  if(!identical(root$identity,s$portable_identity)) .bh_archive_fail("Archive report snapshot capsule identity mismatch")
  s$portable
}
.bh_archive_report_context <- function(handle) list(
  status="retained_snapshot_not_reverified",manifest_md5=.bh_pf_get(handle,"manifest_md5"),
  source_scope="in_memory_snapshot_only_no_archive_or_original_file_access",
  freshness="Snapshot retained at explicit archive creation or reopen; no automatic refresh")
.bh_archive_report <- function(handle,operation,...) {
  portable <- .bh_archive_report_snapshot(handle)
  result <- switch(operation,summary=summary.bmeas_portable_fit(portable,...),
    fit_checks=.bh_pf_fit_checks(portable),
    parameter_summary=posterior_summary(portable),posterior_summary=posterior_summary(portable))
  # Preserve genuinely absent parameter caches as NULL, rather than fabricate a table.
  if(is.null(result))return(NULL)
  if(operation %in% c("summary","fit_checks")) {
    result$archive_context <- .bh_archive_report_context(handle)
    class(result) <- c(if(operation=="summary")"summary.bmeas_fit_archive" else "bmeas_archive_fit_checks",class(result))
  } else attr(result,"archive_context") <- .bh_archive_report_context(handle)
  result
}
#' @export
summary.bmeas_fit_archive <- function(object,...) .bh_archive_report(object,"summary",...)
#' @export
print.bmeas_fit_archive <- function(x,...) {
  print.summary.bmeas_fit_archive(summary.bmeas_fit_archive(x),...)
  invisible(x)
}
#' @export
print.summary.bmeas_fit_archive <- function(x,...) {
  cat("Archive report | retained snapshot; archive not reverified by this report\n")
  print.summary.bmeas_portable_fit(x,...)
  invisible(x)
}
#' @export
print.bmeas_archive_fit_checks <- function(x,...) {
  cat("Archive report | retained snapshot; archive not reverified by this report\n")
  print.bmeas_portable_fit_checks(x,...)
  invisible(x)
}
