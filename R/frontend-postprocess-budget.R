# Finite, operation-specific starting allowances for modest retained fits.
# Fresh ordinary lists; no global options, auto-growth, or resource discovery.
postprocess_budget <- function(operation) {
  allowed <- c("archive_fit", "read_fit_archive", "component_draws")
  if (!is.character(operation) || is.object(operation) || length(operation) != 1L ||
      is.na(operation) || !operation %in% allowed)
    .mp_abort("E_POSTPROCESS_BUDGET", paste("operation must be exactly one of", paste(allowed, collapse=", ")))
  posterior <- list(
    reader = "streaming",
    max_source_bytes = 256 * 1024^2,
    max_read_bytes = 7 * 256 * 1024^2,
    max_materialized_values = 10000000,
    max_record_bytes = 1048576,
    max_columns = 100000,
    max_source_rows = 100000,
    max_chains = 16,
    max_temp_bytes = 2 * 256 * 1024^2,
    max_scan_values = 100000000
  )
  context <- list(max_context_bytes = 16 * 1024^2,
    max_context_nodes = 2000000, max_depth = 64)
  if (operation == "component_draws") return(list(
    posterior = posterior, max_components = 12,
    max_output_values = 10000000, max_matrix_work = 100000000,
    context = context))
  list(max_members = 64, max_member_bytes = 256 * 1024^2,
    max_total_bytes = 512 * 1024^2, max_context_bytes = 16 * 1024^2,
    max_read_bytes = 8 * 1024^3, max_temp_bytes = 2 * 1024^3,
    posterior = posterior, context = context)
}
