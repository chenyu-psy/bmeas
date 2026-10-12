# Descriptive support and repetition are deliberately separate from inference.
.bh_sddr_repetition <- function(mapped, population, values) {
    keys <- mapped$draw_keys
    chains <- split(seq_along(values), keys$chain_id)
    covariance <- matrix(population$covariance, nrow = length(values))
    cloned <- character()
    if (length(chains) > 32L)
        .bh_structural_fail("S4 repetition diagnostics support at most 32 chains", "E_STRUCTURAL_BUDGET")
    if (length(chains) > 1L) for (i in seq_len(length(chains) - 1L)) {
        for (j in seq.int(i + 1L, length(chains))) {
            a <- chains[[i]]; b <- chains[[j]]
            if (length(a) == length(b) && identical(unname(covariance[a, , drop = FALSE]),
                unname(covariance[b, , drop = FALSE])))
                cloned <- c(cloned, paste(names(chains)[c(i, j)], collapse = ":"))
        }
    }
    list(repeated_coefficients = sum(duplicated(values)),
         repeated_population_states = sum(duplicated(covariance)),
         identical_chain_sequences = cloned,
         interpretation = "Repeated states retained; may be legitimate MCMC rejections. Counts and distinct values are not ESS.")
}

.bh_sddr_support <- function(values, bandwidth) {
    h <- bandwidth * 0.5
    left <- sum(values < 0 & values >= -h)
    right <- sum(values > 0 & values <= h)
    reasons <- character()
    if (length(unique(values)) < 2L) reasons <- c(reasons, "constant_coefficient")
    if (min(values) >= 0 || max(values) <= 0) reasons <- c(reasons, "zero_not_inside_observed_range")
    if (left < 25L || right < 25L) reasons <- c(reasons, "sparse_observed_local_support")
    list(status = if (length(reasons)) "insufficient_observed_support" else "descriptive_counts_available",
         reason_codes = reasons, minimum = min(values), maximum = max(values),
         minimum_absolute_distance = min(abs(values)), exact_zeros = sum(values == 0),
         narrowest_bandwidth = h, left_count = left, right_count = right,
         count_within_base_bandwidth = sum(abs(values) <= bandwidth),
         count_within_twice_base_bandwidth = sum(abs(values) <= 2 * bandwidth),
         support_regular_interior = "not_assessed", boundary_distance = "not_assessed",
         interpretation = "Counts are not effective visits; Gaussian kernel positivity cannot establish true support or regularity.")
}

.bh_sddr_sampling <- function(values, keys, repetition, source_kind) {
    result <- list(status = "not_assessed", independence = "not_assessed",
                   mcse = list(status = "not_assessed", value = NA_real_,
                       reason = "Estimator-specific serial-dependence MCSE has not been validated."))
    if (source_kind == "synthetic") {
        result$reason <- "Synthetic fixtures are not posterior sampling evidence."
        return(result)
    }
    if (source_kind == "bmeas_component_prior") {
        result$status <- "native_iid_generation_provenance_retained"
        result$independence <- "native_generator_scope_only"
        return(result)
    }
    chains <- split(seq_along(values), keys$chain_id)
    ordered <- all(vapply(chains, function(i) all(diff(keys$saved_row[i]) > 0), logical(1)))
    lengths <- lengths(chains)
    spacing <- unique(unlist(lapply(chains, function(i) diff(keys$saved_row[i])), use.names = FALSE))
    regular <- length(spacing) == 1L && spacing > 0
    if (!regular || !ordered || length(chains) < 2L || length(unique(lengths)) != 1L || min(lengths) < 20L ||
        length(repetition$identical_chain_sequences)) {
        result$reason <- "Need regularly spaced, ordered equal-length nonidentical sequences with at least two chains and twenty retained rows each; no truncation or padding."
        return(result)
    }
    if (!requireNamespace("posterior", quietly = TRUE)) {
        result$reason <- "Optional posterior package unavailable."
        return(result)
    }
    chain_matrix <- vapply(chains, function(i) values[i], numeric(lengths[1L]))
    metrics <- tryCatch(c(rhat = posterior::rhat(chain_matrix),
                         ess_bulk = posterior::ess_bulk(chain_matrix),
                         ess_tail = posterior::ess_tail(chain_matrix)), error = function(e) NULL)
    if (is.null(metrics) || any(!is.finite(metrics))) {
        result$reason <- "Coefficient diagnostics unavailable or nonfinite."
        return(result)
    }
    result$status <- "coefficient_diagnostics_only"
    result$metrics <- metrics
    result$posterior_version <- as.character(utils::packageVersion("posterior"))
    result$reason <- "Ordered-sequence diagnostics do not authenticate independent chains, the sampled law, kernel-density ESS, or posterior validity."
    result
}
