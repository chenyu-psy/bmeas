# Public S4a orchestration. Empirical density ratios are never promoted to BF.
#' Diagnose a single structural coefficient's zero-point density ratio
#' @export
structural_sddr <- function(x, predictor, outcome, null = 0,
                            null_prior = "encompassing_conditional", control = list()) {
    cap <- .bh_sddr_control(control)
    .bh_sddr_arguments(predictor, outcome, null, null_prior)
    if (typeof(x) != "list" || !identical(attr(x, "class", exact = TRUE), "bmeas_structural_draws"))
        .bh_structural_fail("Use a newly created structural_draws() result")
    binding <- .bh_structural_binding_check(x)
    result <- list(schema_version = "bmeas_structural_sddr_v1", status = binding,
        target = list(predictor = predictor, outcome = outcome, null = null),
        null_prior = null_prior, computation_status = "blocked", density_status = "not_assessed",
        bf_status = "not_assessed", log_bf01 = NA_real_, bf01 = NA_real_, bf10 = NA_real_,
        experimental_log_density_ratio = NA_real_, density = NULL, diagnostics = NULL,
        estimator = list(name = "gaussian_point_kde_v1", evaluation = "direct_log_sum_exp",
            reference_bandwidth = "raw_count_reference_rule_no_degeneracy_fallback",
            bandwidth_multipliers = c(0.5, 0.75, 1, 1.5, 2)),
        formal_bf_reason = "S4a does not qualify a Bayes factor. Prior legitimacy, posterior sampling/propriety, regularity and encompassing-conditional null compatibility require independent assessment.")
    class(result) <- "bmeas_structural_sddr"
    if (binding != "bound_scientific_snapshot") return(result)
    target_status <- .bh_sddr_target(x, predictor, outcome, null, null_prior, cap)
    result$evidence_scope <- x$evidence_binding$scope
    result$mapping_spec <- x$mapping_spec
    result$estimand_type <- x$estimand_type
    result$hypothesis_definition <- list(parameter = "selected mapped coefficient",
        restriction = "coefficient equals zero", coefficient_scale = x$mapping_spec$coefficient_scale,
        null_nuisance_prior = "Encompassing model conditional prior at this parameterization-specific restriction; declared, not verified.",
        standardization = if (identical(x$mapping_spec$coefficient_scale, "standardized"))
            "Draw-dependent SD scaling can change the conditional null prior despite the same zero set." else
            "Fixed units retained; no draw-dependent standardization requested.")
    result$provenance <- x$provenance
    result$draw_status <- list(prior = x$prior$draw_status, posterior = x$posterior$draw_status)
    result$source_status <- list(mapping = "creation_bound_scientific_projection",
        prior_law = "source_evidence_retained_not_upgraded", posterior_law = "not_assessed",
        posterior_propriety = "upstream_evidence_retained_not_upgraded",
        null_prior_compatibility = "declared_not_verified", support_regularity = "not_assessed")
    result$status <- target_status
    if (target_status != "eligible_for_empirical_density_diagnostics") return(result)
    factors <- c(0.5, 0.75, 1, 1.5, 2)
    densities <- list(); diagnostics <- list()
    for (role in c("prior", "posterior")) {
        values <- x[[role]]$coefficients[, predictor, outcome]
        repeated <- .bh_sddr_repetition(x[[role]], x$provenance[[role]], values)
        supplied <- if (is.null(cap$bandwidth)) NULL else cap$bandwidth[[role]]
        bandwidth <- .bh_sddr_bandwidth(values, supplied,
            repeated$repeated_coefficients > 0 || repeated$repeated_population_states > 0)
        diagnostics[[role]] <- list(draws = length(values), repetition = repeated, bandwidth = bandwidth,
            sampling = .bh_sddr_sampling(values, x[[role]]$draw_keys, repeated,
                x$provenance[[role]]$provenance$source_kind))
        if (!is.finite(bandwidth$value)) {
            densities[[role]] <- list(status = bandwidth$status)
            next
        }
        diagnostics[[role]]$support <- .bh_sddr_support(values, bandwidth$value)
        if (length(unique(values)) < 2L) {
            densities[[role]] <- list(status = "blocked_constant_coefficient")
            next
        }
        densities[[role]] <- .bh_sddr_density(values, bandwidth$value, factors)
    }
    result$density <- densities
    result$diagnostics <- diagnostics
    if (!all(vapply(densities, function(d) identical(d$status, "computed"), logical(1)))) {
        result$status <- "density_unavailable"
        return(result)
    }
    prior <- densities$prior$estimates$log_density_at_zero
    posterior <- densities$posterior$estimates$log_density_at_zero
    grid <- expand.grid(prior_multiplier = factors, posterior_multiplier = factors)
    grid$log_density_ratio <- as.vector(outer(prior, posterior, function(a, b) b - a))
    base <- posterior[3L] - prior[3L]
    result$bandwidth_sensitivity <- list(grid = grid, baseline_log_ratio = base,
        maximum_absolute_shift = max(abs(grid$log_density_ratio - base)),
        interpretation = "All 25 combinations; sensitivity is not an error bound or a validity certificate.")
    result$experimental_log_density_ratio <- base
    result$computation_status <- "computed"
    adverse <- any(vapply(diagnostics, function(d)
        d$support$status != "descriptive_counts_available", logical(1)))
    sensitive <- result$bandwidth_sensitivity$maximum_absolute_shift > 0.25
    result$reason_codes <- c(if (adverse) "sparse_or_one_sided_observed_support",
        if (sensitive) "reference_bandwidth_sensitivity_above_0.25_log_units")
    result$bandwidth_sensitivity$screening_threshold <- 0.25
    result$density_status <- if (adverse) "insufficient_observed_support" else if (sensitive)
        "bandwidth_sensitive" else "unqualified_empirical_estimate"
    result$status <- "experimental_density_ratio_only"
    result$control <- cap
    result
}
