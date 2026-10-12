# Single-target admission; this module never repairs, remaps or drops a draw.
.bh_sddr_control <- function(control) {
    defaults <- list(bandwidth = NULL, max_draws = 10000, max_kernel_evaluations = 100000)
    if (typeof(control) != "list" || is.object(control) ||
        (length(control) && (is.null(names(control)) || anyDuplicated(names(control)) ||
            any(!names(control) %in% names(defaults)))))
        .bh_structural_fail("Unknown or unnamed SDDR diagnostic controls")
    defaults[names(control)] <- control
    for (name in c("max_draws", "max_kernel_evaluations")) {
        value <- defaults[[name]]
        if (!typeof(value) %in% c("integer", "double") || is.object(value) || length(value) != 1L ||
            !is.finite(value) || value < 1 || value != floor(value))
            .bh_structural_fail(paste(name, "must be a positive finite integer"))
    }
    h <- defaults$bandwidth
    if (!is.null(h) && (!typeof(h) %in% c("integer", "double") || is.object(h) ||
        !identical(names(h), c("prior", "posterior")) || length(h) != 2L ||
        any(!is.finite(h)) || any(h <= 0)))
        .bh_structural_fail("bandwidth must be NULL or positive finite c(prior=..., posterior=...)")
    defaults
}

.bh_sddr_arguments <- function(predictor, outcome, null, null_prior) {
    .bh_structural_scalar(predictor, "predictor")
    .bh_structural_scalar(outcome, "outcome")
    if (!typeof(null) %in% c("integer", "double") || is.object(null) || length(null) != 1L || !is.finite(null) || null != 0)
        .bh_structural_fail("Only one ordinary coefficient at the zero null is supported")
    if (!identical(null_prior, "encompassing_conditional"))
        .bh_structural_fail("Only the declared encompassing-conditional null is supported; this declaration does not verify compatibility")
    invisible(NULL)
}

.bh_sddr_target <- function(x, predictor, outcome, null, null_prior, control) {
    spec <- x$mapping_spec
    if (!predictor %in% spec$predictors || !outcome %in% spec$outcomes)
        .bh_structural_fail("Target must name one mapped predictor and outcome stable ID")
    if (!is.null(spec$path_fixed_values)) {
        i <- match(predictor, spec$predictors); j <- match(outcome, spec$outcomes)
        if (!is.na(spec$path_fixed_values[i, j])) return("blocked_fixed_coefficient")
    }
    if (is.null(x$prior)) return("blocked_missing_prior")
    for (role in c("prior", "posterior")) {
        mapped <- x[[role]]
        n <- nrow(mapped$draw_keys)
        if (n > control$max_draws)
            .bh_structural_fail("S4 draw budget exceeded", "E_STRUCTURAL_BUDGET")
        if (any(!mapped$draw_status$status %in% c("mapped", "mapped_near_singular")))
            return("blocked_incomplete_mapping")
        if (any(!is.finite(mapped$coefficients[, predictor, outcome])))
            return("blocked_nonfinite_coefficient")
    }
    n <- nrow(x$prior$draw_keys) + nrow(x$posterior$draw_keys)
    if (5 * n > control$max_kernel_evaluations)
        .bh_structural_fail("S4 kernel-evaluation budget exceeded", "E_STRUCTURAL_BUDGET")
    "eligible_for_empirical_density_diagnostics"
}
