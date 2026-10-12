# Conservative fit acceptance, boundary detection and input-SD standardization.

.bh_lavaan_boundary <- function(Psi, sdy, tolerance) {
    # Guard normalization by input SDs against underflow in their outer product.
    normalized <- .bh_structural_rescale(Psi, outer(-log(sdy), -log(sdy), "+"))
    if (!normalized$available) 
        return(list(status = "unrepresentable_boundary_check", error = NULL))
    ev <- tryCatch(eigen(normalized$value, symmetric = TRUE, only.values = TRUE)$values, error = identity)
    if (inherits(ev, "error") || any(!is.finite(ev))) 
        return(list(status = "lavaan_boundary_check_failed", error = if (inherits(ev, "error")) conditionMessage(ev) else "Nonfinite residual eigenvalues"))
    list(status = "checked", boundary = min(ev) <= tolerance, error = NULL)
}

.bh_lavaan_evaluate <- function(candidate, covariance, spec, scale) {
    result <- list(status = "lavaan_not_converged", admissible = NA, boundary = NA, covariance_residual = NA_real_, 
        error = NULL)
    if (!candidate$converged) 
        return(result)
    finite_values <- c(candidate$B, candidate$Psi, candidate$implied, candidate$objective, 
        candidate$gradient)
    if (any(!is.finite(finite_values))) {
        result$status <- "lavaan_nonfinite_solution"
        return(result)
    }
    result$covariance_residual <- max(abs(candidate$implied - covariance))
    if (!is.finite(result$covariance_residual)) {
        result$status <- "lavaan_nonfinite_diagnostic"
        return(result)
    }
    positive_definite <- function(value) {
        !inherits(tryCatch(chol(value), error = identity), "error")
    }
    result$admissible <- positive_definite(candidate$Psi) && positive_definite(candidate$implied) && 
        all(diag(candidate$Psi) > 0)
    if (!result$admissible) {
        result$status <- "lavaan_inadmissible_solution"
        result$boundary <- TRUE
        return(result)
    }
    input_sd <- sqrt(diag(covariance))
    predictor_sd <- head(input_sd, length(spec$predictors))
    outcome_sd <- tail(input_sd, length(spec$outcomes))
    boundary <- .bh_lavaan_boundary(candidate$Psi, outcome_sd, spec$backend_control$boundary_tol)
    result$error <- boundary$error
    if (boundary$status != "checked") {
        result$status <- boundary$status
        return(result)
    }
    result$boundary <- boundary$boundary
    if (result$boundary) {
        result$status <- "lavaan_boundary_solution"
        return(result)
    }
    # Use input population SD_X / SD_Y, not model-implied lavaan std.all scales.
    if (scale == "standardized") {
        coefficients <- .bh_structural_rescale(candidate$B, outer(log(predictor_sd), -log(outcome_sd), 
            "+"))
        residuals <- .bh_structural_rescale(candidate$Psi, outer(-log(outcome_sd), -log(outcome_sd), 
            "+"))
        if (!coefficients$available || !residuals$available) {
            result$status <- "unrepresentable_mapping"
            return(result)
        }
        candidate$B <- coefficients$value
        candidate$Psi <- residuals$value
    }
    result$status <- "mapped"
    result$coefficients <- candidate$B
    result$residual_covariance <- candidate$Psi
    result
}

