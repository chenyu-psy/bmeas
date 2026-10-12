# Optional, deterministic structural functionals. No sampler, likelihood or BF.
# Scope: explicit input/output numeric values and cubic matrix work, not RSS.

.bh_structural_fail <- function(message, code = "E_STRUCTURAL_CONTRACT") .mp_abort(code, message)

.bh_structural_text <- function(x, label, unique = FALSE) {
    if (!is.character(x) || is.object(x) || anyNA(x) || any(!nzchar(trimws(x))) || (unique && 
        anyDuplicated(x))) 
        .bh_structural_fail(paste(label, "requires nonempty plain strings", if (unique) 
            "without duplicates"
        else ""))
    invisible(x)
}

.bh_structural_scalar <- function(x, label) {
    .bh_structural_text(x, label)
    if (length(x) != 1L) 
        .bh_structural_fail(paste(label, "must be one string"))
    invisible(x)
}

.bh_structural_control <- function(control) {
    defaults <- list(max_draws = 10000, max_variables = 128, max_values = 1e+07, max_matrix_work = 1e+08, 
        near_singular_rcond = 1e-10)
    if (!is.list(control) || is.object(control) || (length(control) && (is.null(names(control)) || 
        anyDuplicated(names(control)) || any(!names(control) %in% names(defaults))))) 
        .bh_structural_fail("Unknown or unnamed structural control")
    defaults[names(control)] <- control
    for (n in names(defaults)) {
        x <- defaults[[n]]
        if (!is.numeric(x) || is.object(x) || length(x) != 1L || !is.finite(x) || x <= 0) 
            .bh_structural_fail(paste(n, "must be one positive finite number"))
    }
    if (defaults$near_singular_rcond >= 1) 
        .bh_structural_fail("near_singular_rcond must be below one")
    defaults
}

.bh_structural_dimensions <- function(a, control) {
    dims <- dim(a)
    if (!is.numeric(a) || is.object(a) || length(dims) != 3L || dims[1L] < 1L || dims[2L] < 
        1L || dims[2L] != dims[3L]) 
        .bh_structural_fail("covariance must be a nonempty numeric draw-by-variable-by-variable array")
    D <- dims[1L]
    K <- dims[2L]
    if (D > control$max_draws || K > control$max_variables || D * K * K > control$max_values || 
        D * K^3 > control$max_matrix_work) 
        .bh_structural_fail("Population draws exceed structural control limits", "E_STRUCTURAL_BUDGET")
    dims
}

.bh_structural_validate <- function(x, control) {
    if (!identical(class(x), "bmeas_population_draws") || !identical(x$schema_version, "bmeas_population_draws_v1")) 
        .bh_structural_fail("Use population_draws() or component_population_draws()")
    a <- x$covariance
    dims <- .bh_structural_dimensions(a, control)
    D <- dims[1L]
    K <- dims[2L]
    v <- x$variables
    if (!identical(class(v), "data.frame") || !identical(names(v), c("id", "scale", "orientation")) || 
        nrow(v) != K) 
        .bh_structural_fail("variables must contain exactly id, scale and orientation, one row per coordinate")
    .bh_structural_text(v$id, "variable IDs", TRUE)
    .bh_structural_text(v$scale, "variable scales")
    .bh_structural_text(v$orientation, "variable orientations")
    if (!identical(dimnames(a)[[2L]], v$id) || !identical(dimnames(a)[[3L]], v$id)) 
        .bh_structural_fail("Covariance axes must match variable IDs in exactly the declared order")
    keys <- x$draw_keys
    if (!identical(class(keys), "data.frame") || !identical(names(keys), c("chain_id", "saved_row")) || 
        nrow(keys) != D) 
        .bh_structural_fail("draw_keys must contain chain_id and saved_row for every covariance draw")
    for (n in names(keys)) {
        z <- keys[[n]]
        if (!is.numeric(z) || is.object(z) || anyNA(z) || any(!is.finite(z)) || any(z < 1 | 
            z != floor(z) | z > 2^53 - 1)) 
            .bh_structural_fail("Draw keys must be positive exactly representable integer-valued numbers")
    }
    if (anyDuplicated(keys)) 
        .bh_structural_fail("Duplicate chain_id/saved_row identities are not allowed")
    if (!is.character(x$distribution) || length(x$distribution) != 1L || !x$distribution %in% 
        c("prior", "posterior")) 
        .bh_structural_fail("distribution must explicitly be prior or posterior")
    .bh_structural_scalar(x$model_id, "model_id")
    .bh_structural_scalar(x$prior_id, "prior_id")
    p <- x$provenance
    if (!is.list(p) || is.object(p) || !identical(sort(names(p)), sort(c("source_kind", "source", 
        "joint_scope", "verification")))) 
        .bh_structural_fail("Provenance requires source_kind, source, joint_scope and verification")
    for (n in names(p)) .bh_structural_scalar(p[[n]], paste("provenance", n))
    if (!p$source_kind %in% c("external", "synthetic", "bmeas_component_posterior", "bmeas_component_prior") || 
        !identical(p$joint_scope, "joint_population_covariance_parameters")) 
        .bh_structural_fail("Only explicitly declared joint population covariance parameters are supported")
    if (p$source_kind == "bmeas_component_posterior" && x$distribution != "posterior") 
        .bh_structural_fail("A component posterior cannot be relabeled as prior")
    if (p$source_kind %in% c("external", "synthetic") && p$verification != "user_declared_not_verified") 
        .bh_structural_fail("External and synthetic provenance is declared, not source-verified")
    if (!is.logical(x$available) || !identical(dim(x$available), dims) || anyNA(x$available)) 
        .bh_structural_fail("available must be a complete logical array matching covariance")
    if (!is.logical(x$direction_resolved) || length(x$direction_resolved) != K || anyNA(x$direction_resolved)) 
        .bh_structural_fail("Each variable requires an explicit direction status")
    if (!is.list(x$parameterization) || !length(x$parameterization)) 
        .bh_structural_fail("Missing fixed parameterization contract")
    if (p$source_kind == "bmeas_component_posterior") {
        expected <- .bh_structural_component(x$upstream, control, validate = FALSE)
        for (n in c("covariance", "draw_keys", "variables", "distribution", "model_id", "prior_id", 
            "provenance", "available", "direction_resolved", "parameterization")) if (!identical(x[[n]], 
            expected[[n]])) 
            .bh_structural_fail(paste("Native population contract differs from retained extraction:", 
                n), "E_STRUCTURAL_IDENTITY")
    }
    else if (p$source_kind == "bmeas_component_prior") {
        expected <- .bh_component_prior_restore(x$upstream, control)
        for (n in names(expected)) if (!identical(x[[n]], expected[[n]])) 
            .bh_structural_fail(paste("Native prior contract differs from retained generation:", 
                n), "E_STRUCTURAL_IDENTITY")
    }
    else if (!is.null(x$upstream) || !identical(x$parameterization, list(variables = x$variables))) {
        .bh_structural_fail("External parameterization must match its declared variables; native evidence cannot be relabeled")
    }
    invisible(x)
}

#' Declare external or synthetic joint population covariance draws
#' @export
population_draws <- function(covariance, draw_keys, variables, distribution, model_id, prior_id, 
    source_kind, source, joint_scope, control = list()) {
    if (missing(distribution) || missing(source_kind) || missing(joint_scope)) 
        .bh_structural_fail("Declare distribution, source_kind and joint_scope explicitly")
    if (!is.character(source_kind) || length(source_kind) != 1L || is.na(source_kind) || !source_kind %in% 
        c("external", "synthetic")) 
        .bh_structural_fail("source_kind must be external or synthetic")
    cap <- .bh_structural_control(control)
    dims <- .bh_structural_dimensions(covariance, cap)
    if (!identical(class(variables), "data.frame") || nrow(variables) != dims[2L]) 
        .bh_structural_fail("variables must be a data frame with one row per coordinate")
    x <- structure(list(schema_version = "bmeas_population_draws_v1", covariance = covariance, 
        draw_keys = draw_keys, variables = variables, distribution = distribution, model_id = model_id, 
        prior_id = prior_id, provenance = list(source_kind = source_kind, source = source, 
            joint_scope = joint_scope, verification = "user_declared_not_verified"), available = array(TRUE, 
            dim(covariance)), direction_resolved = rep(TRUE, nrow(variables)), parameterization = list(variables = variables), 
        upstream = NULL), class = "bmeas_population_draws")
    .bh_structural_validate(x, cap)
    x
}

