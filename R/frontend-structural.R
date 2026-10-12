# Analytic covariance mapping, guarded unit conversion, backend dispatch and summaries.
# No draw is removed; no jitter, nearest-PD repair or stochastic retries.
# Bound combined input and output numeric values/work before allocations.

.bh_structural_rescale <- function(value, log_multiplier) {
    out <- value
    nonzero <- value != 0
    out[nonzero] <- sign(value[nonzero]) * exp(log(abs(value[nonzero])) + log_multiplier[nonzero])
    list(value = out, available = all(is.finite(out)) && !any(nonzero & out == 0))
}

.bh_structural_map <- function(x, predictors, outcomes, units, scale, control) {
    K <- nrow(x$variables)
    D <- dim(x$covariance)[1L]
    P <- length(predictors)
    Q <- length(outcomes)
    ix <- match(predictors, x$variables$id)
    iy <- match(outcomes, x$variables$id)
    B <- array(NA_real_, c(D, P, Q), list(NULL, predictors, outcomes))
    residual <- array(NA_real_, c(D, Q, Q), list(NULL, outcomes, outcomes))
    status <- cbind(x$draw_keys, status = rep("not_mapped", D), rcond = rep(NA_real_, D), covariance_rcond = rep(NA_real_, 
        D), stringsAsFactors = FALSE)
    for (d in seq_len(D)) {
        if (!all(x$direction_resolved[c(ix, iy)])) {
            status$status[d] <- "unresolved_direction"
            next
        }
        S <- matrix(x$covariance[d, , ], K, K)
        if (!all(x$available[d, , ])) {
            status$status[d] <- "unavailable_covariance"
            next
        }
        if (any(!is.finite(S))) {
            status$status[d] <- "nonfinite_covariance"
            next
        }
        if (any(S != t(S))) {
            status$status[d] <- "nonexact_symmetric_covariance"
            next
        }
        if (any(units != 1)) {
            converted <- .bh_structural_rescale(S * outer(sign(units), sign(units)), outer(log(abs(units)), 
                log(abs(units)), "+"))
            if (!converted$available) {
                status$status[d] <- "unrepresentable_unit_transform"
                next
            }
            S <- converted$value
        }
        if (any(diag(S) <= 0)) {
            status$status[d] <- "non_positive_definite"
            next
        }
        sd <- sqrt(diag(S))
        R <- diag(1, K)
        represented <- TRUE
        # Divide by the smaller SD first: an SPD cross-covariance divided by
        # either SD is bounded by the other, avoiding intermediate overflow.
        if (K > 1L) 
            for (j in 2:K) for (i in seq_len(j - 1L)) {
                value <- (S[i, j]/min(sd[i], sd[j]))/max(sd[i], sd[j])
                if (!is.finite(value) || (S[i, j] != 0 && value == 0)) 
                  represented <- FALSE
                R[i, j] <- R[j, i] <- value
            }
        if (!represented) {
            status$status[d] <- "unrepresentable_correlation"
            next
        }
        if (inherits(tryCatch(chol(R), error = identity), "error")) {
            status$status[d] <- "non_positive_definite"
            next
        }
            # B = solve(Sigma_XX, Sigma_XY), evaluated on correlation scale.
        XX <- R[ix, ix, drop = FALSE]
        XY <- R[ix, iy, drop = FALSE]
        YY <- R[iy, iy, drop = FALSE]
        ans <- tryCatch({
            U <- chol(XX)
            whitened <- forwardsolve(t(U), XY)
            beta <- backsolve(U, whitened)
            # Schur complement: Psi = Sigma_YY - Sigma_YX B.
            psi <- YY - crossprod(whitened)
            rc <- rcond(S[ix, ix, drop = FALSE])
            full_rc <- rcond(S)
            list(beta = beta, psi = psi, rcond = rc, covariance_rcond = full_rc)
        }, error = identity)
        if (inherits(ans, "error")) {
            status$status[d] <- "numerical_solve_failed"
            next
        }
        if (any(!is.finite(ans$beta)) || any(!is.finite(ans$psi)) || !is.finite(ans$rcond) || 
            !is.finite(ans$covariance_rcond)) {
            status$status[d] <- "nonfinite_mapping"
            next
        }
        if (any(diag(ans$psi) <= 0) || inherits(tryCatch(chol(ans$psi), error = identity), 
            "error")) {
            status$status[d] <- "non_positive_residual_covariance"
            next
        }
        if (scale == "unstandardized") {
            beta_units <- .bh_structural_rescale(ans$beta, outer(-log(sd[ix]), log(sd[iy]), 
                "+"))
            residual_units <- .bh_structural_rescale(ans$psi, outer(log(sd[iy]), log(sd[iy]), 
                "+"))
            if (!beta_units$available || !residual_units$available) {
                status$status[d] <- "unrepresentable_mapping"
                next
            }
            ans$beta <- beta_units$value
            ans$psi <- residual_units$value
        }
        B[d, , ] <- ans$beta
        residual[d, , ] <- ans$psi
        status$rcond[d] <- ans$rcond
        status$covariance_rcond[d] <- ans$covariance_rcond
        status$status[d] <- if (min(ans$rcond, ans$covariance_rcond) < control$near_singular_rcond) 
            "mapped_near_singular"
        else "mapped"
    }
    # Repeated values are valid MCMC states, not evidence of independent draws.
    duplicate_rows <- duplicated(matrix(x$covariance, D, K * K))
    list(coefficients = B, residual_covariance = residual, draw_keys = x$draw_keys, draw_status = status, 
        distribution = x$distribution, diagnostics = list(total_draws = D, mapped_draws = sum(status$status %in% 
            c("mapped", "mapped_near_singular")), repeated_covariance_rows = sum(duplicate_rows), 
            independence = "not_assessed; draw count is not ESS", upstream_inference = "not_upgraded", 
            status_counts = table(status$status)))
}

#' Map joint population covariance draws to structural functionals
#' @export
structural_draws <- function(posterior_draws, predictors, outcomes, prior_draws = NULL, coefficient_scale = c("unstandardized", 
    "standardized"), units = NULL, control = list(), backend = c("analytic", "lavaan"), model = NULL, 
    backend_control = list()) {
    backend <- match.arg(backend)
    if (backend == "analytic" && (!is.null(model) || length(backend_control))) 
        .bh_structural_fail("model and backend_control are only available for backend=\"lavaan\"")
    cap <- .bh_structural_control(control)
    coefficient_scale <- match.arg(coefficient_scale)
    .bh_structural_validate(posterior_draws, cap)
    if (posterior_draws$distribution != "posterior") 
        .bh_structural_fail("posterior_draws must have posterior role")
    .bh_structural_text(predictors, "predictors", TRUE)
    .bh_structural_text(outcomes, "outcomes", TRUE)
    vars <- posterior_draws$variables$id
    if (!length(predictors) || !length(outcomes) || anyDuplicated(c(predictors, outcomes)) || 
        any(!c(predictors, outcomes) %in% vars)) 
        .bh_structural_fail("Use nonempty disjoint predictor/outcome stable IDs from the population contract")
    if (is.null(units)) 
        units <- setNames(rep(1, length(vars)), vars)
    if (!is.numeric(units) || is.object(units) || !identical(names(units), vars) || any(!is.finite(units)) || 
        any(units == 0)) 
        .bh_structural_fail("units must be finite nonzero fixed multipliers named in covariance coordinate order")
    if (!is.null(prior_draws)) {
        .bh_structural_validate(prior_draws, cap)
        if (prior_draws$distribution != "prior") 
            .bh_structural_fail("prior_draws must have prior role")
        for (n in c("model_id", "prior_id", "variables", "parameterization", "direction_resolved")) if (!identical(prior_draws[[n]], 
            posterior_draws[[n]])) 
            .bh_structural_fail(paste("Prior/posterior contracts differ in", n), "E_STRUCTURAL_IDENTITY")
    }
    D <- dim(posterior_draws$covariance)[1L] + if (is.null(prior_draws)) 
        0
    else dim(prior_draws$covariance)[1L]
    K <- length(vars)
    P <- length(predictors)
    Q <- length(outcomes)
    if (D * (K * K + P * Q + Q * Q) > cap$max_values || D * K^3 > cap$max_matrix_work) 
        .bh_structural_fail("Combined structural mapping exceeds numeric/work limits", "E_STRUCTURAL_BUDGET")
    if (backend == "lavaan") {
        bc <- .bh_lavaan_control(backend_control)
        if (D > bc$max_fits) 
            .bh_structural_fail("Combined lavaan draws exceed max_fits", "E_STRUCTURAL_BUDGET")
        spec <- .bh_lavaan_spec(predictors, outcomes, model, bc, posterior_draws$variables, 
            units, coefficient_scale)
        spec$control <- cap
        post <- .bh_lavaan_map(posterior_draws, predictors, outcomes, units, coefficient_scale, 
            cap, spec)
        prior <- if (is.null(prior_draws)) 
            NULL
        else .bh_lavaan_map(prior_draws, predictors, outcomes, units, coefficient_scale, cap, 
            spec)
        po <- post$diagnostics$resolved_options
        pr <- if (is.null(prior)) 
            NULL
        else prior$diagnostics$resolved_options
        if (!is.null(po) && !is.null(pr) && !identical(po, pr)) 
            .bh_structural_fail("Prior/posterior resolved lavaan options differ", "E_STRUCTURAL_IDENTITY")
        spec$resolved_options <- if (!is.null(po)) 
            po
        else pr
        return(.bh_structural_bind(structure(list(prior = prior, posterior = post, mapping_spec = spec, estimand_type = spec$estimand_type, 
            provenance = list(posterior = posterior_draws, prior = prior_draws), diagnostics = list(bayes_factor = "not_implemented", 
                prior_rng = "provided_by_population_contract_if_available", interpretation = "Deterministic covariance discrepancy functional only; no measurement refit or generative SEM comparison")), 
            class = "bmeas_structural_draws")))
    }
    post <- .bh_structural_map(posterior_draws, predictors, outcomes, units, coefficient_scale, 
        cap)
    prior <- if (is.null(prior_draws)) 
        NULL
    else .bh_structural_map(prior_draws, predictors, outcomes, units, coefficient_scale, cap)
    .bh_structural_bind(structure(list(prior = prior, posterior = post, mapping_spec = list(backend = "analytic_cholesky_regression_v1", 
        predictors = predictors, outcomes = outcomes, numerical_parameterization = "correlation_scale_solve_with_guarded_unit_conversion", 
        variables = posterior_draws$variables, units = units, coefficient_scale = coefficient_scale, 
        residual_scale = if (coefficient_scale == "standardized") "divided_by_per_draw_outcome_population_SDs" else "transformed_outcome_units", 
        control = cap, formula = "B = solve(Sigma_XX, Sigma_XY); Psi = Sigma_YY - Sigma_YX B"), 
        estimand_type = "regression_functional", provenance = list(posterior = posterior_draws, 
            prior = prior_draws), diagnostics = list(bayes_factor = "not_implemented", prior_rng = if (!is.null(prior_draws) && 
            identical(prior_draws$provenance$source_kind, "bmeas_component_prior")) "native_full_LKJ_unit_SD" else "not_invoked", 
            interpretation = "Deterministic population covariance functional; no measurement refit, identification upgrade, likelihood or generative SEM comparison")), 
        class = "bmeas_structural_draws"))
}

#' @export
summary.bmeas_structural_draws <- function(object, ...) {
    summarize <- function(x) {
        if (is.null(x)) 
            return(NULL)
        if (any(!x$draw_status$status %in% c("mapped", "mapped_near_singular"))) 
            return(list(status = "unavailable_incomplete_mapping", draw_status = x$draw_status))
        a <- x$coefficients
        p <- dim(a)[2L]
        q <- dim(a)[3L]
        rows <- vector("list", p * q)
        at <- 0L
        for (j in seq_len(q)) for (i in seq_len(p)) {
            at <- at + 1L
            z <- a[, i, j]
            rows[[at]] <- data.frame(predictor = dimnames(a)[[2L]][i], outcome = dimnames(a)[[3L]][j], 
                mean = mean(z), sd = if (length(z) > 1L) 
                  stats::sd(z)
                else NA_real_, q025 = unname(stats::quantile(z, 0.025)), median = stats::median(z), 
                q975 = unname(stats::quantile(z, 0.975)), stringsAsFactors = FALSE)
        }
        list(status = "descriptive_only", coefficients = do.call(rbind, rows), diagnostics = x$diagnostics)
    }
    list(prior = summarize(object$prior), posterior = summarize(object$posterior), mapping_spec = object$mapping_spec, 
        estimand_type = object$estimand_type, bayes_factor = "not_implemented")
}

