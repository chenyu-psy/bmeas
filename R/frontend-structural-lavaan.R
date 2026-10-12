# Bounded deterministic covariance fitting, named extraction and retained draw slots.

.bh_lavaan_fit <- function(S, spec) {
    do.call(lavaan::lavaan, c(list(model = spec$model, sample_cov = S, sample_nobs = spec$reference_n), 
        spec$options))
}

.bh_lavaan_extract <- function(fit, S, spec, resolved) {
    P <- length(spec$predictors)
    Q <- length(spec$outcomes)
    aliases <- spec$aliases$alias
    xx <- head(aliases, P)
    yy <- tail(aliases, Q)
    op <- lavaan::lavInspect(fit, "options")
    for (n in names(spec$options)) {
        key <- if (n == "optim_init_nelder_mead") 
            "optim.init_nelder_mead"
        else gsub("_", ".", n, fixed = TRUE)
        if (!isTRUE(all.equal(op[[key]], spec$options[[n]], tolerance = 0, check.attributes = FALSE))) 
            stop(paste("Resolved lavaan option differs from fixed specification:", key))
    }
    if (!is.null(resolved) && !identical(resolved, op)) 
        stop("Resolved lavaan options changed between draws")
    pt <- lavaan::parTable(fit)
    if (!identical(.bh_lavaan_parameter_contract(pt), spec$parameter_contract)) 
        stop("lavaan parameter/extraction contract changed")
    B <- matrix(0, P, Q, dimnames = list(xx, yy))
    Psi <- matrix(0, Q, Q, dimnames = list(yy, yy))
    for (r in which(pt$op == "~")) B[pt$rhs[r], pt$lhs[r]] <- pt$est[r]
    for (r in which(pt$op == "~~" & pt$lhs %in% yy & pt$rhs %in% yy)) Psi[pt$lhs[r], pt$rhs[r]] <- Psi[pt$rhs[r], 
        pt$lhs[r]] <- pt$est[r]
    implied <- lavaan::lavInspect(fit, "sigma.hat")[aliases, aliases, drop = FALSE]
    actual <- lavaan::lavInspect(fit, "sampstat")$cov[aliases, aliases, drop = FALSE]
    if (!identical(unname(actual), unname(S))) 
        stop("lavaan changed the supplied named covariance")
    grad <- lavaan::lavInspect(fit, "gradient")
    list(resolved_options = op, B = B, Psi = Psi, implied = implied, converged = isTRUE(lavaan::lavInspect(fit, 
        "converged")), iterations = fit@optim$iterations, objective = fit@optim$fx, gradient = if (length(grad)) max(abs(grad)) else 0)
}

.bh_lavaan_map <- function(x, predictors, outcomes, units, scale, control, spec) {
    # Reuse analytic eligibility as a conservative prerequisite; never repair or drop draws.
    out <- .bh_structural_map(x, predictors, outcomes, units, scale, control)
    out$coefficients[] <- NA_real_
    out$residual_covariance[] <- NA_real_
    D <- nrow(out$draw_status)
    ids <- c(predictors, outcomes)
    ix <- match(ids, x$variables$id)
    aliases <- spec$aliases$alias
    st <- out$draw_status
    st$converged <- rep(NA, D)
    st$admissible <- rep(NA, D)
    st$boundary <- rep(NA, D)
    st$iterations <- rep(NA_integer_, D)
    st$objective <- rep(NA_real_, D)
    st$max_abs_covariance_residual <- rep(NA_real_, D)
    st$max_abs_gradient <- rep(NA_real_, D)
    st$global_optimum <- rep("not_verified", D)
    st$local_optima_check <- rep("not_performed", D)
    logs <- vector("list", D)
    resolved <- NULL
    fit_calls <- 0L
    for (d in seq_len(D)) {
        logs[[d]] <- list(warnings = character(), error = NULL, candidate = NULL)
        if (!st$status[d] %in% c("mapped", "mapped_near_singular")) 
            next
        base_status <- st$status[d]
        st$status[d] <- "lavaan_fit_failed"
        S <- matrix(x$covariance[d, , ], nrow(x$variables), nrow(x$variables))
        if (any(units != 1)) {
            converted <- .bh_structural_rescale(S * outer(sign(units), sign(units)), outer(log(abs(units)), 
                log(abs(units)), "+"))
            if (!converted$available) {
                st$status[d] <- "unrepresentable_unit_transform"
                next
            }
            S <- converted$value
        }
        S <- S[ix, ix, drop = FALSE]
        dimnames(S) <- list(aliases, aliases)
        warnings <- character()
        fit_calls <- fit_calls + 1L
        ans <- tryCatch(withCallingHandlers({
            fit <- .bh_lavaan_fit(S, spec)
            .bh_lavaan_extract(fit, S, spec, resolved)
        }, warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }), error = identity)
        logs[[d]]$warnings <- warnings
        if (inherits(ans, "error")) {
            logs[[d]]$error <- conditionMessage(ans)
            next
        }
        if (is.null(resolved)) 
            resolved <- ans$resolved_options
        st$converged[d] <- ans$converged
        st$iterations[d] <- ans$iterations
        st$objective[d] <- ans$objective
        st$max_abs_gradient[d] <- ans$gradient
        # Candidate values retain fitted input units; only accepted output is standardized.
        logs[[d]]$candidate <- list(coefficients = ans$B, residual_covariance = ans$Psi)
        evaluated <- .bh_lavaan_evaluate(ans, S, spec, scale)
        st$admissible[d] <- evaluated$admissible
        st$boundary[d] <- evaluated$boundary
        st$max_abs_covariance_residual[d] <- evaluated$covariance_residual
        if (!is.null(evaluated$error)) 
            logs[[d]]$error <- evaluated$error
        if (evaluated$status != "mapped") {
            st$status[d] <- evaluated$status
            next
        }
        ans$B <- evaluated$coefficients
        ans$Psi <- evaluated$residual_covariance
        out$coefficients[d, , ] <- ans$B
        out$residual_covariance[d, , ] <- ans$Psi
        st$status[d] <- base_status
    }
    out$draw_status <- st
    out$diagnostics$mapped_draws <- sum(st$status %in% c("mapped", "mapped_near_singular"))
    out$diagnostics$status_counts <- table(st$status)
    out$diagnostics$fit_calls <- fit_calls
    out$diagnostics$per_draw <- logs
    out$diagnostics$resolved_options <- resolved
    out$diagnostics$global_optimum <- "not_verified"
    out$diagnostics$local_optima_check <- "not_performed"
    out$diagnostics$interpretation <- "Covariance discrepancy mapping only; no second data likelihood, SE, test, p-value, or generative SEM comparison"
    out
}

