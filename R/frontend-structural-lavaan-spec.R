# Fixed optional-backend model grammar, aliases, parameter contract and numerical options.

.bh_lavaan_control <- function(x) {
    d <- list(max_fits = 100L, reference_n = 1000L, iter_max = 1000L, eval_max = 2000L, rel_tol = 1e-10, 
        boundary_tol = 1e-08)
    if (!is.list(x) || is.object(x) || (length(x) && (is.null(names(x)) || anyDuplicated(names(x)) || 
        any(!names(x) %in% names(d))))) 
        .bh_structural_fail("Unknown or unnamed lavaan backend control")
    d[names(x)] <- x
    for (n in names(d)) if (!is.numeric(d[[n]]) || is.object(d[[n]]) || length(d[[n]]) != 1L || 
        !is.finite(d[[n]]) || d[[n]] <= 0) 
        .bh_structural_fail(paste(n, "must be a positive finite number"))
    for (n in c("max_fits", "reference_n", "iter_max", "eval_max")) if (d[[n]] != floor(d[[n]]) || 
        d[[n]] > 2^31 - 1) 
        .bh_structural_fail(paste(n, "must be an integer within R's integer range"))
    if (d$reference_n < 2 || d$rel_tol >= 1 || d$boundary_tol >= 1) 
        .bh_structural_fail("Invalid reference N or numerical tolerance")
    d
}

.bh_lavaan_version <- function(v) {
    if (!identical(v, "0.7.2")) 
        .bh_structural_fail(paste("Only lavaan 0.7.2 is currently validated; installed:", v), 
            "E_STRUCTURAL_DEPENDENCY")
    invisible(v)
}

.bh_lavaan_dependency <- function() {
    if (!requireNamespace("lavaan", quietly = TRUE)) 
        .bh_structural_fail("The optional lavaan backend requires lavaan 0.7.2", "E_STRUCTURAL_DEPENDENCY")
    v <- as.character(utils::packageVersion("lavaan"))
    .bh_lavaan_version(v)
    v
}

.bh_lavaan_parse_model <- function(model, xx, yy) {
    P <- length(xx)
    Q <- length(yy)
    paths <- matrix(if (is.null(model)) 
        NA_real_
    else 0, P, Q, dimnames = list(xx, yy))
    psi <- matrix(0, Q, Q, dimnames = list(yy, yy))
    diag(psi) <- NA_real_
    if (is.null(model)) 
        psi[] <- NA_real_
    else {
        .bh_structural_scalar(model, "lavaan model")
        lines <- trimws(unlist(strsplit(model, "[;\n]")))
        lines <- lines[nzchar(lines)]
        if (!length(lines)) 
            .bh_structural_fail("Empty lavaan model")
        seen <- character()
        npaths <- 0L
        for (line in lines) {
            m <- regexec("^([xy][1-9][0-9]*)[[:space:]]*(~~|~)[[:space:]]*(.+)$", line)
            z <- regmatches(line, m)[[1L]]
            if (length(z) != 4L) 
                .bh_structural_fail("Only alias-based Y ~ X and Y ~~ Y terms with numeric fixed modifiers are supported")
            lhs <- z[2L]
            op <- z[3L]
            rhs <- gsub("[[:space:]]", "", z[4L])
            rhs <- gsub("([eE])\\+", "\\1", rhs)
            if (!lhs %in% yy) 
                .bh_structural_fail("Only outcome aliases can be on the left; predictor covariance is unrestricted")
            terms <- strsplit(rhs, "+", fixed = TRUE)[[1L]]
            if (!length(terms) || any(!nzchar(terms)) || endsWith(rhs, "+")) 
                .bh_structural_fail("Invalid model term")
            for (term in terms) {
                if (endsWith(term, "*")) 
                  .bh_structural_fail("Invalid trailing modifier separator")
                bits <- strsplit(term, "*", fixed = TRUE)[[1L]]
                if (length(bits) == 1L) {
                  name <- bits
                  value <- NA_real_
                }
                else if (length(bits) == 2L && grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$", 
                  bits[1L])) {
                  name <- bits[2L]
                  value <- as.numeric(bits[1L])
                  if (!is.finite(value)) 
                    .bh_structural_fail("Fixed model values must be finite")
                }
                else .bh_structural_fail("Labels, equality constraints, starts and nonnumeric modifiers are unsupported")
                if (op == "~") {
                  if (!name %in% xx) 
                    .bh_structural_fail("Only predictor aliases may predict outcomes; recursive/latent/mean models are unsupported")
                  key <- paste(lhs, op, name)
                  paths[name, lhs] <- value
                  npaths <- npaths + 1L
                }
                else {
                  if (!name %in% yy) 
                    .bh_structural_fail("Only outcome residual covariance restrictions are supported")
                  key <- paste(sort(c(lhs, name)), collapse = "~~")
                  psi[lhs, name] <- psi[name, lhs] <- value
                }
                if (key %in% seen) 
                  .bh_structural_fail("Duplicate or reversed duplicate model term")
                seen <- c(seen, key)
            }
        }
        if (!npaths) 
            .bh_structural_fail("The model must explicitly contain at least one Y ~ X path")
    }
    list(paths = paths, residuals = psi)
}

.bh_lavaan_spec <- function(predictors, outcomes, model, control, variables, units, scale) {
    P <- length(predictors)
    Q <- length(outcomes)
    xx <- paste0("x", seq_len(P))
    yy <- paste0("y", seq_len(Q))
    aliases <- data.frame(alias = c(xx, yy), id = c(predictors, outcomes), stringsAsFactors = FALSE)
    parsed_model <- .bh_lavaan_parse_model(model, xx, yy)
    paths <- parsed_model$paths
    psi <- parsed_model$residuals
    term <- function(value, name) if (is.na(value)) 
        name
    else paste0(sprintf("%.17g", value), "*", name)
    syntax <- character()
    for (j in seq_len(Q)) {
        syntax <- c(syntax, paste(yy[j], "~", paste(vapply(seq_len(P), function(i) term(paths[i, 
            j], xx[i]), ""), collapse = " + ")))
    }
    for (i in seq_len(P)) {
        for (j in i:P) {
            syntax <- c(syntax, paste(xx[i], "~~", xx[j]))
        }
    }
    for (i in seq_len(Q)) {
        for (j in i:Q) {
            syntax <- c(syntax, paste(yy[i], "~~", term(psi[i, j], yy[j])))
        }
    }
    options <- list(estimator = "ML", likelihood = "normal", sample_cov_rescale = FALSE, meanstructure = FALSE, 
        fixed_x = FALSE, conditional_x = FALSE, se = "none", test = "none", baseline = FALSE, 
        h1 = FALSE, optim_method = "nlminb", optim_attempts = 1L, rstarts = 0L, start = "simple", 
        bounds = "none", optim_parscale = "none", optim_force_converged = FALSE, optim_gradient = "analytic", 
        optim_dx_tol = 0.001, optim_init_nelder_mead = FALSE, optim_var_transform = "none", 
        auto_var = FALSE, auto_cov_y = FALSE, auto_cov_lv_x = FALSE, auto_fix_first = FALSE, 
        auto_fix_single = FALSE, check_gradient = TRUE, check_post = TRUE, control = list(iter.max = control$iter_max, 
            eval.max = control$eval_max, rel.tol = control$rel_tol))
    version <- .bh_lavaan_dependency()
    syntax <- paste(syntax, collapse = "\n")
    parsed <- lavaan::lavaanify(syntax, auto = FALSE, fixed_x = FALSE, meanstructure = FALSE)
    list(backend = "lavaan_covariance_path_v1", lavaan_version = version, parsed_parameter_table = parsed, 
        parameter_contract = .bh_lavaan_parameter_contract(parsed), version_defaults = lavaan::lavOptions(), 
        model_input = model, model = syntax, aliases = aliases, predictors = predictors, outcomes = outcomes, 
        variables = variables, units = units, coefficient_scale = scale, residual_scale = if (scale == 
            "standardized") "divided_by_per_draw_outcome_population_SDs" else "transformed_outcome_units", 
        standardization = "input_population_SDs_not_lavaan_std_all", path_fixed_values = paths, 
        residual_fixed_values = psi, estimand_type = if (is.null(model)) "regression_functional" else "projected_path", 
        objective = "single_group_normal_ML_covariance_discrepancy_only", options = options, 
        reference_n = control$reference_n, backend_control = control, retry_policy = "none; no warm starts; no random starts", 
        global_optimum = "not_verified", local_optima_check = "not_performed", identification = "not_certified_by_optimizer_convergence", 
        runtime = list(R = R.version.string, platform = R.version$platform, BLAS = unname(extSoftVersion()["BLAS"])))
}

.bh_lavaan_parameter_contract <- function(pt) {
    z <- data.frame(lhs = pt$lhs, op = pt$op, rhs = pt$rhs, free = pt$free > 0, fixed = ifelse(pt$free > 
        0, NA_real_, pt$ustart), stringsAsFactors = FALSE)
    z <- z[order(z$lhs, z$op, z$rhs), , drop = FALSE]
    rownames(z) <- NULL
    z
}

