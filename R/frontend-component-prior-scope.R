# Narrow component-prior module. No likelihood, density estimation or BF.

# A capability refusal is distinct from malformed structural draw evidence.
.bh_component_prior_fail <- function(message) .mp_abort("E_COMPONENT_PRIOR_SCOPE", message)

# Scientific admission is deliberately narrower than all mathematical LKJ models.
# Only complete free unique final-R entries with unit SD use unrestricted LKJ.
# Proper independent nuisance factors integrate to one; they are not resampled.
# Ordinary independent residuals are excluded for auditability, not statistical necessity.
.bh_component_prior_gate <- function(spec, validate_seal = TRUE, control = .bh_structural_control(list()), 
    draws = 1) {
    if (!identical(class(spec), c("bmeas_spec", "bhmm_spec"))) 
        .bh_component_prior_fail("A currently prepared frontend specification is required; saved portable consumers are not supported")
    if (validate_seal) 
        .bh_diagnostic_refresh_validate_spec(spec)
    t <- spec$tables
    v <- t$coordinates
    K <- nrow(v)
    sc <- t$component_scales
    cor <- t$correlations
    .bh_component_prior_budget(draws, K, control)
    if (K < 1L || !all(v$kind == "scalar") || !identical(spec$formula$person, "iid") || !identical(t$augmentation$type, 
        "common_plus_parameter_residual") || .bh_raw_cor(spec) || .bh_has_shares(spec) || 
        nrow(.bh_amplitudes(spec)) || nrow(t$loading_constraint_rules) || nrow(t$parameter_residuals) || 
        !identical(t$correlation_coordinate_system$type, "final_marginal_mask") || nrow(t$correlation_constraint_rules)) 
        .bh_component_prior_fail("Initial implementation requires ordinary iid scalar components without loading constraints, basis, shares, marker, raw-B, affine or parameter-residual extensions; residual exclusion is an implementation limitation")
    .bh_component_maps(t, K)
    if (nrow(sc) != K || any(sc$state != "fixed") || anyNA(sc$fixed_sd) || any(sc$fixed_sd != 
        1) || any(sc$primitive_index != 0) || any(!is.na(sc$parameter))) 
        .bh_component_prior_fail("Every component must have deterministic unit population SD and no SD primitive")
    E <- K * (K - 1)/2
    expected <- which(upper.tri(matrix(FALSE, K, K)), arr.ind = TRUE)
    got <- paste(cor$row, cor$col, sep = ":")
    wanted <- paste(expected[, 1], expected[, 2], sep = ":")
    if (nrow(cor) != E || anyNA(cor$state) || any(cor$state != "free") || anyNA(cor$parameter) || 
        anyDuplicated(cor$parameter) || anyDuplicated(got) || !setequal(got, wanted)) 
        .bh_component_prior_fail("Full declared K requires every unordered correlation pair free exactly once with distinct primitives")
    ori <- t$orientation
    if (!identical(ori$coordinate_id, v$id)) 
        .bh_component_prior_fail("Orientation axes disagree")
    resolved <- ori$pinned | (!is.na(ori$anchor) & nzchar(ori$anchor))
    if (!is.null(t$orientation_sign_system)) {
        basis <- t$orientation_sign_system$unanchored_binary_basis
        if (!is.matrix(basis) || nrow(basis) != K || anyNA(basis) || any(!basis %in% c(0, 
            1))) 
            .bh_component_prior_fail("Invalid sign-system certificate")
        resolved <- rowSums(basis) == 0
    }
    if (anyNA(resolved) || !all(resolved)) 
        .bh_component_prior_fail("Every component direction must be resolved")
    p <- t$priors
    expected_parameters <- c(paste0("Intercept:", t$targets$key), t$loading_primitives$parameter, 
        paste0("observation_sd:", t$observation_parameters$key), if (E) "component_R[unique_free_entries]", 
        "z_raw[component,person]")
    if (!nrow(t$observation_parameters)) 
        expected_parameters <- setdiff(expected_parameters, "observation_sd:")
    if (anyDuplicated(p$parameter) || !setequal(p$parameter, expected_parameters) || anyNA(p$status) || 
        any(p$status != "specified") || any(!is.na(.bh_resolved_prior_errors(p)))) 
        .bh_component_prior_fail("Complete unique resolved primitive prior coverage is required")
    eta <- 1
    for (i in seq_len(nrow(p))) {
        d <- .bh_prior_distribution(p$prior[i])
        role <- p$class[i]
        values <- d$values
        valid <- switch(role, Intercept = d$distribution %in% c("normal", "logistic", "student_t"), 
            loading = d$distribution == "normal" && values[1] == 0, sigma = d$distribution %in% 
                c("half_normal", "half_student_t"), sdlog = d$distribution %in% c("half_normal", 
                "half_student_t"), observation_sd = d$distribution %in% c("half_normal", 
                "half_student_t"), cor = d$distribution == "lkj" && p$layer[i] == "component" && 
                values[1] >= 1 && values[1] <= 10000, structural_latent = d$distribution == 
                "normal" && identical(values, c(0, 1)), FALSE)
        if (!isTRUE(valid)) 
            .bh_component_prior_fail("Unsupported actual primitive prior/support in the narrow independent-prior audit")
        if (role == "cor") 
            eta <- values[1]
    }
    if (sum(p$class == "cor") != as.integer(E > 0) || !all(t$loading_primitives$support %in% 
        c("positive", "real")) || anyDuplicated(t$ordinary_loading_map$parameter) || any(t$ordinary_loading_map$state != 
        "free")) 
        .bh_component_prior_fail("One LKJ factor and singleton free Normal loading primitives are required")
    for (n in c("posterior_propriety", "posterior_normalizability")) if (spec$checks[[n]]$status %in% 
        c("proven_improper", "proven_non_normalizable")) 
        .bh_component_prior_fail("Retained evidence proves an improper or non-normalizable posterior")
    tables <- .bh_pf_take(t, .bh_component_table_fields())
    vars <- data.frame(id = v$id, scale = rep("fixed_unit_population_sd", K), orientation = rep("resolved_declared_component_direction", 
        K), stringsAsFactors = FALSE)
    list(K = K, E = E, eta = eta, variables = vars, direction_resolved = resolved, parameterization = list(coordinates = v, 
        maps = tables[setdiff(names(tables), "coordinates")], prior = spec$prior), model_id = spec$fingerprint, 
        prior_id = paste0("prepared_model_prior:", spec$fingerprint), audit = list(version = "ordinary_full_LKJ_unit_SD_v1", 
            normalized_measure = "joint full-R LKJ marginal with deterministic unit SD", 
            nuisance = "independent normalized intercept/noise priors, singleton Normal/half-Normal loadings and standard-Normal whites integrate out", 
            residual_scope = "all parameter residual layers excluded only as an initial implementation limitation", 
            hyperparameters = "resolved prepared prior parameters held fixed; data calibration is not rerun", 
            posterior_propriety = "not established by this generator", bayes_factor = "not implemented"))
}

