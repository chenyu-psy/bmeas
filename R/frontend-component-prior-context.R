# Narrow component-prior module. No likelihood, density estimation or BF.

# Reserve explicit output/copy, availability, keys and matrix workspace.
# The bound covers generation plus deterministic validation, not RSS or metadata hashing.
.bh_component_prior_budget <- function(n, K, control) {
    scalar <- function(z) is.numeric(z) && !is.object(z) && length(z) == 1L && is.finite(z)
    if (!scalar(n) || n < 1 || n != floor(n) || n > .Machine$integer.max || K > control$max_variables || 
        n > control$max_draws) 
        .bh_structural_fail("Invalid or over-budget prior draw dimensions", "E_STRUCTURAL_BUDGET")
    E <- K * (K - 1)/2
    slots <- n * (8 * K * K + 2 * E + 16) + 8 * K * K + 8 * K
    work <- n * (3 * K^3 + 16 * K * K + 4 * E)
    if (!is.finite(slots) || !is.finite(work) || slots > control$max_values || work > control$max_matrix_work || 
        n * K * K > .Machine$integer.max) 
        .bh_structural_fail("Prior generation exceeds explicit numeric/work allowance", "E_STRUCTURAL_BUDGET")
    list(numeric_slots = slots, matrix_work = work, scope = "explicit generation/validation numeric storage and work; not RSS, metadata serialization or elapsed time")
}

# Rebuild native prior evidence without RNG, invoking no model callbacks.
# The original prepared seal is validated, never replaced. The integrity digest
# detects inconsistent mutation; it is not cryptographic provenance authentication.
.bh_component_prior_restore <- function(upstream, control) {
    if (!is.list(upstream) || is.object(upstream) || !identical(names(upstream), c("spec", 
        "generation", "integrity"))) 
        .bh_structural_fail("Missing native prior generation evidence")
    g <- upstream$generation
    n <- nrow(g$draw_keys)
    gate <- .bh_component_prior_gate(upstream$spec, control = control, draws = n)
    K <- gate$K
    accounting <- .bh_component_prior_budget(n, K, control)
    if (!identical(upstream$integrity, .mp_hash(list(spec_id = gate$model_id, generation = g)))) 
        .bh_structural_fail("Native prior generation evidence changed", "E_STRUCTURAL_IDENTITY")
    if (!identical(g$receipt$model_id, gate$model_id) || !identical(g$receipt$prior_id, gate$prior_id) || 
        !identical(g$receipt$coordinates, gate$variables$id) || !identical(g$receipt$audit, 
        gate$audit) || !identical(g$receipt$eta, gate$eta) || !identical(g$receipt$K, gate$K) || 
        !identical(g$receipt$accounting, accounting) || !identical(g$receipt$algorithm, "canonical_partial_beta_v1") || 
        !identical(g$receipt$requested, n) || length(g$status) != n || !identical(dim(g$covariance), 
        as.integer(c(n, K, K))) || !identical(dimnames(g$covariance), list(NULL, gate$variables$id, 
        gate$variables$id)) || !identical(g$draw_keys, data.frame(chain_id = rep(1, n), saved_row = seq_len(n)))) 
        .bh_structural_fail("Native prior receipt does not match its prepared specification", 
            "E_STRUCTURAL_IDENTITY")
    good <- g$status == "generated"
    if (anyNA(good) || !identical(g$receipt$generated, sum(good)) || !identical(g$receipt$failed_ids, 
        which(!good))) 
        .bh_structural_fail("Native prior failure accounting changed")
    for (d in seq_len(n)) {
        R <- g$covariance[d, , ]
        dim(R) <- c(K, K)
        if (good[d]) {
            if (any(!is.finite(R)) || !identical(R, t(R)) || any(abs(diag(R) - 1) > 64 * 
                K * .Machine$double.eps) || inherits(tryCatch(chol(R), error = identity), 
                "error")) 
                .bh_structural_fail("Native generated correlation matrix is invalid")
        }
        else if (!all(is.na(R))) 
            .bh_structural_fail("Failed native draw must remain unavailable")
    }
    available <- array(rep(good, K * K), c(n, K, K), dimnames(g$covariance))
    structure(list(schema_version = "bmeas_population_draws_v1", covariance = g$covariance, 
        draw_keys = g$draw_keys, variables = gate$variables, distribution = "prior", model_id = gate$model_id, 
        prior_id = gate$prior_id, provenance = list(source_kind = "bmeas_component_prior", 
            source = "component_prior_draws(spec): native full-LKJ unit-SD joint covariance marginal", 
            joint_scope = "joint_population_covariance_parameters", verification = "prepared_spec_and_generation_integrity_checked_not_external_authentication"), 
        available = available, direction_resolved = gate$direction_resolved, parameterization = gate$parameterization, 
        prior_generation = g$receipt, draw_status = g$status, upstream = upstream), class = "bmeas_population_draws")
}

