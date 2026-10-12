# Narrow component-prior module. No likelihood, density estimation or BF.

# Deterministic canonical-partial-correlation -> Cholesky map.
# Column j consumes K-j coordinates; the remaining row length enforces unit SD.
# These coordinates are NOT independent marginal correlations.
.bh_prior_lkj_factor <- function(cpc, K) {
    if (!is.numeric(K) || length(K) != 1L || !is.finite(K) || K < 1 || K != floor(K))
        stop("invalid K")
    if (!is.numeric(cpc) || length(cpc) != K * (K - 1)/2 || any(!is.finite(cpc)) || any(abs(cpc) >=
        1))
        stop("noninterior canonical coordinate")
    L <- matrix(0, K, K)
    remaining <- rep(1, K)
    at <- 0L
    for (j in seq_len(K)) {
        L[j, j] <- sqrt(remaining[j])
        if (j < K)
            for (i in (j + 1L):K) {
                at <- at + 1L
                z <- cpc[at]
                L[i, j] <- z * sqrt(remaining[i])
                remaining[i] <- remaining[i] * (1 - z) * (1 + z)
            }
    }
    if (any(!is.finite(L)) || any(diag(L) <= 0))
        stop("unrepresentable factor")
    L
}

#' Generate the narrow full-LKJ unit-SD component population prior
#' @export
# Public orchestration: admit the full prepared model before touching RNG state.
# Beta shape eta + (K-j-1)/2 is dimension-dependent; always generate full K.
# Failed draws retain IDs and unavailable matrices, with no repair or retry.
component_prior_draws <- function(spec, draws, seed, control = list()) {
    cap <- .bh_structural_control(control)
    gate <- .bh_component_prior_gate(spec, control = cap, draws = draws)
    n <- draws
    K <- gate$K
    accounting <- .bh_component_prior_budget(n, K, cap)
    if (!is.numeric(seed) || is.object(seed) || length(seed) != 1L || !is.finite(seed) ||
        seed < 0 || seed > .Machine$integer.max || seed != floor(seed))
        .bh_structural_fail("seed must be an explicit nonnegative integer in R seed range")
    n <- as.integer(n)
    seed <- as.integer(seed)
    shape <- as.numeric(unlist(lapply(seq_len(K - 1L), function(j) rep(gate$eta + (K - j -
        1)/2, K - j)), use.names = FALSE))
    if (any(!is.finite(shape)) || any(shape <= 0))
        .bh_component_prior_fail("Beta shapes are not representable")
    covariance <- array(NA_real_, c(n, K, K), list(NULL, gate$variables$id, gate$variables$id))
    status <- rep("not_generated", n)
    # Restore the caller's exact RNG state on success, numerical failure or validation error.
    oldkind <- RNGkind()
    hadseed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
    if (hadseed)
        oldseed <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
    on.exit({
        do.call(RNGkind, as.list(oldkind))
        if (hadseed) {
            assign(".Random.seed", oldseed, .GlobalEnv)
        } else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(seed)
    # One attempt per requested ID. A failed matrix is retained as unavailable.
    for (d in seq_len(n)) {
        cpc <- 2 * stats::rbeta(length(shape), shape, shape) - 1
        value <- tryCatch({
            L <- .bh_prior_lkj_factor(cpc, K)
            R <- tcrossprod(L)
            if (any(!is.finite(R)) || any(abs(diag(R) - 1) > 64 * K * .Machine$double.eps) ||
                !identical(R, t(R)))
                stop("invalid represented correlation")
            chol(R)
            R
        }, error = identity)
        if (inherits(value, "error")) {
            status[d] <- paste0("numerical_failure: ", conditionMessage(value))
            next
        }
        covariance[d, , ] <- value
        status[d] <- "generated"
    }
    receipt <- list(algorithm = "canonical_partial_beta_v1", K = gate$K, eta = gate$eta,
        coordinates = gate$variables$id, model_id = gate$model_id, prior_id = gate$prior_id,
        audit = gate$audit, seed = seed, rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
        R_version = as.character(getRversion()), code_digest = .mp_hash(list(body(.bh_prior_lkj_factor),
            body(component_prior_draws))), requested = n, generated = sum(status == "generated"),
        failed_ids = which(status != "generated"), retries = 0L, key_semantics = "one IID batch, not an HMC chain",
        rng_state_restored_on_exit = TRUE, accounting = accounting)
    g <- list(covariance = covariance, draw_keys = data.frame(chain_id = rep(1, n), saved_row = seq_len(n)),
        status = status, receipt = receipt)
    upstream <- list(spec = spec, generation = g, integrity = .mp_hash(list(spec_id = gate$model_id,
        generation = g)))
    out <- .bh_component_prior_restore(upstream, cap)
    .bh_structural_validate(out, cap)
    out
}

