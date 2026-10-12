source(file.path("helpers", "native-guard.R"))
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if (nzchar(root)) for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f) else library(bmeas)
internal <- function(n) if (nzchar(root)) get(n, .GlobalEnv) else getFromNamespace(n, "bmeas")
local({
    passed <- 0L
    ok <- function(value, label) { stopifnot(isTRUE(value)); passed <<- passed + 1L; cat("PASS", label, "\n") }
    bad <- function(expr, code) {
        e <- tryCatch(force(expr), error = identity)
        ok(inherits(e, "measurement_error") && identical(e$code, code), code)
    }
    make <- function(beta, role) {
        n <- length(beta)
        covariance <- array(0, c(n, 2, 2), list(NULL, c("x", "y"), c("x", "y")))
        covariance[, 1, 1] <- 1; covariance[, 1, 2] <- beta
        covariance[, 2, 1] <- beta; covariance[, 2, 2] <- beta^2 + 1
        population_draws(covariance, data.frame(chain_id = rep(1L, n), saved_row = seq_len(n)),
            data.frame(id = c("x", "y"), scale = c("unit", "unit"), orientation = c("positive", "positive")),
            role, "normal-conjugate-fixture", "proper-normal-prior", "synthetic",
            "deterministic quantile quadrature, not sampling", "joint_population_covariance_parameters")
    }
    had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
    if (had_seed) seed <- get(".Random.seed", .GlobalEnv)
    q <- function(n, m = 0, s = 1) stats::qnorm((seq_len(n) - 0.5)/n, m, s)
    post <- make(q(192, 0.2, 0.6), "posterior")
    prior <- make(q(128), "prior")
    x <- structural_draws(post, "x", "y", prior)
    d <- structural_sddr(x, "x", "y", control = list(bandwidth = c(prior = 0.3, posterior = 0.2)))
    ok(d$status == "experimental_density_ratio_only" && nrow(d$bandwidth_sensitivity$grid) == 25L, "unequal draw counts and independent bandwidth grid")
    ok(all(is.na(c(d$log_bf01, d$bf01, d$bf10))) && d$bf_status == "not_assessed", "formal BF never qualified")
    ok(d$diagnostics$posterior$sampling$status == "not_assessed", "quantile fixtures never sampling evidence")
    expected <- log(mean(dnorm(q(192, 0.2, 0.6)/0.2))/0.2) - log(mean(dnorm(q(128)/0.3))/0.3)
    ok(abs(d$experimental_log_density_ratio - expected) < 1e-12, "direct kernel sum comparison")
    smooth <- dnorm(0, 0.2, sqrt(0.6^2 + 0.2^2), log = TRUE) - dnorm(0, 0, sqrt(1 + 0.3^2), log = TRUE)
    ok(abs(expected - smooth) < 0.002, "finite quadrature versus exact Gaussian convolution")
    same <- structural_draws(make(q(128), "posterior"), "x", "y", prior)
    equal <- structural_sddr(same, "x", "y")
    ok(equal$experimental_log_density_ratio == 0, "identical empirical distributions yield ratio one")
    changed <- x; changed$posterior$coefficients[1, 1, 1] <- 100
    bad(structural_sddr(changed, "x", "y"), "E_STRUCTURAL_IDENTITY")
    changed <- x; changed$posterior$draw_status$status[1] <- "failed"
    bad(structural_sddr(changed, "x", "y"), "E_STRUCTURAL_IDENTITY")
    changed <- x; changed$provenance$prior$prior_id <- "other"
    bad(structural_sddr(changed, "x", "y"), "E_STRUCTURAL_IDENTITY")
    legacy <- x; legacy$evidence_binding <- NULL
    ok(structural_sddr(legacy, "x", "y")$status == "blocked_unbound_result", "legacy mapping cannot be retroactively sealed by diagnostic")
    bad(structural_sddr(x, c("x", "y"), "y"), "E_STRUCTURAL_CONTRACT")
    bad(structural_sddr(x, "x", "y", null = 1), "E_STRUCTURAL_CONTRACT")
    bad(structural_sddr(x, "x", "y", null_prior = "independent"), "E_STRUCTURAL_CONTRACT")
    bad(structural_sddr(x, "x", "y", control = list(bandwidth = c(1, 1))), "E_STRUCTURAL_CONTRACT")
    bad(structural_sddr(x, "x", "y", control = list(max_draws = 10)), "E_STRUCTURAL_BUDGET")
    bad(structural_sddr(x, "x", "y", control = list(max_kernel_evaluations = 10)), "E_STRUCTURAL_BUDGET")
    duplicate <- structural_draws(make(rep(q(64), 2), "posterior"), "x", "y", prior)
    ok(structural_sddr(duplicate, "x", "y")$status == "density_unavailable", "automatic bandwidth blocked on repetition")
    explicit <- structural_sddr(duplicate, "x", "y", control = list(bandwidth = c(prior = .3, posterior = .3)))
    ok(explicit$diagnostics$posterior$draws == 128 && explicit$diagnostics$posterior$repetition$repeated_population_states == 64,
       "explicit bandwidth retains all duplicate states without ESS claim")
    constant <- structural_draws(make(rep(0, 30), "posterior"), "x", "y", prior)
    ok(structural_sddr(constant, "x", "y", control = list(bandwidth = c(prior = .3, posterior = .3)))$status == "density_unavailable", "constant coefficient blocked even with bandwidth")
    tail <- structural_draws(make(q(128, 10, .1), "posterior"), "x", "y", prior)
    far <- structural_sddr(tail, "x", "y", control = list(bandwidth = c(prior = .3, posterior = .1)))
    ok(far$density_status == "insufficient_observed_support" && is.na(far$bf01), "tail estimate explicitly unqualified")
    ok(!all(far$density$posterior$estimates$density_representable), "density underflow disclosed while log value retained")
    no_prior <- structural_draws(post, "x", "y")
    ok(structural_sddr(no_prior, "x", "y")$status == "blocked_missing_prior", "posterior-only gives no ratio")
    unavailable <- post; unavailable$covariance[1, , ] <- NA_real_
    failed <- structural_draws(unavailable, "x", "y", prior)
    out <- structural_sddr(failed, "x", "y")
    ok(out$status == "blocked_incomplete_mapping" && nrow(out$draw_status$posterior) == 192, "failed draw retained and prevents selected KDE")
    # Pre-admission hostile objects must not execute methods or active bindings.
    changed <- x; changed$provenance <- new.env(parent = emptyenv())
    makeActiveBinding("prior", function(...) stop("ACTIVE BINDING EXECUTED"), changed$provenance)
    bad(structural_sddr(changed, "x", "y"), "E_STRUCTURAL_CONTRACT")
    changed <- x; names(changed$provenance) <- c("prior", "prior")
    bad(structural_sddr(changed, "x", "y"), "E_STRUCTURAL_CONTRACT")
    changed <- x; changed$diagnostics$executable <- function() stop("CALLBACK EXECUTED")
    bad(structural_sddr(changed, "x", "y"), "E_PORTABLE_TYPE")
    # Artificial snapshot fixture only: not a generated native prior claim.
    snapshot <- x
    snapshot$provenance$prior$provenance$source_kind <- "bmeas_component_prior"
    snapshot$provenance$prior$upstream <- list(spec = list(fingerprint = "creation-identity",
        checks = list(posterior_propriety = list(status = "not_assessed")),
        callback = function() stop("EXCLUDED CALLBACK EXECUTED")), generation = list(), integrity = "fixture")
    snapshot <- internal(".bh_structural_bind")(snapshot)
    payload <- internal(".bh_structural_evidence_payload")(snapshot)
    ok(isTRUE(payload$provenance$prior$upstream$spec$excluded_legacy_spec_remainder), "legacy spec projection is explicit")
    changed <- snapshot; changed$provenance$prior$upstream$spec$fingerprint <- "changed"
    bad(structural_sddr(changed, "x", "y"), "E_STRUCTURAL_IDENTITY")
    changed <- snapshot; changed$provenance$prior$upstream$spec$checks$posterior_propriety$status <- "proven_improper"
    bad(structural_sddr(changed, "x", "y"), "E_STRUCTURAL_IDENTITY")
    changed <- snapshot; changed$provenance$prior$upstream$spec$callback <- new.env(parent = emptyenv())
    ok(identical(internal(".bh_structural_evidence_payload")(changed), payload), "excluded remainder explicitly unauthenticated without traversal")
    # Install sentinel methods briefly, restoring existing global bindings.
    method_name <- "$<-.sddr_hostile"
    existed <- exists(method_name, .GlobalEnv, inherits = FALSE)
    if (existed) previous <- get(method_name, .GlobalEnv)
    assign(method_name, function(...) stop("REPLACEMENT METHOD EXECUTED"), .GlobalEnv)
    changed <- snapshot; class(changed$provenance$prior$upstream) <- "sddr_hostile"
    projected <- internal(".bh_structural_evidence_payload")(changed)
    ok(inherits(projected$provenance$prior$upstream, "sddr_hostile"), "projection replacement never dispatches S3 method")
    if (existed) assign(method_name, previous, .GlobalEnv) else rm(list = method_name, envir = .GlobalEnv)
    # A chain count budget is checked before the pairwise clone scan.
    many <- post; many$draw_keys$chain_id <- seq_len(nrow(many$draw_keys))
    many <- structural_draws(many, "x", "y", prior)
    bad(structural_sddr(many, "x", "y"), "E_STRUCTURAL_BUDGET")
    # Explicit bandwidth replication does not manufacture a different density.
    unique_post <- structural_draws(make(q(64), "posterior"), "x", "y", prior)
    unique_result <- structural_sddr(unique_post, "x", "y", control = list(bandwidth = c(prior=.3, posterior=.3)))
    ok(abs(unique_result$experimental_log_density_ratio - explicit$experimental_log_density_ratio) < 1e-12,
       "fixed-bandwidth duplication preserves empirical density")
    # Deterministic delegation tests, not a statistical MCMC validation.
    rows <- rep(seq_len(24), 4); chain <- rep(seq_len(4), each = 24)
    vals <- sin(rows * 1.413 + chain / 4) + cos(rows * .732 + chain)
    keys <- data.frame(chain_id = chain, saved_row = rows)
    diagnostics <- internal(".bh_sddr_sampling")(vals, keys, list(identical_chain_sequences = character()), "external")
    if (requireNamespace("posterior", quietly = TRUE)) {
        expected_metrics <- c(rhat = posterior::rhat(matrix(vals, 24, 4)),
            ess_bulk = posterior::ess_bulk(matrix(vals, 24, 4)), ess_tail = posterior::ess_tail(matrix(vals, 24, 4)))
        ok(identical(diagnostics$metrics, expected_metrics) && is.na(diagnostics$mcse$value),
           "optional coefficient diagnostics exactly delegate; no density MCSE")
    } else ok(diagnostics$status == "not_assessed", "missing optional posterior dependency is explicit")
    keys$saved_row[24] <- 30
    irregular <- internal(".bh_sddr_sampling")(vals, keys, list(identical_chain_sequences = character()), "external")
    ok(irregular$status == "not_assessed", "irregular retained spacing blocks chain diagnostics")
    cloned <- internal(".bh_sddr_sampling")(vals, keys, list(identical_chain_sequences = "1:2"), "external")
    ok(cloned$status == "not_assessed", "identical chain evidence never independent precision")
    # Pure target-schema fixtures; no lavaan fitting or new origin assertion.
    fixed_target <- x
    fixed_target$mapping_spec$path_fixed_values <- matrix(0, 1, 1)
    ok(internal(".bh_sddr_target")(fixed_target, "x", "y", 0, "encompassing_conditional",
        internal(".bh_sddr_control")(list())) == "blocked_fixed_coefficient", "fixed or omitted path cannot become continuous target")
    changed <- x; changed$evidence_binding$excess <- rep("x", 3000)
    bad(structural_sddr(changed, "x", "y"), "E_PORTABLE_BUDGET")
    bad(structural_sddr(x, "x", "y", control = list(bandwidth = c(prior=1+1i, posterior=1+1i))), "E_STRUCTURAL_CONTRACT")
    # Historical 522 checks used exact densities, not KDE. Reuse one analytic identity.
    X <- seq(-1.1, 1.3, length.out = 5); y <- .15 * X + .17 * sin(seq_len(5) * 1.4)
    sigma <- .4; tau <- .7; v <- 1/(sum(X^2)/sigma^2 + 1/tau^2); m <- v * sum(X*y)/sigma^2
    exact <- .5 * log(tau^2/v) - m^2/(2*v)
    log_mvn <- function(y, C) { L <- chol(C); z <- backsolve(L, y, transpose = TRUE); -.5*(length(y)*log(2*pi)+2*sum(log(diag(L)))+sum(z^2)) }
    marginal <- log_mvn(y, diag(sigma^2, 5)) - log_mvn(y, diag(sigma^2, 5) + tau^2 * tcrossprod(X))
    ok(abs(exact - marginal) < 1e-10, "normal-conjugate exact SDDR versus independent marginal likelihood")
    ok(identical(had_seed, exists(".Random.seed", .GlobalEnv, inherits = FALSE)) &&
       (!had_seed || identical(seed, get(".Random.seed", .GlobalEnv))), "entire focused suite leaves RNG unchanged")
    cat("PASSED", passed, "deterministic S4a checks; no sampling or KDE Monte Carlo validation\n")
})
