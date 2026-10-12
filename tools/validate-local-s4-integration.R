# PROPOSED, NOT EXECUTED: opt-in installed-package integration on the user's Mac.
# Budget: 24 genuine IID prior states (20 + 4), at most 8 lavaan fits, serial R.
# Synthetic retained-posterior CSVs are engineering fixtures, never HMC evidence.
# This script belongs in tools/, not the automatically executed tests/ directory.

run_local_s4_integration <- function() {
    if (!identical(Sys.info()[["sysname"]], "Darwin"))
        stop("This proposed integration run is restricted to the verified user Mac.")
    if (!identical(Sys.getenv("BMEAS_RUN_LOCAL_S4_INTEGRATION"), "approved_24_prior_8_lavaan"))
        stop("Separate parent-approved local integration budget is required; no work started.")
    thread_controls <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS")
    if (!all(Sys.getenv(thread_controls) == "1"))
        stop("Launch with all four documented library thread controls set to 1.")
    run_id <- Sys.getenv("BMEAS_LOCAL_S4_RUN_ID")
    if (!nzchar(run_id)) stop("The parent-owned cumulative ledger must supply a unique run ID.")
    if ("cmdstanr" %in% loadedNamespaces())
        stop("Use a fresh Rscript --vanilla process so the native guard is installed before cmdstanr loads.")

    capture_rng <- function() {
        present <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
        list(kind = RNGkind(), present = present,
             seed = if (present) get(".Random.seed", .GlobalEnv, inherits = FALSE) else NULL)
    }
    restore_rng <- function(state) {
        do.call(RNGkind, as.list(state$kind))
        if (state$present) assign(".Random.seed", state$seed, .GlobalEnv) else
            if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
                rm(".Random.seed", envir = .GlobalEnv)
    }
    entry_rng <- capture_rng()
    checks <- 0L
    requested_prior_states <- 0L
    returned_generated_states <- 0L
    prior_calls_started <- 0L
    prior_calls_returned <- 0L
    requested_lavaan_fits <- 0L
    actual_lavaan_fits <- 0L
    lavaan_call_returned <- FALSE
    completed <- FALSE
    on.exit({
        cat("\nFINAL ATTEMPT RECEIPT\nrun_id:", run_id,
            "\ncompleted:", completed, "\nchecks_passed:", checks,
            "\nprior_calls_started:", prior_calls_started,
            "\nprior_calls_returned:", prior_calls_returned,
            "\nrequested_prior_states:", requested_prior_states,
            "\nreturned_generated_states:", returned_generated_states,
            "\nrequested_lavaan_fits:", requested_lavaan_fits,
            "\nactual_lavaan_fits_from_returned_result:", actual_lavaan_fits,
            "\nlavaan_call_returned:", lavaan_call_returned,
            "\nrng_matches_entry_before_cleanup:", identical(capture_rng(), entry_rng),
            "\nledger_rule: charge requested budgets for interrupted/nonreturned calls; do not infer zero usage.\n")
        restore_rng(entry_rng)
    }, add = TRUE)

    # Reuse tests/helpers/native-guard.R's compiler prohibition. This guard does
    # not disable any production frontend validation or replace provider data.
    event <- packageEvent("cmdstanr", "onLoad")
    original_hooks <- getHook(event)
    on.exit(setHook(event, original_hooks, action = "replace"), add = TRUE)
    setHook(event, function(...) {
        trace("cmdstan_model", tracer = quote(stop(
            "FORBIDDEN: real CmdStan model construction in local S4 integration")),
            where = asNamespace("cmdstanr"), print = FALSE)
    }, action = "append")

    if (!requireNamespace("bmeas", quietly = TRUE) ||
        as.character(utils::packageVersion("bmeas")) != "0.0.40.9000")
        stop("Install the separately verified .40 candidate into the approved private library first; this script installs nothing.")
    cat("bmeas version:", as.character(utils::packageVersion("bmeas")), "\n")
    cat("bmeas path:", find.package("bmeas"), "\n")
    cat("R version:", as.character(getRversion()), "\n")

    ok <- function(value, label) {
        if (!isTRUE(value)) stop("FAIL: ", label)
        checks <<- checks + 1L
        cat("PASS", label, "\n")
    }
    expect_identity_error <- function(expr, label) {
        error <- tryCatch(force(expr), error = identity)
        ok(inherits(error, "measurement_error") &&
            identical(error$code, "E_STRUCTURAL_IDENTITY"), label)
    }
    same_rng <- function(label) ok(identical(capture_rng(), entry_rng), label)
    close_numeric <- function(a, b, tolerance = 1e-6) {
        all(is.finite(c(a, b))) && max(abs(a - b)) <= tolerance * (1 + max(abs(b)))
    }

    prepare_model <- function() {
        data <- data.frame(person = as.character(seq_len(8)))
        for (j in seq_len(6)) data[[paste0("x", j)]] <- sin(seq_len(8) * j / 4) + j / 10
        bmeas::bmeas(bmeas::bmf(A = ~x1 + x2 + x3, B = ~x4 + x5 + x6),
                     data, subject = "person", fit = FALSE)
    }
    generate_prior <- function(spec, n, seed) {
        if (requested_prior_states + n > 24L) stop("The 24-state prior budget would be exceeded.")
        requested_prior_states <<- requested_prior_states + n
        prior_calls_started <<- prior_calls_started + 1L
        cat("ATTEMPT native prior:", n, "states; seed:", seed, "\n")
        value <- bmeas::component_prior_draws(spec, draws = n, seed = seed)
        prior_calls_returned <<- prior_calls_returned + 1L
        receipt <- value$prior_generation
        returned_generated_states <<- returned_generated_states + receipt$generated
        ok(identical(value$provenance$source_kind, "bmeas_component_prior") &&
            identical(value$distribution, "prior"), "genuine first-party prior source")
        ok(receipt$requested == n && receipt$generated == n &&
            length(receipt$failed_ids) == 0L && receipt$retries == 0L &&
            receipt$seed == seed && isTRUE(receipt$rng_state_restored_on_exit),
            "complete original generation receipt without retries")
        ok(identical(receipt$algorithm, "canonical_partial_beta_v1") && receipt$K == 2L &&
            is.finite(receipt$eta) && receipt$eta >= 1 && receipt$eta <= 10000 &&
            identical(receipt$model_id, spec$fingerprint) &&
            identical(receipt$prior_id, value$prior_id) &&
            identical(receipt$coordinates, value$variables$id), "full-LKJ model and coordinate receipt")
        ok(identical(value$covariance, value$upstream$generation$covariance) &&
            identical(value$draw_keys, value$upstream$generation$draw_keys) &&
            identical(receipt, value$upstream$generation$receipt) &&
            all(value$draw_status == "generated") && all(value$available),
            "retained native generation arrays and keys are unmodified")
        ok(all(abs(value$covariance[, 1, 1] - 1) < 1e-12) &&
            all(abs(value$covariance[, 2, 2] - 1) < 1e-12), "fixed unit population SDs")
        same_rng("native prior generator restored entry RNG state")
        value
    }

    make_retained_fixture <- function(spec, correlations) {
        # Adapt the existing frontend-component-prior.R synthetic CSV fixture.
        # The saved-reader adapter is real; the supplied states are not fitted.
        path <- tempfile("synthetic-s4-posterior-", fileext = ".csv")
        on.exit(unlink(path), add = TRUE)
        columns <- c("R_component.1.1", "R_component.2.1", "R_component.1.2", "R_component.2.2",
            "component_row_sd.1", "component_row_sd.2", "component_covariance_available.1.1",
            "component_covariance_available.2.1", "component_covariance_available.1.2",
            "component_covariance_available.2.2")
        rows <- vapply(correlations, function(rho)
            paste(c(1, rho, rho, 1, 1, 1, 1, 1, 1, 1), collapse = ","), character(1))
        writeLines(c("# ENGINEERING SYNTHETIC FIXTURE; no posterior inference; no HMC was run",
            "# stan_version_major = 2", "# stan_version_minor = 40", "# stan_version_patch = 0",
            "# model = scalar_components_model", "# id = 7", "# method = sample (Default)",
            paste0("# num_samples = ", length(correlations)), "# num_warmup = 0",
            "# save_warmup = false (Default)", "# thin = 1", "# algorithm = hmc (Default)",
            "# engine = nuts (Default)", "# metric = diag_e (Default)", "# max_depth = 4",
            "# sig_figs = 17", paste(columns, collapse = ","), rows,
            "# Elapsed Time: 0 seconds (Warm-up)", "# 0 seconds (Sampling)", "# 0 seconds (Total)"), path)
        fit <- structure(list(spec = spec,
            run = structure(list(chains = 1L, csv_sig_figs = 17L), class = "measurement_run_config"),
            cache = list(inference = list(status = "not_assessed",
                reason = "Synthetic engineering states; no measurement sampling or posterior qualification")),
            provenance = list(output_files = path, model_fingerprint = spec$fingerprint,
                csv_sig_figs = 17L, kind = "engineering_synthetic_fixture_no_posterior_qualification")),
            class = c("bmeas_fit", "bhmm_fit"))
        posterior <- bmeas::component_population_draws(fit)
        ok(nrow(posterior$draw_keys) == length(correlations) &&
            all(posterior$draw_keys$chain_id == 7) &&
            identical(as.numeric(posterior$draw_keys$saved_row), as.numeric(seq_along(correlations))),
            "synthetic saved-reader chain and row identities preserved")
        ok(close_numeric(posterior$covariance[, 1, 2], correlations, 1e-14),
            "synthetic saved-reader correlations preserved")
        posterior
    }
    check_matching_contracts <- function(prior, posterior) {
        for (field in c("model_id", "prior_id", "variables", "parameterization", "direction_resolved"))
            ok(identical(prior[[field]], posterior[[field]]), paste("exact prior/posterior", field))
        ok(identical(posterior$upstream$upstream$current_cache$statuses$inference$status, "not_assessed"),
            "synthetic retained-posterior inference remains not assessed")
    }
    point_log_density <- function(values, h) log(mean(stats::dnorm(values / h)) / h)
    check_s4 <- function(mapped, predictor, outcome, label) {
        widths <- c(prior = 0.35, posterior = 0.35)
        answer <- bmeas::structural_sddr(mapped, predictor, outcome, control = list(bandwidth = widths))
        ok(identical(mapped$evidence_binding$schema_version, "bmeas_structural_scientific_binding_v1") &&
            identical(answer$source_status$mapping, "creation_bound_scientific_projection"),
            paste(label, "creation binding accepted by public S4"))
        expected <- point_log_density(mapped$posterior$coefficients[, predictor, outcome], widths[["posterior"]]) -
            point_log_density(mapped$prior$coefficients[, predictor, outcome], widths[["prior"]])
        ok(identical(answer$status, "experimental_density_ratio_only") &&
            is.finite(answer$experimental_log_density_ratio) &&
            abs(answer$experimental_log_density_ratio - expected) < 1e-12 &&
            nrow(answer$bandwidth_sensitivity$grid) == 25L,
            paste(label, "finite diagnostic KDE agrees with direct kernel sums"))
        ok(identical(answer$density_status, "insufficient_observed_support") &&
            identical(answer$bf_status, "not_assessed") &&
            all(is.na(c(answer$log_bf01, answer$bf01, answer$bf10))),
            paste(label, "small fixture cannot qualify density precision or formal BF"))
        for (role in c("prior", "posterior")) {
            mcse <- answer$diagnostics[[role]]$sampling$mcse
            ok(identical(mcse$status, "not_assessed") && is.na(mcse$value),
                paste(label, role, "density MCSE remains unavailable"))
        }
        changed <- mapped
        changed$posterior$coefficients[1, 1, 1] <- changed$posterior$coefficients[1, 1, 1] + 0.01
        expect_identity_error(bmeas::structural_sddr(changed, predictor, outcome,
            control = list(bandwidth = widths)), paste(label, "bound coefficient mutation rejected"))
        answer
    }

    spec <- prepare_model()
    prior <- generate_prior(spec, 20L, 61701L)
    posterior <- make_retained_fixture(spec, c(-0.35, -0.25, -0.15, -0.05, 0.05, 0.15, 0.25, 0.35))
    check_matching_contracts(prior, posterior)
    ids <- prior$variables$id
    analytic <- bmeas::structural_draws(posterior, ids[1], ids[2], prior_draws = prior)
    ok(all(analytic$prior$draw_status$status == "mapped") &&
        all(analytic$posterior$draw_status$status == "mapped"), "all real-prior and retained-fixture analytic mappings complete")
    ok(close_numeric(analytic$prior$coefficients[, 1, 1], prior$covariance[, 1, 2], 1e-12),
        "unit-SD two-variable analytic coefficient equals native correlation")
    ok(close_numeric(analytic$posterior$coefficients[, 1, 1], posterior$covariance[, 1, 2], 1e-12),
        "analytic coefficient matches synthetic retained correlation")
    analytic_s4 <- check_s4(analytic, ids[1], ids[2], "analytic")
    changed <- analytic
    changed$provenance$prior$prior_generation$seed <- changed$provenance$prior$prior_generation$seed + 1L
    expect_identity_error(bmeas::structural_sddr(changed, ids[1], ids[2]),
        "real native-prior generation-receipt mutation rejected")
    same_rng("analytic pipeline did not change RNG state")

    lavaan_status <- "NOT_RUN_missing_or_unqualified_lavaan"
    if (requireNamespace("lavaan", quietly = TRUE) &&
        as.character(utils::packageVersion("lavaan")) == "0.7.2") {
        lavaan_prior <- generate_prior(spec, 4L, 61702L)
        lavaan_posterior <- make_retained_fixture(spec, c(-0.3, -0.1, 0.1, 0.3))
        check_matching_contracts(lavaan_prior, lavaan_posterior)
        requested_lavaan_fits <- 8L
        actual_lavaan_fits <- NA_integer_
        cat("ATTEMPT lavaan: at most eight fits; no retries\n")
        mapped <- bmeas::structural_draws(lavaan_posterior, ids[1], ids[2],
            prior_draws = lavaan_prior, backend = "lavaan", backend_control = list(max_fits = 8L))
        lavaan_call_returned <- TRUE
        actual_lavaan_fits <- mapped$prior$diagnostics$fit_calls + mapped$posterior$diagnostics$fit_calls
        ok(actual_lavaan_fits == 8L && all(mapped$prior$draw_status$status == "mapped") &&
            all(mapped$posterior$draw_status$status == "mapped"), "exactly eight complete deterministic lavaan fits")
        ok(identical(mapped$prior$diagnostics$resolved_options, mapped$posterior$diagnostics$resolved_options),
            "lavaan prior and posterior share resolved mapping options")
        ok(close_numeric(mapped$prior$coefficients[, 1, 1], lavaan_prior$covariance[, 1, 2]) &&
            close_numeric(mapped$posterior$coefficients[, 1, 1], lavaan_posterior$covariance[, 1, 2]),
            "actual saturated lavaan coefficients match analytic unit-SD identity")
        lavaan_s4 <- check_s4(mapped, ids[1], ids[2], "lavaan")
        same_rng("lavaan mapping and diagnostic calls did not change RNG state")
        lavaan_status <- "fixture_integration_passed_not_density_precision_qualification"
    } else cat("NOT RUN: optional lavaan branch needs exactly lavaan 0.7.2; no installation or fallback attempted.\n")

    same_rng("whole integration preserved entry RNG state")
    ok(requested_prior_states <= 24L && actual_lavaan_fits <= 8L, "aggregate integration budgets respected")
    cat("INTEGRATION RECEIPT\nchecks_passed:", checks,
        "\nnew_IID_prior_states:", requested_prior_states,
        "\nactual_lavaan_fits:", actual_lavaan_fits,
        "\nanalytic_status: fixture_integration_passed_not_density_precision_qualification",
        "\nlavaan_status:", lavaan_status,
        "\nsynthetic_posterior: true; no real posterior sampling, native compilation or HMC\n")
    if (lavaan_status != "fixture_integration_passed_not_density_precision_qualification")
        stop("PARTIAL: analytic integration passed; actual lavaan integration remains unverified.")
    completed <- TRUE
    invisible(list(checks = checks, prior_states = requested_prior_states, lavaan_fits = actual_lavaan_fits))
}

run_local_s4_integration()
