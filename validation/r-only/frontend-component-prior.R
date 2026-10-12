source(file.path("helpers", "native-guard.R"))

root <- Sys.getenv("MEASUREMENTPKG_SOURCE")

if (nzchar(root)) for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f) else library(bmeas)

internal <- function(n) if (nzchar(root)) get(n, .GlobalEnv) else getFromNamespace(n, "bmeas")

local({
    count <- 0L
    ok <- function(x, label) {
        stopifnot(isTRUE(x))
        count <<- count + 1L
        cat("PASS", label, "\n")
    }
    bad <- function(expr, code, label) {
        error <- tryCatch(force(expr), error = identity)
        ok(inherits(error, "measurement_error") && identical(error$code, code), label)
    }
    d <- data.frame(person = as.character(1:8))
    for (j in 1:6) d[[paste0("x", j)]] <- sin((1:8) * j/4) + j/10
    spec <- bmeas(bmf(A = ~x1 + x2 + x3, B = ~x4 + x5 + x6), d, subject = "person", fit = FALSE)
    # RNG lifecycle and native generation receipt.
    set.seed(612)
    before <- .Random.seed
    prior <- component_prior_draws(spec, 32, 61701)
    ok(identical(before, .Random.seed), "native generator restores RNG")
    ok(all(prior$draw_status == "generated") && all(prior$available), "native full-LKJ unit-SD generated")
    ok(prior$provenance$source_kind == "bmeas_component_prior" && prior$distribution == "prior", 
        "first-party prior identity")
    ok(prior$prior_generation$requested == 32 && prior$prior_generation$retries == 0, "bounded receipt")
    # Scientific admission and pre-RNG resource refusal.
    gate <- internal(".bh_component_prior_gate")
    validator <- internal(".bh_structural_validate")
    control <- internal(".bh_structural_control")(list())
    variant <- spec
    variant$tables$correlations$state[1] <- "zero"
    variant$tables$correlation_mask[1, 2] <- variant$tables$correlation_mask[2, 1] <- "zero"
    variant$tables$correlation_affine_cells$state[1] <- "zero"
    variant$tables$correlation_affine_cells$fixed_value[1] <- 0
    bad(gate(variant, FALSE), "E_COMPONENT_PRIOR_SCOPE", "reject zero/restricted correlation")
    variant <- spec
    variant$tables$component_scales$fixed_sd[1] <- 2
    bad(gate(variant, FALSE), "E_COMPONENT_PRIOR_SCOPE", "reject nonunit SD")
    variant <- spec
    variant$tables$orientation$anchor[1] <- NA_character_
    bad(gate(variant, FALSE), "E_COMPONENT_PRIOR_SCOPE", "reject unresolved direction")
    variant <- spec
    variant$tables$priors$prior[variant$tables$priors$class == "cor"] <- "lkj(0.5)"
    bad(gate(variant, FALSE), "E_COMPONENT_PRIOR_SCOPE", "reject eta below first profile")
    variant <- spec
    variant$tables$priors$prior[variant$tables$priors$class == "cor"] <- "lkj(1e40)"
    bad(gate(variant, FALSE), "E_COMPONENT_PRIOR_SCOPE", "reject numerically unqualified large eta")
    variant <- spec
    variant$tables$priors$prior[variant$tables$priors$class == "loading"][1] <- "normal(1,1)"
    bad(gate(variant, FALSE), "E_COMPONENT_PRIOR_SCOPE", "reject unsupported loading law")
    bad(component_prior_draws(variant, 1, 10), "E_SPEC_CHANGED", "public modified spec refused")
    bad(component_prior_draws(spec, 32, 10, control = list(max_variables = 1)), "E_STRUCTURAL_BUDGET", 
        "coordinate cap before gate workspace")
    bad(component_prior_draws(spec, 32, 10, control = list(max_values = 1)), "E_STRUCTURAL_BUDGET", 
        "pre-RNG budget refused")
    ok(identical(before, .Random.seed), "rejected requests preserve RNG")
# Native provenance mutation: assert exact error class and code.
    altered <- prior
    altered$distribution <- "posterior"
    bad(validator(altered, control), "E_STRUCTURAL_IDENTITY", "native prior cannot become posterior")
    altered <- prior
    altered$covariance[1, 1, 2] <- 0
    bad(validator(altered, control), "E_STRUCTURAL_IDENTITY", "native covariance mutation refused")
    altered <- prior
    altered$prior_generation$eta <- 1
    bad(validator(altered, control), "E_STRUCTURAL_IDENTITY", "receipt mutation refused")
    altered <- prior
    altered$upstream$generation$receipt$eta <- 1
    bad(validator(altered, control), "E_STRUCTURAL_IDENTITY", "retained generation mutation refused")
    altered <- prior
    altered$provenance$source_kind <- "external"
    altered$provenance$verification <- "user_declared_not_verified"
    bad(validator(altered, control), "E_STRUCTURAL_CONTRACT", "native evidence cannot be relabeled external")
    # Engineering CSV only: exact prior/posterior pairing, with no fitted posterior.
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    vars <- c("R_component.1.1", "R_component.2.1", "R_component.1.2", "R_component.2.2", 
        "component_row_sd.1", "component_row_sd.2", "component_covariance_available.1.1", 
        "component_covariance_available.2.1", "component_covariance_available.1.2", "component_covariance_available.2.2")
    writeLines(c("# ENGINEERING SYNTHETIC FIXTURE; no posterior inference", "# stan_version_major = 2", 
        "# stan_version_minor = 40", "# stan_version_patch = 0", "# model = scalar_components_model", 
        "# id = 7", "# method = sample (Default)", "# num_samples = 2", "# num_warmup = 0", 
        "# save_warmup = false (Default)", "# thin = 1", "# algorithm = hmc (Default)", "# engine = nuts (Default)", 
        "# metric = diag_e (Default)", "# max_depth = 4", "# sig_figs = 17", paste(vars, 
            collapse = ","), "1,0.5,0.5,1,1,1,1,1,1,1", "1,0.25,0.25,1,1,1,1,1,1,1", "# Elapsed Time: 0 seconds (Warm-up)", 
        "# 0 seconds (Sampling)", "# 0 seconds (Total)"), path)
    fit <- structure(list(spec = spec, run = structure(list(chains = 1L, csv_sig_figs = 17L), 
        class = "measurement_run_config"), cache = list(inference = list(status = "not_assessed", 
        reason = "Synthetic fixture")), provenance = list(output_files = path, model_fingerprint = spec$fingerprint, 
        csv_sig_figs = 17L)), class = c("bmeas_fit", "bhmm_fit"))
    post <- component_population_draws(fit)
    for (n in c("model_id", "prior_id", "variables", "parameterization", "direction_resolved")) ok(identical(prior[[n]], 
        post[[n]]), paste("exact native identity", n))
    ids <- prior$variables$id
    result <- structural_draws(post, ids[1], ids[2], prior)
    ok(all(result$prior$draw_status$status == "mapped"), "native prior and posterior share mapping")
    ok(max(abs(result$prior$coefficients[, 1, 1] - prior$covariance[, 1, 2])) < 1e-14, "K2 beta equals rho at unit SD")
    ok(identical(result$diagnostics$bayes_factor, "not_implemented"), "no BF claim")
    ok(identical(before, .Random.seed), "structural mapping performs no RNG")
    # Original RNG absence must also be restored.
    rm(".Random.seed", envir = .GlobalEnv)
    small <- component_prior_draws(spec, 1, 61702)
    ok(!exists(".Random.seed", .GlobalEnv, inherits = FALSE), "original RNG absence restored")
    cat("PASSED", count, "component prior checks; 33 IID draws; synthetic posterior fixture only\n")
})

