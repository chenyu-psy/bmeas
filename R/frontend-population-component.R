# Native posterior reader; retains source identity, scale, orientation and adverse evidence.
# The identity is the whole fixed mapping, not a shortened display label.

.bh_structural_component <- function(x, control, validate = TRUE) {
    if (!identical(class(x), "bmeas_component_draws") || !identical(x$schema_version, "bmeas_component_draws_v1") || 
        !identical(x$domain, "declared_component_population")) 
        .bh_structural_fail("Unsupported component extraction")
    .bh_component_status_validate(x$upstream)
    v <- x$coordinates
    sc <- x$maps$component_scales
    ori <- x$maps$orientation
    if (!identical(sc$coordinate_id, v$id) || !identical(ori$coordinate_id, v$id)) 
        .bh_structural_fail("Component scale/orientation IDs disagree with covariance axes")
    resolved <- ori$pinned | (!is.na(ori$anchor) & nzchar(ori$anchor))
    sign_system <- x$maps$orientation_sign_system
    if (!is.null(sign_system)) {
        basis <- sign_system$unanchored_binary_basis
        if (!is.matrix(basis) || nrow(basis) != nrow(v) || anyNA(basis) || any(!basis %in% 
            c(0, 1))) 
            .bh_structural_fail("Incomplete component orientation system")
        resolved <- rowSums(basis) == 0
    }
    vars <- data.frame(id = v$id, scale = ifelse(sc$state == "fixed", "fixed_unit_population_sd", 
        "marker_population_sd"), orientation = ifelse(resolved, "resolved_declared_component_direction", 
        "unresolved_component_direction"), stringsAsFactors = FALSE)
    identity <- x$source$bound_context
    if (!identical(identity$fit_fingerprint, identity$spec_fingerprint)) 
        .bh_structural_fail("Component model identities disagree")
    out <- structure(list(schema_version = "bmeas_population_draws_v1", covariance = x$covariance, 
        draw_keys = x$draw_keys, variables = vars, distribution = "posterior", model_id = identity$spec_fingerprint, 
        prior_id = paste0("prepared_model_prior:", identity$spec_fingerprint), provenance = list(source_kind = "bmeas_component_posterior", 
            source = "component_draws(fit): aligned saved R_component and component_row_sd", 
            joint_scope = "joint_population_covariance_parameters", verification = "existing_component_reader_validation_only"), 
        available = x$available, direction_resolved = resolved, parameterization = list(coordinates = v, 
            maps = x$maps, prior = x$prior), upstream = x), class = "bmeas_population_draws")
    if (validate) 
        .bh_structural_validate(out, control)
    out
}

#' Read retained component population posterior draws for structural mapping
#' @export
component_population_draws <- function(fit, budget = postprocess_budget("component_draws"), 
    control = list()) {
    cap <- .bh_structural_control(control)
    .bh_structural_component(component_draws(fit, budget = budget), cap)
}

