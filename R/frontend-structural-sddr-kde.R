# Pointwise Gaussian KDE only. No density grid, normal fit, resampling or BF.
.bh_sddr_log_kernel <- function(values, bandwidth) {
    terms <- -0.5 * (values / bandwidth)^2
    largest <- max(terms)
    if (!is.finite(largest)) return(NA_real_)
    largest + log(sum(exp(terms - largest))) - log(length(values)) -
        log(bandwidth) - 0.5 * log(2 * pi)
}

.bh_sddr_bandwidth <- function(values, supplied, repeated) {
    if (!is.null(supplied)) return(list(value = supplied, status = "explicit"))
    if (repeated) return(list(value = NA_real_, status = "automatic_bandwidth_blocked_repetition"))
    # This is a reference smoothing heuristic, not an independence or ESS claim.
    spread <- min(stats::sd(values), stats::IQR(values, type = 7) / 1.349)
    value <- 0.9 * spread * length(values)^(-0.2)
    if (!is.finite(value) || value <= 0)
        return(list(value = NA_real_, status = "degenerate_reference_bandwidth"))
    list(value = value, status = "raw_count_reference_heuristic_dependence_not_adjusted")
}

.bh_sddr_density <- function(values, bandwidth, factors) {
    widths <- bandwidth * factors
    if (any(!is.finite(widths)) || any(widths <= 0))
        return(list(status = "unrepresentable_bandwidth_grid"))
    logs <- vapply(widths, function(h) .bh_sddr_log_kernel(values, h), numeric(1))
    densities <- exp(logs)
    list(status = if (all(is.finite(logs))) "computed" else "unrepresentable_kernel_distance",
         estimates = data.frame(multiplier = factors, bandwidth = widths,
             log_density_at_zero = logs, density_at_zero = densities,
             density_representable = is.finite(densities) & densities > 0))
}
