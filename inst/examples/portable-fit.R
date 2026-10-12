# INERT EXAMPLE: every operation is commented to prevent accidental file work.
# Run export in an environment where the unchanged original fit seal verifies,
# using a bmeas version that provides portable_fit(), before upgrading/moving.
# A legacy producer without portable_fit() needs a separately supplied, reviewed
# exporter in that original environment. This example does not restore or
# fabricate an old namespace and does not repair an already incompatible seal.
#
# The allowance covers canonical exported context, including saved cache data.
# Original-seal serialization, R/native overhead and saveRDS are separate costs;
# these limits are not hard RAM or wall-time guarantees.
#
# portable <- bmeas::portable_fit(fit, budget = list(
#   max_context_bytes = 8 * 1024^2,
#   max_context_nodes = 1000000,
#   max_depth = 64
# ))
# saveRDS(portable, "fit-postprocessing.rds", version = 2)
#
# Transport this detached RDS to a compatible environment with bmeas installed.
# Read only an RDS you trust. CSV files remain separate; original paths and their
# order are unchanged. Moving the RDS does not copy, relocate or restore CSVs.
#
# library(bmeas)
# portable <- readRDS("fit-postprocessing.rds")
# summary(portable)  # saved reports only; this call does not verify integrity
# bmeas::diagnostic_parameters(portable)  # saved structural metadata only
#
# Supported active interfaces are refresh_diagnostics(),
# individual_parameter_summary() and assess_measurement_fit(), subject to their
# existing scientific domains, source requirements and operation-specific
# finite budgets. Each verifies the immutable context before active work.
# Choose explicit finite source, read, numeric-work and operation limits for the
# actual files before requesting an active call. This example requests none.
# Its illustrated work is one export, one save, one load and two passive or
# metadata reports; it performs zero CSV acquisitions, fits or simulations.
#
# This is post-processing portability only. It does not provide refitting,
# code/data generation, model rebuilding, callback/backend execution or source
# path rebinding. A matching digest is integrity evidence, not proof of original
# sampler output, convergence or scientific validity. Retained adverse history
# remains visible after fresh cache updates.
