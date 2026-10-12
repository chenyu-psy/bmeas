\name{bmeas-package}
\alias{bmeas-package}
\alias{make_m3_simulator_code}
\alias{compile_m3_simulator}
\alias{m3_simulator_data}
\alias{m3_probabilities}
\alias{simulate_m3_responses}
\alias{binomial_model}
\alias{block_prior}
\alias{cfa_parameter_prior}
\alias{cfa_structure}
\alias{check_measurement}
\alias{compile_measurement}
\alias{component_covariance_structure}
\alias{component_prior_set}
\alias{correlation_draws}
\alias{csmu_gpc}
\alias{data_field}
\alias{demo_cognitive_wiring_spec}
\alias{demo_gaussian_cfa}
\alias{demo_measurement_spec}
\alias{demo_mixed_spec}
\alias{density_route}
\alias{evaluate_component_structure}
\alias{fit_measurement}
\alias{gaussian_model}
\alias{half_normal}
\alias{lkj}
\alias{logistic}
\alias{lognormal_model}
\alias{m3_cs_model}
\alias{m3_model}
\alias{m3_reference_bindings}
\alias{m3_reference_domain}
\alias{make_stan_code}
\alias{measurement}
\alias{missing_policy}
\alias{mixed_prior_set}
\alias{model_capabilities}
\alias{model_resource_manifest}
\alias{new_observation_model}
\alias{normal}
\alias{observation_ir}
\alias{observe}
\alias{parameter_binding_map}
\alias{parameter_binding_table}
\alias{parameter_covariance}
\alias{parameter_keys}
\alias{parameter_prior_set}
\alias{parameter_spec}
\alias{prior_preset}
\alias{prior_set}
\alias{prior_table}
\alias{reference_priors}
\alias{reference_run_budget}
\alias{reference_structure}
\alias{register_routed_adapter}
\alias{register_stan_adapter}
\alias{response_schema}
\alias{run_config}
\alias{sdm_model}
\alias{sdm_reference_model}
\alias{shared_subjects}
\alias{stan_constant}
\alias{stan_data}
\alias{stan_density}
\alias{stan_resource}
\alias{stan_symbol_map}
\alias{structure_ir}
\alias{student_t}
\alias{subject_crosswalk}
\alias{targets}
\alias{task_parameters}
\alias{verification_outputs}
\title{Bayesian Hierarchical Measurement Modeling}
\description{
bmeas combines flexible response models, including standard statistical
and cognitive-model likelihoods, with user-specified latent measurement
structures. Parameters and latent measurement quantities are estimated jointly
within the documented execution scope. Current numerical evidence, scientific
identification and sampling qualification have separate explicit limits.
}
\details{
The main modeling entries are \code{bmf} and \code{bmeas}; \code{bhmf} and \code{bhmm} remain transitional aliases. The helper APIs below retain their existing names and scopes.
\code{observation_model} objects declare typed data, parameter scales,
likelihood resources, provenance and independent capabilities.
\code{gaussian_model}, \code{binomial_model}, and \code{lognormal_model}
share this interface. Gaussian, binomial-logit and lognormal blocks can be combined under one shared CFA structure.
\code{new_observation_model} declares custom contracts. \code{register_stan_adapter} adds a restricted typed, same-task direct likelihood compiler with pinned \code{stan_resource} files and optional function wrappers/constants. Scalar or fixed-width numeric-array fields are supported. \code{density_route}, \code{register_routed_adapter}, and \code{parameter_binding_map} provide explicit branch-specific formal-argument bindings to target keys or fixed predictors. Registration does not invent prediction methods.

\code{observe} binds explicit data columns, units, subject/task identifiers,
and optional observation IDs and source provenance. Repeated subject/task
rows require explicit unique observation IDs. Missing response rows require
\code{missing_policy(response = "observed_only")}; omitted auxiliary values
are errors. \code{shared_subjects} explicitly declares an ID namespace;
\code{subject_crosswalk} maps model/local_id/person_id. No row-order joining,
automatic scaling, deduplication or imputation is performed.

In the current development frontend, \code{observe(parameters=...)} accepts a named
list of block-level variation choices: \code{"subject_task"}, \code{"task"}, or
\code{list(fixed=value, scale="natural")}. Fixed scale can also be predictor or
received. A scalar broadcasts over the complete logical task domain; a named
vector must match that domain exactly. This changes variation, never the
observation model's scientific role or link. \code{parameter_spec(role="predictor")}
allows a custom task/fixed default to retain its predictor role. The optional
role is absent from older canonical model contracts. The model's \code{levels}
field supplies one executable default, not a menu of permitted overrides.
Task/fixed binding uses the same unified Stan lowering. Its bounded numerical
checks cover Gaussian and pinned M3-CS/SDM configurations; other compositions
retain their explicit qualification limits. No sampling or inferential
validation was added. Fully fixed models refuse sampling; zero-parameter
CmdStan log_prob is unsupported, so fixed-only validation uses normalized
generated row likelihoods rather than fixed_param lp__.

\code{targets} creates block::parameter::task keys. \code{cfa_structure}
requires three or more indicators per factor, positive anchors, unit factor
variances and explicit task-level sigma/sdlog nuisance parameters for Gaussian/lognormal. Binomial has no free observation scale. Task-specific
latent residuals are fixed to zero. Factor correlations and response errors
are distinct. \code{parameter_covariance} is a declaration only; the generic
direct covariance compiler and prior resolver are not implemented.

\code{prior_set} requires normal intercepts, zero-centered normal loadings,
half-normal sigma and LKJ correlations. For mixed/non-Gaussian models, \code{mixed_prior_set} requires a \code{block_prior} for every block, with matching observation units and response/logit/log_response predictor scale. Gaussian/lognormal blocks require a positive observation SD prior; binomial blocks prohibit it. The only preset,
\code{gaussian_cfa_standardized_v1}, requires explicit standardized units.
\code{parameter_prior_set} and \code{cfa_parameter_prior} provide exact-key predictor-scale normal, logistic or Student-t intercept priors and normal loadings for registered models. \code{model_resource_manifest} records external hashes, source and unresolved license status. The package never standardizes data. \code{prior_table} displays all
resolved priors, scales and sources.

\code{measurement} and \code{check_measurement} perform deterministic
contract checks, not statistical identification. \code{stan_data},
\code{stan_symbol_map}, and \code{make_stan_code} expose inspectable products.
None compiles or samples. \code{compile_measurement} explicitly compiles.
For the generic registry/component compiler, \code{make_stan_code} and
\code{compile_measurement} accept \code{threaded = TRUE}; default is false.
\code{stan_data(spec, reduction = "serial", grainsize = 1, threaded = FALSE)}
controls the explicit runtime reduction. A threaded binary supports both
serial and \code{reduce_sum_static} branches using one normalized row kernel.
The historical \code{run_config} route accepts the same reduction and grainsize, with at most2
threads per chain and total CPU budget2. Main-API resource planning is separate; see \code{execution_plan} and docs/RESOURCE_POLICY.md. Ordinary optimized basic templates
reject threading. Joint observation rows stay intact, shared person parameters
are read-only, and priors/hierarchy are outside the reduction. The generic
compiler retains length-one array/vector ranks for JSON serialization.
Threading is numerically checked on tiny M3/component and M3-CS+SDM fixtures;
no useful speedup, bitwise equality or general scaling is established.
\code{fit_measurement} requires a separate \code{run_config} and explicit
cache/output directories. An explicit \code{reference_run_budget} allows only the bounded27-indicator/15-common-dimension/8-person/72-row reference scope; the ordinary12-indicator protection remains. \code{verification_outputs} opts into pointwise densities only for explicit checks, without running LOO. The historical one-argument \code{individual_parameter_summary} exports predictor-scale plugin summaries with uncertainty and observation coverage. Main-API fits use the separate explicit scales/budget interface documented in \code{individual_parameter_summary}. These historical APIs retain their tiny smoke-test limits. Main-API fitting instead requires an explicit finite resources policy, with the old tiny policy available as the named development preset. Wall-time and process-memory enforcement are not implemented.

\code{summary} reads only cached diagnostic results. Empirical fit,
identification, admissibility and computation have separate statuses.
\code{parameter_summary} returns cached posterior summaries; explicitly
requesting \code{correlation_draws(space = "factor")} reads draws.
No PPC, LOO or log-likelihood arrays are computed by default. Historical measurement_fit population-level PPC for basic-family simple-CFA structures is available via \code{assess_measurement_fit}, with \code{diagnostic_budget}; \code{ppc_plan} describes its cost before execution. It redraws correlated person effects, preserves design and trials, and retains only aggregate discrepancies. \code{fit_checks} reads the cached table. Tiny replication intervals are descriptive screening only.
No external reference source, DDM weights or private data are bundled. The G/P/C blueprint is newly generated from numeric designs; it is not a bundled research Stan program. \code{m3_cs_model} supports the explicitly limited CS observation branch, and \code{sdm_reference_model} the pinned original SDM likelihood/200-order grid. The narrow CS-only and SDM adapters require external exact-version resources, have no standalone simulator/PPC, and do not import a reference G/P/C hierarchy. The six-parameter \code{demo_cognitive_wiring_spec} is synthetic API wiring only. \code{m3_model} now provides all three CS/MU branches with an explicit27-key domain from \code{m3_reference_bindings}. \code{reference_structure}/\code{csmu_gpc} and \code{reference_priors} select the exact Exp1 component blueprint and prior roles. Generic \code{component_covariance_structure} and \code{component_prior_set} are assembled through separate \code{structure_ir} and \code{observation_ir}; they do not dispatch a fixed research Stan file. \code{evaluate_component_structure} supports deterministic numeric checks.
For the exact bounded full-M3/reference-component combination,
\code{make_m3_simulator_code} and \code{compile_m3_simulator} expose/compile a
separate hash-pinned auxiliary probability/RNG model with no fitted parameters.
\code{m3_simulator_data} binds a finite predictor matrix through the same named
route arguments; \code{m3_probabilities} explicitly evaluates probabilities and
\code{simulate_m3_responses} explicitly generates one fixed-total count matrix.
\code{assess_measurement_fit} accepts this explicit \code{simulator} argument;
\code{ppc_plan} checks at most10 replications and N<=8/O<=72/J27/K15 scope.
Fresh centered same-N groups preserve the original finite-N scaling and shared
person covariance; fitted latent effects are not reused. Statistics pool counts
and totals within person/route/content, then compare active-category mean/SD
and36 paired correct-rate task covariances. Zero-total people are excluded from
rates, not filled with zero; insufficient pairs are not assessed. Only aggregates
and a small replication-by-statistic matrix keyed to table statistic_id are cached; legacy aggregate samples are explicitly unavailable without automatic replay. Supported RNG state is restored (Box-Muller/user-supplied kinds are rejected before mutation), failed attempts invalidate predictive caches,
and upstream poor-fit warnings remain. Conditional-person and SDM prediction
remain unsupported. Tiny replication intervals are descriptive screening, never
a mechanical pass/fail verdict or evidence of model validity.
}
\examples{
spec <- demo_measurement_spec()
check_measurement(spec)
prior_table(spec)
head(stan_symbol_map(spec))
mixed <- demo_mixed_spec()
check_measurement(mixed)
prior_table(mixed)
# No sampling or compilation occurs in these examples.
}
\seealso{\code{stats::Normal}}
