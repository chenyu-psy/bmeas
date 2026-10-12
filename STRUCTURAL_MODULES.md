# Structural postprocessing: responsibility boundaries

This optional layer operates after measurement fitting. It does not introduce a second data likelihood, sampler or BF computation.

- `R/frontend-population-draws.R`: external population contract, shared limits and validation, including native evidence revalidation.
- `R/frontend-population-component.R`: retained native posterior reader, full parameterization/scale/orientation and provenance bridge.
- `R/frontend-structural.R`: guarded unit conversion, analytic Cholesky regression/Schur complement, public backend dispatch and complete-mapping summaries.
- `R/frontend-component-prior-scope.R`: scientific admission for the narrow native prior family.
- `R/frontend-component-prior.R`: bounded full-K joint prior generation and RNG lifecycle.
- `R/frontend-component-prior-context.R`: prior context/receipt reconstruction and identity validation.
- `R/frontend-structural-lavaan-spec.R`: restricted model parser, stable aliases, explicit parameter table, version pin and immutable numerical specification.
- `R/frontend-structural-lavaan.R`: bounded fitting, named coefficient extraction, option/parameter verification and retained per-draw output slots.
- `R/frontend-structural-lavaan-diagnostics.R`: convergence/admissibility/boundary acceptance and guarded input-SD standardization.

The public population and structural API remains in `man/structural_draws.Rd`; native prior admission has its own help/documentation. Tests retain separate contract, native reader, prior, optional backend and inherited callback-capture responsibilities.

Shared validation is reused rather than reimplemented inside the optional backend. The backend conservatively requires the analytic eligibility checks; this can reject extreme-scale inputs that another numerical method might handle, and is explicitly documented. Mapping specifications, numerical thresholds, source evidence and scientific qualifications are separate fields. No convergence flag upgrades upstream identification or inference status.

The .38 organization moves existing structural definitions without changing their parsed bodies, except the explicit optional-backend dispatch extension. Typed missing values and mathematical comments are retained. Earlier frozen source/evidence remains untouched.
