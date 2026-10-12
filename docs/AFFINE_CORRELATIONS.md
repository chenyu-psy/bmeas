# Fixed and equal final marginal correlations: source increment

Current 0.0.33.9000 supplement: the iid canonical-Gaussian common and/or residual-product profile now admits exact zero-separated one-label blocks under explicit experimental execution. See AFFINE_PRODUCT_CORRELATIONS.md for the complete scope, prior and historical arithmetic evidence. This extends the older fixed/one-label scope described below without granting general connected multilabel execution. Product saved-value support has a separate bounded profile described in PRODUCT_FIT_ARCHIVES.md.

Version 0.0.15.9002 retains the reviewed fixed/equal marginal-correlation declarations and adds inspectable Stan/data generation for fully fixed strict-PD matrices and one unique free marginal label per layer. The iid/common-plus-residual final-marginal route has bounded fixed-state native evidence and requires explicit experimental execution for new affine layers. Ordinary zero/free graphs retain the same MGS coordinate map and their existing execution policy, including fixed-zero no-op declarations.

```r
# One known correlation. Only one simultaneous loading-sign anchor is valid.
m <- bmf(A = ~x1 + x2, B = ~x3 + x4,
  cor = list(default = "zero",
    fixed = list(list(pairs = ~A:B, value = .3))))

# One unknown correlation shared by two pairs, with one joint prior coordinate.
m <- bmf(A = ~x1 + x2 + x3, B = ~x4 + x5 + x6,
  C = ~x7 + x8 + x9, D = ~x10 + x11 + x12,
  cor = list(default = "zero",
    equal = list(shared = list(~A:B, ~C:D))))
s <- bmeas(m, data, subject = "person", fit = FALSE)
model_table(s, "correlation_primitives")
model_table(s, "correlation_affine_cells")
model_table(s, "correlation_domains")
model_table(s, "orientation_sign_system")
get_prior(s)
check_model(s)
```

Each fixed/equal selector resolves exactly one scalar pair. Select a basis column explicitly, for example `~P["linear"]:C`. Residual correlations use the same fields inside `residual$cor`, with existing qualified target selectors. Labels are scoped by layer: a common `shared` and residual `shared` are separate unknowns. A named equality class needs at least two distinct pairs. Reversing a pair does not create a second pair.

Fixed values and structural zeros propagate through transitive equality classes. Conflicting constants or an explicit free edge that becomes fixed give an error. All aliases and captured selector text remain inspectable. No live formula environment or annotated syntax tree is retained by the added correlation-rule table.

The matrix is represented as a fixed constant plus sparse unique-label directions. Fully fixed matrices allocate no correlation primitive or prior. For one label, the domain helper assesses the complete strict-PD interval; it does not use prefix-only checks. Domains with more labels remain inspectable. Numerical inability to certify a domain or transformed state is distinct from proven strict-PD infeasibility. No inner-domain replacement, clipping or jitter is applied. Fixed correlations of +/-1 are outside this strict-PD capability; that statement does not exclude singular PSD scientific models in a future capability.

The existing `set_prior("lkj(2)", "cor")` spelling denotes one restricted determinant kernel on the unique-label measure for a new affine layer. It is not an unconstrained LKJ normalizer and is not multiplied once per aliased edge. `correlation_prior_geometry` records its measure, domain and normalizability separately from posterior propriety. Positive eta below one remains inspectable; its normalizability depends on the actual constraints and execution is unqualified. For two independent pairs sharing rho, the prior is proper exactly when eta > 1/2. Fully fixed layers do not consume correlation-prior rules.

Fixed nonzero correlations and equality constraints alter the allowable factor-sign symmetries. Inspection exposes the GF(2) basis and independent loading-anchor characters. The four-factor shared-pair example has three independent sign anchors; a fictitious fourth anchor would change the model. No analogous sign folding is applied to positive-SD residual correlations.

Gaussian diagnostics count unique correlation labels and retain covariance terms involving a fixed nonzero correlation and free loadings. In particular, fixed residual correlations can identify three stable residual SDs and three observation SDs without repeats, and a shared residual correlation can need only one repeated target. These bounded local checks do not imply global identification. Exact scoped basis-reflection and crossed-marker aliases remain hard identification negatives.

Fresh declarations with nonempty correlation rules use an explicit affine schema. Compatible old reference-law contexts are inspected without mutation or fingerprint rewriting. New affine execution requires `execution="experimental"`, fixed eta >= 1, a resolved fixed/one-label domain and all ordinary prior, identification, propriety and resource guards. `exploratory=TRUE` does not bypass a conclusive proof or an unavailable chart. The additional supported conjunction is sample-centered people, task-Cholesky augmentation and ordered raw common coordinates with a separate fixed or one-label residual affine layer; it also requires explicit experimental execution and has separate bounded evidence in CENTERED_AFFINE_RESIDUAL.md. Centered/task models with effective ordinary zero/free final-marginal common MGS graphs can also use these residual layers under the separate experimental profile in CENTERED_FINAL_MARGINAL.md. Genuine fixed/equal common affine constraints in centered/task mode and other legal combinations remain source-only, even with either opt-in. Ordered raw B and affine constraints cannot be combined on the same common matrix. Old native evidence and released package artifacts remain historical evidence for their exact earlier source and scope.

The computational one-label coordinate is unconstrained z, with rho = midpoint + half_width * tanh(z). Initialization uses z=0 and the actual scientific midpoint; it does not assume rho=0 is feasible. Each free layer adds its interval Jacobian once and its joint restricted determinant kernel once. Fixed layers add neither. The generated helper recomputes a fixed factor from the authoritative matrix and certifies the actual transformed matrix; an uncertain or saturated numerical state is rejected without moving the scientific boundary.

Prior normalizability, source capability and execution evidence are separate. Eta below one can have a proper restricted prior for some constraints while remaining outside the initial execution subset. General multilabel affine declarations remain inspectable without a supplied multidimensional HMC chart. The new bmeas_ helper prefix is reserved, alongside the existing bhmm_ prefix, to prevent custom observation exports from shadowing generated functions.

The independent retained native gate covers 32 configurations, 766 primary derivatives, 594 normalized likelihood rows, 10 exact earlier-state replays and two same-state reductions. Maximum discrepancy was 2.03e-13. All 64 evaluations succeeded without retries. Active whole-stage compilation used 65.2036 seconds and deterministic evaluation 6.4157 seconds; a capacity/restart gap is excluded from active compute. The post-restart frozen hashes and binaries were verified before the remaining unrun batch. No MCMC was performed. These checks establish bounded arithmetic, not posterior recovery or broad HMC adequacy.

Posterior summaries contain one scientific rho row per unique label, with its structured aliases. Expanded matrix summaries retain every cell separately; fixed constants receive no invented uncertainty diagnostics. Component covariance can remain uncertain under a fixed nonzero rho when a component SD is free. Computational chart coordinates are documented separately from the final marginal labels.

For an existing zero/free MGS layer mixed with an affine layer, the computational raw vector is transformed jointly. A scalar marginal rho is not labeled as one raw coordinate. Only the new one-label interval chart records its direct z association.
