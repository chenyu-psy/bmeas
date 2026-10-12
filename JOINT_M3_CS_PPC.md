# Two-block pinned M3-CS predictive checks: live integrated capability

This increment admits exactly two canonical, pinned M3-CS blocks in a live main-API fit. It does not add mixed basic/cognitive, SDM, EZ, arbitrary adapters, routed full M3, or three-plus cognitive blocks. Existing single-block output shapes and saved-reader capability remain unchanged.

## Joint law and response simulation

A selected posterior row supplies the complete aligned joint state. Conditional checks scatter its saved eta entries into the existing full person/target map. Population checks generate one complete iid common-plus-residual state or one same-N sample-centered task-Cholesky state, including original unobserved roster members. Both blocks then read that same eta. They never independently regenerate common factors, people, or selected posterior states.

The two blocks' rows are concatenated, in saved block and row order, into one existing `bmeas_m3cs_rng_v2` fixed_param invocation per replication. The emitted Stan code and pinned resource are unchanged. The kernel calls multinomial_rng separately for each row; responses are conditionally independent given the shared eta and row design. It does not reseed by block or clone response noise. Counts are split back into block-named matrices using saved row offsets; empty blocks retain zero-row matrices.

Public `m3cs_probabilities()` returns a block-named list of matrices for two-block inputs, and the original matrix for single-block inputs. Public joint `m3cs_simulator_data()` carries a `row_map` attribute containing simulator_row, block, block_row, observation_id, person_key, person_index, and task. The actual native payload uses the private data function and contains no extra map metadata. The original single-block data return is unchanged.

## Statistics and budgets

A response domain is block + task, so identical task labels in different blocks never collide. Counts from repeat rows are pooled within each person and domain; positive-total people receive equal weight. Each domain has five category means and five sample SDs. Every distinct domain pair has all 25 category-rate sample covariances, using exactly the positive-total overlapping people and denominator overlap-1. No-overlap, one-person, zero-total, and degenerate cases retain their original explicit statuses.

For T total block/task domains the complete table has 10*T + 25*T*(T-1)/2 statistics. No category pairs are silently omitted. Budgets cover both blocks' total observations and all ten response cells, joint latent generation, required posterior columns, complete statistic expansion, paired-person terms and declared working buffers. Streaming acquisition retains balanced selected joint states and validates the complete source under the existing finite policy.

As before, an execution failure clears the current table and replicate matrix; it retains the attempted phase, completed-replication count and available source receipt. The caller RNG is restored on both success and failure. Assessment never implicitly compiles, refits, or launches HMC.

## Saved compatibility and qualification

The 0.0.34.9000 candidate adds a distinct producer/reader profile for newly exported two-block portable/archive PPC; see SAVED_JOINT_M3CS_PPC.md. Historical general-profile two-block capsules remain explicitly refused. Legacy immutable producer profiles, original seals, historical resource paths, the single-block reader predicate and legacy saved capabilities are not changed or resealed. Structurally identical passive v3 plan/v2 result tables remain readable; that is not an execution capability grant.

The initial frozen source checkpoint was R-only: exact shared-factor/Helmert oracles, deterministic runner/controller doubles, row-map and statistics checks, typed refusal/budget checks, and regression coverage. The response double verifies invocation, row RNG transaction and output plumbing, not native CS probabilities. After that source freeze, a separately approved cached-native seam completed exactly four fixed_param calls (iid/centered × conditional/population), 280 count and 280 probability values, with maximum probability error 2.220446e-16. Synthetic joint states are not fitted posterior draws. No compilation, posterior sampling, model recovery, real-data analysis, calibration or population statistical independence study was performed. The integrated release does not repeat native execution; its exact installed/source-package check status is recorded in the accompanying VALIDATION.md.
