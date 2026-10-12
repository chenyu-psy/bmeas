# Centered task models with selective final-marginal correlations

This experimental execution profile composes the existing `sample_centered` roster and `task_cholesky` augmentation with an ordinary zero/free graph on final common marginal correlations. Unlike the separately qualified ordered raw-B law, this graph can retain orthogonality within parameter groups and set selected cross-group pairs to zero while estimating the remaining pairs jointly.

Use the existing correlation grammar and omit `cor$coordinates`:

```r
cor = list(default = "free", zero = list(
  ~a_G + a_P + a_C, ~c_G + c_P + c_C,
  ~a_P:c_C, ~a_C:c_P))
```

For six scalar components this leaves seven free cross-group entries, with the other eight off-diagonal entries fixed to zero. The component names carry no automatic G/P/C interpretation. Declare membership, contrast bases and variance-share owners explicitly through the existing `bmf()`/`component()` interface. A vector basis can expand into several scalar coordinates, so count the resolved coordinates rather than names.

The common layer has one joint determinant density on its free final marginal entries and the existing joint MGS chart Jacobian. It has no independent Normal B priors and no scalar MGS-input-to-rho association. Residual MGS, fixed SPD and one-label affine layers retain their own measures and unique coordinates. Fixed residual correlation does not fix its row SDs. The scientific prior differs from ordered raw B; a matching covariance alone does not show prior or posterior equivalence.

The new source uses the same generic correlation assembly as bounded affine layers, with mode0 and inactive data for ordinary zero/free graphs. Existing MGS basis, prefix-Cholesky, diagonal-identity and reconstruction guards remain mandatory. Its factor enters the normalized task Gram directly. A positive task Gram cannot excuse failure of the common correlation chart. Existing required-operand and optional-report rules are unchanged.

Output inventories include the common E-vector and (K+1)-by-K chart matrix, with no invented B coordinates. Scientific final-rho rows, fixed constants and expanded aliases remain separate. Task white axes, centered scores and task deviations retain their roster/target keys; common and residual generative person scores remain unsampled under task augmentation.

A six-target signed-owner Gaussian control has a scoped population covariance inverse: cross-person blocks recover the stable covariance, within-owner contrasts identify strictly positive shares, invertible signed loading blocks recover the seven cross-correlations, and within-person diagonals recover observation noise. This relies on three distinct scalar unit-SD parts per owner, shared intercept-only fractions, independent residuals and a complete known centered roster. A separate fixed-identity control covers one-label residual correlation using a known-zero common edge. Neither statement gives a generic centered/cognitive identification or posterior-propriety certificate; known G/P covariance shear still rejects its applicable model.

Only effective ordinary zero/free common graphs belong to this increment, including harmless explicit fixed-zero rules. Genuinely fixed-nonzero or equal common correlations, general multidimensional affine charts and other augmentation combinations retain their previous limits. Execution requires an explicit `execution="experimental"` request with an explicit resource policy (see RESOURCE_POLICY.md). Generic centered/cognitive identification remains unassessed unless a scoped certificate applies; `exploratory=TRUE` records an explicit request to proceed where that existing policy requires it. Numerical prior restrictions and proven nonidentification/impropriety still take precedence.

Independent deterministic checks cover 14 configurations, 412 primary derivatives, 264 normalized rows, 10684 scalar outputs and 194 empty fields. Two same-state threading controls cover 81 repeated derivatives. The seven-edge S17 control retains a nonzero likelihood derivative at an exactly zero early MGS row, and the K0 same-law bridge agrees exactly with retained old native states. Required fields and rows agree within 5.33e-15, derivatives within 2.66e-15 and all emitted fields including optional log magnitudes within 1.87e-14. Structural-zero log fields use an explicit support mask and the existing 1e-10 reconstruction tolerance; a finite cancellation residue is not presented as a changed scientific zero. These checks establish bounded arithmetic behavior, not HMC, finite-data recovery or general inferential equivalence. No posterior sampling was performed.

Accepted pre-affine historical contexts without structured correlation primitive/cell maps keep their existing scientific inspection. The new summary/cache profile requires an explicit rebuild; E_CORRELATION_REPORT_REBUILD prevents silent omission of marginal-rho rows. Old prepared objects are never resealed automatically.
