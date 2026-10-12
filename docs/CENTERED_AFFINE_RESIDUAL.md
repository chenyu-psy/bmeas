# Centered task models with a bounded affine residual layer

Version 0.0.16.9005 supports an explicit experimental execution profile for the existing `sample_centered` person law, `task_cholesky` augmentation and `ordered_blocks` common correlation coordinates with a separate fixed or one-label affine residual correlation matrix. The public declaration syntax is unchanged. Prepare with `fit=FALSE`, inspect `check_model()` and `get_prior()`, then request `execution="experimental"` only when the stated limits are acceptable.

For four targets with one residual correlation shared by all six pairs, the core declaration is:

```r
f <- bmf(
  A = ~x1 + x2, B = ~x3 + x4,
  loadings = list(fixed = list(list(
    value = 1, cells = list(A = ~x1 + x2, B = ~x3 + x4)))),
  cor = list(default = "free", coordinates = list(
    type = "ordered_blocks", blocks = list(first = ~A, second = ~B))),
  residual = list(targets = ~x1 + x2 + x3 + x4,
    cor = list(default = "zero", equal = list(shared = list(
      ~x1:x2, ~x1:x3, ~x1:x4, ~x2:x3, ~x2:x4, ~x3:x4)))),
  person = list(distribution = "sample_centered", ids = as.character(1:4)),
  augmentation = list(type = "task_cholesky", targets = ~x1 + x2 + x3 + x4)
)
s <- bmeas(f, data, subject = "person", fit = FALSE,
  prior = c(set_prior("normal(0,.2)", "cor_raw", block = "second"),
            set_prior("lkj(2)", "cor", layer = "parameter")))
check_model(s)
get_prior(s)
```

Here `data` is a wide data frame with person IDs 1 through 4 and numeric x1 through x4. The roster and task order define the centered white axes and must be intentional. Changing task order may induce a covariance-dependent transformation of white coordinates; saved states cannot simply be relabeled.

Raw common B cells keep their direct Normal priors. Common R entries are derived quantities and are not marginal-correlation primitives. A free residual rho has one scientific label and structured aliases; its interval coordinate is separate. Fixed residual matrices have no correlation parameter, density or Jacobian. MGS remains a joint vector map. The output inventory counts unique coordinates rather than alias cells. Task white basis axes, centered N-person white outputs and task deviations keep separate identities. Common and residual generative person scores are unsampled under task augmentation. Detached summaries retain these roles without contacting a backend.

The independent deterministic gate covered 14 configurations with 330 primary derivatives, 234 normalized likelihood rows, 7,754 scalar outputs and 160 empty fields. Both same-state threading controls matched all 58 repeated derivatives exactly. Earlier reference states also replayed exactly. The largest new derivative difference was 2.99e-15 and row difference 4.45e-15. All 30 calls succeeded without retries; actual stanc --O0 and model translation-unit C++ -O0 were verified. These checks establish bounded arithmetic evidence. They do not establish HMC behavior, finite-data recovery, arbitrary-composition equivalence or general cognitive identification.

The scoped Gaussian control has four centered people, four targets, fixed unit loadings/component SDs, one raw common correlation, four positive residual SDs, one residual rho shared by six pairs and four positive observation SDs. Its complete population covariance has an explicit inverse through cross-person covariance blocks. This does not promote generic centered identification or prove posterior propriety. The matching iid free-rho model has a scaling ambiguity; that argument does not automatically apply to fixed nonzero residual correlations.

The task Gram uses the raw common square factor directly. Its existing finite/positive-Cholesky and representable-operand checks remain in force. The affine residual matrix has its explicit numerical SPD certificate. No universal task-covariance error certificate is claimed. Optional rounded matrix exports can have narrower numerical reliability than the required task factor.

Execution still requires eta >= 1 for free final-rho layers and all ordinary prior, initialization, conclusive identification/propriety and resource guards. A mathematically proper eta below one does not gain runtime qualification. Same-common-layer raw-B plus affine constraints is invalid; general multi-label affine charts remain inspection-only. Other legal person/augmentation combinations keep their source-only status. The larger original reference fixture does not grant a resource allowance or broader numerical qualification; main fitting requires the separate explicit policy described in RESOURCE_POLICY.md.
