# bmeas

**Bayesian hierarchical measurement modeling in R.**

bmeas combines response models with latent measurement structures. It estimates
individual parameters and population relationships jointly, so uncertainty in
response-model parameters can be carried into the measurement model. The package
supports explicitly documented subsets of statistical and cognitive response
models, factor structures, and postprocessing workflows.

## Features

- Specify measurement structures with `bmf()` and prepare models with `bmeas()`.
- Inspect model support, prior specifications and execution plans before fitting.
- Read saved fit reports and parameter summaries without automatically acquiring
  posterior files or running new computations.
- Request supported diagnostics and predictive checks with explicit resource
  and consumer budgets.
- Map joint population covariance draws to structural coefficients through an
  analytic backend or a narrowly supported optional lavaan backend.

## Installation

R **4.1.0 or later** is required. The current version is an experimental
**0.0.47.9000** development snapshot. A stable GitHub Release and CRAN distribution
are not yet available. Install the current development branch with:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("chenyu-psy/bmeas", ref = "work/current-0.0.47.9000")
```

Preparation and model inspection require only the standard R packages `stats`
and `utils`. Optional workflows use `posterior`, `jsonlite`, `lavaan` or
`cmdstanr`. Native fitting additionally requires CmdStan and a supported C++
toolchain; see the [official CmdStanR installation guide](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

Provider support is version-specific. The documented deferred fitting profile
requires CmdStanR **0.9.0** and CmdStan **2.40.0** on supported POSIX platforms.
Selected diagnostics use posterior **1.7.0**, and the optional structural lavaan
backend requires lavaan **0.7.2**. Installing a newer dependency does not establish
compatibility. Consult the relevant workflow documentation before execution.

## Quick start

Prepare a small Gaussian measurement model without compilation or sampling:

```r
library(bmeas)

data <- data.frame(
  person = 1:4,
  x1 = c(.2, .8, .3, 1),
  x2 = c(.7, .6, -.3, .1),
  x3 = c(-.2, .6, .9, .4)
)

spec <- bmeas(bmf(G = ~ x1 + x2 + x3), data,
              subject = "person", fit = FALSE)
check_model(spec)
get_prior(spec)
```

These data illustrate the interface. They do not establish model identification,
adequate priors or scientific validity. Fitting is a separate explicit operation
that requires a supported model and a finite resource policy.

## Documentation

The [package homepage](https://chenyu-psy.github.io/bmeas/) will provide the API
reference generated from the package's Rd documentation once approved package
source reaches main. The current public homepage describes the development
status; the complete development API is available in a separate local preview.

- [Getting started](vignettes/articles/getting-started.Rmd)
- [Current capabilities](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/CURRENT_CAPABILITIES_zh.md) and [workflow guide](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/CURRENT_WORKFLOW_zh.md)
- [Resource policies](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/RESOURCE_POLICY.md)
- [Diagnostics](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/DIAGNOSTIC_REFRESH.md) and [predictive checks](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/BASIC_FAMILY_PPC.md)
- [Structural postprocessing](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/STRUCTURAL_MAPPING_zh.md)
- [Documentation index](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/README.md), [migration](https://github.com/chenyu-psy/bmeas/blob/work/current-0.0.47.9000/docs/MIGRATION.md) and [release notes](NEWS.md)

## Current limitations

The package is experimental. Interface availability, native execution support
and inferential validity are separate claims. Use `check_model()` and the
capability notes to inspect the applicable profile; general model combinations
and broader native compatibility are not established.

Formal Bayes factors and density MCSE are **not implemented**. Experimental
point-density diagnostics do not provide a formal Bayes factor. Deferred fit
reports distinguish unavailable diagnostics from successful computation, and
synthetic engineering tests do not qualify a scientific analysis.

## Development

Changes follow feature → develop → main through explicitly approved pull
requests and required checks. The current source remains on the development
branch; package publication requires an approved develop-to-main merge.
