# bmeas

Experimental Bayesian hierarchical measurement modeling in R.

## Install the current development snapshot

This version is **0.0.47.9000**, an experimental development snapshot. There is no
GitHub Release yet. Install this dedicated version branch with R >= 4.1.0:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("chenyu-psy/bmeas", ref = "work/current-0.0.47.9000")
```

For preparation and inspection, only R's standard `stats` and `utils` packages
are required. Optional features use `posterior`, `jsonlite`, `lavaan`, and
`cmdstanr`. Native fitting additionally needs CmdStan and a supported compiler.
Provider support is narrow and version-specific: the documented deferred profile
requires CmdStanR 0.9.0 and CmdStan 2.40.0 on supported POSIX platforms; the optional
lavaan backend requires lavaan 0.7.2. Installing newer versions does not establish
compatibility. Start with `fit = FALSE` and read the relevant capability notes
before fitting. No installation command here installs CmdStan or runs a sampler.

Formal Bayes factors and density MCSE are not implemented. Synthetic engineering
tests and mocked providers do not establish native execution or scientific
validity. See [current capabilities](docs/CURRENT_CAPABILITIES_zh.md),
[resource policies](docs/RESOURCE_POLICY.md), and [migration](docs/MIGRATION.md).

## Prepare a model without sampling

```r
library(bmeas)
d <- data.frame(person = 1:4, x1 = c(.2, .8, .3, 1),
                x2 = c(.7, .6, -.3, .1), x3 = c(-.2, .6, .9, .4))
spec <- bmeas(bmf(G = ~ x1 + x2 + x3), d, subject = "person", fit = FALSE)
check_model(spec)
```

## Development status

Current source is maintained on `work/current-0.0.47.9000`. Changes to `develop`
and `main`, pull requests targeting them, and the first publication require the
maintainer's explicit authorization. No current version is published to CRAN.
The inherited DESCRIPTION author/contact fields are prototype metadata.

## Documentation

- [Current capabilities](docs/CURRENT_CAPABILITIES_zh.md) and [workflow](docs/CURRENT_WORKFLOW_zh.md)
- [Documentation index](docs/README.md)
- [Changes by version](NEWS.md)
