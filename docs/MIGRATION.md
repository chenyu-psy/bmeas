# Moving from bhmm to bmeas

The package is now named `bmeas`. The main entries are `bmeas()` and `bmf()`. The other modeling and inspection helpers retain their names. `bhmm()` and `bhmf()` remain exact callable aliases inside the new package, so an existing script can begin by changing `library(bhmm)` to `library(bmeas)`.

```r
# Install a source tar built from the version you intend to use.
install.packages("path/to/bmeas_VERSION.tar.gz", repos = NULL, type = "source")
library(bmeas)
m <- bmf(G = ~x1 + x2 + x3)
s <- bmeas(m, data, subject = "person", fit = FALSE)
check_model(s)
get_prior(s)
```

The rename does not change the likelihood, parameter units, priors, latent law or measurement structure. New formulas, prepared contexts and fits have a bmeas-specific leading S3 class and retain the legacy class second. This prevents another installed bhmm namespace from overwriting their print/summary dispatch. Already saved objects keep their original classes and fingerprints. Scientific source/cache identifiers remain in use for compatibility. Generated-model comments may therefore retain the historical name; numerical qualification belongs to the exact model source and evidence, not to a branding string. Existing default cache/draw directory names are retained as well.

Historical `bhmm` archives remain separately installable. A saved object can contain functions bound to its original package namespace. Keeping that original package installed can preserve those bindings; both packages may be installed together, and explicit `bmeas::` or `bhmm::` calls avoid search-path ambiguity. Bounded checks cover the qualified .9008 Gaussian and pinned CS/SDM contexts with both namespaces available, including cold reloads and unchanged code/prior values. This is not a blanket guarantee for every historical or custom object.

If a saved object's namespace is missing, its schema is incompatible, or a captured fingerprint/information policy no longer validates, rebuild explicitly using the new constructors, original data and intended priors. The package does not rewrite callback environments, replace missing namespaces or reset fingerprints. A data-calibrated default can depend on the data, so retain and deliberately specify the original resolved priors when reproducing an analysis. Do not repair a saved object by editing its fingerprint.

Detached numerical summaries retain their historical role and evidence labels. Reading a cache does not grant a new execution profile. Task-Cholesky augmentation still reports task-white coordinates and task deviations separately; common-factor and parameter-residual generative scores are unsampled in that mode. Fixed/equal final-marginal correlations have bounded fixed/one-label iid and centered/task residual-layer profiles requiring explicit experimental execution. Centered/task common correlations can use either the separately qualified ordered raw-B law or an effective ordinary zero/free final-marginal MGS graph. Genuine fixed/equal common affine constraints in centered/task mode and multidimensional affine charts remain outside execution support. Historical caches keep their own evidence and role labels; inspecting them does not promote their old compiled body into the new profile. Compatible prepared objects must still pass the original fingerprint and current policy guards, with no namespace or fingerprint rewriting.

## Historical centered/final-marginal reporting maps

An accepted pre-affine reference-law context may remain valid for its existing scientific tables and code inspection while lacking the later structured correlation primitive/cell tables. The new centered/final-marginal correlation summaries and cache path require an explicit rebuild from the original data, formula and intended priors. They raise E_CORRELATION_REPORT_REBUILD rather than silently omit rho rows or invent metadata. check_model() identifies this capability boundary. Historical object fingerprints and detached numerical summaries are not rewritten.
