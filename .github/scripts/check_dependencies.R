# Run after setup-r-dependencies, in a fresh process before build/check.
# Package runtime guards remain authoritative and unchanged.
expected <- c(lavaan="0.7.2",posterior="1.7.0",cmdstanr="0.9.0")
cmdstan_sha <- "da99e2ba954658bdad63bffb738c4444c33a4e0e"
installed_version <- function(package) as.character(packageVersion(package))
# The general solver may choose a newer Suggests dependency. Replace only a
# mismatched CRAN package, using its explicit official source release.
cran_sources <- list(
  lavaan="https://cran.r-project.org/src/contrib/Archive/lavaan/lavaan_0.7-2.tar.gz",
  posterior=c("https://cran.r-project.org/src/contrib/posterior_1.7.0.tar.gz",
    "https://cran.r-project.org/src/contrib/Archive/posterior/posterior_1.7.0.tar.gz"))
for(package in names(cran_sources)) {
  if(identical(installed_version(package),expected[[package]])) next
  archive <- tempfile(fileext=".tar.gz")
  downloaded <- FALSE
  for(url in cran_sources[[package]]) {
    result <- tryCatch(download.file(url,archive,mode="wb",quiet=FALSE),error=function(e)1L)
    if(identical(result,0L)){downloaded <- TRUE;break}
  }
  if(!downloaded) stop("Cannot retrieve pinned official CRAN release: ",package)
  install.packages(archive,repos=NULL,type="source",lib=.libPaths()[1],Ncpus=1L)
  unlink(archive)
}
# Assert the effective library after all dependency setup. Version failures stop
# before build/check; no contract is skipped and no native program is installed.
actual <- vapply(names(expected),installed_version,character(1))
print(data.frame(package=names(expected),expected=unname(expected),actual=unname(actual),
  library=vapply(names(expected),find.package,character(1))),row.names=FALSE)
if(!identical(actual,expected)) stop("Effective dependency versions differ from CI pins")
metadata <- packageDescription("cmdstanr")
if(!identical(metadata$RemoteSha,cmdstan_sha) || !identical(metadata$RemoteUsername,"stan-dev") ||
   !identical(metadata$RemoteRepo,"cmdstanr")) stop("cmdstanr origin/commit differs from the official CI pin")
if(!identical(as.character(getRversion()),"4.5.2")) stop("R differs from the CI pin")
cat("PASS effective dependency pins after setup; cmdstanr official SHA:",cmdstan_sha,"\n")
