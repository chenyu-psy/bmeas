# Explicit manual R-only regression runner; never invoked by package checks.
args <- commandArgs(TRUE)
if(length(args)!=2L || !identical(args[1],"extended"))
  stop("Usage: Rscript --vanilla validation/run.R extended /absolute/new-output-directory")
file_arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
root <- dirname(dirname(normalizePath(sub("^--file=","",file_arg))))
if(nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))) stop("Unset MEASUREMENTPKG_SOURCE: this runner tests the installed tar")
output <- path.expand(args[2])
if(!grepl("^/",output) || file.exists(output))
  stop("Output must be a new absolute directory; existing paths are never overwritten")
output <- file.path(normalizePath(dirname(output),mustWork=TRUE),basename(output))
if(startsWith(output,paste0(root,"/"))) stop("Keep generated logs outside the repository")
if(!requireNamespace("bmeas",quietly=TRUE)) stop("Install the exact source tar in an isolated library first")
installed <- find.package("bmeas")
expected <- read.dcf(file.path(root,"DESCRIPTION"))[1,"Version"]
if(as.character(packageVersion("bmeas"))!=expected) stop("Installed package version does not match this checkout")
manifest <- read.delim(file.path(root,"validation/test-suites.tsv"),stringsAsFactors=FALSE)
stopifnot(!anyDuplicated(basename(manifest$script)),all(file.exists(file.path(root,manifest$script))))
dir.create(output,recursive=TRUE)
stage <- file.path(output,"tests"); dir.create(stage)
stopifnot(all(file.copy(file.path(root,manifest$script),stage)))
# Preserve peer script names and relative support paths used by fresh-process tests.
stopifnot(all(file.copy(list.files(file.path(root,"tests"),full.names=TRUE)[
  file.info(list.files(file.path(root,"tests"),full.names=TRUE))$isdir],stage,recursive=TRUE)))
support <- file.path(root,"validation/support")
for(relative in list.files(support,recursive=TRUE,all.files=TRUE,no..=TRUE)) {
  destination <- file.path(stage,relative)
  dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
  stopifnot(file.copy(file.path(support,relative),destination,overwrite=FALSE))
}
# Native programs/research recovery remain separately authorized tools, never this runner.
Sys.setenv(OMP_NUM_THREADS=1,OPENBLAS_NUM_THREADS=1,MKL_NUM_THREADS=1,
  VECLIB_MAXIMUM_THREADS=1,MAKEFLAGS="-j1",`_R_CHECK_NCPU_`=1,`_R_CHECK_TIMINGS_`=0,
  R_LIBS=paste(.libPaths(),collapse=.Platform$path.sep))
writeLines(c(paste("Installed path:",installed),paste("Installed version:",as.character(packageVersion("bmeas"))),
 paste("R:",R.version.string),paste("Tests:",nrow(manifest)),
 "R-only extended regression; optional absent-resource branches are skips, not scientific qualification."),
 file.path(output,"scope.txt"))
setwd(stage)
# The same base-R batch launcher as R CMD check: each file gets an isolated process,
# full timing footer, failures remain .Rout.fail, and later files still run.
failures <- tools:::.runPackageTests(stop_on_error=FALSE,Log=file.path(output,"batch.log"))
reporter <- file.path(root,".github/scripts/report_check.py")
report <- system2("python3",c(shQuote(reporter),"--check-dir",shQuote(output),
 "--suite","extended-r-only","--tests-dir",shQuote(stage),"--output-dir",shQuote(output)))
quit(status=if(failures>0L || report!=0L)1L else 0L)
