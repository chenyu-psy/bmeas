description <- read.dcf("DESCRIPTION")
stopifnot(description[1, "Package"] == "bmeas")
version <- unname(description[1, "Version"])
if (!grepl("^(0|[1-9][0-9]*)[.](0|[1-9][0-9]*)[.](0|[1-9][0-9]*)([.](0|[1-9][0-9]*))?$", version))
  stop("Version must have three or four numeric R version components")
invisible(package_version(version))
base <- Sys.getenv("BASE_DESCRIPTION")
if (nzchar(base) && file.exists(base)) {
  previous <- unname(read.dcf(base)[1, "Version"])
  if (utils::compareVersion(version, previous) <= 0L)
    stop("Release version must increase relative to main")
}
heading <- readLines("NEWS.md", warn = FALSE)
if (!any(startsWith(heading, paste0("# bmeas ", version, " "))) &&
    !any(heading == paste0("# bmeas ", version)))
  stop("NEWS must include the exact DESCRIPTION version")
parts <- as.numeric(strsplit(version, ".", fixed = TRUE)[[1]])
development <- length(parts) == 4L && parts[4] >= 9000L
cat("version=", version, "\nprerelease=", tolower(development), "\n", sep = "")
