# Run both optional pinned-M3 capture profiles in fresh R processes.
# Keeping the process boundary is essential: a warmed namespace can hide the
# lazy callback-resolution regression. No likelihood, simulator or RNG is run.
resource <- Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
if (!nzchar(resource) || !file.exists(resource)) {
    cat("SKIP pinned-M3 callback capture profiles: reference resource unavailable\n")
} else {
    script <- normalizePath("frontend-callback-capture.R", mustWork = TRUE)
    executable <- file.path(R.home("bin"), "Rscript")
    library_path <- paste(.libPaths(), collapse = .Platform$path.sep)
    for (factory in c("m3_cs_model", "m3_model")) {
        output <- system2(executable, c("--vanilla", shQuote(script)),
            stdout = TRUE, stderr = TRUE,
            env = c(paste0("MEASUREMENTPKG_CALLBACK_FACTORY=", factory),
                paste0("R_LIBS=", shQuote(library_path))), timeout = 180)
        cat(output, sep = "\n")
        status <- attr(output, "status")
        if (!is.null(status) && status != 0L) {
            stop("Fresh-process callback profile failed: ", factory,
                " (exit ", status, ")")
        }
    }
}
