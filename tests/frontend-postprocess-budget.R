source(file.path("helpers", "native-guard.R"))
root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
if(nzchar(root)) for(f in list.files(file.path(root,"R"),pattern="[.]R$",full.names=TRUE)) source(f) else library(bmeas)
pvt <- function(n) if(nzchar(root)) get(n,.GlobalEnv) else getFromNamespace(n,"bmeas")
local({
 refuses <- function(action) stopifnot(inherits(tryCatch(action(),error=identity),"error"))
 for(operation in c("archive_fit","read_fit_archive","component_draws")) {
  b <- postprocess_budget(operation)
  validate <- pvt(if(operation=="component_draws") ".bh_component_budget" else ".bh_archive_budget")
  validate(b)
  original <- b;b$context$max_depth <- 1;b$posterior$max_source_bytes <- 1
  stopifnot(identical(postprocess_budget(operation),original),identical(class(original),"list"))
  b <- original
  # Every numeric field retains the actual validator's lower and upper boundary.
  fields <- c(names(b)[vapply(b,is.numeric,logical(1))],
    paste0("posterior/",setdiff(names(b$posterior),"reader")),paste0("context/",names(b$context)))
  for(field in fields) {
   path <- strsplit(field,"/",fixed=TRUE)[[1]]
   change <- function(value) { out <- b;if(length(path)==1)out[[path]] <- value else out[[path[1]]][[path[2]]] <- value;out }
   low <- if(identical(field,"posterior/max_scan_values"))0 else 1
   high <- if(tail(path,1)%in%c("max_components","max_record_bytes","max_columns","max_source_rows","max_chains","max_depth")) .Machine$integer.max else 2^53-1
   for(value in list(NA_real_,NaN,Inf,-Inf,low-1,.5,high+1,"1",TRUE,c(1,2))) refuses(function()validate(change(value)))
   validate(change(low));validate(change(high-1));validate(change(high))
  }
  for(value in list(NULL,list(),c(b,b[1]),c(b,list(unknown=1)))) refuses(function()validate(value))
  for(part in c("context","posterior")) {
   bad <- b;bad[[part]] <- NULL;refuses(function()validate(bad))
  }
 }
 stopifnot(identical(postprocess_budget("archive_fit"),postprocess_budget("read_fit_archive")))
 for(operation in list(NULL,NA_character_,character(),c("archive_fit","component_draws"),"archive","",1,TRUE))
  refuses(function()postprocess_budget(operation))
 cat("PASS fresh operation defaults, every numeric invalid/boundary field, exact operation names and unchanged explicit schema\n")
})
