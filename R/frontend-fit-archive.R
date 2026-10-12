# A trusted-local, plain-data envelope. No sampler-time provenance is rewritten.
.bh_archive_fail <- function(message) .mp_abort("E_FIT_ARCHIVE", message)
.bh_archive_fields <- function(x, fields, label) {
  if (!is.list(x) || !is.null(class(x)) && !identical(class(x), "list") ||
      !identical(names(x), fields)) .bh_archive_fail(paste("Invalid", label, "schema"))
  invisible(x)
}
.bh_archive_budget <- function(budget) {
  fields <- c("max_members", "max_member_bytes", "max_total_bytes", "max_context_bytes",
    "max_read_bytes", "max_temp_bytes", "posterior", "context")
  if(!is.list(budget) || !identical(class(budget),"list") || is.null(names(budget)) ||
      anyNA(names(budget)) || anyDuplicated(names(budget)) ||
      length(budget)!=length(fields) || !setequal(names(budget),fields))
    .bh_archive_fail("Invalid archive budget fields")
  budget <- budget[fields]
  for (n in fields[1:6]) .bh_pf_number(budget[[n]], n, 1, 2^53-1, TRUE)
  .bh_pf_policy(budget$context)
  .bh_posterior_policy(budget$posterior, character())
  budget
}
.bh_archive_md5 <- function(path) {
  value <- unname(tools::md5sum(path))
  if (anyNA(value)) .bh_archive_fail("Could not hash archive member")
  value
}
.bh_archive_path <- function(root, member) {
  if (!is.character(member) || length(member)!=1L || is.na(member) ||
      !grepl("^(portable\\.rds|manifest\\.rds|draws/[0-9]{6}\\.csv|resources/[0-9]{6})$", member))
    .bh_archive_fail("Unsafe archive member name")
  parts <- strsplit(member, "/", fixed=TRUE)[[1L]]
  current <- root
  for (part in parts) {
    current <- file.path(current,part)
    link <- Sys.readlink(current)
    if (!is.na(link) && nzchar(link)) .bh_archive_fail("Archive member symlinks are unsupported")
  }
  actual <- normalizePath(current,winslash="/",mustWork=TRUE)
  if (!startsWith(actual,paste0(root,"/")) || !file_test("-f",actual))
    .bh_archive_fail("Archive member is not a regular file beneath its root")
  actual
}
.bh_archive_sizes <- function(paths, budget) {
  sizes <- as.double(file.info(paths)$size)
  if (anyNA(sizes) || any(!file_test("-f", paths)) || any(sizes>budget$max_member_bytes) ||
      sum(sizes)>budget$max_total_bytes || length(paths)>budget$max_members)
    .bh_archive_fail("Archive members exceed file/count allowance or are unavailable")
  sizes
}
.bh_archive_resources <- function(portable) {
  result <- list()
  descriptors <- portable$immutable$retained_artifacts$resource_descriptors
  for (model in names(descriptors)) {
    entries <- descriptors[[model]]$resources
    for (i in seq_along(entries)) result[[length(result)+1L]] <-
      list(model=model,index=as.integer(i),descriptor=entries[[i]])
  }
  result
}
.bh_archive_copy <- function(from,to,size) {
  a <- file(from,"rb"); on.exit(close(a))
  b <- file(to,"wb"); on.exit(close(b),add=TRUE)
  remaining <- size
  while (remaining>0) {
    bytes <- readBin(a,"raw",n=min(65536,remaining))
    if (!length(bytes)) .bh_archive_fail("Source truncated during archive copy")
    writeBin(bytes,b); remaining <- remaining-length(bytes)
  }
  if (length(readBin(a,"raw",n=1L))) .bh_archive_fail("Source grew during archive copy")
}
.bh_archive_layout <- function(paths,budget,hashes) {
  if (!length(paths)) return(list())
  value <- .bh_posterior_ingest_csv(paths,character(),budget$posterior,states="all",
    expected_model="scalar_components_model",archive_hashes=hashes,archive_layout=TRUE)
  value$archive_layout
}
.bh_archive_manifest_fields <- function() c("format","producer","created_utc","scope",
  "portable_identity","members","posterior_layout","resources","completeness","integrity","accounting")
.bh_archive_inventory <- function(portable, resources) list(
  observed_data_and_maps="retained_in_unchanged_capsule",generated_source="retained_in_unchanged_capsule",
  posterior="all_retained_csv_bytes",resources=if(!length(resources))"not_required" else
    if(any(vapply(resources,function(x)x$status=="missing_now",FALSE)))"missing_now" else "retained",
  exact_input_json="not_collected",exact_init_json="not_collected",
  warmup="not_collected_by_supported_csv_profile",
  optional_log_likelihood="retained_if_present_in_full_csv_no_generation",
  unsampled_quantities="not_collected",reexecution_completeness="not_claimed")

#' Archive complete retained posterior files and an unchanged portable capsule
#' @export
archive_fit <- function(fit,path,budget=postprocess_budget("archive_fit")) {
  budget <- .bh_archive_budget(budget)
  if (!.bh_pf_is_portable(fit)) fit <- portable_fit(fit,budget$context)
  .bh_pf_active(fit,"component_draws",archive_inspection=TRUE)
  .bh_portable_codec_count(fit,budget$context,Sys.getlocale("LC_CTYPE"))
  payload <- serialize(fit,NULL,version=3L)
  if(length(payload)>budget$max_context_bytes) .bh_archive_fail("Portable serialization exceeds context allowance")
  .bh_pf_string(path,"archive destination")
  if(file.exists(path) || dir.exists(path) || (!is.na(Sys.readlink(path)) && nzchar(Sys.readlink(path))))
    .bh_archive_fail("Archive destination must be absent")
  parent <- normalizePath(dirname(path),winslash="/",mustWork=TRUE)
  destination <- file.path(parent,basename(path))
  paths <- unname(fit$immutable$source_binding$original_paths$output_files)
  if(is.null(paths)) paths <- character()
  if(anyDuplicated(paths)) .bh_archive_fail("Repeated original posterior paths")
  resources <- .bh_archive_resources(fit)
  resource_sources <- character(); resource_members <- character()
  for(i in seq_along(resources)) {
    descriptor <- resources[[i]]$descriptor
    present <- file.exists(descriptor$file)
    resources[[i]]$status <- if(present)"retained_pin_unverified" else "missing_now"
    resources[[i]]$member <- if(present)sprintf("resources/%06d",i) else NA_character_
    if(present) {
      resource_sources <- c(resource_sources,descriptor$file)
      resource_members <- c(resource_members,resources[[i]]$member)
    }
  }
  sources <- c(paths,resource_sources)
  sizes <- .bh_archive_sizes(sources,budget)
  members <- c(sprintf("draws/%06d.csv",seq_along(paths)),resource_members)
  total <- sum(sizes)+length(payload)
  # Seven CSV-reader passes plus two copies, two MD5 passes and optional pin pass.
  read_reservation <- 12*sum(sizes)+2*length(payload)+budget$max_context_bytes
  temp_reservation <- 4*sum(sizes)+length(payload)+budget$max_context_bytes
  if(length(sources)+2>budget$max_members || total>budget$max_total_bytes ||
      length(payload)>budget$max_member_bytes || read_reservation>budget$max_read_bytes ||
      temp_reservation>budget$max_temp_bytes) .bh_archive_fail("Archive transaction exceeds admitted I/O or disk allowance")
  owned <- NULL
  on.exit(if(!is.null(owned))unlink(owned,recursive=TRUE,force=TRUE),add=TRUE)
  suspendInterrupts({
    candidate <- tempfile(".bmeas-archive-",tmpdir=parent)
    if(!dir.create(candidate,mode="0700")) .bh_archive_fail("Could not create archive staging directory")
    owned <- candidate
  })
  dir.create(file.path(owned,"draws"));dir.create(file.path(owned,"resources"))
  before <- file.info(sources)[,c("size","mtime","ctime"),drop=FALSE]
  hashes <- character(length(sources))
  for(i in seq_along(sources)) {
    target <- file.path(owned,members[i]);check <- file.path(owned,"verify.tmp")
    .bh_archive_copy(sources[i],target,sizes[i]);.bh_archive_copy(sources[i],check,sizes[i])
    hashes[i] <- .bh_archive_md5(target)
    if(!identical(hashes[i],.bh_archive_md5(check))) .bh_archive_fail("Source changed during archive snapshot")
    unlink(check)
  }
  if(!identical(before,file.info(sources)[,c("size","mtime","ctime"),drop=FALSE]))
    .bh_archive_fail("Source metadata changed during archive snapshot")
  for(i in seq_along(resources)) if(resources[[i]]$status!="missing_now") {
    pin <- resources[[i]]$descriptor$sha256
    if(grepl("^[a-fA-F0-9]{64}$",pin)) {
      if(!identical(.mp_sha256(file.path(owned,resources[[i]]$member)),tolower(pin)))
        .bh_archive_fail("Surviving resource contradicts its saved SHA-256 pin")
      resources[[i]]$status <- "retained_pin_verified"
    }
  }
  layout <- .bh_archive_layout(file.path(owned,members[seq_along(paths)]),budget,hashes[seq_along(paths)])
  expected <- fit$immutable$retained_artifacts$run$chains
  if(!is.null(expected) && length(paths)!=expected) .bh_archive_fail("Retained file count contradicts saved chain count")
  .bh_archive_precision(fit,layout)
  writeBin(payload,file.path(owned,"portable.rds"))
  table <- data.frame(role=c("portable",rep("posterior",length(paths)),rep("resource",length(resource_sources))),
    member=c("portable.rds",members),bytes=c(length(payload),sizes),
    algorithm=rep("md5",length(sources)+1L),digest=c(.bh_archive_md5(file.path(owned,"portable.rds")),hashes),
    source_index=c(NA_integer_,seq_along(paths),match(resource_members,vapply(resources,function(x)x$member,""))),
    original_path=c(NA_character_,sources),stringsAsFactors=FALSE)
  manifest <- list(format="bmeas_measurement_archive_v1",producer="bmeas development archive v1",
    created_utc=format(Sys.time(),tz="UTC",usetz=TRUE),scope="retained_measurement_evidence",
    portable_identity=fit$identity,members=table,posterior_layout=layout,resources=resources,
    completeness=.bh_archive_inventory(fit,resources),
    integrity=list(algorithm="md5",scope="archive-time corruption detection; not authenticity or sampler-time provenance"),
    accounting=list(read_bytes_reserved=read_reservation,temp_bytes_reserved=temp_reservation,
      source_bytes=sum(sizes),portable_bytes=length(payload),scope="logical I/O; not RSS or elapsed-time bound"))
  encoded <- serialize(manifest,NULL,version=3L)
  if(length(encoded)>budget$max_context_bytes || length(encoded)>budget$max_member_bytes ||
      total+length(encoded)>budget$max_total_bytes) .bh_archive_fail("Manifest exceeds context allowance")
  writeBin(encoded,file.path(owned,"manifest.rds"))
  # No success handle is returned before the complete directory is committed.
  if(file.exists(destination) || !file.rename(owned,destination)) .bh_archive_fail("Archive commit failed; destination must remain absent")
  owned <- NULL
  .bh_archive_handle(destination,.bh_archive_md5(file.path(destination,"manifest.rds")),budget,fit)
}

#' Verify a trusted-local retained measurement archive at its current location
#' @export
.bh_archive_read <- function(path,budget) {
  budget <- .bh_archive_budget(budget)
  root <- normalizePath(path,winslash="/",mustWork=TRUE)
  manifest_path <- .bh_archive_path(root,"manifest.rds")
  msize <- file.info(manifest_path)$size
  if(msize>budget$max_context_bytes || msize>budget$max_member_bytes || 3*msize>budget$max_read_bytes) .bh_archive_fail("Manifest exceeds context allowance")
  initial_hash <- .bh_archive_md5(manifest_path)
  manifest <- readRDS(manifest_path)
  .bh_archive_fields(manifest,.bh_archive_manifest_fields(),"manifest")
  if(!identical(manifest$format,"bmeas_measurement_archive_v1") ||
     !identical(manifest$scope,"retained_measurement_evidence")) .bh_archive_fail("Unsupported archive profile")
  table <- manifest$members
  if(!identical(class(table),"data.frame") || !identical(names(table),c("role","member","bytes","algorithm","digest","source_index","original_path")) ||
     nrow(table)+1>budget$max_members || anyNA(table$member) || anyDuplicated(table$member) ||
     !identical(table$member[table$role=="portable"],"portable.rds") ||
     any(!table$role %in% c("portable","posterior","resource")) || any(table$algorithm!="md5"))
    .bh_archive_fail("Invalid member table")
  paths <- vapply(table$member,function(member).bh_archive_path(root,member),"")
  sizes <- .bh_archive_sizes(paths,budget)
  if(sum(sizes)+msize>budget$max_total_bytes || 2*sum(sizes[table$role=="posterior"])>budget$max_temp_bytes)
    .bh_archive_fail("Archive read exceeds total bytes or temporary disk allowance")
  if(anyDuplicated(paths) || !identical(sizes,table$bytes)) .bh_archive_fail("Member sizes or aliases disagree")
  portable_index <- which(table$role=="portable")
  if(sizes[portable_index]>budget$max_context_bytes ||
      10*sum(sizes)+3*msize>budget$max_read_bytes) .bh_archive_fail("Archive read exceeds allowance")
  if(!identical(.bh_archive_md5(paths),table$digest)) .bh_archive_fail("Archive member bytes changed")
  portable <- readRDS(paths[portable_index])
  .bh_portable_codec_count(portable,budget$context,Sys.getlocale("LC_CTYPE"))
  .bh_pf_active(portable,"component_draws",archive_inspection=TRUE)
  if(!identical(portable$identity,manifest$portable_identity)) .bh_archive_fail("Capsule identity mismatch")
  posterior <- table[table$role=="posterior",,drop=FALSE]
  original <- unname(portable$immutable$source_binding$original_paths$output_files)
  if(is.null(original)) original <- character()
  if(!identical(posterior$source_index,seq_along(original)) || !identical(posterior$original_path,original))
    .bh_archive_fail("Original ordered posterior association mismatch")
  if(!identical(posterior$member,sprintf("draws/%06d.csv",seq_along(original)))) .bh_archive_fail("Posterior member order mismatch")
  if(length(manifest$posterior_layout)!=length(original)) .bh_archive_fail("Posterior layout count mismatch")
  layout <- .bh_archive_layout(paths[table$role=="posterior"],budget,posterior$digest)
  if(!identical(layout,manifest$posterior_layout)) .bh_archive_fail("Saved posterior layout does not match member bytes")
  .bh_archive_precision(portable,layout)
  expected_resources <- .bh_archive_resources(portable)
  if(length(expected_resources)!=length(manifest$resources)) .bh_archive_fail("Resource descriptor count mismatch")
  linked <- character()
  for(i in seq_along(expected_resources)) {
    actual <- manifest$resources[[i]]; expected <- expected_resources[[i]]
    if(!identical(actual[c("model","index","descriptor")],expected)) .bh_archive_fail("Resource descriptor binding mismatch")
    if(identical(actual$status,"missing_now")) {
      if(!identical(actual$member,NA_character_)) .bh_archive_fail("Missing resource has a member")
    } else {
      if(!actual$status %in% c("retained_pin_verified","retained_pin_unverified") ||
          !actual$member %in% table$member[table$role=="resource"]) .bh_archive_fail("Invalid resource member binding")
      row <- table[table$member==actual$member,,drop=FALSE]
      if(!identical(row$source_index,as.integer(i)) || !identical(row$original_path,actual$descriptor$file) ||
          !identical(actual$member,sprintf("resources/%06d",i))) .bh_archive_fail("Resource source association mismatch")
      linked <- c(linked,actual$member)
      pin <- actual$descriptor$sha256
      if(grepl("^[a-fA-F0-9]{64}$",pin)) {
        if(!identical(actual$status,"retained_pin_verified") ||
           !identical(.mp_sha256(.bh_archive_path(root,actual$member)),tolower(pin)))
          .bh_archive_fail("Resource differs from its supported saved pin")
      }
    }
  }
  if(!identical(linked,table$member[table$role=="resource"])) .bh_archive_fail("Unbound resource member")
  if(!identical(manifest$completeness,.bh_archive_inventory(portable,manifest$resources)))
    .bh_archive_fail("Completeness inventory contradicts capsule")
  if(!identical(initial_hash,.bh_archive_md5(manifest_path))) .bh_archive_fail("Manifest changed while reading")
  list(handle=.bh_archive_handle(root,initial_hash,budget,portable),
    portable=portable,manifest=manifest)
}

read_fit_archive <- function(path,budget=postprocess_budget("read_fit_archive")) .bh_archive_read(path,budget)$handle

.bh_archive_component <- function(handle,cap) {
  reopened <- .bh_archive_read(handle$root,handle$budget)
  verified <- reopened$handle; portable <- reopened$portable; manifest <- reopened$manifest
  if(!identical(handle$manifest_md5,verified$manifest_md5)) .bh_archive_fail("Archive manifest changed since opening")
  validated <- .bh_pf_active(portable,"component_draws")
  posterior <- manifest$members[manifest$members$role=="posterior",,drop=FALSE]
  .bh_component_execute(validated$view,cap,validated$receipt,
    acquisition_paths=vapply(posterior$member,function(x).bh_archive_path(verified$root,x),""),
    archive_hashes=posterior$digest)
}

.bh_archive_precision <- function(portable,layout) {
  run <- portable$immutable$retained_artifacts$run$csv_sig_figs
  provenance <- portable$immutable$protected_evidence$original_provenance$csv_sig_figs
  for(expected in list(run,provenance)) if(!is.null(expected)) {
    for(chain in layout) {
      observed <- chain$metadata$sig_figs
      if(!is.null(observed) && observed>0 && expected!=observed)
        .bh_archive_fail("Recorded precision contradicts posterior header")
    }
  }
  if(!is.null(run) && !is.null(provenance) && run!=provenance)
    .bh_archive_fail("Saved run/provenance precision disagree")
}
