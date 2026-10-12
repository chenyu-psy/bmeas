# Reviewed public-provider boundary. Planning never discovers the runtime.
.bh_provider_metadata_plan <- function(dimensions,run,output,policy=NULL) {
  chains<-.bh_count(as.double(run$chains),"provider configured chains",1)
  P<-.bh_count(dimensions$total_sampled_parameter_dimension,"unconstrained sampled parameter dimension")
  mode<-if(is.list(policy))policy$cache else NULL
  deferred<-identical(mode,"deferred")
  out<-list(version="bmeas_provider_metadata_plan_v1",status="expected",
    scope=if(deferred)"Expected metadata for the requested reviewed deferred constructor with diagnostics=NULL"else
      "Deferred-provider reference estimates only; no runtime pin or zero-materialization claim is applied to eager or missing-policy requests",
    provider=list(name="CmdStanR",version="0.9.0",cmdstan_version="2.40.0",
      platform="reviewed non-Windows, non-WSL POSIX branch",
      runtime_compatibility=if(deferred)"unverified"else"not_applied_to_requested_mode"),
    configured_chains=chains,
    full_file_scan=list(status="expected_before_sample_returns",actual_bytes=NA_real_,
      configured_csv_byte_heuristic=output$estimated_csv_bytes,
      reason="The provider scans every successful CSV for metadata before the package size check; actual scan bytes are unknown and the output heuristic is not an enforced scan limit"),
    headers=list(status="unavailable",columns_per_chain=NA_real_,name_entries_all_chains=NA_real_,
      encoded_text_bytes_per_chain=NA_real_,encoded_text_bytes_all_chains=NA_real_,
      reason="No emitted-source header estimate is available"),
    inverse_metric=list(status="unavailable",geometric_unconstrained_dimension=P,unconstrained_parameters_per_chain=NA_real_,
      scalar_slots=NA_real_,numeric_payload_bytes=NA_real_,
      reason="Configured chains times actual unconstrained sampled parameters; expected adaptation metadata, not observed adaptation or a total-allocation bound"),
    scalar_metadata=list(status="partially_counted_expected_fields",
      configured_fields=c("id","seed","iter_warmup","iter_sampling","save_warmup","thin","threads_per_chain","max_treedepth"),
      configured_field_slots=.bh_count_product(chains,8,name="named configured metadata field slots"),
      adaptation_and_timing_fields=c("step_size_adaptation","warmup","sampling","total"),
      adaptation_and_other_numeric_slots=NA_real_,
      reason="The named configured fields are a subset; adaptation/timing presence and additional numeric metadata are unobserved and their complete materialization count is unknown"),
    requested_cache=mode,
    deferred_materialization_status=if(deferred)"deferred_requested"else if(identical(mode,"eager"))"inapplicable_eager"else"uncomputed_policy_missing",
    deferred_posterior_draw_scalar_slots=if(deferred)0 else NA_real_,
    limitations=c("diagnostics=NULL disables numeric posterior/diagnostic draw arrays; provider diagnostic_summary(NULL) is a permitted no-op.",
      "Header names, run metadata, adaptation values and inverse metric arrays still materialize; full-file metadata scans remain mandatory.",
      "String/object overhead, parsing buffers, temporary/repaired-name copies and other numeric metadata are unknown; no total memory, RSS, time or pre-read-byte bound is enforced.",
      "No dense metric support is introduced; a future dense metric would require a separate quadratic-cost policy review."))
  inventory<-output$inventory
  if(identical(output$status,"unavailable_lowering")||is.null(inventory)) {
    out$status<-"unavailable_lowering"
    out$inverse_metric$reason<-"A geometric parameter count is retained, but no emitted provider program establishes inverse-metric dimensions"
    out$scalar_metadata$status<-"unavailable_lowering"
    out$scalar_metadata$configured_field_slots<-NA_real_
    out$deferred_posterior_draw_scalar_slots<-NA_real_
    return(out)
  }
  slots<-.bh_count_product(chains,P,name="default diagonal inverse-metric metadata slots")
  out$inverse_metric$status<-"expected_default_diagonal"
  out$inverse_metric$unconstrained_parameters_per_chain<-P
  out$inverse_metric$scalar_slots<-slots
  out$inverse_metric$numeric_payload_bytes<-.bh_count_product(8,slots,name="diagonal metric numeric payload")
  if(!is.data.frame(inventory)||!all(c("variable","elements","dimensions")%in%names(inventory)))return(out)
  digit_sum<-function(n) {
    n<-.bh_count(n,"header dimension")
    if(n==0)return(0)
    total<-0;lower<-1;digits<-1
    while(lower<=n) {
      upper<-min(n,if(lower>floor((2^53-1)/10))2^53-1 else lower*10-1)
      total<-.bh_count_sum(c(total,.bh_count_product(upper-lower+1,digits,name="decimal index text bytes")),"decimal index text bytes")
      if(upper==n)break
      lower<-lower*10;digits<-digits+1
    }
    total
  }
  label_bytes<-numeric(nrow(inventory))
  for(i in seq_len(nrow(inventory))) {
    name<-inventory$variable[i];axes<-inventory$dimensions[[i]]
    if(!is.character(name)||length(name)!=1L||is.na(name)||!validEnc(name)||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$",name))return(out)
    if(!typeof(axes)%in%c("integer","double")||is.object(axes)||!is.null(attributes(axes)))return(out)
    cells<-if(length(axes))do.call(.bh_count_product,c(as.list(axes),list(name="header group cells")))else 1
    if(!identical(as.double(inventory$elements[i]),cells))return(out)
    if(!cells)next
    total<-.bh_count_product(cells,.bh_count_sum(c(nchar(name,type="bytes"),length(axes))),name="header base text bytes")
    for(j in seq_along(axes)) {
      other<-if(length(axes)>1L)do.call(.bh_count_product,c(as.list(axes[-j]),list(name="header repeated indices")))else 1
      total<-.bh_count_sum(c(total,.bh_count_product(other,digit_sum(axes[j]),name="header index text bytes")),"header group text bytes")
    }
    label_bytes[i]<-total
  }
  columns<-.bh_count_sum(inventory$elements,"provider header columns")
  bytes<-.bh_count_sum(c(label_bytes,max(0,columns-1),if(columns)1 else 0),"encoded CSV header text")
  out$headers<-list(status="counted_from_declared_shapes",columns_per_chain=columns,
    standard_hmc_fields_per_chain=7,name_entries_all_chains=.bh_count_product(chains,columns,name="per-chain header name entries"),
    encoded_text_bytes_per_chain=bytes,encoded_text_bytes_all_chains=.bh_count_product(chains,bytes,name="per-chain encoded header bytes"),
    reason="ASCII unchanged declared symbols with dot-separated decimal indices, comma separators and one LF; repeated per-chain text counted without constructing expanded names. This is header text only, not all retained/repaired string storage")
  out
}

.bh_provider_platform_info <- function() {
  os_type<-.Platform$OS.type
  if(!identical(os_type,"unix"))return(list(os_type=os_type,wsl=FALSE))
  wsl<-nzchar(Sys.getenv("WSL_DISTRO_NAME"))||nzchar(Sys.getenv("WSL_INTEROP"))||
    identical(Sys.getenv("CMDSTANR_USE_WSL"),"1")
  if(!wsl) {
    info<-Sys.info()
    wsl<-any(grepl("microsoft|wsl",unname(info[c("release","version")]),ignore.case=TRUE))
  }
  list(os_type=os_type,wsl=wsl)
}
.bh_provider_runtime_info <- function() {
  platform<-.bh_provider_platform_info()
  if(!identical(platform$os_type,"unix")||!identical(platform$wsl,FALSE))
    return(c(list(available=FALSE,discovery="skipped_unsupported_platform"),platform))
  if(!requireNamespace("cmdstanr",quietly=TRUE))return(c(list(available=FALSE),platform))
  version<-as.character(utils::packageVersion("cmdstanr"))
  if(!identical(version,"0.9.0"))return(c(list(available=TRUE,cmdstanr_version=version,cmdstan_version=character()),platform))
  # The inspected 0.9.0 getter returns cached .cmdstanr$VERSION; it does not
  # discover a path or execute stanc/make/WSL. The WSL getter is also metadata-only.
  cmdstan<-tryCatch(as.character(cmdstanr::cmdstan_version(error_on_NA=FALSE)),error=function(e)character())
  platform$wsl<-platform$wsl||isTRUE(getFromNamespace("os_is_wsl","cmdstanr")())
  c(list(available=TRUE,cmdstanr_version=version,cmdstan_version=cmdstan),platform)
}
.bh_provider_path_text <- function(x,label="output directory") {
  if(typeof(x)!="character"||is.object(x)||!is.null(attributes(x))||length(x)!=1L||
    is.na(x)||!validEnc(x)||identical(Encoding(x),"bytes")||!nzchar(x))
    .mp_abort("E_PROVIDER_PATH",paste(label,"must be one plain valid text path"))
  points<-utf8ToInt(enc2utf8(x))
  if(any(points==39L|points<32L|(points>=127L&points<=159L)))
    .mp_abort("E_PROVIDER_PATH",paste(label,"contains a single quote or control character unsafe for the reviewed provider metadata reader; choose an explicit safe destination"))
  invisible(x)
}
.bh_provider_output_dir <- function(output_dir) {
  .bh_provider_path_text(output_dir)
  expanded<-path.expand(output_dir)
  .bh_provider_path_text(expanded,"expanded output directory")
  if(startsWith(expanded,"~")) .mp_abort("E_PROVIDER_PATH","the output-directory tilde could not be expanded")
  absolute<-if(startsWith(expanded,"/"))expanded else {
    cwd<-getwd();.bh_provider_path_text(cwd,"current working directory")
    file.path(cwd,expanded)
  }
  # Resolve every existing prefix, including symlink parents of a new suffix.
  parts<-strsplit(absolute,"/",fixed=TRUE)[[1L]];current<-"/"
  for(part in parts[nzchar(parts)]) {
    if(part==".")next
    if(part=="..") {current<-dirname(current);next}
    next_path<-file.path(current,part)
    if(dir.exists(next_path)) {
      current<-tryCatch(withCallingHandlers(normalizePath(next_path,winslash="/",mustWork=TRUE),
        warning=function(w).mp_abort("E_PROVIDER_PATH","an existing output-directory component could not be resolved unambiguously")),
        error=function(e) {
          if(inherits(e,"measurement_error"))stop(e)
          .mp_abort("E_PROVIDER_PATH","an existing output-directory component could not be resolved unambiguously")
        })
      .bh_provider_path_text(current,"resolved output-directory component")
    }else {
      link<-Sys.readlink(next_path)
      if(file.exists(next_path)||(!is.na(link)&&nzchar(link)))
        .mp_abort("E_PROVIDER_PATH","an output-directory component is not a directory or is an unresolved symbolic link")
      current<-next_path
    }
  }
  .bh_provider_path_text(current,"resolved output directory")
  current
}
.bh_provider_preflight <- function(plan,output_dir) {
  if(!identical(plan$resource_policy$cache,"deferred"))return(list(
    version="bmeas_provider_runtime_v1",status="eager_legacy_scope",requested_output_dir=output_dir,output_dir=output_dir,
    output_basename=NULL,known_artifact_paths=character()))
  .bh_provider_path_text(output_dir)
  runtime<-.bh_provider_runtime_info()
  if(!identical(runtime$os_type,"unix")||!identical(runtime$wsl,FALSE))
    .mp_abort("E_PROVIDER_PLATFORM","deferred execution is reviewed only for the non-Windows, non-WSL POSIX metadata-reader branch")
  if(!isTRUE(runtime$available)) .mp_abort("E_PROVIDER_UNAVAILABLE","deferred execution requires the reviewed CmdStanR 0.9.0 and configured CmdStan 2.40.0 provider")
  if(!identical(runtime$cmdstanr_version,"0.9.0")||!identical(runtime$cmdstan_version,"2.40.0"))
    .mp_abort("E_PROVIDER_VERSION","deferred execution is reviewed only for CmdStanR 0.9.0 with configured CmdStan 2.40.0; planning does not establish runtime compatibility")
  directory<-.bh_provider_output_dir(output_dir)
  available<-FALSE
  for(attempt in seq_len(16L)) {
    basename<-.bh_provider_new_basename()
    if(!grepl("^[A-Za-z0-9_-]+$",basename)) .mp_abort("E_PROVIDER_PATH","could not create a safe package-controlled output basename")
    if(.bh_provider_basename_available(directory,basename)) {available<-TRUE;break}
  }
  if(!available).mp_abort("E_PROVIDER_OUTPUT_EXISTS","could not select a noncolliding output basename in 16 name-only attempts; sampling was not started and existing files were preserved")
  list(version="bmeas_provider_runtime_v1",status="reviewed_deferred_scope",runtime=runtime,
    requested_output_dir=output_dir,output_dir=directory,output_basename=basename,known_artifact_paths=character(),
    constructor_boundary="diagnostics=NULL skips posterior/diagnostic draw arrays; the provider still scans full CSVs and retains header, adaptation and inverse-metric metadata before returning")
}
.bh_provider_new_basename <- function() basename(tempfile("bmeas-"))
.bh_provider_basename_available <- function(directory,basename) {
  if(!dir.exists(directory))return(TRUE)
  if(file.access(directory,4L)!=0L||file.access(directory,1L)!=0L)
    .mp_abort("E_PROVIDER_PATH","existing output-directory filenames cannot be inspected; basename collision status is unknown")
  !length(list.files(directory,pattern=paste0("^",basename,"-[0-9]+[.]csv$"),all.files=TRUE))
}
.bh_provider_sample <- function(model,sample_args,provider_context) {
  tryCatch(do.call(model$sample,sample_args),interrupt=function(e)stop(e),error=function(e) {
    if(inherits(e,"interrupt"))stop(e)
    stop(structure(list(message=paste0("E_PROVIDER_SAMPLE_FAILED: provider operation failed before a fit was returned: ",conditionMessage(e)),
      call=NULL,code="E_PROVIDER_SAMPLE_FAILED",cause=e,original_condition=e,
      provider_operation=list(status="failed",reason="The public provider call did not return; process completion and return-code evidence remain unknown"),
      output_dir=provider_context$output_dir,output_files=provider_context$known_artifact_paths,
      sampling=list(status="unknown",return_codes=NULL,observed_csv_rows=NA_real_),
      provider_context=provider_context),class=c("bmeas_provider_error","measurement_error","error","condition")))
  })
}
