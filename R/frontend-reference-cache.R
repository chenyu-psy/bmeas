# Reference-law reporting keeps primitive coordinates, centered white scores and
# task deviations separate. None is silently relabelled as a common-factor score.
.bh_reference_runtime_supported <- function(spec) .bh_centered_person(spec)&&.bh_task_augmentation(spec)&&
  (.bh_raw_cor(spec)||(.bh_centered_final_marginal_mode(spec)&&.bh_final_correlation_metadata_available(spec)))
# The scientific mode is independent of a particular graph or fixture name.
.bh_centered_final_marginal_mode <- function(spec) {
  .bh_centered_person(spec)&&.bh_task_augmentation(spec)&&!.bh_raw_cor(spec)&&
    !.bh_is_affine_cor_table(spec$tables$correlations)&&
    (!.bh_is_affine_cor_table(spec$tables$parameter_correlations)||
      .bh_cor_primitive_count(spec$tables$parameter_correlations)<=1L)
}
.bh_reference_reporting_supported <- function(spec) .bh_reference_runtime_supported(spec)||.bh_centered_final_marginal_mode(spec)
.bh_final_correlation_reporting <- function(spec) .bh_has_affine_correlations(spec)||.bh_centered_final_marginal_mode(spec)
.bh_final_correlation_metadata_available <- function(spec) {
  primitives<-spec$tables$correlation_primitives;cells<-spec$tables$correlation_affine_cells
  is.data.frame(primitives)&&all(c("layer","parameter")%in%names(primitives))&&
    is.data.frame(cells)&&all(c("layer","row","col","state","parameter","fixed_value")%in%names(cells))&&
    (!nrow(cells)||all(c("endpoint1","endpoint2")%in%names(cells)))
}
.bh_final_correlation_reporting_gate <- function(spec) {
  if(.bh_centered_final_marginal_mode(spec)&&!.bh_final_correlation_metadata_available(spec))
    .mp_abort("E_CORRELATION_REPORT_REBUILD","this accepted historical context predates structured correlation primitive/cell maps; existing scientific inspection remains available, but rebuild from original data, formula and intended priors before the new final-marginal reporting or execution path. The old object is not resealed")
  invisible(TRUE)
}


.bh_strip_optimization_flags <- function(value) {
  # Read shell quoting without evaluating it. Preserve every byte of retained
  # arguments, including quoted macro values and escaped whitespace.
  chars<-strsplit(value,"",fixed=TRUE)[[1L]];n<-length(chars);keep<-rep(TRUE,n);i<-1L
  whitespace<-c(" ","\t","\r","\n")
  while(i<=n) {
    if(chars[i]%in%whitespace){i<-i+1L;next}
    start<-i;quote<-"";word<-character()
    while(i<=n) {
      ch<-chars[i]
      if(quote=="") {
        if(ch%in%whitespace)break
        if(ch%in%c("'",'"')){quote<-ch;i<-i+1L;next}
        if(ch=="\\") {
          if(i==n).mp_abort("E_COMPILER_FLAGS","compiler environment has an incomplete escape; repair CPPFLAGS/CXXFLAGS before compilation")
          if(chars[i+1L]!="\n")word<-c(word,chars[i+1L])
          i<-i+2L;next
        }
      } else if(ch==quote){quote<-"";i<-i+1L;next
      } else if(quote=='"'&&ch=="\\"&&i<n&&chars[i+1L]%in%c("$","`",'"',"\\","\n")) {
        if(chars[i+1L]!="\n")word<-c(word,chars[i+1L])
        i<-i+2L;next
      }
      word<-c(word,ch);i<-i+1L
    }
    if(nzchar(quote)).mp_abort("E_COMPILER_FLAGS","compiler environment has an unterminated quote; repair CPPFLAGS/CXXFLAGS before compilation")
    if(grepl("^-O([0-9]+|g|s|z|fast)?$",paste0(word,collapse="")))keep[seq.int(start,i-1L)]<-FALSE
  }
  trimws(paste0(chars[keep],collapse=""))
}
.bh_reference_compiler_environment <- function() {
  vapply(Sys.getenv(c("CPPFLAGS","CXXFLAGS")),.bh_strip_optimization_flags,character(1))
}
.bh_reference_output_inventory <- function(spec,save_log_lik=FALSE) {
  if(!.bh_reference_reporting_supported(spec)) .mp_abort("E_REFERENCE_OUTPUT_SCOPE","this output inventory describes the supported centered/task raw-common or ordinary final-marginal-common source modes")
  rows<-function(x)if(is.null(x))0L else nrow(x)
  J<-nrow(spec$tables$targets);K<-nrow(spec$tables$components);U<-nrow(spec$tables$parameter_residuals);N<-length(spec$data_map$subject_keys)
  JT<-length(spec$tables$augmentation$target);loads<-.bh_free_loadings(spec);L<-nrow(loads);A<-sum(loads$sign=='positive_anchor')
  W<-rows(spec$tables$share_parts);primitives<-spec$tables$correlation_primitives
  residual_coordinates<-if(.bh_has_affine_correlations(spec))sum(primitives$layer=='parameter')else sum(spec$tables$parameter_correlations$state=='free')
  raw<-.bh_raw_cor(spec);common_coordinates<-if(raw)nrow(spec$tables$raw_correlation_coordinates)else .bh_cor_primitive_count(spec$tables$correlations)
  common_parameter<-stats::setNames(common_coordinates,if(raw)"correlation_B"else"correlation_raw")
  parameters<-c(component_sd=nrow(.bh_free_component_sd(spec)),total_sd=rows(spec$tables$share_targets),share_beta=rows(spec$tables$share_coefficients),
    amplitude=nrow(.bh_amplitudes(spec)),alpha=J,loading_anchor=A,loading_free=L-A,obs_sd=nrow(spec$tables$observation_parameters),
    common_parameter,parameter_sd=nrow(.bh_free_parameter_sd(spec)),
    parameter_correlation_raw=residual_coordinates,z_task=JT*N)
  common_transformed<-if(raw)c(F_component=K*K,R_component=K*K)else
    c(correlation_map=(K+1L)*K,L_component=K*K,R_component=K*K)
  common_jacobian<-stats::setNames(1L,if(raw)"raw_prior_coordinate_log_jacobian"else"correlation_log_jacobian")
  transformed<-c(parameter_correlation_map=(U+1L)*U,common_transformed,L_parameter=U*U,R_parameter=U*U,
    common_jacobian,parameter_correlation_log_jacobian=1L,component_row_sd=K,effective_loadings_work=J*K,
    Lambda_work=J*K,D_parameter_work=J*U,loading=L,eta=N*J,task_row_scale=J,common_task_factor=J*K,parameter_task_factor=J*U,
    Sigma_common_work=J*J,Sigma_parameter_work=J*J,Sigma_predictor_work=J*J,task_covariance_work=JT*JT,L_task_work=JT*JT,
    task_white_scores=JT*N,task_work=JT*N,task_delta=N*J,share_logits=W,share_weight=W,parameter_row_sd_work=U)
  generated<-c(Lambda=J*K,effective_loadings=J*K,D_parameter=J*U,parameter_row_sd=U,Lambda_available=J*K,
    effective_loadings_available=J*K,D_parameter_available=J*U,log_component_row_sd=K,component_covariance=K*K,
    component_covariance_log_abs=K*K,component_covariance_available=K*K,Sigma_common=J*J,Sigma_parameter=J*J,Sigma_predictor=J*J,
    Sigma_common_available=J*J,Sigma_parameter_available=J*J,Sigma_predictor_available=J*J,Sigma_predictor_log_abs=J*J,
    task_white_unconstrained=JT*(N-1L))
  if(save_log_lik)generated<-c(generated,log_lik=sum(vapply(spec$data_map$blocks,nrow,integer(1))))
  blocks<-list(parameters=parameters,transformed=transformed,generated=generated)
  do.call(rbind,lapply(names(blocks),function(block)data.frame(block=block,variable=names(blocks[[block]]),elements=unname(blocks[[block]]),stringsAsFactors=FALSE)))
}
.bh_reference_output_reals <- function(spec,save_log_lik=FALSE) {
  rows<-function(x)if(is.null(x))0L else nrow(x)
  J<-nrow(spec$tables$targets);K<-nrow(spec$tables$components);U<-nrow(spec$tables$parameter_residuals);N<-length(spec$data_map$subject_keys)
  JT<-length(spec$tables$augmentation$target);L<-nrow(.bh_free_loadings(spec));W<-rows(spec$tables$share_parts)
  residual_coordinates<-if(.bh_has_affine_correlations(spec))sum(spec$tables$correlation_primitives$layer=='parameter')else sum(spec$tables$parameter_correlations$state=='free')
  population<-nrow(.bh_free_component_sd(spec))+rows(spec$tables$share_targets)+rows(spec$tables$share_coefficients)+nrow(.bh_amplitudes(spec))+J+L+nrow(spec$tables$observation_parameters)+nrow(spec$tables$raw_correlation_coordinates)+nrow(.bh_free_parameter_sd(spec))+residual_coordinates
  parameters<-population+JT*N
  transformed<-3*U*U+2*U+2*K*K+K+3*J*K+2*J*U+L+2*N*J+J+3*J*J+2*JT*JT+2*JT*N+2*W+2
  generated<-4*J*K+2*J*U+U+K+3*K*K+7*J*J+JT*(N-1L)+if(save_log_lik)sum(vapply(spec$data_map$blocks,nrow,integer(1)))else 0
  out<-list(parameters=parameters,transformed=transformed,generated=generated,
    sampled_white_coordinates=JT*(N-1L),constrained_white_values=JT*N,total=parameters+transformed+generated+66L,
    scope="Declared centered/task/raw program output dimensions, including optional values/flags and a66-field sampler/format allowance; covariance dimensions are separate from sampled white dimensions")
  if((.bh_has_affine_correlations(spec)&&.bh_reference_runtime_supported(spec))||.bh_centered_final_marginal_mode(spec)) {
    inventory<-.bh_reference_output_inventory(spec,save_log_lik)
    for(block in c('parameters','transformed','generated'))out[[block]]<-sum(inventory$elements[inventory$block==block])
    out$total<-out$parameters+out$transformed+out$generated+66L
    out$inventory<-inventory
    out$coordinate_scope<-if(.bh_raw_cor(spec))'Common raw B and unique residual rho are population primitives; constrained task white outputs have N entries per target, while sampled white coordinates have N-1. Expanded matrix aliases add no primitive.'else'Common and residual unique final marginal rhos are scientific primitives; MGS transforms each computational vector jointly. Constrained task whites have N values per target and N-1 sampled axes. Expanded matrix entries add no primitive.'
    if(!.bh_raw_cor(spec))out$scope<-'Declared centered/task/final-marginal common program dimensions; source/reporting support is separate from runtime qualification'
  }
  out
}
.bh_reference_summary_map <- function(spec) {
  empty<-data.frame(variable=character(),parameter_key=character(),parameter_role=character(),target=character(),person_id=character(),basis_index=integer(),raw_block=character(),previous_whitened_coordinate=character(),stringsAsFactors=FALSE)
  if(!.bh_reference_mode(spec))return(empty)
  rows<-list();add<-function(variable,key,role,target=NA_character_,person=NA_character_,basis=NA_integer_,block=NA_character_,previous=NA_character_)
    rows[[length(rows)+1L]]<<-data.frame(variable=variable,parameter_key=key,parameter_role=role,target=target,person_id=person,basis_index=basis,raw_block=block,previous_whitened_coordinate=previous,stringsAsFactors=FALSE)
  raw<-spec$tables$raw_correlation_coordinates
  for(i in seq_len(nrow(raw)))add(paste0("correlation_B[",i,"]"),raw$parameter[i],"cor_raw",block=raw$block[i],previous=raw$previous_whitened_coordinate[i])
  if(.bh_has_affine_correlations(spec)&&.bh_raw_cor(spec)) {
    cells<-spec$tables$correlation_affine_cells
    cells<-cells[cells$layer=='component'&cells$state=='free',,drop=FALSE]
    for(i in seq_len(nrow(cells)))add(paste0('R_component[',cells$row[i],',',cells$col[i],']'),
      .bh_public_id('component_correlation',cells$endpoint1[i],cells$endpoint2[i]),'derived_marginal_component_correlation_entry')
  }
  if(.bh_task_augmentation(spec)) {
    target<-spec$tables$augmentation$target;index<-spec$tables$augmentation$target_index;roster<-spec$tables$person_roster;N<-nrow(roster)
    for(j in seq_along(target)) {
      for(k in seq_len(N-1L))add(paste0("task_white_unconstrained[",j,",",k,"]"),.bh_public_id("task_white",target[j],as.character(k)),"task_white_basis_coordinate",target=target[j],basis=k)
      for(n in seq_len(N)) {
        add(paste0("z_task[",j,",",n,"]"),.bh_public_id("centered_task_white",target[j],roster$person_key[n]),"derived_centered_task_white_score",target=target[j],person=roster$person_id[n])
        add(paste0("task_delta[",n,",",index[j],"]"),.bh_public_id("task_deviation",target[j],roster$person_key[n]),"derived_task_deviation",target=target[j],person=roster$person_id[n])
      }
    }
  }
  if(length(rows))do.call(rbind,rows)else empty
}
.bh_reference_cache_metadata <- function(spec,map) {
  .bh_final_correlation_reporting_gate(spec)
  list(person_law=spec$tables$person_law,person_roster=spec$tables$person_roster,
    augmentation=spec$tables$augmentation,task_target_domain=spec$tables$task_target_domain,
    latent_coordinate_domains=spec$tables$task_latent_coordinates,correlation_coordinates=spec$tables$correlation_coordinate_system,
    ordered_raw_blocks=spec$tables$correlation_blocks,raw_correlation_coordinates=spec$tables$raw_correlation_coordinates,
    resolved_priors=spec$tables$priors,summary_map=map,
    score_interpretation="Task-white basis coordinates have N-1 axes, not person IDs. Centered white scores and task deviations use the full ordered person roster. Common-factor and parameter-residual generative scores are unsampled under task augmentation.",
    white_output_identity="Constrained z_task and generated task_white_unconstrained fields use their actual indexed output coordinates. The Stan2.40 log_prob gradient display-label issue does not reorder these cached fields.",
    prior_measure=if(.bh_raw_cor(spec))"Raw B cells carry their specified Normal densities once; final component R entries are derived. The centered Gaussian measure is on each N-1-dimensional isometric subspace; no task-transform Jacobian is added."else"Each free final-marginal layer has one joint determinant density and one joint MGS or one-label chart Jacobian. Common R free entries are scientific primitives, not raw B. The centered Gaussian measure is on each N-1-dimensional isometric subspace; no task-transform Jacobian is added.")
}
.bh_reference_report_cache <- function(backend,spec) {
  J<-nrow(spec$tables$targets);K<-nrow(spec$tables$components);U<-nrow(spec$tables$parameter_residuals)
  dims<-list(Sigma_common_available=c(J,J),Sigma_parameter_available=c(J,J),Sigma_predictor_available=c(J,J),
    Lambda_available=c(J,K),effective_loadings_available=c(J,K),D_parameter_available=c(J,U),component_covariance_available=c(K,K))
  dims<-Filter(function(x)prod(x)>0L,dims)
  if(!length(dims))return(list(status="not_applicable",reason="there are no covariance or loading report entries",unavailable_values=0L))
  labels<-function(name,d)paste0(name,"[",rep(seq_len(d[1]),d[2]),",",rep(seq_len(d[2]),each=d[1]),"]")
  tryCatch({
    flags<-as.matrix(backend$draws(variables=names(dims),format="draws_matrix"));class(flags)<-c("matrix","array")
    expected<-unlist(Map(labels,names(dims),dims),use.names=FALSE)
    if(!all(expected%in%colnames(flags)))stop("availability fields do not cover every expected task covariance/loading entry")
    flags<-flags[,expected,drop=FALSE]
    if(any(!is.finite(flags))||any(!flags%in%c(0,1)))stop("availability fields must contain zero/one flags")
    log_fields<-c(if(J)"Sigma_predictor_log_abs",if(K)c("log_component_row_sd","component_covariance_log_abs"))
    expected_logs<-c(if(J)labels("Sigma_predictor_log_abs",c(J,J)),if(K)c(paste0("log_component_row_sd[",seq_len(K),"]"),labels("component_covariance_log_abs",c(K,K))))
    logs<-tryCatch({x<-as.matrix(backend$draws(variables=log_fields,format="draws_matrix"));class(x)<-c("matrix","array");if(!all(expected_logs%in%colnames(x)))stop("log metadata does not cover expected report entries");x[,expected_logs,drop=FALSE]},error=function(e)list(status="not_assessed",reason=conditionMessage(e)))
    list(status=if(any(flags==0))"unavailable_values"else"available",unavailable_values=sum(flags==0),flags=flags,
      entries=data.frame(variable=colnames(flags),unavailable_draws=colSums(flags==0),total_draws=rep(nrow(flags),ncol(flags)),stringsAsFactors=FALSE),
      log_magnitudes=logs,log_metadata_semantics="Negative infinity denotes exact zero magnitude. Explicit availability flags distinguish unrepresentable nonzero values; finite log metadata does not certify matrix positive definiteness.",
      reason="Normalized task Gram factors and required task deviations define the target. Optional full covariance/loading reports can be unavailable; every draw is retained and unavailable values are not replaced by zero.")
  },error=function(e)list(status="not_assessed",reason=paste("optional task-report cache unavailable:",conditionMessage(e)),unavailable_values=NA_integer_))
}
