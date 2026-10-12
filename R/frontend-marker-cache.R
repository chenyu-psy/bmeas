# Primitive and derived marker estimands retain distinct public identities.
.bh_marker_summary_map <- function(spec) {
  empty<-data.frame(variable=character(),parameter_key=character(),parameter_role=character(),target=character(),component=character(),component2=character(),person=character(),units=character(),declared_beta_primitive=character(),component_sd_parameter=character(),component_sd_parameter2=character(),stringsAsFactors=FALSE)
  if(!.bh_has_marker(spec))return(empty)
  rows<-list();add<-function(variable,key,role,target=NA_character_,component=NA_character_,component2=NA_character_,person=NA_character_,units="numeric_latent_coordinate",beta=NA_character_,sd=NA_character_,sd2=NA_character_)
    rows[[length(rows)+1L]]<<-data.frame(variable=variable,parameter_key=key,parameter_role=role,target=target,component=component,component2=component2,person=person,units=units,declared_beta_primitive=beta,component_sd_parameter=sd,component_sd_parameter2=sd2,stringsAsFactors=FALSE)
  cells<-spec$tables$loading_cells;scales<-spec$tables$component_scales
  for(i in seq_len(nrow(cells)))if(cells$effective_support[i]&&(cells$state[i]!="fixed"||scales$state[cells$component_index[i]]=="free"))
    add(paste0("effective_loadings[",cells$target_index[i],",",cells$component_index[i],"]"),
      .bh_public_id("effective_loading",spec$tables$coordinates$id[cells$component_index[i]],cells$target[i]),"derived_effective_loading",
      target=cells$target[i],component=scales$component[cells$component_index[i]],units=cells$units[i],
      beta=if(cells$state[i]=="free")cells$parameter[i]else NA_character_,sd=scales$parameter[cells$component_index[i]])
  K<-nrow(scales);people<-spec$data_map$subject_keys
  for(k in seq_len(K))for(n in seq_along(people)) {
    add(paste0("z[",k,",",n,"]"),.bh_public_id("standardized_component_score",scales$component[k],people[n]),"standardized_component_person_score",component=scales$component[k],person=people[n])
    add(paste0("f[",k,",",n,"]"),.bh_public_id("scaled_component_score",scales$component[k],people[n]),"scaled_component_person_score",component=scales$component[k],person=people[n],sd=scales$parameter[k])
  }
  mask<-spec$tables$correlation_mask
  fixed_nonzero<-matrix(FALSE,K,K);tab<-spec$tables$correlations
  if("fixed_value"%in%names(tab))for(i in which(tab$state=="fixed" & !is.na(tab$fixed_value) & tab$fixed_value!=0))fixed_nonzero[tab$row[i],tab$col[i]]<-fixed_nonzero[tab$col[i],tab$row[i]]<-TRUE
  for(k in seq_len(K))for(l in seq_len(k))if((k==l&&scales$state[k]=="free")||(k!=l&&(mask[k,l]=="free"||(fixed_nonzero[k,l]&&(scales$state[k]=="free"||scales$state[l]=="free")))))
    add(paste0("component_covariance[",k,",",l,"]"),.bh_public_id("component_covariance",scales$component[k],scales$component[l]),"derived_component_covariance",
      component=scales$component[k],component2=scales$component[l],units="numeric_latent_coordinate_squared",sd=scales$parameter[k],sd2=scales$parameter[l])
  if(length(rows))do.call(rbind,rows)else empty
}
.bh_report_availability_cache <- function(backend,spec) {
  J<-nrow(spec$tables$targets);K<-nrow(spec$tables$components);N<-length(spec$data_map$subject_keys)
  matrix_names<-function(field,n,m)if(n*m)paste0(field,"[",rep(seq_len(n),m),",",rep(seq_len(m),each=n),"]")else character()
  fields<-c(if(J)c("Sigma_common_available","Sigma_parameter_available","Sigma_predictor_available"),
    if(K)c("component_covariance_available","f_available"))
  if(!length(fields))return(list(status="not_applicable",reason="no latent covariance or component score entries to report",unavailable_values=0L))
  tryCatch({
    flags<-as.matrix(backend$draws(variables=fields,format="draws_matrix"));class(flags)<-c("matrix","array")
    expected<-c(unlist(lapply(c("Sigma_common_available","Sigma_parameter_available","Sigma_predictor_available"),matrix_names,J,J),use.names=FALSE),
      matrix_names("component_covariance_available",K,K),matrix_names("f_available",K,N))
    if(!all(expected%in%colnames(flags)))stop("availability fields do not cover every expected covariance/score entry")
    flags<-flags[,expected,drop=FALSE]
    if(!ncol(flags)||any(!is.finite(flags))||any(!flags%in%c(0,1)))stop("availability fields must contain only zero/one flags")
    table<-data.frame(variable=colnames(flags),unavailable_draws=colSums(flags==0),total_draws=rep(nrow(flags),ncol(flags)),stringsAsFactors=FALSE)
    logs<-if(K)tryCatch({
      x<-as.matrix(backend$draws(variables=c("log_component_row_sd","component_covariance_log_abs","f_log_abs"),format="draws_matrix"));class(x)<-c("matrix","array")
      expected_logs<-c(paste0("log_component_row_sd[",seq_len(K),"]"),matrix_names("component_covariance_log_abs",K,K),matrix_names("f_log_abs",K,N))
      if(!all(expected_logs%in%colnames(x)))stop("log metadata does not cover expected component covariance/scaled-score entries")
      x[,expected_logs,drop=FALSE]
    },error=function(e)list(status="not_assessed",reason=conditionMessage(e)))else matrix(numeric(),nrow(flags),0L)
    list(status=if(any(flags==0))"unavailable_values"else"available",unavailable_values=sum(flags==0),flags=flags,entries=table,
      log_magnitudes=logs,log_metadata_semantics="negative infinity is legitimate for an exact zero correlation or score; availability comes from explicit flags, not a blanket finite-log check",
      reason="optional covariance or scaled-score reports can be unavailable without changing the A*z likelihood; all draws are retained and unavailable values are not replaced by zero or silently omitted")
  },error=function(e)list(status="not_assessed",reason=paste("optional report cache unavailable:",conditionMessage(e)),unavailable_values=NA_integer_))
}
