# Lifecycle representation for the separately qualified bounded affine profiles.
.bh_affine_product_runtime_supported <- function(spec,plan=NULL) {
  if(!isTRUE(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1"))||.bh_reference_mode(spec))return(FALSE)
  if(is.null(plan))plan<-tryCatch(.bh_affine_lowering_plan(spec),error=function(e)NULL)
  if(is.null(plan)||!all(vapply(plan,function(p)p$mode%in%0:3,logical(1)))||!any(vapply(plan,function(p)identical(p$mode,3L),logical(1))))return(FALSE)
  canonical<-gaussian_model();callbacks<-c("validate_data","validate_bindings","informative_rows")
  all(vapply(spec$models,function(b) {
    cap<-b$model$capabilities;fit<-cap$available[cap$capability=="fit"]
    identical(.bh_builtin_kind(b$model),"gaussian")&&length(fit)==1L&&isTRUE(fit)&&identical(b$model[callbacks],canonical[callbacks])
  },logical(1)))
}
.bh_affine_runtime_supported <- function(spec) {
  if(!.bh_has_affine_correlations(spec))return(FALSE)
  plan<-tryCatch(.bh_affine_lowering_plan(spec),error=function(e)NULL)
  if(is.null(plan))return(FALSE)
  if(any(vapply(plan,function(p)identical(p$mode,3L),logical(1))))return(.bh_affine_product_runtime_supported(spec,plan))
  if(!all(vapply(plan,function(p)p$mode%in%0:2,logical(1))))return(FALSE)
  if(!.bh_reference_mode(spec))return(TRUE)
  .bh_reference_runtime_supported(spec)&&plan$component$mode==0L&&plan$parameter$mode%in%1:2
}
.bh_affine_summary_map <- function(spec) {
  .bh_final_correlation_reporting_gate(spec)
  empty<-data.frame(variable=character(),parameter_key=character(),parameter_role=character(),layer=character(),computational_variable=character(),computational_mapping=character(),stringsAsFactors=FALSE)
  empty$alias_cells<-I(list())
  if(!.bh_final_correlation_reporting(spec))return(empty)
  rows<-list()
  for(layer in c('component','parameter')) {
    # The raw-B common scaffold also uses state='free', but its R entries are
    # derived. Only the resolved primitive table defines a marginal primitive.
    tab<-spec$tables$correlation_affine_cells
    tab<-tab[tab$layer==layer,,drop=FALSE]
    primitives<-spec$tables$correlation_primitives
    ids<-primitives$parameter[primitives$layer==layer]
    plan<-.bh_affine_layer_plan(spec,layer)
    for(j in seq_along(ids)) {
      at<-which(!is.na(tab$parameter)&tab$parameter==ids[j]);representative<-at[1L]
      row<-data.frame(variable=paste0('R_',layer,'[',tab$row[representative],',',tab$col[representative],']'),
        parameter_key=ids[j],parameter_role='unique_marginal_correlation',layer=layer,
        computational_variable=if(plan$mode%in%c(2L,3L))paste0(if(layer=='component')'correlation_raw'else'parameter_correlation_raw','[',if(plan$mode==2L)1L else j,']')else NA_character_,
        computational_mapping=if(plan$mode==2L)'one_label_interval_coordinate'else if(plan$mode==3L)'unique_label_product_interval_coordinate'else'MGS_vector_map_no_scalar_association',stringsAsFactors=FALSE)
      aliases<-tab[at,c('row','col','endpoint1','endpoint2'),drop=FALSE]
      attr(aliases,'row.names')<-as.integer(at)
      row$alias_cells<-I(list(aliases))
      rows[[length(rows)+1L]]<-row
    }
  }
  if(length(rows))do.call(rbind,rows)else empty
}
.bh_affine_matrix_cache <- function(backend,spec) {
  .bh_final_correlation_reporting_gate(spec)
  if(!.bh_final_correlation_reporting(spec))return(NULL)
  tryCatch({
    variables<-c(if(nrow(spec$tables$components))'R_component',if(nrow(spec$tables$parameter_residuals))'R_parameter')
    if(!length(variables))return(list(status='not_applicable',entries=data.frame(),scope='No common or parameter-residual correlation matrix is present; no backend summary is requested.'))
    result<-as.data.frame(backend$summary(variables=variables,'mean','median','sd','quantile2','rhat','ess_bulk','ess_tail','mcse_mean'))
    cells<-list()
    for(layer in c('component','parameter')) {
      tab<-spec$tables$correlation_affine_cells
      tab<-tab[tab$layer==layer,,drop=FALSE]
      primitives<-spec$tables$correlation_primitives
      ids<-primitives$parameter[primitives$layer==layer]
      raw<-layer=='component'&&.bh_raw_cor(spec)
      n<-if(layer=='component')nrow(spec$tables$components)else nrow(spec$tables$parameter_residuals)
      for(j in seq_len(n))for(i in seq_len(n)) {
        at<-if(i==j)integer()else which(tab$row==min(i,j)&tab$col==max(i,j))
        free<-length(at)==1L&&!is.na(tab$parameter[at])&&tab$parameter[at]%in%ids
        derived<-length(at)==1L&&raw&&tab$state[at]=='free'
        value<-if(i==j)1 else if(free||derived)NA_real_ else tab$fixed_value[at]
        cells[[length(cells)+1L]]<-data.frame(variable=paste0('R_',layer,'[',i,',',j,']'),layer=layer,row=i,col=j,
          parameter_key=if(free)tab$parameter[at]else NA_character_,role=if(free)'expanded_correlation_alias'else if(derived)'derived_common_correlation'else'fixed_correlation_constant',fixed_value=value,stringsAsFactors=FALSE)
      }
    }
    metadata<-if(length(cells))do.call(rbind,cells)else data.frame()
    if(nrow(metadata)) {
      stopifnot(setequal(result$variable,metadata$variable));at<-match(result$variable,metadata$variable)
      for(field in setdiff(names(metadata),'variable'))result[[field]]<-metadata[[field]][at]
      fixed<-result$role=='fixed_correlation_constant'
      for(field in intersect(c('sd','rhat','ess_bulk','ess_tail','mcse_mean'),names(result)))result[[field]][fixed]<-NA_real_
    }
    list(status='available',entries=result,scope='Expanded matrix cells are derived aliases or fixed constants, not additional prior coordinates or independent diagnostic monitors. Fixed cells have no posterior uncertainty diagnostics.')
  },error=function(e)list(status='not_assessed',reason=conditionMessage(e)))
}
.bh_affine_cache_metadata <- function(spec,map) {
  .bh_final_correlation_reporting_gate(spec)
  product<-any(vapply(.bh_affine_lowering_plan(spec),function(p)identical(p$mode,3L),logical(1)))
  product_runtime<-product&&.bh_affine_product_runtime_supported(spec)
  product_source_only<-product&&!product_runtime
  list(
  version=if(product_runtime)if(.bh_affine_layer_plan(spec,'parameter')$mode==3L)'iid_gaussian_residual_dual_product_live_cache_v1'else'iid_gaussian_common_product_live_cache_v1'else if(product_source_only)'affine_product_source_only_no_cache_qualification'else if(.bh_centered_final_marginal_mode(spec))'centered_final_marginal_cache_v1'else if(.bh_reference_mode(spec))'reference_affine_residual_cache_v1'else'unique_marginal_correlation_cache_v2',primitive_summary_map=map,
  primitives=spec$tables$correlation_primitives,cells=spec$tables$correlation_affine_cells,
  domains=spec$tables$correlation_domains,prior_geometry=spec$tables$correlation_prior_geometry,
  coordinate_scope='Scientific primitives are unique final marginal rho labels. The one-label interval chart has one z coordinate. The product chart has one interval z per unique label, even if that label spans disconnected blocks. Existing MGS transforms the whole raw layer vector jointly; no scalar raw-to-rho correspondence is asserted. Repeated matrix cells and computational coordinates are distinct roles. Fully fixed layers have no correlation prior coordinate.',
  runtime_scope=if(product_runtime)'explicit experimental iid canonical Gaussian common and/or residual product chart with remaining layers in mode0/1/2; bounded fixed-state arithmetic and live cache controls only; saved-value product profile supports durable postprocessing with active PPC closed; HMC and recovery unvalidated'else if(product_source_only)'source-only product chart; execution and cache lifecycle remain unqualified and closed'else if(.bh_centered_final_marginal_mode(spec))'explicit experimental centered/task ordinary final-marginal common MGS with residual MGS, fixed or one-label maps; bounded fixed-state evidence only'else if(.bh_reference_mode(spec)) {
    if(.bh_affine_runtime_supported(spec))'explicit experimental centered/task/ordered raw common plus separate fixed or one-label affine residual layer; bounded fixed-state evidence only'else
      'source-only reference/affine combination; no qualified runtime or cache profile'
  }else'iid people and common-plus-residual final-marginal augmentation only; no raw/centered/task affine runtime profile')

}
