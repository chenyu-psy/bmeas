# Explicit population-monitor discovery uses saved tables only. No source,
# provider, namespace, scientific rebuild or person expansion is triggered.
.bh_diag_error <- function(message, code="E_DIAGNOSTIC_CONTEXT") .mp_abort(code,message)
.bh_diag_plain <- function(x,types) typeof(x)%in%types && !is.object(x) &&
  !isS4(x) && is.null(attributes(x))
.bh_diag_names <- function(x,label) {
  n<-attr(x,"names",exact=TRUE)
  if(length(x) && (!.bh_diag_plain(n,"character") || length(n)!=length(x) ||
    anyNA(n) || any(!validEnc(n)) || any(!nzchar(n)) || base::anyDuplicated.default(n)))
    .bh_diag_error(paste(label,"requires distinct plain valid field names"))
  if(!length(x) && !is.null(n) && (!.bh_diag_plain(n,"character") || length(n)))
    .bh_diag_error(paste(label,"has invalid empty names"))
  n
}
.bh_diag_list <- function(x,label,classes=NULL) {
  if(typeof(x)!="list" || isS4(x) || !is.null(attr(x,"dim",exact=TRUE)) ||
     any(!names(attributes(x))%in%c("names",if(!is.null(classes))"class")) ||
     (is.object(x) && (is.null(classes) || !all(attr(x,"class",exact=TRUE)%in%classes))))
    .bh_diag_error(paste(label,"must be a saved plain list"))
  x<-unclass(x);.bh_diag_names(x,label)
  for(i in seq_along(x)) if(identical(.subset2(x,i),quote(expr=)))
    .bh_diag_error(paste(label,"has a missing argument field"))
  x
}
.bh_diag_field <- function(x,name,required=TRUE) {
  if(!name%in%names(x)) {
    if(required).bh_diag_error(paste("saved context is missing",name))
    return(NULL)
  }
  .subset2(x,name)
}
.bh_diag_count <- function(x,label,minimum=0,maximum=2^53-1) {
  if(!.bh_diag_plain(x,c("integer","double")) || length(x)!=1L ||
    is.na(x) || !is.finite(x) || x!=floor(x) || x<minimum || x>maximum)
    .bh_diag_error(paste(label,"must be a plain finite exact count"),"E_DIAGNOSTIC_BUDGET")
  as.double(x)
}
.bh_diag_sum <- function(x,label="count") {
  total<-0
  for(v in x) {
    .bh_diag_count(v,label)
    if(v>2^53-1-total).bh_diag_error(paste(label,"exceeds exact representation"),"E_DIAGNOSTIC_BUDGET")
    total<-total+v
  }
  total
}
.bh_diag_budget <- function(budget) {
  budget<-.bh_diag_list(budget,"diagnostic discovery budget")
  if(!setequal(names(budget),c("max_rows","max_context_rows")) || length(budget)!=2L)
    .bh_diag_error("discovery budget needs exactly max_rows and max_context_rows","E_DIAGNOSTIC_BUDGET")
  list(max_rows=.bh_diag_count(budget$max_rows,"max_rows",0,.Machine$integer.max),
    max_context_rows=.bh_diag_count(budget$max_context_rows,"max_context_rows",1,.Machine$integer.max))
}
.bh_diag_table_rows <- function(x,label) {
  if(typeof(x)!="list" || isS4(x) || !identical(attr(x,"class",exact=TRUE),"data.frame"))
    .bh_diag_error(paste(label,"must be a saved plain data.frame"))
  .bh_diag_names(unclass(x),label)
  n<-.row_names_info(x,2L)
  .bh_diag_count(n,paste(label,"rows"),0,.Machine$integer.max)
}
.bh_diag_column <- function(tab,name,types="character",missing=FALSE,optional=FALSE) {
  n<-.row_names_info(tab,2L);x<-unclass(tab)
  empty<-function()if("character"%in%types)character()else if("double"%in%types)numeric()else
    if("integer"%in%types)integer()else logical()
  if(!name%in%names(x)) {
    # Some qualified zero-domain builders start from data.frame() and attach
    # only index columns. An absent column then contains no omitted value.
    if(n==0L)return(empty())
    if(optional)return(rep(NA_character_,n))
    .bh_diag_error(paste("saved table lacks column",name))
  }
  value<-.subset2(x,name)
  # ifelse(logical(0), text, text) produces logical(0). Normalize only truly
  # empty plain atomic columns; populated mismatches and all objects refuse.
  if(n==0L && .bh_diag_plain(value,c("character","integer","double","logical")) &&
    length(value)==0L)return(empty())
  if(!.bh_diag_plain(value,types) || length(value)!=n || (!missing && anyNA(value)))
    .bh_diag_error(paste("saved column is nonplain or inconsistent:",name))
  if(typeof(value)=="character" && (any(!validEnc(value[!is.na(value)])) ||
     (!missing && any(!nzchar(value)))))
    .bh_diag_error(paste("saved column has invalid text:",name))
  value
}
# Construct only the package-owned fixed schema; no data-frame conversion,
# row-binding, AsIs or caller subsetting method is needed for these plain values.
.bh_diag_catalogue_frame <- function(rows) {
  text_fields<-c("key","label","role","layer","units","support","source_kind",
    "source_variable","display_alias")
  logical_fields<-c("prior_coordinate","eligible","fixed")
  order<-c("key","label","role","layer","units","support","source_kind",
    "prior_coordinate","eligible","fixed","source_variable","display_alias","source_aliases")
  values<-list()
  for(name in text_fields)values[[name]]<-vapply(rows,function(row).subset2(row,name),character(1))
  for(name in logical_fields)values[[name]]<-vapply(rows,function(row).subset2(row,name),logical(1))
  values$source_aliases<-structure(lapply(rows,function(row).subset2(row,"source_aliases")),class="AsIs")
  structure(values[order],class="data.frame",row.names=.set_row_names(as.integer(length(rows))))
}
.bh_diag_empty_catalogue <- function() .bh_diag_catalogue_frame(list())
.bh_diagnostic_catalogue <- function(fit,budget) {
  cap<-.bh_diag_budget(budget)
  portable<-.bh_pf_is_view(fit)
  if(typeof(fit)!="list" || isS4(fit) || (!inherits(fit,"bhmm_fit")&&!portable) || inherits(fit,"measurement_fit"))
    .bh_diag_error("diagnostic discovery requires a retained main-API bmeas fit","E_FIT")
  fit<-.bh_diag_list(fit,"fit",if(portable)"bmeas_portable_consumer_view"else c("bmeas_fit","bhmm_fit"))
  original<-.bh_diag_field(fit,"spec")
  if(!inherits(original,"bhmm_spec")&&!(portable&&identical(.bh_pf_class(original),"bmeas_portable_context"))) .bh_diag_error("a retained structured specification is required")
  spec<-.bh_diag_list(original,"specification",if(portable)"bmeas_portable_context"else c("bmeas_spec","bhmm_spec"))
  tables<-.bh_diag_list(.bh_diag_field(spec,"tables"),"specification tables")
  required<-c("targets","parameter_roles","free_loadings","loading_primitives","loading_cells",
    "component_scales","amplitudes","observation_parameters","parameter_residuals",
    "correlation_primitives","correlation_affine_cells","raw_correlation_coordinates",
    "components","coordinates","priors")
  optional<-c("share_targets","share_coefficients","share_parts")
  counts<-setNames(numeric(length(c(required,optional))),c(required,optional))
  saved<-list();absent<-character()
  for(name in names(counts)) {
    value<-.bh_diag_field(tables,name,name%in%required)
    if(is.null(value)) { absent<-c(absent,name);next }
    counts[[name]]<-.bh_diag_table_rows(value,name)
    saved[[name]]<-value
  }
  base_rows<-.bh_diag_sum(unname(counts),"consumed metadata rows")
  if(base_rows>cap$max_context_rows)
    .bh_diag_error("saved table rows exceed max_context_rows before catalogue expansion","E_DIAGNOSTIC_BUDGET")
  if(length(absent) && length(absent)!=length(optional))
    .bh_diag_error("share target, coefficient and part tables must be present together")
  col<-function(table,name,types="character",missing=FALSE,optional=FALSE)
    .bh_diag_column(saved[[table]],name,types,missing,optional)
  keys<-function(table,name="parameter") {
    x<-col(table,name)
    if(base::anyDuplicated.default(x)) .bh_diag_error(paste(table,"has duplicate primitive keys"))
    x
  }
  target_keys<-keys("targets","key")
  loading_keys<-keys("free_loadings")
  if(!identical(loading_keys,keys("loading_primitives")))
    .bh_diag_error("free loading and primitive maps disagree")
  scale_state<-col("component_scales","state")
  if(any(!scale_state%in%c("free","fixed"))) .bh_diag_error("unknown component scale state")
  free_scale<-which(scale_state=="free")
  scale_keys<-col("component_scales","parameter",missing=TRUE)[free_scale]
  if(anyNA(scale_keys)||any(!nzchar(scale_keys))||base::anyDuplicated.default(scale_keys))
    .bh_diag_error("free component scale keys are invalid")
  amplitude_keys<-keys("amplitudes")
  obs_keys<-keys("observation_parameters","key")
  residual_keys<-col("parameter_residuals","parameter",missing=TRUE)
  residual_expression<-if("expression"%in%names(unclass(saved$parameter_residuals)))
    col("parameter_residuals","expression")else rep("free_parameter_sd",counts[["parameter_residuals"]])
  if(any(!residual_expression%in%c("free_parameter_sd","share_derived")))
    .bh_diag_error("unknown residual scale expression")
  free_residual<-which(residual_expression=="free_parameter_sd")
  if(anyNA(residual_keys[free_residual])||any(!nzchar(residual_keys[free_residual]))||
    base::anyDuplicated.default(residual_keys[free_residual])) .bh_diag_error("free residual scale keys are invalid")
  total_keys<-if(length(absent))character()else keys("share_targets")
  share_keys<-if(length(absent))character()else keys("share_coefficients")
  rho_keys<-keys("correlation_primitives")
  rho_layer<-col("correlation_primitives","layer")
  if(any(!rho_layer%in%c("component","parameter"))) .bh_diag_error("unknown primitive correlation layer")
  raw_keys<-keys("raw_correlation_coordinates")
  coordinate_system<-.bh_diag_list(.bh_diag_field(tables,"correlation_coordinate_system"),"correlation coordinates")
  coordinate_type<-.bh_diag_field(coordinate_system,"type")
  if(!.bh_diag_plain(coordinate_type,"character")||length(coordinate_type)!=1L||
    is.na(coordinate_type)||!validEnc(coordinate_type)||
    !coordinate_type%in%c("ordered_blocks","final_marginal_mask","affine_final_marginal_labels"))
    .bh_diag_error("unsupported saved correlation coordinate system")
  raw<-identical(coordinate_type,"ordered_blocks")
  if(!raw && length(raw_keys)) .bh_diag_error("raw coordinates contradict the saved marginal law")
  cell_layer<-col("correlation_affine_cells","layer")
  cell_state<-col("correlation_affine_cells","state")
  cell_key<-col("correlation_affine_cells","parameter",missing=TRUE)
  cell_row<-col("correlation_affine_cells","row",c("integer","double"))
  cell_col<-col("correlation_affine_cells","col",c("integer","double"))
  endpoint1<-col("correlation_affine_cells","endpoint1")
  endpoint2<-col("correlation_affine_cells","endpoint2")
  if(any(!cell_layer%in%c("component","parameter"))||any(!cell_state%in%c("free","zero","fixed"))||
    any(!is.finite(cell_row)|!is.finite(cell_col))||any(cell_row!=floor(cell_row)|cell_col!=floor(cell_col))||
    any(cell_row<1|cell_col<=cell_row)) .bh_diag_error("invalid correlation cell map")
  bounds<-ifelse(cell_layer=="component",counts[["components"]],counts[["parameter_residuals"]])
  if(any(cell_col>bounds)||base::anyDuplicated.default(paste(cell_layer,cell_row,cell_col,sep=":")))
    .bh_diag_error("correlation cells exceed or duplicate their declared domain")
  primitive_at<-match(cell_key,rho_keys)
  matched<-which(!is.na(primitive_at))
  if(length(rho_keys) && (any(tabulate(primitive_at[matched],nbins=length(rho_keys))==0L)||
     any(cell_state[matched]!="free")||any(cell_layer[matched]!=rho_layer[primitive_at[matched]])))
    .bh_diag_error("correlation primitive and alias cells disagree")
  ordinary_free<-cell_state=="free" & !(raw & cell_layer=="component")
  if(anyNA(cell_key[ordinary_free])||any(!cell_key[ordinary_free]%in%rho_keys)||
    (raw && any(rho_layer=="component"))) .bh_diag_error("free correlation cells lack exact primitive identities")
  derived<-which(raw & cell_layer=="component" & cell_state=="free")
  group_counts<-c(intercepts=length(target_keys),loadings=length(loading_keys),
    component_sd=length(scale_keys),amplitudes=length(amplitude_keys),observation_sd=length(obs_keys),
    parameter_sd=length(free_residual),total_sd=length(total_keys),share_coefficients=length(share_keys),
    final_rho=length(rho_keys),raw_B=length(raw_keys),derived_common_R=length(derived))
  M<-.bh_diag_sum(unname(group_counts),"catalogue rows")
  if(M>cap$max_rows) .bh_diag_error("complete catalogue exceeds max_rows; no monitor was truncated","E_DIAGNOSTIC_BUDGET")
  alias_count<-.bh_diag_sum(c(M-length(rho_keys),sum(ordinary_free)),"expanded source alias references")
  notice_count<-4
  context_total<-.bh_diag_sum(c(base_rows,alias_count,notice_count),"complete metadata rows")
  if(context_total>cap$max_context_rows)
    .bh_diag_error("catalogue aliases and notices exceed max_context_rows before expansion","E_DIAGNOSTIC_BUDGET")
  # Allocate alias groups once after admission, rather than repeatedly scanning
  # the full edge table separately for every correlation primitive.
  alias_groups<-base::split.default(matched,factor(primitive_at[matched],levels=seq_along(rho_keys)))
  rows<-vector("list",M);next_row<-0L
  add<-function(key,label,role,layer,unit,support,variable,kind="scientific_primitive",prior=TRUE,
                aliases=variable,display=NA_character_) {
    next_row<<-next_row+1L
    row<-list(key=key,label=label,role=role,layer=layer,units=unit,support=support,
      source_kind=kind,prior_coordinate=prior,eligible=TRUE,fixed=FALSE,source_variable=variable,
      display_alias=display,source_aliases=aliases)
    rows[[next_row]]<<-row
  }
  target_units<-col("targets","predictor_units",missing=TRUE,optional=TRUE)
  target_labels<-paste(col("targets","model"),col("targets","task"),col("targets","parameter"),sep=" / ")
  target_label<-function(key) {
    at<-match(key,target_keys)
    if(anyNA(at)).bh_diag_error("a saved population label references an unknown target")
    target_labels[at]
  }
  for(i in seq_along(target_keys)) add(paste0("Intercept:",target_keys[i]),paste("Intercept for",target_labels[i]),
    "Intercept","population",target_units[i],"real",paste0("alpha[",i,"]"))
  loading_support<-col("free_loadings","support",missing=TRUE,optional=TRUE)
  loading_units<-col("free_loadings","units",missing=TRUE,optional=TRUE)
  loading_components<-col("free_loadings","component")
  loading_targets<-col("free_loadings","target")
  declared_loading_keys<-col("loading_cells","parameter",missing=TRUE)
  loading_members<-tabulate(match(declared_loading_keys,loading_keys),nbins=length(loading_keys))
  if(length(loading_keys)&&any(loading_members<1L)).bh_diag_error("a loading primitive has no declared member")
  for(i in seq_along(loading_keys)) add(loading_keys[i],paste(
    if(loading_members[i]>1L)paste0("Shared loading (",loading_members[i]," cells):")else"Loading:",
    loading_components[i],"to",target_label(loading_targets[i])),"loading","component",
    loading_units[i],loading_support[i],paste0("loading[",i,"]"))
  scale_units<-col("component_scales","units",missing=TRUE,optional=TRUE)
  scale_names<-col("component_scales","component")
  for(i in seq_along(free_scale)) { at<-free_scale[i];add(scale_keys[i],paste("Component SD for",scale_names[at]),
    "component_sd","component",scale_units[at],"positive",paste0("component_sd[",i,"]")) }
  amplitude_units<-col("amplitudes","units",missing=TRUE,optional=TRUE)
  amplitude_components<-col("amplitudes","component")
  for(i in seq_along(amplitude_keys)) add(amplitude_keys[i],paste("Shared basis amplitude for",amplitude_components[i]),"amplitude","component",
    amplitude_units[i],"positive",paste0("amplitude[",i,"]"))
  obs_units<-col("observation_parameters","units",missing=TRUE,optional=TRUE)
  obs_labels<-paste(col("observation_parameters","model"),col("observation_parameters","task"),col("observation_parameters","parameter"),sep=" / ")
  for(i in seq_along(obs_keys)) add(paste0("observation_sd:",obs_keys[i]),paste("Observation SD for",obs_labels[i]),
    "observation_sd","observation",obs_units[i],"positive",paste0("obs_sd[",i,"]"))
  residual_units<-col("parameter_residuals","units",missing=TRUE,optional=TRUE)
  residual_targets<-col("parameter_residuals","target")
  for(i in seq_along(free_residual)) { at<-free_residual[i];add(residual_keys[at],paste("Residual SD for",target_label(residual_targets[at])),
    "parameter_sd","parameter",residual_units[at],"positive",paste0("parameter_sd[",i,"]")) }
  if(length(total_keys)) {
    units<-col("share_targets","units",missing=TRUE,optional=TRUE)
    total_targets<-col("share_targets","target");total_owners<-col("share_targets","owner")
    for(i in seq_along(total_keys)) add(total_keys[i],paste("Total SD for",target_label(total_targets[i]),"under",total_owners[i]),"total_sd","population",units[i],"positive",paste0("total_sd[",i,"]"))
  }
  share_labels<-if(length(share_keys))paste("Share coefficient:",col("share_coefficients","owner"),
    "stratum",col("share_coefficients","stratum_index",c("integer","double")),
    "term",col("share_coefficients","term"),"contrast",col("share_coefficients","coordinate"))else character()
  for(i in seq_along(share_keys)) add(share_keys[i],share_labels[i],"share","population",
    "dimensionless","real",paste0("share_beta[",i,"]"))
  cell_variable<-function(at)paste0("R_",cell_layer[at],"[",cell_row[at],",",cell_col[at],"]")
  for(i in seq_along(rho_keys)) {
    at<-alias_groups[[i]];first<-at[1L]
    display<-.bh_public_id(if(rho_layer[i]=="component")"component_correlation"else"parameter_residual_correlation",
      endpoint1[first],endpoint2[first])
    endpoint_labels<-if(rho_layer[i]=="parameter")target_label(c(endpoint1[first],endpoint2[first]))else c(endpoint1[first],endpoint2[first])
    add(rho_keys[i],paste(if(length(at)>1L)paste0("Shared correlation (",length(at)," pairs):")else"Correlation:",endpoint_labels[1],"with",endpoint_labels[2]),"unique_marginal_correlation",rho_layer[i],
      "dimensionless","correlation",cell_variable(first),aliases=cell_variable(at),display=display)
  }
  raw_labels<-character()
  if(length(raw_keys)) {
    coordinate_ids<-keys("coordinates","id");coordinate_labels<-col("coordinates","display")
    destination<-match(col("raw_correlation_coordinates","destination"),coordinate_ids)
    previous<-match(col("raw_correlation_coordinates","previous_whitened_coordinate"),coordinate_ids)
    if(anyNA(destination)||anyNA(previous)).bh_diag_error("raw coordinate labels reference unknown coordinates")
    raw_labels<-paste("Raw coordinate in",col("raw_correlation_coordinates","block"),":",
      coordinate_labels[destination],"relative to earlier white coordinate",coordinate_labels[previous])
  }
  for(i in seq_along(raw_keys)) add(raw_keys[i],raw_labels[i],"cor_raw","component",
    "dimensionless","real",paste0("correlation_B[",i,"]"))
  for(at in derived) {
    key<-.bh_public_id("component_correlation",endpoint1[at],endpoint2[at])
    add(key,paste("Derived correlation",endpoint1[at],"with",endpoint2[at]),"derived_common_correlation","component",
      "dimensionless","correlation",cell_variable(at),kind="derived_common_correlation",prior=FALSE)
  }
  out<-.bh_diag_catalogue_frame(rows)
  if(.row_names_info(out,2L)!=M || base::anyDuplicated.default(.subset2(unclass(out),"key")) ||
     base::anyDuplicated.default(.subset2(unclass(out),"source_variable")))
    .bh_diag_error("resolved catalogue has ambiguous keys or source representatives")
  attr(out,"diagnostic_catalogue")<-list(version="bmeas_diagnostic_catalogue_v1",status="metadata_only",
    selected_domain="population primitives and explicit raw-law derived common correlations",
    group_counts=group_counts,catalogue_rows=M,budget=cap,
    context=list(table_rows=counts,missing_optional_tables=absent,expanded_alias_references=alias_count,
      grouped_notices=notice_count,admitted_rows=context_total,
      scope="Consumed saved metadata rows and expanded aliases/notices; not byte or RSS accounting"),
    exclusions=list(fixed=list(status="excluded",count_unit="saved component scale rows",known_rows=sum(scale_state=="fixed")),
      share_weights=list(status="excluded",count_unit="saved share-part rows",known_rows=if(length(absent))NA_real_ else counts[["share_parts"]]),
      person_and_basis=list(status="excluded_not_expanded",known_rows=NA_real_),
      other_derived=list(status="excluded_not_expanded",known_rows=NA_real_)),
    source_scope="Saved metadata only; no CSV, original-output authentication, backend or provider access",
    numerical_scope="No diagnostics computed; eligibility does not establish identification, convergence or inference")
  out
}

diagnostic_parameters <- function(fit,budget=list(max_rows=10000,max_context_rows=100000)) {
  if(missing(fit)) .bh_diag_error("diagnostic discovery requires a retained main-API fit","E_FIT")
  .bh_pf_public_input(fit,"diagnostic_parameters")
  if(.bh_pf_is_portable(fit)) {
    out<-.bh_diagnostic_catalogue(.bh_pf_metadata(fit,"diagnostic_parameters"),budget)
    info<-attr(out,"diagnostic_catalogue",exact=TRUE)
    info$portable_context<-.bh_pf_metadata_notice(fit)
    attr(out,"diagnostic_catalogue")<-info
    return(out)
  }
  .bh_diagnostic_catalogue(fit,budget)
}
.bh_diagnostic_select <- function(catalogue,parameters) {
  if(!.bh_diag_plain(parameters,"character") || anyNA(parameters) || any(!validEnc(parameters)) ||
    any(!nzchar(parameters)) || base::anyDuplicated.default(parameters))
    .bh_diag_error("parameters must be distinct exact plain catalogue keys","E_DIAGNOSTIC_SELECTION")
  .bh_diag_table_rows(catalogue,"diagnostic catalogue")
  keys<-.bh_diag_column(catalogue,"key")
  if(base::anyDuplicated.default(keys)) .bh_diag_error("catalogue keys are ambiguous","E_DIAGNOSTIC_SELECTION")
  at<-match(parameters,keys)
  if(anyNA(at)) .bh_diag_error("a requested key is outside the supported population catalogue","E_DIAGNOSTIC_SELECTION")
  columns<-unclass(catalogue)
  rows<-lapply(at,function(i) {
    row<-lapply(names(columns),function(name) {
      value<-.subset2(columns,name)
      if(name=="source_aliases")return(.subset2(unclass(value),i))
      if(!.bh_diag_plain(value,c("character","logical")))
        .bh_diag_error("catalogue columns are not the package-owned plain schema","E_DIAGNOSTIC_SELECTION")
      .subset2(value,i)
    })
    names(row)<-names(columns);row
  })
  out<-.bh_diag_catalogue_frame(rows)
  # Exact distinct positive indices retain the old data-frame row-name rule,
  # without entering [.data.frame or a caller row-name method.
  attr(out,"row.names")<-attr(catalogue,"row.names",exact=TRUE)[at]
  attr(out,"diagnostic_catalogue")<-attr(catalogue,"diagnostic_catalogue",exact=TRUE)
  out
}
