# A marker fixes declared beta and allocates a separate population component SD.
.bh_capture_marker_scales <- function(scale,components) {
  if(is.null(scale)||(is.list(scale)&&!is.object(scale)&&!length(scale)))return("unit_variance")
  if(!is.list(scale)||is.object(scale)) .mp_abort("E_MARKER_SCALE","scale requires a named component-to-marker list")
  .mp_names(scale,"marked components")
  if(any(!names(scale)%in%components)) .mp_abort("E_MARKER_SCALE","scale names an undeclared component")
  lapply(scale,function(rule) {
    if(!is.list(rule)||is.object(rule)||is.null(names(rule))||anyDuplicated(names(rule))||!"marker"%in%names(rule)||any(!names(rule)%in%c("marker","value")))
      .mp_abort("E_MARKER_SCALE","each component scale needs marker and optional value")
    value<-if("value"%in%names(rule))rule$value else 1
    if(!typeof(value)%in%c("integer","double")||is.object(value)||length(attributes(value))||length(value)!=1L||!is.finite(value)||value==0)
      .mp_abort("E_MARKER_SCALE","marker value must be one plain finite nonzero real; omitted value means 1")
    target<-.bh_formula_targets(rule$marker)
    if(length(target)!=1L) .mp_abort("E_MARKER_SCALE","one component requires exactly one declared marker cell")
    list(marker=target,value=as.numeric(value))
  })
}
.bh_marker_declarations <- function(formula)if(is.list(formula$scale))formula$scale else list()
.bh_has_marker <- function(spec)length(.bh_marker_declarations(spec$formula))>0L
.bh_free_component_sd <- function(spec) {
  table<-spec$tables$component_scales
  if(is.null(table))return(data.frame(parameter=character(),component=character(),component_index=integer()))
  table[table$state=="free",,drop=FALSE]
}
.bh_attach_component_scales <- function(spec) {
  coords<-spec$tables$coordinates;K<-nrow(coords);cells<-spec$tables$ordinary_loading_map
  table<-data.frame(component=coords$display,component_index=seq_len(K),coordinate_id=coords$id,
    state=rep("fixed",K),fixed_sd=rep(1,K),parameter=rep(NA_character_,K),primitive_index=rep(0L,K),
    marker_target=rep(NA_character_,K),marker_cell=rep(NA_character_,K),marker_value=rep(NA_real_,K),
    marker_units=rep(NA_character_,K),units=rep("numeric_latent_coordinate",K),support=rep("fixed_one",K),
    affected_class_members=rep("",K),expression=rep("effective_loading=declared_beta",K),stringsAsFactors=FALSE)
  table$captured_selector<-I(rep(list(NULL),K))
  for(group in names(.bh_marker_declarations(spec$formula))) {
    rule<-spec$formula$scale[[group]];k<-which(coords$group==group)
    t<-rule$marker[[1L]];target<-if(t$kind=="wide")targets("scores","mu",t$column)else targets(t$block,t$parameter,t$task)
    at<-which(cells$component_index==k&cells$target==target)
    table$state[k]<-"free";table$fixed_sd[k]<-NA_real_
    table$parameter[k]<-.bh_public_id("component_sd",coords$id[k])
    table$marker_target[k]<-target;table$marker_cell[k]<-cells$cell_id[at];table$marker_value[k]<-rule$value
    table$marker_units[k]<-cells$units[at];table$support[k]<-"positive"
    table$affected_class_members[k]<-paste(sort(cells$cell_id[cells$constraint_class==cells$constraint_class[at]],method="radix"),collapse="; ")
    table$expression[k]<-"effective_loading=declared_beta*component_sd"
    table$captured_selector[k]<-list(rule$marker)
  }
  table$primitive_index[table$state=="free"]<-seq_len(sum(table$state=="free"))
  spec$tables$component_scales<-table
  spec$tables$components$standardized_variance<-rep(1,K)
  spec$tables$components$variance<-as.numeric(ifelse(table$state=="fixed",1,NA_real_))
  spec$tables$components$sd_parameter<-table$parameter
  spec$tables$components$variance_expression<-as.character(ifelse(table$state=="fixed","1",paste0(table$parameter,"^2")))
  spec$tables$components$score_definition<-as.character(ifelse(table$state=="fixed","f=z","f=component_sd*z"))
  for(name in c("loading_cells","ordinary_loading_map","loading_map")) {
    tab<-spec$tables[[name]];index<-if(name=="loading_map")tab$coordinate_index else tab$component_index
    tab$component_sd_parameter<-table$parameter[index]
    tab$effective_expression<-as.character(ifelse(table$state[index]=="fixed","declared_beta","declared_beta*component_sd"))
    spec$tables[[name]]<-tab
  }
  spec
}
# Stable ratios are proposals, not modifications of the scientific coefficients.
.bh_marker_calibration <- function(spec,calibration) {
  table<-.bh_free_component_sd(spec);rows<-list()
  for(i in seq_len(nrow(table))) {
    cal<-calibration[[match(table$marker_target[i],spec$tables$targets$key)]]
    a<-if(is.null(cal))NA_real_ else cal$scale;c<-abs(table$marker_value[i])
    valid<-is.finite(a)&&a>0
    log_sd<-if(valid)log(a)-log(c)else NA_real_
    proposal<-if(valid)exp(log_sd)else NA_real_
    representable<-is.finite(proposal)&&proposal>0
    rows[[i]]<-data.frame(parameter=table$parameter[i],component=table$component[i],component_index=table$component_index[i],
      marker_target=table$marker_target[i],marker_value=table$marker_value[i],marker_calibration=a,
      log_sd_proposal=log_sd,proposed_scale=proposal,
      status=if(!valid)"missing_marker_calibration"else if(!representable)"unrepresentable_default_ratio"else"available",
      source=if(is.null(cal))"missing"else cal$source,
      observations=if(is.null(cal))""else paste(cal$observations,collapse="; "),
      policy="half_normal(marker_target_loading_scale/abs(marker_value)); independent parameterization-specific prior",stringsAsFactors=FALSE)
  }
  if(length(rows))do.call(rbind,rows)else data.frame()
}
.bh_marker_loading_proposal <- function(cal,k,scale_calibration) {
  at<-match(k,scale_calibration$component_index)
  if(is.na(at))return(cal)
  a<-if(is.null(cal))NA_real_ else cal$scale
  log_sd<-scale_calibration$log_sd_proposal[at]
  log_beta<-if(is.finite(a)&&a>0&&is.finite(log_sd))log(a)-log_sd else NA_real_
  value<-if(is.finite(log_beta))exp(log_beta)else NA_real_
  source<-if(is.null(cal))"missing"else cal$source
  out<-if(is.null(cal))list(observations=character())else cal
  out$target_calibration_scale<-a;out$original_source<-source
  out$marker_component<-scale_calibration$component[at]
  out$marker_value<-scale_calibration$marker_value[at]
  out$marker_calibration_scale<-scale_calibration$marker_calibration[at]
  out$marker_sd_proposal<-scale_calibration$proposed_scale[at]
  out$marker_log_sd_proposal<-log_sd;out$log_beta_proposal<-log_beta
  out$proposal_status<-if(!is.finite(a)||a<=0)"missing_target_calibration"else if(!is.finite(log_sd))"missing_marker_calibration"else if(!is.finite(value)||value<=0)"unrepresentable_beta_ratio"else"available"
  out$proposal_reason<-switch(out$proposal_status,
    missing_target_calibration="the target has no valid finite positive loading calibration",
    missing_marker_calibration="the marker has no valid finite positive calibration; a user SD prior does not replace this denominator",
    unrepresentable_beta_ratio="target calibration divided by the marker-derived default SD is outside finite positive representation; no clipping or unit fallback",
    "target calibration divided by marker-derived default SD, evaluated through its log ratio")
  out$scale<-value;out$source<-paste0("marker_scaled_",source);out
}
.bh_marker_identification <- function(spec) {
  sd<-.bh_free_component_sd(spec)
  if(!nrow(sd)||.bh_is_affine_cor_table(spec$tables$correlations))return(NULL)
  cells<-spec$tables$ordinary_loading_map;coordinates<-spec$tables$coordinates;cor<-spec$tables$correlations
  # The crossed two-column equality survives exchanging scaled columns.
  if(nrow(sd)==2L&&nrow(coordinates)==2L&&nrow(cells)==4L&&!.bh_has_shares(spec)&&!nrow(.bh_amplitudes(spec))) {
    fixed<-cells[cells$state=="fixed",,drop=FALSE];free<-cells[cells$state=="free",,drop=FALSE]
    if(nrow(fixed)==2L&&nrow(free)==2L&&length(unique(cells$target))==2L&&
       length(unique(fixed$target))==2L&&length(unique(fixed$component_index))==2L&&all(fixed$fixed_value!=0)&&
       length(unique(free$parameter))==1L&&length(unique(free$target))==2L&&length(unique(free$component_index))==2L&&
       all(fixed$cell_id%in%sd$marker_cell))
      return(list(reason="two marked columns with a crossed free loading class retain an exact scaled-column permutation alias",
        proof="beta=[[c1,b],[b,c2]]: b_new=c1*c2/b; s1_new=abs(b/c1)*s2; s2_new=abs(b/c2)*s1; rho_new=sign(c1*c2)*rho. Effective covariance is unchanged and both marker constants remain exact",
        scope="exactly two ordinary marked columns, two fixed nonzero diagonal markers and one free off-diagonal class, independent or free marginal correlation; no other loading cells",
        parameters=c(unique(free$parameter),sd$parameter)))
  }
  # A marked column can shear by changing its SD and keeping beta_marker fixed.
  map<-spec$tables$loading_map
  for(i in seq_len(nrow(sd))) {
    g<-sd$component_index[i];gcells<-cells[cells$component_index==g,,drop=FALSE]
    if(sum(gcells$state=="fixed")!=1L||!sd$marker_cell[i]%in%gcells$cell_id)next
    free<-gcells[gcells$state=="free",,drop=FALSE]
    if(anyDuplicated(free$parameter)||any(vapply(free$parameter,function(id)sum(cells$parameter==id,na.rm=TRUE)!=1L,logical(1))))next
    for(a in seq_len(nrow(.bh_amplitudes(spec)))) {
      p<-which(coordinates$group==.bh_amplitudes(spec)$component[a]);if(length(p)!=1L)next
      support<-map$target[map$coordinate_index==p&map$multiplier!=0]
      if(!all(support%in%gcells$target))next
      edge<-which(cor$row==min(g,p)&cor$col==max(g,p)&cor$state=="free")
      incident<-which((cor$row%in%c(p,g)|cor$col%in%c(p,g))&cor$state=="free")
      if(length(edge)!=1L||!identical(edge,incident))next
      return(list(reason="a free marked component SD restores a continuous shear with a freely correlated basis amplitude while preserving the declared marker",
        proof="a=s*beta; a_new=a+delta*q; s_new=s+delta*q_marker/c; beta_new=a_new/s_new; t_new=r*h-delta; h_new=sqrt(h^2*(1-r^2)+t_new^2); r_new=t_new/h_new",
        scope="one marked ordinary column spanning nonzero one-column basis support, only its marker fixed, other cells singleton free, no other incident free correlations",
        parameters=c(sd$parameter[i],free$parameter,.bh_amplitudes(spec)$parameter[a])))
    }
  }
  NULL
}
