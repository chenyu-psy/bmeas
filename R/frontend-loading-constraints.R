# Exact ordinary-cell constraints; fixed and aliased cells share one affine IR.
.bh_capture_loading_constraints <- function(x,components) {
  if(is.null(x))return(NULL)
  if(!is.list(x)||is.object(x)||!length(x)||is.null(names(x))||anyDuplicated(names(x))||any(!names(x)%in%c("fixed","equal")))
    .mp_abort("E_LOADING_CONSTRAINT","loadings needs fixed/equal rule lists")
  capture<-function(cells) {
    if(!is.list(cells)||is.object(cells)||!length(cells)).mp_abort("E_LOADING_CONSTRAINT","each rule needs a nonempty component-to-selector cells map")
    .mp_names(cells,"loading constraint components")
    if(any(!names(cells)%in%components)).mp_abort("E_LOADING_CONSTRAINT","a loading rule names an undeclared component")
    lapply(cells,.bh_formula_targets)
  }
  fixed<-x$fixed;equal<-x$equal
  if(!is.null(fixed)&&(!is.list(fixed)||is.object(fixed)||!length(fixed))).mp_abort("E_LOADING_CONSTRAINT","fixed must be a nonempty list of value/cells rules")
  fixed<-lapply(seq_along(fixed),function(i) {
    rule<-fixed[[i]]
    if(!is.list(rule)||is.object(rule)||!setequal(names(rule),c("value","cells"))||anyDuplicated(names(rule))) .mp_abort("E_LOADING_CONSTRAINT","each fixed rule requires exactly value and cells")
    value<-rule$value
    if(!typeof(value)%in%c("integer","double")||is.object(value)||length(attributes(value))||length(value)!=1L||!is.finite(value)) .mp_abort("E_LOADING_CONSTRAINT","a fixed loading is one plain finite real value")
    list(value=if(value==0)0 else as.numeric(value),cells=capture(rule$cells))
  })
  if(!is.null(equal)) {if(!is.list(equal)||is.object(equal)).mp_abort("E_LOADING_CONSTRAINT","equal requires a plain named list");.mp_names(equal,"loading equality classes");if(!length(equal)).mp_abort("E_LOADING_CONSTRAINT","equal must contain named classes")}
  equal<-lapply(equal,capture)
  if(!length(fixed)&&!length(equal)).mp_abort("E_LOADING_CONSTRAINT","loadings must contain at least one fixed or equality rule")
  list(fixed=fixed,equal=equal,version="ordinary_loading_constraints_v1")
}
.bh_apply_loading_constraints <- function(map,formula,selected,primary) {
  ordinary<-map$free_loadings;n<-nrow(ordinary);coordinates<-formula$coordinates;K<-nrow(coordinates)
  ordinary$cell_id<-vapply(seq_len(n),function(i).bh_public_id("loading_cell",coordinates$id[ordinary$component_index[i]],ordinary$target[i]),character(1))
  parent<-seq_len(n)
  root<-function(i){while(parent[i]!=i)i<-parent[i];i}
  join<-function(ids){if(length(ids)>1L){r<-vapply(ids,root,integer(1));parent[r]<<-min(r)}}
  resolve<-function(cells) {
    ids<-integer()
    for(group in names(cells)) {
      keys<-vapply(cells[[group]],function(t)if(t$kind=="wide")targets("scores","mu",t$column)else targets(t$block,t$parameter,t$task),character(1))
      if(anyDuplicated(keys)).mp_abort("E_LOADING_CONSTRAINT","a constraint selects the same cell twice")
      if(any(!keys%in%selected[[group]])) .mp_abort("E_LOADING_COVERAGE",paste("constraint does not match every declared cell in",group,":",paste(setdiff(keys,selected[[group]]),collapse=", ")))
      for(key in keys) {
        at<-which(ordinary$component==group&ordinary$target==key)
        if(length(at)!=1L) {
          owner<-formula$decomposition$owned$owner[match(key,formula$decomposition$owned$target)]
          if(!length(owner)||is.na(owner))owner<-"none"
          expression<-if(inherits(formula$components[[group]],"bhmm_basis_component"))"fixed numeric basis expression (including structural zero)"else"share-owned loading expression"
          .mp_abort("E_LOADING_DERIVED",paste("cannot constrain derived cell",group,key,"expression:",expression,"owner:",owner,"; this increment only constrains ordinary unowned scalar cells"))
        }
        ids<-c(ids,at)
      }
    }
    if(anyDuplicated(ids)).mp_abort("E_LOADING_CONSTRAINT","a constraint contains duplicate resolved cells")
    ids
  }
  declarations<-formula$loadings;rules<-list();fixed_assignments<-list()
  for(label in names(declarations$equal)) {
    ids<-resolve(declarations$equal[[label]])
    if(length(ids)<2L).mp_abort("E_LOADING_CONSTRAINT","an equality class needs at least two distinct cells")
    join(ids);rules[[length(rules)+1L]]<-data.frame(rule=paste0("equal:",label),kind="equal",label=label,value=NA_real_,cell_id=ordinary$cell_id[ids],stringsAsFactors=FALSE)
    rules[[length(rules)]]$captured_selector<-I(rep(list(declarations$equal[[label]]),length(ids)))
  }
  for(i in seq_along(declarations$fixed)) {
    rule<-declarations$fixed[[i]];ids<-resolve(rule$cells)
    fixed_assignments[[i]]<-list(ids=ids,value=rule$value)
    rules[[length(rules)+1L]]<-data.frame(rule=paste0("fixed:",i),kind="fixed",label=NA_character_,value=rule$value,cell_id=ordinary$cell_id[ids],stringsAsFactors=FALSE)
    rules[[length(rules)]]$captured_selector<-I(rep(list(rule$cells),length(ids)))
  }
  for(group in names(.bh_marker_declarations(formula))) {
    rule<-formula$scale[[group]];k<-which(coordinates$group==group)
    if(length(k)!=1L||coordinates$kind[k]!="scalar"||any(map$loading_map$coordinate_index%in%k & map$loading_map$role!="loading"))
      .mp_abort("E_MARKER_DERIVED","marker scaling requires an entirely ordinary scalar column; basis or share-derived cells cannot be rescaled")
    ids<-resolve(stats::setNames(list(rule$marker),group))
    if(length(ids)!=1L).mp_abort("E_MARKER_SCALE","marker must resolve exactly one declared ordinary cell")
    fixed_assignments[[length(fixed_assignments)+1L]]<-list(ids=ids,value=rule$value)
    rules[[length(rules)+1L]]<-data.frame(rule=paste0("marker:",group),kind="marker",label=group,value=rule$value,cell_id=ordinary$cell_id[ids],stringsAsFactors=FALSE)
    rules[[length(rules)]]$captured_selector<-I(list(rule$marker))
  }
  roots<-if(n)vapply(seq_len(n),root,integer(1))else integer()
  groups<-split(seq_len(n),factor(roots,levels=unique(roots)))
  values<-rep(NA_real_,n);primitive<-rep(NA_character_,n);free_rows<-list();members<-list()
  for(ids in groups) {
    fixed<-unique(vapply(Filter(function(a)any(a$ids%in%ids),fixed_assignments),`[[`,numeric(1),"value"))
    if(length(fixed)>1L).mp_abort("E_LOADING_CONFLICT",paste("conflicting fixed values propagate into one loading class:",paste(ordinary$cell_id[ids],collapse=", ")))
    if(length(fixed)) {values[ids]<-fixed;next}
    units<-primary$predictor_units[ordinary$target_index[ids]]
    if(anyNA(units)||any(!nzchar(units))||length(unique(units))!=1L) .mp_abort("E_LOADING_UNITS",paste("one free loading class requires compatible declared predictor-unit labels; repair declarations for",paste(ordinary$cell_id[ids],collapse=", ")))
    id<-if(length(ids)==1L)ordinary$parameter[ids]else .bh_public_id("loading_class",paste(sort(ordinary$cell_id[ids],method="radix"),collapse="|"))
    primitive[ids]<-id
    representative<-ordinary[ids[1L],,drop=FALSE];representative$parameter<-id;representative$sign<-"free";representative$units<-units[1L]
    representative$members<-paste(sort(ordinary$cell_id[ids],method="radix"),collapse="; ")
    free_rows[[length(free_rows)+1L]]<-representative;members[[id]]<-ids
  }
  free<-if(length(free_rows))do.call(rbind,free_rows)else {
    z<-ordinary[FALSE,,drop=FALSE];z$units<-character();z$members<-character();z
  }
  rownames(free)<-NULL
  # Equality couples sign flips. Fixed nonzero/derived signs pin components.
  sign_parent<-seq_len(K)
  sign_root<-function(k){while(sign_parent[k]!=k)k<-sign_parent[k];k}
  for(ids in members) {
    r<-unique(vapply(ordinary$component_index[ids],sign_root,integer(1)))
    if(length(r)>1L)sign_parent[r]<-min(r)
  }
  sign_groups<-if(K)vapply(seq_len(K),sign_root,integer(1))else integer()
  pinned_cells<-which(!is.na(values)&values!=0)
  derived<-map$loading_map[map$loading_map$role!="loading"&map$loading_map$multiplier!=0,,drop=FALSE]
  pinned_coordinates<-unique(c(ordinary$component_index[pinned_cells],derived$coordinate_index))
  pinned_groups<-unique(sign_groups[pinned_coordinates]);anchor_for<-rep(NA_character_,K)
  for(g in unique(sign_groups)) {
    at<-which(vapply(members,function(ids)any(sign_groups[ordinary$component_index[ids]]==g),logical(1)))
    if(!g%in%pinned_groups&&length(at)) {free$sign[at[1L]]<-"positive_anchor";anchor_for[sign_groups==g]<-free$parameter[at[1L]]}
  }
  free$support<-ifelse(free$sign=="positive_anchor","positive","real")
  free$orientation_reason<-vapply(seq_len(nrow(free)),function(i)if(free$sign[i]=="positive_anchor")"one independent simultaneous sign gauge"else if(sign_groups[free$component_index[i]]%in%pinned_groups)"fixed or derived sign pins this connected group"else"signed primitive shares the group's one gauge",character(1))
  orientation<-data.frame(coordinate=coordinates$display,coordinate_id=coordinates$id,sign_group=sign_groups,
    pinned=sign_groups%in%pinned_groups,anchor=anchor_for,allowed_simultaneous_flip=!sign_groups%in%pinned_groups,
    latent_only=vapply(seq_len(K),function(k)!any(ordinary$component_index==k&is.na(values))&&!k%in%pinned_coordinates,logical(1)),stringsAsFactors=FALSE)
  orientation$pin_witness<-vapply(seq_len(K),function(k) {
    fixed<-ordinary$cell_id[pinned_cells][sign_groups[ordinary$component_index[pinned_cells]]==sign_groups[k]]
    at<-which(sign_groups[derived$coordinate_index]==sign_groups[k])
    paste(c(fixed,if(length(at))paste0("derived:",derived$coordinate_id[at],":",derived$target[at])),collapse="; ")
  },character(1))
  ordinary$constraint_class<-rep(NA_character_,n)
  for(ids in groups)ordinary$constraint_class[ids]<-.bh_public_id("loading_constraint_class",paste(sort(ordinary$cell_id[ids],method="radix"),collapse="|"))
  ordinary$state<-ifelse(is.na(values),"free","fixed");ordinary$fixed_value<-values;ordinary$parameter<-primitive
  ordinary$primitive_index<-match(primitive,free$parameter);ordinary$effective_support<-is.na(values)|values!=0
  ordinary$sign<-ifelse(ordinary$state=="fixed","fixed",free$sign[ordinary$primitive_index])
  rule_table<-if(length(rules))do.call(rbind,rules)else data.frame(rule=character(),kind=character(),label=character(),value=numeric(),cell_id=character())
  if(!length(rules))rule_table$captured_selector<-I(list())
  rule_table$primitive<-primitive[match(rule_table$cell_id,ordinary$cell_id)]
  rule_table$state<-ordinary$state[match(rule_table$cell_id,ordinary$cell_id)]
  rule_table$resolved_fixed_value<-values[match(rule_table$cell_id,ordinary$cell_id)]
  ordinary$equality_labels<-vapply(ordinary$cell_id,function(id)paste(sort(unique(rule_table$label[rule_table$cell_id==id&rule_table$kind=="equal"])),collapse="; "),character(1))
  ordinary$constraint_provenance<-vapply(ordinary$cell_id,function(id)paste(rule_table$rule[rule_table$cell_id==id],collapse="; "),character(1))
  for(ids in groups) {
    selected_rules<-rule_table[rule_table$cell_id%in%ordinary$cell_id[ids],,drop=FALSE]
    ordinary$equality_labels[ids]<-paste(sort(unique(selected_rules$label[selected_rules$kind=="equal"])),collapse="; ")
    ordinary$constraint_provenance[ids]<-paste(sort(unique(selected_rules$rule)),collapse="; ")
  }
  ordinary$units<-primary$predictor_units[ordinary$target_index]
  ordinary$group<-ordinary$component
  ordinary$expression<-ifelse(ordinary$state=="fixed","fixed_constant","ordinary_primitive")
  ordinary$owner<-rep(NA_character_,nrow(ordinary))
  map$free_loadings<-free
  old<-map$loading_map;at<-which(old$role=="loading")
  cell_match<-match(vapply(at,function(i).bh_public_id("loading_cell",old$coordinate_id[i],old$target[i]),character(1)),ordinary$cell_id)
  old$coefficient[at]<-primitive[cell_match]
  old$role[at[!is.na(values[cell_match])]]<-"fixed_loading"
  old$fixed_value<-rep(NA_real_,nrow(old));old$fixed_value[at]<-values[cell_match]
  map$loading_map<-old
  # Effective loadings keep fixed nonzero cells; the declaration table retains zeros.
  live<-ordinary[ordinary$effective_support,c("target","target_index","component","component_index","parameter","sign"),drop=FALSE]
  map$loadings<-rbind(live,map$loadings[map$loadings$sign%in%c("basis_derived","share_derived"),,drop=FALSE])
  coeff<-rbind(data.frame(parameter=free$parameter,role=rep("loading",nrow(free)),component=free$component,target=free$target,support=ifelse(free$sign=="positive_anchor","positive","real"),stringsAsFactors=FALSE),
    map$loading_coefficients[map$loading_coefficients$role!="loading",c("parameter","role","component","target","support"),drop=FALSE])
  coeff$coefficient_index<-seq_len(nrow(coeff));map$loading_coefficients<-coeff
  map$loading_map$coefficient_index<-match(map$loading_map$coefficient,coeff$parameter)
  declared_cells<-ordinary
  for(group in names(formula$components)) {
    declaration<-formula$components[[group]];keys<-selected[[group]];cs<-which(coordinates$group==group)
    basis<-inherits(declaration,"bhmm_basis_component")
    for(j in seq_along(keys))for(k in seq_along(cs)) {
      if(!basis&&any(ordinary$component==group&ordinary$target==keys[j]))next
      row<-ordinary[NA_integer_,,drop=FALSE]
      row$target<-keys[j];row$target_index<-match(keys[j],primary$key);row$component<-coordinates$display[cs[k]];row$component_index<-cs[k]
      row$cell_id<-.bh_public_id("loading_cell",coordinates$id[cs[k]],keys[j]);row$state<-"derived"
      row$group<-group;row$units<-primary$predictor_units[row$target_index]
      row$constraint_provenance<-row$equality_labels<-""
      owned<-match(keys[j],formula$decomposition$owned$target)
      is_owned<-length(owned)==1L&&!is.na(owned)
      row$owner<-if(is_owned)formula$decomposition$owned$owner[owned]else NA_character_
      row$expression<-if(is_owned)"total_sd*sqrt(share)*supplied_multiplier"else"positive_amplitude*supplied_basis"
      row$sign<-if(is_owned)"share_derived"else"basis_derived"
      row$parameter<-if(is_owned)formula$decomposition$owned$parameter[owned]else map$amplitudes$parameter[match(group,map$amplitudes$component)]
      row$effective_support<-if(basis)declaration$basis[j,k]!=0 else TRUE
      declared_cells<-rbind(declared_cells,row)
    }
  }
  rownames(declared_cells)<-NULL
  map$loading_cells<-declared_cells;map$loading_primitives<-free;map$loading_constraint_rules<-rule_table;map$orientation<-orientation
  map$ordinary_loading_map<-ordinary
  map$ordinary_fixed_matrix<-matrix(0,nrow(primary),K)
  if(any(!is.na(values)))map$ordinary_fixed_matrix[cbind(ordinary$target_index[!is.na(values)],ordinary$component_index[!is.na(values)])]<-values[!is.na(values)]
  map
}

# A conservative symbolic dependency bound, never a pointwise derivative test.
# Signed/aliased cancellations may keep an extra feature; they cannot remove a
# feature that varies with an actual primitive or a free correlation entry.
.bh_covariance_entry_has_unknown <- function(spec,left,right) {
  K<-nrow(spec$tables$components);map<-spec$tables$loading_map
  support<-unknown<-matrix(FALSE,2L,K)
  keys<-c(left,right)
  for(row in 1:2) {
    cells<-map[map$target==keys[row]&map$multiplier!=0,,drop=FALSE]
    for(i in seq_len(nrow(cells))) {
      fixed<-cells$role[i]=="fixed_loading"
      active<-!fixed||(!is.na(cells$fixed_value[i])&&cells$fixed_value[i]!=0)
      if(active){support[row,cells$coordinate_index[i]]<-TRUE;unknown[row,cells$coordinate_index[i]]<-!fixed||cells$coordinate_index[i]%in%.bh_free_component_sd(spec)$component_index}
    }
  }
  if(K&&any(support[1,]&support[2,]&(unknown[1,]|unknown[2,])))return(TRUE)
  cor<-spec$tables$correlations
  for(e in which(cor$state=="free")) {
    a<-cor$row[e];b<-cor$col[e]
    if((support[1,a]&&support[2,b])||(support[1,b]&&support[2,a]))return(TRUE)
  }
  for(e in which(cor$state=="fixed")) {
    a<-cor$row[e];b<-cor$col[e]
    if(cor$fixed_value[e]!=0&&((support[1,a]&&support[2,b]&&(unknown[1,a]||unknown[2,b]))||
       (support[1,b]&&support[2,a]&&(unknown[1,b]||unknown[2,a]))))return(TRUE)
  }
  residual<-spec$tables$parameter_residuals
  a<-match(left,residual$target);b<-match(right,residual$target)
  if(!is.na(a)&&!is.na(b)) {
    if(a==b)return(TRUE)
    cor<-spec$tables$parameter_correlations
    if(any(cor$row==min(a,b)&cor$col==max(a,b)&cor$state=="free"))return(TRUE)
    if(any(cor$row==min(a,b)&cor$col==max(a,b)&cor$state=="fixed"))return(TRUE)
  }
  FALSE
}

# A narrow population-parameter alias, beyond the already resolved sign gauge.
# Two crossed classes can exchange factor columns and still satisfy every
# equality. This does not reject permutations that leave all primitives fixed.
.bh_loading_permutation_identification <- function(spec) {
  if(nrow(spec$tables$components)!=2L||!isTRUE(all(spec$tables$components$variance==1))||
     .bh_has_shares(spec)||nrow(.bh_amplitudes(spec))||!all(spec$tables$coordinates$kind=="scalar"))return(NULL)
  cells<-spec$tables$ordinary_loading_map;free<-.bh_free_loadings(spec)
  if(is.null(cells)||nrow(cells)!=4L||any(cells$state!="free")||nrow(free)!=2L||
     sum(free$sign=="positive_anchor")!=1L||sum(free$sign=="free")!=1L||length(unique(cells$target))!=2L)return(NULL)
  keys<-unique(cells$target);ids<-matrix(NA_character_,2L,2L)
  ids[cbind(match(cells$target,keys),cells$component_index)]<-cells$parameter
  if(anyNA(ids)||ids[1,1]!=ids[2,2]||ids[1,2]!=ids[2,1]||ids[1,1]==ids[1,2])return(NULL)
  list(reason="two crossed loading classes admit a factor-column permutation plus a common sign flip that preserves the positive anchor and predictor covariance while changing both population coefficients",
    parameters=unique(cells$parameter),components=spec$tables$components$component,targets=keys,
    proof="For Lambda=[[a,b],[b,a]], exchanging columns swaps a,b. The two-coordinate unit-diagonal R is unchanged by that exchange; a simultaneous sign flip can restore the one positive anchor. The resulting generic coefficient pair is distinct but Lambda R Lambda' and the independent residual layer are unchanged",
    scope="exactly two unit-SD ordinary scalar coordinates, four free cells in two crossed classes, one simultaneous sign gauge, no fixed/basis/share loading cells; this is a known global alias, not a numerical-rank conclusion")
}
