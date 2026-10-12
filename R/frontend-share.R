# Variance-share declarations: captured target-level designs and ownership.
.bh_target_keys <- function(parts) vapply(parts,function(t)
  if(t$kind=="wide")targets("scores","mu",t$column)else targets(t$block,t$parameter,t$task),character(1))
.bh_share_groups <- function(x) {
  x<-.bh_unwrap(x)
  if(is.numeric(x)&&length(x)==1L&&identical(as.numeric(x),0))return(character())
  if(is.symbol(x))return(as.character(x))
  if(is.call(x)&&identical(x[[1L]],as.name("+"))&&length(x)==3L)return(c(.bh_share_groups(x[[2L]]),.bh_share_groups(x[[3L]])))
  .mp_abort("E_SHARE_COMPONENTS","select whole component groups with +, or ~0 for a residual-only owner; coordinate subsets and arbitrary calls are not parts")
}
.bh_share_matrix <- function(x,label,zero_columns=FALSE) {
  if(!typeof(x)%in%c("integer","double")||is.object(x)||!is.matrix(x)||nrow(x)<1L||
     ncol(x)<(if(zero_columns)0L else 1L)||any(!is.finite(x))) .mp_abort("E_SHARE_MATRIX",paste(label,"must be a plain finite real matrix with the declared dimensions"))
  extra<-setdiff(names(attributes(x)),c("dim","dimnames"))
  if(any(!extra%in%c("assign","contrasts"))||any(!vapply(attributes(x)[extra],.bh_inert_annotation,logical(1)))) .mp_abort("E_SHARE_MATRIX",paste(label,"has unsupported or live annotations"))
  labels<-dimnames(x)
  plain<-function(y)typeof(y)=="character"&&!is.object(y)&&all(names(attributes(y))%in%"names")&&.bh_inert_annotation(attributes(y))&&!anyNA(y)&&!any(!nzchar(y))&&!anyDuplicated(y)
  if(length(labels)!=2L||any(!names(attributes(labels))%in%"names")||!.bh_inert_annotation(attributes(labels))||!plain(labels[[1L]])||
     (ncol(x)>0L&&!plain(labels[[2L]]))||(ncol(x)==0L&&length(labels[[2L]])>0L)) .mp_abort("E_SHARE_MATRIX_NAMES",paste(label,"requires unique plain row/column labels; zero-column contrasts have no column labels"))
  out<-matrix(as.vector(x),nrow(x),ncol(x),dimnames=list(as.vector(labels[[1L]]),if(ncol(x))as.vector(labels[[2L]])else NULL))
  list(value=out,annotations=unserialize(serialize(attributes(x)[extra],NULL,version=2)),axis_names=names(labels))
}
.bh_share_matrix_collection <- function(x,label,zero_columns=FALSE) {
  if(is.matrix(x))return(list(mode="matrix",matrices=list(.bh_share_matrix(x,label,zero_columns))))
  if(!is.list(x)||is.object(x)||!length(x)) .mp_abort("E_SHARE_MATRIX",paste(label,"must be a numeric matrix or a named active-set matrix list"))
  .mp_names(x,paste(label,"active sets"))
  list(mode="strata",matrices=lapply(x,.bh_share_matrix,label=label,zero_columns=zero_columns))
}
.bh_parse_decomposition <- function(decomposition,components) {
  if(is.null(decomposition))return(list())
  if(!is.list(decomposition)||is.object(decomposition)||!length(decomposition)) .mp_abort("E_DECOMPOSITION","decomposition requires a named nonempty owner list")
  .mp_names(decomposition,"decomposition owners")
  out<-lapply(names(decomposition),function(name) {
    d<-decomposition[[name]]
    allowed<-c("type","targets","components","residual","shares","support","contrast","contrast_keys")
    if(!is.list(d)||is.object(d)||is.null(names(d))||anyDuplicated(names(d))||any(!names(d)%in%allowed)||
       !all(c("type","targets","components","residual","shares")%in%names(d))) .mp_abort("E_DECOMPOSITION","each owner explicitly needs type, targets, components, residual and shares")
    if(!identical(d$type,"variance_share")) .mp_abort("E_FRONTEND_UNSUPPORTED","only the explicit variance_share decomposition is implemented")
    selected<-.bh_formula_targets(d$targets)
    if(!inherits(d$components,"formula")||length(d$components)!=2L) .mp_abort("E_SHARE_COMPONENTS","owner components need a one-sided whole-group formula")
    groups<-.bh_share_groups(d$components[[2L]])
    if(anyDuplicated(groups)||!all(groups%in%names(components))) .mp_abort("E_SHARE_COMPONENTS","owner groups must be distinct declared components")
    if(!is.logical(d$residual)||length(d$residual)!=1L||is.na(d$residual)) .mp_abort("E_SHARE_RESIDUAL","each owner must explicitly choose residual=TRUE or FALSE")
    if(!is.null(d$support)&&!identical(d$support,"separate")) .mp_abort("E_SHARE_SUPPORT","support currently accepts only an explicit separate active-set declaration")
    design<-if(inherits(d$shares,"formula")) {
      if(length(d$shares)!=2L||!is.numeric(.bh_unwrap(d$shares[[2L]]))||!identical(as.numeric(.bh_unwrap(d$shares[[2L]])),1)) .mp_abort("E_SHARE_DESIGN","only shares=~1 or a captured target-level numeric design is supported")
      list(mode="intercept")
    }else .bh_share_matrix_collection(d$shares,"share design")
    contrast<-if(is.null(d$contrast))list(mode="helmert")else .bh_share_matrix_collection(d$contrast,"share contrast",TRUE)
    key_mode<-if(is.null(d$contrast_keys))"auto"else d$contrast_keys
    if(!identical(key_mode,"auto")&&!identical(key_mode,"canonical")) .mp_abort("E_SHARE_CONTRAST_KEYS","contrast_keys must be auto or canonical")
    list(owner=name,targets=selected,target_keys=.bh_target_keys(selected),groups=groups,residual=d$residual,
      design=design,contrast=contrast,contrast_keys=key_mode,support=d$support)
  })
  names(out)<-names(decomposition)
  keys<-unlist(lapply(out,`[[`,"target_keys"),use.names=FALSE)
  if(anyDuplicated(keys)) .mp_abort("E_SHARE_OWNERSHIP","decomposition target domains cannot overlap")
  out
}
.bh_share_part_id <- function(group) .bh_public_id("component",group)
.bh_share_stratum_id <- function(parts) paste0("parts:",paste(sort(parts,method="radix"),collapse=","))
.bh_share_helmert <- function(parts) {
  m<-length(parts);C<-matrix(0,m,max(0L,m-1L),dimnames=list(parts,if(m>1L)paste0("h",seq_len(m-1L))else NULL))
  if(m>1L)for(k in seq_len(m-1L)){C[seq_len(k),k]<- -1/sqrt(k*(k+1));C[k+1L,k]<-k/sqrt(k*(k+1))}
  C
}
.bh_share_rank <- function(x,label,context=list()) {
  fail<-function(message,diagnostic) stop(structure(list(message=paste0("E_SHARE_DESIGN_RANK: ",message),call=NULL,code="E_SHARE_DESIGN_RANK",diagnostic=c(context,diagnostic)),class=c("measurement_error","error","condition")))
  if(any(!is.finite(x))) fail(paste(label,"cannot be represented as a finite matrix; supplied values were not changed"),list(columns=colnames(x),singular_values=NA_real_,relative_tolerance=1e-10))
  if(!ncol(x))return(list(rank=0L,singular_values=numeric(),relative_tolerance=1e-10))
  maximum<-max(abs(x));s<-if(maximum==0)rep(0,min(dim(x)))else svd(x/maximum,nu=0,nv=0)$d
  rank<-if(maximum==0)0L else sum(s>1e-10*max(s))
  if(rank<ncol(x)) fail(paste(label,"is rank deficient at relative tolerance1e-10; values and columns were not changed"),
    list(matrix_role=label,columns=colnames(x),rank=rank,singular_values=s,diagnostic_scale=maximum,relative_tolerance=1e-10,
      condition_estimate=if(!length(s)||min(s)==0)Inf else max(s)/min(s)))
  list(rank=rank,singular_values=s,diagnostic_scale=maximum,relative_tolerance=1e-10,
    scope="numerical conditioning threshold, not an exact algebraic invalidity claim")
}
.bh_share_row_match <- function(labels,parts,label) {
  keys<-.bh_target_keys(parts)
  kind<-vapply(parts,`[[`,character(1),"kind")
  short<-if(all(kind=="wide"))vapply(parts,`[[`,character(1),"column")else if(all(kind=="target")&&length(unique(vapply(parts,function(t)paste(t$block,t$parameter,sep="::"),character(1))))==1L)vapply(parts,`[[`,character(1),"task")else NULL
  at<-vapply(labels,function(x){hits<-which(keys==x);if(!is.null(short))hits<-union(hits,which(short==x));if(length(hits)!=1L)NA_integer_ else hits},integer(1))
  if(anyNA(at)||anyDuplicated(at)||!setequal(at,seq_along(parts))) .mp_abort("E_SHARE_DESIGN_ROWS",paste(label,"rows must identify every selected target exactly once; use qualified IDs across parameter/block scopes"))
  match(seq_along(parts),at)
}
.bh_share_contrast <- function(captured,part_ids,part_labels,key_mode,context=list()) {
  C<-captured$value
  if(nrow(C)!=length(part_ids)||ncol(C)>length(part_ids)-1L) .mp_abort("E_SHARE_CONTRAST","contrast dimensions must be m by d with 0 <= d <= m-1")
  at<-vapply(rownames(C),function(x) {
    hit<-which(part_ids==x)
    if(key_mode=="auto")hit<-union(hit,which(part_labels==x))
    if(length(hit)!=1L)NA_integer_ else hit
  },integer(1))
  if(anyNA(at)||anyDuplicated(at)||!setequal(at,seq_along(part_ids))) .mp_abort("E_SHARE_CONTRAST_KEYS","contrast row labels must resolve uniquely to the active parts; use contrast_keys='canonical' for typed IDs")
  C<-C[match(seq_along(part_ids),at),,drop=FALSE];rownames(C)<-part_ids
  centered<-sweep(C,2L,colMeans(C),`-`)
  rank<-.bh_share_rank(centered,"centered share contrast",context)
  list(value=C,rank=rank,input_annotations=captured$annotations,input_axis_names=captured$axis_names)
}
.bh_share_plan <- function(components,owners,coordinates) {
  if(!length(owners))return(NULL)
  group_keys<-lapply(components,function(g).bh_target_keys(.bh_component_targets(g)))
  Q<-lapply(components,function(g)if(inherits(g,"bhmm_basis_component"))g$basis else matrix(1,length(g),1L))
  owned<-parts<-cells<-coefficients<-list();strata<-list();requirements<-list()
  for(owner in owners) {
    keys<-owner$target_keys;active<-vector("list",length(keys))
    for(j in seq_along(keys)) {
      active_groups<-character()
      for(g in names(components)) {
        at<-match(keys[j],group_keys[[g]])
        if(is.na(at)||!any(Q[[g]][at,]!=0))next
        if(!g%in%owner$groups) .mp_abort("E_SHARE_EXTRA_COMPONENT",paste("owned target",keys[j],"has a nonzero component outside its part declaration:",g))
        q<-Q[[g]][at,]
        if(!is.finite(sum(q^2))||abs(sum(q^2)-1)>1e-10) .mp_abort("E_SHARE_NORM",paste("active owned basis row must have squared norm one within1e-10:",keys[j],g,"; values were not normalized"))
        active_groups<-c(active_groups,g)
        coord<-which(coordinates$group==g);used<-which(q!=0)
        cells[[length(cells)+1L]]<-data.frame(owner=owner$owner,target=keys[j],group=g,coordinate_index=coord[used],
          multiplier=q[used],part=.bh_share_part_id(g),stringsAsFactors=FALSE)
      }
      active_groups<-owner$groups[owner$groups%in%active_groups]
      active[[j]]<-c(vapply(active_groups,.bh_share_part_id,character(1),USE.NAMES=FALSE),if(owner$residual)"residual")
      if(!length(active[[j]])) .mp_abort("E_SHARE_EMPTY_PARTS",paste("owned target",keys[j],"has no active common or residual part"))
    }
    observed_groups<-unique(unlist(lapply(active,function(p)sub("^component:","",p[p!="residual"])),use.names=FALSE))
    used_groups<-owner$groups[vapply(owner$groups,function(g)any(vapply(active,function(p).bh_share_part_id(g)%in%p,logical(1))),logical(1))]
    if(!setequal(used_groups,owner$groups)) .mp_abort("E_SHARE_COMPONENTS","every owner-selected component group must have a nonzero owned row")
    stratum_id<-vapply(active,.bh_share_stratum_id,character(1));sets<-unique(stratum_id)
    if(length(sets)>1L&&!identical(owner$support,"separate")) .mp_abort("E_SHARE_SUPPORT","varying active parts require support='separate'; no cross-set logit sharing is inferred")
    if(owner$design$mode=="strata"&&!setequal(names(owner$design$matrices),sets)) .mp_abort("E_SHARE_DESIGN","design list must have exactly the resolved active-set IDs")
    if(owner$contrast$mode=="strata"&&!setequal(names(owner$contrast$matrices),sets)) .mp_abort("E_SHARE_CONTRAST","contrast list must have exactly the resolved active-set IDs")
    if(owner$contrast$mode=="matrix"&&length(sets)>1L) .mp_abort("E_SHARE_CONTRAST","varying active sets need explicitly keyed contrast matrices; no rows/columns are cropped")
    fullX<-if(owner$design$mode=="matrix") {
      x<-owner$design$matrices[[1L]]$value
      x[.bh_share_row_match(rownames(x),owner$targets,"share design"),,drop=FALSE]
    }else NULL
    for(s in sets) {
      rows<-which(stratum_id==s);ids<-active[[rows[1L]]]
      display<-vapply(ids,function(p)if(p=="residual")"residual"else owner$groups[match(p,vapply(owner$groups,.bh_share_part_id,character(1)))],character(1),USE.NAMES=FALSE)
      X<-if(owner$design$mode=="intercept")matrix(1,length(rows),1L,dimnames=list(keys[rows],"Intercept"))else if(!is.null(fullX))fullX[rows,,drop=FALSE]else {
        x<-owner$design$matrices[[s]]$value
        x[.bh_share_row_match(rownames(x),owner$targets[rows],"stratum share design"),,drop=FALSE]
      }
      rownames(X)<-keys[rows]
      contrast<-if(owner$contrast$mode=="helmert")list(value=.bh_share_helmert(ids),rank=list(rank=length(ids)-1L,relative_tolerance=1e-10),source="deterministic_orthonormal_Helmert")else
        .bh_share_contrast(owner$contrast$matrices[[if(owner$contrast$mode=="matrix")1L else s]],ids,display,owner$contrast_keys,list(owner=owner$owner,stratum=s))
      C<-contrast$value;rankX<-if(ncol(C)) .bh_share_rank(X,"within-stratum share design",list(owner=owner$owner,stratum=s))else list(rank=NA_integer_,status="no_coefficients")
      index<-length(strata)+1L
      strata[[index]]<-list(owner=owner$owner,stratum=s,targets=keys[rows],parts=ids,part_labels=display,
        X=X,C=C,design_diagnostic=rankX,contrast_diagnostic=contrast$rank,coefficient_indices=matrix(integer(),ncol(X),0L),
        contrast_scope=if(ncol(C)==length(ids)-1L)"full_simplex"else"restricted_logit_subspace",
        evaluation_gauge="column-mean offsets removed from C only when evaluating softmax; original C/B/prior preserved")
      names(strata)[index]<-.bh_public_id("share_stratum",owner$owner,s)
      if(ncol(C)) {
        grid<-expand.grid(term=colnames(X),coordinate=colnames(C),stringsAsFactors=FALSE)
        start<-length(coefficients)
        for(i in seq_len(nrow(grid)))coefficients[[length(coefficients)+1L]]<-data.frame(parameter=.bh_public_id("share",owner$owner,s,grid$term[i],grid$coordinate[i]),
          owner=owner$owner,stratum=s,term=grid$term[i],coordinate=grid$coordinate[i],stratum_index=index,stringsAsFactors=FALSE)
        strata[[index]]$coefficient_indices<-matrix(start+seq_len(nrow(grid)),ncol(X),ncol(C))
      }
      for(j in rows) {
        owned[[length(owned)+1L]]<-data.frame(owner=owner$owner,target=keys[j],parameter=.bh_public_id("total_sd",keys[j]),
          stratum=s,stratum_index=index,design_row=match(j,rows),residual=owner$residual,stringsAsFactors=FALSE)
        for(p in seq_along(ids))parts[[length(parts)+1L]]<-data.frame(owner=owner$owner,target=keys[j],part=ids[p],
          group=if(ids[p]=="residual")NA_character_ else display[p],residual=ids[p]=="residual",stratum_index=index,part_index=p,stringsAsFactors=FALSE)
      }
    }
    coord<-which(coordinates$group%in%owner$groups)
    if(length(coord)>1L) {
      pair<-t(utils::combn(coord,2L));requirements[[length(requirements)+1L]]<-data.frame(row=pair[,1L],col=pair[,2L],owner=owner$owner,stringsAsFactors=FALSE)
    }
  }
  bind<-function(x,empty)if(length(x)){y<-do.call(rbind,x);rownames(y)<-NULL;y}else empty
  list(owners=owners,owned=bind(owned,data.frame()),parts=bind(parts,data.frame()),strata=strata,
    coefficients=bind(coefficients,data.frame(parameter=character(),owner=character(),stratum=character(),term=character(),coordinate=character(),stratum_index=integer())),
    cells=bind(cells,data.frame(owner=character(),target=character(),group=character(),coordinate_index=integer(),multiplier=numeric(),part=character())),
    required_zeros=bind(requirements,data.frame(row=integer(),col=integer(),owner=character())))
}
.bh_share_correlations <- function(correlations,plan) {
  if(is.null(plan))return(correlations)
  req<-plan$required_zeros
  if(.bh_has_cor_extensions(correlations)) {
    correlations<-.bh_cor_required_zeros(correlations,req)
    correlations$required_by<-vapply(seq_len(nrow(correlations)),function(i){owners<-unique(req$owner[req$row==correlations$row[i]&req$col==correlations$col[i]]);if(length(owners))paste(owners,collapse=", ")else NA_character_},character(1))
    return(correlations)
  }
  correlations$required_by<-rep(NA_character_,nrow(correlations))
  for(i in seq_len(nrow(correlations))) {
    owners<-unique(req$owner[req$row==correlations$row[i]&req$col==correlations$col[i]])
    if(!length(owners))next
    if(correlations$state[i]=="free"&&correlations$source[i]=="explicit") .mp_abort("E_SHARE_CORRELATION_CONFLICT","explicit free component correlation conflicts with a declared owner's orthogonal variance parts")
    correlations$required_by[i]<-paste(owners,collapse=", ")
    correlations$source[i]<-if(correlations$source[i]=="explicit")"explicit_zero_and_decomposition"else"decomposition_required_zero"
    correlations$state[i]<-"zero";correlations$parameter[i]<-NA_character_
  }
  correlations
}
.bh_has_shares <- function(spec) !is.null(spec$tables$share_targets)&&nrow(spec$tables$share_targets)>0L
.bh_free_parameter_sd <- function(spec) {
  x<-spec$tables$parameter_residuals
  if("expression"%in%names(x))x<-x[x$expression=="free_parameter_sd",,drop=FALSE]
  x
}
.bh_attach_share_tables <- function(spec,plan) {
  targets<-spec$tables$targets
  owned<-plan$owned;owned$target_index<-match(owned$target,targets$key);owned$total_sd_index<-seq_len(nrow(owned))
  owned$units<-targets$predictor_units[owned$target_index]
  cells<-plan$cells;cells$target_index<-match(cells$target,targets$key)
  cells$total_sd_index<-match(cells$target,owned$target)
  parts<-plan$parts;parts$target_index<-match(parts$target,targets$key)
  parts$total_sd_index<-match(parts$target,owned$target)
  spec$tables$share_targets<-owned;spec$tables$share_parts<-parts
  spec$tables$share_cells<-cells;spec$tables$share_coefficients<-plan$coefficients
  spec$tables$share_strata<-plan$strata;spec$tables$required_component_zeros<-plan$required_zeros
  lm<-spec$tables$loading_map
  lm$owner<-rep(NA_character_,nrow(lm));lm$part<-rep(NA_character_,nrow(lm));lm$stratum<-rep(NA_character_,nrow(lm))
  lm$expression<-ifelse(lm$role=="loading","free_loading","amplitude * supplied_multiplier")
  for(i in which(lm$role=="share_derived")) {
    at<-which(cells$target==lm$target[i]&cells$coordinate_index==lm$coordinate_index[i])
    lm$owner[i]<-cells$owner[at];lm$part[i]<-cells$part[at]
    lm$stratum[i]<-owned$stratum[match(lm$target[i],owned$target)]
    lm$expression[i]<-"total_sd(target) * sqrt(share(target,part)) * supplied_multiplier"
  }
  spec$tables$loading_map<-lm
  u<-spec$tables$parameter_residuals
  u$owner<-owned$owner[match(u$target,owned$target)]
  u$total_sd_parameter<-owned$parameter[match(u$target,owned$target)]
  u$free_parameter_index<-rep(NA_integer_,nrow(u))
  u$free_parameter_index[u$expression=="free_parameter_sd"]<-seq_len(sum(u$expression=="free_parameter_sd"))
  spec$tables$parameter_residuals<-u
  spec$tables$residual_declarations<-if(is.list(spec$formula$residual))spec$formula$residual$provenance else data.frame()
  spec$tables$component_groups$kind[spec$tables$component_groups$kind=="basis_shared_amplitude"]<-"basis_loading_expression"
  spec$tables$decompositions<-data.frame(owner=names(plan$owners),residual=vapply(plan$owners,`[[`,logical(1),"residual"),
    population="iid",correlation_prior="generic_final_marginal_R",stringsAsFactors=FALSE)
  for(k in seq_len(nrow(spec$tables$components)))if(spec$tables$coordinates$kind[k]=="scalar"&&any(cells$coordinate_index==k))
    spec$tables$components$orientation[k]<-"share_owned_positive_loading"
  spec
}
.bh_share_weights <- function(spec,beta) {
  tab<-spec$tables$share_coefficients
  if(!is.numeric(beta)||length(beta)!=nrow(tab)||any(!is.finite(beta))) .mp_abort("E_SHARE_STATE","share coefficient state must be finite and match unique coefficient rows")
  out<-vector("list",length(spec$tables$share_strata))
  for(s in seq_along(out)) {
    stratum<-spec$tables$share_strata[[s]]
    B<-matrix(beta[as.vector(stratum$coefficient_indices)],nrow(stratum$coefficient_indices),ncol(stratum$coefficient_indices))
    C_eval<-sweep(stratum$C,2L,colMeans(stratum$C),`-`)
    logits<-stratum$X%*%B%*%t(C_eval)
    if(any(!is.finite(logits))) .mp_abort("E_SHARE_NUMERICAL","share design/contrast products exceed finite numeric representation; values were not clipped")
    weights<-matrix(0,nrow(logits),ncol(logits),dimnames=list(stratum$targets,stratum$parts))
    for(j in seq_len(nrow(logits))) {
      centered<-logits[j,]-max(logits[j,]);normalizer<-log(sum(exp(centered)))
      w<-exp(centered-normalizer)
      if(!is.finite(normalizer)||any(!is.finite(w))||any(w<=0)||abs(sum(w)-1)>1e-12) .mp_abort("E_SHARE_NUMERICAL","an active share cannot be represented with positive finite weight and unit sum; no epsilon or renormalized support is used")
      weights[j,]<-w
    }
    out[[s]]<-weights
  }
  names(out)<-names(spec$tables$share_strata);out
}
.bh_covariance_state <- function(spec,values,covariance=TRUE) {
  layout<-.bh_covariance_layout(spec);indices<-layout$indices
  if(!is.numeric(values)||length(values)!=sum(layout$counts)||any(!is.finite(values))) .mp_abort("E_STRUCTURE_STATE","covariance state must be finite and match unique parameter coordinates")
  positive<-unlist(indices[c("component_sd","amplitude","total_sd","parameter_sd","observation_variance")],use.names=FALSE)
  if(any(values[positive]<=0)) .mp_abort("E_STRUCTURE_STATE","scale, amplitude and observation-variance coordinates must be positive")
  J<-nrow(spec$tables$targets);K<-nrow(spec$tables$components);u<-spec$tables$parameter_residuals;U<-nrow(u)
  loads<-.bh_free_loadings(spec);Lambda<-spec$tables$ordinary_fixed_matrix
  cells<-spec$tables$ordinary_loading_map
  free_cells<-which(cells$state=="free")
  if(length(free_cells))Lambda[cbind(cells$target_index[free_cells],cells$component_index[free_cells])]<-values[indices$loading][cells$primitive_index[free_cells]]
  if(length(indices$amplitude)) {
    at<-which(spec$tables$basis_amplitude_index>0L)
    Lambda[,at]<-sweep(spec$tables$basis_multiplier[,at,drop=FALSE],2L,values[indices$amplitude][spec$tables$basis_amplitude_index[at]],`*`)
  }
  residual_sd<-numeric(U);free_u<-.bh_free_parameter_sd(spec)
  if(nrow(free_u))residual_sd[free_u$residual_index]<-values[indices$parameter_sd]
  shares<-NULL
  if(.bh_has_shares(spec)) {
    shares<-.bh_share_weights(spec,values[indices$share]);owned<-spec$tables$share_targets
    scales<-values[indices$total_sd]
    part_scale<-function(target,part) {
      t<-match(target,owned$target);s<-owned$stratum_index[t]
      value<-scales[t]*sqrt(shares[[s]][target,part])
      if(!is.finite(value)||value<=0) .mp_abort("E_SHARE_NUMERICAL","active derived scale overflowed or underflowed; no scale clipping is used")
      value
    }
    cells<-spec$tables$share_cells
    for(i in seq_len(nrow(cells))) {
      value<-part_scale(cells$target[i],cells$part[i])*cells$multiplier[i]
      if(!is.finite(value)||value==0) .mp_abort("E_SHARE_NUMERICAL","active derived loading cannot be represented; supplied basis entries were not changed")
      Lambda[cells$target_index[i],cells$coordinate_index[i]]<-value
    }
    for(i in which(u$expression=="share_derived"))residual_sd[i]<-part_scale(u$target[i],"residual")
  }
  correlation<-function(n,tab,vals) {
    R<-.bh_cor_affine_matrix(tab,n,vals)
    if(.bh_is_affine_cor_table(tab)&&!identical(.bh_cor_spd_certificate(R)$status,"strictly_positive_definite")) .mp_abort("E_CORRELATION_NUMERIC","the requested affine marginal matrix cannot be certified strictly positive definite at this precision; no clipping or domain replacement")
    if(n&&inherits(try(chol(R),silent=TRUE),"try-error")) .mp_abort("E_STRUCTURE_STATE","final marginal correlation state is not positive definite")
    R
  }
  raw_state<-if(.bh_raw_cor(spec)).bh_raw_correlation_state(spec,values[indices$component_cor],global_cholesky=!.bh_task_augmentation(spec))else NULL
  raw_task<-.bh_raw_cor(spec)&&.bh_task_augmentation(spec)
  R<-if(.bh_raw_cor(spec))raw_state$R else correlation(K,spec$tables$correlations,values[indices$component_cor])
  Ru<-correlation(U,spec$tables$parameter_correlations,values[indices$parameter_cor])
  D<-matrix(0,J,U);if(U)D[cbind(u$target_index,seq_len(U))]<-residual_sd
  component_sd<-rep(1,K)
  scaled<-.bh_free_component_sd(spec)
  if(nrow(scaled))component_sd[scaled$component_index]<-values[indices$component_sd]
  effective<-sweep(Lambda,2L,component_sd,`*`)
  if(any(!is.finite(effective))||any(Lambda!=0 & effective==0)) .mp_abort("E_COMPONENT_SCALE_NUMERICAL","effective loading overflowed or underflowed; declared coefficients and component scales are not clipped")
  if(!isTRUE(covariance)){out<-list(Lambda=Lambda,effective_loadings=effective,component_sd=component_sd,D_parameter=D,R_component=R,R_parameter=Ru,parameter_sd=residual_sd,shares=shares);if(raw_task)out$F_component<-raw_state$F_global;return(out)}
  log_component_sd<-log(component_sd)
  component_covariance_log_abs<-log(abs(R))+outer(log_component_sd,log_component_sd,`+`)
  component_covariance<-sign(R)*exp(component_covariance_log_abs)
  unavailable<-!is.finite(component_covariance)|(R!=0 & component_covariance==0)
  component_covariance_status<-if(any(unavailable))"unrepresentable_export"else"represented"
  component_covariance[unavailable]<-NA_real_
  Sigma_common<-if(raw_task)tcrossprod(effective%*%raw_state$F_global)else effective%*%R%*%t(effective);Sigma_parameter<-D%*%Ru%*%t(D)
  Sigma_predictor<-Sigma_common+Sigma_parameter
  if(any(!is.finite(c(Lambda,D,Sigma_common,Sigma_parameter,Sigma_predictor)))) .mp_abort("E_STRUCTURE_STATE","derived covariance or its common-plus-residual sum is nonfinite")
  common_active<-if(K)rowSums(effective!=0)>0L else rep(FALSE,J)
  parameter_active<-if(U)rowSums(D!=0)>0L else rep(FALSE,J)
  if(any(common_active & diag(Sigma_common)<=0)||any(parameter_active & diag(Sigma_parameter)<=0))
    .mp_abort("E_STRUCTURE_STATE","a strictly positive predictor variance was lost in covariance arithmetic; nonzero effective rows are not structural zeros")
  out<-list(Lambda=Lambda,effective_loadings=effective,component_sd=component_sd,component_covariance=component_covariance,log_component_sd=log_component_sd,component_covariance_log_abs=component_covariance_log_abs,component_covariance_status=component_covariance_status,component_covariance_unavailable=unavailable,parameter_sd=residual_sd,D_parameter=D,R_component=R,R_parameter=Ru,
    Sigma_common=Sigma_common,Sigma_parameter=Sigma_parameter,Sigma_predictor=Sigma_predictor,shares=shares)
  if(raw_task)out$F_component<-raw_state$F_global
  out
}
.bh_covariance_scales <- function(spec) {
  layout<-.bh_covariance_layout(spec);scale<-rep(1,length(layout$names))
  marked<-.bh_free_component_sd(spec)
  if(nrow(marked)) {
    column_log<-rep(0,nrow(spec$tables$components))
    column_log[marked$component_index]<-log(abs(marked$marker_value))
    sd_scale<-exp(-column_log[marked$component_index])
    free<-.bh_free_loadings(spec);cells<-spec$tables$ordinary_loading_map
    beta_scale<-vapply(free$parameter,function(id)exp(min(column_log[cells$component_index[cells$state=="free" & !is.na(cells$parameter) & cells$parameter==id]])),numeric(1))
    if(any(!is.finite(c(sd_scale,beta_scale)))||any(c(sd_scale,beta_scale)<=0)) .mp_abort("E_MARKER_DIAGNOSTIC","marker-unit probe scaling is not representable; scientific marker values and priors remain unchanged")
    scale[layout$indices$component_sd]<-sd_scale;scale[layout$indices$loading]<-beta_scale
  }
  if(.bh_has_shares(spec))for(s in spec$tables$share_strata)if(ncol(s$C)) {
    C_eval<-sweep(s$C,2L,colMeans(s$C),`-`)
    Xmax<-apply(abs(s$X),2L,max);Cmax<-apply(abs(C_eval),2L,max)
    norms<-as.vector(outer(log(Xmax),log(Cmax),`+`))
    normalized<-exp(-norms)
    if(any(!is.finite(normalized))||any(normalized<=0)) .mp_abort("E_SHARE_DIAGNOSTIC","share coordinate scaling is outside finite diagnostic representation; scientific X/C values remain unchanged")
    scale[layout$indices$share[as.vector(s$coefficient_indices)]]<-normalized
  }
  scale<-.bh_affine_probe_scales(spec,scale,layout)
  stats::setNames(scale,layout$names)
}
.bh_covariance_probe <- function(spec,probe) {
  layout<-.bh_covariance_layout(spec);loads<-.bh_free_loadings(spec);nload<-nrow(loads)
  loading<-.6+.25*sin(seq_len(nload)*(.7+.2*probe))
  loading[loads$sign=="free"]<-loading[loads$sign=="free"]*ifelse(seq_len(sum(loads$sign=="free"))%%3==0,-1,1)
  n<-layout$counts;K<-nrow(spec$tables$components);U<-nrow(spec$tables$parameter_residuals)
  values<-c(loading,.85+.11*cos(seq_len(n[["component_sd"]])+probe),.7+.1*cos(seq_len(n[["amplitude"]])+probe),.9+.18*sin(seq_len(n[["total_sd"]])+probe),
    .3*sin(seq_len(n[["share"]])*(.61+.13*probe)),if(n[["component_cor"]]).05/K*sin(seq_len(n[["component_cor"]])+probe),
    .8+.12*sin(seq_len(n[["parameter_sd"]])+probe),if(n[["parameter_cor"]]).05/U*cos(seq_len(n[["parameter_cor"]])+probe),
    1+.1*cos(seq_len(n[["observation_variance"]])+probe))
  .bh_affine_probe_values(spec,unname(values*.bh_covariance_scales(spec)),layout,probe)
}
.bh_share_identification <- function(spec,gaussian,moments) {
  if(!.bh_has_shares(spec)||.bh_is_affine_cor_table(spec$tables$parameter_correlations))return(NULL)
  strata<-spec$tables$share_strata;owned<-spec$tables$share_targets
  repeated<-if(is.null(moments)) {
    obs<-.bh_observation_targets(spec,informative=TRUE)
    unique(unlist(lapply(split(obs$target,obs$person),function(keys){counts<-table(keys);names(counts)[counts>1L]}),use.names=FALSE))
  }else .bh_gaussian_responses(spec)$key[moments$row[moments$kind=="repeat_covariance"]]
  fixed_noise<-if(gaussian){r<-.bh_gaussian_responses(spec);obs<-.bh_observation_targets(spec,informative=TRUE);r$key[!r$noise_free & r$key%in%obs$target]}else character()
  pinned<-union(repeated,fixed_noise)
  if(gaussian)for(s in strata)if("residual"%in%s$parts) {
    repeat_rows<-which(s$targets%in%pinned)
    if(length(s$parts)==1L&&length(repeat_rows)<length(s$targets))return(list(reason="residual-only total SD is independently adjustable and confounded with free Gaussian observation SD without a repeat",
      proof="one-part weight is exactly one, so total_sd is an ordinary residual SD; fixed observation noise pins the transformation",targets=setdiff(s$targets,pinned)))
    full<-ncol(s$C)==length(s$parts)-1L
    certificate<-if(!length(repeat_rows))"no repeated target rows"else if(ncol(s$X)>length(repeat_rows))"more share-design columns than repeated rows"else if(any(colSums(abs(s$X[repeat_rows,,drop=FALSE]))==0))"an exact zero-on-repeat share-design column"else NULL
    if(length(s$parts)>1L&&full&&!is.null(certificate))return(list(reason="a residual-versus-common share-logit direction trades stable residual variance against Gaussian observation noise while preserving the observed law",
      proof="delta=X*gamma vanishes at targets pinned by repeats or fixed observation sigma; total_sd, residual shares/correlations and free observation variances can be jointly rescaled locally",certificate=certificate,stratum=s$stratum,owner=s$owner,pinned_targets=intersect(s$targets,pinned)))
  }
  # A separate hierarchy proof: a single common part plus full residual R,
  # one shared intercept weight, and no external common/residual effects.
  if(length(strata)==1L&&nrow(owned)==nrow(spec$tables$targets)&&nrow(spec$tables$decompositions)==1L) {
    s<-strata[[1L]];J<-nrow(owned);K<-nrow(spec$tables$components)
    if(length(s$parts)==2L&&"residual"%in%s$parts&&K==1L&&ncol(s$C)==1L&&ncol(s$X)==1L&&
       all(s$X[,1L]==s$X[1L,1L])&&s$X[1L,1L]!=0&&nrow(spec$tables$parameter_residuals)==J&&
       all(spec$tables$parameter_correlations$state=="free")&&nrow(.bh_free_loadings(spec))==0L&&nrow(.bh_amplitudes(spec))==0L)
      return(list(reason="one isolated common share and a fully free residual correlation matrix admit a weight/residual-correlation tradeoff preserving the entire latent covariance",
        proof="S=w*q*q'+(1-w)*R_u; vary w and solve R_u'=(S-w'*q*q')/(1-w') locally",scope="single owner covers the whole hierarchy; no external loading or residual effect"))
  }
  NULL
}
# Lower one resolved loading-expression IR; no scientific component names occur
# in the generated program. The same empty projection covers no-owner models.
.bh_share_backend_data <- function(spec) {
  U<-nrow(spec$tables$parameter_residuals);free_u<-.bh_free_parameter_sd(spec)
  parameter_sd_index<-integer(U)
  if(nrow(free_u))parameter_sd_index[free_u$residual_index]<-seq_len(nrow(free_u))
  empty<-list(T=0L,H=0L,W=0L,V=0L,UF=nrow(free_u),share_design=matrix(numeric(),0L,0L),
    share_target=integer(),share_weight_start=integer(),share_weight_count=integer(),
    share_weight_total_sd=integer(),share_cell_target=integer(),share_cell_component=integer(),
    share_cell_weight=integer(),share_cell_multiplier=numeric(),parameter_sd_index=parameter_sd_index,
    parameter_share_weight=integer(U))
  if(!.bh_has_shares(spec)) {
    if(!is.null(spec$formula$decomposition)||!is.null(spec$tables$share_targets)) .mp_abort("E_SHARE_IR","empty-owner projection requires a validated declaration without hidden ownership fields")
    return(empty)
  }
  owned<-spec$tables$share_targets;parts<-spec$tables$share_parts;cells<-spec$tables$share_cells
  T<-nrow(owned);H<-nrow(spec$tables$share_coefficients);W<-nrow(parts)
  weight_key<-vapply(seq_len(W),function(i).bh_public_id("share_weight",parts$target[i],parts$part[i]),character(1))
  if(anyDuplicated(weight_key)) .mp_abort("E_SHARE_IR","active target/part weights are duplicated")
  weight_counts<-vapply(owned$target,function(t)sum(parts$target==t),integer(1))
  weight_start<-match(owned$target,parts$target)
  if(anyNA(weight_start)||any(weight_counts<1L)||!identical(as.integer(parts$total_sd_index),rep(seq_len(T),weight_counts))) .mp_abort("E_SHARE_IR","owned target weights must form one complete contiguous active-part map")
  design<-matrix(0,W,H)
  for(s in spec$tables$share_strata)if(ncol(s$C)) {
    C_eval<-sweep(s$C,2L,colMeans(s$C),`-`)
    for(j in seq_along(s$targets))for(p in seq_along(s$parts)) {
      w<-match(.bh_public_id("share_weight",s$targets[j],s$parts[p]),weight_key)
      products<-outer(s$X[j,],C_eval[p,],`*`)
      nonzero<-outer(s$X[j,]!=0,C_eval[p,]!=0,`&`)
      if(any(!is.finite(products))||any(nonzero&products==0)) .mp_abort("E_SHARE_NUMERICAL","captured X/C logit design cannot be represented as finite nonzero products; no clipping or column deletion is used")
      design[w,as.vector(s$coefficient_indices)]<-as.vector(products)
    }
  }
  cell_weight<-vapply(seq_len(nrow(cells)),function(i)match(.bh_public_id("share_weight",cells$target[i],cells$part[i]),weight_key),integer(1))
  u<-spec$tables$parameter_residuals;u_weight<-integer(U)
  for(i in which(u$expression=="share_derived"))u_weight[i]<-match(.bh_public_id("share_weight",u$target[i],"residual"),weight_key)
  if(anyNA(c(cell_weight,u_weight))||any((parameter_sd_index>0L)+(u_weight>0L)!=1L)) .mp_abort("E_SHARE_IR","each residual row must consume exactly one free or derived SD expression")
  list(T=T,H=H,W=W,V=nrow(cells),UF=nrow(free_u),share_design=design,
    share_target=owned$target_index,share_weight_start=weight_start,share_weight_count=unname(weight_counts),
    share_weight_total_sd=parts$total_sd_index,share_cell_target=cells$target_index,
    share_cell_component=cells$coordinate_index,share_cell_weight=cell_weight,share_cell_multiplier=cells$multiplier,
    parameter_sd_index=parameter_sd_index,parameter_share_weight=u_weight)
}
