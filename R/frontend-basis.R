# Fixed numeric bases are loading expressions with one unique positive amplitude.
component <- function(targets, basis, loading) {
  if(missing(loading)).mp_abort("E_BASIS_LOADING","declare loading=shared explicitly: it means one positive amplitude multiplying the supplied basis; other loading maps are not implemented")
  if(!identical(loading,"shared")&&!identical(loading,"decomposition")) .mp_abort("E_FRONTEND_UNSUPPORTED","numeric basis loading must be shared or explicit decomposition")
  if(!inherits(targets,"formula")||length(targets)!=2L).mp_abort("E_FORMULA","component targets require a one-sided target formula")
  parts<-.bh_formula_targets(targets)
  .bh_make_basis_component(parts,basis,loading)
}
.bh_make_basis_component <- function(parts,basis,loading="shared") {
  if(!typeof(basis)%in%c("integer","double")||is.object(basis)||!is.matrix(basis)||any(dim(basis)<1L)||any(!is.finite(basis)))
    .mp_abort("E_BASIS","basis must be a plain finite real numeric matrix with positive dimensions")
  extra<-setdiff(names(attributes(basis)),c("dim","dimnames"))
  if(any(!extra%in%c("assign","contrasts"))||any(!vapply(attributes(basis)[extra],.bh_inert_annotation,logical(1))))
    .mp_abort("E_BASIS","basis accepts only inert standard assign/contrasts annotations in addition to dimensions and labels")
  annotations<-unserialize(serialize(attributes(basis)[extra],NULL,version=2))
  labels<-dimnames(basis)
  valid_names<-function(x)typeof(x)=="character"&&!is.object(x)&&
    all(names(attributes(x))%in%"names")&&.bh_inert_annotation(attributes(x))
  if(length(labels)!=2L||any(!names(attributes(labels))%in%"names")||!.bh_inert_annotation(attributes(labels))||
     any(!vapply(labels,valid_names,logical(1))))
    .mp_abort("E_BASIS_NAMES","basis needs plain character row and column labels without live or method-bearing attributes")
  axis_names<-names(labels)
  labels<-unname(lapply(labels,as.vector))
  basis<-matrix(as.vector(basis),nrow(basis),ncol(basis),dimnames=labels)
  if(any(vapply(labels,function(x)anyNA(x)||any(!nzchar(x))||anyDuplicated(x)>0L,logical(1))))
    .mp_abort("E_BASIS_NAMES","basis row and column labels must be distinct, nonempty and nonmissing")
  if(nrow(basis)!=length(parts)).mp_abort("E_BASIS_ROWS","basis row count must equal the captured target count")
  kinds<-vapply(parts,`[[`,character(1),"kind")
  if(length(unique(kinds))!=1L).mp_abort("E_BASIS_ROWS","use one target-binding syntax within a basis component")
  keys<-vapply(parts,function(t)if(t$kind=="wide")targets("scores","mu",t$column)else targets(t$block,t$parameter,t$task),character(1))
  short<-if(kinds[1L]=="wide")vapply(parts,`[[`,character(1),"column")else if(length(unique(vapply(parts,function(t)paste(t$block,t$parameter,sep="::"),character(1))))==1L)vapply(parts,`[[`,character(1),"task")else NULL
  at<-match(labels[[1L]],keys)
  if(!is.null(short))at[is.na(at)]<-match(labels[[1L]][is.na(at)],short)
  if(anyNA(at)||anyDuplicated(at)||!setequal(at,seq_along(parts)))
    .mp_abort("E_BASIS_ROWS","basis row labels must match every selected target exactly once; multi-block/parameter bases require qualified target IDs")
  order<-match(seq_along(parts),at)
  captured<-unserialize(serialize(basis[order,,drop=FALSE],NULL,version=2))
  diagnostic<-list(status="not_assessed_size_limit",rank=NA_integer_,relative_tolerance=1e-8,condition=NA_real_)
  if(max(dim(captured))<=200L&&length(captured)<=20000L) {
    maximum<-max(abs(captured))
    singular<-if(maximum==0)rep(0,min(dim(captured)))else svd(captured/maximum,nu=0,nv=0)$d
    diagnostic<-list(status="computed",rank=if(maximum==0)0L else sum(singular>max(singular)*1e-8),
      relative_tolerance=1e-8,condition=if(!length(singular)||min(singular)==0)Inf else max(singular)/min(singular),
      scope="diagnostic scaling only; captured numeric entries and structural zeros are unchanged")
  }
  out<-structure(list(targets=parts,basis=captured,input_row_labels=labels[[1L]],input_row_index=order,
    target_keys=keys,input_annotations=annotations,input_axis_names=axis_names,loading=if(loading=="shared")"shared_positive_amplitude"else"decomposition_only",diagnostics=diagnostic,version="numeric_basis_shared_v1"),class="bhmm_basis_component")
  out$fingerprint<-.mp_hash(out);out
}
.bh_validate_basis <- function(x) {
  if(!inherits(x,"bhmm_basis_component")).mp_abort("E_BASIS","expected a captured numeric basis component")
  original<-x;fingerprint<-original$fingerprint;original$fingerprint<-NULL
  if(!identical(fingerprint,.mp_hash(original))).mp_abort("E_SPEC_CHANGED","captured basis component changed; construct it again with component()")
  # Revalidate captured plain values and bijection without re-evaluating an environment.
  probe<-.bh_make_basis_component(x$targets,x$basis,if(identical(x$loading,"decomposition_only"))"decomposition"else"shared")
  if(!identical(probe$basis,x$basis)||!identical(probe$target_keys,x$target_keys)) .mp_abort("E_BASIS","captured basis values or target map are inconsistent")
  invisible(TRUE)
}
.bh_component_targets <- function(x)if(inherits(x,"bhmm_basis_component"))x$targets else x
.bh_free_loadings <- function(spec)if(is.null(spec$tables$free_loadings))spec$tables$loadings else spec$tables$free_loadings
.bh_amplitudes <- function(spec) {
  if(!is.null(spec$tables$amplitudes))return(spec$tables$amplitudes)
  data.frame(component=character(),parameter=character(),amplitude_index=integer(),units=character(),stringsAsFactors=FALSE)
}
.bh_component_coordinates <- function(parsed) {
  rows<-lapply(seq_along(parsed),function(g) {
    name<-names(parsed)[g];basis<-inherits(parsed[[g]],"bhmm_basis_component")
    labels<-if(basis)colnames(parsed[[g]]$basis)else NA_character_
    display<-if(basis)paste0(name,"[",vapply(labels,encodeString,character(1),quote='"'),"]")else name
    data.frame(group=name,group_index=g,kind=if(basis)"basis"else"scalar",coordinate=labels,display=display,
      id=vapply(seq_along(labels),function(k)if(basis).bh_public_id("basis_coordinate",name,labels[k])else .bh_public_id("scalar_coordinate",name),character(1)),stringsAsFactors=FALSE)
  })
  out<-if(length(rows))do.call(rbind,rows)else data.frame(group=character(),group_index=integer(),kind=character(),coordinate=character(),display=character(),id=character())
  rownames(out)<-NULL;out$index<-seq_len(nrow(out))
  if(anyDuplicated(out$id)||anyDuplicated(out$display)).mp_abort("E_COORDINATE_COLLISION","scalar and expanded basis coordinate names must have unambiguous distinct identities/displays")
  out
}
.bh_coordinate_selection <- function(x,coordinates) {
  x<-.bh_unwrap(x)
  if(is.symbol(x)) {
    name<-as.character(x);at<-which(coordinates$group==name)
    if(!length(at)).mp_abort("E_CORRELATION_TARGET",paste("unknown component group",name))
    return(at)
  }
  if(is.call(x)&&identical(x[[1L]],as.name("+"))&&length(x)==3L)
    return(c(.bh_coordinate_selection(x[[2L]],coordinates),.bh_coordinate_selection(x[[3L]],coordinates)))
  if(is.call(x)&&identical(x[[1L]],as.name("["))&&length(x)==3L&&is.symbol(x[[2L]])&&
     typeof(x[[3L]])=="character"&&!is.object(x[[3L]])&&length(x[[3L]])==1L&&!is.na(x[[3L]])) {
    at<-which(coordinates$kind=="basis"&coordinates$group==as.character(x[[2L]])&coordinates$coordinate==x[[3L]])
    if(length(at)!=1L).mp_abort("E_CORRELATION_TARGET","unknown or ambiguous exact basis coordinate")
    return(at)
  }
  .mp_abort("E_CORRELATION_SYNTAX","use group names or one exact literal coordinate such as P[\"linear\"], joined by + or disjoint cross groups")
}
.bh_expanded_cor_pairs <- function(rule,components,coordinates) {
  if(!inherits(rule,"formula")||length(rule)!=2L).mp_abort("E_CORRELATION_SYNTAX","correlation rules require one-sided formulas")
  rhs<-.bh_unwrap(rule[[2L]])
  if(is.call(rhs)&&identical(rhs[[1L]],as.name(":"))&&length(rhs)==3L) {
    a<-.bh_coordinate_selection(rhs[[2L]],coordinates);b<-.bh_coordinate_selection(rhs[[3L]],coordinates)
    if(anyDuplicated(c(a,b))).mp_abort("E_CORRELATION_SYNTAX","expanded correlation cross groups must be distinct and disjoint")
    pairs<-as.matrix(expand.grid(a=a,b=b))
  }else {
    selected<-.bh_coordinate_selection(rhs,coordinates)
    if(length(selected)<2L||anyDuplicated(selected)).mp_abort("E_CORRELATION_SYNTAX","an expanded clique needs two or more distinct scalar coordinates")
    pairs<-t(utils::combn(selected,2L))
  }
  t(apply(pairs,1L,sort))
}
.bh_bind_component_map <- function(formula,selected,primary) {
  coordinates<-formula$coordinates
  share<-formula$decomposition
  owned_keys<-share$owned$target
  empty_loads<-function()data.frame(target=character(),target_index=integer(),component=character(),component_index=integer(),parameter=character(),sign=character(),stringsAsFactors=FALSE)
  ordinary<-cells<-maps<-amps<-rows<-groups<-list();basis_multiplier<-matrix(0,nrow(primary),nrow(coordinates));basis_index<-integer(nrow(coordinates))
  for(g in seq_along(formula$components)) {
    name<-names(formula$components)[g];declaration<-formula$components[[g]];keys<-selected[[g]]
    coordinate_ids<-which(coordinates$group_index==g)
    is_basis<-inherits(declaration,"bhmm_basis_component")
    groups[[g]]<-data.frame(component=name,kind=if(is_basis)"basis_shared_amplitude"else"scalar_free_loadings",dimensions=length(coordinate_ids),stringsAsFactors=FALSE)
    if(!is_basis) {
      free_keys<-keys[!keys%in%owned_keys]
      owned_positive<-any(keys%in%owned_keys)
      entry<-data.frame(target=free_keys,target_index=match(free_keys,primary$key),component=rep(name,length(free_keys)),component_index=rep(coordinate_ids,length(free_keys)),
        parameter=vapply(free_keys,function(t).bh_public_id("loading",name,t),character(1)),
        sign=if(owned_positive)rep("free",length(free_keys))else if(length(free_keys))c("positive_anchor",rep("free",length(free_keys)-1L))else character(),stringsAsFactors=FALSE)
      ordinary[[length(ordinary)+1L]]<-entry;cells[[length(cells)+1L]]<-entry
      maps[[length(maps)+1L]]<-data.frame(target=entry$target,target_index=entry$target_index,group=rep(name,nrow(entry)),
        coordinate=rep(coordinates$display[coordinate_ids],nrow(entry)),coordinate_id=rep(coordinates$id[coordinate_ids],nrow(entry)),coordinate_index=entry$component_index,
        coefficient=entry$parameter,role=rep("loading",nrow(entry)),multiplier=rep(1,nrow(entry)),stringsAsFactors=FALSE)
    }else {
      .bh_validate_basis(declaration)
      Q_original<-declaration$basis;Q<-Q_original;target_index<-match(keys,primary$key)
      Q[keys%in%owned_keys,]<-0
      active_rows<-rowSums(Q!=0)>0L
      if(identical(declaration$loading,"decomposition_only")&&any(active_rows)) .mp_abort("E_SHARE_UNOWNED_BASIS","loading=decomposition requires every nonzero basis row to be owned; no amplitude fallback was declared")
      unit<-unique(primary$predictor_units[target_index[active_rows]])
      if(length(unit)>1L).mp_abort("E_BASIS_UNITS","nonzero shared-amplitude basis rows require one predictor-unit scale; cross-unit conversions are not inferred")
      if(!length(unit))unit<-"undetermined_zero_effect"
      allocate<-any(active_rows)||(identical(declaration$loading,"shared_positive_amplitude")&&!any(Q_original!=0))
      if(allocate) {
      a<-length(amps)+1L;id<-.bh_public_id("amplitude",name)
      amps[[a]]<-data.frame(component=name,parameter=id,amplitude_index=a,units=unit,stringsAsFactors=FALSE)
      basis_multiplier[target_index,coordinate_ids]<-Q;basis_index[coordinate_ids]<-a
      }
      used<-which(Q!=0,arr.ind=TRUE)
      if(nrow(used)) {
        entry<-data.frame(target=keys[used[,1]],target_index=target_index[used[,1]],component=coordinates$display[coordinate_ids[used[,2]]],
          component_index=coordinate_ids[used[,2]],parameter=id,sign="basis_derived",stringsAsFactors=FALSE)
        cells[[length(cells)+1L]]<-entry
        maps[[length(maps)+1L]]<-data.frame(target=entry$target,target_index=entry$target_index,group=name,
          coordinate=entry$component,coordinate_id=coordinates$id[entry$component_index],coordinate_index=entry$component_index,
          coefficient=id,role="amplitude",multiplier=Q[used],stringsAsFactors=FALSE)
      }
      rows[[length(rows)+1L]]<-data.frame(component=name,target=keys,target_index=target_index,
        input_row_label=declaration$input_row_labels[declaration$input_row_index],input_row_index=declaration$input_row_index,
        zero_row=rowSums(Q_original!=0)==0L,stringsAsFactors=FALSE)
    }
    owned_cells<-if(is.null(share))NULL else share$cells[share$cells$group==name,,drop=FALSE]
    if(!is.null(owned_cells)&&nrow(owned_cells)) {
      ti<-match(owned_cells$target,primary$key);ci<-owned_cells$coordinate_index
      tau<-share$owned$parameter[match(owned_cells$target,share$owned$target)]
      entry<-data.frame(target=owned_cells$target,target_index=ti,component=coordinates$display[ci],component_index=ci,parameter=tau,sign="share_derived",stringsAsFactors=FALSE)
      cells[[length(cells)+1L]]<-entry
      maps[[length(maps)+1L]]<-data.frame(target=entry$target,target_index=ti,group=name,coordinate=entry$component,coordinate_id=coordinates$id[ci],coordinate_index=ci,coefficient=tau,role="share_derived",multiplier=owned_cells$multiplier,stringsAsFactors=FALSE)
    }
  }
  bind<-function(x,empty)if(length(x)){ans<-do.call(rbind,x);rownames(ans)<-NULL;ans}else empty
  free<-bind(ordinary,empty_loads());all<-bind(cells,empty_loads())
  amplitude<-bind(amps,data.frame(component=character(),parameter=character(),amplitude_index=integer(),units=character()))
  coefficient<-rbind(data.frame(parameter=free$parameter,role=rep("loading",nrow(free)),component=free$component,
      target=free$target,support=ifelse(free$sign=="positive_anchor","positive","real"),stringsAsFactors=FALSE),
    data.frame(parameter=amplitude$parameter,role=rep("amplitude",nrow(amplitude)),component=amplitude$component,
      target=rep(NA_character_,nrow(amplitude)),support=rep("positive",nrow(amplitude)),stringsAsFactors=FALSE))
  coefficient$coefficient_index<-seq_len(nrow(coefficient))
  map<-bind(maps,data.frame(target=character(),target_index=integer(),group=character(),coordinate=character(),coordinate_id=character(),coordinate_index=integer(),coefficient=character(),role=character(),multiplier=numeric()))
  map$coefficient_index<-match(map$coefficient,coefficient$parameter)
  list(loadings=all,free_loadings=free,amplitudes=amplitude,loading_coefficients=coefficient,loading_map=map,
    component_groups=bind(groups,data.frame(component=character(),kind=character(),dimensions=integer())),
    basis_rows=bind(rows,data.frame(component=character(),target=character(),target_index=integer(),input_row_label=character(),input_row_index=integer(),zero_row=logical())),
    basis_multiplier=basis_multiplier,basis_amplitude_index=basis_index,coordinates=coordinates)
}
# A release gate, not a scientific restriction on valid declarations.
.bh_coefficient_generator_qualified <- function() TRUE
.bh_basis_identification <- function(spec) {
  amplitudes<-.bh_amplitudes(spec)
  if(!nrow(amplitudes))return(NULL)
  map<-spec$tables$loading_map;coordinates<-spec$tables$coordinates;cor<-spec$tables$correlations
  unused<-amplitudes$parameter[!amplitudes$parameter%in%map$coefficient]
  if(length(unused))return(list(reason="a declared shared amplitude multiplies an entirely zero basis and has no likelihood effect",parameters=unused,proof="all loading expressions for this amplitude are zero"))
  if(.bh_is_affine_cor_table(cor))return(NULL)
  # Narrow exact shear: one basis coordinate and an ordinary fully adjustable
  # loading column spanning its nonzero support, with no external correlations.
  for(a in seq_len(nrow(amplitudes))) {
    p<-which(coordinates$group==amplitudes$component[a])
    if(length(p)!=1L||(.bh_has_shares(spec)&&any(spec$tables$share_cells$coordinate_index%in%p)))next
    support<-map$target[map$coordinate_index==p]
    for(g in which(coordinates$kind=="scalar")) {
      if(g%in%.bh_free_component_sd(spec)$component_index)next
      if(.bh_has_shares(spec)&&any(spec$tables$share_cells$coordinate_index==g))next
      edge<-which(cor$row==min(p,g)&cor$col==max(p,g))
      if(length(edge)!=1L||cor$state[edge]!="free")next
      incident<-which((cor$row%in%c(p,g)|cor$col%in%c(p,g))&cor$state=="free")
      if(!identical(incident,edge))next
      cells<-spec$tables$ordinary_loading_map
      gcells<-cells[cells$component_index==g,,drop=FALSE]
      if(!all(support%in%gcells$target))next
      # Current sufficient certificates require singleton free coefficients and
      # no equality to any outside cell. Their scientific map is checked first.
      free_g<-gcells[gcells$state=="free",,drop=FALSE]
      if(anyDuplicated(free_g$parameter)||any(vapply(free_g$parameter,function(id)sum(cells$parameter==id,na.rm=TRUE)!=1L,logical(1))))next
      active_fixed<-gcells[gcells$target%in%support&gcells$state=="fixed",,drop=FALSE]
      if(nrow(active_fixed)) {
        if(nrow(active_fixed)==1L&&active_fixed$fixed_value!=0&&nrow(coordinates)==2L&&
           all(gcells$target%in%support)&&sum(gcells$state=="fixed")==1L) {
          return(list(reason="one nonzero fixed ordinary loading and a freely correlated positive basis amplitude retain an exact discrete covariance alias despite possible full local rank",
            components=c(coordinates$group[g],coordinates$group[p]),fixed_cell=active_fixed$cell_id,
            proof="t=r*h; xi=lambda+t*q; u=h*sqrt(1-r^2). Set t_new=-2*c/q_j-t, lambda_new=-lambda+2*c*q/q_j, h_new=sqrt(u^2+t_new^2), r_new=t_new/h_new. Then xi_new=-xi and covariance is unchanged",
            scope="isolated two-coordinate G plus one-column positive basis, exactly one nonzero fixed loading at nonzero q, other G cells singleton free over basis support, no ownership or other common correlations"))
        }
        next
      }
      return(list(reason="a freely correlated fixed-basis amplitude and adjustable scalar loading column admit an exact local shear preserving predictor covariance",
        components=c(coordinates$group[g],coordinates$group[p]),
        proof="a=g+h*rho*q, b=h*sqrt(1-rho^2); vary rho=t, h=b/sqrt(1-t^2), g=a-h*t*q. Anchor positivity is preserved locally, not its numerical value",
        scope="one basis coordinate, scalar free loading on every nonzero basis row, no correlations to other common coordinates; independent residual layer is unchanged"))
    }
  }
  NULL
}

# Standard numeric-model-matrix annotations are copied as inert data only.
.bh_inert_annotation <- function(x,depth=0L) {
  if(depth>8L||is.object(x)||!typeof(x)%in%c("NULL","logical","integer","double","character","list"))return(FALSE)
  if(is.list(x)&&!all(vapply(x,function(y).bh_inert_annotation(y,depth+1L),logical(1))))return(FALSE)
  a<-attributes(x)
  is.null(a)||all(vapply(a,function(y).bh_inert_annotation(y,depth+1L),logical(1)))
}
