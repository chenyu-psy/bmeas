# Final marginal correlation constraints. Directions are stored sparsely by
# unique label; no dense K by K by number-of-labels array is allocated.
.bh_cor_extensions <- function(cor) {
  if(!is.list(cor))return(NULL)
  extra<-cor[intersect(names(cor),c("fixed","equal"))]
  if(!length(extra))return(NULL)
  for(name in names(extra))if(!is.null(extra[[name]])&&(!is.list(extra[[name]])||is.object(extra[[name]])))
    .mp_abort("E_CORRELATION_SYNTAX",paste(name,"requires a plain rule list"))
  extra<-extra[lengths(extra)>0L]
  if(length(extra))extra else NULL
}
.bh_has_cor_extensions <- function(table)!is.null(attr(table,"affine_rules"))
.bh_is_affine_cor_table <- function(table) {
  any(table$state=="fixed")||anyDuplicated(table$parameter[table$state=="free"])>0L
}
.bh_affine_cor_table <- function(cor,components,resolve_pairs) {
  extra<-.bh_cor_extensions(cor);base<-cor;base$fixed<-base$equal<-NULL
  out<-.bh_cor_table(base,components,resolve_pairs)
  zeros<-which(out$state=="zero"&out$source=="explicit")
  pins<-data.frame(edge=zeros,value=rep(0,length(zeros)),source=rep("explicit_zero",length(zeros)),stringsAsFactors=FALSE)
  # Construct zero-length columns explicitly, including K=0/1.
  rules<-list();equal<-list()
  one_pair<-function(formula) {
    pair<-resolve_pairs(formula,components)
    if(nrow(pair)!=1L).mp_abort("E_CORRELATION_PAIR","a fixed/equal selector must resolve exactly one final scalar pair")
    edge<-match(paste(pair[1L,1L],pair[1L,2L]),paste(out$row,out$col))
    if(is.na(edge)).mp_abort("E_CORRELATION_PAIR","the selected scalar pair is absent")
    text<-paste(deparse(formula[[2L]],width.cutoff=500L,control=c("keepNA","keepInteger","niceNames")),collapse=" ")
    list(edge=edge,capture=list(expression_text=text,endpoints=components[pair[1L,]],indices=as.integer(pair[1L,]),capture_mode="resolved_symbolic_text_without_environment_or_AST_annotations"))
  }
  record<-function(rule,kind,label,value,edge,capture) {
    row<-data.frame(rule=rule,kind=kind,label=label,value=value,edge=edge,stringsAsFactors=FALSE)
    row$captured_selector<-I(list(capture));rules[[length(rules)+1L]]<<-row
  }
  if(length(extra$equal)) {
    .mp_names(extra$equal,"correlation equality labels")
    for(label in names(extra$equal)) {
      selectors<-extra$equal[[label]]
      if(!is.list(selectors)||is.object(selectors)||!length(selectors)).mp_abort("E_CORRELATION_SYNTAX","an equality class requires a nonempty list of scalar-pair formulas")
      pairs<-lapply(selectors,one_pair);edges<-unique(vapply(pairs,`[[`,integer(1),"edge"))
      if(length(edges)<2L).mp_abort("E_CORRELATION_PAIR","an equality class requires at least two distinct scalar pairs")
      equal[[length(equal)+1L]]<-list(label=label,edges=edges)
      for(pair in pairs)record(paste0("equal:",label),"equal",label,NA_real_,pair$edge,pair$capture)
    }
  }
  for(i in seq_along(extra$fixed)) {
    rule<-extra$fixed[[i]]
    if(!is.list(rule)||is.object(rule)||anyDuplicated(names(rule))||!setequal(names(rule),c("pairs","value")))
      .mp_abort("E_CORRELATION_SYNTAX","each fixed rule needs exactly pairs and value")
    value<-rule$value
    if(!typeof(value)%in%c("integer","double")||is.object(value)||length(attributes(value))||length(value)!=1L||!is.finite(value)||abs(value)>1)
      .mp_abort("E_CORRELATION_VALUE","a fixed correlation is one plain finite value in [-1,1]; boundary values have no strictly positive-definite support")
    value<-if(value==0)0 else as.numeric(value)
    pair<-one_pair(rule$pairs);id<-paste0("fixed:",i)
    pins<-rbind(pins,data.frame(edge=pair$edge,value=value,source=id))
    record(id,"fixed",NA_character_,value,pair$edge,pair$capture)
  }
  attr(out,"affine_rules")<-list(base=out,equal=equal,pins=pins,
    explicit_free=which(out$state=="free"&out$source=="explicit"),
    rules=if(length(rules))do.call(rbind,rules)else data.frame(rule=character(),kind=character(),label=character(),value=numeric(),edge=integer()))
  .bh_resolve_cor_classes(out)
}
.bh_resolve_cor_classes <- function(table) {
  info<-attr(table,"affine_rules");if(is.null(info))return(table)
  out<-info$base;n<-nrow(out);parent<-seq_len(n)
  root<-function(i){while(parent[i]!=i)i<-parent[i];i}
  for(group in info$equal){at<-vapply(group$edges,root,integer(1));parent[at]<-min(at)}
  groups<-if(n)split(seq_len(n),vapply(seq_len(n),root,integer(1)))else list()
  out$constraint_group<-integer(n);out$fixed_value<-rep(NA_real_,n)
  out$equality_labels<-I(rep(list(character()),n));out$constraint_sources<-I(rep(list(character()),n))
  for(at in groups) {
    pins<-info$pins[info$pins$edge%in%at,,drop=FALSE];values<-unique(pins$value)
    if(length(values)>1L).mp_abort("E_CORRELATION_CONFLICT",paste("different fixed values propagate into one correlation class:",paste(pins$source,collapse=", ")))
    labels<-vapply(Filter(function(x)any(x$edges%in%at),info$equal),`[[`,character(1),"label")
    if(length(values)&&any(at%in%info$explicit_free)).mp_abort("E_CORRELATION_CONFLICT",paste("an explicit free edge is fixed by the resolved equality/zero constraints:",paste(at,collapse=", ")))
    fixed<-if(length(values))values else if(!length(labels)&&all(out$state[at]=="zero"))0 else NA_real_
    out$constraint_group[at]<-min(at);out$fixed_value[at]<-fixed
    out$state[at]<-if(is.na(fixed))"free"else if(fixed==0)"zero"else"fixed"
    if(length(labels)||nrow(pins))out$source[at]<-if(is.na(fixed))"equality_class"else"resolved_fixed_class"
    out$equality_labels[at]<-rep(list(sort(unique(labels),method="radix")),length(at))
    out$constraint_sources[at]<-rep(list(unique(c(pins$source,if(length(labels))paste0("equal:",labels)else character()))),length(at))
  }
  attr(out,"affine_rules")<-info
  .bh_cor_rekey(out,"component")
}
.bh_cor_rekey <- function(table,layer) {
  if(!.bh_has_cor_extensions(table))return(table)
  a<-if(layer=="component")table$component1 else table$target1
  b<-if(layer=="component")table$component2 else table$target2
  table$parameter<-rep(NA_character_,nrow(table));free<-which(table$state=="free")
  for(at in split(free,table$constraint_group[free])) {
    if(length(at)==1L)id<-.bh_public_id(if(layer=="component")"cor"else"parameter_cor",a[at],b[at])else {
      pair<-vapply(at,function(i){x<-sort(c(a[i],b[i]),method="radix");.bh_public_id("pair",x[1],x[2])},character(1))
      id<-.bh_public_id("correlation_class",layer,paste(sort(pair,method="radix"),collapse="|"))
    }
    table$parameter[at]<-id
  }
  table
}
.bh_cor_required_zeros <- function(table,requirements) {
  info<-attr(table,"affine_rules");if(is.null(info))return(table)
  for(i in seq_len(nrow(requirements))) {
    edge<-match(paste(requirements$row[i],requirements$col[i]),paste(table$row,table$col))
    if(!is.na(edge))info$pins<-rbind(info$pins,data.frame(edge=edge,value=0,source=paste0("owner_required_zero:",requirements$owner[i])))
  }
  attr(table,"affine_rules")<-info;.bh_resolve_cor_classes(table)
}
.bh_cor_primitive_table <- function(table,layer) {
  free<-which(table$state=="free");ids<-unique(table$parameter[free])
  out<-data.frame(parameter=ids,layer=rep(layer,length(ids)),primitive_index=seq_along(ids),members=integer(length(ids)),stringsAsFactors=FALSE)
  out$edge_indices<-I(lapply(ids,function(id)free[table$parameter[free]==id]))
  out$members<-lengths(out$edge_indices);out
}
.bh_cor_primitive_count <- function(table)length(unique(table$parameter[table$state=="free"]))
.bh_has_affine_correlations <- function(spec) {
  common<-spec$tables$correlations;residual<-spec$tables$parameter_correlations
  if(is.null(common))common<-spec$formula$correlations
  if(is.list(spec$formula$residual)&&.bh_is_affine_cor_table(spec$formula$residual$correlations))return(TRUE)
  .bh_is_affine_cor_table(common)||.bh_is_affine_cor_table(residual)
}
.bh_has_correlation_declarations <- function(spec) {
  formula<-if(inherits(spec,"bhmm_formula"))spec else spec$formula
  .bh_has_cor_extensions(formula$correlations)||(is.list(formula$residual)&&.bh_has_cor_extensions(formula$residual$correlations))
}
.bh_affine_correlation_lowering_gate <- function(spec) {
  if(.bh_has_affine_correlations(spec)) {
    if(!.bh_affine_lowering_available()).mp_abort("E_CORRELATION_LOWERING_UNAVAILABLE","new affine lowering is not available")
    .bh_affine_lowering_plan(spec)
  }
  invisible(TRUE)
}
.bh_cor_constant_matrix <- function(table,n) {
  C<-diag(n)
  fixed<-which(table$state=="fixed")
  for(i in fixed)C[table$row[i],table$col[i]]<-C[table$col[i],table$row[i]]<-table$fixed_value[i]
  C
}
.bh_cor_affine_matrix <- function(table,n,values) {
  ids<-unique(table$parameter[table$state=="free"])
  if(length(values)!=length(ids)||any(!is.finite(values))).mp_abort("E_STRUCTURE_STATE","final correlation values must match unique finite marginal labels")
  R<-.bh_cor_constant_matrix(table,n)
  for(i in which(table$state=="free"))R[table$row[i],table$col[i]]<-R[table$col[i],table$row[i]]<-values[match(table$parameter[i],ids)]
  R
}
.bh_cor_spd_certificate <- function(R) {
  n<-nrow(R)
  if(!n)return(list(status="strictly_positive_definite",method="empty_matrix",margin=Inf,error_bound=0,factor=R))
  if(any(!is.finite(R))||!identical(R,t(R)))return(list(status="unknown",reason="nonfinite or nonsymmetric numerical matrix"))
  gamma<-(n+2)*.Machine$double.eps/(1-(n+2)*.Machine$double.eps)
  eig<-tryCatch(eigen(R,symmetric=TRUE),error=identity)
  if(inherits(eig,"error"))return(list(status="unknown",reason="symmetric eigensolver failed"))
  # Any certified negative quadratic form disproves positive definiteness.
  v<-eig$vectors[,n];q<-as.numeric(crossprod(v,R%*%v))
  absolute<-as.numeric(crossprod(abs(v),abs(R)%*%abs(v)))
  error<-(2*gamma+gamma^2)*absolute/(1-gamma)^2+n*n*.Machine$double.xmin
  if(is.finite(q)&&is.finite(error)&&q+error<0)return(list(status="proven_indefinite",method="negative_quadratic_form",quadratic=q,error_bound=error,witness=v))
  L<-tryCatch(t(chol(R)),error=identity)
  if(inherits(L,"error"))return(list(status="unknown",reason="Cholesky failed without a verified negative direction",eigenvalues=eig$values))
  B<-tryCatch(forwardsolve(L,diag(n)),error=identity)
  if(inherits(B,"error")||any(!is.finite(B))||any(diag(B)<=0))return(list(status="unknown",reason="inverse-factor congruence is not representable"))
  G<-abs(B)%*%abs(R)%*%t(abs(B));A<-B%*%R%*%t(B)
  bound<-(2*gamma+gamma^2)*G/(1-gamma)^2+n*n*.Machine$double.xmin*(1+max(abs(B)))^2
  margin<-diag(A)-rowSums(abs(A-diag(diag(A),nrow=n)))-rowSums(bound)
  if(any(!is.finite(c(G,A,bound,margin)))||any(margin<=0))return(list(status="unknown",reason="the inverse-factor Gershgorin margin does not exceed its arithmetic bound",margin=margin,error_bound=bound,eigenvalues=eig$values))
  list(status="strictly_positive_definite",method="inverse_factor_congruence_Gershgorin_with_roundoff_bound",margin=min(margin),error_bound=max(rowSums(bound)),factor=L,eigenvalues=eig$values)
}
# Upper bounds for the computed congruence use the computed B as an exact
# invertible coordinate matrix. Thus Cholesky/solve error appears in G=B R0 B',
# rather than being incorrectly omitted by assuming G is exactly identity.
.bh_cor_upper_sum <- function(x) {
  if(any(!is.finite(x)))return(Inf)
  gamma<-(length(x)+8)*.Machine$double.eps
  sum(abs(x))/(1-gamma)+length(x)*.Machine$double.xmin
}
.bh_cor_congruence_bound <- function(B,M) {
  n<-nrow(B);gamma<-(n+2)*.Machine$double.eps/(1-(n+2)*.Machine$double.eps)
  value<-B%*%M%*%t(B);absolute<-abs(B)%*%abs(M)%*%t(abs(B))
  error<-(2*gamma+gamma^2)*absolute/(1-gamma)^2+n*n*.Machine$double.xmin*(1+max(abs(B)))^2
  list(value=value,error=error)
}
.bh_cor_extreme_eigen_bound <- function(H) {
  n<-nrow(H);u<-.Machine$double.eps;gamma<-(n+2)*u/(1-(n+2)*u)
  eig<-eigen(H,symmetric=TRUE);V<-eig$vectors;lambda<-eig$values
  Q<-crossprod(V);Qbound<-gamma/(1-gamma)*(t(abs(V))%*%abs(V))
  orthogonality<-.bh_cor_upper_sum(abs(Q-diag(n))+Qbound+u/(1-u)*(abs(Q)+diag(n)))
  Y<-H%*%V;D<-sweep(V,2,lambda,`*`);residual<-Y-D
  boundY<-gamma/(1-gamma)*(abs(H)%*%abs(V))
  boundD<-u/(1-u)*abs(D)+.Machine$double.xmin
  boundDifference<-u/(1-u)*(abs(Y)+abs(D))+.Machine$double.xmin
  normResidual<-.bh_cor_upper_sum(abs(residual)+boundY+boundD+boundDifference)
  error<-if(orthogonality>=.5)Inf else normResidual*sqrt(1+orthogonality)/(1-orthogonality)*(1+8*u)
  list(values=lambda,error=error,orthogonality_bound=orthogonality,residual_bound=normResidual)
}
.bh_cor_domain <- function(table,n) {
  C<-.bh_cor_constant_matrix(table,n);d<-.bh_cor_primitive_count(table)
  out<-list(dimension=d,status="unknown",constant=C,coordinate_measure="Lebesgue measure on unique final marginal correlation labels",
    method="not_assessed",reason="no certified strict interior point",
    affine_chart_scope=if(d==0L)"fully_fixed"else if(d==1L)"one_unique_label"else"multilabel_inspection_only",
    runtime_scope=if(.bh_is_affine_cor_table(table))"affine_layer_domain_only; inspect check_model()$correlation_lowering and $backend for current whole-model capabilities"else"existing_zero_free_MGS_route")
  fixed<-which(table$state=="fixed")
  if(length(fixed)&&any(abs(table$fixed_value[fixed])==1)) {
    out$status<-"proven_infeasible_strict_PD";out$method<-"fixed_boundary_pair_minor";out$reason<-"a fixed +/-1 pair has zero 2x2 determinant; a possibly singular PSD scientific model is outside this strictly-PD capability"
    return(out)
  }
  if(n>100L) {
    if(identical(C,diag(n))){out$status<-"feasible";out$interior<-rep(0,d);out$method<-"exact_identity_interior";if(!d)out$factor<-C}
    out$interval_status<-"not_assessed";out$reason<-"numerical correlation-domain assessment is currently limited to 100 coordinates; the declaration and any exact identity witness remain intact";return(out)
  }
  if(d>1L&&.bh_is_affine_cor_table(table)) {
    product<-.bh_cor_product_domain(table,n)
    if(!is.null(product))return(product)
  }
  evaluate<-function(theta).bh_cor_affine_matrix(table,n,theta)
  theta<-rep(0,d);certificate<-.bh_cor_spd_certificate(C)
  if(d==0L) {
    out$certificate<-certificate;if(!is.null(certificate$method))out$method<-certificate$method
    if(certificate$status=="strictly_positive_definite") {out$status<-"feasible";out$reason<-"the complete fixed matrix has a verified strict-PD factor";out$factor<-certificate$factor}
    else if(certificate$status=="proven_indefinite"){out$status<-"proven_infeasible_strict_PD";out$reason<-"the fixed matrix has a verified negative quadratic form"}
    else out$reason<-certificate$reason
    out$interior<-numeric();return(out)
  }
  if(certificate$status!="strictly_positive_definite"&&d==1L) {
    candidate<-tryCatch(stats::optimize(function(x)-min(eigen(evaluate(x),symmetric=TRUE,only.values=TRUE)$values),c(-1,1),tol=1e-10),error=identity)
    if(!inherits(candidate,"error")) {theta<-candidate$minimum;certificate<-.bh_cor_spd_certificate(evaluate(theta));out$interior_search<-list(method="concave_minimum_eigenvalue_search_candidate_only",candidate=theta)}
  }
  if(certificate$status!="strictly_positive_definite") {out$certificate<-certificate;out$reason<-"no numerically verified strict interior was found; optimization failure is not a proof of infeasibility";return(out)}
  out$status<-"feasible";out$interior<-theta;out$certificate<-certificate;out$method<-"verified_strict_interior";out$reason<-"a strict interior point is verified; the affine domain is open, convex and bounded"
  if(d!=1L)return(out)
  E<-evaluate(1)-C;L<-certificate$factor;B<-forwardsolve(L,diag(n))
  base_congruence<-.bh_cor_congruence_bound(B,evaluate(theta))
  direction_congruence<-.bh_cor_congruence_bound(B,E)
  G<-base_congruence$value;Hraw<-direction_congruence$value;H<-(Hraw+t(Hraw))/2
  if(any(!is.finite(c(G,H,base_congruence$error,direction_congruence$error)))){out$interval_status<-"not_assessed";out$interval_reason<-"affine congruence or its arithmetic bound is not representable";return(out)}
  u<-.Machine$double.eps
  base_error<-.bh_cor_upper_sum(abs(G-diag(n))+base_congruence$error+u/(1-u)*(abs(G)+diag(n)))
  direction_error<-.bh_cor_upper_sum(abs(Hraw-H)+direction_congruence$error+u/(1-u)*(abs(Hraw)+abs(H)))
  spectral<-.bh_cor_extreme_eigen_bound(H);lambda<-spectral$values
  positive<-lambda[lambda>0];negative<-lambda[lambda<0]
  uncertainty<-direction_error+spectral$error
  if(!length(positive)||!length(negative)||!is.finite(uncertainty)||base_error>=1||max(positive)<=uncertainty||-min(negative)<=uncertainty){out$interval_status<-"not_assessed";out$interval_reason<-"generalized eigenvalue signs or congruence error could not be resolved";return(out)}
  lp<-max(positive);lm<-min(negative)
  lp_bounds<-c((lp-uncertainty)/(1+base_error),(lp+uncertainty)/(1-base_error))
  lm_bounds<-c((lm-uncertainty)/(1-base_error),(lm+uncertainty)/(1+base_error))
  a<-theta-1/lp;b<-theta-1/lm
  a_bounds<-theta-1/lp_bounds;b_bounds<-theta-1/lm_bounds
  endpoint_error<-max(abs(c(a_bounds-a,b_bounds-b)))+64*u*(1+abs(theta)+max(abs(c(1/lp_bounds,1/lm_bounds))))
  # The (-1,1) intersection is an exact 2x2-principal-minor requirement, not a
  # clipping of scientific values or a conservative inner support interval.
  lower<-max(-1,a);upper<-min(1,b);width<-upper-lower
  out$whitening_error_bound<-base_error;out$direction_error_bound<-direction_error
  out$eigensystem_error_bound<-spectral$error;out$endpoint_enclosures<-list(lower=a_bounds,upper=b_bounds)
  if(any(!is.finite(c(lower,upper,width,endpoint_error)))||width<=0||theta<=lower||theta>=upper||endpoint_error>1e-7*width) {
    out$interval_status<-"not_assessed";out$interval_reason<-"interval endpoints/width lack the required numerical resolution";out$endpoint_error_bound<-endpoint_error;return(out)
  }
  out$interval_status<-"available";out$lower<-lower;out$upper<-upper;out$midpoint<-lower+width/2;out$half_width<-width/2
  out$interval_method<-"generalized symmetric eigenvalue bounds intersected with exact pairwise correlation bounds"
  out$endpoint_error_bound<-endpoint_error;out$direction_eigenvalues<-lambda;out$direction<-E
  out
}
.bh_cor_interval_chart <- function(z,domain) {
  if(length(z)!=1L||!is.finite(z)||!identical(domain$interval_status,"available")) .mp_abort("E_CORRELATION_NUMERIC","one finite chart coordinate and a resolved interval are required")
  t<-tanh(z);theta<-domain$midpoint+domain$half_width*t
  if(abs(t)>=1||!is.finite(theta)||theta<=domain$lower||theta>=domain$upper) .mp_abort("E_CORRELATION_NUMERIC","correlation interval transform saturated or left its strict interval; no clipping")
  actual<-.bh_cor_spd_certificate(domain$constant+theta*domain$direction)
  if(!identical(actual$status,"strictly_positive_definite")) .mp_abort("E_CORRELATION_NUMERIC","the transformed marginal matrix cannot be certified strictly positive definite at this precision; the scientific interval is not clipped")
  log_jacobian<-log(domain$half_width)+2*(log(2)-abs(z)-log1p(exp(-2*abs(z))))
  if(!is.finite(log_jacobian)).mp_abort("E_CORRELATION_NUMERIC","correlation interval Jacobian is not representable")
  list(theta=theta,log_jacobian=log_jacobian,coordinate="one unique final marginal label")
}
.bh_cor_interval_inverse <- function(theta,domain) {
  if(length(theta)!=1L||!is.finite(theta)||!identical(domain$interval_status,"available")||theta<=domain$lower||theta>=domain$upper) .mp_abort("E_CORRELATION_NUMERIC","the requested correlation is outside the resolved strict interval")
  actual<-.bh_cor_spd_certificate(domain$constant+theta*domain$direction)
  if(!identical(actual$status,"strictly_positive_definite")) .mp_abort("E_CORRELATION_NUMERIC","the requested marginal matrix is not numerically certified strictly positive definite")
  ratio<-(theta-domain$midpoint)/domain$half_width
  if(!is.finite(ratio)||abs(ratio)>=1).mp_abort("E_CORRELATION_NUMERIC","inverse interval coordinate is not representable")
  atanh(ratio)
}
.bh_gf2_reduction <- function(A,n=ncol(A)) {
  A<-matrix(as.integer(A%%2),nrow=nrow(A),ncol=n);row<-1L;pivots<-integer()
  for(col in seq_len(n)) {
    if(row>nrow(A))break
    possible<-which(A[seq.int(row,nrow(A)),col]==1L)
    if(!length(possible))next
    at<-row+possible[1L]-1L
    if(at!=row){temporary<-A[row,];A[row,]<-A[at,];A[at,]<-temporary}
    for(i in setdiff(which(A[,col]==1L),row))A[i,]<-(A[i,]+A[row,])%%2
    pivots<-c(pivots,col);row<-row+1L
  }
  free<-setdiff(seq_len(n),pivots);basis<-matrix(0L,n,length(free))
  for(j in seq_along(free)) {
    basis[free[j],j]<-1L
    if(length(pivots))basis[pivots,j]<-A[seq_along(pivots),free[j]]
  }
  list(rank=length(pivots),rref=A,pivots=pivots,basis=basis)
}
.bh_apply_affine_orientation <- function(map,formula) {
  cor<-formula$correlations
  if(!.bh_is_affine_cor_table(cor))return(map)
  K<-nrow(formula$coordinates);equations<-list();sources<-character()
  add<-function(indices,source) {
    row<-tabulate(indices,nbins=K)%%2
    equations[[length(equations)+1L]]<<-row;sources<<-c(sources,source)
  }
  ordinary<-map$ordinary_loading_map;free<-map$free_loadings
  pinned<-which(ordinary$state=="fixed"&ordinary$fixed_value!=0)
  for(i in pinned)add(ordinary$component_index[i],paste0("fixed_loading:",ordinary$cell_id[i]))
  derived<-map$loading_map[!map$loading_map$role%in%c("loading","fixed_loading")&map$loading_map$multiplier!=0,,drop=FALSE]
  for(i in seq_len(nrow(derived)))add(derived$coordinate_index[i],paste0("derived_loading:",derived$coordinate_id[i],":",derived$target[i]))
  for(id in free$parameter) {
    coordinates<-unique(ordinary$component_index[ordinary$state=="free"&ordinary$parameter==id])
    if(length(coordinates)>1L)for(k in coordinates[-1L])add(c(coordinates[1L],k),paste0("loading_alias:",id))
  }
  for(i in which(cor$state=="fixed"))add(c(cor$row[i],cor$col[i]),paste0("fixed_nonzero_correlation:",cor$component1[i],":",cor$component2[i]))
  for(id in unique(cor$parameter[cor$state=="free"])) {
    edges<-which(cor$state=="free"&cor$parameter==id)
    if(length(edges)>1L)for(i in edges[-1L])add(c(cor$row[edges[1L]],cor$col[edges[1L]],cor$row[i],cor$col[i]),paste0("correlation_alias:",id))
  }
  A<-if(length(equations))do.call(rbind,equations)else matrix(integer(),0L,K)
  reduction<-.bh_gf2_reduction(A,K);basis<-reduction$basis;D<-ncol(basis)
  characters<-matrix(0L,nrow(free),D)
  for(i in seq_len(nrow(free))) {
    members<-unique(ordinary$component_index[ordinary$state=="free"&ordinary$parameter==free$parameter[i]])
    if(length(members))characters[i,]<-basis[members[1L],]
    if(length(members)>1L&&any(vapply(members,function(k)!identical(as.integer(basis[k,]),as.integer(characters[i,])),logical(1))))
      .mp_abort("E_ORIENTATION_INTERNAL","loading alias members have inconsistent GF(2) characters")
  }
  anchors<-integer();anchor_matrix<-matrix(integer(),0L,D);rank<-0L
  for(i in seq_len(nrow(free))) {
    candidate<-rbind(anchor_matrix,characters[i,,drop=FALSE]);new_rank<-.bh_gf2_reduction(candidate,D)$rank
    if(new_rank>rank){anchors<-c(anchors,i);anchor_matrix<-candidate;rank<-new_rank}
  }
  free$sign<-rep("free",nrow(free));free$sign[anchors]<-"positive_anchor"
  free$support<-rep("real",nrow(free));free$support[anchors]<-"positive"
  free$orientation_reason<-rep("signed coefficient under the resolved GF(2) constraint system",nrow(free));free$orientation_reason[anchors]<-"one independent GF(2) loading sign character"
  free$sign_character<-I(lapply(seq_len(nrow(free)),function(i)as.integer(characters[i,])))
  orientation<-map$orientation;orientation$sign_group<-rep(NA_integer_,K)
  orientation$pinned<-if(D)rowSums(basis)==0L else rep(TRUE,K)
  orientation$allowed_simultaneous_flip<-!orientation$pinned
  orientation$anchor<-rep(NA_character_,K)
  for(k in seq_len(K))for(i in anchors)if(identical(as.integer(basis[k,]),as.integer(characters[i,])))orientation$anchor[k]<-free$parameter[i]
  orientation$sign_character<-I(lapply(seq_len(K),function(k)as.integer(basis[k,])))
  orientation$representation<-rep("GF2_binary_sign_basis",K)
  orientation$pin_witness<-ifelse(orientation$pinned,"zero character in the full fixed/derived/loading-alias/correlation constraint null space","")
  residual_basis<-.bh_gf2_reduction(anchor_matrix,D)$basis
  unresolved<-(basis%*%residual_basis)%%2
  rownames(basis)<-formula$coordinates$display;colnames(basis)<-if(D)paste0("gauge_",seq_len(D))else character()
  label_ids<-unique(cor$parameter[cor$state=="free"])
  label_characters<-lapply(label_ids,function(id){i<-which(cor$state=="free"&cor$parameter==id)[1L];as.integer((basis[cor$row[i],]+basis[cor$col[i],])%%2)})
  names(label_characters)<-label_ids
  map$orientation_sign_system<-list(equations=A,equation_sources=sources,rref=reduction$rref,
    allowed_binary_basis=basis,anchor_parameters=free$parameter[anchors],anchor_characters=anchor_matrix,
    correlation_label_characters=label_characters,unanchored_binary_basis=unresolved,
    scope="Exact GF(2) symmetry of the resolved loading/fixed/equal marginal correlation map under the unchanged symmetric prior policy; no independent residual sign folding")
  map$orientation<-orientation;map$free_loadings<-map$loading_primitives<-free
  for(name in c("ordinary_loading_map","loading_cells","loadings")) {
    x<-map[[name]];at<-match(x$parameter,free$parameter);take<-which(!is.na(at))
    x$sign[take]<-free$sign[at[take]];map[[name]]<-x
  }
  at<-match(map$loading_coefficients$parameter,free$parameter);take<-which(!is.na(at))
  map$loading_coefficients$support[take]<-free$support[at[take]]
  map
}
.bh_attach_correlation_tables <- function(spec) {
  spec$tables$parameter_correlations<-.bh_cor_rekey(spec$tables$parameter_correlations,"parameter")
  layers<-list(component=spec$tables$correlations,parameter=spec$tables$parameter_correlations)
  sizes<-c(component=nrow(spec$tables$components),parameter=nrow(spec$tables$parameter_residuals))
  primitives<-cells<-rules<-list();domains<-constants<-list()
  for(layer in names(layers)) {
    tab<-layers[[layer]];raw<-layer=="component"&&.bh_raw_cor(spec)
    p<-.bh_cor_primitive_table(tab,layer);if(raw)p<-p[FALSE,,drop=FALSE];primitives[[layer]]<-p
    domain<-.bh_cor_domain(tab,sizes[[layer]]);domain$parameter<-p$parameter
    domain$primitive_role<-if(raw)"derived final marginals; actual raw-B primitives are in raw_correlation_coordinates"else"unique final marginal correlation labels"
    if(raw)domain$runtime_scope<-"separate_ordered_raw_B_policy; not a final-marginal primitive chart"
    domain$coordinates<-if(layer=="component"&&.bh_raw_cor(spec))"ordered_raw_B_separate_law"else if(.bh_is_affine_cor_table(tab))"unique_final_marginal_labels"else"existing_zero_free_marginal_entries"
    domains[[layer]]<-domain;constants[[layer]]<-domain$constant
    endpoint1<-if(layer=="component")tab$component1 else tab$target1
    endpoint2<-if(layer=="component")tab$component2 else tab$target2
    fixed_value<-rep(NA_real_,nrow(tab));fixed_value[tab$state=="zero"]<-0
    if("fixed_value"%in%names(tab))fixed_value[tab$state=="fixed"]<-tab$fixed_value[tab$state=="fixed"]
    cells[[layer]]<-data.frame(layer=rep(layer,nrow(tab)),row=tab$row,col=tab$col,endpoint1=endpoint1,endpoint2=endpoint2,
      state=tab$state,fixed_value=fixed_value,parameter=tab$parameter,primitive_index=match(tab$parameter,p$parameter),source=tab$source,stringsAsFactors=FALSE)
    if(raw)cells[[layer]]$parameter<-rep(NA_character_,nrow(tab))
    info<-attr(tab,"affine_rules")
    if(!is.null(info)&&nrow(info$rules)) {
      x<-info$rules;x$layer<-layer;x$endpoint1<-endpoint1[x$edge];x$endpoint2<-endpoint2[x$edge]
      x$resolved_parameter<-tab$parameter[x$edge];x$resolved_state<-tab$state[x$edge];x$resolved_fixed_value<-fixed_value[x$edge]
      rules[[layer]]<-x
    }
  }
  spec$tables$correlation_primitives<-do.call(rbind,primitives)
  spec$tables$correlation_affine_cells<-do.call(rbind,cells)
  spec$tables$correlation_affine_constants<-constants
  spec$tables$correlation_domains<-domains
  if(.bh_is_affine_cor_table(layers$component)) {
    spec$tables$correlation_coordinate_system$type<-"affine_final_marginal_labels"
    spec$tables$correlation_coordinate_system$prior_measure<-"restricted determinant density on unique final marginal labels"
    spec$tables$correlation_coordinate_system$applied_prior_coordinate_jacobian<-"R/IR scientific rho coordinates; no backend transform is lowered"
  }
  if(length(rules))spec$tables$correlation_constraint_rules<-do.call(rbind,rules)else {
    empty<-data.frame(rule=character(),kind=character(),label=character(),value=numeric(),edge=integer())
    empty$captured_selector<-I(list());empty$layer<-empty$endpoint1<-empty$endpoint2<-empty$resolved_parameter<-empty$resolved_state<-character()
    empty$resolved_fixed_value<-numeric();spec$tables$correlation_constraint_rules<-empty
  }
  spec
}
.bh_correlation_prior_annotations <- function(spec) {
  if(!.bh_has_affine_correlations(spec))return(spec)
  p<-spec$tables$priors
  p$coordinate_measure<-p$constraint_domain<-p$normalization_scope<-rep(NA_character_,nrow(p))
  for(i in which(p$class=="cor")) {
    p$coordinate_measure[i]<-"Lebesgue measure on unique final marginal correlation labels; one joint density per layer"
    p$constraint_domain[i]<-paste0("R(theta) strictly positive definite; inspect correlation_domains$",p$layer[i])
    p$normalization_scope[i]<-"restricted determinant normalizer depends on fixed constraints and fixed eta; omitted only from posterior kernel"
  }
  spec$tables$priors<-p;spec
}
.bh_correlation_prior_geometry <- function(spec) {
  out<-list()
  for(layer in c("component","parameter")) {
    tab<-if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations
    domain<-.bh_correlation_domains(spec)[[layer]]
    if(layer=="component"&&.bh_raw_cor(spec)) {out[[layer]]<-list(status="separate_raw_B_law",measure="independent declared raw-B Normal coordinates");next}
    d<-.bh_cor_primitive_count(tab)
    if(!d){out[[layer]]<-list(status="not_applicable_no_free_correlations",measure="fixed correlation matrix, no prior coordinate");next}
    row<-spec$tables$priors[spec$tables$priors$class=="cor"&spec$tables$priors$layer==layer,,drop=FALSE]
    eta<-if(nrow(row)==1L&&!is.na(row$prior)).bh_prior_distribution(row$prior)$values[1L]else NA_real_
    result<-list(status="unassessed",eta=eta,unique_parameters=d,measure="Lebesgue measure on unique final marginal labels",
      kernel="det(R(theta))^(eta-1), once for the complete layer",normalization="constraint and eta dependent constant omitted only because they are fixed; not an unconstrained LKJ normalization",posterior_propriety="separate assessment")
    if(!identical(domain$status,"feasible"))result$reason<-"strictly feasible prior support has not been verified"
    else if(is.finite(eta)&&eta>=1) {result$status<-"proven_proper_prior";result$reason<-"a bounded positive determinant kernel on a bounded domain with a strict interior has finite positive normalization"}
    else if(!.bh_is_affine_cor_table(tab)&&is.finite(eta)) {result$status<-"existing_zero_free_prior_scope";result$reason<-"unchanged zero/free marginal-coordinate prior contract; eta<1 execution remains unqualified"}
    else if(d==1L&&is.finite(eta)&&!any(tab$state=="fixed")) {
      # Exact graph certificate for a union of complete blocks sharing rho.
      n<-nrow(domain$constant);adj<-matrix(FALSE,n,n)
      for(i in which(tab$state=="free"))adj[tab$row[i],tab$col[i]]<-adj[tab$col[i],tab$row[i]]<-TRUE
      seen<-rep(FALSE,n);sizes<-integer();complete<-TRUE
      for(start in seq_len(n))if(!seen[start]) {
        group<-start;at<-1L;seen[start]<-TRUE
        while(at<=length(group)){next_nodes<-which(adj[group[at],]&!seen);seen[next_nodes]<-TRUE;group<-c(group,next_nodes);at<-at+1L}
        if(length(group)>1L){sizes<-c(sizes,length(group));if(sum(adj[group,group,drop=FALSE])!=length(group)*(length(group)-1L))complete<-FALSE}
      }
      if(complete&&length(sizes)) {
        multiplicity<-sum(sizes-1L);threshold<-1-1/multiplicity
        result$status<-if(eta>threshold)"proven_proper_prior"else"proven_improper_prior"
        result$reason<-"exact determinant factorization of complete blocks sharing one rho; the rho=1 boundary gives the strongest integrability threshold"
        result$proper_iff_eta_greater_than<-threshold;result$complete_block_sizes<-sizes
      } else result$reason<-"eta<1 normalizability is not established for this affine geometry"
    } else result$reason<-"eta<1 normalizability is not established for this affine geometry"
    out[[layer]]<-result
  }
  out
}
.bh_correlation_domains <- function(spec) {
  if(!is.null(spec$tables$correlation_domains))return(spec$tables$correlation_domains)
  list(component=.bh_cor_domain(spec$tables$correlations,nrow(spec$tables$components)),
    parameter=.bh_cor_domain(spec$tables$parameter_correlations,nrow(spec$tables$parameter_residuals)))
}
.bh_correlation_feasibility <- function(spec) {
  domains<-.bh_correlation_domains(spec);status<-vapply(domains,`[[`,character(1),"status")
  list(status=if(any(status=="proven_infeasible_strict_PD"))"proven_infeasible_strict_PD"else if(all(status=="feasible"))"feasible"else"unknown",
    layers=domains,reason="strict-PD affine support is distinct from numerical chart availability, prior normalizability, likelihood identification and backend capability")
}
.bh_affine_probes_available <- function(spec) {
  domains<-.bh_correlation_domains(spec)
  for(layer in c("component","parameter")) {
    tab<-if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations
    if(!.bh_is_affine_cor_table(tab))next
    domain<-domains[[layer]]
    if(!identical(domain$status,"feasible")||domain$dimension>1L||domain$dimension==1L&&!identical(domain$interval_status,"available"))return(FALSE)
  }
  TRUE
}
.bh_affine_probe_scales <- function(spec,scale,layout) {
  domains<-.bh_correlation_domains(spec)
  for(layer in c("component","parameter")) {
    tab<-if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations
    if(.bh_is_affine_cor_table(tab)&&identical(domains[[layer]]$interval_status,"available"))scale[layout$indices[[paste0(layer,"_cor")]]]<-domains[[layer]]$half_width
  }
  scale
}
.bh_affine_probe_values <- function(spec,values,layout,probe) {
  domains<-.bh_correlation_domains(spec)
  for(layer in c("component","parameter")) {
    tab<-if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations
    if(!.bh_is_affine_cor_table(tab)||!length(layout$indices[[paste0(layer,"_cor")]]))next
    domain<-domains[[layer]]
    if(!identical(domain$interval_status,"available")) .mp_abort("E_CORRELATION_DIAGNOSTIC","an unresolved/multilabel affine domain has no qualified default numerical rank probe")
    values[layout$indices[[paste0(layer,"_cor")]]]<-domain$lower+(domain$upper-domain$lower)*c(.2,.5,.8)[probe]
  }
  unname(values)
}
.bh_correlation_uninformed_edges <- function(table,uninformed) {
  free<-which(table$state=="free");ids<-unique(table$parameter[free])
  unlist(lapply(ids,function(id){members<-free[table$parameter[free]==id];if(all(members%in%uninformed))members[1L] else integer()}),use.names=FALSE)
}
.bh_affine_correlation_alias <- function(spec) {
  cor<-spec$tables$correlations;coordinates<-spec$tables$coordinates
  if(nrow(coordinates)!=2L||nrow(cor)!=1L||cor$state!="fixed"||cor$fixed_value==0||.bh_has_shares(spec))return(NULL)
  cells<-spec$tables$ordinary_loading_map;sd<-.bh_free_component_sd(spec);amplitude<-.bh_amplitudes(spec)
  if(nrow(sd)==2L&&nrow(cells)==4L&&!nrow(amplitude)&&all(coordinates$kind=="scalar")) {
    fixed<-cells[cells$state=="fixed",,drop=FALSE];free<-cells[cells$state=="free",,drop=FALSE]
    if(nrow(fixed)==2L&&nrow(free)==2L&&length(unique(cells$target))==2L&&length(unique(fixed$target))==2L&&
       length(unique(fixed$component_index))==2L&&all(fixed$fixed_value!=0)&&all(fixed$cell_id%in%sd$marker_cell)&&
       length(unique(free$parameter))==1L&&length(unique(free$target))==2L&&length(unique(free$component_index))==2L) {
      c<-fixed$fixed_value[order(fixed$component_index)]
      return(list(reason="two marked crossed-loading columns retain a distinct covariance alias even with a known nonzero correlation",
        proof=if(sign(c[1])==sign(c[2]))"same-sign markers: b_new=c1*c2/b, s1_new=abs(b/c1)*s2, s2_new=abs(b/c2)*s1; fixed rho is unchanged"else
          "opposite-sign markers: w=b/(sign(c1)*sqrt(abs(c1*c2))), u_i=sqrt(abs(c_i))*s_i, d=(u1^2-u2^2)/2, o=rho*u1*u2. Set w_new=(w*o+d)/(o-w*d), u_new=sqrt((1+w^2)/(1+w_new^2))*reverse(u), then recover b and positive SDs. On an open interior set this is a distinct state with unchanged covariance",
        parameters=c(unique(free$parameter),sd$parameter),fixed_rho=cor$fixed_value,
        scope="exactly two ordinary marked coordinates, two fixed nonzero diagonal markers, one shared free offdiagonal beta and no other loading/correlation constraints; means and independent residual layer unchanged"))
    }
  }
  if(nrow(sd)||nrow(amplitude)!=1L||sum(coordinates$kind=="basis")!=1L||sum(coordinates$kind=="scalar")!=1L)return(NULL)
  p<-which(coordinates$kind=="basis");g<-which(coordinates$kind=="scalar")
  if(sum(coordinates$group==amplitude$component)!=1L)return(NULL)
  q<-spec$tables$basis_multiplier[,p]
  gcells<-cells[cells$component_index==g,,drop=FALSE]
  if(any(gcells$state!="free")||anyDuplicated(gcells$parameter)||!all(which(q!=0)%in%gcells$target_index))return(NULL)
  list(reason="an unmarked free loading column and positive one-column basis amplitude retain a reflection alias at a fixed nonzero correlation",
    proof="lambda_new=-lambda-2*rho*h*q and h_new=h preserve (lambda+rho*h*q)(lambda+rho*h*q)' + h^2*(1-rho^2)*q*q' exactly",
    parameters=c(gcells$parameter,amplitude$parameter),fixed_rho=cor$fixed_value,
    scope="isolated two-coordinate unit-SD ordinary/basis hierarchy; every nonzero basis row has a singleton unconstrained ordinary loading, no fixed/aliased/owned ordinary cells or external common effects; residual layer unchanged")
}
