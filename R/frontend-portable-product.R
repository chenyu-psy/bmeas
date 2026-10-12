# Saved Cartesian-product correlation records. These checks only bind retained
# plain values and their algebraic identities. They do not solve a domain,
# recertify positive definiteness, or promote retained numerical evidence.
.bh_pp_vec <- function(x,path,type="n",size=NULL,names=NULL) {
  if(isS4(x)||any(!base::names(attributes(x))%in%if(is.null(names))character()else"names"))
    .bh_pf_fail(path,"product operands must be plain saved vectors")
  .bh_ps_vec(x,path,type,size)
  if(!identical(attr(x,"names",exact=TRUE),names))
    .bh_pf_fail(path,"product operand names differ from their retained label order")
  invisible(TRUE)
}
.bh_pp_matrix <- function(x,path,n=NULL,unit=FALSE,direction=FALSE) {
  d<-attr(x,"dim",exact=TRUE)
  if(isS4(x)||!typeof(x)%in%c("integer","double")||
     any(!names(attributes(x))%in%"dim")||!.bh_pf_plain(d,"integer")||
     length(d)!=2L||anyNA(d)||d[1L]<1L||d[1L]>100L||d[1L]!=d[2L]||
     length(x)!=as.double(d[1L])*d[2L]||(!is.null(n)&&d[1L]!=n))
    .bh_pf_fail(path,"product matrices need bounded plain square numeric storage")
  .bh_ps_matrix(x,path,square=TRUE)
  if(unit||direction) {
    if(any(x!=base::t.default(x))) .bh_pf_fail(path,"stored product matrix is not symmetric")
    diagonal<-1L+(seq_len(d[1L])-1L)*(d[1L]+1L)
    if(unit&&(any(x[diagonal]!=1)||any(abs(x)>1)))
      .bh_pf_fail(path,"product constant must have unit diagonal and correlation-valued entries")
    if(direction&&(any(x[diagonal]!=0)||any(!x%in%c(0,1))||!any(x==1)))
      .bh_pf_fail(path,"one-label direction must be a nonempty symmetric zero-diagonal binary matrix")
  }
  invisible(d[1L])
}
.bh_pp_close <- function(x,y,path,message,multiplier=128) {
  if(length(x)!=length(y)||any(!is.finite(x))||any(!is.finite(y))||
     any(abs(x-y)>multiplier*.Machine$double.eps*pmax(1,abs(x),abs(y))))
    .bh_pf_fail(path,message)
  invisible(TRUE)
}
.bh_pp_same_matrix <- function(x,y,path,message) {
  if(!identical(.bh_ps_dim(x),.bh_ps_dim(y))||any(x!=y)) .bh_pf_fail(path,message)
  invisible(TRUE)
}
.bh_pp_groups <- function(support) {
  # At most 100 vertices. Only exact nonzero saved support is examined.
  n<-.bh_ps_dim(support)[1L];parent<-seq_len(n)
  root<-function(i) {while(parent[i]!=i)i<-parent[i];i}
  for(col in seq_len(n))for(row in seq_len(col-1L))if(support[row,col]!=0) {
    a<-root(row);b<-root(col);parent[max(a,b)]<-min(a,b)
  }
  roots<-integer(n)
  for(i in seq_len(n))roots[i]<-root(i)
  ids<-sort(base::unique.default(roots),method="radix")
  out<-vector("list",length(ids));names(out)<-as.character(ids)
  for(i in seq_along(ids))out[[i]]<-which(roots==ids[i])
  out
}
.bh_pp_same_groups <- function(x,y,path) {
  if(length(x)!=length(y)||!identical(names(x),names(y)))
    .bh_pf_fail(path,"stored graph partition differs from the complete nonzero-support graph")
  for(i in seq_along(x))if(length(x[[i]])!=length(y[[i]])||any(x[[i]]!=y[[i]]))
    .bh_pf_fail(path,"stored graph component membership differs from the complete nonzero-support graph")
  invisible(TRUE)
}
.bh_pp_certificate <- function(x,R,path) {
  n<-.bh_ps_dim(R)[1L]
  z<-.bh_pf_record(x,path,c("status","method","margin","error_bound","factor","eigenvalues"))
  .bh_pf_string(z$status,paste0(path,".status"),"strictly_positive_definite")
  .bh_pf_string(z$method,paste0(path,".method"),"inverse_factor_congruence_Gershgorin_with_roundoff_bound")
  .bh_pp_vec(z$margin,paste0(path,".margin"),size=1L)
  .bh_pp_vec(z$error_bound,paste0(path,".error_bound"),size=1L)
  .bh_pp_vec(z$eigenvalues,paste0(path,".eigenvalues"),size=n)
  .bh_pp_matrix(z$factor,paste0(path,".factor"),n)
  .bh_ps_certificate(z,path,n)
  L<-z$factor;diagonal<-1L+(seq_len(n)-1L)*(n+1L)
  if(z$margin<=0||z$error_bound<0||any(L[diagonal]<=0)||
     any(L[upper.tri(L)]!=0)||any(diff(z$eigenvalues)>0))
    .bh_pf_fail(path,"saved positive-definite evidence has inconsistent signs or factor form")
  # Factor and eigenvalue moments are redundant saved-value identities, not a
  # fresh eigenanalysis, Cholesky factorization, or numerical qualification.
  .bh_pp_close(L%*%base::t.default(L),R,path,"saved factor does not correspond to the retained interior matrix",256*n)
  .bh_pp_close(sum(z$eigenvalues),sum(R[diagonal]),path,"saved eigenvalue trace differs from the interior matrix",256*n)
  .bh_pp_close(sum(z$eigenvalues^2),sum(R^2),path,"saved eigenvalue square sum differs from the interior matrix",256*n)
  invisible(TRUE)
}
.bh_pp_interval <- function(x,path) {
  fields<-c("dimension","status","constant","coordinate_measure","method","reason","affine_chart_scope","runtime_scope",
    "interior","certificate","whitening_error_bound","direction_error_bound","eigensystem_error_bound","endpoint_enclosures",
    "interval_status","lower","upper","midpoint","half_width","interval_method","endpoint_error_bound","direction_eigenvalues","direction","indices")
  z<-.bh_pf_record(x,path,c(fields,"interior_search"),fields)
  scalar<-z;scalar$indices<-NULL
  .bh_ps_correlation_domain(scalar,path)
  .bh_pf_number(z$dimension,paste0(path,".dimension"),1,1,integer=TRUE)
  for(k in c("status","method","reason","coordinate_measure","affine_chart_scope","runtime_scope","interval_status","interval_method"))
    .bh_pf_string(z[[k]],paste0(path,".",k))
  if(!identical(z$status,"feasible")||!identical(z$interval_status,"available")||
     !identical(z$method,"verified_strict_interior")||!identical(z$affine_chart_scope,"one_unique_label")||
     !identical(z$coordinate_measure,"Lebesgue measure on unique final marginal correlation labels")||
     !identical(z$interval_method,"generalized symmetric eigenvalue bounds intersected with exact pairwise correlation bounds"))
    .bh_pf_fail(path,"product factor is not a retained available one-label interval")
  n<-.bh_pp_matrix(z$constant,paste0(path,".constant"),unit=TRUE)
  .bh_pp_matrix(z$direction,paste0(path,".direction"),n,direction=TRUE)
  if(any(z$constant[z$direction==1]!=0)) .bh_pf_fail(path,"free marginal entries cannot have a nonzero affine constant")
  numbers<-c("interior","lower","upper","midpoint","half_width","whitening_error_bound","direction_error_bound","eigensystem_error_bound","endpoint_error_bound")
  for(k in numbers).bh_pp_vec(z[[k]],paste0(path,".",k),size=1L)
  .bh_pp_vec(z$direction_eigenvalues,paste0(path,".direction_eigenvalues"),size=n)
  if("interior_search"%in%names(z)) {
    search<-.bh_pf_record(z$interior_search,paste0(path,".interior_search"),c("method","candidate"))
    .bh_pf_string(search$method,paste0(path,".interior_search.method"),"concave_minimum_eigenvalue_search_candidate_only")
    .bh_pp_vec(search$candidate,paste0(path,".interior_search.candidate"),size=1L)
    if(search$candidate!=z$interior) .bh_pf_fail(path,"saved interior-search candidate differs from the retained interior")
  }else if(z$interior!=0) .bh_pf_fail(path,"nonzero scalar interior lacks its retained search record")
  bounds<-.bh_pf_record(z$endpoint_enclosures,paste0(path,".endpoint_enclosures"),c("lower","upper"))
  for(k in names(bounds)).bh_pp_vec(bounds[[k]],paste0(path,".endpoint_enclosures.",k),size=2L)
  w<-z$upper-z$lower;theta<-z$interior;lambda<-z$direction_eigenvalues
  e0<-z$whitening_error_bound;ed<-z$direction_error_bound;es<-z$eigensystem_error_bound;ee<-z$endpoint_error_bound
  if(z$lower< -1||z$upper>1||w<=0||z$half_width<=0||theta<=z$lower||theta>=z$upper||
     z$midpoint<=z$lower||z$midpoint>=z$upper||e0<0||e0>=1||ed<0||es<0||ee<0||ee>1e-7*w||
     any(diff(lambda)>0)||!any(lambda>0)||!any(lambda<0))
    .bh_pf_fail(path,"retained interval bounds, uncertainties, or interior are inconsistent")
  .bh_pp_close(z$half_width,w/2,path,"half-width differs from retained interval endpoints")
  .bh_pp_close(z$midpoint,z$lower+w/2,path,"midpoint differs from retained interval endpoints")
  lp<-max(lambda);lm<-min(lambda);uncertainty<-ed+es
  if(!is.finite(uncertainty)||lp<=uncertainty||-lm<=uncertainty)
    .bh_pf_fail(path,"retained generalized eigenvalue signs are unresolved")
  lp_bounds<-c((lp-uncertainty)/(1+e0),(lp+uncertainty)/(1-e0))
  lm_bounds<-c((lm-uncertainty)/(1-e0),(lm+uncertainty)/(1+e0))
  a<-theta-1/lp;b<-theta-1/lm
  ab<-theta-1/lp_bounds;bb<-theta-1/lm_bounds
  expected_error<-max(abs(c(ab-a,bb-b)))+64*.Machine$double.eps*(1+abs(theta)+max(abs(c(1/lp_bounds,1/lm_bounds))))
  .bh_pp_close(z$lower,max(-1,a),path,"lower endpoint differs from retained generalized eigenvalues")
  .bh_pp_close(z$upper,min(1,b),path,"upper endpoint differs from retained generalized eigenvalues")
  .bh_pp_close(bounds$lower,ab,path,"lower endpoint enclosure differs from retained uncertainty operands")
  .bh_pp_close(bounds$upper,bb,path,"upper endpoint enclosure differs from retained uncertainty operands")
  .bh_pp_close(ee,expected_error,path,"endpoint error bound differs from retained enclosure operands",8)
  if(bounds$lower[1L]>bounds$lower[2L]||bounds$upper[1L]>bounds$upper[2L]||
     bounds$lower[1L]>a||bounds$lower[2L]<a||bounds$upper[1L]>b||bounds$upper[2L]<b)
    .bh_pf_fail(path,"retained endpoint enclosures do not enclose their endpoint estimates")
  .bh_pp_certificate(z$certificate,z$constant+theta*z$direction,paste0(path,".certificate"))
  invisible(z)
}
.bh_ps_product_domain <- function(x,path) {
  fields<-c("dimension","status","constant","interior","certificate","method","reason","coordinate_measure",
    "affine_chart_scope","product_status","product_domains","graph_components","runtime_scope")
  attached<-c("parameter","primitive_role","coordinates")
  z<-.bh_pf_record(x,path,c(fields,attached),fields)
  present<-attached[attached%in%names(z)]
  if(length(present)&&length(present)!=length(attached)) .bh_pf_fail(path,"incomplete attached product-domain metadata")
  .bh_pf_number(z$dimension,paste0(path,".dimension"),2,100,integer=TRUE)
  expected<-list(status="feasible",product_status="available",method="exact_zero_separated_one_label_components",
    reason="each connected nonzero-support component contains at most one unique free label; repeated labels across components use their joint full interval",
    coordinate_measure="Lebesgue measure on unique final marginal correlation labels",
    affine_chart_scope="cartesian_product_of_one_label_intervals",
    runtime_scope="exact product chart; inspect whole-model backend capability and evidence before execution")
  for(k in names(expected)).bh_pf_string(z[[k]],paste0(path,".",k),expected[[k]])
  n<-.bh_pp_matrix(z$constant,paste0(path,".constant"),unit=TRUE)
  domains<-.bh_pf_named(z$product_domains,paste0(path,".product_domains"),empty=FALSE)
  labels<-names(domains)
  if(length(domains)!=z$dimension||z$dimension>n) .bh_pf_fail(path,"product dimension differs from its unique-label factors")
  .bh_pp_vec(z$interior,paste0(path,".interior"),size=z$dimension,names=labels)
  if(length(present)) {
    .bh_pp_vec(z$parameter,paste0(path,".parameter"),"c",z$dimension)
    if(!identical(z$parameter,labels)) .bh_pf_fail(path,"attached product parameters differ from the factor order")
    .bh_pf_string(z$primitive_role,paste0(path,".primitive_role"),"unique final marginal correlation labels")
    .bh_pf_string(z$coordinates,paste0(path,".coordinates"),"unique_final_marginal_labels")
  }
  groups<-.bh_pf_named(z$graph_components,paste0(path,".graph_components"),empty=FALSE)
  if(length(groups)>n) .bh_pf_fail(path,"product graph has too many components")
  membership<-integer(n);roots<-integer(length(groups))
  for(i in seq_along(groups)) {
    g<-groups[[i]];p<-paste0(path,".graph_components.",names(groups)[i])
    .bh_pp_vec(g,p,"i")
    if(!length(g)||any(g<1|g>n)||any(diff(g)<=0)||any(membership[g]!=0L))
      .bh_pf_fail(p,"graph components must be ordered disjoint nonempty coordinate sets")
    membership[g]<-i;roots[i]<-g[1L]
  }
  if(any(membership==0L)||any(diff(roots)<=0)||!identical(names(groups),as.character(roots)))
    .bh_pf_fail(path,"graph components must partition all coordinates in minimum-index order")
  owner<-integer(n);support<-z$constant;R<-z$constant
  for(j in seq_along(domains)) {
    p<-paste0(path,".product_domains.",labels[j]);d<-.bh_pp_interval(domains[[j]],p);ix<-d$indices
    .bh_pp_vec(ix,paste0(p,".indices"),"i")
    if(length(ix)<2L||length(ix)!=.bh_ps_dim(d$constant)[1L]||any(ix<1|ix>n)||any(diff(ix)<=0)||any(owner[ix]!=0L))
      .bh_pf_fail(p,"scalar factors must have distinct ordered in-range coordinate unions")
    selected<-base::unique.default(membership[ix])
    if(any(!which(membership%in%selected)%in%ix)) .bh_pf_fail(p,"a scalar factor cuts through a retained graph component")
    for(g in selected) {
      local<-which(membership[ix]==g)
      if(!any(d$direction[local,local,drop=FALSE]==1))
        .bh_pf_fail(p,"a scalar factor includes a graph component without its free label")
    }
    owner[ix]<-j
    .bh_pp_same_matrix(d$constant,z$constant[ix,ix,drop=FALSE],p,"scalar and full affine constants disagree")
    if(z$interior[j]!=d$midpoint) .bh_pf_fail(p,"product interior must retain every scalar midpoint in label order")
    support[ix,ix]<-d$constant+d$direction
    R[ix,ix]<-d$constant+d$midpoint*d$direction
  }
  .bh_pp_same_groups(groups,.bh_pp_groups(support),paste0(path,".graph_components"))
  .bh_pp_certificate(z$certificate,R,paste0(path,".certificate"))
  invisible(TRUE)
}
.bh_pf_product_tables <- function(tables,path="spec.tables") {
  t<-.bh_pf_named(tables,path)
  if(!"correlation_domains"%in%names(t))return(invisible(TRUE))
  domains<-.bh_pf_record(t$correlation_domains,paste0(path,".correlation_domains"),c("component","parameter"))
  for(layer in c("component","parameter")) {
    d<-domains[[layer]]
    if(typeof(d)!="list"||isS4(d)) .bh_pf_fail(path,"expected a plain retained correlation-domain record")
    if(!identical(.bh_pf_get(d,"product_status"),"available"))next
    p<-paste0(path,".correlation_domains.",layer)
    .bh_ps_product_domain(d,p)
    edge_name<-if(layer=="component")"correlations"else"parameter_correlations"
    required<-c(edge_name,"correlation_affine_constants","correlation_affine_cells","correlation_primitives")
    if(!all(required%in%names(t))) .bh_pf_fail(path,"product domains require their complete correlation-table companions")
    for(k in c(edge_name,"correlation_affine_cells","correlation_primitives"))
      .bh_portable_scientific_schema(t[[k]],paste0(path,".",k))
    edge<-unclass(t[[edge_name]]);cells<-unclass(t$correlation_affine_cells);primitives<-unclass(t$correlation_primitives)
    constants<-.bh_pf_record(t$correlation_affine_constants,paste0(path,".correlation_affine_constants"),c("component","parameter"))
    n<-.bh_ps_dim(d$constant)[1L];m<-n*(n-1L)/2L
    if(length(edge$row)!=m||any(edge$row<1L|edge$col>n|edge$row>=edge$col)||
       base::anyDuplicated.default(paste(edge$row,edge$col,sep=":")))
      .bh_pf_fail(p,"product edge table must cover every distinct full-matrix upper-triangle pair")
    free<-which(edge$state=="free");fixed<-which(edge$state=="fixed")
    labels<-base::unique.default(edge$parameter[free])
    if(!identical(labels,names(d$product_domains))||!identical(d$parameter,labels))
      .bh_pf_fail(p,"product label order differs from the complete free-edge table")
    if(!"fixed_value"%in%names(edge)||any(!is.finite(edge$fixed_value[fixed]))||any(edge$fixed_value[fixed]==0))
      .bh_pf_fail(p,"product fixed edges require retained nonzero finite constants")
    C<-diag(n);support<-matrix(0,n,n)
    for(i in fixed)C[edge$row[i],edge$col[i]]<-C[edge$col[i],edge$row[i]]<-edge$fixed_value[i]
    for(i in c(free,fixed))support[edge$row[i],edge$col[i]]<-support[edge$col[i],edge$row[i]]<-1
    .bh_pp_same_matrix(d$constant,C,p,"product constant differs from the complete fixed-edge table")
    .bh_pp_matrix(constants[[layer]],paste0(path,".correlation_affine_constants.",layer),n,unit=TRUE)
    .bh_pp_same_matrix(constants[[layer]],C,p,"affine constants differ from the product/full-edge constants")
    groups<-.bh_pp_groups(support);.bh_pp_same_groups(d$graph_components,groups,p)
    membership<-integer(n);group_label<-rep(NA_character_,length(groups))
    for(i in seq_along(groups))membership[groups[[i]]]<-i
    for(i in seq_along(groups)) {
      ids<-base::unique.default(edge$parameter[free[membership[edge$row[free]]==i]])
      if(length(ids)>1L) .bh_pf_fail(p,"a nonzero-support component contains more than one free label")
      if(length(ids))group_label[i]<-ids
    }
    for(j in seq_along(labels)) {
      q<-d$product_domains[[j]];ix<-which(!is.na(group_label[membership])&group_label[membership]==labels[j])
      if(length(ix)!=length(q$indices)||any(ix!=q$indices))
        .bh_pf_fail(p,"shared label factor does not contain exactly the union of its graph components")
      E<-matrix(0,length(ix),length(ix));at<-free[edge$parameter[free]==labels[j]]
      for(i in at) {
        a<-match(edge$row[i],ix);b<-match(edge$col[i],ix)
        E[a,b]<-E[b,a]<-1
      }
      .bh_pp_same_matrix(q$constant,C[ix,ix,drop=FALSE],p,"scalar constant differs from the full fixed-edge table")
      .bh_pp_same_matrix(q$direction,E,p,"scalar direction differs from all and only its retained free-label edges")
    }
    # Bind the redundant scientific summaries as well: a forged bridge cannot
    # be hidden by changing only a domain, mask, cell table, or label alias.
    at<-which(cells$layer==layer)
    if(length(at)!=m) .bh_pf_fail(p,"affine cells omit or duplicate full product edges")
    for(k in c("row","col","state","parameter","source"))if(!identical(cells[[k]][at],edge[[k]]))
      .bh_pf_fail(p,"affine cells disagree with the full product edge table")
    if(!identical(cells$endpoint1[at],edge[[if(layer=="component")"component1"else"target1"]])||
       !identical(cells$endpoint2[at],edge[[if(layer=="component")"component2"else"target2"]]))
      .bh_pf_fail(p,"affine cell endpoint identities differ from the full product edge table")
    expected_fixed<-rep(NA_real_,m);expected_fixed[edge$state=="zero"]<-0;expected_fixed[fixed]<-edge$fixed_value[fixed]
    expected_index<-match(edge$parameter,labels)
    if(!identical(cells$fixed_value[at],expected_fixed)||!identical(as.integer(cells$primitive_index[at]),expected_index))
      .bh_pf_fail(p,"affine cells have inconsistent fixed values or unique-label indices")
    at<-which(primitives$layer==layer)
    if(!identical(primitives$parameter[at],labels)||!identical(as.integer(primitives$primitive_index[at]),seq_along(labels)))
      .bh_pf_fail(p,"primitive table differs from product label order")
    members<-unclass(primitives$edge_indices)
    for(j in seq_along(labels)) {
      expected_edges<-free[edge$parameter[free]==labels[j]];saved<-members[[at[j]]]
      .bh_pp_vec(saved,paste0(p,".primitive_edges"),"i")
      if(primitives$members[at[j]]!=length(expected_edges)||length(saved)!=length(expected_edges)||any(saved!=expected_edges))
        .bh_pf_fail(p,"primitive membership omits or duplicates a full product-label edge")
    }
  }
  invisible(TRUE)
}
