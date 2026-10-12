# Exact Cartesian-product subfamily of affine marginal-correlation domains.
# Connected components use every potentially nonzero edge, including fixed
# nonzero edges. A component may contain at most one unique free label.
# Repeated labels across components share one interval and one Jacobian.
.bh_cor_product_domain <- function(table,n) {
  ids<-unique(table$parameter[table$state=="free"])
  if(length(ids)<2L||n>100L)return(NULL)
  parent<-seq_len(n)
  root<-function(i){while(parent[i]!=i)i<-parent[i];i}
  for(i in which(table$state%in%c("free","fixed"))) {
    a<-root(table$row[i]);b<-root(table$col[i]);parent[max(a,b)]<-min(a,b)
  }
  groups<-split(seq_len(n),vapply(seq_len(n),root,integer(1)))
  labels<-lapply(groups,function(g)unique(table$parameter[table$state=="free"&table$row%in%g&table$col%in%g]))
  if(any(lengths(labels)>1L))return(NULL)
  subset_table<-function(indices) {
    x<-table[table$row%in%indices&table$col%in%indices,,drop=FALSE]
    x$row<-match(x$row,indices);x$col<-match(x$col,indices)
    attr(x,"affine_rules")<-NULL;x
  }
  fixed<-groups[lengths(labels)==0L]
  for(g in fixed) {
    domain<-.bh_cor_domain(subset_table(g),length(g))
    if(!identical(domain$status,"feasible"))return(NULL)
  }
  domains<-vector("list",length(ids))
  names(domains)<-ids
  for(j in seq_along(ids)) {
    selected<-vapply(labels,function(x)length(x)==1L&&identical(x,ids[j]),logical(1))
    indices<-sort(unlist(groups[selected],use.names=FALSE))
    domain<-.bh_cor_domain(subset_table(indices),length(indices))
    if(!identical(domain$status,"feasible")||!identical(domain$interval_status,"available"))return(NULL)
    domain$indices<-indices;domains[[j]]<-domain
  }
  interior<-vapply(domains,`[[`,numeric(1),"midpoint")
  C<-.bh_cor_constant_matrix(table,n);R<-C
  for(j in seq_along(ids)) {
    d<-domains[[j]];R[d$indices,d$indices]<-d$constant+interior[j]*d$direction
  }
  certificate<-.bh_cor_spd_certificate(R)
  if(!identical(certificate$status,"strictly_positive_definite"))return(NULL)
  list(dimension=length(ids),status="feasible",constant=C,interior=interior,
    certificate=certificate,method="exact_zero_separated_one_label_components",
    reason="each connected nonzero-support component contains at most one unique free label; repeated labels across components use their joint full interval",
    coordinate_measure="Lebesgue measure on unique final marginal correlation labels",
    affine_chart_scope="cartesian_product_of_one_label_intervals",product_status="available",
    product_domains=domains,graph_components=groups,
    runtime_scope="exact product chart; inspect whole-model backend capability and evidence before execution")
}
.bh_cor_product_chart <- function(z,domain) {
  if(!identical(domain$product_status,"available")||length(z)!=domain$dimension||any(!is.finite(z)))
    .mp_abort("E_CORRELATION_NUMERIC","product chart requires one finite coordinate per unique marginal label")
  values<-numeric(length(z));jacobian<-0;R<-domain$constant
  for(j in seq_along(z)) {
    chart<-.bh_cor_interval_chart(z[j],domain$product_domains[[j]])
    values[j]<-chart$theta;jacobian<-jacobian+chart$log_jacobian
    d<-domain$product_domains[[j]]
    R[d$indices,d$indices]<-d$constant+values[j]*d$direction
  }
  certificate<-.bh_cor_spd_certificate(R)
  if(!identical(certificate$status,"strictly_positive_definite")||!is.finite(jacobian))
    .mp_abort("E_CORRELATION_NUMERIC","product chart is not numerically certified; no support clipping")
  list(theta=values,R=R,L=certificate$factor,log_jacobian=jacobian,
    coordinate="one interval coordinate per unique final marginal label")
}
