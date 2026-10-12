# Separate bounded affine lowering. The reviewed scientific coordinates remain
# unique final marginal labels; a backend vector is a computational chart.
.bh_affine_lowering_available <- function() TRUE
.bh_affine_generator_qualified <- function() TRUE
.bh_affine_engine <- function(spec) !.bh_reference_mode(spec)||.bh_has_affine_correlations(spec)||.bh_centered_final_marginal_mode(spec)
.bh_affine_layer_plan <- function(spec,layer) {
  tab<-if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations
  n<-if(layer=="component")nrow(spec$tables$components)else nrow(spec$tables$parameter_residuals)
  ids<-unique(tab$parameter[tab$state=="free"])
  raw<-layer=="component"&&.bh_raw_cor(spec)
  mode<-if(!.bh_is_affine_cor_table(tab)||raw)0L else if(!length(ids))1L else 2L
  C<-diag(n);E<-matrix(0,n,n);bounds<-c(-1,1,0,1,0)
  domain<-.bh_correlation_domains(spec)[[layer]]
  if(mode==2L&&length(ids)>1L&&identical(domain$product_status,"available"))mode<-3L
  if(mode) {
    if(length(ids)>1L&&mode!=3L).mp_abort("E_CORRELATION_DIMENSION_UNSUPPORTED","new affine lowering supports a fully fixed matrix or one unique marginal label per layer; connected multi-label declarations remain inspectable; exact zero-separated one-label products have a source-only chart")
    if(identical(domain$status,"proven_infeasible_strict_PD")) .mp_abort("E_CORRELATION_DOMAIN","the affine layer has no strictly positive-definite support")
    if(!identical(domain$status,"feasible")) .mp_abort("E_CORRELATION_NUMERIC","affine lowering needs a certified strict-PD interior; unresolved numerical feasibility does not invalidate the declaration")
    C<-domain$constant
    if(mode==2L) {
      if(!identical(domain$interval_status,"available")) .mp_abort("E_CORRELATION_NUMERIC","one-label affine lowering needs a numerically resolved full interval; no inner interval or clipping is substituted")
      E<-domain$direction;bounds<-c(domain$lower,domain$upper,domain$midpoint,domain$half_width,domain$endpoint_error_bound)
    }
  }
  list(layer=layer,mode=mode,size=n,parameters=if(raw)character()else ids,constant=C,direction=E,bounds=bounds,domain=domain,
    coordinate=if(raw)"separate ordered raw-B law"else if(mode==0L)"existing MGS raw coordinates"else if(mode==1L)"none: fixed matrix"else if(mode==2L)"one unconstrained interval coordinate z; scientific rho remains separate"else"one independent interval coordinate per unique marginal label; whole-model experimental capability is assessed separately")
}
.bh_affine_lowering_plan <- function(spec) lapply(stats::setNames(c("component","parameter"),c("component","parameter")),function(layer).bh_affine_layer_plan(spec,layer))
.bh_affine_lowering_capability <- function(spec) {
  if(!.bh_has_affine_correlations(spec))return(list(status="existing_route",reason="ordinary zero/free or separate ordered raw-B coordinates"))
  x<-tryCatch(.bh_affine_lowering_plan(spec),error=identity)
  if(inherits(x,"error"))return(list(status="unavailable",code=x$code,reason=conditionMessage(x)))
  list(status="bounded_source_available",reason="fully fixed strict-PD, one-label full-interval, or exact zero-separated product source lowering; product execution depends on the separately qualified whole-model profile",layers=x)
}
.bh_affine_runtime_gate <- function(spec) {
  .bh_final_correlation_reporting_gate(spec)
  if(.bh_centered_final_marginal_mode(spec)) {
    policy<-.bh_numerical_policy(spec)
    if(policy$status=="execution_unqualified_prior") .mp_abort("E_NUMERICAL_PRIOR_UNQUALIFIED",policy$reason)
  }
  if(.bh_has_affine_correlations(spec)) {
    if(!.bh_affine_generator_qualified()) .mp_abort("E_FRONTEND_BACKEND_UNSUPPORTED","new affine source/data are inspectable; native qualification and execution remain closed")
    if(!.bh_affine_runtime_supported(spec)) .mp_abort("E_FRONTEND_BACKEND_UNSUPPORTED","affine runtime support requires iid/common-plus-residual final-marginal coordinates, or sample-centered/task-Cholesky with an ordered raw common law or ordinary zero/free final-marginal common graph and a separate fixed or one-label residual layer; other combinations remain source-only")
    policy<-.bh_numerical_policy(spec)
    if(policy$status=="execution_unqualified_prior") .mp_abort("E_NUMERICAL_PRIOR_UNQUALIFIED",policy$reason)
  }
  if(!.bh_reference_mode(spec)&&!.bh_coefficient_generator_qualified()) .mp_abort("E_FRONTEND_BACKEND_UNSUPPORTED","the amended common/residual generator awaits its independent source/state/native gate")
  invisible(TRUE)
}
.bh_affine_backend_data <- function(spec) {
  if(!.bh_affine_engine(spec))return(list(declarations=character(),data=list()))
  plans<-.bh_affine_lowering_plan(spec);declarations<-character();data<-list()
  for(layer in names(plans)) {
    if(layer=="component"&&.bh_raw_cor(spec))next
    p<-plans[[layer]];prefix<-paste0(layer,"_cor_");n<-if(layer=="component")"K"else"U"
    declarations<-c(declarations,paste0("int<lower=0,upper=3> ",prefix,"mode;"),
      paste0("matrix[",n,",",n,"] ",prefix,"constant;"),paste0("matrix[",n,",",n,"] ",prefix,"direction;"),paste0("vector[5] ",prefix,"bounds;"))
    count<-if(p$mode==3L)length(p$parameters)else 0L
    declarations<-c(declarations,paste0("int<lower=0> ",prefix,"product_count;"),
      paste0("array[",n,",",n,"] int<lower=0,upper=",prefix,"product_count> ",prefix,"product_labels;"),
      paste0("matrix[",prefix,"product_count,5] ",prefix,"product_bounds;"))
    data[[paste0(prefix,"product_count")]]<-count
    labels<-matrix(0L,p$size,p$size);intervals<-matrix(0,count,5)
    for(j in seq_len(count)) {
      d<-p$domain$product_domains[[j]]
      block<-labels[d$indices,d$indices,drop=FALSE];block[d$direction!=0]<-j
      labels[d$indices,d$indices]<-block
      intervals[j,]<-c(d$lower,d$upper,d$midpoint,d$half_width,d$endpoint_error_bound)
    }
    data[[paste0(prefix,"product_labels")]]<-labels
    data[[paste0(prefix,"product_bounds")]]<-intervals
    data[[paste0(prefix,"mode")]]<-p$mode;data[[paste0(prefix,"constant")]]<-p$constant
    data[[paste0(prefix,"direction")]]<-p$direction;data[[paste0(prefix,"bounds")]]<-p$bounds
  }
  list(declarations=declarations,data=data,plans=plans)
}
.bh_affine_layer_state <- function(spec,layer,raw) {
  p<-.bh_affine_layer_plan(spec,layer)
  if(length(raw)!=length(p$parameters)||any(!is.finite(raw))) .mp_abort("E_CORRELATION_NUMERIC","computational correlation state must match the layer's unique coordinates")
  if(p$mode==0L) {
    if(!p$size)return(list(L=matrix(numeric(),0L,0L),R=matrix(numeric(),0L,0L),log_jacobian=0,log_determinant=0,mode=0L,scientific_values=numeric()))
    tab<-if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations
    mask<-.bh_mask(tab,seq_len(p$size))=="free"
    x<-.bh_correlation_chart(raw,mask)
    return(c(x,list(mode=0L,scientific_values=x$R[cbind(tab$row[tab$state=="free"],tab$col[tab$state=="free"])])))
  }
  if(p$mode==1L)return(list(L=p$domain$factor,R=p$constant,log_jacobian=0,mode=1L,scientific_values=numeric()))
  if(p$mode==3L) {
    x<-.bh_cor_product_chart(raw,p$domain)
    return(list(L=x$L,R=x$R,log_jacobian=x$log_jacobian,mode=3L,scientific_values=x$theta))
  }
  x<-.bh_cor_interval_chart(raw[1L],p$domain);R<-p$constant+x$theta*p$direction
  list(L=t(chol(R)),R=R,log_jacobian=x$log_jacobian,mode=2L,scientific_values=x$theta)
}
.bh_affine_initial_correlations <- function(spec,prior_data) {
  rows<-list();scientific<-list();reasons<-character();kernel<-jacobian<-0
  for(layer in c("component","parameter")) {
    if(layer=="component"&&.bh_raw_cor(spec))next
    p<-.bh_affine_layer_plan(spec,layer);d<-length(p$parameters);raw<-rep(0,d)
    value<-tryCatch({
      x<-.bh_affine_layer_state(spec,layer,raw)
      logdet<-if(p$size)2*sum(log(diag(x$L)))else 0
      eta<-prior_data[[if(layer=="component")"cor_eta"else"parameter_cor_eta"]]
      term<-if(d)2*(eta-1)*sum(log(diag(x$L)))else 0
      gradient<-if(p$mode==2L)(eta-1)*sum(diag(solve(x$R,p$direction)))*p$bounds[4]else if(p$mode==3L)sum(vapply(seq_len(d),function(j)
        {domain<-p$domain$product_domains[[j]];
          (eta-1)*sum(diag(solve(x$R[domain$indices,domain$indices,drop=FALSE],domain$direction)))*domain$half_width},numeric(1)))else 0
      if(any(!is.finite(c(logdet,term,gradient,x$log_jacobian)))) .mp_abort("E_CORRELATION_NUMERIC","initial joint correlation kernel, interval Jacobian or derivative is not finite")
      list(x=x,logdet=logdet,term=term,gradient=gradient)
    },error=identity)
    failed<-inherits(value,"error")
    reason<-if(failed)conditionMessage(value)else""
    if(failed){reasons<-c(reasons,paste(layer,reason));kernel<-jacobian<-NA_real_;scientific[[layer]]<-rep(NA_real_,d)}else {
      scientific[[layer]]<-value$x$scientific_values;kernel<-kernel+value$term;jacobian<-jacobian+value$x$log_jacobian
    }
    row<-data.frame(layer=layer,mode=p$mode,unique_parameters=d,
      log_determinant=if(failed)NA_real_ else value$logdet,
      joint_prior_kernel=if(failed)NA_real_ else value$term,
      chart_log_jacobian=if(failed)NA_real_ else value$x$log_jacobian,
      initial_kernel_derivative=if(failed)NA_real_ else value$gradient,
      status=if(failed)"numerically_unavailable"else"screened",reason=reason,
      normalization=if(d)"restricted correlation normalizer is not evaluated; eta and constraints are fixed"else"not applicable: no correlation prior coordinate",stringsAsFactors=FALSE)
    row$computational_values<-I(list(raw));row$scientific_values<-I(list(scientific[[layer]]));rows[[layer]]<-row
  }
  list(table=do.call(rbind,rows),scientific=scientific,kernel=kernel,log_jacobian=jacobian,reasons=reasons)
}
.bh_affine_transformed_data_stan <- function(raw=FALSE) {
  declarations<-if(!raw)"matrix[K,K] component_fixed_factor=diag_matrix(rep_vector(1.0,K));"else character()
  declarations<-c(declarations,"matrix[U,U] parameter_fixed_factor=diag_matrix(rep_vector(1.0,U));")
  statements<-c(if(!raw)"if(component_cor_mode==1 && K>0) component_fixed_factor=bmeas_checked_affine_cholesky(component_cor_constant);",
    "if(parameter_cor_mode==1 && U>0) parameter_fixed_factor=bmeas_checked_affine_cholesky(parameter_cor_constant);")
  list(declarations=declarations,statements=statements)
}
.bh_affine_chart_stan <- function() c(
  "// Unique final marginal labels: fixed matrices have no prior coordinate.",
  "matrix bmeas_checked_affine_cholesky(matrix R) {",
  "  int n=rows(R); matrix[n,n] L=cholesky_decompose(R);",
  "  matrix[n,n] B=mdivide_left_tri_low(L,diag_matrix(rep_vector(1.0,n)));",
  "  matrix[n,n] abs_B; matrix[n,n] abs_R; matrix[n,n] G; matrix[n,n] A;",
  "  real gamma=(n+2)*2.220446049250313e-16/(1-(n+2)*2.220446049250313e-16);",
  "  real largest=0; real underflow_bound;",
  "  if(n>0) for(i in 1:n) for(j in 1:n) {",
  "    if(is_nan(B[i,j])||is_inf(B[i,j])) reject(\"affine correlation chart inverse factor is not representable\");",
  "    abs_B[i,j]=abs(B[i,j]); abs_R[i,j]=abs(R[i,j]); largest=fmax(largest,abs_B[i,j]);",
  "  }",
  "  G=abs_B*abs_R*transpose(abs_B); A=B*R*transpose(B);",
  "  underflow_bound=n*n*2.2250738585072014e-308*square(1+largest);",
  "  if(n>0) for(i in 1:n) {",
  "    real margin=A[i,i]; real accumulated=abs(A[i,i]);",
  "    for(j in 1:n) {",
  "      real error=(2*gamma+square(gamma))*G[i,j]/square(1-gamma)+underflow_bound;",
  "      margin-=error; accumulated+=abs(error);",
  "      if(i!=j) { margin-=abs(A[i,j]); accumulated+=abs(A[i,j]); }",
  "    }",
  "    if(is_nan(margin)||is_inf(margin)||is_nan(accumulated)||is_inf(accumulated)||margin<=4*gamma*accumulated)",
  "      reject(\"affine correlation chart strict positive definiteness is not numerically certified; no clipping\");",
  "  }",
  "  return L;",
  "}",
  "real bmeas_interval_label(real z, data vector bounds) {",
  "  real t=tanh(z); real rho=bounds[3]+bounds[4]*t;",
  "  if(is_nan(z)||is_inf(z)||abs(t)>=1||is_nan(rho)||is_inf(rho)||rho<=bounds[1]||rho>=bounds[2])",
  "    reject(\"affine correlation chart interval saturation or nonrepresentable state; no clipping\");",
  "  return rho;",
  "}",
  "matrix bmeas_corr_layer(vector raw, data int mode, data array[,] int free_mask, data matrix C, data matrix direction, data vector bounds, data matrix fixed_factor, data array[,] int product_labels, data matrix product_bounds) {",
  "  int n=rows(C); matrix[n+1,n] result=rep_matrix(0.0,n+1,n);",
  "  if(mode==0) return bhmm_corr_chart(raw,free_mask);",
  "  if(mode==1) {",
  "    if(num_elements(raw)!=0) reject(\"fixed correlation matrix cannot have a free coordinate\");",
  "    if(n>0) result[1:n,1:n]=fixed_factor;",
  "    return result;",
  "  }",
  "  if(mode==3) {",
  "    int d=num_elements(raw); matrix[n,n] R=C; real log_jacobian=0; vector[d] rho_values;",
  "    if(d<2||size(product_labels)!=n||rows(product_bounds)!=d||cols(product_bounds)!=5) reject(\"invalid affine product dimensions\");",
  "    for(j in 1:d) {",
  "      vector[5] b=to_vector(product_bounds[j]);",
  "      real rho;",
  "      if(b[4]<=0||b[1]>=b[2]||b[5]<0||b[5]>1e-7*(b[2]-b[1])) reject(\"unresolved affine product interval\");",
  "      rho=bmeas_interval_label(raw[j],to_vector(product_bounds[j])); rho_values[j]=rho;",
  "      log_jacobian+=log(b[4])+2*(log(2.0)-abs(raw[j])-log1p(exp(-2*abs(raw[j]))));",
  "    }",
  "    for(i in 1:n) for(j in 1:n) if(product_labels[i,j]>0) R[i,j]+=rho_values[product_labels[i,j]];",
  "    if(is_nan(log_jacobian)||is_inf(log_jacobian)) reject(\"affine product Jacobian is not representable\");",
  "    result[1:n,1:n]=bmeas_checked_affine_cholesky(R); result[n+1,1]=log_jacobian;",
  "    return result;",
  "  }",
  "  if(num_elements(raw)!=1||n<2||bounds[4]<=0||bounds[1]>=bounds[2]||bounds[5]<0||bounds[5]>1e-7*(bounds[2]-bounds[1]))",
  "    reject(\"affine correlation chart needs exactly one coordinate and a resolved full interval\");",
  "  {",
  "    real rho=bmeas_interval_label(raw[1],bounds);",
  "    real log_jacobian=log(bounds[4])+2*(log(2.0)-abs(raw[1])-log1p(exp(-2*abs(raw[1]))));",
  "    matrix[n,n] R=C+rho*direction;",
  "    if(is_nan(log_jacobian)||is_inf(log_jacobian)) reject(\"affine correlation chart Jacobian is not representable\");",
  "    result[1:n,1:n]=bmeas_checked_affine_cholesky(R); result[n+1,1]=log_jacobian;",
  "  }",
  "  return result;",
  "}")
