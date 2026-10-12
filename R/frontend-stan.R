# Exact zero/free *marginal* correlation chart. The Jacobian is relative to
# the unique free marginal entries, not the ambient Cholesky coordinates.
.bh_positive_thin_r <- function(G) {
  d<-ncol(G);Q<-matrix(0,nrow(G),d);T<-matrix(0,d,d)
  for(j in seq_len(d)) {
    v<-G[,j]
    if(j>1L)for(pass in 1:2)for(k in seq_len(j-1L)) {
      correction<-sum(Q[,k]*v);T[k,j]<-T[k,j]+correction;v<-v-correction*Q[,k]
    }
    T[j,j]<-sqrt(sum(v*v))
    if(!is.finite(T[j,j])||T[j,j]<=0).mp_abort("E_CORRELATION_NUMERIC","correlation basis cannot be resolved without pivoting or regularization")
    Q[,j]<-v/T[j,j]
  }
  T
}
.bh_correlation_chart <- function(raw, mask) {
  if(!is.matrix(mask)||nrow(mask)<1L||nrow(mask)!=ncol(mask)||!identical(mask,t(mask))||anyNA(mask)||any(!mask%in%c(0,1))||any(diag(mask)))
    .mp_abort("E_CORRELATION_MASK","expected a symmetric logical free mask with false diagonal")
  K <- nrow(mask); E <- sum(mask[lower.tri(mask)])
  if(!is.numeric(raw)||length(raw)!=E||any(!is.finite(raw))) .mp_abort("E_CORRELATION_STATE","raw correlation coordinate dimension/support differs")
  L <- R <- diag(K); log_jacobian <- 0; offset <- 0L
  if(K>1L) for(i in 2:K) {
    free <- which(mask[i,seq_len(i-1L)]); d <- length(free)
    if(!d) next
    selector <- diag(i-1L)[,free,drop=FALSE]
    G <- forwardsolve(L[seq_len(i-1L),seq_len(i-1L),drop=FALSE],selector)
    T <- .bh_positive_thin_r(G)
    x <- raw[offset+seq_len(d)]; maximum <- max(abs(x))
    log_norm <- if(maximum>1)2*log(maximum)+log(sum((x/maximum)^2)+(1/maximum)^2)else log1p(sum(x*x))
    if(!is.finite(log_norm)||any(!is.finite(T))||any(diag(T)<=0)) .mp_abort("E_CORRELATION_NUMERIC","correlation chart is outside reliable numeric support")
    z <- x*exp(-.5*log_norm)
    y <- backsolve(T,z)
    R[i,free] <- R[free,i] <- y
    prefix <- tryCatch(t(chol(R[seq_len(i),seq_len(i),drop=FALSE])),
      error=function(e).mp_abort("E_CORRELATION_NUMERIC","marginal correlation prefix is numerically non-positive-definite; no clipping or jitter is applied"))
    if(abs(log(prefix[i,i])+.5*log_norm)>log1p(1e-6))
      .mp_abort("E_CORRELATION_NUMERIC","correlation chart diagonal identity lost numeric precision (multiplicative tolerance 1e-6)")
    L[seq_len(i),seq_len(i)] <- prefix
    log_jacobian <- log_jacobian-sum(log(diag(T)))-.5*(d+2)*log_norm
    offset <- offset+d
  }
  if(!is.finite(log_jacobian)||any(!is.finite(L))||max(abs(tcrossprod(L)-R))>1e-10)
    .mp_abort("E_CORRELATION_NUMERIC","correlation chart reconstruction lost numeric precision")
  list(L=L,R=R,log_jacobian=log_jacobian,
    log_determinant=2*sum(log(diag(L))))
}
.bh_correlation_inverse <- function(R, mask) {
  if(!is.matrix(R)||!identical(dim(R),dim(mask))||any(!is.finite(R))||max(abs(R-t(R)))>1e-10||max(abs(diag(R)-1))>1e-10)
    .mp_abort("E_CORRELATION_STATE","R must be a finite symmetric unit-diagonal matrix of the mask dimensions")
  if(any(abs(R[!mask & row(R)!=col(R)])>1e-10)) .mp_abort("E_CORRELATION_STATE","R violates a prescribed marginal zero")
  L <- tryCatch(t(chol(R)),error=function(e).mp_abort("E_CORRELATION_STATE","R is not strictly positive definite"))
  out <- numeric();K <- nrow(R)
  if(K>1L) for(i in 2:K) {
    free <- which(mask[i,seq_len(i-1L)]); if(!length(free))next
    G <- forwardsolve(L[seq_len(i-1L),seq_len(i-1L),drop=FALSE],diag(i-1L)[,free,drop=FALSE])
    T<-.bh_positive_thin_r(G);z <- as.vector(T%*%R[i,free])
    if(any(!is.finite(z))||sum(z*z)>=1) .mp_abort("E_CORRELATION_NUMERIC","correlation inverse cannot resolve an interior point")
    out <- c(out,z/sqrt(1-sum(z*z)))
  }
  out
}
.bh_chart_stan <- function() c(
  "matrix bhmm_positive_thin_r(matrix G) {",
  "  int n=rows(G); int d=cols(G);",
  "  matrix[n,d] Q=rep_matrix(0.0,n,d); matrix[d,d] T=rep_matrix(0.0,d,d);",
  "  for(j in 1:d) {",
  "    vector[n] v=G[,j];",
  "    if(j>1) for(pass in 1:2) for(k in 1:(j-1)) {",
  "      real correction=dot_product(Q[,k],v);",
  "      T[k,j]+=correction; v-=correction*Q[,k];",
  "    }",
  "    T[j,j]=sqrt(dot_self(v));",
  "    if(is_nan(T[j,j])||is_inf(T[j,j])||T[j,j]<=0) reject(\"correlation basis lost numeric precision\");",
  "    Q[,j]=v/T[j,j];",
  "  }",
  "  return T;",
  "}",
  "matrix bhmm_corr_chart(vector raw, data array[,] int free_mask) {",
  "  int K=size(free_mask);",
  "  matrix[K+1,K] result=rep_matrix(0.0,K+1,K);",
  "  matrix[K,K] marginal_R=diag_matrix(rep_vector(1.0,K));",
  "  int raw_cursor=1;",
  "  real log_jacobian=0;",
  "  for(i in 1:K) result[i,i]=1;",
  "  if(K>1) for(i in 2:K) {",
  "    int d=sum(free_mask[i,1:(i-1)]);",
  "    if(d>0) {",
  "      matrix[i-1,d] E=rep_matrix(0.0,i-1,d);",
  "      matrix[i-1,d] G; matrix[d,d] T;",
  "      vector[d] x=segment(raw,raw_cursor,d);",
  "      real maximum=max(abs(x));",
  "      real log_norm; vector[d] z;",
  "      vector[d] y; int column=1; matrix[i,i] prefix;",
  "      if(maximum>1) log_norm=2*log(maximum)+log(dot_self(x/maximum)+square(1/maximum));",
  "      else log_norm=log1p(dot_self(x));",
  "      if(is_nan(log_norm)||is_inf(log_norm)) reject(\"correlation chart numeric overflow\");",
  "      z=x*exp(-0.5*log_norm);",
  "      for(j in 1:(i-1)) if(free_mask[i,j]==1) { E[j,column]=1; column+=1; }",
  "      G=mdivide_left_tri_low(block(result,1,1,i-1,i-1),E);",
  "      T=bhmm_positive_thin_r(G);",
  "      y=to_vector(mdivide_right_tri_low(to_row_vector(z),transpose(T)));",
  "      column=1;",
  "      for(j in 1:(i-1)) if(free_mask[i,j]==1) { marginal_R[i,j]=y[column]; marginal_R[j,i]=y[column]; column+=1; }",
  "      prefix=cholesky_decompose(block(marginal_R,1,1,i,i));",
  "      if(abs(log(prefix[i,i])+0.5*log_norm)>log1p(1e-6)) reject(\"correlation chart diagonal identity lost numeric precision; multiplicative tolerance 1e-6\");",
  "      result[1:i,1:i]=prefix;",
  "      log_jacobian+=-sum(log(diagonal(T)))-0.5*(d+2)*log_norm;",
  "      raw_cursor+=d;",
  "    }",
  "  }",
  "  if(is_nan(log_jacobian)||is_inf(log_jacobian)) reject(\"correlation chart Jacobian numeric overflow\");",
  "  if(max(abs(to_vector(tcrossprod(block(result,1,1,K,K))-marginal_R)))>1e-10) reject(\"correlation chart reconstruction lost numeric precision\");",
  "  result[K+1,1]=log_jacobian;",
  "  return result;",
  "}"
)
.bh_prior_data <- function(spec) {
  tab <- spec$tables$priors; targets <- spec$tables$targets; loads <- .bh_free_loadings(spec)
  entry <- function(id) .bh_prior_distribution(tab$prior[match(id,tab$parameter)])
  mean_priors <- lapply(vapply(targets$key,function(key)paste0("Intercept:",key),character(1)),entry)
  load_priors <- lapply(loads$parameter,entry)
  sd_priors <- if(nrow(spec$tables$observation_parameters))lapply(paste0("observation_sd:",spec$tables$observation_parameters$key),entry)else list()
  distribution_id <- function(p)match(p$distribution,c("normal","logistic","student_t","half_normal","half_student_t"))
  location <- function(p)if(p$distribution %in% c("student_t","half_student_t"))p$values[2]else if(p$distribution=="half_normal")0 else p$values[1]
  scale <- function(p)tail(p$values,1)
  df <- function(p)if(p$distribution %in% c("student_t","half_student_t"))p$values[1]else 1
  amplitude_priors<-lapply(.bh_amplitudes(spec)$parameter,entry)
  component_priors<-lapply(.bh_free_component_sd(spec)$parameter,entry)
  parameter_priors<-lapply(.bh_free_parameter_sd(spec)$parameter,entry)
  total_priors<-lapply(spec$tables$share_targets$parameter,entry)
  share_priors<-lapply(spec$tables$share_coefficients$parameter,entry)
  cor_shape<-function(layer){at<-which(tab$class=="cor" & tab$layer==layer);if(length(at)).bh_prior_distribution(tab$prior[at])$values[1]else 1}
  out<-list(alpha_dist=vapply(mean_priors,distribution_id,integer(1)),alpha_location=vapply(mean_priors,location,numeric(1)),
    alpha_scale=vapply(mean_priors,scale,numeric(1)),alpha_df=vapply(mean_priors,df,numeric(1)),
    amplitude_dist=vapply(amplitude_priors,distribution_id,integer(1)),amplitude_scale=vapply(amplitude_priors,scale,numeric(1)),amplitude_df=vapply(amplitude_priors,df,numeric(1)),
    loading_scale=vapply(load_priors,scale,numeric(1)),sd_dist=vapply(sd_priors,distribution_id,integer(1)),
    sd_scale=vapply(sd_priors,scale,numeric(1)),sd_df=vapply(sd_priors,df,numeric(1)),
    cor_eta=cor_shape("component"),parameter_cor_eta=cor_shape("parameter"),
    component_sd_dist=vapply(component_priors,distribution_id,integer(1)),component_sd_scale=vapply(component_priors,scale,numeric(1)),component_sd_df=vapply(component_priors,df,numeric(1)),
    parameter_sd_dist=vapply(parameter_priors,distribution_id,integer(1)),
    parameter_sd_scale=vapply(parameter_priors,scale,numeric(1)),parameter_sd_df=vapply(parameter_priors,df,numeric(1)),
    total_sd_dist=vapply(total_priors,distribution_id,integer(1)),total_sd_scale=vapply(total_priors,scale,numeric(1)),total_sd_df=vapply(total_priors,df,numeric(1)),
    share_beta_location=vapply(share_priors,location,numeric(1)),share_beta_scale=vapply(share_priors,scale,numeric(1)))
  c(out,.bh_reference_prior_data(spec))
}
.bh_backend_ir <- function(spec, threaded=FALSE) {
  spec<-.bh_normalize_spec(spec); .bh_fit_gate(spec)
  if(.bh_reference_mode(spec)&&!.bh_reference_lowering_available()).mp_abort("E_REFERENCE_LOWERING_UNAVAILABLE","the new reference-law IR is inspectable; code generation is closed until its separate lowering/native gate")
  if(!isTRUE(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1"))) .bh_schema_error("the parameter-level compiler requires an explicitly rebuilt context from the saved formula, original data and intended priors; retained missing domains can change unknowns, so old fingerprints are never rewritten")
  raw<-.bh_raw_cor(spec);task<-.bh_task_augmentation(spec);affine_engine<-.bh_affine_engine(spec)
  affine_data<-.bh_affine_backend_data(spec)
  reference_data<-.bh_reference_backend_data(spec)
  index <- list(keys=spec$tables$targets$key,nuisance=spec$tables$observation_parameters$key,
    J=nrow(spec$tables$targets),K=nrow(spec$tables$components),U=nrow(spec$tables$parameter_residuals),S=nrow(spec$tables$observation_parameters))
  oi <- observation_ir(spec,index,threaded)
  loads <- .bh_free_loadings(spec); positive <- loads$sign=="positive_anchor"
  cells<-spec$tables$ordinary_loading_map
  nload <- nrow(loads); aid <- fid <- integer(nload)
  aid[positive] <- seq_len(sum(positive));fid[!positive] <- seq_len(sum(!positive))
  mask <- spec$tables$correlation_mask=="free";storage.mode(mask)<-"integer"
  parameter_mask<-spec$tables$parameter_correlation_mask=="free";storage.mode(parameter_mask)<-"integer"
  share_data<-.bh_share_backend_data(spec)
  declarations <- c("int<lower=0> CS;","array[K] int<lower=0,upper=CS> component_sd_index;",
    "array[CS] int<lower=4,upper=5> component_sd_dist;","vector<lower=0>[CS] component_sd_scale;","vector<lower=0>[CS] component_sd_df;",
    "int<lower=0> T;","int<lower=0> H;","int<lower=0> W;","int<lower=0> V;","int<lower=0> UF;",
    "matrix[W,H] share_design;","array[T] int<lower=1,upper=J> share_target;",
    "array[T] int<lower=1,upper=W> share_weight_start;","array[T] int<lower=1> share_weight_count;",
    "array[W] int<lower=1,upper=T> share_weight_total_sd;",
    "array[V] int<lower=1,upper=J> share_cell_target;","array[V] int<lower=1,upper=K> share_cell_component;",
    "array[V] int<lower=1,upper=W> share_cell_weight;","vector[V] share_cell_multiplier;",
    "array[T] int<lower=4,upper=5> total_sd_dist;","vector<lower=0>[T] total_sd_scale;","vector<lower=0>[T] total_sd_df;",
    "vector[H] share_beta_location;","vector<lower=0>[H] share_beta_scale;",
    "int<lower=0> B;","matrix[J,K] basis_multiplier;","array[K] int<lower=0,upper=B> basis_amplitude_index;",
    "array[B] int<lower=4,upper=5> amplitude_dist;","vector<lower=0>[B] amplitude_scale;","vector<lower=0>[B] amplitude_df;",
    "int<lower=0> L;","int<lower=0> A;","int<lower=0> F;","int<lower=0> E;","int<lower=0> U;","int<lower=0> EU;",
    "array[U] int<lower=1,upper=J> parameter_target;","array[U,U] int<lower=0,upper=1> parameter_free_mask;",
    "array[U] int<lower=0,upper=UF> parameter_sd_index;","array[U] int<lower=0,upper=W> parameter_share_weight;",
    "array[UF] int<lower=4,upper=5> parameter_sd_dist;","vector<lower=0>[UF] parameter_sd_scale;","vector<lower=0>[UF] parameter_sd_df;","real<lower=0> parameter_cor_eta;",
    "int<lower=0> LC;","array[LC] int<lower=1,upper=J> loading_cell_target;","array[LC] int<lower=1,upper=K> loading_cell_component;",
    "array[LC] int<lower=0,upper=L> loading_cell_primitive;","vector[LC] loading_cell_fixed;",
    "array[L] int<lower=0,upper=A> loading_anchor_index;","array[L] int<lower=0,upper=F> loading_free_index;",
    "array[K,K] int<lower=0,upper=1> correlation_free_mask;",
    "array[J] int<lower=1,upper=3> alpha_dist;","vector[J] alpha_location;","vector<lower=0>[J] alpha_scale;","vector<lower=0>[J] alpha_df;",
    "vector<lower=0>[L] loading_scale;","array[S] int<lower=4,upper=5> sd_dist;","vector<lower=0>[S] sd_scale;","vector<lower=0>[S] sd_df;","real<lower=0> cor_eta;")
  if(raw)declarations<-setdiff(declarations,"real<lower=0> cor_eta;")
  declarations<-c(declarations,reference_data$declarations,affine_data$declarations)
  data <- c(list(N=length(spec$data_map$subject_keys),J=index$J,K=index$K,S=index$S,O=oi$observation_count,
    CS=nrow(.bh_free_component_sd(spec)),component_sd_index=spec$tables$component_scales$primitive_index,
    B=nrow(.bh_amplitudes(spec)),basis_multiplier=spec$tables$basis_multiplier,basis_amplitude_index=spec$tables$basis_amplitude_index,
    mp_use_reduce_sum=0L,mp_grainsize=1L,L=nload,A=sum(positive),F=sum(!positive),E=.bh_cor_primitive_count(spec$tables$correlations),
    LC=nrow(cells),loading_cell_target=cells$target_index,loading_cell_component=cells$component_index,
    loading_cell_primitive=ifelse(cells$state=="free",cells$primitive_index,0L),loading_cell_fixed=ifelse(cells$state=="fixed",cells$fixed_value,0),
    loading_anchor_index=aid,loading_free_index=fid,
    U=index$U,EU=.bh_cor_primitive_count(spec$tables$parameter_correlations),parameter_target=spec$tables$parameter_residuals$target_index,
    parameter_free_mask=parameter_mask,correlation_free_mask=mask),share_data,.bh_prior_data(spec),oi$data)
  if(raw){data$E<-0L;data$cor_eta<-NULL}
  data<-c(data,reference_data$data,affine_data$data)
  data <- lapply(data,function(x)if(is.null(dim(x)))unname(x)else x)
  data <- .mp_stan_data_shapes(data,c(declarations,oi$declarations))
  parameters <- c("vector<lower=0>[CS] component_sd;","vector<lower=0>[T] total_sd;","vector[H] share_beta;","vector<lower=0>[B] amplitude;","vector[J] alpha;","vector<lower=0>[A] loading_anchor;","vector[F] loading_free;",
    "vector<lower=0>[S] obs_sd;",if(raw)"vector[RB] correlation_B;"else"vector[E] correlation_raw;",if(!task)"matrix[K,N] z_raw;",
    "vector<lower=0>[UF] parameter_sd;","vector[EU] parameter_correlation_raw;",if(!task)"matrix[U,N] e_raw;"else"array[JT] sum_to_zero_vector[N] z_task;")
  transformed <- c(if(!raw)"matrix[K+1,K] correlation_map=rep_matrix(0.0,K+1,K);",
    "matrix[U+1,U] parameter_correlation_map=rep_matrix(0.0,U+1,U);",
    if(!raw||!task)"matrix[K,K] L_component=diag_matrix(rep_vector(1.0,K));",if(raw)"matrix[K,K] F_component;","matrix[K,K] R_component;",
    "matrix[U,U] L_parameter=diag_matrix(rep_vector(1.0,U));","matrix[U,U] R_parameter;",
    if(raw)"real raw_prior_coordinate_log_jacobian=0;"else"real correlation_log_jacobian=0;","real parameter_correlation_log_jacobian=0;",
    "vector[K] component_row_sd=rep_vector(1.0,K);",if(task)"matrix[J,K] effective_loadings_work;"else"matrix[J,K] effective_loadings;",
    if(task)"matrix[J,K] Lambda_work=rep_matrix(0.0,J,K);"else"matrix[J,K] Lambda=rep_matrix(0.0,J,K);",if(task)"matrix[J,U] D_parameter_work=rep_matrix(0.0,J,U);"else"matrix[J,U] D_parameter=rep_matrix(0.0,J,U);","vector[L] loading;",
    if(!task)c("matrix[K,N] z;","matrix[U,N] e;","matrix[N,J] u;"),"matrix[N,J] eta;",
    if(task)c("vector[J] task_row_scale=rep_vector(1.0,J);","matrix[J,K] common_task_factor;","matrix[J,U] parameter_task_factor;",
      "matrix[J,J] Sigma_common_work;","matrix[J,J] Sigma_parameter_work;","matrix[J,J] Sigma_predictor_work;",
      "matrix[JT,JT] task_covariance_work;","matrix[JT,JT] L_task_work;","matrix[JT,N] task_white_scores;","matrix[JT,N] task_work;","matrix[N,J] task_delta=rep_matrix(0.0,N,J);"),
    "vector[W] share_logits=share_design*share_beta;","vector[W] share_weight=rep_vector(0.0,W);",if(task)"vector[U] parameter_row_sd_work=rep_vector(0.0,U);"else"vector[U] parameter_row_sd=rep_vector(0.0,U);",
    "if(T>0) for(t in 1:T) {",
    "  int first=share_weight_start[t]; int count=share_weight_count[t];",
    "  vector[count] logits=segment(share_logits,first,count); vector[count] weight;",
    "  for(p in 1:count) if(is_nan(logits[p])||is_inf(logits[p])) reject(\"share logit numeric overflow\");",
    "  weight=softmax(logits);",
    "  if(abs(sum(weight)-1)>1e-12) reject(\"share normalization lost numeric precision\");",
    "  for(p in 1:count) if(is_nan(weight[p])||is_inf(weight[p])||weight[p]<=0) reject(\"active share underflow or overflow; no clipping\");",
    "  share_weight[first:(first+count-1)]=weight;",
    "}",
    if(raw)c("F_component=bhmm_raw_block_factor(correlation_B,raw_block_size,raw_component_order);","R_component=tcrossprod(F_component);",
      if(!task)"if(K>0) L_component=cholesky_decompose(R_component);")else c(
      if(affine_engine)"if(K>0) { correlation_map=bmeas_corr_layer(correlation_raw,component_cor_mode,correlation_free_mask,component_cor_constant,component_cor_direction,component_cor_bounds,component_fixed_factor,component_cor_product_labels,component_cor_product_bounds);"else"if(K>0) { correlation_map=bhmm_corr_chart(correlation_raw,correlation_free_mask);",
      "  L_component=block(correlation_map,1,1,K,K); correlation_log_jacobian=correlation_map[K+1,1]; }"),
    if(affine_engine)"if(U>0) { parameter_correlation_map=bmeas_corr_layer(parameter_correlation_raw,parameter_cor_mode,parameter_free_mask,parameter_cor_constant,parameter_cor_direction,parameter_cor_bounds,parameter_fixed_factor,parameter_cor_product_labels,parameter_cor_product_bounds);"else"if(U>0) { parameter_correlation_map=bhmm_corr_chart(parameter_correlation_raw,parameter_free_mask);",
    "  L_parameter=block(parameter_correlation_map,1,1,U,U); parameter_correlation_log_jacobian=parameter_correlation_map[U+1,1]; }",
    if(raw)"R_parameter=tcrossprod(L_parameter);"else"R_component=tcrossprod(L_component); R_parameter=tcrossprod(L_parameter);",
    if(affine_engine)c(if(!raw)c(
      "if(component_cor_mode==1) R_component=component_cor_constant;",
      "if(component_cor_mode==2) R_component=component_cor_constant+bmeas_interval_label(correlation_raw[1],component_cor_bounds)*component_cor_direction;",
      "if(component_cor_mode==3) { R_component=component_cor_constant; for(i in 1:K) for(j in 1:K) if(component_cor_product_labels[i,j]>0) { int label=component_cor_product_labels[i,j]; R_component[i,j]+=bmeas_interval_label(correlation_raw[label],to_vector(component_cor_product_bounds[label])); } }"),
      "if(parameter_cor_mode==1) R_parameter=parameter_cor_constant;",
      "if(parameter_cor_mode==2) R_parameter=parameter_cor_constant+bmeas_interval_label(parameter_correlation_raw[1],parameter_cor_bounds)*parameter_cor_direction;",
      "if(parameter_cor_mode==3) { R_parameter=parameter_cor_constant; for(i in 1:U) for(j in 1:U) if(parameter_cor_product_labels[i,j]>0) { int label=parameter_cor_product_labels[i,j]; R_parameter[i,j]+=bmeas_interval_label(parameter_correlation_raw[label],to_vector(parameter_cor_product_bounds[label])); } }"),
    if(!task)"z=L_component*z_raw; e=L_parameter*e_raw;",
    .bh_coefficient_transform_stan(task),
    if(task).bh_task_transform_stan(raw)else c("u=transpose(e)*transpose(D_parameter);",
      "eta=rep_matrix(to_row_vector(alpha),N)+transpose(z)*transpose(effective_loadings)+u;"))
  prior <- c("if(CS>0) for(c in 1:CS) {",
    "  if(component_sd_dist[c]==4) component_sd[c] ~ normal(0,component_sd_scale[c]);",
    "  else component_sd[c] ~ student_t(component_sd_df[c],0,component_sd_scale[c]);","}",
    "if(H>0) share_beta ~ normal(share_beta_location,share_beta_scale);",
    "if(T>0) for(t in 1:T) {",
    "  if(total_sd_dist[t]==4) total_sd[t] ~ normal(0,total_sd_scale[t]);",
    "  else total_sd[t] ~ student_t(total_sd_df[t],0,total_sd_scale[t]);","}",
    "if(J>0) for(j in 1:J) {",
    "  if(alpha_dist[j]==1) alpha[j] ~ normal(alpha_location[j],alpha_scale[j]);",
    "  else if(alpha_dist[j]==2) alpha[j] ~ logistic(alpha_location[j],alpha_scale[j]);",
    "  else alpha[j] ~ student_t(alpha_df[j],alpha_location[j],alpha_scale[j]);","}",
    "if(L>0) loading ~ normal(0,loading_scale);",
    "if(B>0) for(b in 1:B) {",
    "  if(amplitude_dist[b]==4) amplitude[b] ~ normal(0,amplitude_scale[b]);",
    "  else amplitude[b] ~ student_t(amplitude_df[b],0,amplitude_scale[b]);","}",
    "if(S>0) for(s in 1:S) {",
    "  if(sd_dist[s]==4) obs_sd[s] ~ normal(0,sd_scale[s]);",
    "  else obs_sd[s] ~ student_t(sd_df[s],0,sd_scale[s]);","}",
    "if(UF>0) for(r in 1:UF) {",
    "  if(parameter_sd_dist[r]==4) parameter_sd[r] ~ normal(0,parameter_sd_scale[r]);",
    "  else parameter_sd[r] ~ student_t(parameter_sd_df[r],0,parameter_sd_scale[r]);","}",
    if(task)"if(JT>0) for(j in 1:JT) z_task[j] ~ normal(0,sqrt(1.0*N/(N-1)));"else c("if(K>0) to_vector(z_raw) ~ std_normal();","if(U>0) to_vector(e_raw) ~ std_normal();"),
    if(raw)c("if(RB>0) correlation_B ~ normal(0,raw_cor_scale);","target += parameter_correlation_log_jacobian;")else c(
      "target += correlation_log_jacobian + parameter_correlation_log_jacobian;",
      if(affine_engine)"if(E>0) target += 2*(cor_eta-1)*sum(log(diagonal(L_component)));"else"if(K>0) target += 2*(cor_eta-1)*sum(log(diagonal(L_component)));"),
    if(affine_engine)"if(EU>0) target += 2*(parameter_cor_eta-1)*sum(log(diagonal(L_parameter)));"else"if(U>0) target += 2*(parameter_cor_eta-1)*sum(log(diagonal(L_parameter)));")
  initialization<-.bh_initialization(spec)
  list(index=index,data=data,representation_projection=list(from=spec$schema_version,to=if(affine_engine)"affine-correlation-lowering-v1"else if(.bh_reference_mode(spec))"reference-law-lowering-v1"else"marker-scale-lowering-v1",empty_owner=!.bh_has_shares(spec),scientific_change=FALSE),observation=oi,declarations=declarations,parameters=parameters,transformed=transformed,prior=prior,
    initialization=initialization,init=.bh_materialize_init(spec,initialization))
}
stancode <- function(object, file=NULL, threads=1L, log_lik=FALSE) {
  .bh_pf_refuse_execution(object,"stancode")
  threads <- .bh_backend_integer(threads,"threads")
  if(!is.logical(log_lik)||length(log_lik)!=1L||is.na(log_lik)).mp_abort("E_ARGUMENT","log_lik must be explicit logical")
  if(inherits(object,"measurement_spec")) {
    if(threads>2L).mp_abort("E_RUN_BUDGET","legacy development specifications allow at most two threads")
    if(log_lik).mp_abort("E_FRONTEND_UNSUPPORTED","use the historical verification_outputs() contract explicitly for a historical spec")
    return(make_stan_code(object,file,threaded=threads>1L))
  }
  object<-.bh_normalize_spec(object)
  ir <- .bh_backend_ir(object,threads>1L);oi <- ir$observation
  resources <- .mp_adapter_resources(object);wrappers<-list()
  for(b in object$models)if(!is.null(b$model$adapter)&&length(b$model$adapter$function_code))wrappers[[b$model$adapter$wrapper_hash]]<-b$model$adapter$function_code
  generated<-if(.bh_task_augmentation(object)).bh_task_generated()else .bh_marker_generated()
  reference<-.bh_reference_mode(object);raw<-.bh_raw_cor(object);task<-.bh_task_augmentation(object)
  affine_engine<-.bh_affine_engine(object);affine_transformed<-if(affine_engine).bh_affine_transformed_data_stan(raw)else list(declarations=character(),statements=character())
  code <- c("// bhmm component loading-expression generator; no scientific G/P/C names are special.",
    if(raw)"// Common correlations have direct Normal B priors; applied prior-coordinate Jacobian zero, geometric B-to-R Jacobian not applied."else if(affine_engine)"// Correlation density is det(R)^(eta-1) on unique free marginal labels, plus one chart Jacobian per free layer."else"// Correlation density is det(R)^(eta-1) on final free marginal entries, plus the reduced chart Jacobian once.",
    "functions {",vapply(resources,function(r)paste0("#include ",r$cache_name),character(1)),unlist(wrappers,use.names=FALSE),.bh_chart_stan(),if(affine_engine).bh_affine_chart_stan(),if(raw).bh_raw_block_stan(),oi$functions,"}",
    "data {",if(task)"int<lower=2> N;"else"int<lower=1> N;","int<lower=0> J;","int<lower=0> K;","int<lower=0> S;","int<lower=1> O;",
    "int<lower=0,upper=1> mp_use_reduce_sum;","int<lower=1> mp_grainsize;",ir$declarations,oi$declarations,"}",
    if(reference||affine_engine)c("transformed data {",affine_transformed$declarations,
      if(threads==1L)'if(mp_use_reduce_sum==1) reject("reduced likelihood needs threaded compilation");',
      if(reference).bh_reference_transformed_data(object),affine_transformed$statements,"}")else
      if(threads==1L)c("transformed data {",'if(mp_use_reduce_sum==1) reject("reduced likelihood needs threaded compilation");',"}"),
    "parameters {",ir$parameters,"}","transformed parameters {",ir$transformed,"}",
    "model {",ir$prior,oi$likelihood,"}","generated quantities {",generated$declarations,
    if(log_lik)"vector[O] log_lik;",generated$statements,
    if(log_lik)oi$log_lik,"}")
  text <- paste(code,collapse="\n")
  if(!is.null(file)){.mp_scalar_string(file,"Stan file");writeLines(text,file)}
  result<-structure(text,class=c("bhmm_stan_code","character"),backend_status="generated_not_numerically_qualified",representation_projection=ir$representation_projection)
  if(!is.null(object$schema_migration))attr(result,"schema_migration")<-object$schema_migration
  result
}
standata <- function(object, threads=1L, reduction="serial", grainsize=1L) {
  .bh_pf_refuse_execution(object,"standata")
  threads <- .bh_backend_integer(threads,"threads")
  if(inherits(object,"measurement_spec")) {
    if(threads>2L).mp_abort("E_RUN_BUDGET","legacy development specifications allow at most two threads")
    return(stan_data(object,reduction,grainsize,threaded=threads>1L))
  }
  object<-.bh_normalize_spec(object)
  grainsize<-.bh_backend_integer(grainsize,"grainsize")
  .mp_reduction(reduction,grainsize,development=FALSE)
  if(reduction!="serial"&&threads==1L).mp_abort("E_THREADING_UNSUPPORTED","reduced data require threads>1 for a threaded compiler")
  ir <- .bh_backend_ir(object,threads>1L);data<-ir$data
  data$mp_use_reduce_sum<-as.integer(reduction!="serial");data$mp_grainsize<-as.integer(grainsize)
  attr(data,"representation_projection")<-ir$representation_projection
  if(!is.null(object$schema_migration))attr(data,"schema_migration")<-object$schema_migration
  data
}
