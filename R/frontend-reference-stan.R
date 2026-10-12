# Generic lowering fragments selected by explicit covariance coordinates and
# augmentation, never by component names or an experiment-specific graph.
.bh_reference_lowering_available <- function() TRUE
.bh_reference_generator_qualified <- function() TRUE
.bh_reference_prior_data <- function(spec) {
  if(!.bh_raw_cor(spec))return(list())
  ids<-spec$tables$raw_correlation_coordinates$parameter
  priors<-spec$tables$priors$prior[match(ids,spec$tables$priors$parameter)]
  scale<-vapply(priors,function(p){d<-.bh_prior_distribution(p);if(d$distribution!="normal"||d$values[1L]!=0).mp_abort("E_PRIOR_COORDINATES","raw B lowering requires its resolved zero-centered Normal priors");d$values[2L]},numeric(1))
  list(raw_cor_scale=unname(scale))
}
.bh_reference_backend_data <- function(spec) {
  declarations<-character();data<-list()
  if(.bh_raw_cor(spec)) {
    declarations<-c(declarations,"int<lower=0> RB;","int<lower=0> BR;","array[BR] int<lower=1> raw_block_size;",
      "array[K] int<lower=1,upper=K> raw_component_order;","vector<lower=0>[RB] raw_cor_scale;")
    data<-c(data,list(RB=nrow(spec$tables$raw_correlation_coordinates),BR=length(spec$formula$correlation_coordinates$blocks),
      raw_block_size=as.integer(lengths(spec$formula$correlation_coordinates$blocks)),raw_component_order=spec$formula$correlation_coordinates$ordered_indices))
  }
  if(.bh_task_augmentation(spec)) {
    declarations<-c(declarations,"int<lower=0> JT;","array[JT] int<lower=1,upper=J> task_target;","array[J] int<lower=0,upper=T> target_total_sd_index;")
    owner_index<-integer(nrow(spec$tables$targets))
    if(.bh_has_shares(spec))owner_index[spec$tables$share_targets$target_index]<-spec$tables$share_targets$total_sd_index
    data<-c(data,list(JT=length(spec$tables$augmentation$target),task_target=spec$tables$augmentation$target_index,target_total_sd_index=owner_index))
  }
  list(declarations=declarations,data=data)
}
.bh_raw_block_stan <- function()c(
  "matrix bhmm_raw_block_factor(vector raw_B, array[] int block_size, array[] int component_order) {",
  "  int K=size(component_order); int M=size(block_size); int previous=0; int raw_position=1;",
  "  matrix[K,K] L_ordered=diag_matrix(rep_vector(1.0,K)); matrix[K,K] F_global=rep_matrix(0.0,K,K);",
  "  if(M>0) for(m in 1:M) {",
  "    int width=block_size[m];",
  "    if(previous>0) {",
  "      matrix[width,previous] raw_block=to_matrix(segment(raw_B,raw_position,width*previous),width,previous);",
  "      matrix[width,width] gram=diag_matrix(rep_vector(1.0,width))+tcrossprod(raw_block);",
  "      matrix[width,width] factor=cholesky_decompose(gram);",
  "      L_ordered[(previous+1):(previous+width),1:previous]=mdivide_left_tri_low(factor,raw_block);",
  "      L_ordered[(previous+1):(previous+width),(previous+1):(previous+width)]=mdivide_left_tri_low(factor,diag_matrix(rep_vector(1.0,width)));",
  "      raw_position+=width*previous;",
  "    }",
  "    previous+=width;",
  "  }",
  "  if(K>0) for(k in 1:K) F_global[component_order[k]]=L_ordered[k];",
  "  return F_global;",
  "}")
.bh_task_transform_stan <- function(raw) c(
  "if(J>0) for(j in 1:J) {",
  "  if(target_total_sd_index[j]>0) task_row_scale[j]=total_sd[target_total_sd_index[j]];",
  "  if(is_nan(task_row_scale[j])||is_inf(task_row_scale[j])||task_row_scale[j]<=0) reject(\"required task row scale cannot be represented\");",
  "}",
  paste0("common_task_factor=effective_loadings_work*",if(raw)"F_component"else"L_component",";"),
  "parameter_task_factor=D_parameter_work*L_parameter;",
  "Sigma_common_work=tcrossprod(common_task_factor); Sigma_parameter_work=tcrossprod(parameter_task_factor);",
  "Sigma_predictor_work=Sigma_common_work+Sigma_parameter_work;",
  "if(JT>0) {",
  "  for(j in 1:JT) {",
  "    task_white_scores[j]=to_row_vector(z_task[j]);",
  "    for(k in 1:JT) task_covariance_work[j,k]=Sigma_predictor_work[task_target[j],task_target[k]];",
  "  }",
  "  L_task_work=cholesky_decompose(task_covariance_work);",
  "  task_work=L_task_work*task_white_scores;",
  "  for(j in 1:JT) for(n in 1:N) {",
  "    real value=task_row_scale[task_target[j]]*task_work[j,n];",
  "    if(is_nan(value)||is_inf(value)||(task_work[j,n]!=0 && value==0)) reject(\"required task deviation cannot be represented; no clipping\");",
  "    task_delta[n,task_target[j]]=value;",
  "  }",
  "}",
  "eta=rep_matrix(to_row_vector(alpha),N)+task_delta;",
  "if(J>0) for(j in 1:J) for(n in 1:N) if(is_nan(eta[n,j])||is_inf(eta[n,j])) reject(\"required task predictor is nonfinite\");")
.bh_task_generated <- function() {
  declarations<-c("matrix[J,K] Lambda;","matrix[J,K] effective_loadings;","matrix[J,U] D_parameter;","vector[U] parameter_row_sd;",
    "array[J,K] int<lower=0,upper=1> Lambda_available;","array[J,K] int<lower=0,upper=1> effective_loadings_available;","array[J,U] int<lower=0,upper=1> D_parameter_available;",
    "vector[K] log_component_row_sd=log(component_row_sd);","matrix[K,K] component_covariance;","matrix[K,K] component_covariance_log_abs;","array[K,K] int<lower=0,upper=1> component_covariance_available;",
    "matrix[J,J] Sigma_common;","matrix[J,J] Sigma_parameter;","matrix[J,J] Sigma_predictor;",
    "array[J,J] int<lower=0,upper=1> Sigma_common_available;","array[J,J] int<lower=0,upper=1> Sigma_parameter_available;","array[J,J] int<lower=0,upper=1> Sigma_predictor_available;",
    "matrix[J,J] Sigma_predictor_log_abs;","matrix[JT,N-1] task_white_unconstrained;")
  statements<-c("if(J>0) for(j in 1:J) {",
    "  if(K>0) for(k in 1:K) {",
    "    real declared=task_row_scale[j]*Lambda_work[j,k]; real effective=task_row_scale[j]*effective_loadings_work[j,k];",
    "    Lambda_available[j,k]=!(is_nan(declared)||is_inf(declared)||(Lambda_work[j,k]!=0 && declared==0));",
    "    effective_loadings_available[j,k]=!(is_nan(effective)||is_inf(effective)||(effective_loadings_work[j,k]!=0 && effective==0));",
    "    Lambda[j,k]=Lambda_available[j,k] ? declared : not_a_number();",
    "    effective_loadings[j,k]=effective_loadings_available[j,k] ? effective : not_a_number();",
    "  }",
    "  if(U>0) for(r in 1:U) {",
    "    real value=task_row_scale[j]*D_parameter_work[j,r];",
    "    D_parameter_available[j,r]=!(is_nan(value)||is_inf(value)||(D_parameter_work[j,r]!=0 && value==0));",
    "    D_parameter[j,r]=D_parameter_available[j,r] ? value : not_a_number();",
    "  }",
    "  for(k in 1:J) {")
  for(name in c("Sigma_common","Sigma_parameter","Sigma_predictor"))statements<-c(statements,
    "    {",paste0("      real original=",name,"_work[j,k];"),
    "      real logarithm=log(abs(original))+log(task_row_scale[j])+log(task_row_scale[k]);",
    "      real value=original*task_row_scale[j]*task_row_scale[k];",
    "      if(is_nan(value)||is_inf(value)||(original!=0 && value==0)) value=(original<0 ? -1.0 : (original>0 ? 1.0 : 0.0))*exp(logarithm);",
    paste0("      ",name,"_available[j,k]=!(is_nan(value)||is_inf(value)||(original!=0 && value==0));"),
    paste0("      ",name,"[j,k]=",name,"_available[j,k] ? value : not_a_number();"),
    if(name=="Sigma_predictor")"      Sigma_predictor_log_abs[j,k]=logarithm;","    }")
  statements<-c(statements,"  }","}","if(U>0) for(r in 1:U) parameter_row_sd[r]=D_parameter[parameter_target[r],r];",
    "if(K>0) for(k in 1:K) for(l in 1:K) {",
    "  real value;",
    "  component_covariance_log_abs[k,l]=log(abs(R_component[k,l]))+log_component_row_sd[k]+log_component_row_sd[l];",
    "  value=(R_component[k,l]<0 ? -1.0 : (R_component[k,l]>0 ? 1.0 : 0.0))*exp(component_covariance_log_abs[k,l]);",
    "  component_covariance_available[k,l]=!(is_nan(value)||is_inf(value)||(R_component[k,l]!=0 && value==0));",
    "  component_covariance[k,l]=component_covariance_available[k,l] ? value : not_a_number();",
    "}","if(JT>0) for(j in 1:JT) {",
    "  real prefix=z_task[j][1];",
    "  for(k in 1:(N-1)) { task_white_unconstrained[j,k]=(prefix-k*z_task[j][k+1])/sqrt(1.0*k*(k+1)); prefix+=z_task[j][k+1]; }",
    "}")
  list(declarations=declarations,statements=statements)
}
.bh_reference_transformed_data <- function(spec) {
  out<-character()
  if(.bh_raw_cor(spec))out<-c(out,"{ int previous=0; int expected=0; array[K] int seen=rep_array(0,K);",
    "  if(BR>0) for(b in 1:BR) { expected+=previous*raw_block_size[b]; previous+=raw_block_size[b]; }",
    "  if(previous!=K || expected!=RB) reject(\"raw block dimensions disagree with the unique primitive map\");",
    "  if(K>0) for(k in 1:K) { if(seen[raw_component_order[k]]) reject(\"raw component order must be a permutation\"); seen[raw_component_order[k]]=1; }","}")
  if(.bh_task_augmentation(spec))out<-c(out,"{ array[J] int seen=rep_array(0,J);",
    "  if(JT>0) for(t in 1:JT) { if(seen[task_target[t]]) reject(\"task target order contains a duplicate\"); seen[task_target[t]]=1; }","}")
  out
}
