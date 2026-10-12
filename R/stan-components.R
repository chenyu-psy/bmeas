.mp_component_structure_ir <- function(spec) {
  s<-spec$structure;p<-spec$priors;i<-.mp_registry_indices(spec);J<-i$J;K<-i$K;N<-length(spec$data_map$subject_keys)
  data<-list(component_contrast=s$share_contrast,
    mu_dist=vapply(p$mean[s$keys],function(x)match(x$distribution,c("normal","logistic","student_t")),integer(1)),
    mu_location=vapply(p$mean[s$keys],function(x)x$mean,numeric(1)),mu_scale=vapply(p$mean[s$keys],function(x)x$sd,numeric(1)),
    mu_df=vapply(p$mean[s$keys],function(x)if(x$distribution=="student_t")x$df else 1,numeric(1)),
    tau_df=p$task_sd$df,tau_scale=p$task_sd$sd,base_sd=p$base$sd,par_sd=p$paradigm$sd,con_sd=p$content$sd,int_sd=p$interaction$sd,
    sd_prior=vapply(p$nuisance[i$nuisance],function(x)x$sd,numeric(1)))
  declarations<-c("matrix[4,3] component_contrast;","array[J] int<lower=1,upper=3> mu_dist;","vector[J] mu_location;","vector<lower=0>[J] mu_scale;","vector<lower=0>[J] mu_df;",
    "real<lower=0> tau_df;","real<lower=0> tau_scale;","real<lower=0> base_sd;","real<lower=0> par_sd;","real<lower=0> con_sd;","real<lower=0> int_sd;","vector<lower=0>[S] sd_prior;")
  parameters<-c("vector[J] mu;","vector<lower=0>[J] tau;","vector<lower=0>[S] obs_sd;")
  prior<-c("for(j in 1:J) {","if(mu_dist[j]==1)mu[j] ~ normal(mu_location[j],mu_scale[j]);","else if(mu_dist[j]==2)mu[j] ~ logistic(mu_location[j],mu_scale[j]);","else mu[j] ~ student_t(mu_df[j],mu_location[j],mu_scale[j]);","}",
    "tau ~ student_t(tau_df,0,tau_scale);","obs_sd ~ normal(0,sd_prior);")
  init<-list(mu=unname(data$mu_location),tau=rep(.5,J),obs_sd=rep(1,i$S),z_task=matrix(0,J,N))
  shares<-cross<-character();monitored<-c("mu","tau",if(i$S)"obs_sd");common_keys<-character();row0<-col0<-0L;coefficient_count<-0L
  for(fi in seq_along(s$families)) {
    name<-names(s$families)[fi];f<-s$families[[fi]];n<-length(f$keys);pd<-ncol(f$paradigm);cd<-ncol(f$content);id<-ncol(f$interaction);k<-s$common_dimensions[fi];prefix<-paste0("f",fi,"_")
    for(field in c("paradigm","content","interaction","paradigm_loadings","content_loadings")) {
      value<-f[[field]];v<-paste0(prefix,field);data[[v]]<-value;declarations<-c(declarations,paste0("matrix[",n,",",ncol(value),"] ",v,";"))
    }
    parameters<-c(parameters,paste0("vector[3] ",prefix,"base;"),paste0("matrix[3,",pd,"] ",prefix,"par;"),paste0("matrix[3,",cd,"] ",prefix,"con;"),paste0("matrix[3,",id,"] ",prefix,"int;"))
    init[[paste0(prefix,"base")]]<-rep(0,3);init[[paste0(prefix,"par")]]<-matrix(0,3,pd);init[[paste0(prefix,"con")]]<-matrix(0,3,cd);init[[paste0(prefix,"int")]]<-matrix(0,3,id)
    prior<-c(prior,paste0(prefix,"base ~ normal(0,base_sd);"),paste0("to_vector(",prefix,"par) ~ normal(0,par_sd);"),paste0("to_vector(",prefix,"con) ~ normal(0,con_sd);"),paste0("to_vector(",prefix,"int) ~ normal(0,int_sd);"))
    monitored<-c(monitored,paste0(prefix,c("base","par","con","int")))
    shares<-c(shares,paste0("for(t in 1:",n,") { int j=",row0,"+t;"),
      paste0("vector[3] predictor=",prefix,"base + ",prefix,"par*to_vector(",prefix,"paradigm[t]) + ",prefix,"con*to_vector(",prefix,"content[t]) + ",prefix,"int*to_vector(",prefix,"interaction[t]);"),
      "load_share[j]=to_row_vector(softmax(component_contrast*predictor));","load_std[j]=sqrt(load_share[j]);",
      paste0("Lambda[j,",col0+1L,"]=load_std[j,1];"),
      paste0("for(p in 1:",pd,")Lambda[j,",col0+1L,"+p]=load_std[j,2]*",prefix,"paradigm_loadings[t,p];"),
      paste0("for(c in 1:",cd,")Lambda[j,",col0+1L+pd,"+c]=load_std[j,3]*",prefix,"content_loadings[t,c];"),"residual_var[j]=load_share[j,4];","}")
    cc<-paste0(col0+1L,":",col0+k)
    if(fi==1L)cross<-c(cross,paste0("L_component[",cc,",",cc,"]=diag_matrix(rep_vector(1.0,",k,"));"))else {
      parameters<-c(parameters,paste0("matrix[",k,",",col0,"] ",prefix,"cross;"));init[[paste0(prefix,"cross")]]<-matrix(0,k,col0)
      data[[paste0(prefix,"cross_sd")]]<-p$cross[[name]]$sd;declarations<-c(declarations,paste0("real<lower=0> ",prefix,"cross_sd;"))
      prior<-c(prior,paste0("to_vector(",prefix,"cross) ~ normal(0,",prefix,"cross_sd);"));monitored<-c(monitored,paste0(prefix,"cross"))
      cross<-c(cross,"{",paste0("matrix[",k,",",k,"] C=cholesky_decompose(diag_matrix(rep_vector(1.0,",k,")) + ",prefix,"cross*transpose(",prefix,"cross));"),
        paste0("L_component[",cc,",1:",col0,"]=mdivide_left_tri_low(C,",prefix,"cross);"),
        paste0("L_component[",cc,",",cc,"]=mdivide_left_tri_low(C,diag_matrix(rep_vector(1.0,",k,")));"),"}")
      coefficient_count<-coefficient_count+k*col0
    }
    coefficient_count<-coefficient_count+3L*(1L+pd+cd+id)
    common_keys<-c(common_keys,paste0(name,"::",c("G",paste0("P",seq_len(pd)),paste0("C",seq_len(cd)))))
    row0<-row0+n;col0<-col0+k
  }
  parameters<-c(parameters,"array[J] sum_to_zero_vector[N] z_task;")
  prior<-c(prior,"for(j in 1:J)z_task[j] ~ normal(0,sqrt(1.0*N/(N-1)));")
  transformed<-c("matrix[N,J] eta;","matrix[N,J] delta;","matrix[J,4] load_share;","matrix[J,4] load_std;","matrix[J,4] load_raw;","matrix[J,K] Lambda=rep_matrix(0.0,J,K);","matrix[K,K] Omega_component;","matrix[J,J] Sigma_task;",
    "{","vector[J] residual_var;","matrix[K,K] L_component=rep_matrix(0.0,K,K);","matrix[J,K] task_common;","matrix[J,J] L_task;","matrix[J,N] Z;","matrix[N,J] task_std;",
    shares,cross,"Omega_component=L_component*transpose(L_component);","task_common=Lambda*L_component;","Sigma_task=add_diag(task_common*transpose(task_common),residual_var);","L_task=cholesky_decompose(Sigma_task);",
    "for(j in 1:J)Z[j]=to_row_vector(z_task[j]);","task_std=transpose(L_task*Z);","for(j in 1:J) { delta[,j]=tau[j]*task_std[,j]; eta[,j]=rep_vector(mu[j],N)+delta[,j]; }",
    "load_raw=diag_pre_multiply(tau,load_std);","}")
  family_ids<-rep(seq_along(s$families),s$common_dimensions)
  cross_cells<-which(outer(family_ids,family_ids,"!=")&lower.tri(matrix(0,K,K)),arr.ind=TRUE)
  correlation_monitors<-if(nrow(cross_cells))paste0("Omega_component[",cross_cells[,1],",",cross_cells[,2],"]")else character()
  structure(list(data=data,declarations=declarations,parameters=parameters,transformed=transformed,prior=prior,
    generated="matrix[J,J] Sigma_predictor=quad_form_diag(Sigma_task,tau);",init=init,monitored=c(monitored,"z_task",correlation_monitors),common_keys=common_keys,
    estimand="declared common-component correlation, task-predictor correlation/covariance, and centered individual eta; not generic CFA factors",
    min_subjects=2L,free_parameters=2L*J+coefficient_count+J*(N-1L)+i$S,
    estimated_output_reals=2L*J+coefficient_count+J*N+i$S+2L*N*J+12L*J+J*K+K*K+2L*J*J),class="measurement_structure_ir")
}
