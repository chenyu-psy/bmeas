// Mixed basic-family simple CFA; original source, MIT.
// Shared subject z_i ~ MVN(0, Omega), eta_ij=alpha_j+lambda_j*z_i,f(j).
// Gaussian: mu=eta, sigma=obs_sd. Lognormal: meanlog=eta, sdlog=obs_sd.
// Binomial: logit(p)=eta; binomial_logit avoids redundant/unstable transformations.
// No extra task-specific latent residuals. No raw-scale mixed covariance claim.
// No pointwise log_lik or y_rep arrays; independent serial observation likelihoods.
data {
  int<lower=1> N;
  int<lower=3> J;
  int<lower=1> K;
  int<lower=1> O;
  int<lower=0> S;
  int<lower=0> O_cont;
  array[O_cont] int<lower=1,upper=N> person_cont;
  array[O_cont] int<lower=1,upper=J> indicator_cont;
  vector[O_cont] y;
  int<lower=0> O_bin;
  array[O_bin] int<lower=1,upper=N> person_bin;
  array[O_bin] int<lower=1,upper=J> indicator_bin;
  array[O_bin] int<lower=0> successes;
  array[O_bin] int<lower=0> trials;
  array[J] int<lower=1,upper=3> family_id;
  array[J] int<lower=0,upper=S> sd_id;
  array[J] int<lower=1,upper=K> factor_id;
  array[J] int<lower=0,upper=K> anchor_id;
  array[J] int<lower=0,upper=J-K> free_id;
  vector[J] alpha_mean;
  vector<lower=0>[J] alpha_sd;
  vector<lower=0>[J] loading_sd;
  vector<lower=0>[S] sd_prior;
  real<lower=0> lkj_shape;
}
parameters {
  vector[J] alpha;
  vector<lower=0>[K] loading_anchor;
  vector[J-K] loading_free;
  vector<lower=0>[S] obs_sd;
  cholesky_factor_corr[K] L_Omega;
  matrix[K,N] z_raw;
}
transformed parameters {
  vector[J] lambda;
  matrix[K,N] z = L_Omega * z_raw;
  for(j in 1:J) {
    if(anchor_id[j]>0) lambda[j]=loading_anchor[anchor_id[j]];
    else lambda[j]=loading_free[free_id[j]];
  }
}
model {
  alpha ~ normal(alpha_mean,alpha_sd);
  lambda ~ normal(0,loading_sd);
  obs_sd ~ normal(0,sd_prior);
  L_Omega ~ lkj_corr_cholesky(lkj_shape);
  to_vector(z_raw) ~ std_normal();
  if(O_cont>0) for(o in 1:O_cont) {
    int j=indicator_cont[o];
    real eta=alpha[j]+lambda[j]*z[factor_id[j],person_cont[o]];
    if(family_id[j]==1) target += normal_lpdf(y[o] | eta,obs_sd[sd_id[j]]);
    else target += lognormal_lpdf(y[o] | eta,obs_sd[sd_id[j]]);
  }
  if(O_bin>0) for(o in 1:O_bin) {
    int j=indicator_bin[o];
    real eta=alpha[j]+lambda[j]*z[factor_id[j],person_bin[o]];
    target += binomial_logit_lpmf(successes[o] | trials[o],eta);
  }
}
generated quantities {
  corr_matrix[K] Omega=multiply_lower_tri_self_transpose(L_Omega);
  matrix[J,K] Lambda=rep_matrix(0.0,J,K);
  matrix[J,J] Sigma_predictor;
  for(j in 1:J) Lambda[j,factor_id[j]]=lambda[j];
  Sigma_predictor=Lambda*Omega*transpose(Lambda);
}
