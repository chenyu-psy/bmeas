// Gaussian simple CFA, original prototype template, MIT.
// eta_ij = alpha_j + lambda_j * z_i,f(j); tau_mu = 0.
// z_i ~ MVN(0, Omega); y_o ~ normal(eta_i,j, sigma_j).
// Positive anchors identify factor directions; other loadings are unconstrained.
// Priors for sigma and anchor loadings are on their constrained original scales.
// No log_lik or posterior-predictive array is generated.
data {
  int<lower=1> N;
  int<lower=3> J;
  int<lower=1> K;
  int<lower=1> O;
  array[O] int<lower=1, upper=N> person;
  array[O] int<lower=1, upper=J> indicator;
  vector[O] y;
  array[J] int<lower=1, upper=K> factor_id;
  array[J] int<lower=0, upper=K> anchor_id;
  array[J] int<lower=0, upper=J-K> free_id;
  real alpha_mean;
  real<lower=0> alpha_sd;
  real<lower=0> loading_sd;
  real<lower=0> sigma_sd;
  real<lower=0> lkj_shape;
}
parameters {
  vector[J] alpha;
  vector<lower=0>[K] loading_anchor;
  vector[J-K] loading_free;
  vector<lower=0>[J] sigma;
  cholesky_factor_corr[K] L_Omega;
  matrix[K, N] z_raw;
}
transformed parameters {
  vector[J] lambda;
  matrix[K, N] z = L_Omega * z_raw;
  for (j in 1:J) {
    if (anchor_id[j] > 0) lambda[j] = loading_anchor[anchor_id[j]];
    else lambda[j] = loading_free[free_id[j]];
  }
}
model {
  alpha ~ normal(alpha_mean, alpha_sd);
  loading_anchor ~ normal(0, loading_sd);
  loading_free ~ normal(0, loading_sd);
  sigma ~ normal(0, sigma_sd);
  L_Omega ~ lkj_corr_cholesky(lkj_shape);
  to_vector(z_raw) ~ std_normal();
  for (o in 1:O) {
    int j = indicator[o];
    y[o] ~ normal(alpha[j] + lambda[j] * z[factor_id[j], person[o]], sigma[j]);
  }
}
generated quantities {
  corr_matrix[K] Omega = multiply_lower_tri_self_transpose(L_Omega);
  matrix[J, K] Lambda = rep_matrix(0.0, J, K);
  matrix[J, J] Sigma_latent;
  matrix[J, J] Sigma_observed;
  for (j in 1:J) Lambda[j, factor_id[j]] = lambda[j];
  Sigma_latent = Lambda * Omega * transpose(Lambda);
  Sigma_observed = add_diag(Sigma_latent, square(sigma));
}
