.mp_gaussian_stan_data <- function(spec) {
  check_measurement(spec)
  indicators <- unlist(spec$structure$factors, use.names = FALSE)
  factor_names <- names(spec$structure$factors); K <- length(factor_names); J <- length(indicators)
  factor_id <- rep(seq_len(K), lengths(spec$structure$factors))
  anchors <- match(unname(spec$structure$positive_anchors), indicators)
  free <- setdiff(seq_len(J), anchors)
  anchor_id <- free_id <- integer(J)
  anchor_id[anchors] <- seq_len(K); free_id[free] <- seq_along(free)
  blocks <- spec$data_map$blocks
  obs <- do.call(rbind, lapply(names(blocks), function(b) {
    d <- blocks[[b]]
    data.frame(person = d$.person_index, indicator = match(paste(b, "mu", d$.task, sep = "::"), indicators), y = d$y)
  }))
  p <- spec$priors
  list(N = length(spec$data_map$subject_keys), J = J, K = K, O = nrow(obs),
       person = as.integer(obs$person), indicator = as.integer(obs$indicator), y = obs$y,
       factor_id = as.integer(factor_id), anchor_id = anchor_id, free_id = free_id,
       alpha_mean = p$intercept$mean, alpha_sd = p$intercept$sd,
       loading_sd = p$loading$sd, sigma_sd = p$sigma$sd, lkj_shape = p$correlation$shape)
}
make_stan_code <- function(spec, file = NULL, threaded = FALSE) {
  check_measurement(spec)
  .mp_threaded_flag(spec,threaded)
  # One controlled template. Public keys are mapped separately, never substituted into Stan symbols.
  code <- if(.mp_registry_mode(spec)) .mp_registry_code(spec,threaded) else if (.mp_legacy_gaussian(spec)) .mp_gaussian_template() else .mp_basic_template()
  if (!is.null(file)) {
    .mp_scalar_string(file, "Stan destination"); writeLines(code, file, useBytes = TRUE)
  }
  structure(paste(code, collapse = "\n"), class = c("measurement_stan_code", "character"),
            symbol_map = stan_symbol_map(spec), code_hash = .mp_hash(code))
}
stan_data <- function(spec, reduction="serial", grainsize=1L, threaded=FALSE) {
  check_measurement(spec); .mp_threaded_flag(spec,threaded)
  .mp_reduction(reduction,grainsize)
  if(reduction!="serial"&&!threaded) .mp_abort("E_THREADING_UNSUPPORTED","reduced data require an explicitly threaded compilation")
  if(.mp_registry_mode(spec)) {
    d<-.mp_registry_ir(spec,threaded)$data
    d$mp_use_reduce_sum<-as.integer(reduction=="reduce_sum_static");d$mp_grainsize<-as.integer(grainsize);d
  } else if (.mp_legacy_gaussian(spec)) .mp_gaussian_stan_data(spec) else .mp_basic_stan_data(spec)
}
.mp_indicator_info <- function(spec) {
  keys <- unlist(spec$structure$factors, use.names = FALSE)
  parts <- strsplit(keys, "::", fixed = TRUE)
  out <- data.frame(key = keys, block = vapply(parts, `[`, character(1), 1L),
    parameter = vapply(parts, `[`, character(1), 2L), task = vapply(parts, `[`, character(1), 3L),
    factor_id = rep(seq_along(spec$structure$factors), lengths(spec$structure$factors)), stringsAsFactors = FALSE)
  out$family <- vapply(out$block, function(b) spec$models[[b]]$model$name, character(1))
  out$family_id <- vapply(out$family, function(f) .mp_family_info(f)$family_id, integer(1))
  out$nuisance <- vapply(out$family, function(f) {n <- .mp_family_info(f)$nuisance; if(length(n)) n else NA_character_}, character(1))
  out$sd_id <- 0L; out$sd_id[!is.na(out$nuisance)] <- seq_len(sum(!is.na(out$nuisance)))
  out
}
stan_symbol_map <- function(spec) {
  check_measurement(spec); if(.mp_registry_mode(spec)) return(.mp_registry_symbol_map(spec))
  info <- .mp_indicator_info(spec); J <- nrow(info)
  out <- data.frame(key=info$key,intercept=paste0("alpha[",seq_len(J),"]"),loading=paste0("lambda[",seq_len(J),"]"),
    observation_sd = NA_character_, observation_sd_key = NA_character_, nuisance_parameter=info$nuisance,
    predictor_scale=vapply(info$family,function(f).mp_family_info(f)$scale,character(1)),stringsAsFactors=FALSE)
  has_sd <- !is.na(info$nuisance)
  out$observation_sd[has_sd] <- paste0(if(.mp_legacy_gaussian(spec)) "sigma[" else "obs_sd[",info$sd_id[has_sd],"]")
  out$observation_sd_key[has_sd] <- paste(info$block[has_sd],info$nuisance[has_sd],info$task[has_sd],sep="::")
  out
}
.mp_basic_stan_data <- function(spec) {
  info <- .mp_indicator_info(spec); J <- nrow(info); K <- length(spec$structure$factors)
  anchor_positions <- match(unname(spec$structure$positive_anchors), info$key)
  anchor_id <- free_id <- integer(J)
  anchor_id[anchor_positions] <- seq_len(K)
  free_id[-anchor_positions] <- seq_len(J-K)
  cont <- list(); bin <- list()
  for (b in names(spec$models)) {
    block <- spec$models[[b]]; d <- spec$data_map$blocks[[b]]
    key <- paste(b,.mp_family_info(block$model$name)$primary,d$.task,sep="::")
    map <- data.frame(person=d$.person_index,indicator=match(key,info$key))
    if (block$model$name == "binomial") {
      map$successes <- as.integer(d$successes); map$trials <- as.integer(d$trials); bin[[b]] <- map
    } else { map$y <- d$y; cont[[b]] <- map }
  }
  continuous <- if(length(cont)) do.call(rbind,cont) else data.frame(person=integer(),indicator=integer(),y=numeric())
  counts <- if(length(bin)) do.call(rbind,bin) else data.frame(person=integer(),indicator=integer(),successes=integer(),trials=integer())
  priors <- .mp_resolve_priors(spec)
  alpha_mean <- vapply(info$block,function(b)priors[[b]]$intercept$mean,numeric(1))
  alpha_sd <- vapply(info$block,function(b)priors[[b]]$intercept$sd,numeric(1))
  loading_sd <- vapply(info$block,function(b)priors[[b]]$loading$sd,numeric(1))
  scale_priors <- vapply(info$block[info$sd_id>0],function(b)priors[[b]]$observation_sd$sd,numeric(1))
  list(N=length(spec$data_map$subject_keys),J=J,K=K,O=nrow(continuous)+nrow(counts),S=sum(info$sd_id>0),
    O_cont=nrow(continuous),person_cont=as.integer(continuous$person),indicator_cont=as.integer(continuous$indicator),y=continuous$y,
    O_bin=nrow(counts),person_bin=as.integer(counts$person),indicator_bin=as.integer(counts$indicator),successes=counts$successes,trials=counts$trials,
    family_id=info$family_id,sd_id=info$sd_id,factor_id=info$factor_id,anchor_id=anchor_id,free_id=free_id,
    alpha_mean=unname(alpha_mean),alpha_sd=unname(alpha_sd),loading_sd=unname(loading_sd),sd_prior=unname(scale_priors),lkj_shape=spec$priors$correlation$shape)
}
.mp_basic_template <- function() strsplit('// Mixed basic-family simple CFA; original source, MIT.
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
}', "\n", fixed=TRUE)[[1L]]
.mp_gaussian_template <- function() strsplit('// Gaussian simple CFA, original prototype template, MIT.
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
}', "\n", fixed = TRUE)[[1L]]
