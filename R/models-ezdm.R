# Independently authored from the first-exit Laplace transform; see EZDM_REVISED.md.
ezdm_model <- function(version = "revised_54a07b6_v1") {
  if (!identical(version, "revised_54a07b6_v1")) .mp_abort("E_MODEL_VERSION", "only pinned revised_54a07b6_v1 is implemented; historical bmm fits are not reproduced")
  model <- new_observation_model("ezdm_revised", version,
    response_schema(mean_rt=data_field("positive_real", units="second"),
      var_rt=data_field("positive_real", units="second_squared"),
      n_upper=data_field("nonnegative_integer", units="count"),
      n_trials=data_field("integer", units="count", lower=3),
      response=c("mean_rt", "var_rt", "n_upper")),
    list(drift=parameter_spec("real","identity","theta","subject_task","evidence_per_second","Signed diffusion drift",role="predictor"),
      bound=parameter_spec("positive","log","theta","subject_task","evidence","Total boundary separation",predictor_units="log_evidence",role="predictor"),
      ndt=parameter_spec("positive","log","theta","subject_task","second","Nondecision time",predictor_units="log_second",role="predictor")),
    stan_density(symbol="measurement_ez3_revised_lpdf", arguments=c("mean_rt","var_rt","n_upper","n_trials","drift","bound","ndt"),
      argument_types=c("real","real","int","int","real","real","real"), normalized=TRUE,
      approximation="Binomial-logit count; cumulant-matched Gamma sample variance; Normal mean conditional on sample variance", builtin=TRUE,license="MIT; independently authored from mathematical definitions"),
    model_capabilities(fit=TRUE,log_lik=TRUE),validate_data=.mp_validate_ez3,
    provenance=list(license="MIT independent implementation; no upstream implementation copied or translated",
      contract_commit="54a07b6fa34506bd14116d295eea9db234071f28",
      contract_url="https://github.com/popov-lab/bmm/tree/54a07b6fa34506bd14116d295eea9db234071f28",
      derivation="Centered Brownian first-exit Laplace transform; EZDM_REVISED.md",
      diffusion_sd=1,relative_start=0.5))
  model <- register_stan_adapter(model,function_code=.mp_ez3_stan_code(),
    function_exports=c("measurement_ez3_cumulants","measurement_ez3_revised_lpdf"))
  model$required_units <- "second"
  model$limitations <- "Approximate summary likelihood; independent nonoverlapping aggregates; revised snapshot, not historical-fit parity; bounded native density checks passed; arbitrary hierarchical/HMC qualification not claimed; no simulator/PPC"
  model
}
.mp_validate_ez3 <- function(data) {
  fields <- c("mean_rt","var_rt","n_upper","n_trials")
  if(any(vapply(data[fields],function(x)any(!is.finite(x)),logical(1))))
    .mp_abort("E_EZDM_DATA", "all four EZ summary fields must be finite and observed")
  if(any(data$mean_rt<=0 | data$var_rt<=0 | data$n_trials<3 | data$n_trials!=floor(data$n_trials) | data$n_upper<0 | data$n_upper>data$n_trials | data$n_upper!=floor(data$n_upper)))
    .mp_abort("E_EZDM_DATA", "positive mean and unbiased sample variance, integer n>=3, and integer 0<=n_upper<=n are required")
  TRUE
}
.mp_ez3_cumulants <- function(drift, bound) {
  if(length(drift)!=1L || length(bound)!=1L || !is.finite(drift) || !is.finite(bound) || bound<=0) stop("finite scalar drift and positive scalar bound required")
  h <- bound/2; x <- abs(h*drift)
  if(x<=0.5) {
    z <- x*x
    coefficients <- list(
      c(1,-0.33333333333333331,0.13333333333333333,-0.053968253968253971,0.021869488536155203,-0.0088632355299021973,0.0035921280365724811,-0.0014558343870513183,0.00059002744094558595,-0.00023912911424355248,9.6915379569294509e-05,-3.9278323883316833e-05,1.5918905069328964e-05,-6.4516892156554306e-06,2.6147711512907546e-06,-1.0597268320104654e-06,4.2949110782738057e-07,-1.7406618963571648e-07,7.0546369464009681e-08,-2.859136662305254e-08,1.1587644432798853e-08),
      c(0.66666666666666663,-0.53333333333333333,0.32380952380952382,-0.17495590828924162,0.08863235529902197,-0.043105536438869774,0.020381681418718454,-0.0094404390551293751,0.0043043240563839444,-0.0019383075913858901,0.00086412312543297039,-0.00038205372166389513,0.00016774391960704119,-7.3213592236141128e-05,3.1791804960313962e-05,-1.3743715450476178e-05,5.9182504476143599e-06,-2.5396693007043486e-06,1.0864719316759965e-06,-4.6350577731195406e-07,1.9724440672569787e-07),
      c(1.0666666666666667,-1.2952380952380953,1.0497354497354496,-0.70905884239217576,0.43105536438869774,-0.24458017702462148,0.13216614677181127,-0.06886918490214311,0.034889536644946019,-0.017282462508659408,0.0084051818766056937,-0.0040258540705689887,0.0019035533981396693,-0.00089017053888879093,0.00041231146351428539,-0.00018938401432365952,8.6348756223947849e-05,-3.9112989540335876e-05,1.7613219537854254e-05,-7.8897762690279144e-06,3.517366469104998e-06),
      c(2.5904761904761906,-4.1989417989417985,4.2543530543530546,-3.4484429151095819,2.4458017702462147,-1.5859937612617352,0.96416858863000365,-0.5582325863191363,0.31108432515586931,-0.16810363753211388,0.088568789552517749,-0.045685281555352064,0.023144434011108564,-0.011544720978399991,0.0056815204297097858,-0.0027631601991663312,0.0013298416443714197,-0.0006340759033627532,0.00029981149822306077,-0.00014069465876419992,6.557460704746032e-05)    )
    return(vapply(seq_len(4),function(i) {
      c <- coefficients[[i]]; y <- c[length(c)]
      for(j in seq.int(length(c)-1L,1L)) y <- c[j]+z*y
      h^(2*i)*y
    },numeric(1)))
  }
  u <- 1/x; e <- exp(-2*x); t <- (1-e)/(1+e); q <- 4*e/(1+e)^2
  c(h^2*t*u, h^4*(t*u-q)*u^2,
    h^6*(3*t*u^3-3*q*u^2-2*q*t*u)*u^2,
    h^8*(15*t*u^4-15*q*u^3-12*q*t*u^2+(2*q^2-4*q*t^2)*u)*u^3)
}
# Scalar, normalized reference evaluator, for qualification rather than posterior tooling.
.mp_ez3_log_density <- function(mean_rt,var_rt,n_upper,n_trials,drift,bound,ndt) {
  .mp_validate_ez3(data.frame(mean_rt,var_rt,n_upper,n_trials))
  if(length(ndt)!=1L || !is.finite(ndt) || ndt<=0) stop("finite positive scalar ndt required")
  k <- .mp_ez3_cumulants(drift,bound)
  W <- k[4]/n_trials+2*k[2]^2/(n_trials-1); C <- k[3]/n_trials
  cv <- k[2]/n_trials-C^2/W; cm <- ndt+k[1]+C/W*(var_rt-k[2])
  if(!is.finite(W) || W<=0 || !is.finite(cv) || cv<=0) stop("EZ3 cumulants outside representable positive domain")
  logit <- bound*drift
  # Avoid 0*Inf and probability rounding at extreme logits.
  count <- lchoose(n_trials,n_upper) - n_upper*log1p(exp(-abs(logit))) - (n_trials-n_upper)*log1p(exp(-abs(logit)))
  count <- count - if(logit>=0)(n_trials-n_upper)*logit else n_upper*(-logit)
  count + stats::dgamma(var_rt,shape=k[2]^2/W,rate=k[2]/W,log=TRUE) + stats::dnorm(mean_rt,cm,sqrt(cv),log=TRUE)
}
.mp_ez3_stan_code <- function() c(
  "vector measurement_ez3_cumulants(real drift, real bound) {",
  "  real h = bound / 2;",
  "  real x = abs(h * drift);",
  "  vector[4] k;",
  "  if (x <= 0.5) {",
  "    real z = square(x);",
  "    k[1] = pow(h, 2) * (1 + z * (-0.33333333333333331 + z * (0.13333333333333333 + z * (-0.053968253968253971 + z * (0.021869488536155203 + z * (-0.0088632355299021973 + z * (0.0035921280365724811 + z * (-0.0014558343870513183 + z * (0.00059002744094558595 + z * (-0.00023912911424355248 + z * (9.6915379569294509e-05 + z * (-3.9278323883316833e-05 + z * (1.5918905069328964e-05 + z * (-6.4516892156554306e-06 + z * (2.6147711512907546e-06 + z * (-1.0597268320104654e-06 + z * (4.2949110782738057e-07 + z * (-1.7406618963571648e-07 + z * (7.0546369464009681e-08 + z * (-2.859136662305254e-08 + z * 1.1587644432798853e-08))))))))))))))))))));",
  "    k[2] = pow(h, 4) * (0.66666666666666663 + z * (-0.53333333333333333 + z * (0.32380952380952382 + z * (-0.17495590828924162 + z * (0.08863235529902197 + z * (-0.043105536438869774 + z * (0.020381681418718454 + z * (-0.0094404390551293751 + z * (0.0043043240563839444 + z * (-0.0019383075913858901 + z * (0.00086412312543297039 + z * (-0.00038205372166389513 + z * (0.00016774391960704119 + z * (-7.3213592236141128e-05 + z * (3.1791804960313962e-05 + z * (-1.3743715450476178e-05 + z * (5.9182504476143599e-06 + z * (-2.5396693007043486e-06 + z * (1.0864719316759965e-06 + z * (-4.6350577731195406e-07 + z * 1.9724440672569787e-07))))))))))))))))))));",
  "    k[3] = pow(h, 6) * (1.0666666666666667 + z * (-1.2952380952380953 + z * (1.0497354497354496 + z * (-0.70905884239217576 + z * (0.43105536438869774 + z * (-0.24458017702462148 + z * (0.13216614677181127 + z * (-0.06886918490214311 + z * (0.034889536644946019 + z * (-0.017282462508659408 + z * (0.0084051818766056937 + z * (-0.0040258540705689887 + z * (0.0019035533981396693 + z * (-0.00089017053888879093 + z * (0.00041231146351428539 + z * (-0.00018938401432365952 + z * (8.6348756223947849e-05 + z * (-3.9112989540335876e-05 + z * (1.7613219537854254e-05 + z * (-7.8897762690279144e-06 + z * 3.517366469104998e-06))))))))))))))))))));",
  "    k[4] = pow(h, 8) * (2.5904761904761906 + z * (-4.1989417989417985 + z * (4.2543530543530546 + z * (-3.4484429151095819 + z * (2.4458017702462147 + z * (-1.5859937612617352 + z * (0.96416858863000365 + z * (-0.5582325863191363 + z * (0.31108432515586931 + z * (-0.16810363753211388 + z * (0.088568789552517749 + z * (-0.045685281555352064 + z * (0.023144434011108564 + z * (-0.011544720978399991 + z * (0.0056815204297097858 + z * (-0.0027631601991663312 + z * (0.0013298416443714197 + z * (-0.0006340759033627532 + z * (0.00029981149822306077 + z * (-0.00014069465876419992 + z * 6.557460704746032e-05))))))))))))))))))));",
  "  } else {",
  "    real u = 1 / x;",
  "    real e = exp(-2 * x);",
  "    real t = (1 - e) / (1 + e);",
  "    real q = 4 * e / square(1 + e);",
  "    k[1] = square(h) * t * u;",
  "    k[2] = pow(h, 4) * (t * u - q) * square(u);",
  "    k[3] = pow(h, 6) * (3*t*pow(u, 3) - 3*q*square(u) - 2*q*t*u) * square(u);",
  "    k[4] = pow(h, 8) * (15*t*pow(u, 4) - 15*q*pow(u, 3) - 12*q*t*square(u) + (2*square(q)-4*q*square(t))*u) * pow(u, 3);",
  "  }",
  "  return k;",
  "}",
  "real measurement_ez3_revised_lpdf(real mean_rt, real var_rt, int n_upper, int n_trials, real drift, real bound, real ndt) {",
  "  vector[4] k = measurement_ez3_cumulants(drift, bound);",
  "  real W = k[4] / n_trials + 2 * square(k[2]) / (n_trials - 1.0);",
  "  real C = k[3] / n_trials;",
  "  real conditional_variance = k[2] / n_trials - square(C) / W;",
  "  real conditional_mean = ndt + k[1] + C / W * (var_rt - k[2]);",
  "  if (!(W > 0) || !(conditional_variance > 0))",
  "    reject(\"EZ3 cumulants outside representable positive domain\");",
  "  return binomial_logit_lpmf(n_upper | n_trials, bound * drift)",
  "    + gamma_lpdf(var_rt | square(k[2]) / W, k[2] / W)",
  "    + normal_lpdf(mean_rt | conditional_mean, sqrt(conditional_variance));",
  "}"
)
