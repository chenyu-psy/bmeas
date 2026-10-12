demo_gaussian_cfa <- function(n = 8L, seed = 20261003L) {
  n <- .mp_integer(n, "n", lower = 3L)
  if (n > 30L) .mp_abort("E_SMOKE_BUDGET", "demo fixture is intentionally limited to 30 synthetic people")
  seed <- .mp_integer(seed, "seed")
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  z <- matrix(stats::rnorm(2L*n), nrow = n) %*% chol(matrix(c(1, .35, .35, 1), 2L))
  loading <- c(.7, .6, .8, .8, .65, .7)
  y <- vapply(seq_len(6L), function(j) loading[j]*z[, if (j <= 3L) 1L else 2L] + stats::rnorm(n, sd = .6), numeric(n))
  data.frame(person = rep(sprintf("synthetic_%02d", seq_len(n)), 6L),
    item = rep(c("v1", "v2", "v3", "s1", "s2", "s3"), each = n),
    value_z = as.vector(y), stringsAsFactors = FALSE)
}
demo_measurement_spec <- function(data = demo_gaussian_cfa()) {
  scores <- observe(gaussian_model(), data, subject = "person", task = "item",
                    response = "value_z", units = "externally_standardized", source_id = "synthetic_fixture_v1")
  verbal <- targets("scores", "mu", c("v1", "v2", "v3"))
  spatial <- targets("scores", "mu", c("s1", "s2", "s3"))
  measurement(models = list(scores = scores), subjects = shared_subjects("synthetic_demo"),
    structure = cfa_structure(list(Verbal = verbal, Spatial = spatial),
      positive_anchors = c(Verbal = verbal[1L], Spatial = spatial[1L]),
      nuisance = task_parameters("sigma")),
    priors = prior_preset("gaussian_cfa_standardized_v1"))
}

demo_mixed_spec <- function(n = 8L, seed = 20261003L) {
  n <- .mp_integer(n,"n",3L); if(n>30L) .mp_abort("E_SMOKE_BUDGET","synthetic demo limited to 30 people")
  seed <- .mp_integer(seed,"seed")
  had_seed <- exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(had_seed) old_seed <- get(".Random.seed",envir=.GlobalEnv)
  on.exit(if(had_seed) assign(".Random.seed",old_seed,envir=.GlobalEnv) else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv))
  set.seed(seed)
  z <- matrix(stats::rnorm(n*2L),n,2L) %*% chol(matrix(c(1,.3,.3,1),2L))
  people <- rep(sprintf("synthetic_%02d",seq_len(n)),2L)
  task <- rep(c("t1","t2"),each=n)
  g <- data.frame(person=people,task=task,y=.7*as.vector(z)+stats::rnorm(2*n,sd=.6))
  trials <- rep(c(5L,8L),each=n); trials[1L] <- 0L
  b <- data.frame(person=people,task=task,successes=stats::rbinom(2*n,trials,stats::plogis(.7*as.vector(z))),trials=trials)
  l <- data.frame(person=people,task=task,y=stats::rlnorm(2*n,log(.6)+.3*as.vector(z),.3))
  blocks <- list(scores=observe(gaussian_model(),g,"person","task","y",units="externally_standardized",source_id="mixed_synthetic_gaussian_v1"),
    accuracy=observe(binomial_model(),b,"person","task",fields=list(successes="successes",trials="trials"),units="count",source_id="mixed_synthetic_binomial_v1"),
    time=observe(lognormal_model(),l,"person","task","y",units="second",source_id="mixed_synthetic_lognormal_v1"))
  a <- c(targets("scores","mu","t1"),targets("accuracy","p","t1"),targets("time","meanlog","t1"))
  bb <- c(targets("scores","mu","t2"),targets("accuracy","p","t2"),targets("time","meanlog","t2"))
  structure <- cfa_structure(list(A=a,B=bb),positive_anchors=c(A=a[1],B=bb[1]),nuisance=task_parameters(c("sigma","sdlog")))
  priors <- mixed_prior_set(list(
    scores=block_prior(normal(0,1),normal(0,.5),half_normal(1),units="externally_standardized",predictor_scale="response"),
    accuracy=block_prior(normal(0,1.5),normal(0,.5),units="count",predictor_scale="logit"),
    time=block_prior(normal(log(.6),.5),normal(0,.3),half_normal(.5),units="second",predictor_scale="log_response")),correlation=lkj(2))
  measurement(blocks,shared_subjects("synthetic_mixed"),structure,priors)
}

demo_cognitive_wiring_spec <- function(m3_file, sdm_file) {
  # Deliberately tiny valid synthetic rows; not a substantive factor hypothesis.
  people<-sprintf("synthetic_%02d",1:4)
  counts<-matrix(0L,4L,10L);counts[,c(1:4,10)]<-rbind(c(4,1,1,0,1),c(3,2,0,1,1),c(5,0,1,0,1),c(2,2,1,1,1))
  options<-matrix(rep(c(1L,3L,1L,3L,0L,0L,0L,0L,0L,10L),each=4L),4L,10L)
  memory<-data.frame(person=people,task="CS",stringsAsFactors=FALSE);memory$counts<-counts;memory$options<-options
  report<-data.frame(person=rep(people,each=5L),task="CR",run=rep(1:4,each=5L),
    obs=sprintf("synthetic_trial_%02d",1:20),error=c(-.8,-.2,0,.25,.7,-1,-.3,.1,.4,1.2,-.5,-.15,.05,.3,.9,-1.4,-.4,.15,.5,1.6))
  blocks<-list(memory=observe(m3_cs_model(m3_file),memory,"person","task",fields=list(counts="counts",options="options"),units="count",source_id="synthetic_CS_only_v1"),
    report=observe(sdm_reference_model(sdm_file),report,"person","task",fields=list(error="error",run="run"),units="radian",observation_id="obs",source_id="synthetic_SDM_v1"))
  a<-c(targets("memory",c("a","c"),"CS"),targets("report","log_c","CR"))
  b<-c(targets("memory",c("ra","rc"),"CS"),targets("report","log_kappa","CR"))
  keys<-c(a,b)
  parameters<-stats::setNames(list(
    cfa_parameter_prior(normal(1,.4),normal(0,.25),"log","log_activation"),
    cfa_parameter_prior(normal(1,.4),normal(0,.25),"log","log_activation"),
    cfa_parameter_prior(normal(.75,.5),normal(0,.25),"identity","reference_log_c"),
    cfa_parameter_prior(logistic(0,1),normal(0,.25),"logit","log_odds"),
    cfa_parameter_prior(logistic(0,1),normal(0,.25),"logit","log_odds"),
    cfa_parameter_prior(student_t(5,1.75,.75),normal(0,.25),"identity","log_precision")),keys)
  spec<-measurement(blocks,shared_subjects("synthetic_cognitive_wiring"),
    cfa_structure(list(Wiring_A=a,Wiring_B=b),positive_anchors=c(Wiring_A=a[1L],Wiring_B=b[1L]),nuisance=task_parameters(character())),
    parameter_prior_set(parameters,lkj(2)))
  spec$example_warning<-"Synthetic API wiring only: six jointly observed parameter indicators from two tasks, not six independent tasks or validated scientific constructs; reference G/P/C hierarchy not substituted"
  spec
}
