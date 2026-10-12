# Explicit custom scalar example, no compilation or sampling.
# Run after library(bmeas). This example resource is original MIT code.
file <- system.file("stan","two_afc_example.stanfunctions",package="bmeas")
resource <- stan_resource(file,
  expected_sha256="a23365e38172f8762cff6d57ae2536bfde0eb728b4b449748d9aeb827090de27",
  exports="example_two_afc_lpmf",license="MIT",source="Original bhmm tutorial")
model <- new_observation_model("custom_two_afc","0.1",
  response_schema(y="binary",difficulty="real"),
  list(sensitivity=parameter_spec("real","identity","eta","subject_task","log_odds","Sensitivity relative to known difficulty",predictor_units="log_odds")),
  stan_density(file,"example_two_afc_lpmf",c("y","sensitivity","difficulty"),c("int","real","real"),TRUE,"none"),
  model_capabilities(fit=TRUE))
model <- register_stan_adapter(model,resources=list(resource))
data <- data.frame(person=rep(c("synthetic_a","synthetic_b"),each=3),task=rep(c("t1","t2","t3"),2),y=c(1,0,1,0,1,1),difficulty=c(-.5,0,.5,-.5,0,.5))
block <- observe(model,data,"person","task",fields=list(y="y",difficulty="difficulty"),units="binary")
keys <- targets("choices","sensitivity",c("t1","t2","t3"))
priors <- stats::setNames(lapply(keys,function(k)cfa_parameter_prior(normal(0,1),normal(0,.5),"identity","log_odds")),keys)
spec <- measurement(list(choices=block),shared_subjects("synthetic_custom"),
  cfa_structure(list(Wiring=keys),positive_anchors=c(Wiring=keys[1]),nuisance=task_parameters(character())),parameter_prior_set(priors,lkj(2)))
check_measurement(spec)
make_stan_code(spec)
