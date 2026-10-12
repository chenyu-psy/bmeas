m3_cs_model <- function(file, version = "csmu_cs_subset_v1",
  expected_sha256 = "4b070861489b188e63575c4e0a8dae18cd54d01afd8f1d56a804714061fdc4d1") {
  if(missing(file)) .mp_abort("E_RESOURCE","provide the external original m3.stanfunctions file")
  if(expected_sha256!="4b070861489b188e63575c4e0a8dae18cd54d01afd8f1d56a804714061fdc4d1") .mp_abort("E_MODEL_VERSION","locked reference version cannot accept a different hash; use a generic adapter for other sources")
  if(version!="csmu_cs_subset_v1") .mp_abort("E_MODEL_VERSION","only the explicitly bounded CS subset is implemented")
  resource<-stan_resource(file,expected_sha256,exports=c("ac_task","shifted_activation","shifted_zero_activation","cs_logits","mu_no_update_logits","mu_update_logits","partial_m3_cs","partial_m3_mu_no_update","partial_m3_mu_update"),
    license="unknown; external user-provided research source, not relicensed",source="CSMU input ZIP SHA256 0792bef7fed70cda499fdfd7cff7652e64465d193c9b4c4744022b5419982f73; scripts/stan/likelihoods/m3.stanfunctions")
  parameters<-list(
    a=parameter_spec("positive","log","eta","subject_task","activation","CS activation a; log predictor received by original function",predictor_units="log_activation"),
    c=parameter_spec("positive","log","eta","subject_task","activation","CS activation c; log predictor received by original function",predictor_units="log_activation"),
    ra=parameter_spec("unit_interval","logit","eta","subject_task","probability","CS ra probability; logit received by original function",predictor_units="log_odds"),
    rc=parameter_spec("unit_interval","logit","eta","subject_task","probability","CS rc probability; logit received by original function",predictor_units="log_odds"))
  model<-new_observation_model("m3_cs_reference",version,
    response_schema(counts=data_field("nonnegative_integer",width=10L),options=data_field("nonnegative_integer",width=10L)),parameters,
    stan_density(symbol="measurement_m3_cs_lpmf",arguments=c("counts","options","a","c","ra","rc"),
      argument_types=c("int_array","int_array",rep("real",4)),normalized=TRUE,approximation="original stabilized CS logits",builtin=TRUE,license="MIT wrapper; external function license unresolved"),
    model_capabilities(fit=TRUE),validate_data=compiler::cmpfun(.mp_validate_m3_cs),
    provenance=list(source="External hash-pinned CSMU CS likelihood; no MU or G/P/C claim",license="wrapper MIT; reference resource not relicensed"))
  wrapper<-c("real measurement_m3_cs_lpmf(array[] int counts, array[] int options, real a, real c, real ra, real rc) {",
    "  array[5] int y = {counts[1],counts[2],counts[3],counts[4],counts[10]};",
    "  vector[10] log_options;",
    "  for(k in 1:10) log_options[k] = options[k]>0 ? log(options[k]) : 0;",
    "  return multinomial_logit_lpmf(y | cs_logits(a,c,ra,rc,log_options));","}")
  model<-register_stan_adapter(model,list(resource),wrapper,"measurement_m3_cs_lpmf")
  model$required_units<-"count";model$informative_rows<-.mp_stable_callback(.mp_m3_cs_informative)
  model$reference_parameter_hash<-.mp_hash(model$parameters)
  model$limitations<-"CS only, positive option multiplicities in the five active categories; no MU or reference G/P/C hierarchy; main-API explicit CS simulator and population response checks are separately available"
  model
}
.mp_validate_m3_cs <- function(data) {
  active<-c(1:4,10)
  if(any(data$options[,active,drop=FALSE]<=0)) .mp_abort("E_M3_OPTIONS","CS-only adapter requires positive option multiplicities in active columns 1:4 and 10")
  if(any(data$counts[,5:9,drop=FALSE]!=0,na.rm=TRUE)) .mp_abort("E_M3_BRANCH","CS-only rows require inactive count columns 5:9 to be zero")
  TRUE
}
sdm_reference_model <- function(file, version = "csmu_sqrtexp_v1",
  expected_sha256 = "66d521033044ecd68fc6f5253e669773a5fc352cd5bc280fc1b905eff37154a4") {
  if(missing(file)) .mp_abort("E_RESOURCE","provide the external original sdm.stanfunctions file")
  if(expected_sha256!="66d521033044ecd68fc6f5253e669773a5fc352cd5bc280fc1b905eff37154a4") .mp_abort("E_MODEL_VERSION","locked reference version cannot accept a different hash; use a generic adapter for other sources")
  if(version!="csmu_sqrtexp_v1") .mp_abort("E_MODEL_VERSION","only the pinned reference SDM parameterization is implemented")
  resource<-stan_resource(file,expected_sha256,exports=c("bin_search","get_m","sdm_simple_lpdf","sdm_simple_ldenom_chquad_adaptive","partial_sdm"),
    license="unknown; external user-provided research source, not relicensed",source="CSMU input ZIP SHA256 0792bef7fed70cda499fdfd7cff7652e64465d193c9b4c4744022b5419982f73; scripts/stan/likelihoods/sdm.stanfunctions")
  CN<-matrix(0,200L,200L)
  for(m in seq_len(200L)) CN[seq_len(m),m]<-cos((2*seq_len(m)-1)*pi/(2*m))-1
  model<-new_observation_model("sdm_reference",version,
    response_schema(error=data_field("real",lower=-pi,upper=pi),run=data_field("integer",lower=1)),
    list(log_c=parameter_spec("real","identity","eta","subject_task","reference_log_c","Reference predictor consumed directly, without exp",predictor_units="reference_log_c"),
      log_kappa=parameter_spec("real","identity","eta","subject_task","log_precision","Reference log-kappa; exp once inside adapter",predictor_units="log_precision")),
    stan_density(symbol="measurement_sdm_lpdf",arguments=c("error","log_c","log_kappa","CN"),argument_types=c("real","real","real","matrix"),
      normalized=TRUE,approximation="reference adaptive Clenshaw-Curtis rule, orders 2:200",builtin=TRUE,license="MIT wrapper; external function license unresolved"),
    model_capabilities(fit=TRUE),validate_data=compiler::cmpfun(.mp_validate_sdm_runs),
    provenance=list(source="External pinned SDM sqrtexp likelihood; fixed zero angular center; serial per-trial equivalent of run sum",license="wrapper MIT; reference resource not relicensed"),
    constants=list(CN=stan_constant(CN,"matrix","cos(angle)-1","Exact reference 200x200 cosine grid; unused triangle initialized to zero")))
  wrapper<-c("real measurement_sdm_lpdf(real error, real log_c, real log_kappa, matrix CN) {",
    "  vector[1] y;", "  real kappa=exp(log_kappa);", "  y[1]=error;",
    "  return sdm_simple_lpdf(y | log_c,kappa) + sdm_simple_ldenom_chquad_adaptive(log_c,kappa,CN) - (log(2.0)+log(pi()));", "}")
  model<-register_stan_adapter(model,list(resource),wrapper,"measurement_sdm_lpdf")
  model$required_units<-"radian"
  model$reference_parameter_hash<-.mp_hash(model$parameters)
  model$limitations<-"Fixed zero center; original adaptive quadrature unchanged; serial per-trial evaluation preserves but does not accelerate original run likelihood; no simulator/PPC"
  model
}
.mp_validate_sdm_runs <- function(data) {
  groups<-split(paste(data$.local_id,data$.task,sep="::"),data$run)
  if(any(vapply(groups,function(x)length(unique(x))!=1L,logical(1)))) .mp_abort("E_SDM_RUN","each block-global run ID must belong to exactly one subject/task")
  TRUE
}

.mp_m3_cs_informative <- function(d) rowSums(d$counts[,c(1:4,10),drop=FALSE],na.rm=TRUE)>0
