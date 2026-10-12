# Construction only: all response values below are synthetic, not research data.
# Requires library(bmeas) and an explicitly provided original M3 source.
file <- Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
if(!nzchar(file)) stop("Set MEASUREMENTPKG_M3_RESOURCE to the original hash-matched m3.stanfunctions path")
d <- expand.grid(content=1:3,route=1:3,person=c("synthetic_a","synthetic_b"),stringsAsFactors=FALSE)
d$observation_id <- sprintf("synthetic_%03d",seq_len(nrow(d)))
d$observation_task <- paste(d$route,d$content,sep="_")
d$counts <- d$options <- matrix(0L,nrow(d),10)
active <- list(c(1:4,10),c(5:7,9:10),5:10)
for(i in seq_len(nrow(d))) {
  d$counts[i,active[[d$route[i]]]] <- 1L
  d$options[i,active[[d$route[i]]]] <- 1L
}
bindings <- m3_reference_bindings(d)
block <- observe(m3_model(file),d,subject="person",task="observation_task",observation_id="observation_id",
  fields=list(counts="counts",options="options",route="route",content="content"),
  units="count",source_id="synthetic_full_M3_example",parameter_bindings=bindings)
spec <- measurement(list(memory=block),shared_subjects("synthetic_example"),
  reference_structure("csmu_exp1_memory_gpc_v1",bindings$domains,"memory"),
  reference_priors("csmu_exp1_memory_gpc_v1",bindings$domains,"memory"))
check_measurement(spec)
parameter_binding_table(spec)
prior_table(spec)
make_stan_code(spec) # No compilation or sampling; log_lik arrays are absent by default.
