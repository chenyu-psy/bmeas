source(file.path("helpers", "native-guard.R"))
source('frontend-predict-m3cs.R')
if(exists('s'))local({
  kind<-private('.bh_m3cs_kind');oldhash<-private('.mp_hash')
  changes<-list(
    resource_hash=function(m){m$adapter$resources[[1]]$sha256<-paste(rep('0',64),collapse='');m},
    argument_order=function(m){m$likelihood$arguments[c(3,4)]<-m$likelihood$arguments[c(4,3)];m},
    transform=function(m){m$parameters$a$predictor<-'identity';m},
    receiver=function(m){m$parameters$a$receives<-'theta';m},
    output_schema=function(m){m$data_schema$counts$width<-5L;m},
    normalized=function(m){m$likelihood$normalized<-FALSE;m},
    wrapper=function(m){m$adapter$function_code[1]<-'different code';m},
    argument_type=function(m){m$likelihood$argument_types[3]<-'vector';m},
    active_options=function(m){m$data_schema$options$width<-5L;m},
    constants=function(m){m$constants<-list(other=1);m},
    export=function(m){m$adapter$function_exports<-'other';m})
  stopifnot(kind(m),all(!vapply(changes,function(change)kind(change(m)),logical(1))))
  .bmeas_test_set('.mp_hash',function(...)stop('planning hash/file-I/O trap'))
  on.exit(.bmeas_test_set('.mp_hash',oldhash))
  set.seed(22);rng<-.Random.seed
  p<-ppc_plan(hidden_fit,2,budget,level='conditional')
  stopifnot(nrow(p$statistics)==105,identical(rng,.Random.seed))
  cat('PASS literal complete contract signature rejects 11 impostors; passive planning avoids hash/file I/O and preserves RNG\n')
})
