source(file.path("helpers", "native-guard.R"))
# The first section uses labelled in-memory controller/reader doubles; the
# separate actual section reads tiny generated engineering CSVs. Neither is
# fitted posterior evidence, native/HMC execution, or inference qualification.
source_root <- Sys.getenv("MEASUREMENTPKG_SOURCE")
source_mode <- nzchar(source_root)
if (source_mode) {
  for (file in list.files(file.path(source_root,"R"),full.names=TRUE)) source(file)
} else library(bmeas)
private <- function(name) if(source_mode)get(name,.GlobalEnv)else getFromNamespace(name,"bmeas")
passed <- 0L
ok <- function(x,label) {
  if(!isTRUE(x))stop(label)
  passed <<- passed+1L
  cat("PASS",label,"\n")
}
err <- function(expr,code,label) {
  e<-tryCatch(force(expr),error=identity)
  ok(inherits(e,"measurement_error")&&identical(e$code,code),label)
}
budget <- function() list(posterior=list(reader="streaming",max_source_bytes=100000,
  max_read_bytes=700000,max_materialized_values=5000000),max_parameter_cells=2000,
  max_summary_rows=6000,max_transformation_terms=1000000,
  max_declared_observation_rows=2000,max_consumed_observation_rows=2000,
  max_operand_context_rows=30000,max_context_terms=100000000)
legacy_budget<-list(max_source_bytes=10485760,max_posterior_rows=100,
  max_draw_values=50000,max_parameter_cells=1000)
make_fit<-function(n=3L,shared=FALSE,fixed=FALSE,repetitions=1L) {
  people<-paste0("person",seq_len(n));tasks<-c("one","two")
  d<-expand.grid(person=people[1:2],task=tasks,stringsAsFactors=FALSE)
  d<-d[rep(seq_len(nrow(d)),times=repetitions),,drop=FALSE];rownames(d)<-NULL
  d$y<-seq_len(nrow(d))/10;d$obs<-paste0("row",seq_len(nrow(d)))
  par<-if(fixed)list(mu=list(fixed=c(one=.2,two=.4)))else if(shared)list(mu="task")else NULL
  block<-observe(gaussian_model(),d,"person","task","y",units="score",observation_id="obs",parameters=par)
  formula<-if(shared||fixed)bmf(person=list(distribution="sample_centered",ids=people),augmentation=list(type="task_cholesky")) else
    bmf(residual=list(targets=~score$mu[c("one","two")]),person=list(distribution="sample_centered",ids=people),augmentation=list(type="task_cholesky"))
  spec<-bmeas(formula,list(score=block),fit=FALSE)
  structure(list(spec=spec,code=as.character(stancode(spec)),run=list(chains=4L),
    backend=list(draws=function(...)stop("backend callback forbidden"),metadata=function(...)stop("backend callback forbidden")),
    cache=list(computation=list(status="engineering_double"),inference=list(status="not_assessed")),
    provenance=list(kind="synthetic_in_memory_reader_double_not_posterior_evidence",
      model_fingerprint=spec$fingerprint,output_files=paste0("engineering-memory-chain-",c(11,3,8,4)))),
    class=c("bmeas_fit","bhmm_fit"))
}
if (!identical(Sys.getenv("BMEAS_INDIVIDUAL_STREAMING_PHASE"),"actual")) local({
  env<-if(source_mode).GlobalEnv else asNamespace("bmeas")
  name<-".bh_posterior_ingest_csv";old<-get(name,env);locked<-bindingIsLocked(name,env)
  if(locked)unlockBinding(name,env)
  on.exit({assign(name,old,env);if(locked)lockBinding(name,env)},add=TRUE)
  calls<-0L;bad_alias<-FALSE;last_arguments<-NULL
  reader<-function(files,variables,posterior,states="all",alias_groups=list(),expected_model=NULL,
      expected_chains=NULL,original_hashes=NULL,consumer_buffers=list(),consumer_work=list(),...) {
    calls<<-calls+1L
    stopifnot(identical(expected_model,"scalar_components_model"),expected_chains==4,
      all(c("fixed","per_selected_row")%in%names(consumer_buffers)))
    ids<-c(3L,4L,8L,11L);L<-32L;R<-4L*L
    keys<-data.frame(chain_id=rep(ids,L),saved_row=rep(seq_len(L),each=4L))
    full<-matrix(0,R,length(variables),dimnames=list(NULL,variables))
    for(j in seq_along(variables))full[,j]<-keys$chain_id/10+keys$saved_row/100+j/1000
    for(group in alias_groups) {
      stopifnot(typeof(group)%in%c("integer","double"),all(group%in%seq_along(variables)))
      for(column in group)full[,column]<-full[,group[1L]]
    }
    if(bad_alias&&length(alias_groups))full[R,tail(alias_groups[[1L]],1L)]<-999
    # The double explicitly implements the all-source alias obligation, including
    # unselected final rows. This does not qualify the real streaming parser.
    for(group in alias_groups)for(column in group)
      if(any(full[,column]!=full[,group[1L]]))
        private(".mp_abort")("E_INDIVIDUAL_SOURCE","engineering all-source alias disagreement")
    method<-if(identical(states,"all"))"all"else "balanced"
    D<-if(method=="all")R else states$draws
    alloc<-rep(floor(D/4),4);if(D%%4)alloc[seq_len(D%%4)]<-alloc[seq_len(D%%4)]+1
    chosen<-integer()
    for(rank in seq_len(max(alloc)))for(chain in seq_along(ids))if(rank<=alloc[chain]) {
      row<-if(alloc[chain]==1)floor((L+1)/2)else 1+floor((rank-1)*(L-1)/(alloc[chain]-1))
      chosen<-c(chosen,which(keys$chain_id==ids[chain]&keys$saved_row==row))
    }
    if(D*consumer_work$per_selected_row[["transformation_terms"]]>consumer_work$limits[["transformation_terms"]])
      private(".mp_abort")("E_PPC_SOURCE_BUDGET","engineering selected work refuses before projection")
    last_arguments<<-list(alias_groups=alias_groups,buffers=consumer_buffers,work=consumer_work,states=states)
    list(draws=full[chosen,,drop=FALSE],draw_keys=keys[chosen,,drop=FALSE],
      receipt=list(version="synthetic_reader_double_v1",status="engineering_double",
        source_rows=R,selected_rows=D,selected_required_values=D*length(variables),
        selection=list(method=method,allocations=data.frame(chain_id=ids,source_rows=rep(L,4),
          selected_rows=alloc,omitted_rows=L-alloc))))
  }
  assign(name,reader,env)
  fit<-make_fit();before<-serialize(fit,NULL)
  had_rng<-exists(".Random.seed",.GlobalEnv,inherits=FALSE)
  rng<-if(had_rng)get(".Random.seed",.GlobalEnv)else NULL
  p<-private(".bh_individual_plan")(fit,c("predictor","natural","received"),budget())
  ok(calls==0L&&length(p$output_files)==4L&&nrow(p$cells)==6L,
    "streaming planning accepts four saved paths without source access")
  ok(p$context$planned_work$context_terms==sum(unlist(p$context$planned_work$context_terms_by_operation))&&
    p$context$planned_work$context_terms_by_operation$complete_cell_coverage_search==
      nrow(p$cells)*p$context$planned_work$coverage_rows_upper_bound,
    "complete cell coverage and all declared metadata join work are itemized")
  ok(sum(p$consumer_buffers$per_selected_row)==12&&
     p$consumer_buffers$fixed[["summary_numeric_columns"]]>0&&
     p$consumer_buffers$fixed[["planning_phase_numeric_slots_bound"]]>0,
    "reservation includes planning, numeric output and one-cell exact-summary vectors")
  all<-individual_parameter_summary(fit,scales=c("predictor","natural","received"),budget=budget())
  sub<-individual_parameter_summary(fit,scales=c("predictor","natural","received"),budget=budget(),
    states=list(method="balanced",draws=5))
  ok(all(all$n_draws==128L)&&attr(all,"work")$retained_source_rows==128&&
     attr(all,"work")$selected_source_rows==128&&attr(all,"metadata")$selection$omitted_rows==0,
    "default streaming summaries use all 128 states across four complete chains")
  ok(all(sub$n_draws==5L)&&attr(sub,"work")$retained_source_rows==128&&
     attr(sub,"work")$selected_source_rows==5&&attr(sub,"metadata")$selection$omitted_rows==123,
    "explicit balanced subset preserves separate selected and full-source denominators")
  expected_keys<-data.frame(chain_id=c(3L,4L,8L,11L,3L),saved_row=c(1L,16L,16L,16L,32L))
  rownames(expected_keys)<-rownames(attr(sub,"draw_keys"))
  ok(isTRUE(all.equal(attr(sub,"draw_keys"),expected_keys,check.attributes=FALSE)),"balanced controller retains exact interleaved selected keys")
  ok(grepl("selected states only",attr(sub,"metadata")$selection$limitations,fixed=TRUE)&&
    grepl("All retained source states",attr(sub,"metadata")$required_raw_validation_scope,fixed=TRUE),
    "result distinguishes full-source raw validation from selected empirical summaries")
  printed<-capture.output(print(sub));ok(any(grepl("omitted states: 123",printed,fixed=TRUE)),
    "passive printing accepts an honest larger full count and discloses the subset")
  bad<-sub;attr(bad,"source_receipt")<-new.env(parent=emptyenv())
  ok(length(capture.output(print(bad)))>0,"malformed optional receipt does not erase valid saved table evidence")
  bad<-sub;attr(bad,"metadata")$selection$full_source_rows<-127
  err(print(bad),"E_INDIVIDUAL_RESULT","passive selection consistency rejects a false full-source count")
  bad<-sub;attr(bad,"draw_keys")$saved_row[1L]<-33L
  err(print(bad),"E_INDIVIDUAL_RESULT","passive selected keys are checked against saved full-chain rows")
  bad<-sub;attr(bad,"draw_keys")$saved_row[1L]<-2L
  err(print(bad),"E_INDIVIDUAL_RESULT","passive validation checks the exact deterministic selected ranks")
  large<-individual_parameter_summary(make_fit(502L),scales=c("predictor","natural","received"),budget=budget())
  ok(nrow(large)==3012L&&attr(large,"work")$logical_parameter_cells==1004&&
    attr(large,"work")$transformation_terms==385536&&all(large$n_draws==128L),
    "explicit caps admit complete 1004-cell, 3012-row and 385536-transform engineering output")
  ok(length(capture.output(print(large)))>0,"passive v2 printing admits larger complete cell and output counts")
  long_design<-make_fit(repetitions=251L)
  lp<-private(".bh_individual_plan")(long_design,"predictor",budget())
  ok(lp$context$planned_work$declared_observation_rows==1004L&&
    lp$context$planned_work$consumed_observation_rows==1004L,
    "streaming context caps replace former 1000-row declared and consumed ceilings")
  for(field in c("max_declared_observation_rows","max_consumed_observation_rows")) {
    low<-budget();low[[field]]<-1000
    err(private(".bh_individual_plan")(long_design,"predictor",low),"E_INDIVIDUAL_BUDGET",paste("complete row quota enforced for",field))
  }
  low<-budget();low$max_operand_context_rows<-lp$context$planned_work$operand_context_rows_upper_bound-1
  err(private(".bh_individual_plan")(long_design,"predictor",low),"E_INDIVIDUAL_BUDGET",
    "combined operand route exclusion and literal expansion is budgeted conservatively")
  fixed<-individual_parameter_summary(make_fit(fixed=TRUE),scales="natural",budget=budget())
  ok(all(fixed$n_draws==128L)&all(fixed$n_sampled_values==0L)&all(fixed$sd==0),
    "fixed-only zero-column summaries retain all keys and known-input semantics")
  empty_model<-new_observation_model("engineering_empty_predictor","v1",response_schema(y=data_field("real")),
    list(sigma=parameter_spec("positive","log","theta","task","score","scale only",role="observation_scale")),
    stan_density(symbol="engineering_empty_predictor_lpdf",arguments=c("y","sigma"),argument_types=c("real","real"),
      normalized=TRUE,approximation="synthetic declaration only",builtin=TRUE,license="MIT"),model_capabilities(fit=TRUE))
  ed<-data.frame(person=c("one","two"),task="only",y=c(1,2),obs=c("r1","r2"))
  es<-bmeas(bmf(),list(scale=observe(empty_model,ed,"person","task","y",units="score",observation_id="obs")),fit=FALSE)
  ef<-structure(list(spec=es,code="SYNTHETIC EMPTY-DOMAIN MAP: no compiled or fitted source",cache=list(),
    provenance=list(model_fingerprint=es$fingerprint,kind="synthetic_empty_predictor_domain",output_files=NULL)),
    class=c("bmeas_fit","bhmm_fit"))
  zero<-budget();zero$max_parameter_cells<-zero$max_summary_rows<-zero$max_transformation_terms<-0
  count<-calls;empty<-individual_parameter_summary(ef,scales="natural",budget=zero)
  ok(nrow(empty)==0L&&calls==count&&attr(empty,"metadata")$selection$full_source_rows==0&&
    length(capture.output(print(empty)))>0,"empty predictor domain supports zero cell/output/transform caps without acquisition")
  shared<-make_fit(shared=TRUE)
  sh<-individual_parameter_summary(shared,scales="predictor",budget=budget(),states=list(method="balanced",draws=4))
  ok(length(last_arguments$alias_groups)==2&&all(vapply(last_arguments$alias_groups,length,integer(1))==3L),
    "every shared-person scalar reaches the all-source alias map as an exact column position")
  bad_alias<-TRUE
  err(individual_parameter_summary(shared,scales="predictor",budget=budget(),states=list(method="balanced",draws=4)),
      "E_INDIVIDUAL_SOURCE","reader double refuses an alias mismatch outside selected rows")
  bad_alias<-FALSE
  low<-budget();low$max_context_terms<-p$context$planned_work$context_terms-1
  count<-calls
  err(individual_parameter_summary(fit,scales=c("predictor","natural","received"),budget=low),
    "E_INDIVIDUAL_BUDGET","complete metadata work refuses before source acquisition")
  ok(calls==count,"context refusal makes no reader call")
  low<-budget();low$posterior$max_materialized_values<-1
  err(private(".bh_individual_plan")(fit,"predictor",low),"E_INDIVIDUAL_BUDGET",
    "planning numeric reservation refuses before cell and coverage expansion")
  low<-budget();low$max_transformation_terms<-6*127
  err(individual_parameter_summary(fit,scales="predictor",budget=low),"E_PPC_SOURCE_BUDGET",
    "actual all-row transform work refuses in the reader double before projection")
  ok(nrow(individual_parameter_summary(fit,scales="predictor",budget=low,states=list(method="balanced",draws=4)))==6,
    "same full source supports an explicitly budgeted subset without tightening source-row limits")
  mixed<-budget();mixed$max_posterior_rows<-100
  err(private(".bh_individual_plan")(fit,"predictor",mixed),"E_INDIVIDUAL_BUDGET",
    "streaming budget rejects a mixed legacy acquisition field")
  for(field in setdiff(names(budget()),"posterior")) {
    malformed<-budget();malformed[[field]]<-structure(100,class="Date")
    err(private(".bh_individual_plan")(fit,"predictor",malformed),"E_INDIVIDUAL_BUDGET",paste("plain finite work cap required for",field))
  }
  oldfit<-fit;oldfit$provenance$output_files<-oldfit$provenance$output_files[1:2]
  oldplan<-private(".bh_individual_plan")(oldfit,"predictor",legacy_budget)
  ok(identical(names(oldplan$budget),names(legacy_budget))&&is.null(oldplan$states)&&is.null(oldplan$consumer_buffers),
    "legacy four-field plan keeps its original budget and result shape")
  err(private(".bh_individual_plan")(oldfit,"predictor",legacy_budget,states=list(method="balanced",draws=4)),
    "E_INDIVIDUAL_OPTIONS","state subsets require explicit streaming budget")
  legacy_calls<-0L
  legacy_values<-matrix(c(1,3,2,4),2L,2L,dimnames=list(NULL,c("eta[1,1]","eta[2,1]")))
  legacy<-structure(list(spec=list(structure=list(type="component_covariance",keys="old::mu::task"),
    models=list(old=list(model=gaussian_model())),data_map=list(subject_keys=c("old1","old2"),
      blocks=list(old=data.frame(.person_key=c("old1","old2"),.task="task",.observation_id=c("row1","row2"))))),
    backend=list(draws=function(...) {legacy_calls<<-legacy_calls+1L;legacy_values})),class="measurement_fit")
  legacy_result<-individual_parameter_summary(legacy)
  ok(legacy_calls==1L&&identical(legacy_result$mean,c(2,3))&&all(legacy_result$saved_draws==2L),
    "historical one-argument branch keeps its original backend and summary behavior")
  err(individual_parameter_summary(legacy,states="all"),"E_INDIVIDUAL_OPTIONS",
    "explicit new states argument on legacy fits refuses before backend access")
  ok(legacy_calls==1L,"new legacy options never reach the historical backend")
  err(private(".bh_individual_product")(2^53-1,2),"E_INDIVIDUAL_CAPABILITY",
    "checked multiplication refuses before loss of exact aggregate counts")
  err(private(".bh_individual_count")(.Machine$integer.max+1,"output rows",TRUE),"E_INDIVIDUAL_CAPABILITY",
    "unrepresentable output dimensions are capability refusals before integer coercion")
  # The numerical helper remains exact on the chosen empirical set after the
  # per-state nonlinear transform; full and selected summaries can differ.
  pp<-p;pp$cells<-p$cells[1L,,drop=FALSE];pp$cells$predictor<-"log";pp$cells$support<-"positive"
  pp$scales<-"natural";pp$consumer_buffers<-NULL
  raw<-log(c(1,2,9,20));matrix_all<-matrix(raw,4L,1L,dimnames=list(NULL,pp$cells$source_variable))
  full_sum<-private(".bh_individual_summarize")(pp,list(draws=matrix_all))
  selected_sum<-private(".bh_individual_summarize")(pp,list(draws=matrix_all[c(1,3),,drop=FALSE]))
  ok(abs(full_sum$mean-8)<1e-12&&abs(selected_sum$mean-5)<1e-12&&abs(selected_sum$lower90-1.4)<1e-12&&
    abs(selected_sum$upper90-8.6)<1e-12&&selected_sum$n_draws==2L,
    "all versus selected nonlinear mean and exact type-7 quantiles use their stated empirical set")
  sat<-matrix_all;sat[1L,1L]<-1000
  unavailable<-private(".bh_individual_summarize")(pp,list(draws=sat))
  ok(unavailable$n_draws==4L&&unavailable$n_invalid_states==1L&&is.na(unavailable$mean),
    "invalid selected transforms preserve original denominator without finite-subset summaries")
  ok(identical(before,serialize(fit,NULL))&&identical(had_rng,exists(".Random.seed",.GlobalEnv,inherits=FALSE))&&
     (!had_rng||identical(rng,get(".Random.seed",.GlobalEnv))),
    "streaming controller doubles preserve original fit and RNG across success and refusal")
})

# Separate bounded actual-reader/public-consumer integration. One narrow
# four-chain fixture is reused; only the explicit bad-alias derivative differs.
# Source-only runs may select this phase without replaying the in-memory phase.
if (!identical(Sys.getenv("BMEAS_INDIVIDUAL_STREAMING_PHASE"),"doubles")) local({
  start_passed<-passed
  tmp<-tempfile("bmeas-individual-actual-");dir.create(tmp)
  on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  evidence<-Sys.getenv("BMEAS_INDIVIDUAL_STREAMING_EVIDENCE_DIR")
  if(nzchar(evidence))dir.create(evidence,recursive=TRUE,showWarnings=FALSE)
  d<-expand.grid(person=c("one","two","three"),task=c("early","late"),stringsAsFactors=FALSE)
  d$successes<-c(1L,2L,1L,0L,1L,2L);d$trials<-3L;d$obs<-paste0("b",seq_len(nrow(d)))
  block<-observe(binomial_model(),d,"person","task",fields=list(successes="successes",trials="trials"),
    units="count",observation_id="obs",parameters=list(p="task"))
  spec<-bmeas(bmf(),list(coins=block),fit=FALSE)
  backend_trap<-function(...)stop("forbidden backend access")
  environment(backend_trap)<-baseenv()
  fit<-structure(list(spec=spec,code="SYNTHETIC CSV INTEGRATION: canonical saved map, no fitted or compiled posterior source",
    run=list(chains=4L),backend=list(draws=backend_trap,metadata=backend_trap),
    cache=list(computation=list(status="engineering_only_no_fit"),inference=list(status="not_assessed")),
    provenance=list(model_fingerprint=spec$fingerprint,kind="synthetic_CmdStan_format_consumer_integration_not_posterior_evidence",
      output_files=character())),class=c("bmeas_fit","bhmm_fit"))
  cap<-budget();cap$posterior$max_source_bytes<-1024^2;cap$posterior$max_read_bytes<-7*1024^2
  scales<-c("predictor","natural","received")
  plan<-private(".bh_individual_plan")(fit,scales,cap)
  variables<-plan$required_variables
  target<-plan$cells$target_index[match(variables,plan$cells$source_variable)]
  ids<-c(19L,3L,11L,7L);L<-33L
  raw_value<-function(id,row,target)id/16+row/64+target/8-2
  make_source<-function(id,bad=FALSE) {
    path<-file.path(tmp,paste0(if(bad)"bad-"else"chain-",id,".csv"))
    header<-c("# ENGINEERING ONLY: deterministic invented states; not a fit or posterior validation",
      "# stan_version_major = 2","# stan_version_minor = 40","# stan_version_patch = 0",
      "# model = scalar_components_model",paste("# id =",id),"# method = sample (Default)",
      paste("# num_samples =",L),"# num_warmup = 0","# save_warmup = false (Default)",
      "# thin = 1 (Default)","# algorithm = hmc (Default)","# engine = nuts (Default)",
      "# metric = diag_e (Default)","# sig_figs = 18",
      paste(c("lp__",gsub("\\]","",gsub("[\\[,]",".",variables)),"unused.1"),collapse=","))
    body<-vapply(seq_len(L),function(row) {
      values<-raw_value(id,row,target)
      if(bad&&row==L)values[3L]<-values[3L]+1/8
      paste(c("-Inf",format(values,digits=17,scientific=TRUE,trim=TRUE),"123.5"),collapse=",")
    },"")
    writeLines(c(header,body,"# Elapsed Time: 0 seconds (Warm-up)",
      "# 0.01 seconds (Sampling)","# 0.01 seconds (Total)"),path,useBytes=TRUE)
    path
  }
  paths<-vapply(ids,make_source,"")
  bytes<-sum(file.info(paths)$size)
  ok(bytes<=1024^2&&length(paths)==4L,"actual fixture is four complete narrow chains below the 1 MiB aggregate envelope")
  fit$provenance$output_files<-unname(paths)
  before<-serialize(fit,NULL);cwd<-getwd();kind<-RNGkind()
  had_seed<-exists(".Random.seed",.GlobalEnv,inherits=FALSE)
  seed<-if(had_seed)get(".Random.seed",.GlobalEnv)else NULL
  owned_before<-sort(list.files(tempdir(),pattern="^bmeas-streaming-",full.names=TRUE))
  full<-individual_parameter_summary(fit,scales=scales,budget=cap)
  selected<-individual_parameter_summary(fit,scales=scales,budget=cap,states=list(method="balanced",draws=5L))
  expected_all<-data.frame(chain_id=rep(sort(ids),L),saved_row=rep(seq_len(L),each=4L))
  expected_selected<-data.frame(chain_id=c(3L,7L,11L,19L,3L),saved_row=c(1L,17L,17L,17L,33L))
  ok(identical(attr(full,"draw_keys"),expected_all)&&identical(attr(selected,"draw_keys"),expected_selected),
    "actual public acquisition retains independently specified all and balanced selected joint-state keys")
  oracle<-function(out,keys) {
    for(i in seq_len(nrow(out))) {
      t<-out$target_index[i]
      eta<-raw_value(keys$chain_id,keys$saved_row,t)
      value<-if(out$scale[i]=="natural")stats::plogis(eta)else eta
      want<-c(mean(value),stats::median(value),stats::sd(value),unname(stats::quantile(value,c(.05,.95),type=7)))
      got<-unname(unlist(as.data.frame(out)[i,c("mean","median","sd","lower90","upper90")],use.names=FALSE))
      if(!isTRUE(all.equal(got,want,tolerance=1e-14,check.attributes=FALSE)))return(FALSE)
    }
    TRUE
  }
  ok(oracle(full,expected_all)&&oracle(selected,expected_selected),
    "actual reader and public summaries match independent predictor/logit-natural/received type-7 mean median and SD oracles")
  ok(all(full$n_draws==132L)&&all(selected$n_draws==5L)&&
    attr(selected,"metadata")$selection$full_source_rows==132&&attr(selected,"metadata")$selection$omitted_rows==127,
    "actual full-source and selected empirical denominators remain distinct")
  for(out in list(full,selected)) {
    r<-attr(out,"source_receipt");D<-unique(out$n_draws);V<-length(variables)
    ok(r$status=="acquired"&&identical(r$reader$provider_called,FALSE)&&r$source_rows==132&&
      r$selected_rows==D&&r$selected_required_values==D*V,
      paste("actual receipt binds source/selected/value counts without provider materialization for",D,"states"))
    ok(r$numeric_conversions$validation==132*V&&r$numeric_conversions$projection==D*V&&
      r$required_numeric_conversions==(132+D)*V&&r$lexical_fields$validation==132*(V+2)&&
      r$lexical_fields$projection==132*(V+2),
      paste("actual receipt separates full-source validation and lexical work from selected conversions for",D,"states"))
    ok(r$alias_validation$redundant_columns==4L&&r$alias_validation$comparisons==132*4&&
      identical(r$alias_validation$scope,"all retained source rows")&&
      attr(out,"work")$selected_alias_comparison_terms==D*4,
      paste("actual alias receipt covers every source row plus separately counted selected rechecks for",D,"states"))
    ok(r$consumer_work$requested[["transformation_terms"]]==18*D&&
      r$consumer_work$admitted[["transformation_terms"]]==18*D&&
      r$buffer_schedule[["consumer_per_selected_row:exact_quantile_vectors"]]==2*D&&
      r$buffer_schedule[["consumer_fixed:summary_numeric_columns"]]>0&&
      r$max_materialized_values==sum(unlist(r$buffer_schedule))&&r$max_materialized_values<=cap$posterior$max_materialized_values,
      paste("actual preallocation receipt includes exact consumer work and itemized numeric envelope for",D,"states"))
    ok(r$source_bytes==bytes&&r$read_bytes_charged==7*bytes&&r$read_bytes_measured==4*bytes&&
      r$hash_bytes_reserved==3*bytes&&r$temp_bytes_written==2*bytes&&
      identical(r$expected_chain_context$availability,"retained"),
      paste("actual public receipt keeps seven logical byte passes and saved expected-chain context for",D,"states"))
  }
  permuted<-fit;permuted$provenance$output_files<-rev(fit$provenance$output_files)
  permuted_out<-individual_parameter_summary(permuted,scales=scales,budget=cap,states=list(method="balanced",draws=5L))
  ok(identical(lapply(selected,identity),lapply(permuted_out,identity))&&
    identical(attr(selected,"draw_keys"),attr(permuted_out,"draw_keys")),
    "actual public summaries and keys are invariant to recorded file order")
  # A final row from chain 19 is outside the five-state selection. Only one
  # derivative file is written, and its entire four-file fixture is still tiny.
  bad_fit<-fit;bad_path<-make_source(19L,bad=TRUE)
  bad_fit$provenance$output_files[match(19L,ids)]<-bad_path
  ok(sum(file.info(bad_fit$provenance$output_files)$size)<=1024^2,
    "bad-alias derivative remains within the same bounded fixture size")
  failure<-tryCatch(individual_parameter_summary(bad_fit,scales=scales,budget=cap,
    states=list(method="balanced",draws=5L)),error=identity)
  ok(inherits(failure,"bh_posterior_source_error")&&identical(failure$code,"E_INDIVIDUAL_SHARED_ALIAS")&&
    failure$receipt$status=="failed"&&failure$receipt$numeric_conversions$projection==0,
    "actual public acquisition refuses late unselected alias disagreement before any projection or summaries")
  low<-cap;low$max_transformation_terms<-18*131
  work_failure<-tryCatch(individual_parameter_summary(fit,scales=scales,budget=low),error=identity)
  ok(inherits(work_failure,"bh_posterior_source_error")&&identical(work_failure$code,"E_PPC_SOURCE_CONSUMER_WORK")&&
    work_failure$receipt$projected_rows==0&&
    work_failure$receipt$consumer_work$requested[["transformation_terms"]]==18*132&&
    !"transformation_terms"%in%names(work_failure$receipt$consumer_work$admitted),
    "actual all-row transformation refusal retains requested work without falsely admitting or projecting it")
  rank_fun<-private(".bh_individual_saved_selection_rank")
  fail_rank<-function(message)stop(message,call.=FALSE)
  ok(rank_fun(90000001,90000002,100000001,fail_rank)==99999999&&
     rank_fun(94906265,94906265,94906266,fail_rank)==94906266&&
     rank_fun(4,8,33,fail_rank)==14,
    "passive balanced-rank arithmetic retains exact large-bound integer quotient checks without allocating states")
  saved<-file.path(tmp,"saved-summary.rds");saveRDS(selected,saved)
  unlink(c(paths,bad_path));cold<-readRDS(saved)
  env<-if(source_mode).GlobalEnv else asNamespace("bmeas")
  traps<-c(".bh_posterior_ingest_csv",".bh_individual_ingest_csv",".bh_posterior_policy",".bh_ppc_validate_saved_spec")
  restored<-list();trap_calls<-0L
  for(name in traps) {
    locked<-bindingIsLocked(name,env);restored[[name]]<-list(value=get(name,env),locked=locked)
    if(locked)unlockBinding(name,env)
    assign(name,function(...) {trap_calls<<-trap_calls+1L;stop("passive source or callback access")},env)
  }
  on.exit(for(name in names(restored)) {
    assign(name,restored[[name]]$value,env)
    if(restored[[name]]$locked)lockBinding(name,env)
  },add=TRUE)
  printed<-capture.output(print(cold))
  ok(trap_calls==0L&&any(grepl("Retained source states: 132",printed,fixed=TRUE))&&
    any(grepl("Selected empirical states: 5",printed,fixed=TRUE)),
    "cold saved result prints full and selected counts after source deletion without acquisition policy or spec callbacks")
  ok(identical(before,serialize(fit,NULL)),"actual successes and typed refusals preserve the engineering fit bytes")
  ok(identical(cwd,getwd())&&identical(kind,RNGkind())&&
    identical(had_seed,exists(".Random.seed",.GlobalEnv,inherits=FALSE))&&
    (!had_seed||identical(seed,get(".Random.seed",.GlobalEnv))),
    "actual successes and typed refusals preserve caller RNG and cwd")
  ok(identical(owned_before,sort(list.files(tempdir(),pattern="^bmeas-streaming-",full.names=TRUE))),
    "actual successes and typed refusals clean all owned parser snapshots")
  if(nzchar(evidence)) {
    saveRDS(list(full_receipt=attr(full,"source_receipt"),selected_receipt=attr(selected,"source_receipt"),
      full_keys=attr(full,"draw_keys"),selected_keys=attr(selected,"draw_keys"),
      alias_failure=failure$receipt,work_failure=work_failure$receipt,fixture_source_bytes=bytes,
      status="synthetic_engineering_integration_only_not_posterior_evidence"),file.path(evidence,"actual-receipts.rds"))
    writeLines(printed,file.path(evidence,"actual-cold-print.txt"))
  }
  cat("frontend-individual-streaming actual-reader phase:",passed-start_passed,"checks passed\n")
})
cat("frontend-individual-streaming:",passed,"checks passed\n")
