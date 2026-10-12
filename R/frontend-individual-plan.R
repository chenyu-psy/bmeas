# Saved-map-only individual-parameter planning. No source acquisition, model
# inspection/callback, fingerprint recomputation, posterior cache, or RNG access.
.bh_individual_fail <- function(message, code="E_INDIVIDUAL_LAYOUT") .mp_abort(code,message)

.bh_individual_list <- function(x,label,allow_class=FALSE,allow_null=FALSE) {
  if(is.null(x)&&allow_null)return(list())
  if(typeof(x)!="list"||isS4(x)||(!allow_class&&is.object(x))||
      !is.null(attr(x,"dim",exact=TRUE)))
    .bh_individual_fail(paste(label,"must be a saved plain list"))
  value<-unclass(x);keys<-attr(value,"names",exact=TRUE)
  if(length(value)&&(typeof(keys)!="character"||is.object(keys)||
      !is.null(attributes(keys))||length(keys)!=length(value)||anyNA(keys)||
      any(!nzchar(keys))||anyDuplicated(keys)))
    .bh_individual_fail(paste(label,"has missing, duplicate or ambiguous field names"))
  value
}
.bh_individual_value <- function(x,label,type,scalar=FALSE,missing=FALSE) {
  if(!typeof(x)%in%type||is.object(x)||!is.null(attributes(x))||
      (scalar&&length(x)!=1L))
    .bh_individual_fail(paste(label,"requires a plain saved scalar/vector"))
  if(!missing&&anyNA(x)) .bh_individual_fail(paste(label,"has missing values"))
  x
}
.bh_individual_ids <- function(x,label,unique=FALSE,missing=FALSE) {
  .bh_individual_value(x,label,"character",missing=missing)
  if(any(!is.na(x)&!nzchar(x))||(unique&&anyDuplicated(x)))
    .bh_individual_fail(paste(label,"has empty or duplicate keys"))
  invisible(TRUE)
}
.bh_individual_index <- function(x,upper,label,complete=FALSE) {
  .bh_individual_value(x,label,c("integer","double"))
  if(any(!is.finite(x))||any(x!=floor(x))||any(x<1|x>upper)||
      (complete&&!identical(as.integer(x),seq_len(upper))))
    .bh_individual_fail(paste(label,"has inconsistent saved indices"))
  invisible(TRUE)
}
.bh_individual_nrow <- function(x,label) {
  # Examine attributes before any data.frame accessor or method can run.
  if(typeof(x)!="list"||isS4(x)||!identical(attr(x,"class",exact=TRUE),"data.frame"))
    .bh_individual_fail(paste(label,"must be a saved plain data.frame"))
  keys<-attr(x,"names",exact=TRUE)
  if(typeof(keys)!="character"||is.object(keys)||!is.null(attributes(keys))||
      length(keys)!=length(x)||anyNA(keys)||any(!nzchar(keys))||anyDuplicated(keys))
    .bh_individual_fail(paste(label,"has ambiguous column names"))
  n<-.row_names_info(x,2L)
  if(length(n)!=1L||is.na(n)||n<0L) .bh_individual_fail(paste(label,"has invalid row names"))
  n
}
.bh_individual_frame <- function(x,fields,label,max_rows=Inf) {
  n<-.bh_individual_nrow(x,label)
  if(n>max_rows) .bh_individual_fail(paste(label,"exceeds its published row limit"),"E_INDIVIDUAL_BUDGET")
  cols<-unclass(x)
  if(!all(fields%in%names(cols))) .bh_individual_fail(paste(label,"is missing required columns"))
  out<-vector("list",length(fields));names(out)<-fields
  for(name in fields) {
    z<-cols[[name,exact=TRUE]]
    if(!typeof(z)%in%c("logical","integer","double","character")||is.object(z)||
        !is.null(attributes(z))||length(z)!=n)
      .bh_individual_fail(paste(label,"has a nonplain or inconsistent column",name))
    out[[name]]<-z
  }
  structure(out,class="data.frame",row.names=.set_row_names(n))
}
.bh_individual_budget <- function(budget) {
  if(typeof(budget)!="list"||is.object(budget)||isS4(budget)||
      any(!names(attributes(budget))%in%"names"))
    .bh_individual_fail("an explicit plain named individual-parameter budget is required","E_INDIVIDUAL_BUDGET")
  budget_names<-attr(budget,"names",exact=TRUE)
  if(typeof(budget_names)!="character"||is.object(budget_names)||!is.null(attributes(budget_names))||
      length(budget_names)!=length(budget)||anyNA(budget_names)||any(!nzchar(budget_names))||anyDuplicated(budget_names))
    .bh_individual_fail("individual-parameter budget requires distinct plain field names","E_INDIVIDUAL_BUDGET")
  if("posterior"%in%budget_names) {
    fields<-c("posterior","max_parameter_cells","max_summary_rows","max_transformation_terms",
      "max_declared_observation_rows","max_consumed_observation_rows","max_operand_context_rows","max_context_terms")
    keys<-attr(budget,"names",exact=TRUE)
    if(typeof(keys)!="character"||is.object(keys)||!is.null(attributes(keys))||
        length(keys)!=length(fields)||anyNA(keys)||anyDuplicated(keys)||!setequal(keys,fields))
      .bh_individual_fail("streaming individual budget requires posterior and the seven complete-work limits; legacy acquisition fields cannot be mixed in","E_INDIVIDUAL_BUDGET")
    for(i in seq_along(budget))if(identical(.subset2(budget,i),quote(expr=)))
      .bh_individual_fail(paste("streaming individual budget field",keys[i],"must contain an explicit value"),"E_INDIVIDUAL_BUDGET")
    for(name in setdiff(fields,"posterior")) .bh_individual_count(budget[[name]],name)
    # Pure policy validation only. Column-dependent defaults are resolved later.
    .bh_posterior_consumer_policy(budget$posterior,character())
    return(budget[fields])
  }
  ceiling<-list(max_source_bytes=10485760L,max_posterior_rows=100L,
    max_draw_values=50000L,max_parameter_cells=1000L)
  names_<-attr(budget,"names",exact=TRUE)
  if(typeof(names_)!="character"||is.object(names_)||!is.null(attributes(names_))||
      length(budget)!=4L||length(names_)!=4L||anyNA(names_)||anyDuplicated(names_)||
      !setequal(names_,names(ceiling)))
    .bh_individual_fail("individual-parameter budget needs exactly four distinct supported fields","E_INDIVIDUAL_BUDGET")
  for(name in names(ceiling)) {
    x<-budget[[name,exact=TRUE]];lower<-switch(name,max_source_bytes=1L,max_posterior_rows=2L,0L)
    if(!typeof(x)%in%c("integer","double")||is.object(x)||!is.null(attributes(x))||
        length(x)!=1L||!is.finite(x)||x!=floor(x)||x<lower||x>ceiling[[name]])
      .bh_individual_fail(paste("invalid or excessive individual-parameter budget",name),"E_INDIVIDUAL_BUDGET")
    ceiling[[name]]<-as.integer(x)
  }
  ceiling
}
# Counts remain exact doubles until a checked, representable R dimension is needed.
.bh_individual_count <- function(x,label,dimension=FALSE) {
  if(!typeof(x)%in%c("integer","double")||is.object(x)||!is.null(attributes(x))||
      length(x)!=1L||!is.finite(x)||x<0||x!=floor(x)||x>2^53-1)
    .bh_individual_fail(paste("invalid exact count",label),"E_INDIVIDUAL_BUDGET")
  if(dimension&&x>.Machine$integer.max)
    .bh_individual_fail(paste(label,"is not representable as an R table dimension"),"E_INDIVIDUAL_CAPABILITY")
  as.double(x)
}
.bh_individual_add <- function(...,label="aggregate work") {
  values<-list(...);out<-0
  for(value in values) {
    value<-.bh_individual_count(value,label)
    if(value>2^53-1-out) .bh_individual_fail(paste(label,"exceeds exact count arithmetic"),"E_INDIVIDUAL_CAPABILITY")
    out<-out+value
  }
  out
}
.bh_individual_product <- function(...,label="aggregate work") {
  values<-list(...);out<-1
  for(value in values) {
    value<-.bh_individual_count(value,label)
    if(value!=0&&out>floor((2^53-1)/value))
      .bh_individual_fail(paste(label,"exceeds exact count arithmetic"),"E_INDIVIDUAL_CAPABILITY")
    out<-out*value
    if(out>2^53-1) .bh_individual_fail(paste(label,"exceeds exact count arithmetic"),"E_INDIVIDUAL_CAPABILITY")
  }
  out
}
.bh_individual_states <- function(states,streaming) {
  if(identical(states,"all"))return("all")
  if(!streaming) .bh_individual_fail("an explicit state subset requires a streaming posterior budget","E_INDIVIDUAL_OPTIONS")
  if(typeof(states)!="list"||is.object(states)||isS4(states)||
      any(!names(attributes(states))%in%"names"))
    .bh_individual_fail("states must be all or a plain balanced selection","E_INDIVIDUAL_ARGUMENT")
  keys<-attr(states,"names",exact=TRUE)
  if(typeof(keys)!="character"||is.object(keys)||!is.null(attributes(keys))||
      length(keys)!=2L||anyNA(keys)||anyDuplicated(keys)||!setequal(keys,c("method","draws")))
    .bh_individual_fail("states requires exactly method=balanced and draws","E_INDIVIDUAL_ARGUMENT")
  for(i in seq_along(states))if(identical(.subset2(states,i),quote(expr=)))
    .bh_individual_fail(paste("states field",keys[i],"must contain an explicit value"),"E_INDIVIDUAL_ARGUMENT")
  if(!identical(states$method,"balanced"))
    .bh_individual_fail("states requires exactly method=balanced and draws","E_INDIVIDUAL_ARGUMENT")
  draws<-.bh_individual_count(states$draws,"selected states",dimension=TRUE)
  if(draws<2) .bh_individual_fail("individual summaries require at least two selected states","E_INDIVIDUAL_ARGUMENT")
  list(method="balanced",draws=draws)
}
.bh_individual_work_limits <- function(cap) {
  if(!is.null(cap$posterior))return(cap[setdiff(names(cap),"posterior")])
  list(max_parameter_cells=cap$max_parameter_cells,max_summary_rows=3000L,
    max_transformation_terms=300000L,max_declared_observation_rows=1000L,
    max_consumed_observation_rows=1000L,max_operand_context_rows=10000L,
    max_context_terms=Inf)
}
# Conservative complete-domain work preflight, before coverage or cell expansion.
# Search joins are charged by their full candidate pairs even where R uses a hash.
.bh_individual_context_preflight <- function(tab,N,K,P,J,S,declared,consumed,binding,operands,cap) {
  total<-function(x)do.call(.bh_individual_add,as.list(as.double(x)))
  mul<-.bh_individual_product;add<-.bh_individual_add
  D<-total(declared);U<-total(consumed);B<-total(binding);O<-as.double(operands)
  .bh_individual_count(D,"complete declared context rows",TRUE)
  .bh_individual_count(U,"complete consumed context rows",TRUE)
  I<-.bh_individual_nrow(tab$information,"saved information")
  E<-.bh_individual_nrow(tab$exclusions,"exclusions")
  M<-.bh_individual_nrow(tab$partial_response_exclusions,"partial-response exclusions")
  A<-add(mul(E,P),B) # Every excluded predictor role plus all original bindings.
  combined<-add(O,B,E,M,A,O) # Operand rows, routes, exclusions, attributions, literals.
  cells<-mul(N,P);output<-mul(cells,S)
  .bh_individual_count(cells,"complete parameter cells",TRUE)
  .bh_individual_count(output,"complete summary rows",TRUE)
  .bh_individual_count(combined,"combined operand context rows",TRUE)
  if(D>cap$max_declared_observation_rows||U>cap$max_consumed_observation_rows||
      I>cap$max_consumed_observation_rows||E>cap$max_declared_observation_rows||
      M>cap$max_declared_observation_rows||combined>cap$max_operand_context_rows)
    .bh_individual_fail("complete declared, consumed or combined operand/route/exclusion context exceeds its streaming limit","E_INDIVIDUAL_BUDGET")
  if(cells>cap$max_parameter_cells||output>cap$max_summary_rows)
    .bh_individual_fail("complete person-parameter-scale domain exceeds its streaming limit","E_INDIVIDUAL_BUDGET")
  terms<-list(complete_cell_coverage_search=mul(cells,O),
    block_context_filter_search=mul(length(declared),add(I,E,M,O)),
    declared_partition_joins=mul(D,add(U,E,M)),
    consumed_information_joins=mul(U,I),partial_exclusion_join=mul(M,E),
    operand_consumed_join=mul(O,U),operand_role_and_index_joins=mul(O,add(K,J,K)),
    excluded_declared_join=mul(E,D),excluded_role_search=mul(add(E,B),K),
    binding_exclusion_join=mul(B,E),exclusion_attribution_search=mul(E,A),
    roster_coverage_and_exclusion_joins=mul(N,add(U,E,A)),
    saved_role_domain_joins=mul(4,add(mul(J,K),mul(K,K),mul(J,J))),
    roster_identity_checks=mul(N,N),expanded_cell_target_join=mul(cells,J),
    shared_alias_group_search=mul(P,cells),shared_alias_column_join=mul(cells,cells),
    context_row_expansion=add(D,U,I,E,M,O,B,A,O))
  work<-do.call(add,terms)
  if(work>cap$max_context_terms)
    .bh_individual_fail("complete metadata joins, expansion and cell coverage searches exceed max_context_terms","E_INDIVIDUAL_BUDGET")
  planning_numeric<-list(cell_table_and_expansion=mul(18,cells),
    context_frames_and_explicit_combination=mul(2,add(mul(2,N),mul(3,P),mul(3,J),I,
      mul(3,D),mul(4,O),E,A,mul(2,M))),
    operand_fields_and_per_block_join_indices=mul(24,O),
    observation_join_indices=mul(4,add(D,U,I)),
    exclusion_binding_role_and_roster_indices=add(mul(8,add(E,B,A)),mul(6,K),mul(4,J),mul(4,N)),
    count_and_map_scratch=128)
  planning_peak<-do.call(add,planning_numeric)
  list(context_terms=work,context_terms_by_operation=terms,
    planning_numeric_slots_upper_bound=planning_peak,planning_numeric_slots_by_buffer=planning_numeric,
    operand_context_rows_upper_bound=combined,excluded_attribution_rows_upper_bound=A,
    coverage_rows_upper_bound=O,declared_observation_rows=D,consumed_observation_rows=U)
}
.bh_individual_consumer_buffers <- function(plan) {
  slots<-function(value) {
    if(typeof(value)%in%c("integer","double","logical"))return(as.double(length(value)))
    if(typeof(value)!="list")return(0)
    total<-0
    for(element in unclass(value))total<-.bh_individual_add(total,slots(element),label="saved consumer numeric slots")
    total
  }
  n<-nrow(plan$cells);s<-length(plan$scales)
  output_rows<-.bh_individual_product(n,s,label="summary output rows")
  numeric_columns<-sum(vapply(unclass(plan$cells),function(x)typeof(x)%in%c("integer","double","logical"),logical(1)))
  # Sixteen generated columns: ten double statistics/diagnostics, five integer
  # denominators/invalid counts and one integer quantile type.
  output_slots<-.bh_individual_product(output_rows,numeric_columns+16,label="summary numeric output slots")
  planning_peak<-plan$context$planned_work$planning_numeric_slots_upper_bound
  if(is.null(planning_peak))planning_peak<-0
  list(fixed=c(planning_phase_numeric_slots_bound=planning_peak,
      saved_cells=slots(plan$cells),saved_context=slots(plan$context),
      summary_numeric_columns=output_slots,summary_numeric_column_construction=output_slots,
      output_expansion_indices=output_rows,cell_expansion_indices=as.double(n),
      one_cell_and_scalar_summary_scratch=.bh_individual_add(64,as.double(numeric_columns))),
    per_selected_row=c(raw_cell_series=1,transformed_cell_series=1,
      transform_finite_comparison_vectors=4,exact_quantile_vectors=2,
      exact_median_vectors=2,sd_summary_vectors=2))
}
.bh_individual_scales <- function(scales) {
  if(typeof(scales)!="character"||is.object(scales)||!is.null(attributes(scales))||
      !length(scales)||length(scales)>3L||anyNA(scales)||anyDuplicated(scales)||
      any(!scales%in%c("predictor","natural","received")))
    .bh_individual_fail("select one to three distinct plain predictor, natural or received scales","E_INDIVIDUAL_ARGUMENT")
  scales
}
.bh_individual_evidence <- function(x,label) {
  # Selected saved evidence is validated, never coerced through user methods.
  .bh_fit_report_context_plain(x,label)
  if(typeof(x)=="list") {
    values<-unclass(x);keys<-attr(values,"names",exact=TRUE)
    if(!is.null(keys)&&(anyNA(keys)||any(!nzchar(keys))||anyDuplicated(keys)))
      .bh_individual_fail(paste(label,"has ambiguous evidence fields"))
    for(i in seq_along(values)).bh_individual_evidence(values[[i]],label)
  }
  x
}
.bh_individual_empty_cells <- function() data.frame(
  person_key=character(),person_id=character(),person_index=integer(),
  parameter_key=character(),model=character(),parameter=character(),task=character(),
  declared_level=character(),effective_role=character(),shared_alias_key=character(),
  source_variable=character(),predictor=character(),receives=character(),support=character(),
  units=character(),predictor_units=character(),received_units=character(),
  fixed_received=double(),fixed_predictor=double(),fixed_natural=double(),
  predictor_representation=character(),natural_representation=character(),
  n_likelihood_rows=integer(),n_informative_rows=integer(),target_index=integer(),
  logical_role_index=integer(),has_symbolic_support=logical(),observed_person=logical(),
  coverage_policy=character(),coverage_qualification=character(),stringsAsFactors=FALSE)

.bh_individual_plan <- function(fit,scales,budget,states="all") {
  cap<-.bh_individual_budget(budget);scales<-.bh_individual_scales(scales)
  streaming<-!is.null(cap$posterior);states<-.bh_individual_states(states,streaming)
  limits<-.bh_individual_work_limits(cap)
  portable<-.bh_pf_is_view(fit,"individual_parameter_summary")
  if(typeof(fit)!="list"||isS4(fit)||(!inherits(fit,"bhmm_fit")&&!portable))
    .bh_individual_fail("expected a retained main-API bmeas fit","E_FIT")
  fit<-.bh_individual_list(fit,"fit",allow_class=TRUE)
  original_spec<-fit[["spec",exact=TRUE]]
  if(typeof(original_spec)!="list"||(!inherits(original_spec,"bhmm_spec")&&!(portable&&identical(.bh_pf_class(original_spec),"bmeas_portable_context"))))
    .bh_individual_fail("a retained structured specification is required; cached marginals cannot reconstruct it")
  spec<-.bh_individual_list(original_spec,"specification",allow_class=TRUE)
  tab<-.bh_individual_list(spec$tables,"specification tables")
  mapped<-.bh_individual_list(spec$data_map,"consumed data map")
  consumed<-.bh_individual_list(mapped$blocks,"consumed blocks")
  models<-.bh_individual_list(spec$models,"observation blocks")
  provenance<-.bh_individual_list(fit$provenance,"fit provenance",allow_class=TRUE)
  cache<-.bh_individual_list(fit$cache,"fit cache",allow_null=TRUE)
  .bh_individual_value(spec$fingerprint,"specification fingerprint","character",TRUE)
  .bh_individual_value(provenance$model_fingerprint,"fit model fingerprint","character",TRUE)
  if(!nzchar(spec$fingerprint)||!identical(spec$fingerprint,provenance$model_fingerprint))
    .bh_individual_fail("saved fit and specification identities disagree; historical inputs are not resealed")
  .bh_individual_value(fit$code,"retained fitted source","character",TRUE)
  if(!nzchar(fit$code)) .bh_individual_fail("the fitted source identity was not retained")
  upstream<-list()
  for(name in c("computation","inference","identification","posterior_propriety",
      "posterior_normalizability","execution_profile","backend_qualification")) {
    record<-cache[[name,exact=TRUE]]
    if(!is.null(record)) {
      record<-.bh_individual_list(record,paste("saved",name),allow_class=TRUE)
      status<-record$status
      if(!is.null(status)) .bh_individual_value(status,paste(name,"status"),"character",TRUE)
      if(name%in%c("posterior_propriety","posterior_normalizability")&&
          length(status)&&status%in%c("proven_improper","proven_non_normalizable"))
        .bh_individual_fail("a proved improper or non-normalizable target cannot supply posterior summaries","E_INDIVIDUAL_POSTERIOR")
      .bh_individual_evidence(record,paste("saved",name))
    }
    upstream[name]<-list(record)
  }
  if(is.null(upstream$execution_profile)) {
    upstream$execution_profile<-.bh_individual_evidence(provenance$execution_profile,"execution profile")
  }
  for(name in c("hmc","numerical_evaluations","admissibility")) {
    record<-cache[[name,exact=TRUE]]
    upstream[name]<-list(tryCatch(.bh_individual_evidence(record,paste("saved",name)),
      error=function(e)list(status="unavailable_invalid_saved_evidence",reason=conditionMessage(e))))
  }
  qualification<-provenance$kind
  if(!is.null(qualification)) .bh_individual_value(qualification,"source qualification","character",TRUE)
  else qualification<-"unavailable_in_saved_provenance"
  identity<-list(specification_fingerprint=spec$fingerprint,
    saved_fit_model_fingerprint=provenance$model_fingerprint,
    identity_scope="Association with supplied saved identities; original fit-time CSV, executable and data origin are not authenticated")

  # Count original/consumed rows and operands before projecting or joining them.
  if(!identical(names(models),names(consumed)))
    .bh_individual_fail("saved model and consumed block identities/order disagree")
  block_names<-names(models);.bh_individual_ids(block_names,"block names",TRUE)
  if(streaming).bh_individual_count(length(models),"saved block count",TRUE)
  declared_n<-consumed_n<-integer(length(models));names(declared_n)<-names(consumed_n)<-block_names
  binding_n<-integer(length(models));names(binding_n)<-block_names
  for(b in block_names) {
    block<-.bh_individual_list(models[[b]],paste(b,"block"),allow_class=TRUE)
    declared_n[[b]]<-.bh_individual_nrow(block$data,paste(b,"declared data"))
    consumed_n[[b]]<-.bh_individual_nrow(consumed[[b]],paste(b,"consumed data"))
    if(!is.null(block$parameter_bindings)) {
      binding<-.bh_individual_list(block$parameter_bindings,paste(b,"bindings"),allow_class=TRUE)
      binding_n[[b]]<-.bh_individual_nrow(binding$bindings,paste(b,"original bindings"))
    }
  }
  declared_total<-if(streaming)do.call(.bh_individual_add,as.list(as.double(declared_n)))else sum(as.double(declared_n))
  consumed_total<-if(streaming)do.call(.bh_individual_add,as.list(as.double(consumed_n)))else sum(as.double(consumed_n))
  binding_total<-if(streaming)do.call(.bh_individual_add,as.list(as.double(binding_n)))else sum(as.double(binding_n))
  if(declared_total>limits$max_declared_observation_rows||consumed_total>limits$max_consumed_observation_rows)
    .bh_individual_fail("complete declared and consumed designs exceed their row limits","E_INDIVIDUAL_BUDGET")
  operands_n<-if(is.null(tab$observation_operands))0L else .bh_individual_nrow(tab$observation_operands,"observation operands")
  if(operands_n>limits$max_operand_context_rows||binding_total>limits$max_operand_context_rows)
    .bh_individual_fail("saved operands or original route bindings exceed their row limits","E_INDIVIDUAL_BUDGET")

  roster<-.bh_individual_frame(tab$person_roster,c("person_index","person_id","person_key","observed"),"person roster")
  N<-nrow(roster)
  .bh_individual_ids(roster$person_id,"person IDs",TRUE);.bh_individual_ids(roster$person_key,"person keys",TRUE)
  .bh_individual_index(roster$person_index,N,"roster indices",TRUE)
  .bh_individual_value(roster$observed,"roster observed flags","logical")
  .bh_individual_ids(mapped$subject_keys,"saved subject keys",TRUE)
  if(!N||!identical(mapped$subject_keys,roster$person_key))
    .bh_individual_fail("saved full roster and subject keys disagree")
  person<-.bh_individual_list(tab$person_law,"person law")
  aug<-.bh_individual_list(tab$augmentation,"augmentation")
  .bh_individual_value(person$distribution,"person distribution","character",TRUE)
  .bh_individual_value(person$roster_size,"roster size",c("integer","double"),TRUE)
  .bh_individual_value(aug$type,"augmentation type","character",TRUE)
  iid<-identical(person$distribution,"iid")&&identical(aug$type,"common_plus_parameter_residual")
  centered<-identical(person$distribution,"sample_centered")&&identical(aug$type,"task_cholesky")
  if((!iid&&!centered)||!isTRUE(person$roster_size==N)||(centered&&N<2L))
    .bh_individual_fail("saved person law, roster and augmentation are inconsistent")
  if(centered) {
    .bh_individual_value(aug$isometric_basis,"centered basis","character",TRUE)
    .bh_individual_value(aug$raw_coordinate_variance,"centered coordinate variance",c("integer","double"),TRUE)
  }
  .bh_individual_evidence(person,"person law");.bh_individual_evidence(aug,"augmentation")

  role_fields<-c("key","model","parameter","task","role","level","predictor","receives","support", "units","predictor_units","received_units",
    "fixed_received","fixed_predictor","fixed_natural","predictor_representation","natural_representation")
  roles<-.bh_individual_frame(tab$parameter_roles,role_fields,"parameter roles")
  .bh_individual_ids(roles$key,"logical role keys",TRUE)
  for(name in setdiff(role_fields,c("key","fixed_received","fixed_predictor","fixed_natural")))
    .bh_individual_ids(roles[[name]],paste("role",name))
  for(name in c("fixed_received","fixed_predictor","fixed_natural"))
    .bh_individual_value(roles[[name]],paste("role",name),c("double","integer"),missing=TRUE)
  if(any(!roles$model%in%block_names)||any(!roles$role%in%c("predictor","observation_scale"))||
      any(!roles$level%in%c("subject_task","task","fixed"))||
      any(!roles$predictor%in%c("identity","log","logit"))||
      any(!roles$receives%in%c("eta","theta"))||
      any(!roles$support%in%c("real","positive","unit_interval"))||
      any(duplicated(roles[c("model","parameter","task")])))
    .bh_individual_fail("saved role/scale/structured target fields are inconsistent")
  fixed<-roles$level=="fixed"
  if(any(!is.finite(roles$fixed_received[fixed]))||
      any(!roles$predictor_representation[fixed]%in%c("supplied_exact","available","no_finite_predictor","not_available"))||
      any(!roles$natural_representation[fixed]%in%c("supplied_exact","available","optional_transform_not_representable","not_available")))
    .bh_individual_fail("fixed roles require finite received values and saved representation flags")
  if(any(roles$predictor_representation[!fixed]!="free")||any(roles$natural_representation[!fixed]!="free"))
    .bh_individual_fail("nonfixed logical roles require free representation flags")
  logical_rows<-which(roles$role=="predictor");P<-length(logical_rows)
  cell_count<-if(streaming).bh_individual_product(N,P,label="complete parameter cells")else as.double(N)*P
  if(streaming).bh_individual_count(cell_count,"complete parameter cells",TRUE)
  if(cell_count>cap$max_parameter_cells)
    .bh_individual_fail("the complete original roster by logical-target domain exceeds max_parameter_cells","E_INDIVIDUAL_BUDGET")
  targets<-.bh_individual_frame(tab$targets,c("key","target_index","level","model","parameter","task"),"free predictor targets")
  J<-nrow(targets);.bh_individual_ids(targets$key,"free target keys",TRUE)
  .bh_individual_index(targets$target_index,J,"free target indices",TRUE)
  if(!setequal(targets$key,roles$key[roles$role=="predictor"&!fixed]))
    .bh_individual_fail("free target domain does not equal the nonfixed predictor-role domain")
  tr<-match(targets$key,roles$key)
  for(name in c("level","model","parameter","task"))
    if(!identical(targets[[name]],roles[[name]][tr])) .bh_individual_fail("free target fields disagree with saved roles")
  observation_scales<-.bh_individual_frame(tab$observation_parameters,c("key","model","parameter","task","level"),"observation scale roles")
  .bh_individual_ids(observation_scales$key,"observation scale keys",TRUE)
  if(!setequal(observation_scales$key,roles$key[roles$role=="observation_scale"&!fixed]))
    .bh_individual_fail("observation-scale domain disagrees with nonfixed logical roles")
  sr<-match(observation_scales$key,roles$key)
  for(name in c("level","model","parameter","task"))
    if(!identical(observation_scales[[name]],roles[[name]][sr])) .bh_individual_fail("observation-scale fields disagree with saved roles")
  domain<-.bh_individual_frame(tab$task_target_domain,c("target","level","has_symbolic_support","task_white_target"),"symbolic task domain")
  .bh_individual_value(domain$has_symbolic_support,"symbolic support","logical")
  .bh_individual_value(domain$task_white_target,"task white flags","logical")
  if(!identical(domain$target,targets$key)||!identical(domain$level,targets$level))
    .bh_individual_fail("symbolic target domain disagrees with the authoritative target order")
  .bh_individual_ids(aug$target,"augmentation target keys",TRUE)
  .bh_individual_index(aug$target_index,J,"augmentation target indices")
  if(anyDuplicated(aug$target_index)||!identical(aug$target,targets$key[aug$target_index]))
    .bh_individual_fail("augmentation target indices disagree with saved keys")
  active<-targets$key[targets$level=="subject_task"&domain$has_symbolic_support]
  if((iid&&(length(aug$target)||any(domain$task_white_target)))||
      (centered&&(!setequal(aug$target,active)||!identical(domain$task_white_target,targets$key%in%aug$target))))
    .bh_individual_fail("augmentation does not preserve the saved symbolic person domain")
  if(centered&&(!identical(aug$isometric_basis,"Stan2.40_sum_to_zero_positive_Helmert")||
      !isTRUE(aug$raw_coordinate_variance==N/(N-1))))
    .bh_individual_fail("saved centered basis/normalization is unsupported")

  preflight<-if(streaming).bh_individual_context_preflight(tab,N,nrow(roles),P,J,length(scales),
    declared_n,consumed_n,binding_n,operands_n,limits)else NULL
  if(streaming&&preflight$planning_numeric_slots_upper_bound>
      .bh_posterior_policy(cap$posterior,character())$limits$max_materialized_values)
    .bh_individual_fail("metadata planning numeric buffers exceed max_materialized_values before cell/coverage expansion","E_INDIVIDUAL_BUDGET")
  coverage<-.bh_individual_coverage(spec,models,consumed,roles,roster,declared_n,consumed_n,operands_n,
    limits=limits,streaming=streaming)
  context<-c(list(person_law=person,person_roster=roster,augmentation=aug,
    logical_parameters=roles[logical_rows,,drop=FALSE],symbolic_target_domain=domain),coverage$context)
  context$work_limits<-list(max_declared_observation_rows=1000L,max_consumed_observation_rows=1000L,
    max_operand_context_rows=10000L,max_summary_rows=3000L,max_transformation_terms=300000L)
  context$planned_work<-list(parameter_cells=as.integer(cell_count),summary_rows=as.integer(cell_count*length(scales)),
    declared_observation_rows=sum(declared_n),consumed_observation_rows=sum(consumed_n),
    saved_operand_rows=operands_n,original_route_binding_rows=sum(binding_n))
  if(streaming) {
    context$work_limits<-limits
    context$planned_work$parameter_cells<-cell_count
    context$planned_work$summary_rows<-.bh_individual_product(cell_count,length(scales))
    context$planned_work<-c(context$planned_work,preflight[c("context_terms","context_terms_by_operation",
      "operand_context_rows_upper_bound","excluded_attribution_rows_upper_bound","coverage_rows_upper_bound",
      "planning_numeric_slots_upper_bound","planning_numeric_slots_by_buffer")])
  }
  cells<-.bh_individual_empty_cells()
  if(P) {
    rr<-roles[rep(logical_rows,each=N),,drop=FALSE];nn<-rep(seq_len(N),P)
    ti<-match(rr$key,targets$key);support<-domain$has_symbolic_support[ti]
    fixed<-rr$level=="fixed";support[fixed]<-FALSE
    effective<-ifelse(fixed,"fixed_by_declaration",ifelse(rr$level=="task","shared_task_predictor",
      ifelse(support,"person_specific_predictor","shared_mean_only_predictor")))
    shared<-effective%in%c("shared_task_predictor","shared_mean_only_predictor")
    cells<-data.frame(person_key=roster$person_key[nn],person_id=roster$person_id[nn],person_index=as.integer(nn),
      parameter_key=rr$key,model=rr$model,parameter=rr$parameter,task=rr$task,
      declared_level=rr$level,effective_role=effective,shared_alias_key=ifelse(shared,rr$key,NA_character_),
      source_variable=ifelse(fixed,NA_character_,paste0("eta[",nn,",",ti,"]")),predictor=rr$predictor,receives=rr$receives,
      support=rr$support,units=rr$units,predictor_units=rr$predictor_units,received_units=rr$received_units,
      fixed_received=as.double(rr$fixed_received),fixed_predictor=as.double(rr$fixed_predictor),fixed_natural=as.double(rr$fixed_natural),
      predictor_representation=rr$predictor_representation,natural_representation=rr$natural_representation,
      n_likelihood_rows=integer(nrow(rr)),n_informative_rows=integer(nrow(rr)),target_index=as.integer(ti),
      logical_role_index=rep(as.integer(logical_rows),each=N),has_symbolic_support=support,observed_person=roster$observed[nn],
      coverage_policy=rep("no_local_consumed_rows",nrow(rr)),
      coverage_qualification=rep("No local likelihood row; not a prior-only or identification claim",nrow(rr)),stringsAsFactors=FALSE)
    rows<-coverage$rows
    for(i in seq_len(nrow(cells))) {
      at<-which(rows$person_index==cells$person_index[i]&rows$parameter_key==cells$parameter_key[i])
      cells$n_likelihood_rows[i]<-length(at)
      cells$n_informative_rows[i]<-sum(rows$informative[at])
      if(length(at)) {
        cells$coverage_policy[i]<-paste(unique(rows$policy[at]),collapse="; ")
        cells$coverage_qualification[i]<-if(any(rows$policy[at]=="observed_rows_information_not_qualified"))
          "Saved consumed-row screen; parameter information/identification not qualified" else
          "Saved row-policy informative count; not a parameter-specific identification claim"
      }
    }
  }
  required<-cells$source_variable[!is.na(cells$source_variable)]
  if(anyDuplicated(required)) .bh_individual_fail("logical targets map to duplicate source scalars")
  if(!streaming&&length(required)&&2*length(required)>cap$max_draw_values)
    .bh_individual_fail("max_draw_values cannot contain the minimum two rows for every required scalar","E_INDIVIDUAL_BUDGET")
  paths<-if(P)provenance$output_files else character()
  # Missing sources are refused by acquisition; no-predictor plans need no paths.
  if(P&&!is.null(paths)) {
    .bh_individual_ids(paths,"source paths",TRUE)
    if(!streaming&&length(paths)>2L) .bh_individual_fail("at most two distinct saved source paths are supported","E_PPC_SOURCE_LAYOUT")
  }
  context$planned_work$required_columns<-length(required)
  if(streaming) {
    policy<-.bh_posterior_policy(cap$posterior,required)
    context$posterior_policy<-policy
    context$planned_work$maximum_source_rows<-policy$limits$max_source_rows
    context$planned_work$source_compatibility<-"Pending explicit acquisition; saved metadata does not establish source support"
  } else context$planned_work$maximum_admissible_source_rows<-as.integer(if(length(required))
    min(cap$max_posterior_rows,floor(cap$max_draw_values/length(required))) else cap$max_posterior_rows)
  out<-list(cells=cells,required_variables=required,scales=scales,budget=cap,context=context,
    identity=identity,output_files=paths,source_qualification=qualification,upstream=upstream,
    original_spec=original_spec,saved_source_code=fit$code,
    status=if(P)"planned"else"not_applicable_no_predictor_parameters")
  if(streaming) {
    aliases<-unique(cells$shared_alias_key[!is.na(cells$shared_alias_key)])
    out$alias_groups<-lapply(aliases,function(alias)as.integer(match(
      cells$source_variable[!is.na(cells$shared_alias_key)&cells$shared_alias_key==alias],required)))
    out$alias_groups<-out$alias_groups[lengths(out$alias_groups)>1L]
    out$states<-states
    out$consumer_buffers<-.bh_individual_consumer_buffers(out)
    out$consumer_work<-list(per_selected_row=c(transformation_terms=
      .bh_individual_product(cell_count,length(scales))),
      limits=c(transformation_terms=as.double(limits$max_transformation_terms)))
    out$maximum_selected_rows<-if(cell_count)floor(limits$max_transformation_terms/
      .bh_individual_product(cell_count,length(scales)))else 0
    if(cell_count&&out$maximum_selected_rows<2)
      .bh_individual_fail("max_transformation_terms cannot cover two selected states for the complete cell/scale domain","E_INDIVIDUAL_BUDGET")
    if(is.list(states)&&.bh_individual_product(states$draws,cell_count,length(scales))>limits$max_transformation_terms)
      .bh_individual_fail("selected-state transformations exceed max_transformation_terms","E_INDIVIDUAL_BUDGET")
  }
  out
}

.bh_individual_coverage <- function(spec,models,consumed,roles,roster,declared_n,consumed_n,operands_n,
    limits=list(max_declared_observation_rows=1000L,max_consumed_observation_rows=1000L,
      max_operand_context_rows=10000L),streaming=FALSE) {
  tab<-.bh_individual_list(spec$tables,"specification tables");N<-nrow(roster)
  info<-.bh_individual_frame(tab$information,c("block","observation_id","informative","policy"),"saved information",limits$max_consumed_observation_rows)
  .bh_individual_ids(info$block,"information block");.bh_individual_ids(info$observation_id,"information row")
  .bh_individual_ids(info$policy,"information policy");.bh_individual_value(info$informative,"information flags","logical")
  if(any(duplicated(info[c("block","observation_id")]))||any(!info$block%in%names(models)))
    .bh_individual_fail("saved information has duplicate or unknown row keys")
  exclusions<-.bh_individual_frame(tab$exclusions,c(".block",".observation_id",".person_key"),"exclusions",limits$max_declared_observation_rows)
  .bh_individual_ids(exclusions$.block,"exclusion block");.bh_individual_ids(exclusions$.observation_id,"exclusion row")
  .bh_individual_ids(exclusions$.person_key,"exclusion person")
  if(any(duplicated(exclusions[c(".block",".observation_id")]))||any(!exclusions$.block%in%names(models)))
    .bh_individual_fail("saved exclusions have duplicate or unknown row keys")
  partial_n<-.bh_individual_nrow(tab$partial_response_exclusions,"partial-response exclusions")
  partial<-if(partial_n).bh_individual_frame(tab$partial_response_exclusions,
    c("block","observation_id","missing_response_cells","observed_response_cells_discarded"),"partial-response exclusions",limits$max_declared_observation_rows) else
    data.frame(block=character(),observation_id=character(),missing_response_cells=integer(),observed_response_cells_discarded=integer())
  if(nrow(partial)) {
    .bh_individual_ids(partial$block,"partial exclusion block");.bh_individual_ids(partial$observation_id,"partial exclusion row")
    for(name in c("missing_response_cells","observed_response_cells_discarded"))
      .bh_individual_index(partial[[name]],.Machine$integer.max,paste("partial exclusion",name))
    if(any(duplicated(partial[c("block","observation_id")]))||any(!partial$block%in%names(models)))
      .bh_individual_fail("partial exclusions have duplicate or unknown row keys")
  }
  op_fields<-c("block","observation_id","route","argument","target","role","level","receives","predictor","operand","received_constant","eta_index","observation_scale_index","value_source")
  if(is.null(tab$observation_operands)&&!sum(consumed_n)) {
    ops<-as.data.frame(stats::setNames(rep(list(character()),length(op_fields)),op_fields),stringsAsFactors=FALSE)
    ops$received_constant<-double();ops$eta_index<-ops$observation_scale_index<-integer()
  } else ops<-.bh_individual_frame(tab$observation_operands,op_fields,"observation operands",limits$max_operand_context_rows)
  for(name in c("block","observation_id","argument","role","level","receives","predictor","operand","value_source"))
    .bh_individual_ids(ops[[name]],paste("operand",name))
  .bh_individual_ids(ops$route,"operand route",missing=TRUE);.bh_individual_ids(ops$target,"operand target",missing=TRUE)
  for(name in c("received_constant","eta_index","observation_scale_index"))
    .bh_individual_value(ops[[name]],paste("operand",name),c("integer","double"),missing=TRUE)
  if(any(!ops$block%in%names(models))||any(duplicated(ops[c("block","observation_id","argument")])))
    .bh_individual_fail("saved operand arguments are duplicated or have unknown blocks")
  empty_rows<-data.frame(block=character(),observation_id=character(),person_key=character(),person_index=integer(),
    parameter_key=character(),informative=logical(),policy=character(),stringsAsFactors=FALSE)
  rows<-list();row_context<-list();excluded_targets<-list();literal_context<-list()
  total_attributions<-if(streaming)0 else 0L
  for(b in names(models)) {
    block<-.bh_individual_list(models[[b]],paste(b,"block"),allow_class=TRUE)
    declared<-.bh_individual_frame(block$data,c(".observation_id",".task"),paste(b,"declared row keys"),limits$max_declared_observation_rows)
    .bh_individual_ids(declared$.observation_id,paste(b,"declared row IDs"),TRUE)
    .bh_individual_ids(declared$.task,paste(b,"declared task"))
    d<-.bh_individual_frame(consumed[[b]],c(".observation_id",".person_key",".person_index"),paste(b,"consumed row keys"),limits$max_consumed_observation_rows)
    .bh_individual_ids(d$.observation_id,paste(b,"consumed row IDs"),TRUE)
    .bh_individual_ids(d$.person_key,paste(b,"consumed person keys"))
    .bh_individual_index(d$.person_index,N,paste(b,"consumed person indices"))
    if(!identical(d$.person_key,roster$person_key[d$.person_index]))
      .bh_individual_fail("consumed row identities disagree with the original person roster")
    ix<-which(info$block==b);ex<-exclusions[exclusions$.block==b,,drop=FALSE]
    if(!setequal(info$observation_id[ix],d$.observation_id)||
        any(d$.observation_id%in%ex$.observation_id)||
        !setequal(c(d$.observation_id,ex$.observation_id),declared$.observation_id))
      .bh_individual_fail("consumed, informative, excluded and original row identities do not partition the saved declaration")
    bi<-info[ix[match(d$.observation_id,info$observation_id[ix])],,drop=FALSE]
    pp<-partial[partial$block==b,,drop=FALSE]
    if(any(!pp$observation_id%in%ex$.observation_id))
      .bh_individual_fail("partial-response exclusions are not saved excluded rows")
    bo<-ops[ops$block==b,,drop=FALSE]
    at<-match(bo$observation_id,d$.observation_id)
    if(anyNA(at)||!setequal(bo$observation_id,d$.observation_id))
      .bh_individual_fail("saved operands must cover exactly the consumed row domain")
    is_literal<-bo$operand=="route_literal";ordinary<-which(!is_literal)
    ri<-match(bo$target[ordinary],roles$key)
    if(anyNA(ri)) .bh_individual_fail("operand target is outside saved logical roles")
    rr<-roles[ri,,drop=FALSE]
    if(any(rr$model!=b)||any(bo$role[ordinary]!=rr$role)||any(bo$level[ordinary]!=rr$level)||
        any(bo$receives[ordinary]!=rr$receives)||any(bo$predictor[ordinary]!=rr$predictor))
      .bh_individual_fail("saved operand role/scale fields disagree with the logical target")
    target_index<-match(rr$key,spec$tables$targets$key)
    scale_index<-match(rr$key,spec$tables$observation_parameters$key)
    expected<-ifelse(rr$level=="fixed","fixed_received",ifelse(rr$role=="observation_scale","free_observation_scale",
      ifelse(rr$level=="task","task_predictor","individual_predictor")))
    if(any(bo$operand[ordinary]!=expected)) .bh_individual_fail("saved operand source kind disagrees with its logical role")
    fi<-which(rr$level=="fixed");pr<-which(rr$role=="predictor"&rr$level!="fixed");ob<-which(rr$role=="observation_scale"&rr$level!="fixed")
    if(length(fi)&&(!identical(bo$received_constant[ordinary[fi]],rr$fixed_received[fi])||
        any(!is.finite(bo$received_constant[ordinary[fi]])))) .bh_individual_fail("fixed received operands disagree with the saved binding")
    if(length(pr)).bh_individual_index(bo$eta_index[ordinary[pr]],nrow(spec$tables$targets),"operand eta indices")
    if(length(ob)).bh_individual_index(bo$observation_scale_index[ordinary[ob]],nrow(spec$tables$observation_parameters),"operand observation-scale indices")
    if((length(pr)&&!identical(as.integer(bo$eta_index[ordinary[pr]]),as.integer(target_index[pr])))||
        (length(ob)&&!identical(as.integer(bo$observation_scale_index[ordinary[ob]]),as.integer(scale_index[ob]))))
      .bh_individual_fail("saved operand scalar indices disagree with authoritative role indices")
    if(any(is_literal)) {
      lit<-bo[is_literal,,drop=FALSE]
      if(any(!is.na(lit$target))||any(lit$role!="literal_route_constant")||any(lit$level!="fixed")||
          any(lit$value_source!="intrinsic_route_literal")||any(lit$receives!="eta")||
          any(!lit$predictor%in%c("identity","log","logit"))||any(!is.finite(lit$received_constant))||anyNA(lit$route))
        .bh_individual_fail("intrinsic route literal context is inconsistent")
      li<-at[is_literal]
      literal_context[[b]]<-data.frame(block=b,observation_id=lit$observation_id,person_key=d$.person_key[li],person_index=as.integer(d$.person_index[li]),
        route=lit$route,argument=lit$argument,received_value=lit$received_constant,received_scale=rep("predictor",nrow(lit)),
        predictor=lit$predictor,receives=lit$receives,source_role=lit$value_source,stringsAsFactors=FALSE)
    }
    selected<-ordinary[rr$role=="predictor"]
    if(length(selected)) {
      take<-at[selected]
      rows[[b]]<-unique(data.frame(block=b,observation_id=bo$observation_id[selected],person_key=d$.person_key[take],
        person_index=as.integer(d$.person_index[take]),parameter_key=bo$target[selected],informative=bi$informative[take],policy=bi$policy[take],stringsAsFactors=FALSE))
    }
    row_context[[b]]<-data.frame(block=b,observation_id=declared$.observation_id,declared_task=declared$.task,
      consumed=declared$.observation_id%in%d$.observation_id,excluded=declared$.observation_id%in%ex$.observation_id,
      partially_missing=declared$.observation_id%in%pp$observation_id,stringsAsFactors=FALSE)
    if(nrow(ex)) {
      # Only saved structured maps can attribute excluded routes. Display task
      # strings alone never define a routed target.
      model<-.bh_individual_list(block$model,paste(b,"observation model"),allow_class=TRUE)
      adapter<-.bh_individual_list(model$adapter,paste(b,"adapter"),allow_null=TRUE)
      if(!is.null(adapter$mode)).bh_individual_value(adapter$mode,"saved adapter mode","character",TRUE)
      routed<-identical(adapter$mode,"routed")||any(!is.na(bo$route))||!is.null(block$parameter_bindings)
      if(!routed) {
        attrib<-lapply(seq_len(nrow(ex)),function(i) {
          role_at<-which(roles$model==b&roles$role=="predictor"&
            roles$task==declared$.task[match(ex$.observation_id[i],declared$.observation_id)])
          total_attributions<<-total_attributions+length(role_at)
          if(total_attributions+operands_n>limits$max_operand_context_rows) .bh_individual_fail(paste("operand and exclusion context expansion exceeds",limits$max_operand_context_rows,"rows"),"E_INDIVIDUAL_BUDGET")
          data.frame(block=rep(b,length(role_at)),observation_id=rep(ex$.observation_id[i],length(role_at)),
            person_key=rep(ex$.person_key[i],length(role_at)),parameter_key=roles$key[role_at],
            attribution=rep("saved_nonrouted_structured_task_role",length(role_at)),stringsAsFactors=FALSE)
        })
        if(length(attrib))excluded_targets[[b]]<-do.call(rbind,attrib)
      } else if(!is.null(block$parameter_bindings)) {
        binding<-.bh_individual_list(block$parameter_bindings,paste(b,"bindings"),allow_class=TRUE)
        bindings<-.bh_individual_frame(binding$bindings,c("observation_id","kind","target_parameter","target_task"),paste(b,"original bindings"),limits$max_operand_context_rows)
        .bh_individual_ids(bindings$observation_id,paste(b,"binding row"))
        .bh_individual_ids(bindings$kind,paste(b,"binding kind"))
        .bh_individual_ids(bindings$target_parameter,paste(b,"binding parameter"),missing=TRUE)
        .bh_individual_ids(bindings$target_task,paste(b,"binding task"),missing=TRUE)
        bb<-bindings[bindings$observation_id%in%ex$.observation_id&bindings$kind=="target",,drop=FALSE]
        attrib<-lapply(seq_len(nrow(bb)),function(i) {
          role_at<-which(roles$model==b&roles$parameter==bb$target_parameter[i]&roles$task==bb$target_task[i])
          if(length(role_at)!=1L) .bh_individual_fail("saved excluded binding does not resolve one structured logical target")
          if(roles$role[role_at]!="predictor")return(NULL)
          total_attributions<<-total_attributions+1L
          if(total_attributions+operands_n>limits$max_operand_context_rows) .bh_individual_fail(paste("operand and exclusion context expansion exceeds",limits$max_operand_context_rows,"rows"),"E_INDIVIDUAL_BUDGET")
          data.frame(block=b,observation_id=bb$observation_id[i],person_key=ex$.person_key[match(bb$observation_id[i],ex$.observation_id)],
            parameter_key=roles$key[role_at],attribution="saved_original_routed_binding",stringsAsFactors=FALSE)
        })
        if(length(attrib))excluded_targets[[b]]<-unique(do.call(rbind,attrib))
      }
    }
  }
  rows<-if(length(rows))do.call(rbind,rows)else empty_rows;rownames(rows)<-NULL
  if(!identical(roster$observed,roster$person_index%in%unlist(lapply(consumed,function(d)unclass(d)[[".person_index",exact=TRUE]]),use.names=FALSE)))
    .bh_individual_fail("roster observed flags disagree with saved consumed rows")
  literals<-if(length(literal_context))do.call(rbind,literal_context)else data.frame(block=character(),observation_id=character(),person_key=character(),person_index=integer(),route=character(),argument=character(),received_value=double(),received_scale=character(),predictor=character(),receives=character(),source_role=character())
  attributed<-if(length(excluded_targets))do.call(rbind,excluded_targets)else NULL
  if(is.null(attributed))attributed<-data.frame(block=character(),observation_id=character(),person_key=character(),parameter_key=character(),attribution=character())
  attributed$person_index<-as.integer(match(attributed$person_key,roster$person_key))
  attributed$person_id<-roster$person_id[attributed$person_index]
  exclusion_context<-data.frame(block=exclusions$.block,observation_id=exclusions$.observation_id,person_key=exclusions$.person_key,
    person_index=as.integer(match(exclusions$.person_key,roster$person_key)),stringsAsFactors=FALSE)
  exclusion_context$target_attribution_status<-vapply(seq_len(nrow(exclusion_context)),function(i)
    if(any(attributed$block==exclusion_context$block[i]&attributed$observation_id==exclusion_context$observation_id[i]))
      "available_from_saved_structured_map" else "unavailable_no_saved_predictor_target_join",character(1))
  for(x in c("literals","attributed","exclusion_context")) {
    value<-get(x);rownames(value)<-NULL;assign(x,value)
  }
  missing<-.bh_individual_evidence(spec$missing,"saved missingness policy")
  list(rows=rows,context=list(coverage_rows=rows,information=info,declared_rows=do.call(rbind,row_context),
    route_literals=literals,exclusions=exclusion_context,excluded_target_attribution=attributed,
    partial_response_exclusions=partial,missing_policy=missing,
    missing_assumption=.bh_individual_evidence(spec$data_map$missing_assumption,"missing assumption"),
    response_missing_semantics=.bh_individual_evidence(spec$response_missing_semantics,"response missing semantics"),
    exclusion_scope="Excluded targets are attributed only by retained structured maps; unavailable routed joins remain block/row context",
    coverage_scope="Unique consumed block/observation/person/target rows; saved informative policy is not parameter-specific identification, and zero local rows is not a prior-only claim"))
}
