# Resource policy is execution metadata. It never participates in a scientific
# specification fingerprint, a source identity, or the prior measure.
.bh_count <- function(x, name, lower=0, upper=2^53-1, code="E_RESOURCE_POLICY") {
  if(!typeof(x)%in%c("integer","double") || is.object(x) || !is.null(attributes(x)) ||
     length(x)!=1L || !is.finite(x) || x<lower || x>upper || x!=floor(x))
    .mp_abort(code,paste(name,"must be a plain exactly represented integer between",lower,"and",upper))
  as.double(x)
}
.bh_count_sum <- function(values, name="aggregate count") {
  if(!typeof(values)%in%c("integer","double")||is.object(values)||length(setdiff(names(attributes(values)),"names")))
    .mp_abort("E_RESOURCE_ESTIMATE",paste(name,"must be a plain numeric vector of counts"))
  total<-0
  for(x in values) {
    x<-.bh_count(unname(x),name,code="E_RESOURCE_ESTIMATE")
    if(total>(2^53-1)-x).mp_abort("E_RESOURCE_ESTIMATE",paste(name,"exceeds the exact integer range before allocation"))
    total<-total+x
  }
  total
}
.bh_count_product <- function(..., name="aggregate count") {
  values<-list(...);total<-1
  for(x in values) {
    x<-.bh_count(x,name,code="E_RESOURCE_ESTIMATE")
    if(x==0)total<-0 else {
      if(total>floor((2^53-1)/x)).mp_abort("E_RESOURCE_ESTIMATE",paste(name,"exceeds the exact integer range before allocation"))
      total<-total*x
      if(!is.finite(total)||total>2^53-1).mp_abort("E_RESOURCE_ESTIMATE",paste(name,"is not exactly representable"))
    }
  }
  total
}
.bh_backend_integer <- function(x,name,lower=1L) {
  as.integer(.bh_count(x,name,lower,.Machine$integer.max,"E_RUN_CONFIG"))
}
.bh_resource_named_list <- function(x,label,fields,code,allow_empty=FALSE) {
  if(typeof(x)!="list"||is.object(x)||length(setdiff(names(attributes(x)),"names")))
    .mp_abort(code,paste(label,"must be a plain named list"))
  keys<-attr(x,"names",exact=TRUE)
  if(allow_empty&&!length(x)&&is.null(keys))return(x)
  # Names can themselves carry methods. Check their representation before
  # anyNA(), matching, or any other operation which could dispatch a method.
  if(typeof(keys)!="character"||!is.null(attributes(keys))||length(keys)!=length(x)||
     anyNA(keys)||any(!validEnc(keys))||any(!nzchar(keys))||anyDuplicated(keys)||any(!keys%in%fields))
    .mp_abort(code,paste(label,"requires distinct plain recognized names"))
  # alist() can retain R's missing-argument symbol. Test it directly, without
  # assigning/forcing it or evaluating any other expression stored in a field.
  for(i in seq_along(x))if(identical(.subset2(x,i),quote(expr=)))
    .mp_abort(code,paste(label,"field",keys[i],"must contain an explicit value"))
  x
}
.bh_resource_policy <- function(resources) {
  version<-"resource_policy_v1.2"
  if(is.null(resources))return(list(version=version,kind="required",origin="not_supplied",cache=NULL,limits=list()))
  if(identical(resources,"development"))return(list(version=version,kind="development",origin="explicit_development_preset",cache="eager",
    limits=list(max_output_bytes=10485760,cpu_budget=2,
      max_subjects=30,max_observations=1000,max_saved_draws=100,max_targets=12,
      max_observation_scales=12,max_latent_coordinates=12,max_coefficient_primitives=144)))
  fields<-c("max_output_bytes","cache","max_cache_draw_values","cpu_budget","max_subjects","max_observations",
    "max_saved_draws","max_targets","max_observation_scales","max_latent_coordinates","max_coefficient_primitives")
  resources<-.bh_resource_named_list(resources,"resources",fields,"E_RESOURCE_POLICY")
  if(!all(c("max_output_bytes","cache")%in%names(resources)))
    .mp_abort("E_RESOURCE_POLICY","resources requires max_output_bytes and cache")
  mode<-resources$cache
  if(!is.character(mode)||!is.null(attributes(mode))||length(mode)!=1L||is.na(mode)||!mode%in%c("eager","deferred"))
    .mp_abort("E_RESOURCE_POLICY","resources$cache must be exactly eager or deferred")
  if(mode=="eager"&&!"max_cache_draw_values"%in%names(resources))
    .mp_abort("E_RESOURCE_POLICY","eager cache requires a finite max_cache_draw_values allowance; deferred performs no posterior-value materialization")
  limits<-resources[setdiff(names(resources),"cache")]
  zero<-c("max_targets","max_observation_scales","max_latent_coordinates","max_coefficient_primitives")
  for(name in names(limits))limits[[name]]<-.bh_count(limits[[name]],paste0("resources$",name),
    lower=if(name%in%zero)0 else 1,upper=if(name=="cpu_budget").Machine$integer.max else 2^53-1)
  list(version=version,kind="caller_resources",origin="explicit_caller_policy",cache=mode,limits=limits)
}
.bh_resource_dimensions <- function(spec) {
  rows<-function(x)if(is.null(x))0 else as.double(nrow(x))
  tb<-spec$tables
  N<-as.double(length(spec$data_map$subject_keys));J<-rows(tb$targets);K<-rows(tb$components);U<-rows(tb$parameter_residuals)
  target_count<-if(is.null(tb$parameter_roles))J else as.double(sum(tb$parameter_roles$role=="predictor"))
  nrows<-vapply(spec$data_map$blocks,function(d)as.double(nrow(d)),numeric(1))
  O<-.bh_count_sum(nrows,"consumed likelihood rows")
  task<-.bh_task_augmentation(spec);JT<-if(task)as.double(length(tb$augmentation$target))else 0
  loads<-.bh_free_loadings(spec);L<-rows(loads);A<-as.double(sum(loads$sign=="positive_anchor"));CS<-rows(.bh_free_component_sd(spec));B<-rows(.bh_amplitudes(spec))
  T<-rows(tb$share_targets);H<-rows(tb$share_coefficients);W<-rows(tb$share_parts)
  cp<-function(...) .bh_count_product(...,name="sampled white coordinates")
  white<-c(common=if(task)0 else cp(K,N),parameter=if(task)0 else cp(U,N),task=if(task)cp(JT,max(0,N-1))else 0)
  payload<-lapply(names(spec$models),function(name) {
    schema<-spec$models[[name]]$model$data_schema
    widths<-vapply(schema,function(f).bh_count(f$width,"schema field width",1,.Machine$integer.max),numeric(1))
    response<-vapply(schema,function(f)identical(f$role,"response"),logical(1))
    n<-nrows[[name]]
    data.frame(block=name,consumed_rows=n,response_values=.bh_count_product(n,.bh_count_sum(widths[response]),name="response payload"),
      schema_values=.bh_count_product(n,.bh_count_sum(widths),name="schema payload"),stringsAsFactors=FALSE)
  })
  payload<-if(length(payload))do.call(rbind,payload)else data.frame(block=character(),consumed_rows=numeric(),response_values=numeric(),schema_values=numeric())
  E<-as.double(.bh_cor_primitive_count(tb$correlations));EU<-as.double(.bh_cor_primitive_count(tb$parameter_correlations));RB<-rows(tb$raw_correlation_coordinates)
  population<-.bh_count_sum(c(CS,T,H,B,J,L,rows(tb$observation_parameters),if(.bh_raw_cor(spec))RB else E,rows(.bh_free_parameter_sd(spec)),EU),"population parameter dimensions")
  dims<-list(N=N,O=O,J=J,K=K,U=U,JT=JT,L=L,A=A,F=L-A,CS=CS,B=B,T=T,H=H,W=W,S=rows(tb$observation_parameters),
    UF=rows(.bh_free_parameter_sd(spec)),E=E,EU=EU,RB=RB,
    logical_targets=target_count,latent_coordinates=.bh_count_sum(c(K,U)),
    coefficient_primitives=.bh_count_sum(c(L,CS,B,T,H)),sampled_white_coordinates=white,
    total_sampled_white_coordinates=.bh_count_sum(white),
    population_parameter_count=population,prior_parameter_count=.bh_count_sum(tb$priors$parameter_count,"resolved prior parameter count"),
    total_sampled_parameter_dimension=.bh_count_sum(c(population,white),"sampled parameter dimensions"),
    constrained_task_white_values=if(task)cp(JT,N)else 0,
    task_augmentation=task,raw_common=.bh_raw_cor(spec),consumed_rows_by_block=nrows,payload_by_block=payload,
    response_values=.bh_count_sum(payload$response_values),schema_values=.bh_count_sum(payload$schema_values),
    likelihood_rows_per_sweep=O,likelihood_work="Row counts and fixed-width payload counts do not measure density cost, number of gradients, wall time or threaded speedup",
    individual_cells=.bh_count_product(N,target_count,name="individual target cells"),
    declared_rows=.bh_count_sum(vapply(spec$models,function(b)as.double(nrow(b$data)),numeric(1)),"declared observation rows"),
    operand_rows=rows(tb$observation_operands))
  for(name in c("N","O","J","K","U","JT","L","CS","B","T","H","W","S","UF","E","EU","RB"))
    .bh_backend_integer(dims[[name]],paste("backend dimension",name),0L)
  # These are separate declared matrix axes, including a chart-Jacobian row.
  .bh_backend_integer(.bh_count_sum(c(K,1)),"common chart rows",0L)
  .bh_backend_integer(.bh_count_sum(c(U,1)),"parameter chart rows",0L)
  dims
}
.bh_resource_output_inventory <- function(d,save_log_lik=FALSE) {
  p<-function(a,b) .bh_count_product(a,b,name="output array elements")
  s<-function(...) .bh_count_sum(c(...),"output dimension")
  N<-d$N;J<-d$J;K<-d$K;U<-d$U;JT<-d$JT;task<-d$task_augmentation;raw<-d$raw_common
  parameters<-c(component_sd=d$CS,total_sd=d$T,share_beta=d$H,amplitude=d$B,alpha=J,loading_anchor=d$A,loading_free=d$F,obs_sd=d$S,
    stats::setNames(if(raw)d$RB else d$E,if(raw)"correlation_B"else"correlation_raw"),parameter_sd=d$UF,parameter_correlation_raw=d$EU,
    if(task)c(z_task=p(JT,N))else c(z_raw=p(K,N),e_raw=p(U,N)))
  transformed<-c(if(!raw)c(correlation_map=p(s(K,1),K)),parameter_correlation_map=p(s(U,1),U),
    if(!raw||!task)c(L_component=p(K,K)),if(raw)c(F_component=p(K,K)),R_component=p(K,K),L_parameter=p(U,U),R_parameter=p(U,U),
    stats::setNames(1,if(raw)"raw_prior_coordinate_log_jacobian"else"correlation_log_jacobian"),parameter_correlation_log_jacobian=1,component_row_sd=K,
    effective_loadings=p(J,K),Lambda=p(J,K),D_parameter=p(J,U),loading=d$L,
    if(!task)c(z=p(K,N),e=p(U,N),u=p(N,J)),eta=p(N,J),
    if(task)c(task_row_scale=J,common_task_factor=p(J,K),parameter_task_factor=p(J,U),
      Sigma_common_work=p(J,J),Sigma_parameter_work=p(J,J),Sigma_predictor_work=p(J,J),
      task_covariance_work=p(JT,JT),L_task_work=p(JT,JT),task_white_scores=p(JT,N),task_work=p(JT,N),task_delta=p(N,J)),
    share_logits=d$W,share_weight=d$W,parameter_row_sd=U)
  if(task)names(transformed)[names(transformed)%in%c("effective_loadings","Lambda","D_parameter","parameter_row_sd")]<-
    paste0(names(transformed)[names(transformed)%in%c("effective_loadings","Lambda","D_parameter","parameter_row_sd")],"_work")
  generated<-c(if(task)c(Lambda=p(J,K),effective_loadings=p(J,K),D_parameter=p(J,U),parameter_row_sd=U,
      Lambda_available=p(J,K),effective_loadings_available=p(J,K),D_parameter_available=p(J,U)),
    Sigma_common=p(J,J),Sigma_parameter=p(J,J),Sigma_predictor=p(J,J),
    Sigma_common_available=p(J,J),Sigma_parameter_available=p(J,J),Sigma_predictor_available=p(J,J),
    log_component_row_sd=K,component_covariance=p(K,K),component_covariance_log_abs=p(K,K),component_covariance_available=p(K,K),
    if(task)c(Sigma_predictor_log_abs=p(J,J),task_white_unconstrained=p(JT,max(0,N-1)))else c(f=p(K,N),f_log_abs=p(K,N),f_available=p(K,N)),
    if(save_log_lik)c(log_lik=d$O))
  blocks<-list(parameters=parameters,transformed=transformed,generated=generated,
    sampler=c(lp__=1,accept_stat__=1,stepsize__=1,treedepth__=1,n_leapfrog__=1,divergent__=1,energy__=1))
  out<-do.call(rbind,lapply(names(blocks),function(block)data.frame(block=block,variable=names(blocks[[block]]),elements=unname(blocks[[block]]),stringsAsFactors=FALSE)))
  # Preserve declared array/vector/matrix ranks without expanding scalar names.
  # The dimensions are also sufficient for count-only CSV header-name sizing.
  axes<-list(component_sd=d$CS,total_sd=d$T,share_beta=d$H,amplitude=d$B,alpha=J,
    loading_anchor=d$A,loading_free=d$F,obs_sd=d$S,correlation_B=d$RB,correlation_raw=d$E,
    parameter_sd=d$UF,parameter_correlation_raw=d$EU,z_raw=c(K,N),e_raw=c(U,N),z_task=c(JT,N),
    correlation_map=c(s(K,1),K),parameter_correlation_map=c(s(U,1),U),L_component=c(K,K),F_component=c(K,K),
    R_component=c(K,K),L_parameter=c(U,U),R_parameter=c(U,U),correlation_log_jacobian=numeric(),
    raw_prior_coordinate_log_jacobian=numeric(),parameter_correlation_log_jacobian=numeric(),component_row_sd=K,
    effective_loadings=c(J,K),effective_loadings_work=c(J,K),Lambda=c(J,K),Lambda_work=c(J,K),
    D_parameter=c(J,U),D_parameter_work=c(J,U),loading=d$L,z=c(K,N),e=c(U,N),u=c(N,J),eta=c(N,J),
    task_row_scale=J,common_task_factor=c(J,K),parameter_task_factor=c(J,U),Sigma_common_work=c(J,J),
    Sigma_parameter_work=c(J,J),Sigma_predictor_work=c(J,J),task_covariance_work=c(JT,JT),L_task_work=c(JT,JT),
    task_white_scores=c(JT,N),task_work=c(JT,N),task_delta=c(N,J),share_logits=d$W,share_weight=d$W,
    parameter_row_sd=U,parameter_row_sd_work=U,Lambda_available=c(J,K),effective_loadings_available=c(J,K),
    D_parameter_available=c(J,U),Sigma_common=c(J,J),Sigma_parameter=c(J,J),Sigma_predictor=c(J,J),
    Sigma_common_available=c(J,J),Sigma_parameter_available=c(J,J),Sigma_predictor_available=c(J,J),
    log_component_row_sd=K,component_covariance=c(K,K),component_covariance_log_abs=c(K,K),
    component_covariance_available=c(K,K),Sigma_predictor_log_abs=c(J,J),task_white_unconstrained=c(JT,max(0,N-1)),
    f=c(K,N),f_log_abs=c(K,N),f_available=c(K,N),log_lik=d$O)
  out$dimensions<-I(lapply(seq_len(nrow(out)),function(i) {
    shape<-if(out$block[i]=="sampler")numeric()else axes[[out$variable[i]]]
    if(is.null(shape)).mp_abort("E_RESOURCE_ESTIMATE","output inventory lacks a declared dimension record")
    if(!identical(do.call(.bh_count_product,as.list(shape)),out$elements[i]))
      .mp_abort("E_RESOURCE_ESTIMATE","output element count and declared dimensions disagree")
    shape
  }))
  out
}
.bh_resource_lowering_support <- function(spec,checks) {
  unavailable<-function(code,reason)list(available=FALSE,status="unavailable_lowering",code=code,reason=reason)
  if(identical(checks$correlation_lowering$status,"unavailable"))
    return(unavailable(checks$correlation_lowering$code,checks$correlation_lowering$reason))
  if(.bh_has_affine_correlations(spec)&&!.bh_affine_lowering_available())
    return(unavailable("E_CORRELATION_LOWERING_UNAVAILABLE","Affine lowering is unavailable for this declaration"))
  if(.bh_reference_mode(spec)&&!.bh_reference_lowering_available())
    return(unavailable("E_REFERENCE_LOWERING_UNAVAILABLE","Reference-law lowering is unavailable for this declaration"))
  if(!identical(checks$backend$support$status,"supported_typed_ir"))
    return(unavailable("E_OBSERVATION_CONTRACT_UNSUPPORTED",checks$backend$support$reason))
  list(available=TRUE,status="available_lowering",code=NULL,
    reason="An existing structural lowering defines these emitted fields; scientific gates and runtime qualification remain independent")
}
.bh_resource_output_estimate <- function(dimensions,run,save_log_lik,lowering=NULL) {
  csv_sig_figs<-as.integer(.bh_count(run$csv_sig_figs,"csv_sig_figs",1,18,"E_RESOURCE_ESTIMATE"))
  bytes_per_csv_field<-if(csv_sig_figs>12L)40 else 32
  states<-.bh_count_product(as.double(run$chains),as.double(run$iter_sampling),name="configured saved states")
  if(!is.null(lowering)&&!isTRUE(lowering$available))return(list(version="main_declared_output_inventory_v2",
    status="unavailable_lowering",lowering=lowering,reason=paste("Emitted output cannot be estimated because structural lowering is unavailable:",lowering$reason),
    columns_per_saved_state=NA_real_,saved_states=states,rows_per_chain=as.double(run$iter_sampling),saved_warmup_states=0,
    estimated_csv_bytes=NA_real_,numeric_payload_bytes=NA_real_,inventory=NULL,bytes_per_csv_field=bytes_per_csv_field,
    csv_sig_figs=csv_sig_figs,save_log_lik=save_log_lik,
    assumptions=c("Geometric dimensions and configured states remain inspectable, but no emitted program or output inventory is asserted for absent lowering.",
      "Configured saved states are requested counts, not an observed output receipt.")))
  inventory<-.bh_resource_output_inventory(dimensions,save_log_lik)
  columns<-.bh_count_sum(inventory$elements,"emitted posterior CSV fields")
  values<-.bh_count_product(columns,states,name="output scalar values")
  list(version="main_declared_output_inventory_v2",status="estimated",lowering=lowering,columns_per_saved_state=columns,saved_states=states,
    rows_per_chain=as.double(run$iter_sampling),saved_warmup_states=0,
    estimated_csv_bytes=.bh_count_product(bytes_per_csv_field,values,name="estimated CSV bytes"),numeric_payload_bytes=.bh_count_product(8,values,name="single dense numeric payload"),
    inventory=inventory,bytes_per_csv_field=bytes_per_csv_field,csv_sig_figs=csv_sig_figs,save_log_lik=save_log_lik,
    assumptions=c("Every currently declared parameter, transformed parameter, generated value and integer availability flag is retained, including zero/fixed matrix aliases and all roster members.",
      "Seven default CmdStan sample/HMC fields; no thinning and save_warmup=FALSE. Configured rows are expected, never observed rows.",
      paste0(bytes_per_csv_field," bytes per saved scalar field is a numeric formatting heuristic at csv_sig_figs=",csv_sig_figs,
        " (40 for precision above 12, otherwise 32), not a worst-case CSV bound; headers, comments, path metadata and output text are omitted. Post-run measured file bytes remain authoritative."),
      "A setting of seventeen significant decimal digits targets round-trip double preservation, not well-conditioned algebra or scientific accuracy; historical saved source precision is unchanged.",
      "Eight bytes per numeric slot describes one dense payload only, never peak RSS, sampler/autodiff/compilation memory, parser or R object overhead."))
}
.bh_resource_cache_estimate <- function(spec,d,run,policy,output) {
  D<-output$saved_states;Q<-output$columns_per_saved_state;mode<-policy$cache
  if(identical(output$status,"unavailable_lowering"))return(list(version="eager_materialization_audit_v1",
    status=if(is.null(mode))"policy_required"else if(mode=="deferred")"deferred"else"unavailable",
    max_cache_draw_values=policy$limits$max_cache_draw_values,
    allowance_applicability=if(identical(mode,"deferred"))"inapplicable; retained if supplied"else"unavailable without emitted program dimensions",
    estimated_draw_values=if(identical(mode,"deferred"))0 else NA_real_,listed_materialization_values=NA_real_,
    phases=data.frame(phase=character(),materialization=character(),rows=numeric(),columns=numeric(),copies=numeric(),scalar_slots=numeric(),requested=logical()),
    provider_contract="Not assessed: there is no available emitted program inventory",
    reason=if(identical(mode,"deferred"))"Deferred policy requests no posterior materialization; emitted program and eager phase dimensions remain unavailable"else
      "Posterior materialization cannot be estimated without an available emitted program inventory",
    limitations="Geometric dimensions and configured states do not establish emitted posterior widths, cache work or RSS"))
  rows<-list()
  add<-function(phase,materialization,width,copies=1) {
    width<-.bh_count(width,"cache phase width")
    rows[[length(rows)+1L]]<<-data.frame(phase=phase,materialization=materialization,rows=D,columns=width,copies=copies,
      scalar_slots=if(identical(mode,"deferred"))0 else .bh_count_product(D,width,copies,name="posterior materialization slots"),
      requested=!identical(mode,"deferred"),stringsAsFactors=FALSE)
  }
  # This explicitly records potential provider-wide work at every acquisition.
  # It is not enough to certify the provider's complete materialization schedule:
  # method implementations and option-selected formats can create more arrays.
  acquisition<-function(phase,width) {
    add(phase,"provider-wide CSV/draw-array acquisition",Q)
    add(phase,"provider-wide retained-array conversion or binding",Q)
    add(phase,"returned requested posterior array",width)
  }
  acquisition("parameter_summary",Q)
  acquisition("hmc_diagnostics",Q)
  p<-function(a,b).bh_count_product(a,b,name="cache width")
  for(layer in c("component","parameter")) {
    K<-if(layer=="component")d$K else d$U
    if(!K)next
    width<-p(K,K);phase<-paste0("conditioning_",layer)
    acquisition(phase,width);add(phase,"as.matrix export",width);add(phase,"class-adjusted matrix copy",width)
    stored<-(layer=="component"&&d$raw_common&&d$task_augmentation)||
      .bh_is_affine_cor_table(if(layer=="component")spec$tables$correlations else spec$tables$parameter_correlations)
    if(stored) {
      acquisition(paste0(phase,"_stored_R"),width)
      add(phase,"stored matrix export and column projection",width,2)
    }
    add(phase,"per-draw factor projection, numeric vector, matrix and factor product",width,4)
    if(stored)add(phase,"per-draw stored matrix projection/vector/matrix",width,3)
  }
  if(d$task_augmentation) {
    flags<-.bh_count_sum(c(p(3,p(d$J,d$J)),p(2,p(d$J,d$K)),p(d$J,d$U),p(d$K,d$K)))
    logs<-.bh_count_sum(c(p(d$J,d$J),d$K,p(d$K,d$K)))
  }else {
    flags<-.bh_count_sum(c(p(3,p(d$J,d$J)),p(d$K,d$K),p(d$K,d$N)))
    logs<-.bh_count_sum(c(d$K,p(d$K,d$K),p(d$K,d$N)))
  }
  if(flags) {
    acquisition("availability_flags",flags)
    add("availability_flags","as.matrix export, class copy, selected retained matrix",flags,3)
    if(logs) {
      acquisition("log_magnitude_arrays",logs)
      add("log_magnitude_arrays","as.matrix export, class copy, selected retained matrix",logs,3)
    }
  }
  if(.bh_final_correlation_reporting(spec)&&d$K+d$U>0)acquisition("expanded_correlation_summary",Q)
  phases<-if(length(rows))do.call(rbind,rows)else data.frame()
  listed<-.bh_count_sum(phases$scalar_slots,"listed cumulative posterior slots")
  list(version="eager_materialization_audit_v1",status=if(is.null(mode))"policy_required"else if(mode=="deferred")"deferred"else if(policy$kind=="development")"historical_development"else"unavailable",
    max_cache_draw_values=policy$limits$max_cache_draw_values,
    allowance_applicability=if(identical(mode,"deferred"))"inapplicable; retained if supplied"else if(policy$kind=="development")
      "historical tiny eager compatibility; no newly certified posterior-slot allowance"else"eager posterior scalar-slot materialization work",
    estimated_draw_values=if(identical(mode,"deferred"))0 else NA_real_,listed_materialization_values=listed,phases=phases,
    provider_contract="Audited CmdStanR 0.9.0 R methods may convert provider-wide cached arrays and repeatedly bind/subset acquisitions. Installed provider and option-selected posterior format are not discovered by planning.",
    reason=if(identical(mode,"deferred"))"No posterior-value materialization is requested; diagnostics and optional reports remain deferred/uncomputed by policy."else if(policy$kind=="development")
      "Historical eager compatibility is retained only under the explicit development preset and its tiny execution/output ceilings. Listed phases are an audit, not a new provider-independent cache-work guarantee."else
      "A reliable conservative count for the complete provider-dependent materialization schedule is unavailable. Caller eager admission is refused; choose cache='deferred' explicitly.",
    limitations=c("Listed phases include each potentially full-width acquisition and explicit exported/copied arrays; equal columns, cache reuse and early release are not credited.",
      "The listed sum is incomplete for unqualified provider-internal materializations and is not an admissible caller eager estimate.",
      "Transient internal copies, decomposition workspace, parser/R overhead, compilation and sampler memory are not bounded; neither the quota nor eight times it is peak RSS."))
}
.bh_resource_consumer_support <- function(d,run,output) {
  findings<-character()
  if(run$chains>2L)findings<-c(findings,"Legacy bounded CSV consumers accept only one or two equal-layout chains")
  if(output$saved_states>100)findings<-c(findings,"Legacy bounded consumers accept at most 100 total retained rows, without thinning or dropping chains")
  estimate_risks<-if(identical(output$status,"unavailable_lowering"))
    "Emitted output bytes are unavailable because structural lowering is unavailable; geometric/configured counts do not establish a CSV source"else if(output$estimated_csv_bytes>10485760)
    "The numeric CSV formatting estimate exceeds the legacy bounded reader 10 MiB source-file ceiling; actual CSV bytes may differ and remain unobserved"else character()
  individual<-character()
  if(d$individual_cells>1000)individual<-c(individual,"Complete person-by-logical-target domain exceeds 1000 individual cells")
  if(d$declared_rows>1000||d$O>1000)individual<-c(individual,"Complete declared/consumed individual design exceeds 1000 rows")
  if(d$operand_rows>10000)individual<-c(individual,"Individual observation operand table exceeds 10000 rows")
  ppc<-character()

  record<-function(extra)list(status=if(length(c(findings,extra)))"configured_layout_incompatible"else"configured_limits_not_exceeded",
    findings=c(findings,extra),estimate_risks=estimate_risks,
    actual_source_support="unobserved; an explicit consumer must inspect current CSV layout and bytes",
    requested_value_limit=50000,requested_value_support="unassessed until an explicit consumer resolves unique requested CSV columns; fixed/task aliases can require fewer values than the person-by-logical-target domain",
    scope="Configured/expected layout only; operation-specific laws, selected variables and scientific gates remain independent")
  legacy_ppc<-record(ppc);legacy_individual<-record(individual)
  streaming_ppc<-ppc

  if(output$saved_states<2)streaming_ppc<-c(streaming_ppc,
    "Configured total saved states do not meet the minimum two-state PPC capability")
  streaming_individual<-if(output$saved_states<2&&d$individual_cells>0)
    "Configured total saved states do not meet the minimum two-state individual-summary capability"else character()
  combine<-function(legacy,streaming_findings,operation)list(
    status=if(length(streaming_findings))"configured_layout_incompatible"else
      if(identical(legacy$status,"configured_limits_not_exceeded"))"configured_limits_not_exceeded"else"streaming_policy_required",
    findings=streaming_findings,estimate_risks=legacy$estimate_risks,
    actual_source_support="unobserved; every current source must be acquired and validated by the explicit chosen consumer path",
    legacy_reader=legacy,
    streaming=list(status=if(length(streaming_findings))"configured_operation_incompatible"else"explicit_policy_required",
      policy="consumer budget$posterior with reader='streaming'; no policy is inferred from fitting resources",
      layout="multiple equal complete HMC chains; actual metadata/rows/bytes remain unobserved",
      work=if(operation=="ppc")"Explicit finite predictive allocation/statistic budgets and at least one balanced replication per chain apply"else
        "Complete cells/output/transformation/context work needs an explicit caller budget; all states or explicitly labelled balanced subset",
      findings=streaming_findings),
    scope="Configured layout forecast only; legacy bounded limits, explicit streaming acquisition and operation-specific laws/work remain separate")
  list(version="posterior_consumer_projection_v2",ppc=combine(legacy_ppc,streaming_ppc,"ppc"),
    individual=combine(legacy_individual,streaming_individual,"individual"),
    legacy_limits=list(max_chains=2,max_retained_rows=100,max_source_bytes=10485760,max_requested_values=50000),
    inference="not_assessed; resource or reader admission does not establish source validity or inference")
}
.bh_resource_admission <- function(d,run,policy,output,cache) {
  findings<-list()
  add<-function(code,quantity,requested,permitted,reason) {
    findings[[length(findings)+1L]]<<-list(code=code,quantity=quantity,requested=requested,permitted=permitted,reason=reason)
  }
  if(policy$kind=="required")return(list(status="resource_policy_required",allowed=FALSE,findings=list(list(
    code="E_RESOURCE_POLICY_REQUIRED",quantity="resources",requested=NULL,permitted=NULL,
    reason="Supply an explicit finite resources policy (max_output_bytes and cache), or resources='development' for the historical tiny preset"))))
  actual<-list(cpu_budget=run$simultaneous_workers,max_subjects=d$N,max_observations=d$O,
    max_saved_draws=output$saved_states,max_targets=d$logical_targets,max_observation_scales=d$S,
    max_latent_coordinates=d$latent_coordinates,max_coefficient_primitives=d$coefficient_primitives)
  if(identical(output$status,"unavailable_lowering"))
    add("E_RESOURCE_OUTPUT_UNAVAILABLE","emitted_output_estimate",NA_real_,policy$limits$max_output_bytes,output$reason)
  else actual$max_output_bytes<-output$estimated_csv_bytes
  for(name in names(actual))if(!is.null(policy$limits[[name]])&&actual[[name]]>policy$limits[[name]])
    add(if(name=="max_output_bytes")"E_OUTPUT_BUDGET"else"E_RUN_BUDGET",name,actual[[name]],policy$limits[[name]],
      paste(name,"requested",format(actual[[name]],scientific=FALSE),"exceeds permitted",format(policy$limits[[name]],scientific=FALSE)))
  if(policy$kind=="development") {
    if(run$chains>2L||run$iter_warmup>50L||run$iter_sampling>50L)
      add("E_SMOKE_BUDGET","development_iterations",c(chains=run$chains,warmup=run$iter_warmup,sampling=run$iter_sampling),c(chains=2,warmup=50,sampling=50),"The explicit development preset retains <=2 chains and <=50 warmup + 50 sampling iterations")
    for(name in c("threads_per_chain","grainsize","max_treedepth")) {
      cap<-switch(name,threads_per_chain=2,grainsize=1000,max_treedepth=10)
      if(run[[name]]>cap)add("E_RUN_BUDGET",name,run[[name]],cap,paste("Explicit development ceiling exceeded for",name))
    }
  }else if(identical(policy$cache,"eager")) {
    add("E_CACHE_ESTIMATE_UNAVAILABLE","complete_eager_materialization_schedule",NA_real_,policy$limits$max_cache_draw_values,cache$reason)
  }
  list(status=if(length(findings))"refused"else"admitted",allowed=!length(findings),findings=findings,
    output_enforcement="Estimate-based preflight and a post-run actual file-size check; temporary disk overrun is possible and outputs are preserved",
    cpu_scope="Declared simultaneous sampler workers only; compiler jobs, BLAS/other process threads and whole-process CPU are not governed",
    memory_enforcement="No process-memory/RSS enforcement",wall_time_enforcement="No wall-time enforcement")
}
.bh_resource_fit_gate <- function(plan) {
  if(isTRUE(plan$resource_admission$allowed))return(invisible(plan))
  finding<-plan$resource_admission$findings[[1L]]
  if(is.null(finding))finding<-list(code="E_RESOURCE_PLAN",reason="The resolved resource admission record is missing")
  stop(structure(list(message=paste0(finding$code,": ",finding$reason),call=NULL,code=finding$code,
    plan=plan,resource_admission=plan$resource_admission),class=c("measurement_resource_error","measurement_error","error","condition")))
}
.bh_resource_scientific_plan <- function(spec,exploratory,execution) {
  checks<-check_model(spec)
  result<-tryCatch({
    if(!.bh_count_sum(spec$tables$priors$parameter_count,"free parameter count"))
      .mp_abort("E_NO_FREE_PARAMETERS","this prepared model has no unknown parameters to sample; empty dimensions remain inspectable")
    .bh_fit_gate(spec)
    admitted_checks<-.bh_execution_gate(spec,exploratory,execution)
    .bh_affine_runtime_gate(spec)
    admitted_checks
  },error=identity)
  if(inherits(result,"error"))return(list(allowed=FALSE,checks=checks,
    finding=list(code=if(is.null(result$code))"E_SCIENTIFIC_EXECUTION"else result$code,reason=conditionMessage(result)),
    execution_profile=list(requested=execution,used="blocked",reason=conditionMessage(result))))
  list(allowed=TRUE,checks=result,finding=NULL,execution_profile=result$execution_profile)
}
.bh_revalidate_plan <- function(spec,plan,save_log_lik=FALSE) {
  spec<-.bh_normalize_spec(spec)
  if(!is.list(plan$request)||!identical(plan$spec_fingerprint,spec$fingerprint)||!identical(plan$request$save_log_lik,save_log_lik))
    .mp_abort("E_RESOURCE_PLAN_CHANGED","The plan is not bound to this specification/output request; rebuild the execution plan")
  # The issuing call already delivered the scientific warnings. Revalidation
  # reruns every gate without emitting an identical second warning sequence.
  fresh<-withCallingHandlers(do.call(.bh_run_plan,c(list(spec=spec),plan$request)),
    warning=function(w)invokeRestart("muffleWarning"))
  if(!identical(plan$identity,fresh$identity)||!identical(plan$run,fresh$run)||
     !identical(plan$resource_policy,fresh$resource_policy)||!identical(plan$output_estimate,fresh$output_estimate)||
     !identical(plan$cache_estimate,fresh$cache_estimate)||!identical(plan$resource_admission,fresh$resource_admission)||
     !identical(plan$provider_metadata,fresh$provider_metadata))
    .mp_abort("E_RESOURCE_PLAN_CHANGED","Run controls, policy or estimator identity changed; rebuild the execution plan")
  # Scientific and resource refusals remain independent and are both retained.
  if(!fresh$scientific_eligibility$allowed) {
    f<-fresh$scientific_eligibility$finding;.mp_abort(f$code,f$reason)
  }
  .bh_resource_fit_gate(fresh)
  fresh
}
.bh_resource_revalidate_fit_plan <- .bh_revalidate_plan

print.bmeas_execution_plan <- function(x,...) {
  fail<-function(label).mp_abort("E_RESOURCE_PLAN_PRINT",paste("Cannot print an ambiguous or non-plain saved",label))
  container<-function(value,label,allowed_class=NULL,named=TRUE) {
    if(typeof(value)!="list"||isS4(value))fail(label)
    attrs<-attributes(value);cl<-attr(value,"class",exact=TRUE)
    if(length(setdiff(names(attrs),c("names","class")))||
       (!is.null(cl)&&!identical(cl,allowed_class)))fail(label)
    keys<-attr(value,"names",exact=TRUE)
    if((named&&is.null(keys))||(!is.null(keys)&&
      (typeof(keys)!="character"||!is.null(attributes(keys))||length(keys)!=length(value)||
       anyNA(keys)||any(!nzchar(keys))||anyDuplicated(keys))))fail(label)
    unclass(value)
  }
  slot<-function(value,name).subset2(value,name)
  string<-function(value,label) {
    if(typeof(value)!="character"||!is.null(attributes(value))||length(value)!=1L||is.na(value)||!nzchar(value))fail(label)
    value
  }
  count<-function(value,label).bh_count(value,label,lower=1,code="E_RESOURCE_PLAN_PRINT")
  root<-container(x,"execution plan",allowed_class="bmeas_execution_plan")
  policy<-container(slot(root,"resource_policy"),"resource policy")
  admission<-container(slot(root,"resource_admission"),"resource admission")
  science<-container(slot(root,"scientific_eligibility"),"scientific eligibility")
  backend<-container(slot(root,"backend_qualification"),"backend qualification")
  run<-container(slot(root,"run"),"run",allowed_class="measurement_run_config")
  output<-container(slot(root,"output_estimate"),"output estimate")
  concurrency<-container(slot(root,"concurrency"),"concurrency")
  cache<-container(slot(root,"cache_estimate"),"cache estimate")
  consumers<-container(slot(root,"postprocessing_support"),"postprocessing support")
  output_status<-slot(output,"status")
  if(is.null(output_status))output_status<-"estimated"else output_status<-string(output_status,"output status")
  if(!output_status%in%c("estimated","unavailable_lowering"))fail("output status")
  output_unavailable<-identical(output_status,"unavailable_lowering")
  output_count<-function(value,label) {
    if(!output_unavailable)return(count(value,label))
    if(!typeof(value)%in%c("integer","double")||!is.null(attributes(value))||length(value)!=1L||!is.na(value)||is.nan(value))fail(label)
    NA_real_
  }
  allowed<-slot(science,"allowed")
  if(typeof(allowed)!="logical"||!is.null(attributes(allowed))||length(allowed)!=1L||is.na(allowed))fail("scientific allowed flag")
  # Project only fields that are printed. Unprinted scientific records keep
  # their legitimate richer representations and are never traversed here.
  v<-list(kind=string(slot(policy,"kind"),"resource kind"),status=string(slot(admission,"status"),"resource status"),
    allowed=allowed,backend=string(slot(backend,"status"),"backend status"),
    scientific_reason=if(!allowed)string(slot(container(slot(science,"finding"),"scientific finding"),"reason"),"scientific reason")else NULL,
    chains=count(slot(run,"chains"),"chains"),warmup=count(slot(run,"iter_warmup"),"warmup"),sampling=count(slot(run,"iter_sampling"),"sampling"),
    saved=count(slot(output,"saved_states"),"saved states"),parallel=count(slot(run,"parallel_chains"),"parallel chains"),
    threads=count(slot(run,"threads_per_chain"),"threads"),workers=count(slot(concurrency,"simultaneous_workers"),"workers"),
    cores=count(slot(concurrency,"requested_cores"),"requested cores"),bytes=output_count(slot(output,"estimated_csv_bytes"),"estimated bytes"),
    columns=output_count(slot(output,"columns_per_saved_state"),"output columns"),output_unavailable=output_unavailable,
    bytes_per_field=if(is.null(slot(output,"bytes_per_csv_field")))NA_real_ else count(slot(output,"bytes_per_csv_field"),"CSV field byte reserve"),
    csv_sig_figs=if(is.null(slot(output,"csv_sig_figs")))NA_real_ else
      .bh_count(slot(output,"csv_sig_figs"),"CSV significant figures",1,18,"E_RESOURCE_PLAN_PRINT"),
    output_reason=if(output_unavailable)string(slot(output,"reason"),"unavailable output reason")else NULL,
    cache_status=string(slot(cache,"status"),"cache status"),cache_reason=string(slot(cache,"reason"),"cache reason"))
  findings<-container(slot(admission,"findings"),"resource findings",named=FALSE)
  v$findings<-lapply(findings,function(f) {
    f<-container(f,"resource finding")
    list(code=string(slot(f,"code"),"resource finding code"),reason=string(slot(f,"reason"),"resource finding reason"))
  })
  v$consumers<-vapply(c("ppc","individual"),function(name)
    string(slot(container(slot(consumers,name),paste(name,"consumer")),"status"),paste(name,"consumer status")),character(1))
  provider_record<-slot(root,"provider_metadata")
  v$provider<-NULL
  if(!is.null(provider_record)) {
    metadata<-container(provider_record,"provider metadata")
    provider<-container(slot(metadata,"provider"),"provider scope")
    scan<-container(slot(metadata,"full_file_scan"),"provider scan")
    header<-container(slot(metadata,"headers"),"provider headers")
    metric<-container(slot(metadata,"inverse_metric"),"provider inverse metric")
    compatibility<-string(slot(provider,"runtime_compatibility"),"provider compatibility")
    if(!compatibility%in%c("unverified","not_applied_to_requested_mode"))fail("provider compatibility")
    scan_status<-string(slot(scan,"status"),"provider scan status")
    if(!identical(scan_status,"expected_before_sample_returns"))fail("provider scan status")
    header_status<-string(slot(header,"status"),"provider header status")
    metric_status<-string(slot(metric,"status"),"provider metric status")
    if(!header_status%in%c("unavailable","counted_from_declared_shapes"))fail("provider header status")
    if(!metric_status%in%c("unavailable","expected_default_diagonal"))fail("provider metric status")
    metadata_count<-function(value,label,known) {
      if(known)return(.bh_count(value,label,lower=0,code="E_RESOURCE_PLAN_PRINT"))
      if(!typeof(value)%in%c("integer","double")||!is.null(attributes(value))||length(value)!=1L||!is.na(value)||is.nan(value))fail(label)
      NA_real_
    }
    v$provider<-list(name=string(slot(provider,"name"),"provider name"),version=string(slot(provider,"version"),"provider version"),
      cmdstan=string(slot(provider,"cmdstan_version"),"CmdStan version"),platform=string(slot(provider,"platform"),"provider platform"),
      compatibility=if(compatibility=="unverified")"unverified"else"not applied to requested mode",
      header_entries=metadata_count(slot(header,"name_entries_all_chains"),"header-name entries",header_status=="counted_from_declared_shapes"),
      metric_slots=metadata_count(slot(metric,"scalar_slots"),"inverse-metric slots",metric_status=="expected_default_diagonal"),
      scan_bytes=metadata_count(slot(scan,"actual_bytes"),"actual scan bytes",FALSE))
  }
  # All format()/cat() inputs below are plain local scalars from the projection.
  fmt<-function(value)format(value,scientific=FALSE,trim=TRUE,big.mark=",")
  cat("bmeas execution plan |",v$kind,"| resource admission:",v$status,"\n")
  cat("Scientific execution:",if(v$allowed)"eligible within recorded profile"else"blocked",
    "| backend:",v$backend,"\n")
  if(!v$allowed)cat(v$scientific_reason,"\n")
  cat("Configured:",v$chains,"chains,",v$warmup,"warmup +",v$sampling,"sampling iterations per chain;",
    fmt(v$saved),"expected saved states (unobserved)\n")
  cat("Sampler concurrency:",v$parallel,"parallel chains x",v$threads,"threads =",
    fmt(v$workers),"workers; requested cores:",v$cores,"\n")
  if(v$output_unavailable)cat("Output estimate: unavailable |",v$output_reason,"\n")else
    cat("Output estimate:",fmt(v$bytes),"bytes from",fmt(v$columns),
    "CSV fields/state;",if(is.na(v$bytes_per_field))"field reserve not retained"else paste0(fmt(v$bytes_per_field)," bytes/field"),
    if(is.na(v$csv_sig_figs))"| csv_sig_figs not retained"else paste0("| csv_sig_figs=",fmt(v$csv_sig_figs)),
    "| formatting heuristic, not a hard disk bound\n")
  cat("Cache:",v$cache_status,"|",v$cache_reason,"\n")
  if(is.null(v$provider))cat("Provider metadata: not retained; runtime compatibility and constructor costs unverified\n")else {
    cat("Provider reference:",v$provider$name,v$provider$version,"/ CmdStan",v$provider$cmdstan,"|",v$provider$platform,
      "| runtime:",v$provider$compatibility,"| full-file constructor metadata scan before return\n")
    available_count<-function(value)if(is.na(value))"unavailable"else fmt(value)
    cat("Provider metadata estimates:",available_count(v$provider$header_entries),"header-name entries;",
      available_count(v$provider$metric_slots),"diagonal-metric scalar slots; actual scan bytes unobserved\n")
  }
  if(length(v$findings))for(f in v$findings)cat(f$code,": ",f$reason,"\n",sep="")
  for(name in c("ppc","individual"))cat(name,"consumer:",v$consumers[[name]],
    "| actual CSV support unobserved\n")
  cat("Inference remains unassessed. Compiler jobs, whole-process CPU, RSS and wall time are not enforced.\n")
  invisible(x)
}
