.mp_structure_keys <- function(spec) if(identical(spec$structure$type,"component_covariance"))spec$structure$keys else unlist(spec$structure$factors,use.names=FALSE)
# Function arguments retain their storage kind but omit data dimensions/constraints.
.mp_partial_argument <- function(declaration) {
  x <- gsub("<[^>]*>", "", declaration)
  x <- sub(";$", "", x)
  if (grepl("^array\\[", x)) {
    dims <- sub("^array\\[([^]]*)\\].*$", "\\1", x)
    rank <- length(strsplit(dims, ",", fixed=TRUE)[[1L]])
    x <- sub("^array\\[[^]]*\\]", paste0("array[",paste(rep("",rank),collapse=","),"]"), x)
  } else x <- sub("^(vector|matrix)\\[[^]]*\\]", "\\1", x)
  paste("data",x)
}
.mp_partial_likelihood <- function(name, units, count, call, declarations, routed) {
  args <- vapply(declarations,.mp_partial_argument,character(1))
  symbols <- sub("^.* ","",args)
  signature <- paste(c("array[] int mp_slice", "int mp_start", "int mp_end", args,
                       "matrix eta", "vector obs_sd"),collapse=", ")
  # The ordinary function is deliberately normalized in both branches. Priors
  # stay in the model block and no latent vector is resampled inside a slice.
  code <- c(paste0("real ",name,"(",signature,") {"), "real mp_lp=0;",
    "for(mp_i in 1:size(mp_slice)) {", "int o=mp_slice[mp_i];",
    if(routed)"int r=mp_start+mp_i-1;",paste0("mp_lp += ",call,";"),"}","return mp_lp;","}")
  shared <- paste(c(symbols,"eta","obs_sd"),collapse=", ")
  serial <- paste0(name,"(",units,", 1, ",count,", ",shared,")")
  reduced <- paste0("reduce_sum_static(",name,", ",units,", mp_grainsize, ",shared,")")
  list(functions=code,likelihood=c(paste0("if(",count,">0) {"),
    paste0("if(mp_use_reduce_sum==1) target += ",reduced,";"),
    paste0("else target += ",serial,";"),"}"))
}
.mp_level_operand_ir <- function(spec,index,keys,parameter,primary,index_name,fixed_name,count,person,slot,
                                 literal=rep(FALSE,length(keys)),literal_values=rep(NA_real_,length(keys))) {
  roles<-spec$tables$parameter_roles
  at<-match(keys,roles$key)
  if(anyNA(at[!literal])) .mp_abort("E_PARAMETER_BINDING","operand key is outside the resolved scientific parameter domain")
  fixed<-literal
  fixed[!literal]<-roles$level[at[!literal]]=="fixed"
  mapped<-integer(length(keys));values<-numeric(length(keys))
  values[literal]<-literal_values[literal]
  values[fixed & !literal]<-roles$fixed_received[at[fixed & !literal]]
  free<-which(!fixed)
  mapped[free]<-match(keys[free],if(primary)index$keys else index$nuisance)
  if(anyNA(mapped)||any(!is.finite(values))) .mp_abort("E_PARAMETER_BINDING","operand has no unique free coordinate or finite received constant")
  data<-stats::setNames(list(as.integer(mapped),values),c(index_name,fixed_name))
  declarations<-c(paste0("array[",count,"] int<lower=0,upper=",if(primary)"J"else"S","> ",index_name,";"),paste0("vector[",count,"] ",fixed_name,";"))
  selector<-paste0(index_name,"[",slot,"]")
  free_value<-if(primary)paste0("eta[",person,"[o],",selector,"]")else paste0("obs_sd[",selector,"]")
  if(primary&&parameter$receives=="theta"&&parameter$predictor!="identity")
    free_value<-paste0(if(parameter$predictor=="log")"exp("else"inv_logit(",free_value,")")
  # Only the free predictor branch uses its forward link. Constants already
  # carry the exact received value, including valid natural theta endpoints.
  expression<-paste0("(",selector,">0 ? ",free_value," : ",fixed_name,"[",slot,"])")
  list(data=data,declarations=declarations,expression=expression)
}
observation_ir <- function(spec, index=.mp_registry_indices(spec), threaded=FALSE) {
  .bh_pf_refuse_execution(spec,"observation_ir")
  .mp_threaded_flag(spec,threaded)
  levels<-inherits(spec,"bhmm_spec")&&isTRUE(spec$schema_version%in%c("reference-law-spec-v1","affine-correlation-spec-v1"))
  data<-list();declarations<-likelihood<-gq<-functions<-character();unit_offset<-0L
  for(block_index in seq_along(spec$models)) {
    block_start<-length(declarations)+1L
    b<-names(spec$models)[block_index];model<-spec$models[[b]]$model;d<-spec$data_map$blocks[[b]];info<-if(levels).bh_model_role_info(model)else .mp_model_info(model)
    prefix<-paste0("b",block_index,"_");oname<-paste0(prefix,"O");person<-paste0(prefix,"person")
    data[[oname]]<-nrow(d);data[[person]]<-as.integer(d$.person_index)
    declarations<-c(declarations,paste0("int<lower=",if(levels)0 else 1,"> ",oname,";"),paste0("array[",oname,"] int<lower=1,upper=N> ",person,";"))
    bindings<-list()
    for(name in names(model$data_schema)) {
      f<-model$data_schema[[name]];v<-paste0(prefix,"field_",name);width<-f$width
      integer<-f$type%in%c("integer","nonnegative_integer","binary");value<-d[[name]]
      if(integer)storage.mode(value)<-"integer"
      data[[v]]<-value
      declaration<-if(width>1L)paste0("array[",oname,",",width,"] ",if(integer)"int "else"real ",v,";")else
        if(integer)paste0("array[",oname,"] int ",v,";")else paste0("vector[",oname,"] ",v,";")
      declarations<-c(declarations,declaration);bindings[[name]]<-paste0(v,"[o]")
    }
    for(name in names(model$constants)) {
      constant<-model$constants[[name]];v<-paste0(prefix,"constant_",name);data[[v]]<-constant$value
      declaration<-switch(constant$type,real=paste0("real ",v,";"),int=paste0("int ",v,";"),vector=paste0("vector[",length(constant$value),"] ",v,";"),matrix=paste0("matrix[",nrow(constant$value),",",ncol(constant$value),"] ",v,";"))
      declarations<-c(declarations,declaration);bindings[[name]]<-v
    }
    block_declarations<-declarations[seq.int(block_start,length(declarations))]
    if(identical(model$adapter$mode,"routed")) {
      resolved<-.mp_resolve_bindings(spec$models[[b]],d,b,require_coverage=!levels)
      for(ri in seq_along(model$adapter$routes)) {
        route_start<-length(declarations)+1L
        rn<-names(model$adapter$routes)[ri];route<-model$adapter$routes[[ri]];rp<-paste0(prefix,"r",ri,"_")
        observations<-which(d[[model$adapter$selector]]==route$selector_value)
        nname<-paste0(rp,"O");rname<-paste0(rp,"rows");data[[nname]]<-length(observations);data[[rname]]<-as.integer(observations)
        declarations<-c(declarations,paste0("int<lower=0> ",nname,";"),paste0("array[",nname,"] int<lower=1,upper=",oname,"> ",rname,";"))
        rb<-bindings
        for(a in names(route$parameter_arguments)) {
          records<-resolved[resolved$route==rn & resolved$argument==a,,drop=FALSE]
          records<-records[match(d$.observation_id[observations],records$observation_id),,drop=FALSE]
          parameter_name<-route$parameter_arguments[[a]]
          parameter<-model$parameters[[parameter_name]]
          if(levels) {
            operand<-.mp_level_operand_ir(spec,index,records$target_key,parameter,parameter_name%in%info$primary,
              paste0(rp,a,"_index"),paste0(rp,a,"_fixed"),nname,person,"r",records$kind=="fixed",records$fixed_value)
            data<-c(data,operand$data);declarations<-c(declarations,operand$declarations);rb[[a]]<-operand$expression
            next
          }
          key_index<-match(records$target_key,index$keys);key_index[records$kind=="fixed"]<-0L
          if(anyNA(key_index)) .mp_abort("E_PARAMETER_BINDING","unmapped target in routed compiler")
          iname<-paste0(rp,a,"_index");fname<-paste0(rp,a,"_fixed")
          data[[iname]]<-as.integer(key_index);data[[fname]]<-ifelse(records$kind=="fixed",records$fixed_value,0)
          declarations<-c(declarations,paste0("array[",nname,"] int<lower=0,upper=J> ",iname,";"),paste0("vector[",nname,"] ",fname,";"))
          value<-paste0("(",iname,"[r]>0 ? eta[",person,"[o],",iname,"[r]] : ",fname,"[r])")
          parameter<-model$parameters[[route$parameter_arguments[[a]]]]
          if(parameter$receives=="theta"&&parameter$predictor!="identity") value<-paste0(if(parameter$predictor=="log")"exp("else"inv_logit(",value,")")
          rb[[a]]<-value
        }
        args<-unlist(rb[route$arguments],use.names=FALSE);call<-paste0(route$symbol,"(",args[1]," | ",paste(args[-1],collapse=", "),")")
        opening<-paste0("if(",nname,">0) for(r in 1:",nname,") { int o=",rname,"[r];")
        if(threaded) {
          part<-.mp_partial_likelihood(paste0("mp_",rp,"partial"),rname,nname,call,
            c(block_declarations,declarations[seq.int(route_start,length(declarations))]),TRUE)
          functions<-c(functions,part$functions);likelihood<-c(likelihood,part$likelihood)
        } else likelihood<-c(likelihood,opening,paste0("target += ",call,";"),"}")
        gq<-c(gq,opening,paste0("log_lik[",unit_offset,"+o]=",call,";"),"}")
      }
    } else {
      for(name in names(model$parameters)) {
        parameter<-model$parameters[[name]];v<-paste0(prefix,"index_",name)
        keys<-if(levels&&!nrow(d))character()else targets(b,name,d$.task)
        if(levels) {
          # targets() intentionally rejects an empty task selector; empty
          # blocks have no consumed keys but keep their role/domain tables.
          keys<-if(nrow(d))targets(b,name,d$.task)else character()
          operand<-.mp_level_operand_ir(spec,index,keys,parameter,name%in%info$primary,v,paste0(prefix,"fixed_",name),oname,person,"o")
          data<-c(data,operand$data);declarations<-c(declarations,operand$declarations);bindings[[name]]<-operand$expression
          next
        }
        if(name%in%info$primary) {
          data[[v]]<-as.integer(match(keys,index$keys));declarations<-c(declarations,paste0("array[",oname,"] int<lower=1,upper=J> ",v,";"))
          value<-paste0("eta[",person,"[o],",v,"[o]]")
          if(parameter$receives=="theta"&&parameter$predictor!="identity")value<-paste0(if(parameter$predictor=="log")"exp("else"inv_logit(",value,")")
          bindings[[name]]<-value
        } else {
          data[[v]]<-as.integer(match(keys,index$nuisance));declarations<-c(declarations,paste0("array[",oname,"] int<lower=1,upper=S> ",v,";"));bindings[[name]]<-paste0("obs_sd[",v,"[o]]")
        }
        if(anyNA(data[[v]])) .mp_abort("E_PARAMETER_DEPENDENCY","unmapped same-task observation dependency")
      }
      args<-unlist(bindings[model$likelihood$arguments],use.names=FALSE);call<-paste0(model$likelihood$symbol,"(",args[1]," | ",paste(args[-1],collapse=", "),")")
      if(threaded) {
        units<-paste0(prefix,"units");data[[units]]<-seq_len(nrow(d))
        declarations<-c(declarations,paste0("array[",oname,"] int<lower=1,upper=",oname,"> ",units,";"))
        part<-.mp_partial_likelihood(paste0("mp_",prefix,"partial"),units,oname,call,
          declarations[seq.int(block_start,length(declarations))],FALSE)
        functions<-c(functions,part$functions);likelihood<-c(likelihood,part$likelihood)
      } else likelihood<-c(likelihood,paste0(if(levels)paste0("if(",oname,">0) ")else"","for(o in 1:",oname,") target += ",call,";"))
      gq<-c(gq,paste0(if(levels)paste0("if(",oname,">0) ")else"","for(o in 1:",oname,") log_lik[",unit_offset,"+o]=",call,";"))
    }
    unit_offset<-unit_offset+nrow(d)
  }
  structure(list(data=data,declarations=declarations,functions=functions,likelihood=likelihood,log_lik=gq,observation_count=unit_offset),class="measurement_observation_ir")
}
structure_ir <- function(spec) {
  .bh_pf_refuse_execution(spec,"structure_ir")
  if(identical(spec$structure$type,"component_covariance"))return(.mp_component_structure_ir(spec))
  index<-.mp_registry_indices(spec);p<-.mp_resolve_parameter_priors(spec)
  anchors<-match(unname(spec$structure$positive_anchors),index$keys);aid<-fid<-integer(index$J);aid[anchors]<-seq_len(index$K);fid[-anchors]<-seq_len(index$J-index$K)
  data<-list(factor_id=rep(seq_len(index$K),lengths(spec$structure$factors)),anchor_id=aid,free_id=fid,
    alpha_dist=vapply(p$parameters[index$keys],function(x)match(x$intercept$distribution,c("normal","logistic","student_t")),integer(1)),
    alpha_mean=vapply(p$parameters[index$keys],function(x)x$intercept$mean,numeric(1)),alpha_sd=vapply(p$parameters[index$keys],function(x)x$intercept$sd,numeric(1)),
    alpha_df=vapply(p$parameters[index$keys],function(x)if(x$intercept$distribution=="student_t")x$intercept$df else 1,numeric(1)),loading_sd=vapply(p$parameters[index$keys],function(x)x$loading$sd,numeric(1)),
    sd_prior=vapply(p$nuisance[index$nuisance],function(x)x$sd,numeric(1)),lkj_shape=p$correlation$shape)
  structure(list(data=data,declarations=c("array[J] int<lower=1,upper=K> factor_id;","array[J] int<lower=0,upper=K> anchor_id;","array[J] int<lower=0,upper=J-K> free_id;",
    "array[J] int<lower=1,upper=3> alpha_dist;","vector[J] alpha_mean;","vector<lower=0>[J] alpha_sd;","vector<lower=0>[J] alpha_df;","vector<lower=0>[J] loading_sd;","vector<lower=0>[S] sd_prior;","real<lower=0> lkj_shape;"),
    parameters=c("vector[J] alpha;","vector<lower=0>[K] loading_anchor;","vector[J-K] loading_free;","vector<lower=0>[S] obs_sd;","cholesky_factor_corr[K] L_Omega;","matrix[K,N] z_raw;"),
    transformed=c("vector[J] lambda;","matrix[K,N] z=L_Omega*z_raw;","matrix[N,J] eta;","for(j in 1:J) {","if(anchor_id[j]>0)lambda[j]=loading_anchor[anchor_id[j]];else lambda[j]=loading_free[free_id[j]];","for(n in 1:N)eta[n,j]=alpha[j]+lambda[j]*z[factor_id[j],n];","}"),
    prior=c("for(j in 1:J) {","if(alpha_dist[j]==1)alpha[j] ~ normal(alpha_mean[j],alpha_sd[j]);","else if(alpha_dist[j]==2)alpha[j] ~ logistic(alpha_mean[j],alpha_sd[j]);","else alpha[j] ~ student_t(alpha_df[j],alpha_mean[j],alpha_sd[j]);","}","lambda ~ normal(0,loading_sd);","obs_sd ~ normal(0,sd_prior);","L_Omega ~ lkj_corr_cholesky(lkj_shape);","to_vector(z_raw) ~ std_normal();"),
    generated=c("corr_matrix[K] Omega=multiply_lower_tri_self_transpose(L_Omega);","matrix[J,K] Lambda=rep_matrix(0.0,J,K);","matrix[J,J] Sigma_predictor;","for(j in 1:J)Lambda[j,factor_id[j]]=lambda[j];","Sigma_predictor=Lambda*Omega*transpose(Lambda);"),
    init=list(alpha=unname(data$alpha_mean),loading_anchor=rep(.5,index$K),loading_free=rep(.5,index$J-index$K),obs_sd=rep(1,index$S),L_Omega=diag(index$K),z_raw=matrix(0,index$K,length(spec$data_map$subject_keys))),
    monitored=c("alpha","lambda",if(index$S)"obs_sd","Omega"),estimand="unit-variance factor correlation; predictor-scale task covariance",
    common_keys=names(spec$structure$factors),estimated_output_reals=2*index$K*length(spec$data_map$subject_keys)+4*index$J+2*index$K^2+index$J*index$K+index$J^2),class="measurement_structure_ir")
}
verification_outputs <- function(spec, log_lik=FALSE) {
  .bh_pf_refuse_execution(spec,"verification_outputs")
  if(!inherits(spec,"measurement_spec")||!is.logical(log_lik)||length(log_lik)!=1L||is.na(log_lik)) .mp_abort("E_OUTPUT","explicit logical log_lik flag required")
  spec$verification_outputs<-list(log_lik=log_lik,purpose="explicit deterministic reference verification, not LOO")
  spec
}
