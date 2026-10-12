# Sufficient collapse proofs for exact continuous iid Gaussian data only.
# This is separate from structural/local identification and never changes data.
.bh_gaussian_propriety <- function(spec,diagnostic_scope=.bh_gaussian_diagnostic_scope(spec)) {
  if(.bh_has_affine_correlations(spec)&&!identical(.bh_correlation_feasibility(spec)$status,"feasible"))return(list(status="not_assessed",reason="strict correlation-domain support has not been established; no valid-model posterior normalizability claim is made"))
  scoped<-(identical(spec$formula$person,"iid")||.bh_no_active_person_effect(spec)) && isTRUE(diagnostic_scope$eligible)
  if(!scoped)return(list(status="not_assessed",reason="the exact-data collapse proofs are currently restricted to known iid Gaussian blocks with separate free observation SDs"))
  if(any(spec$tables$priors$status!="specified")||any(!is.na(.bh_resolved_prior_errors(spec$tables$priors))))return(list(status="not_assessed",reason="complete valid finite supported priors are required to assess the local density conditions"))
  findings<-list();targets<-.bh_gaussian_responses(spec)
  for(j in seq_len(nrow(targets))) {
    t<-targets[j,];d<-spec$data_map$blocks[[t$model]];d<-d[d$.task==t$task,,drop=FALSE]
    y<-d$y;M<-length(y);P<-length(unique(d$.person_key))
    if(!M||!t$noise_free)next
    map<-spec$tables$loading_map
    incident<-map[map$target==t$key & map$multiplier!=0,,drop=FALSE]
    scales<-.bh_free_component_sd(spec)
    marked<-incident$coordinate_index%in%scales$component_index
    active<-incident$role!="fixed_loading"|(!is.na(incident$fixed_value)&incident$fixed_value!=0)
    coefficients<-unique(c(incident$coefficient[!marked & active],
      scales$parameter[match(unique(incident$coordinate_index[marked & active]),scales$component_index)],
      spec$tables$parameter_residuals$parameter[spec$tables$parameter_residuals$target==t$key]))
    coefficients<-coefficients[!is.na(coefficients)]
    if(.bh_has_shares(spec)&&t$key%in%spec$tables$share_targets$target)coefficients<-spec$tables$share_targets$parameter[match(t$key,spec$tables$share_targets$target)]
    D<-length(unique(coefficients))
    fixed_nonzero<-any(incident$role=="fixed_loading" & active & !marked)
    constant<-all(y==y[1L])
    within_constant<-all(vapply(split(y,d$.person_key),function(x)all(x==x[1L]),logical(1)))
    paths<-character()
    free_mean<-t$mean_level!="fixed"
    matching_fixed_mean<-!free_mean&&isTRUE(t$fixed_mean==y[1L])
    threshold<-D+if(free_mean)2L else 1L
    if(!fixed_nonzero&&constant&&(free_mean||matching_fixed_mean)&&M>=threshold)paths<-c(paths,if(free_mean)"constant_target_joint_scale_collapse"else"fixed_matching_mean_scale_collapse")
    if((D>0L||fixed_nonzero)&&M>P&&within_constant)paths<-c(paths,"exact_within_person_repeat_sigma_collapse")
    if(length(paths))findings[[length(findings)+1L]]<-data.frame(target=t$key,observations=M,people=P,
      independently_shrinking_coefficients=D,coefficients=paste(coefficients,collapse="; "),
      constant_target=constant,all_within_person_values_equal=within_constant,
      mean_level=t$mean_level,collapse_observation_threshold=threshold,
      sufficient_paths=paste(paths,collapse="; "),stringsAsFactors=FALSE)
  }
  table<-if(length(findings))do.call(rbind,findings)else data.frame()
  opaque<-!isTRUE(diagnostic_scope$positive_factors_certified)
  list(status=if(nrow(table))if(opaque)"proven_non_normalizable"else"proven_improper"else if(opaque)"not_assessed"else"no_scoped_collapse_detected",findings=table,
    reason=if(nrow(table)&&opaque)"the Gaussian factor has a certified divergent normalizing integral; multiplying by an unevaluated parameter-independent constant cannot produce a finite positive normalizer. The opaque factor's value and density validity were not evaluated"else if(nrow(table))"exact Gaussian data and the declared positive-at-zero scale/coefficient priors imply an infinite posterior normalizing integral along a specified collapse path"else"these sufficient exact-data collapse conditions were not detected; this is not a general posterior-propriety proof",
    scope=if(.bh_no_active_person_effect(spec))"known Gaussian mean/noise likelihood with no active person effects; the declared centered roster is retained but contributes no likelihood covariance; free observation SDs and the stated local prior/collapse premises only"else"iid known Gaussian blocks, optionally multiplied by certified positive fixed factors; free observation SDs only, free or explicitly fixed whole means, supported continuous locally positive priors; no shared SD, censoring/rounding likelihood or active sample centering",
    person_law_certificate=if(.bh_no_active_person_effect(spec))"no_active_person_effect"else"iid",
    proof="constant target: dyadic shell mass r^(D+2-M) for a free mean, or r^(D+1-M) for a fixed exactly matching mean; exact repeats with nonzero person variation: r^(P+1-M). These sufficient paths diverge at exponent <=0; fixed positive sigma is excluded and no jitter is used")
}
