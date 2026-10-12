# Diagnostic factorization is separate from observation/compiler qualification.
# A positive fixed factor preserves the Gaussian likelihood equivalence classes
# and normalizing integral. No block or likelihood row is removed from the model.
.bh_gaussian_diagnostic_scope <- function(spec) {
  kinds<-vapply(spec$models,function(b)identical(.bh_builtin_kind(b$model),"gaussian"),logical(1))
  gaussian_blocks<-names(spec$models)[kinds]
  records<-list();eligible<-length(gaussian_blocks)>0L;positive<-TRUE
  for(b in names(spec$models)[!kinds]) {
    model<-spec$models[[b]]$model;d<-spec$data_map$blocks[[b]]
    roles<-spec$tables$parameter_roles
    roles<-roles[roles$model==b,,drop=FALSE]
    operands<-spec$tables$observation_operands
    operands<-operands[operands$block==b,,drop=FALSE]
    fixed<-!is.null(spec$tables$parameter_roles)&&all(!roles$free_parameter)&&
      (!nrow(d)||all(operands$operand%in%c("fixed_received","route_literal")))
    status<-if(fixed)"opaque_fixed_factor"else"not_parameter_independent"
    basis<-if(fixed)"typed operands are fixed; this density's constant value and validity were not evaluated"else"non-Gaussian free operands remain"
    log_constant<-NA_real_
    numerical<-"not_evaluated"
    if(fixed&&!nrow(d)) {
      status<-"positive_finite_constant";basis<-"empty fixed block has product likelihood one";log_constant<-0;numerical<-"finite_log_constant"
    }else if(fixed) {
      fields<-.bh_binomial_row_fields(model)
      if(!is.null(fields)) {
        n<-d[[fields$trials]];y<-d[[fields$response]]
        argument<-model$likelihood$arguments[3L]
        eta<-if(argument%in%names(d))d[[argument]]else if(argument%in%names(model$constants))rep(model$constants[[argument]]$value,nrow(d))else {
          a<-operands[operands$argument==argument,,drop=FALSE]
          a$received_constant[match(d$.observation_id,a$observation_id)]
        }
        valid<-length(eta)==nrow(d)&&all(is.finite(eta))&&all(is.finite(n)&is.finite(y)&n>=0&y>=0&y<=n&n==floor(n)&y==floor(y))
        if(valid) {
          status<-"positive_finite_constant"
          basis<-"authoritative binomial_logit_lpmf with valid finite integer counts and fixed finite received logit; every probability is mathematically strictly positive"
          softplus<-function(x)pmax(x,0)+log1p(exp(-abs(x)))
          left<-right<-numeric(length(y))
          left[y>0]<-y[y>0]*softplus(-eta[y>0])
          right[n>y]<-(n-y)[n>y]*softplus(eta[n>y])
          candidate<-sum(lchoose(n,y)-left-right)
          if(is.finite(candidate)){log_constant<-candidate;numerical<-"finite_log_constant"}else numerical<-"log_constant_not_representable; mathematical positivity certificate remains valid"
        }else basis<-"fixed primitive operands do not satisfy its finite positive row-domain certificate"
      }
    }
    eligible<-eligible&&fixed
    positive<-positive&&status=="positive_finite_constant"
    records[[b]]<-data.frame(block=b,rows=nrow(d),status=status,basis=basis,log_constant=log_constant,numerical_status=numerical,stringsAsFactors=FALSE)
  }
  list(status=if(!eligible)"not_assessed"else if(positive)"gaussian_certificate_applicable"else"fixed_factorization_value_unassessed",
    eligible=eligible,positive_factors_certified=positive,all_gaussian=all(kinds),gaussian_blocks=gaussian_blocks,
    constant_factors=if(length(records))do.call(rbind,records)else data.frame(),
    scope="known Gaussian response moments plus parameter-independent fixed factors; positivity/value certification is reported separately and all original likelihood rows and priors remain present",
    augmentation="iid standard-Normal person coordinates unused by fixed-only rows have normalized independent densities and integrate to one",
    qualification="diagnostic factorization only; this does not confer a Gaussian native execution profile")
}
