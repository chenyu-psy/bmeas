# Explicit portable fit export, contract v1.1. Stage 1 only.
# No consumer dispatch, source acquisition, model constructor, or resealing.
# Source schema inventory: e01a382e122266f3cb6d334165e23e86fd8d4a93af0409c5a20c28fb751f0e80.

.bh_pf_fail <- function(path, message, code = "E_PORTABLE_SCHEMA") {
  .mp_abort(code, paste0(path, ": ", message))
}
.bh_pf_get <- function(x, name) base::.subset2(base::unclass(x), name)
.bh_pf_setequal <- function(x,y) all(match(x,y,nomatch=0L)>0L)&&all(match(y,x,nomatch=0L)>0L)
.bh_pf_setdiff <- function(x,y) { x<-base::unique.default(x); x[match(x,y,nomatch=0L)==0L] }
.bh_pf_intersect <- function(x,y) { x<-base::unique.default(x); x[match(x,y,nomatch=0L)>0L] }
.bh_pf_names <- function(x) base::attr(x, "names", exact = TRUE)
.bh_pf_class <- function(x) base::attr(x, "class", exact = TRUE)
.bh_pf_length <- function(x) base::length(base::unclass(x))
.bh_pf_text_valid <- function(x,path) {
  if(any(Encoding(x)=="bytes") || any(!validEnc(x)))
    .bh_pf_fail(path,"invalid or byte-marked character text","E_PORTABLE_ENCODING")
  invisible(TRUE)
}
.bh_pf_plain <- function(x, types) {
  !base::isS4(x) && base::typeof(x) %in% types && base::is.null(base::attributes(x))
}
.bh_pf_string <- function(x, path, choices = NULL, empty = FALSE) {
  if (.bh_pf_plain(x,"character")) .bh_pf_text_valid(x,path)
  if (!.bh_pf_plain(x, "character") || length(x) != 1L || is.na(x) ||
      (!empty && !nzchar(x)) || (!is.null(choices) && !x %in% choices))
    .bh_pf_fail(path, "expected a supported plain nonmissing string")
  .bh_pf_text_valid(x,path)
  invisible(TRUE)
}
.bh_pf_number <- function(x, path, lower = -Inf, upper = Inf, integer = FALSE,
                          finite = TRUE) {
  if (!.bh_pf_plain(x, c("integer", "double")) || length(x) != 1L || is.na(x) ||
      (finite && !is.finite(x)) || x < lower || x > upper ||
      (integer && x != floor(x)))
    .bh_pf_fail(path, "expected a plain supported numeric scalar")
  invisible(TRUE)
}
.bh_pf_logical <- function(x, path) {
  if (!.bh_pf_plain(x, "logical") || length(x) != 1L || is.na(x))
    .bh_pf_fail(path, "expected a plain TRUE/FALSE scalar")
  invisible(TRUE)
}
.bh_pf_character <- function(x, path, nonempty = FALSE, unique = FALSE,
                              names = FALSE, missing = FALSE) {
  attrs <- attributes(x)
  if (isS4(x) || typeof(x) != "character" ||
      any(!base::names(attrs) %in% if (names) "names" else character()) ||
      (!missing && anyNA(x)) || (nonempty && any(!nzchar(x))) ||
      (unique && base::anyDuplicated.default(x)))
    .bh_pf_fail(path, "expected supported plain character values")
  .bh_pf_text_valid(x,path)
  invisible(TRUE)
}
.bh_pf_record <- function(x, path, allowed, required = allowed, classes = NULL) {
  if (isS4(x) || typeof(x) != "list" ||
      !identical(.bh_pf_class(x), classes) ||
      any(!names(attributes(x)) %in% c("names", if (!is.null(classes)) "class")))
    .bh_pf_fail(path, "expected a record with its exact positional class")
  out <- unclass(x); n <- .bh_pf_names(out)
  if(typeof(n)=="character") .bh_pf_text_valid(n,path)
  if ((!length(out) && is.null(n) && length(required) == 0L)) return(out)
  if (!.bh_pf_plain(n, "character") || length(n) != length(out) || anyNA(n) ||
      any(!nzchar(n)) || base::anyDuplicated.default(n) ||
      any(!n %in% allowed) || any(!required %in% n))
    .bh_pf_fail(path, "unknown, duplicate, absent, or malformed record fields")
  # A missing-argument cell must refuse before .subset2 can force it.
  for (i in seq_along(out)) {
    if(identical(path,"fit") && identical(n[[i]],"backend")) next
    if (identical(unname(.subset(out, i)), unname(alist(value = ))))
      .bh_pf_fail(path, "missing-argument cells are not saved values")
  }
  out
}
.bh_pf_named <- function(x, path, empty = TRUE) {
  if (isS4(x) || typeof(x) != "list" || !is.null(.bh_pf_class(x)) ||
      any(!names(attributes(x)) %in% "names")) .bh_pf_fail(path, "expected a plain named collection")
  n <- .bh_pf_names(x)
  if(typeof(n)=="character") .bh_pf_text_valid(n,path)
  if (!length(x) && empty && is.null(n)) return(x)
  if (!.bh_pf_plain(n, "character") || length(n) != length(x) || anyNA(n) ||
      any(!nzchar(n)) || base::anyDuplicated.default(n) || (!empty && !length(x)))
    .bh_pf_fail(path, "collection keys must be distinct nonempty saved strings")
  x
}
.bh_pf_table <- function(x, path, allowed, required = allowed) {
  if (isS4(x) || typeof(x) != "list" || !identical(.bh_pf_class(x), "data.frame"))
    .bh_pf_fail(path, "expected a plain saved data.frame")
  n <- .bh_pf_names(x)
  if(typeof(n)=="character") .bh_pf_text_valid(n,path)
  if (!.bh_pf_plain(n, "character") || length(n) != .bh_pf_length(x) || anyNA(n) ||
      any(!nzchar(n)) || base::anyDuplicated.default(n) || any(!n %in% allowed) || any(!required %in% n))
    .bh_pf_fail(path, "table has unknown, duplicate, or absent columns")
  .bh_pf_table_shape(x, path)
  unclass(x)
}
.bh_pf_table_shape <- function(x, path) {
  # .row_names_info(type=0) returns compact automatic row names without expansion.
  rn <- base::.row_names_info(x, 0L)
  if (!typeof(rn) %in% c("integer", "character") || isS4(rn) || !is.null(attributes(rn)))
    .bh_pf_fail(path, "unsupported saved row-name representation")
  compact <- typeof(rn) == "integer" && length(rn) == 2L && is.na(rn[[1L]])
  if (compact) {
    if (is.na(rn[[2L]])) .bh_pf_fail(path, "malformed compact row names")
    rows <- abs(as.double(rn[[2L]]))
  } else {
    if (anyNA(rn) || base::anyDuplicated.default(rn)) .bh_pf_fail(path, "malformed explicit row names")
    rows <- as.double(length(rn))
  }
  values <- unclass(x)
  for (i in seq_along(values)) {
    v <- .subset2(values, i); d <- attr(v, "dim", exact = TRUE)
    size <- if (is.null(d)) as.double(.bh_pf_length(v)) else as.double(.subset2(d, 1L))
    if (length(size) != 1L || is.na(size) || size != rows)
      .bh_pf_fail(path, "column dimensions disagree with saved row names")
  }
  invisible(rows)
}
.bh_pf_policy <- function(budget) {
  x <- .bh_pf_record(budget, "budget", c("max_context_bytes", "max_context_nodes", "max_depth"),
    c("max_context_bytes", "max_context_nodes"))
  for (n in c("max_context_bytes", "max_context_nodes"))
    .bh_pf_number(.subset2(x,n), paste0("budget.",n), 1, (2^53 - 1), TRUE)
  depth <- if ("max_depth" %in% names(x)) .subset2(x,"max_depth") else 64.0
  .bh_pf_number(depth,"budget.max_depth",1,2147483647.0,TRUE)
  list(max_context_bytes=as.double(x$max_context_bytes),max_context_nodes=as.double(x$max_context_nodes),
       max_depth=as.double(depth))
}

# Every class is admitted by structural position, never a global tag allowlist.
.bh_pf_context_path <- function(path) {
  if (length(path)>=5L && identical(path[1:3],c("cache","posterior_reporting","phases")) && path[[5L]]=="record") {
    field <- c(conditioning="admissibility", availability_and_covariance="optional_reports", expanded_summary="expanded_correlation_matrices", numerical_messages="numerical_evaluations")[[path[[4L]]]]
    if (!is.null(field)) path <- c("cache",field,if(length(path)>5L)path[-seq_len(5L)])
  }
  if(length(path)>=3L && identical(path[1:3],c("cache","reference_law","resolved_priors"))) path<-c("spec","tables","priors",if(length(path)>3L)path[-seq_len(3L)])
  path
}
.bh_pf_class_allowed <- function(cls, path, type, column = FALSE) {
  path <- .bh_pf_context_path(path)
  if (is.null(cls)) return(TRUE)
  if (column && (identical(cls,"factor") || identical(cls,c("ordered","factor"))))
    return(type == "integer")
  if (column && identical(cls,"AsIs")) return(type == "list" || type %in% c("logical","integer","double","character"))
  if (identical(cls,"data.frame")) return(type == "list" &&
    (length(path) >= 2L && path[[1L]] %in% c("spec","cache","provenance","execution_receipt","resource_outcome","run")))
  fixed <- list(
    spec=c("bmeas_spec","bhmm_spec"), fit=c("bmeas_fit","bhmm_fit"),
    `spec.formula`=c("bmeas_formula","bhmm_formula"),
    `spec.subjects`="measurement_subjects", `spec.prior`="bhmm_prior", run="measurement_run_config",
    `cache.ppc.metadata.plan`="bmeas_ppc_plan")
  key <- paste(path,collapse=".")
  expected <- .subset2(fixed,key)
  if (key=="spec.formula" && identical(cls,"bhmm_formula")) return(TRUE)
  if (!is.null(expected) && identical(cls,expected)) return(TRUE)
  if (length(path)==3L && identical(path[1:2],c("spec","models")))
    return(identical(cls,"observation_block"))
  if (length(path)==4L && identical(path[c(1L,2L,4L)],c("spec","models","model")))
    return(identical(cls,"observation_model"))
  if (length(path)==6L && identical(path[c(1L,2L,4L,5L)],c("spec","models","model","data_schema")))
    return(identical(cls,"measurement_data_field"))
  if (length(path)==4L && identical(path[c(1L,2L,4L)],c("spec","models","parameter_bindings")))
    return(identical(cls,"measurement_parameter_bindings"))
  if (length(path)==7L && identical(path[c(1L,2L,4L,5L,6L)],c("spec","models","model","adapter","resources")))
    return(identical(cls,"measurement_stan_resource"))
  if (length(path)==4L && identical(path[1:3],c("spec","formula","components")))
    return(identical(cls,"bhmm_basis_component"))
  if (length(path)==4L && identical(path[c(1L,2L,4L)],c("spec","prior","target")))
    return(identical(cls,"bhmm_prior_targets"))
  if (identical(cls,c("matrix","array")) &&
      key %in% c("cache.optional_reports.flags","cache.optional_reports.log_magnitudes"))
    return(type %in% c("logical","integer","double"))
  FALSE
}
.bh_pf_scientific_attributes <- function(path) {
  path <- .bh_pf_context_path(path)
  key <- paste(path,collapse=".")
  if (key == "spec.tables.priors") return(c("loading_calibration","loading_aliases","component_sd_calibration",
    "calibration","correlation_measure","selection_semantics","parameterization"))
  if (key == "spec.tables.parameters") return("scope")
  if (key %in% c("spec.formula.correlations","spec.formula.residual.correlations",
                 "spec.tables.correlations","spec.tables.parameter_correlations")) return("affine_rules")
  if (key == "cache.ppc.table") return(c("coverage","level"))
  character()
}
# Callers have already admitted nonnegative integer-valued counts.
.bh_pf_exact_product <- function(a, b, path, message) {
  if (b && a > (2^53 - 1)/b) .bh_pf_fail(path, message)
  a*b
}
.bh_pf_plain_tree <- function(x, path, depth = 1, column = FALSE, limit = 2147483647.0) {
  # path is a vector of literal structural segments; display dots never confer admission.
  summary_args_path <- identical(path,c("cache","parameter_summary"))
  if (length(path)==1L && grepl("\\.",path,fixed=FALSE)) path <- strsplit(path,".",fixed=TRUE)[[1L]]
  label <- paste(path,collapse=".")
  if (depth > limit) .bh_pf_fail(label,"context nesting exceeds the declared allowance","E_PORTABLE_BUDGET")
  if (isS4(x) || !typeof(x) %in% c("NULL","logical","integer","double","character","raw","list"))
    .bh_pf_fail(label,"executable, reference-bearing, or unsupported saved value")
  if (is.null(x)) return(invisible(TRUE))
  type <- typeof(x); attrs <- attributes(x); an <- names(attrs); cls <- .bh_pf_class(x)
  if(type=="character") .bh_pf_text_valid(unclass(x),label)
  if(typeof(an)=="character") .bh_pf_text_valid(an,label)
  if (length(attrs) && (!.bh_pf_plain(an,"character") || anyNA(an) || any(!nzchar(an)) || base::anyDuplicated.default(an)))
    .bh_pf_fail(label,"malformed attribute names")
  if (!.bh_pf_class_allowed(cls,path,type,column)) .bh_pf_fail(label,"class is not supported at this position")
  factor <- identical(cls,"factor") || identical(cls,c("ordered","factor"))
  frame <- identical(cls,"data.frame")
  allowed <- c("names", "dim", "dimnames", if (!is.null(cls)) "class",
    if (frame) "row.names", if (factor) "levels", .bh_pf_scientific_attributes(path),
    if (frame && summary_args_path) "num_args")
  if (any(!an %in% allowed)) .bh_pf_fail(label,"unknown saved attribute")
  if ("num_args" %in% an) {
    num_args <- attr(x,"num_args",exact=TRUE)
    if (!.bh_pf_plain(num_args,"list") || length(num_args)!=0L)
      .bh_pf_fail(label,"saved summary num_args must be an attribute-free empty list")
  }
  n <- .bh_pf_names(x)
  if(typeof(n)=="character") .bh_pf_text_valid(n,label)
  if (!is.null(n) && (!.bh_pf_plain(n,"character") || length(n)!=.bh_pf_length(x)))
    .bh_pf_fail(label,"malformed names attribute")
  if (!is.null(cls) && (!.bh_pf_plain(cls,"character") || anyNA(cls)))
    .bh_pf_fail(label,"malformed class sequence")
  d <- attr(x,"dim",exact=TRUE); dn <- attr(x,"dimnames",exact=TRUE)
  if (!is.null(d)) {
    if (!.bh_pf_plain(d,"integer") || !length(d) || anyNA(d) || any(d<0L))
      .bh_pf_fail(label,"malformed dimension vector")
    product <- 1.0
    for (v in d) {
      product <- .bh_pf_exact_product(product, as.double(v), label, "dimension product overflow")
    }
    if (product != as.double(.bh_pf_length(x))) .bh_pf_fail(label,"dimensions do not cover saved storage")
  } else if (!is.null(dn)) .bh_pf_fail(label,"dimension labels require dimensions")
  if (!is.null(dn)) {
    if (typeof(dn)!="list" || isS4(dn) || any(!names(attributes(dn)) %in% "names") || length(dn)!=length(d))
      .bh_pf_fail(label,"malformed dimension labels")
    for (i in seq_along(dn)) {
      v <- .subset2(dn,i)
      # Character axes may retain their original names metadata. The recursive
      # attribute walk below validates its type, length and text encoding.
      if (!is.null(v) && (isS4(v) || typeof(v)!="character" ||
          any(!names(attributes(v)) %in% "names") || .bh_pf_length(v)!=d[[i]]))
        .bh_pf_fail(label,"dimension-label lengths or attributes differ from the admitted axes")
    }
  }
  if (factor) {
    levels <- attr(x,"levels",exact=TRUE)
    if (!.bh_pf_plain(levels,"character") || anyNA(levels) || base::anyDuplicated.default(levels) ||
        !is.null(d) || any(!is.na(unclass(x)) & (unclass(x)<1L | unclass(x)>length(levels))))
      .bh_pf_fail(label,"malformed factor codes or levels")
  }
  if (identical(cls,"AsIs") && type != "list" && (is.null(d) || length(d)!=2L))
    .bh_pf_fail(label,"AsIs is admitted only for inert list or matrix columns")
  if (frame) .bh_pf_table_shape(x,label)
  if (type=="list") {
    values <- unclass(x)
    for (i in seq_along(values)) {
      # Record/table keys are validated by the positional schema; unnamed collections use index labels.
      part <- if (!is.null(n) && !is.na(n[[i]]) && nzchar(n[[i]])) n[[i]] else paste0("[",i,"]")
      .bh_pf_plain_tree(.subset2(values,i),c(path,part),depth+1.0,frame,limit)
    }
  }
  # Attribute values have separate nodes. Scientific attribute payloads retain
  # their positional schema but cannot smuggle an executable attribute class.
  if (length(attrs)) for (i in seq_along(attrs)) {
    name <- an[[i]]
    v <- if (name=="row.names") base::.row_names_info(x,0L) else .subset2(attrs,i)
    if (name %in% c("names","class","dim","levels","row.names")) {
      if (!is.null(attributes(v))) .bh_pf_fail(label,"attribute values must use ordinary unclassed metadata")
    } else if (name=="dimnames") {
      .bh_pf_plain_tree(v,c(path,"@dimnames"),depth+2.0,FALSE,limit)
    } else .bh_pf_plain_tree(v,c(path,paste0("@",name)),depth+2.0,FALSE,limit)
  }
  invisible(TRUE)
}

.bh_pf_md5 <- function(x, path) {
  .bh_pf_string(x,path)
  if (!grepl("^[0-9a-f]{32}$",x)) .bh_pf_fail(path,"expected a lowercase legacy MD5 string")
  invisible(TRUE)
}
.bh_pf_take <- function(x, fields) {
  values <- unclass(x); n <- .bh_pf_names(values)
  .subset(values,which(n %in% fields))
}
.bh_pf_timestamp <- function(x, path) {
  cls <- .bh_pf_class(x)
  if (is.null(x) || is.null(cls)) return(x)
  if (isS4(x) || !identical(cls,c("POSIXct","POSIXt")) ||
      !typeof(x) %in% c("double","integer") ||
      any(!names(attributes(x)) %in% c("class","tzone")))
    .bh_pf_fail(path,"unsupported timestamp representation")
  zone <- attr(x,"tzone",exact=TRUE)
  present <- "tzone" %in% names(attributes(x))
  if (present && !is.null(zone)) .bh_pf_character(zone,paste0(path,".tzone"),missing=FALSE)
  epoch <- unclass(x); attributes(epoch) <- NULL
  list(representation="bmeas_inert_timestamp_v1",epoch=epoch,original_class=cls,
       timezone_present=present,timezone=zone)
}
.bh_pf_timestamps <- function(provenance, execution) {
  p <- provenance; e <- execution
  for (name in .bh_pf_intersect(c("started","finished"),.bh_pf_names(p)))
    p[name] <- list(.bh_pf_timestamp(.bh_pf_get(p,name),paste0("provenance.",name)))
  if (!is.null(e) && "sampling" %in% .bh_pf_names(e)) {
    s <- .bh_pf_get(e,"sampling")
    for (name in .bh_pf_intersect(c("started","finished"),.bh_pf_names(s)))
      s[name] <- list(.bh_pf_timestamp(.bh_pf_get(s,name),paste0("execution_receipt.sampling.",name)))
    e["sampling"] <- list(s)
  }
  list(provenance=p,execution_receipt=e)
}

.bh_pf_data_field <- function(x,path) {
  v <- .bh_pf_record(x,path,c("type","role","units","lower","upper","width","dimensions"),
                     classes="measurement_data_field")
  .bh_pf_string(v$type,paste0(path,".type"),c("real","positive_real","integer","nonnegative_integer","binary"))
  .bh_pf_string(v$role,paste0(path,".role"),c("response","auxiliary"))
  if (!is.null(v$units)) .bh_pf_string(v$units,paste0(path,".units"))
  .bh_pf_number(v$lower,paste0(path,".lower"),finite=FALSE)
  .bh_pf_number(v$upper,paste0(path,".upper"),finite=FALSE)
  .bh_pf_number(v$width,paste0(path,".width"),1,100,TRUE)
  if (v$lower>v$upper) .bh_pf_fail(path,"field bounds are reversed")
  expected <- if (v$width==1L) "one value per observation" else paste(v$width,"values per observation")
  if (!identical(v$dimensions,expected)) .bh_pf_fail(path,"field dimensions disagree with width")
  invisible(TRUE)
}
.bh_pf_parameter <- function(x,path) {
  fields <- c("support","predictor","receives","levels","units","predictor_units","description","transform_owner","prior_scale")
  v <- .bh_pf_record(x,path,c(fields,"role"),fields)
  .bh_pf_string(v$support,paste0(path,".support"),c("real","positive","unit_interval"))
  .bh_pf_string(v$predictor,paste0(path,".predictor"),c("identity","log","logit"))
  .bh_pf_string(v$receives,paste0(path,".receives"),c("eta","theta"))
  .bh_pf_character(v$levels,paste0(path,".levels"),TRUE,TRUE)
  if (!length(v$levels) || any(!v$levels %in% c("subject_task","task","fixed")))
    .bh_pf_fail(path,"unsupported parameter levels")
  expected <- c(real="identity",positive="log",unit_interval="logit")[[v$support]]
  if (!identical(v$predictor,expected)) .bh_pf_fail(path,"support and predictor link disagree")
  for (n in c("units","predictor_units","description")) .bh_pf_string(v[[n]],paste0(path,".",n))
  if (!identical(v$transform_owner,"adapter") || !identical(v$prior_scale,"must be declared by structure"))
    .bh_pf_fail(path,"unrecognized transformation or prior-scale ownership")
  if ("role" %in% names(v)) {
    .bh_pf_string(v$role,paste0(path,".role"),c("predictor","observation_scale"))
    if (length(v$levels)!=1L || (v$role=="observation_scale" &&
        (v$support!="positive" || v$receives!="theta" || v$levels=="subject_task")))
      .bh_pf_fail(path,"explicit parameter role conflicts with support, scale, or level")
  }
  invisible(TRUE)
}
.bh_pf_constant <- function(x,path) {
  v <- .bh_pf_record(x,path,c("value","type","units","provenance"))
  .bh_pf_string(v$type,paste0(path,".type"),c("real","int","vector","matrix"))
  .bh_pf_string(v$units,paste0(path,".units"));.bh_pf_string(v$provenance,paste0(path,".provenance"))
  z<-v$value;d<-attr(z,"dim",exact=TRUE)
  if (!typeof(z)%in%c("integer","double") || is.object(z) || !length(z) || any(!is.finite(z)))
    .bh_pf_fail(path,"constant storage must be finite plain numeric values")
  if (v$type%in%c("real","int") && length(z)!=1L) .bh_pf_fail(path,"scalar constant has nonscalar storage")
  if (v$type=="int" && (z!=floor(z) || abs(z)>2147483647)) .bh_pf_fail(path,"integer constant exceeds its declared domain")
  if (v$type=="vector" && !is.null(d)) .bh_pf_fail(path,"vector constant has dimensions")
  if (v$type=="matrix" && (length(d)!=2L || any(d>200L))) .bh_pf_fail(path,"unsupported matrix constant shape")
  invisible(TRUE)
}
.bh_pf_resource <- function(x,path) {
  v <- .bh_pf_record(x,path,c("file","sha256","exports","license","source","cache_name"),classes="measurement_stan_resource")
  for(n in c("file","sha256","license","source","cache_name")) .bh_pf_string(v[[n]],paste0(path,".",n))
  if (!grepl("^[0-9a-f]{64}$",v$sha256) || !identical(v$cache_name,paste0("external_",v$sha256,".stanfunctions")))
    .bh_pf_fail(path,"resource pin or pin-derived cache name is malformed")
  .bh_pf_character(v$exports,paste0(path,".exports"),TRUE,TRUE)
  if (!length(v$exports) || any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",v$exports)))
    .bh_pf_fail(path,"resource exports must be explicit distinct identifiers")
  invisible(TRUE)
}
.bh_pf_density <- function(x,path) {
  fields <- c("symbol","arguments","argument_types","normalized","approximation","builtin","source","source_file", "resource_hash","exports","dependencies","license","censoring","truncation")
  v <- .bh_pf_record(x,path,fields)
  .bh_pf_string(v$symbol,paste0(path,".symbol"))
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*_(lpdf|lpmf)$",v$symbol)) .bh_pf_fail(path,"unrecognized density identifier")
  .bh_pf_character(v$arguments,paste0(path,".arguments"),TRUE,TRUE)
  .bh_pf_character(v$argument_types,paste0(path,".argument_types"),TRUE)
  if (!length(v$arguments) || length(v$arguments)!=length(v$argument_types) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",v$arguments)) ||
      any(!v$argument_types%in%c("int","real","int_array","real_array","vector","matrix")))
    .bh_pf_fail(path,"malformed typed density arguments")
  .bh_pf_logical(v$normalized,paste0(path,".normalized"));.bh_pf_logical(v$builtin,paste0(path,".builtin"))
  if (!isTRUE(v$normalized) || !identical(v$censoring,"none") || !identical(v$truncation,"none"))
    .bh_pf_fail(path,"this portable profile requires the saved normalized uncensored row-density contract")
  for(n in c("approximation","license")) .bh_pf_string(v[[n]],paste0(path,".",n))
  for(n in c("exports","dependencies")) .bh_pf_character(v[[n]],paste0(path,".",n),TRUE,TRUE)
  if (!is.null(v$source)) .bh_pf_character(v$source,paste0(path,".source"))
  if (!is.null(v$source_file)) .bh_pf_string(v$source_file,paste0(path,".source_file"))
  if (!is.null(v$resource_hash)) .bh_pf_md5(v$resource_hash,paste0(path,".resource_hash"))
  if (!v$builtin && (is.null(v$source_file) || is.null(v$resource_hash)))
    .bh_pf_fail(path,"external density lacks its original path/pin descriptors")
  invisible(TRUE)
}
.bh_pf_argument_types <- function(arguments,types,model,path,formal=NULL) {
  fields<-model$data_schema;parameters<-model$parameters;constants<-model$constants
  for(i in seq_along(arguments)) {
    key<-arguments[[i]]
    expected<-if(!is.null(formal) && key%in%names(formal)) "real" else
      if(key%in%names(fields)) {
        field<-.bh_pf_get(fields,key)
        paste0(if(.bh_pf_get(field,"type")%in%c("integer","nonnegative_integer","binary"))"int"else"real",
               if(.bh_pf_get(field,"width")>1L)"_array"else"")
      } else if(is.null(formal) && key%in%names(parameters)) "real" else
        if(key%in%names(constants)) .bh_pf_get(.bh_pf_get(constants,key),"type") else NULL
    if(is.null(expected)||!identical(types[[i]],expected)) .bh_pf_fail(path,"a saved formal argument has an absent or differently typed binding")
  }
  invisible(TRUE)
}
.bh_pf_adapter <- function(x,model,path) {
  fields<-c("primary","nuisance","resources","function_code","function_exports","wrapper_hash","version","independence_unit","parameter_binding")
  a<-.bh_pf_record(x,path,c(fields,"mode","selector","routes"),fields)
  if(!identical(a$version,"typed_row_adapter_v1") ||
     !identical(a$independence_unit,"one explicit observation row; arrays remain one joint observation"))
    .bh_pf_fail(path,"unsupported saved adapter version or independence unit")
  routed<-"mode"%in%names(a)
  if(routed && (!identical(a$mode,"routed") || !all(c("selector","routes")%in%names(a))))
    .bh_pf_fail(path,"routed adapter requires its mode, selector and routes")
  if(!routed && any(c("selector","routes")%in%names(a))) .bh_pf_fail(path,"row adapter contains routed fields without a routed mode")
  binding<-if(routed)"explicit per-observation formal argument to domain target or fixed predictor"else"same task for every declared parameter; cross-task references are not implemented"
  if(!identical(a$parameter_binding,binding)) .bh_pf_fail(path,"unsupported saved binding interpretation")
  for(n in c("primary","nuisance","function_code","function_exports")) .bh_pf_character(a[[n]],paste0(path,".",n),unique=n!="function_code")
  .bh_pf_md5(a$wrapper_hash,paste0(path,".wrapper_hash"))
  if(any(grepl("#include",a$function_code,fixed=TRUE)) ||
     (length(a$function_code)>0L && length(a$function_exports)==0L) ||
     any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",a$function_exports)))
    .bh_pf_fail(path,"saved wrapper descriptor is incomplete or contains an unsupported include")
  resources<-a$resources
  if(isS4(resources)||typeof(resources)!="list"||!is.null(.bh_pf_class(resources))||any(!names(attributes(resources))%in%"names"))
    .bh_pf_fail(path,"resources must be a saved plain collection")
  exports<-a$function_exports
  for(i in seq_along(resources)) {
    .bh_pf_resource(.subset2(resources,i),paste0(path,".resources[",i,"]"))
    exports<-c(exports,.bh_pf_get(.subset2(resources,i),"exports"))
  }
  primary<-nuisance<-character()
  for(n in names(model$parameters)) {
    p<-.bh_pf_get(model$parameters,n)
    if(length(.bh_pf_get(p,"levels"))!=1L) .bh_pf_fail(path,"typed adapter has no single default parameter level")
    role<-.bh_pf_get(p,"role")
    if(is.null(role)) {
      default<-.bh_pf_get(p,"levels")
      if(!default%in%c("subject_task","task")) .bh_pf_fail(path,"nonstandard parameter default needs an explicit role")
      role<-if(identical(default,"subject_task"))"predictor"else"observation_scale"
    }
    if(role=="predictor")primary<-c(primary,n)else nuisance<-c(nuisance,n)
  }
  if(!identical(a$primary,primary)||!identical(a$nuisance,nuisance)) .bh_pf_fail(path,"saved adapter role indices disagree with parameter descriptors")
  available<-c(exports,"normal_lpdf","binomial_logit_lpmf","lognormal_lpdf")
  if(!model$likelihood$symbol%in%available || any(!model$likelihood$dependencies%in%available))
    .bh_pf_fail(path,"saved density/dependency symbols lack declared resource or wrapper exports")
  if(routed) {
    .bh_pf_string(a$selector,paste0(path,".selector"))
    sf<-.bh_pf_get(model$data_schema,a$selector)
    if(is.null(sf)||!.bh_pf_get(sf,"type")%in%c("integer","nonnegative_integer")||.bh_pf_get(sf,"width")!=1L)
      .bh_pf_fail(path,"route selector must bind a declared scalar integer field")
    routes<-.bh_pf_named(a$routes,paste0(path,".routes"),FALSE);selectors<-numeric();consumed<-character()
    for(n in names(routes)) {
      rp<-paste0(path,".routes.",n)
      r<-.bh_pf_record(.subset2(routes,n),rp,c("selector_value","symbol","arguments","argument_types","parameter_arguments"))
      .bh_pf_number(r$selector_value,paste0(rp,".selector_value"),1,2147483647,TRUE)
      .bh_pf_string(r$symbol,paste0(rp,".symbol"))
      .bh_pf_character(r$arguments,paste0(rp,".arguments"),TRUE,TRUE)
      .bh_pf_character(r$argument_types,paste0(rp,".argument_types"),TRUE)
      f<-.bh_pf_named(r$parameter_arguments,paste0(rp,".parameter_arguments"),FALSE)
      for(arg in names(f)) .bh_pf_string(.subset2(f,arg),paste0(rp,".parameter_arguments.",arg),names(model$parameters))
      if(length(r$arguments)!=length(r$argument_types)||!r$symbol%in%exports ||
         any(!names(f)%in%r$arguments)||any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",names(f))))
        .bh_pf_fail(rp,"routed descriptor is incomplete or contradicts its exported symbol")
      .bh_pf_argument_types(r$arguments,r$argument_types,model,rp,f)
      selectors<-c(selectors,as.double(r$selector_value));consumed<-c(consumed,unlist(f,use.names=FALSE))
    }
    if(base::anyDuplicated.default(selectors)||!.bh_pf_setequal(consumed,names(model$parameters))) .bh_pf_fail(path,"route selectors repeat or parameter consumption is incomplete")
  }
  invisible(TRUE)
}
.bh_pf_callbacks <- function(model,block_name) {
  allowed<-c("name","version","provenance","data_schema","parameters","parameter_dependencies","likelihood","validate_data","prepare_data","capabilities","check_statistics","backend","methods","constants","adapter","required_units","informative_rows","reference_parameter_hash","limitations","validate_bindings")
  required<-.bh_pf_setdiff(allowed,c("validate_data","prepare_data","methods","adapter","required_units","informative_rows","reference_parameter_hash","limitations","validate_bindings"))
  values<-.bh_pf_record(model,paste0("spec.models.",block_name,".model"),allowed,required,classes="observation_model")
  markers<-vector("list",7L)
  callbacks<-c("validate_data","prepare_data","validate_bindings","informative_rows")
  methods<-.bh_pf_get(values,"methods")
  if(!is.null(methods)) methods<-.bh_pf_record(methods,paste0("spec.models.",block_name,".model.methods"),
                         c("simulate","posterior_predict","posterior_epred"),character())
  slots<-c(callbacks,paste0("methods.",c("simulate","posterior_predict","posterior_epred")))
  for(i in seq_along(slots)) {
    method<-i>4L;field<-if(method)substring(slots[[i]],9L)else slots[[i]]
    owner<-if(method)methods else values;present<-field%in%.bh_pf_names(owner)
    v<-if(present).subset2(owner,field)else NULL
    type<-if(present)typeof(v)else"absent"
    if(present&&!is.null(v)&&!typeof(v)%in%c("closure","builtin","special"))
      .bh_pf_fail(paste0("spec.models.",block_name,".model.",slots[[i]]),"callback slot is neither NULL nor a function")
    markers[[i]]<-list(slot=paste0("spec.models.",block_name,".model.",slots[[i]]),
      presence=if(!present)"absent"else if(is.null(v))"NULL"else"function",
      original_type=type,disposition="execution_only_omitted_not_semantically_replayed_or_equated")
    if(present&&!is.null(v)) {
      if(method)methods[field]<-list(NULL)else values[field]<-list(NULL)
    }
  }
  if("methods"%in%names(values))values["methods"]<-list(methods)
  attributes(values)<-attributes(model)
  list(model=values,markers=markers)
}
.bh_pf_model <- function(x,path) {
  required<-c("name","version","provenance","data_schema","parameters","parameter_dependencies","likelihood",
    "capabilities","check_statistics","backend","constants")
  allowed<-c(required,"validate_data","prepare_data","methods","adapter","required_units","informative_rows","reference_parameter_hash","limitations","validate_bindings")
  m<-.bh_pf_record(x,path,allowed,required,classes="observation_model")
  for(n in c("name","version","backend")) .bh_pf_string(m[[n]],paste0(path,".",n))
  # Explicit inert provenance for the pinned revised EZ producer; legacy records
  # remain byte-identical. These fields grant no execution capabilities.
  extra<-c("contract_commit","contract_url","derivation","diffusion_sd","relative_start")
  p<-.bh_pf_record(m$provenance,paste0(path,".provenance"),c("source","license",extra),"license")
  for(n in setdiff(names(p),c("diffusion_sd","relative_start"))) .bh_pf_string(p[[n]],paste0(path,".provenance.",n))
  if(any(extra%in%names(p))) {
    if(!all(extra%in%names(p))||!identical(m$name,"ezdm_revised")||!identical(m$version,"revised_54a07b6_v1"))
      .bh_pf_fail(path,"extended model provenance requires the complete pinned revised EZ profile")
    for(n in c("diffusion_sd","relative_start")) .bh_pf_number(p[[n]],paste0(path,".provenance.",n))
    if(!identical(p$contract_commit,"54a07b6fa34506bd14116d295eea9db234071f28")||
       !identical(p$contract_url,"https://github.com/popov-lab/bmm/tree/54a07b6fa34506bd14116d295eea9db234071f28")||
       p$diffusion_sd!=1||p$relative_start!=0.5)
      .bh_pf_fail(path,"extended provenance contradicts the pinned revised EZ profile")
  }
  fields<-.bh_pf_named(m$data_schema,paste0(path,".data_schema"),FALSE)
  pars<-.bh_pf_named(m$parameters,paste0(path,".parameters"),FALSE)
  constants<-.bh_pf_named(m$constants,paste0(path,".constants"))
  keys<-c(names(fields),names(pars),names(constants))
  if(base::anyDuplicated.default(keys)||any(!grepl("^[A-Za-z][A-Za-z0-9_]*$",keys))) .bh_pf_fail(path,"schema, parameter, and constant identifiers must be disjoint")
  for(n in names(fields)) .bh_pf_data_field(.subset2(fields,n),paste0(path,".data_schema.",n))
  for(n in names(pars)) .bh_pf_parameter(.subset2(pars,n),paste0(path,".parameters.",n))
  for(n in names(constants)) .bh_pf_constant(.subset2(constants,n),paste0(path,".constants.",n))
  if(!any(vapply(fields,function(v)identical(.bh_pf_get(v,"role"),"response"),logical(1)))) .bh_pf_fail(path,"model has no declared response field")
  .bh_pf_density(m$likelihood,paste0(path,".likelihood"))
  .bh_pf_character(m$parameter_dependencies,paste0(path,".parameter_dependencies"),TRUE,TRUE)
  if(!.bh_pf_setequal(m$parameter_dependencies,names(pars))||any(!names(pars)%in%m$likelihood$arguments))
    .bh_pf_fail(path,"declared parameter consumption is incomplete")
  .bh_pf_argument_types(m$likelihood$arguments,m$likelihood$argument_types,m,paste0(path,".likelihood"))
  cap<-.bh_pf_table(m$capabilities,paste0(path,".capabilities"),c("capability","available","status"))
  .bh_pf_character(cap$capability,paste0(path,".capabilities.capability"),TRUE,TRUE)
  if(!.bh_pf_setequal(cap$capability,c("fit","simulate","posterior_predict","posterior_epred","log_lik","reduce_sum")) ||
     !.bh_pf_plain(cap$available,"logical")||anyNA(cap$available)||
     !.bh_pf_plain(cap$status,"character")||anyNA(cap$status)||any(!cap$status%in%c("unvalidated","smoke_checked","validated","unsupported"))||
     any(!cap$available & cap$status!="unsupported")||any(cap$available & cap$status=="unsupported")) .bh_pf_fail(path,"malformed historical capability table")
  .bh_pf_character(m$check_statistics,paste0(path,".check_statistics"))
  if("required_units"%in%names(m)) .bh_pf_character(m$required_units,paste0(path,".required_units"),TRUE,TRUE)
  if("reference_parameter_hash"%in%names(m)) .bh_pf_md5(m$reference_parameter_hash,paste0(path,".reference_parameter_hash"))
  if("limitations"%in%names(m)) .bh_pf_character(m$limitations,paste0(path,".limitations"))
  if("adapter"%in%names(m) && !is.null(m$adapter)) {
    if(!identical(m$backend,"registered_stan")) .bh_pf_fail(path,"typed adapter has an incompatible saved backend label")
    .bh_pf_adapter(m$adapter,m,paste0(path,".adapter"))
  } else {
    # Typed builtin admission uses saved descriptors, never a model-name factory.
    expected<-switch(m$likelihood$symbol,
      normal_lpdf=list(arguments=c("y","mu","sigma"),types=c("real","real","real"),fields="y",pars=c("mu","sigma")),
      binomial_logit_lpmf=list(arguments=c("successes","trials","p"),types=c("int","int","real"),fields=c("successes","trials"),pars="p"),
      lognormal_lpdf=list(arguments=c("y","meanlog","sdlog"),types=c("real","real","real"),fields="y",pars=c("meanlog","sdlog")),NULL)
    if(is.null(expected)||!isTRUE(m$likelihood$builtin)||length(constants)||
       !identical(names(fields),expected$fields)||!identical(names(pars),expected$pars)||
       !identical(m$likelihood$arguments,expected$arguments)||!identical(m$likelihood$argument_types,expected$types))
      .bh_pf_fail(path,"unregistered model lacks a supported complete builtin descriptor")
    .bh_pf_builtin_descriptor(m,path)
  }
  invisible(TRUE)
}

.bh_pf_builtin_descriptor <- function(m,path) {
  symbol<-m$likelihood$symbol
  family<-if(symbol=="normal_lpdf")"gaussian"else if(symbol=="binomial_logit_lpmf")"binomial"else"lognormal"
  expected_backend<-if(family=="gaussian")"gaussian_cfa"else"basic_cfa"
  if(!identical(m$backend,expected_backend)||!is.null(m$likelihood$source)||!is.null(m$likelihood$source_file)||
     !is.null(m$likelihood$resource_hash)||!identical(m$likelihood$approximation,"none")||
     !identical(m$likelihood$exports,symbol)||length(m$likelihood$dependencies)||!identical(m$likelihood$license,"Stan Math"))
    .bh_pf_fail(path,"unregistered builtin differs from the saved canonical density descriptor")
  for(n in names(m$data_schema)) {
    f<-unclass(.bh_pf_get(m$data_schema,n))
    type<-if(family=="gaussian")"real"else if(family=="binomial")"nonnegative_integer"else"positive_real"
    role<-if(family=="binomial"&&n=="trials")"auxiliary"else"response"
    if(!identical(f$type,type)||!identical(f$role,role)||!is.null(f$units)||
       !identical(f$lower,-Inf)||!identical(f$upper,Inf)||!identical(f$width,1L))
      .bh_pf_fail(path,"unregistered builtin field differs from its complete canonical descriptor")
  }
  descriptors<-switch(family,
    gaussian=list(mu=c("real","identity","theta","subject_task","response","response","Conditional response mean"),
      sigma=c("positive","log","theta","task","response","log(response/unit)","Observation-level standard deviation")),
    binomial=list(p=c("unit_interval","logit","eta","subject_task","probability","log_odds","Success probability")),
    lognormal=list(meanlog=c("real","identity","theta","subject_task","log(response/unit)","log(response/unit)","Log-response location"),
      sdlog=c("positive","log","theta","task","log(response/unit)","log(SD of log response)","Log-response standard deviation")))
  fields<-c("support","predictor","receives","levels","units","predictor_units","description")
  for(n in names(descriptors)) {
    p<-.bh_pf_get(m$parameters,n)
    if("role"%in%names(p)) .bh_pf_fail(path,"unregistered builtin has a noncanonical explicit role")
    for(i in seq_along(fields))if(!identical(.bh_pf_get(p,fields[[i]]),descriptors[[n]][[i]]))
      .bh_pf_fail(path,"unregistered builtin parameter differs from its complete canonical descriptor")
  }
  invisible(TRUE)
}

.bh_pf_labels <- function(x,path,missing=FALSE) {
  cls<-.bh_pf_class(x)
  if(identical(cls,"factor")||identical(cls,c("ordered","factor"))) {
    codes<-unclass(x);attributes(codes)<-NULL
    levels<-attr(x,"levels",exact=TRUE)
    if(typeof(codes)!="integer"||!.bh_pf_plain(levels,"character")||anyNA(levels)||base::anyDuplicated.default(levels)||
       any(!is.na(codes)&(codes<1L|codes>length(levels)))) .bh_pf_fail(path,"malformed factor binding")
    out<-base::.subset(levels,codes)
    if(!missing&&anyNA(out)) .bh_pf_fail(path,"binding labels cannot be missing")
    return(out)
  }
  .bh_pf_character(x,path,missing=missing)
  x
}
.bh_pf_bindings <- function(x,model,path) {
  v<-.bh_pf_record(x,path,c("domains","bindings","version"),classes="measurement_parameter_bindings")
  if(!identical(v$version,"per_observation_argument_v1")) .bh_pf_fail(path,"unsupported parameter-binding version")
  d<-.bh_pf_table(v$domains,paste0(path,".domains"),c("parameter","task"))
  fields<-c("observation_id","route","argument","kind","target_parameter","target_task","fixed_value","fixed_scale")
  b<-.bh_pf_table(v$bindings,paste0(path,".bindings"),fields)
  for(n in c("parameter","task")) .bh_pf_character(d[[n]],paste0(path,".domains.",n),TRUE)
  if(any(!d$parameter%in%names(model$parameters))||base::anyDuplicated.default(paste(d$parameter,d$task,sep="::")))
    .bh_pf_fail(path,"binding domain contains unknown or repeated target keys")
  labels<-lapply(.bh_pf_setdiff(fields,"fixed_value"),function(n).bh_pf_labels(b[[n]],paste0(path,".bindings.",n),missing=n%in%c("target_parameter","target_task","fixed_scale")))
  names(labels)<-.bh_pf_setdiff(fields,"fixed_value")
  kind<-labels$kind;target<-kind=="target"
  if(any(!kind%in%c("target","fixed"))||base::anyDuplicated.default(paste(labels$observation_id,labels$route,labels$argument,sep="::")))
    .bh_pf_fail(path,"binding kinds or row/route/formal keys are malformed")
  fixed<-b$fixed_value
  if(!.bh_pf_plain(fixed,c("double","integer"))) .bh_pf_fail(path,"binding fixed values require numeric storage")
  if(any(target)) {
    keys<-paste(labels$target_parameter[target],labels$target_task[target],sep="::")
    if(anyNA(labels$target_parameter[target])||anyNA(labels$target_task[target])||
       any(!keys%in%paste(d$parameter,d$task,sep="::"))||any(!is.na(fixed[target]))||any(!is.na(labels$fixed_scale[target])))
      .bh_pf_fail(path,"target bindings disagree with domains or carry fixed operands")
  }
  if(any(!target) && (any(!is.finite(fixed[!target]))||any(!labels$fixed_scale[!target]%in%c("identity","log","logit"))||
      any(!is.na(labels$target_parameter[!target]))||any(!is.na(labels$target_task[!target]))))
    .bh_pf_fail(path,"fixed binding operands or predictor scales are malformed")
  invisible(TRUE)
}
.bh_pf_levels <- function(x,model,path) {
  v<-.bh_pf_record(x,path,c("choices","version","fingerprint"))
  if(!identical(v$version,"block-parameter-levels-v1")) .bh_pf_fail(path,"unsupported parameter-level schema")
  .bh_pf_md5(v$fingerprint,paste0(path,".fingerprint"))
  choices<-.bh_pf_named(v$choices,paste0(path,".choices"),FALSE)
  if(any(!names(choices)%in%names(model$parameters))) .bh_pf_fail(path,"parameter-level choice references an absent parameter")
  for(n in names(choices)) {
    y<-.subset2(choices,n);level<-.bh_pf_get(y,"level")
    .bh_pf_string(level,paste0(path,".choices.",n,".level"),c("subject_task","task","fixed"))
    fields<-if(level=="fixed")c("level","value","scale")else"level"
    y<-.bh_pf_record(y,paste0(path,".choices.",n),fields)
    if(level=="fixed") {
      z<-y$value;labels<-.bh_pf_names(z)
      if(!typeof(z)%in%c("double","integer")||is.object(z)||!length(z)||
         any(!names(attributes(z))%in%"names")||any(!is.finite(z))||
         (is.null(labels)&&length(z)!=1L)) .bh_pf_fail(path,"malformed captured fixed parameter operand")
      if(!is.null(labels)) .bh_pf_character(labels,paste0(path,".fixed_names"),TRUE,TRUE)
      .bh_pf_string(y$scale,paste0(path,".fixed_scale"),c("natural","predictor","received"))
    }
  }
  invisible(TRUE)
}
.bh_pf_observation_data <- function(x,model,path,mapped=FALSE) {
  keys<-c(".local_id",".task",".observation_id",names(model$data_schema),
          if(mapped)c(".person_key",".block",".person_index"))
  d<-.bh_pf_table(x,path,keys)
  for(n in c(".local_id",".task",".observation_id",if(mapped)c(".person_key",".block")))
    .bh_pf_character(d[[n]],paste0(path,".",n),TRUE)
  if(base::anyDuplicated.default(d$.observation_id)) .bh_pf_fail(path,"saved observation identifiers repeat")
  rows<-.bh_pf_table_shape(x,path)
  for(n in names(model$data_schema)) {
    f<-.bh_pf_get(model$data_schema,n);v<-.subset2(d,n);dims<-attr(v,"dim",exact=TRUE)
    # No callbacks or field-value scientific checks are rerun. This establishes
    # that the saved typed descriptor can be read without a competing shape.
    field_class<-.bh_pf_class(v);width<-.bh_pf_get(f,"width")
    class_ok<-is.null(field_class)||(width>1L&&identical(field_class,"AsIs")&&length(dims)==2L)
    if(!typeof(v)%in%c("integer","double")||!class_ok||
       (.bh_pf_get(f,"width")==1L&&!is.null(dims))||
       (.bh_pf_get(f,"width")>1L&&(!identical(as.double(dims),c(rows,as.double(.bh_pf_get(f,"width")))))))
      .bh_pf_fail(path,"saved observation storage disagrees with the declared field type/width")
  }
  if(mapped && (!.bh_pf_plain(d$.person_index,c("integer","double"))||anyNA(d$.person_index)||
     any(!is.finite(d$.person_index)|d$.person_index<1|d$.person_index!=floor(d$.person_index))))
    .bh_pf_fail(path,"invalid saved person indices")
  invisible(TRUE)
}
.bh_pf_block <- function(x,path) {
  fields<-c("model","data","units","mapping","subject_column","task_column","observation_key_origin","source_id","parameter_bindings","transformations","data_diagnostics")
  b<-.bh_pf_record(x,path,c(fields,"parameter_levels"),fields,classes="observation_block")
  .bh_pf_model(b$model,paste0(path,".model"));m<-unclass(b$model)
  .bh_pf_observation_data(b$data,m,paste0(path,".data"))
  for(n in c("units","subject_column","task_column","observation_key_origin","transformations")) .bh_pf_string(b[[n]],paste0(path,".",n))
  if(!identical(b$transformations,"none; no automatic standardization")) .bh_pf_fail(path,"unsupported observation transformation descriptor")
  if(!is.null(b$source_id)) .bh_pf_string(b$source_id,paste0(path,".source_id"))
  mapping<-.bh_pf_named(b$mapping,paste0(path,".mapping"),FALSE)
  if(!.bh_pf_setequal(names(mapping),names(m$data_schema))) .bh_pf_fail(path,"field mapping does not cover saved model schema")
  for(n in names(mapping)) .bh_pf_string(.subset2(mapping,n),paste0(path,".mapping.",n))
  diag<-.bh_pf_record(b$data_diagnostics,paste0(path,".data_diagnostics"),"zero_trial_observations")
  .bh_pf_character(diag$zero_trial_observations,paste0(path,".data_diagnostics.zero_trial_observations"),TRUE,TRUE)
  if(!is.null(b$parameter_bindings)) .bh_pf_bindings(b$parameter_bindings,m,paste0(path,".parameter_bindings"))
  if(identical(.bh_pf_get(m$adapter,"mode"),"routed") && is.null(b$parameter_bindings))
    .bh_pf_fail(path,"routed model lacks its explicit saved parameter bindings")
  if("parameter_levels"%in%names(b)) .bh_pf_levels(b$parameter_levels,m,paste0(path,".parameter_levels"))
  invisible(TRUE)
}
.bh_pf_spec <- function(x,policy,detached=FALSE) {
  fields<-c("formula","models","subjects","missing","data_map","parameter_keys","tables","prior","status","backend",
    "response_missing_semantics","observation_key_origin","schema_version","checks","fingerprint")
  s<-.bh_pf_record(x,"spec",c(fields,"schema_migration"),fields,classes=if(detached)"bmeas_portable_context"else c("bmeas_spec","bhmm_spec"))
  .bh_pf_string(s$schema_version,"spec.schema_version",c("reference-law-spec-v1","affine-correlation-spec-v1"))
  .bh_pf_md5(s$fingerprint,"spec.fingerprint")
  for(n in c("status","backend","response_missing_semantics","observation_key_origin")) .bh_pf_string(s[[n]],paste0("spec.",n))
  models<-.bh_pf_named(s$models,"spec.models",FALSE)
  for(n in names(models)) .bh_pf_block(.subset2(models,n),paste0("spec.models.",n))
  subjects<-s$subjects;kind<-.bh_pf_get(subjects,"type")
  .bh_pf_string(kind,"spec.subjects.type",c("shared","crosswalk"))
  su<-.bh_pf_record(subjects,"spec.subjects",if(kind=="shared")c("type","namespace")else c("type","namespace","data"),classes="measurement_subjects")
  .bh_pf_string(su$namespace,"spec.subjects.namespace")
  if(kind=="crosswalk") {
    cw<-.bh_pf_table(su$data,"spec.subjects.data",c("model","local_id","person_id"))
    for(n in names(cw)) .bh_pf_character(cw[[n]],paste0("spec.subjects.data.",n),TRUE)
  }
  missing<-.bh_pf_record(s$missing,"spec.missing",c("subjects","response"))
  if(!identical(missing$subjects,"union")||!missing$response%in%c("error","observed_only")) .bh_pf_fail("spec.missing","unsupported saved missing-response policy")
  dm<-.bh_pf_record(s$data_map,"spec.data_map",c("blocks","subject_keys","coverage","overlap","task_coverage","excluded","missing_assumption"))
  blocks<-.bh_pf_named(dm$blocks,"spec.data_map.blocks",FALSE)
  if(!identical(names(blocks),names(models))) .bh_pf_fail("spec.data_map.blocks","saved consumed block order differs from declared blocks")
  .bh_pf_character(dm$subject_keys,"spec.data_map.subject_keys",TRUE,TRUE)
  if(!length(dm$subject_keys)) .bh_pf_fail("spec.data_map.subject_keys","empty consumed roster")
  for(n in names(blocks)) .bh_pf_observation_data(.subset2(blocks,n),unclass(.bh_pf_get(.subset2(models,n),"model")),paste0("spec.data_map.blocks.",n),TRUE)
  if(!identical(as.double(attr(dm$coverage,"dim",exact=TRUE)),c(as.double(length(dm$subject_keys)),as.double(length(blocks))))||typeof(dm$coverage)!="logical"||anyNA(dm$coverage))
    .bh_pf_fail("spec.data_map.coverage","coverage dimensions or values are malformed")
  if(!identical(as.double(attr(dm$overlap,"dim",exact=TRUE)),rep(as.double(length(blocks)),2L))||!typeof(dm$overlap)%in%c("integer","double"))
    .bh_pf_fail("spec.data_map.overlap","overlap shape is malformed")
  .bh_pf_table(dm$task_coverage,"spec.data_map.task_coverage",c(".person_key",".block",".task"))
  .bh_pf_table(dm$excluded,"spec.data_map.excluded",c(".block",".observation_id",".person_key"))
  .bh_pf_string(dm$missing_assumption,"spec.data_map.missing_assumption")
  .bh_portable_scientific_schema(s$formula,"spec.formula")
  .bh_portable_scientific_schema(s$prior,"spec.prior")
  .bh_portable_scientific_schema(s$parameter_keys,"spec.parameter_keys")
  if("schema_migration"%in%names(s)) .bh_portable_scientific_schema(s$schema_migration,"spec.schema_migration")
  .bh_portable_scientific_schema(s$tables,"spec.tables")
  .bh_pf_raw_declaration_binding(s$formula,s$tables)
  .bh_pf_history_schema(s$checks,"spec.checks")
  invisible(TRUE)
}

# A directory is the exclusive creation primitive. No pre-existing candidate is
# removed, and the registered file lives only inside a successfully owned root.
.bh_pf_owned_hash <- function(writer, expected_bytes = NULL) {
  state<-new.env(parent=emptyenv());state$directory<-NULL;state$file<-NULL;state$connection<-NULL
  cleanup<-function(strict=FALSE) {
    failed<-FALSE
    base::suspendInterrupts({
      if(!is.null(state$connection)) {
        ok<-tryCatch({base::close.connection(state$connection);TRUE},error=function(e)FALSE)
        if(!ok)failed<-TRUE
        state$connection<-NULL
      }
      if(!is.null(state$file)) {
        ok<-tryCatch(unlink(state$file,recursive=FALSE,force=TRUE)==0L,error=function(e)FALSE)
        if(!ok||file.exists(state$file))failed<-TRUE else state$file<-NULL
      }
      if(!is.null(state$directory)) {
        # Refuse to recursively delete unexpected contents, even in our root.
        contents<-tryCatch(list.files(state$directory,all.files=TRUE,no..=TRUE),error=function(e)NA_character_)
        if(length(contents))failed<-TRUE else {
          ok<-tryCatch(unlink(state$directory,recursive=TRUE,force=TRUE)==0L,error=function(e)FALSE)
          if(!ok||dir.exists(state$directory))failed<-TRUE else state$directory<-NULL
        }
      }
    })
    if(strict&&failed) .bh_pf_fail("export transaction","could not remove every owned temporary object","E_PORTABLE_CLEANUP")
    invisible(!failed)
  }
  on.exit(tryCatch(cleanup(FALSE),error=function(e)invisible(NULL)),add=TRUE)
  base::suspendInterrupts({
    candidate<-tempfile("bmeas-portable-")
    created<-suppressWarnings(dir.create(candidate,recursive=FALSE,showWarnings=FALSE))
    if(!isTRUE(created)) .bh_pf_fail("export transaction","exclusive temporary directory creation failed; existing paths were preserved","E_PORTABLE_TEMPFILE")
    state$directory<-candidate
    candidate_file<-file.path(candidate,"context.bin")
    link_target<-Sys.readlink(candidate_file)
    if(file.exists(candidate_file)||(!is.na(link_target)&&nzchar(link_target)))
      .bh_pf_fail("export transaction","unexpected pre-existing hash path was preserved","E_PORTABLE_TEMPFILE")
    created_file<-suppressWarnings(file.create(candidate_file,showWarnings=FALSE))
    if(!isTRUE(created_file)) .bh_pf_fail("export transaction","owned hash-file creation failed","E_PORTABLE_TEMPFILE")
    state$file<-candidate_file
  })
  # writer is package code, never a value obtained from the user's saved object.
  counted<-writer(state)
  if(!is.null(state$connection)) base::suspendInterrupts({
    base::close.connection(state$connection);state$connection<-NULL
  })
  info<-unclass(file.info(state$file))
  if(length(info$size)!=1L||is.na(info$isdir[[1L]])||info$isdir[[1L]]||is.na(info$size[[1L]])||
     !is.finite(info$size[[1L]])||info$size[[1L]]<0||info$size[[1L]]!=floor(info$size[[1L]]))
    .bh_pf_fail("export transaction","closed owned hash file is not a measured regular file","E_PORTABLE_WRITE")
  actual<-as.double(info$size[[1L]])
  expected<-if(is.null(expected_bytes)&&is.list(counted)) .subset2(counted,"bytes") else expected_bytes
  if(!is.null(expected)&&!identical(actual,as.double(expected)))
    .bh_pf_fail("export transaction","closed hash-file bytes disagree with canonical accounting","E_PORTABLE_WRITE")
  digest<-unname(tools::md5sum(state$file))
  .bh_pf_md5(digest,"export transaction digest")
  cleanup(TRUE)
  list(digest=digest,bytes=actual,counts=counted)
}
.bh_pf_verify_original <- function(spec,provenance) {
  saved<-.bh_pf_get(spec,"fingerprint");.bh_pf_md5(saved,"spec.fingerprint")
  bound<-.bh_pf_get(provenance,"model_fingerprint");.bh_pf_md5(bound,"provenance.model_fingerprint")
  if(!identical(saved,bound)) .bh_pf_fail("fit/spec binding","saved model fingerprint differs from original specification seal","E_PORTABLE_BINDING")
  original<-unclass(spec);original["fingerprint"]<-NULL
  original_attributes<-attributes(spec);original_attributes$names<-names(original)
  attributes(original)<-original_attributes
  # Exact inherited .mp_hash byte protocol: pathname, default compression,
  # version=2, and original root class. The entire fit/backend is never saved.
  verified<-.bh_pf_owned_hash(function(state) {
    saveRDS(original,file=state$file,version=2)
    NULL
  })
  if(!identical(saved,verified$digest)) .bh_pf_fail("spec.fingerprint",
    "original serialization seal did not match in this producer/runtime; no reset, migration or reseal was attempted","E_PORTABLE_ORIGINAL_SEAL")
  saved
}
.bh_pf_nested_seals <- function(spec) {
  out<-list()
  add<-function(owner,field,path,required=FALSE) {
    if(!field%in%.bh_pf_names(owner)) {
      if(required) .bh_pf_fail(path,"required original seal is absent")
      return(invisible(NULL))
    }
    value<-.bh_pf_get(owner,field);.bh_pf_md5(value,path)
    out[[length(out)+1L]]<<-list(path=path,value=value,role="original_legacy_seal_ancestry_not_recomputed")
    invisible(NULL)
  }
  migration<-function(x,path) {
    if(is.null(x))return(invisible(NULL))
    add(x,"source_fingerprint",paste0(path,".source_fingerprint"),TRUE)
    if(identical(.bh_pf_get(x,"kind"),"prepared_context"))
      add(x,"source_formula_fingerprint",paste0(path,".source_formula_fingerprint"),TRUE)
    invisible(NULL)
  }
  add(spec,"fingerprint","spec.fingerprint",TRUE)
  formula<-.bh_pf_get(spec,"formula")
  add(formula,"fingerprint","spec.formula.fingerprint",TRUE)
  migration(.bh_pf_get(formula,"schema_migration"),"spec.formula.schema_migration")
  migration(.bh_pf_get(spec,"schema_migration"),"spec.schema_migration")
  components<-.bh_pf_get(formula,"components")
  for(n in .bh_pf_names(components)) {
    component<-.bh_pf_get(components,n)
    if(identical(.bh_pf_class(component),"bhmm_basis_component"))
      add(component,"fingerprint",paste0("spec.formula.components.",n,".fingerprint"),TRUE)
  }
  prior<-.bh_pf_get(spec,"prior")
  if(!is.null(prior))for(i in seq_along(unclass(prior))) {
    rule<-.subset2(unclass(prior),i);target<-.bh_pf_get(rule,"target")
    if(identical(.bh_pf_class(target),"bhmm_prior_targets"))
      add(target,"fingerprint",paste0("spec.prior.[",i,"].target.fingerprint"),TRUE)
  }
  models<-.bh_pf_get(spec,"models")
  for(n in .bh_pf_names(models)) {
    block<-.bh_pf_get(models,n);model<-.bh_pf_get(block,"model");base<-paste0("spec.models.",n)
    level<-.bh_pf_get(block,"parameter_levels")
    if(!is.null(level))add(level,"fingerprint",paste0(base,".parameter_levels.fingerprint"),TRUE)
    add(model,"reference_parameter_hash",paste0(base,".model.reference_parameter_hash"))
    adapter<-.bh_pf_get(model,"adapter")
    if(!is.null(adapter))add(adapter,"wrapper_hash",paste0(base,".model.adapter.wrapper_hash"),TRUE)
  }
  out
}

.bh_pf_project_spec <- function(spec) {
  s<-unclass(spec);models<-.bh_pf_named(s$models,"spec.models",FALSE);markers<-list()
  for(n in names(models)) {
    block<-.subset2(models,n)
    fields<-c("model","data","units","mapping","subject_column","task_column","observation_key_origin","source_id","parameter_bindings","transformations","data_diagnostics")
    b<-.bh_pf_record(block,paste0("spec.models.",n),c(fields,"parameter_levels"),fields,classes="observation_block")
    result<-.bh_pf_callbacks(b$model,n)
    b["model"]<-list(result$model);attributes(b)<-attributes(block)
    models[n]<-list(b)
    markers<-c(markers,result$markers)
  }
  s["models"]<-list(models);attributes(s)<-attributes(spec)
  list(spec=s,callbacks=markers)
}
.bh_pf_protected_cache <- function(cache) {
  # Retain all historical computation/scientific/execution/inference records.
  # Mutable posterior arrays stay in cache; their historical warning envelope
  # is protected separately without introducing another posterior-value cache.
  history<-.bh_pf_take(cache,c("hmc","computation","numerical_evaluations","admissibility",
    "posterior_propriety","posterior_normalizability","gaussian_diagnostics","execution_profile",
    "backend_qualification","identification","inference","predictive_fit","measurement_structure_fit","initialization","posterior_reporting"))
  optional<-.bh_pf_get(cache,"optional_reports")
  if(!is.null(optional)) {
    history["optional_report_history"]<-list(.bh_pf_take(optional,
      c("status","reason","unavailable_values","entries","log_metadata_semantics","scope","warning","warnings")))
    logs<-.bh_pf_get(optional,"log_magnitudes")
    if(typeof(logs)=="list") history["optional_log_history"]<-list(logs)
  }
  expanded<-.bh_pf_get(cache,"expanded_correlation_matrices")
  if(!is.null(expanded))history["expanded_report_history"]<-list(.bh_pf_take(expanded,c("status","reason","scope")))
  ppc<-.bh_pf_get(cache,"ppc");metadata<-.bh_pf_get(ppc,"metadata")
  if(!is.null(metadata))history["predictive_attempt_history"]<-list(.bh_pf_take(metadata,
    c("version","level","law","requested_replications","completed_replications","failure","replicate_statistics_status","screening_only","source_origin_scope","interpretation")))
  history
}
.bh_pf_receipt <- function(locale) list(
  schema_version="bmeas_portable_export_receipt_v1",status="produced",original_seal="matched",fit_spec_binding="matched",
  package_call_scope="no_fitting_generation_model_backend_or_posterior_source_calls",
  callback_disposition="execution_slots_omitted_not_replayed_or_equated",
  legacy_verification_cost_scope="original_spec_serialization_separate_unknown_cost",
  native_locale_at_export=locale,canonical_payload_bytes=0.0,canonical_payload_nodes=0.0,canonical_payload_depth=0.0,
  immutable_bytes=0.0,immutable_nodes=0.0,immutable_depth=0.0,preflight_count_passes=1.0,detached_validation_passes=1.0,
  immutable_encoding_passes=1.0,hash_bytes_written=0.0,hash_read_bytes_reserved=0.0,
  hash_file_cleanup="owned_file_removed_before_return")
.bh_pf_capabilities <- function() list(
  schema_version="bmeas_portable_capabilities_v1",profile="retained_declarative_postprocessing_only",
  metadata_interfaces=c("diagnostic_parameters","ppc_plan"),
  passive_interfaces=c("summary","fit_checks","parameter_summary","posterior_summary","print"),
  active_interfaces=c("refresh_diagnostics","individual_parameter_summary","assess_measurement_fit"),
  interface_qualification="existing_consumer_scientific_domains_and_operation_specific_budgets_remain_required",
  population_diagnostics="existing_primitive_derived_monitor_domains_and_exact_alias_interpretation",
  individual_summaries="existing_predictor_roles_scales_fixed_shared_person_and_state_selection_rules",
  basic_ppc="exact_canonical_gaussian_binomial_logit_lognormal_descriptors_and_package_owned_kernels_only",
  unavailable=c("fitting","source_generation","data_regeneration","model_rebuilding","saved_model_methods",
    "callback_simulation","generic_check_model_reevaluation","source_path_rebinding"),
  dispatch_status="stage1_export_only_consumers_closed")

portable_fit <- function(fit,budget) {
  policy<-.bh_pf_policy(budget)
  locale<-Sys.getlocale("LC_CTYPE")
  .bh_portable_codec_capabilities(locale)
  # Finalize this generated text before the complete self-inclusive count.
  locale_value<-.bh_portable_codec_text(locale,locale)$value
  root<-.bh_pf_record(fit,"fit",c("spec","run","backend","cache","code","execution_receipt","resource_outcome","provenance"),
    c("spec","run","cache","code","provenance"),classes=c("bmeas_fit","bhmm_fit"))
  spec<-.bh_pf_get(root,"spec")
  spec_fields<-c("formula","models","subjects","missing","data_map","parameter_keys","tables","prior","status","backend",
    "response_missing_semantics","observation_key_origin","schema_version","schema_migration","checks","fingerprint")
  .bh_pf_record(spec,"spec",spec_fields,.bh_pf_setdiff(spec_fields,"schema_migration"),classes=c("bmeas_spec","bhmm_spec"))
  provenance<-.bh_pf_get(root,"provenance")
  .bh_pf_history_schema(provenance,"provenance")
  original_seal<-.bh_pf_verify_original(spec,provenance)
  projection<-.bh_pf_project_spec(spec)
  projected_spec<-projection$spec
  # No traversal of root$backend occurs. Only the fixed root field name is observed.
  .bh_pf_plain_tree(projected_spec,c("spec"),limit=policy$max_depth)
  .bh_pf_spec(projected_spec,policy)
  cache<-.bh_pf_get(root,"cache");run<-.bh_pf_get(root,"run");code<-.bh_pf_get(root,"code")
  .bh_pf_history_schema(cache,"cache",scientific_tables=projected_spec$tables);.bh_pf_history_schema(run,"run")
  .bh_pf_product_profile(projected_spec,cache,provenance)
  profile<-.bh_pf_joint_export_profile(projected_spec)
  .bh_pf_character(code,"code")
  execution<-.bh_pf_get(root,"execution_receipt");outcome<-.bh_pf_get(root,"resource_outcome")
  if("execution_receipt"%in%names(root)) .bh_pf_history_schema(execution,"execution_receipt")
  if("resource_outcome"%in%names(root)) .bh_pf_history_schema(outcome,"resource_outcome")
  times<-.bh_pf_timestamps(provenance,execution);provenance<-times$provenance;execution<-times$execution_receipt
  for(n in c("cache","run","provenance","execution_receipt","resource_outcome")) {
    value<-switch(n,cache=cache,run=run,provenance=provenance,execution_receipt=execution,resource_outcome=outcome)
    .bh_pf_plain_tree(value,n,limit=policy$max_depth)
  }
  paths<-.bh_pf_get(provenance,"output_files")
  if(!is.null(paths)) .bh_pf_character(paths,"provenance.output_files",TRUE,TRUE)
  output<-.bh_pf_get(execution,"output")
  receipt_paths<-.bh_pf_get(output,"paths")
  if(!is.null(paths)&&!is.null(receipt_paths)&&!identical(paths,receipt_paths))
    .bh_pf_fail("source binding","provenance and execution receipt record different ordered output paths","E_PORTABLE_BINDING")
  nested_seals<-.bh_pf_nested_seals(projected_spec)
  scientific<-unclass(projected_spec);attr(scientific,"class")<-"bmeas_portable_context"
  resources<-list()
  scientific_models<-.bh_pf_get(projected_spec,"models")
  for(n in names(scientific_models)) {
    model<-.bh_pf_get(.bh_pf_get(scientific_models,n),"model")
    adapter<-.bh_pf_get(model,"adapter")
    if(!is.null(adapter))resources[n]<-list(.bh_pf_take(adapter,c("resources","function_code","function_exports","wrapper_hash")))
  }
  artifacts<-list(saved_source_text=code,source_text_scope="retained_text_export_time_integrity_not_original_compiler_file_authentication",
    resource_descriptors=resources,run=run,
    original_execution_identities=.bh_pf_take(provenance,c("compile_key","cache_hit","executable_sha256","toolchain","initialization","plan_identity","provider","provider_metadata")))
  source<-list(original_paths=.bh_pf_take(provenance,"output_files"),
    configured_chain_context=.bh_pf_take(run,c("chains","iter_warmup","iter_sampling","seed")),
    execution_output=output,
    origin_evidence="original_file_hash_evidence_retained_where_present_no_new_source_authentication_or_rebinding")
  ancestry<-list(original_fit_class=.bh_pf_class(fit),original_spec_class=.bh_pf_class(spec),
    original_spec_fingerprint=original_seal,original_fit_model_fingerprint=.bh_pf_get(provenance,"model_fingerprint"),
    legacy_seals=nested_seals,conversion_profile=.bh_pf_conversion_profile(profile),callback_disposition=projection$callbacks,
    backend_disposition=list(slot="fit.backend",presence=if("backend"%in%names(root))"present"else"absent",
      disposition="omitted_wholesale_without_traversal_or_copy"),
    evidence_scope="original_spec_seal_and_fit_fingerprint_association_do_not_authenticate_other_historically_unsealed_fit_fields")
  history<-list(original_scientific_checks=.bh_pf_get(projected_spec,"checks"),original_provenance=provenance,
    original_fit_history=.bh_pf_protected_cache(cache),retained_cache_scope="copied_existing_evidence_may_include_posterior_valued_arrays_no_new_acquisition")
  if("execution_receipt"%in%names(root))history["original_execution_receipt"]<-list(execution)
  if("resource_outcome"%in%names(root))history["original_resource_outcome"]<-list(outcome)
  immutable<-list(context_profile=profile,scientific_context=scientific,
    retained_artifacts=artifacts,source_binding=source,ancestry=ancestry,protected_evidence=history,
    validation_policy=policy,capabilities=.bh_pf_profile_capabilities(profile))
  planned<-structure(list(schema_version="bmeas_portable_fit_v1",immutable=immutable,cache=cache,
    identity=list(format="bmeas_portable_context_bytes_v1",algorithm="md5",provider="tools::md5sum",
      digest="00000000000000000000000000000000"),export_receipt=.bh_pf_receipt(locale_value)),class="bmeas_portable_fit")
  # This is the single complete planned-output count. Expanded detached atomic
  # storage is allocated only after it succeeds; repeated occurrences all count.
  admitted<-.bh_portable_codec_count(planned,policy,locale)
  detached<-.bh_portable_codec_copy(planned,policy,locale)
  detached_class<-.bh_pf_class(detached);detached<-unclass(detached)
  hashed<-.bh_pf_owned_hash(function(state) {
    base::suspendInterrupts({state$connection<-file(state$file,open="wb")})
    .bh_portable_codec_write(.bh_pf_get(detached,"immutable"),state$connection,policy,locale)
  })
  detached$identity$digest<-hashed$digest
  receipt<-detached$export_receipt
  receipt$canonical_payload_bytes<-admitted$bytes;receipt$canonical_payload_nodes<-admitted$nodes;receipt$canonical_payload_depth<-admitted$depth
  receipt$immutable_bytes<-hashed$counts$bytes;receipt$immutable_nodes<-hashed$counts$nodes;receipt$immutable_depth<-hashed$counts$depth
  receipt$hash_bytes_written<-hashed$bytes;receipt$hash_read_bytes_reserved<-hashed$counts$bytes
  detached$export_receipt<-receipt
  attr(detached,"class")<-detached_class
  .bh_portable_codec_locale(locale)
  final<-.bh_portable_codec_count(detached,policy,locale)
  if(!identical(final,admitted)) .bh_pf_fail("export receipt","complete detached byte/node/depth counts changed after admission","E_PORTABLE_ACCOUNTING")
  detached
}

# Historical evidence validators. These inspect saved shapes and availability
# only: no scientific, numerical, source, or provider result is recomputed.
.bh_pf_history_status <- function(x,path,allowed=c("status","reason"),required="status") {
  v<-.bh_pf_record(x,path,allowed,required)
  .bh_pf_string(v$status,paste0(path,".status"))
  if("reason"%in%names(v)) .bh_pf_string(v$reason,paste0(path,".reason"),empty=TRUE)
  invisible(v)
}
.bh_pf_open_metadata <- function(x,path) {
  # Explicitly open producer metadata (e.g. tool options, retained metric
  # provenance) is inert, not a new scientific declaration or capability.
  if(isS4(x)||!typeof(x)%in%c("NULL","list","logical","integer","double","character","raw"))
    .bh_pf_fail(path,"unsupported historical metadata value")
  if(typeof(x)=="list") {
    values<-unclass(x);n<-.bh_pf_names(values)
    if(!is.null(n)&&(!.bh_pf_plain(n,"character")||anyNA(n)||any(!nzchar(n))||base::anyDuplicated.default(n)))
      .bh_pf_fail(path,"ambiguous historical metadata field names")
    for(i in seq_along(values)) .bh_pf_open_metadata(.subset2(values,i),paste0(path,"[",i,"]"))
  }
  invisible(TRUE)
}
.bh_pf_nullable_numeric <- function(x,path,vector=FALSE) {
  if(!typeof(x)%in%c("integer","double")||isS4(x)||is.object(x)||!is.null(attr(x,"dim",exact=TRUE))||
     any(!names(attributes(x))%in%"names")||(!vector&&length(x)!=1L))
    .bh_pf_fail(path,"expected plain retained numeric evidence")
  invisible(TRUE)
}
.bh_pf_evidence <- function(x,path,kind) {
  if(is.null(x))return(invisible(TRUE))
  schemas<-list(
    computation=c("status","reason"), inference=c("status","reason"),
    predictive_fit=c("phase","coverage","outside_intervals","status","reason","level","law","replications","rows","interpretation","screening_only"),
    measurement_structure_fit=c("coverage","outside_intervals","status","reason","level","law","replications","rows","interpretation","screening_only"),
    posterior_propriety=c("status","findings","reason","scope","person_law_certificate","proof"),
    posterior_normalizability=c("findings","proof","person_law_certificate","status","dimension","person_law","reason","normalized_evidence_bound","prior_measure","positive_mass_premise","checked_blocks","normalized_rows","scope","limitations","gaussian_certificate"),
    gaussian_diagnostics=c("status","eligible","positive_factors_certified","all_gaussian","gaussian_blocks","constant_factors","scope","augmentation","qualification","person_law_certificate"),
    identification=c("status","reason","scope","observable_covariance_moments","free_covariance_parameters","covariance_df","moments","identification_claim","correlation_feasibility","probe_ranks","relative_smallest_singular_value","tolerance_ranks","example_local_null_direction","example_probe_parameters","null_direction_scope","diagnostic_error_code","diagnostic_parameter_scales","diagnostic_coordinate_scope","uninformed_parameters","share_nonidentification","basis_nonidentification","marker_nonidentification","loading_permutation_nonidentification","affine_correlation_alias","unseparated_parameter_observation_noise","confounding_scope","gaussian_factor_status","uninformed_targets","uninformed_correlations","uninformed_parameter_correlations","correlation_alias","mean_only_targets","latent_covariance_dimension_bound","latent_covariance_parameters","latent_covariance_excess","global_identification","qualification","constant_factor_scope"),
    initialization=c("status","recipe_version","primitives","primitive_prior_totals","reasons","scope","correlation_initialization","correlation_scientific","joint_correlation_prior_kernel","correlation_chart_log_jacobian","aggregate_kernel_with_correlation_charts","constrained_init_hash","materialization_version","hash_convention","values"),
    execution_profile=c("support","requested","used","exploratory","exploratory_rank_override","qualification","status","reason","scope"),
    backend_qualification=c("support","qualification","status","scope","reason","evidence_version","evidence_scope","additional_evidence","compiler_policy","limitations"),
    admissibility=c("status","reason","draws","scope","threshold","total_predictor_scope"))
  allowed<-.subset2(schemas,kind)
  if(is.null(allowed)) .bh_pf_fail(path,"no supported historical record schema")
  required<-if(kind=="execution_profile")c("requested","used")else"status"
  v<-.bh_pf_record(x,path,allowed,required)
  if("status"%in%names(v)) .bh_pf_string(v$status,paste0(path,".status"))
  for(n in .bh_pf_intersect(c("reason","scope","recipe_version","materialization_version","hash_convention","requested","used"),names(v)))
    .bh_pf_string(v[[n]],paste0(path,".",n),empty=TRUE)
  if("constrained_init_hash"%in%names(v)) .bh_pf_md5(v$constrained_init_hash,paste0(path,".constrained_init_hash"))
  # Recorded witnesses and proof metadata remain historical opaque operands.
  # The allowed enclosing record fields are closed; no scientific checks replay.
  for(n in .bh_pf_setdiff(names(v),c("status","reason","scope","requested","used")))
    .bh_pf_open_metadata(v[[n]],paste0(path,".",n))
  invisible(TRUE)
}
.bh_pf_numerical <- function(x,path) {
  if(is.null(x))return(invisible(TRUE))
  if(!"schema_version"%in%.bh_pf_names(x)) {
    legacy<-.bh_pf_record(x,path,c("observed_failure_messages","messages","count_scope"))
    .bh_pf_number(legacy$observed_failure_messages,paste0(path,".observed_failure_messages"),0,(2^53 - 1),TRUE)
    .bh_pf_character(legacy$messages,paste0(path,".messages"))
    if(!identical(legacy$count_scope,"observed console message lines, not a complete count of failed evaluations; public backend output may omit chains or suppressed messages"))
      .bh_pf_fail(path,"legacy numerical-message provenance does not match its saved producer schema")
    return(invisible(TRUE))
  }
  fields<-c("schema_version","observed_failure_messages","messages","output_capture","recognition_rule","category_counts",
    "category_counts_may_overlap","count_unit","coverage","total_failed_evaluations","count_scope")
  v<-.bh_pf_record(x,path,fields)
  if(!identical(v$schema_version,"bmeas_numerical_messages_v1")||
     !identical(v$recognition_rule,"structural_or_literal_Stan_Exception_v1")||
     !identical(v$count_unit,"captured_text_line_occurrence")||!identical(v$coverage,"incomplete"))
    .bh_pf_fail(path,"unsupported numerical-message provenance schema")
  .bh_pf_history_status(v$output_capture,paste0(path,".output_capture"),c("status","reason"),c("status","reason"))
  .bh_pf_character(v$messages,paste0(path,".messages"),names=TRUE)
  categories<-.bh_pf_record(v$category_counts,paste0(path,".category_counts"),c("structural_numerical","stan_exception"))
  for(n in names(categories)) .bh_pf_nullable_numeric(categories[[n]],paste0(path,".category_counts.",n))
  for(n in c("observed_failure_messages","total_failed_evaluations")) .bh_pf_nullable_numeric(v[[n]],paste0(path,".",n))
  .bh_pf_logical(v$category_counts_may_overlap,paste0(path,".category_counts_may_overlap"))
  .bh_pf_string(v$count_scope,paste0(path,".count_scope"))
  invisible(TRUE)
}
.bh_pf_reporting <- function(x,path) {
  if(is.null(x))return(invisible(TRUE))
  version<-.bh_pf_get(x,"version")
  .bh_pf_string(version,paste0(path,".version"),c("bmeas_posterior_reporting_v1","bmeas_posterior_reporting_v2"))
  if(version=="bmeas_posterior_reporting_v1") {
    fields<-c("version","status","requested_cache","posterior_diagnostics","reason","phases","observed_csv_rows","observed_csv_rows_availability")
    v<-.bh_pf_record(x,path,fields)
    .bh_pf_string(v$status,paste0(path,".status"),c("deferred_by_policy","uncomputed_output_resource"))
    .bh_pf_string(v$requested_cache,paste0(path,".requested_cache"),c("eager","deferred"))
    if(!identical(v$posterior_diagnostics,"uncomputed")||!identical(v$observed_csv_rows_availability,"not_observed")||
       (v$status=="deferred_by_policy"&&v$requested_cache!="deferred")) .bh_pf_fail(path,"uncomputed reporting fields disagree")
    .bh_pf_string(v$reason,paste0(path,".reason"));.bh_pf_nullable_numeric(v$observed_csv_rows,paste0(path,".observed_csv_rows"))
    if(!is.na(v$observed_csv_rows)||is.nan(v$observed_csv_rows)) .bh_pf_fail(path,"uncomputed reporting claims observed rows")
    .bh_pf_character(v$phases,paste0(path,".phases"),names=TRUE)
    phases<-c("parameter_summary","hmc","conditioning","availability_and_covariance","expanded_summary","numerical_messages")
    if(length(v$phases)!=6L||!.bh_pf_setequal(names(v$phases),phases)||any(v$phases!="uncomputed")) .bh_pf_fail(path,"uncomputed phase record is malformed")
    return(invisible(TRUE))
  }
  v<-.bh_pf_record(x,path,c("version","reason","phases","selection","observed","parameter_metrics","hmc_metrics","provenance"))
  .bh_pf_string(v$reason,paste0(path,".reason"))
  selection<-.bh_pf_record(v$selection,paste0(path,".selection"),c("selectable_keys","selected_keys","excluded","scope"))
  for(n in c("selectable_keys","selected_keys")) .bh_pf_character(selection[[n]],paste0(path,".selection.",n),TRUE,TRUE)
  if(any(!selection$selected_keys%in%selection$selectable_keys)) .bh_pf_fail(path,"selected keys are outside saved selectable domain")
  .bh_pf_string(selection$scope,paste0(path,".selection.scope"))
  excluded<-.bh_pf_record(selection$excluded,paste0(path,".selection.excluded"),c("fixed","share_weights","person_and_basis","other_derived"))
  for(n in names(excluded)) {
    ep<-paste0(path,".selection.excluded.",n)
    with_unit<-n%in%c("fixed","share_weights")
    fields<-if(with_unit)c("status","count_unit","known_rows")else c("status","known_rows")
    entry<-.bh_pf_history_status(excluded[[n]],ep,fields,fields)
    if(!identical(entry$status,if(with_unit)"excluded"else"excluded_not_expanded"))
      .bh_pf_fail(ep,"historical exclusion status differs from its producer category")
    if(with_unit) .bh_pf_string(entry$count_unit,paste0(ep,".count_unit"),
      if(n=="fixed")"saved component scale rows"else"saved share-part rows")
    .bh_pf_nullable_numeric(entry$known_rows,paste0(ep,".known_rows"))
    if(is.nan(entry$known_rows)||(!is.na(entry$known_rows)&&(!is.finite(entry$known_rows)||entry$known_rows<0||entry$known_rows!=floor(entry$known_rows))))
      .bh_pf_fail(ep,"malformed historical excluded-row count")
    if((n=="fixed"&&is.na(entry$known_rows))||(!with_unit&&!is.na(entry$known_rows)))
      .bh_pf_fail(ep,"historical excluded-row availability differs from its producer category")
  }
  observed<-.bh_pf_record(v$observed,paste0(path,".observed"),c("chains","rows_per_chain","rows","chain_ids","split_chains","split_rows_per_chain","split_rows","split_omitted_rows"))
  for(n in .bh_pf_setdiff(names(observed),"chain_ids")) .bh_pf_number(observed[[n]],paste0(path,".observed.",n),if(n%in%c("chains","rows_per_chain","rows"))1 else 0,if(n=="split_chains")4294967294.0 else 2147483647.0,TRUE)
  .bh_pf_nullable_numeric(observed$chain_ids,paste0(path,".observed.chain_ids"),TRUE)
  C<-observed$chains;L<-observed$rows_per_chain;h<-floor(L/2.0)
  if(C<1||L<1||C>2147483647.0/L||observed$rows!=C*L||length(observed$chain_ids)!=C||
     anyNA(observed$chain_ids)||any(!is.finite(observed$chain_ids)|observed$chain_ids<1|observed$chain_ids>2147483647.0|observed$chain_ids!=floor(observed$chain_ids))||
     base::anyDuplicated.default(observed$chain_ids)||(length(observed$chain_ids)>1L&&any(base::diff.default(observed$chain_ids)<=0))||observed$split_chains!=2.0*C||observed$split_rows_per_chain!=h||
     observed$split_rows!=2.0*C*h||observed$split_omitted_rows!=observed$rows-observed$split_rows)
    .bh_pf_fail(path,"recorded original/split chain counts disagree")
  if(length(v$parameter_metrics)!=length(selection$selected_keys)||length(v$hmc_metrics)!=C)
    .bh_pf_fail(path,"recorded metric bundles do not cover selected keys and original chains")
  phases<-.bh_pf_record(v$phases,paste0(path,".phases"),c("parameter_summary","hmc","conditioning","availability_and_covariance","expanded_summary","numerical_messages"))
  for(n in names(phases)) {
    fields<-if(n%in%c("parameter_summary","hmc"))c("status","reason")else c("status","reason","source","record","provenance")
    phase<-.bh_pf_history_status(phases[[n]],paste0(path,".phases.",n),fields,fields)
    permitted<-if(n=="parameter_summary")c("computed","computed_partial","not_requested")else if(n=="hmc")c("computed","partial","unavailable","not_applicable")else
      c("retained_evidence","deferred_by_policy","uncomputed_output_resource","uncomputed","unavailable")
    if(!phase$status%in%permitted) .bh_pf_fail(path,"unsupported saved diagnostic phase status")
    if("record"%in%fields) {
      kind<-c(conditioning="admissibility",availability_and_covariance="optional_reports",expanded_summary="expanded_correlation_matrices",numerical_messages="numerical_evaluations")[[n]]
      if(!identical(phase$source,paste0("cache$",kind))) .bh_pf_fail(path,"saved phase source differs from its declared cache evidence")
      .bh_pf_history_schema(phase$record,paste0("cache.",kind))
      .bh_pf_open_metadata(phase$provenance,paste0(path,".phases.",n,".provenance"))
    }
  }
  for(domain in c("parameter_metrics","hmc_metrics")) {
    records<-.bh_pf_get(v,domain)
    if(typeof(records)!="list"||isS4(records)||!is.null(.bh_pf_class(records))||any(!names(attributes(records))%in%"names")) .bh_pf_fail(path,"metrics must be saved plain records")
    for(i in seq_along(records)) .bh_pf_metric_bundle(.subset2(records,i),paste0(path,".",domain,"[",i,"]"),domain,observed)
  }
  provenance<-.bh_pf_record(v$provenance,paste0(path,".provenance"),c("source","estimator","budget"))
  estimator<-.bh_pf_record(provenance$estimator,paste0(path,".provenance.estimator"),c("id","required","versions","bindings"))
  .bh_pf_string(estimator$id,paste0(path,".provenance.estimator.id"))
  .bh_pf_open_metadata(estimator,paste0(path,".provenance.estimator"))
  .bh_pf_record(provenance$budget,paste0(path,".provenance.budget"),c("requested","metric_admission","source_admission"))
  .bh_pf_open_metadata(provenance,paste0(path,".provenance"))
  invisible(TRUE)
}
.bh_pf_metric_bundle <- function(x,path,domain,observed) {
  fields<-if(domain=="parameter_metrics")c("values","metrics","counts","global_constant","chain_constant","provenance")else
    c("values","metrics","counts","engine","status","coverage","max_depth","provenance","energy_constant","arithmetic")
  v<-.bh_pf_record(x,path,fields)
  metrics<-if(domain=="parameter_metrics")c("mean","median","sd","q5","q95","rhat","ess_bulk","ess_tail","mcse_mean")else
    c("num_divergent","fraction_divergent","num_max_treedepth","fraction_max_treedepth","ebfmi")
  denominator<-if(domain=="parameter_metrics")observed$rows else observed$rows_per_chain
  .bh_pf_nullable_numeric(v$values,paste0(path,".values"),TRUE)
  if(!identical(names(v$values),metrics)) .bh_pf_fail(path,"metric values lack their exact saved ordering")
  mm<-.bh_pf_record(v$metrics,paste0(path,".metrics"),metrics)
  if(!identical(names(mm),metrics)) .bh_pf_fail(path,"metric records lack their exact saved ordering")
  for(n in metrics) {
    mp<-paste0(path,".metrics.",n)
    m<-.bh_pf_record(mm[[n]],mp,c("raw","value","evaluated","outcome","status","reason","warnings","denominator"))
    for(k in c("raw","value")) .bh_pf_nullable_numeric(m[[k]],paste0(mp,".",k))
    .bh_pf_logical(m$evaluated,paste0(mp,".evaluated"))
    .bh_pf_string(m$status,paste0(mp,".status"),c("available","unavailable","not_applicable"))
    .bh_pf_character(m$warnings,paste0(mp,".warnings"),nonempty=TRUE)
    .bh_pf_character(m$reason,paste0(mp,".reason"),missing=TRUE)
    if(length(m$reason)!=1L||(!is.na(m$reason)&&!nzchar(m$reason))) .bh_pf_fail(mp,"metric reason must be NA or a nonempty scalar")
    .bh_pf_number(m$denominator,paste0(mp,".denominator"),1,(2^53 - 1),TRUE)
    if(m$denominator!=denominator) .bh_pf_fail(mp,"metric denominator differs from the retained observed count")
    outcome<-if(!m$evaluated)"not_evaluated"else if(is.nan(m$raw))"NaN"else if(is.na(m$raw))"NA"else
      if(m$raw==Inf)"positive_infinity"else if(m$raw==-Inf)"negative_infinity"else"finite"
    if(!identical(m$outcome,outcome)||(!m$evaluated&&(!is.na(m$raw)||is.nan(m$raw)))||
       (m$status!="available"&&(!is.na(m$value)||is.nan(m$value)||is.na(m$reason)))||
       (m$status=="available"&&(!m$evaluated||!is.na(m$reason)||!identical(as.double(m$value),as.double(m$raw))))||
       !identical(as.double(v$values[[n]]),as.double(m$value)))
      .bh_pf_fail(mp,"metric availability, outcome, and retained values disagree")
    if(m$status=="available") {
      valid<-if(n=="rhat")!is.na(m$value)&&m$value>0 else is.finite(m$value)
      if(n%in%c("sd","ess_bulk","ess_tail","mcse_mean","num_divergent","num_max_treedepth","fraction_divergent","fraction_max_treedepth","ebfmi"))
        valid<-valid&&m$value>=0
      if(n%in%c("num_divergent","num_max_treedepth"))valid<-valid&&m$value==floor(m$value)&&m$value<=denominator
      if(n%in%c("fraction_divergent","fraction_max_treedepth"))valid<-valid&&m$value<=1
      if(n=="ebfmi")valid<-valid&&m$value>0
      if(!isTRUE(valid)) .bh_pf_fail(mp,"available metric violates its saved numeric domain")
    }
  }
  counts<-if(domain=="parameter_metrics")c("chains","rows_per_chain","rows","half_rows","split_chains","split_rows","omitted_rows")else c("rows","thin","adjacent_pairs")
  cc<-.bh_pf_record(v$counts,paste0(path,".counts"),counts)
  for(n in names(cc)) .bh_pf_number(cc[[n]],paste0(path,".counts.",n),0,(2^53 - 1),TRUE)
  if(domain=="parameter_metrics") {
    map<-c(chains="chains",rows_per_chain="rows_per_chain",rows="rows",half_rows="split_rows_per_chain",split_chains="split_chains",split_rows="split_rows",omitted_rows="split_omitted_rows")
    for(n in names(map))if(cc[[n]]!=observed[[map[[n]]]]) .bh_pf_fail(path,"parameter metric counts differ from observed original/split counts")
    .bh_pf_logical(v$global_constant,paste0(path,".global_constant"))
    if(!.bh_pf_plain(v$chain_constant,"logical")||anyNA(v$chain_constant)||length(v$chain_constant)!=observed$chains||
       (v$global_constant&&!all(v$chain_constant))) .bh_pf_fail(path,"constant-chain flags disagree with the saved global flag or chain count")
    for(n in c("sd","rhat","ess_bulk","ess_tail","mcse_mean")) {
      m<-mm[[n]]
      if(m$status!="available")next
      if((v$global_constant&&n!="sd")||(!v$global_constant&&n%in%c("sd","mcse_mean")&&m$value==0)||
         (n=="sd"&&observed$rows<2)||
         (n=="rhat"&&(observed$chains<2||observed$rows_per_chain<4))||
         (n%in%c("ess_bulk","ess_tail","mcse_mean")&&observed$rows_per_chain<6))
        .bh_pf_fail(path,"available parameter metric lacks its saved constant-data or sample-count prerequisites")
    }
    if(mm$mcse_mean$status=="available"&&mm$sd$status!="available")
      .bh_pf_fail(path,"available mean MCSE requires available SD evidence")
  } else {
    L<-observed$rows_per_chain
    if(cc$rows!=L||cc$rows<1||cc$thin<1||cc$adjacent_pairs!=L-1)
      .bh_pf_fail(path,"HMC metric row/thinning/adjacency counts disagree")
    .bh_pf_string(v$engine,paste0(path,".engine"),c("nuts","static"))
    for(pair in list(c("num_divergent","fraction_divergent"),c("num_max_treedepth","fraction_max_treedepth"))) {
      count<-mm[[pair[[1L]]]];fraction<-mm[[pair[[2L]]]]
      if(!identical(count$status,fraction$status)||(count$status=="available"&&fraction$value!=count$value/L))
        .bh_pf_fail(path,"HMC count and fraction evidence disagree")
    }
    statuses<-vapply(mm,function(m)m$status,character(1))
    expected<-if(all(statuses%in%c("available","not_applicable")))"computed"else if(any(statuses=="available"))"partial"else"unavailable"
    if(!identical(v$status,expected)) .bh_pf_fail(path,"HMC chain status overstates its saved metric evidence")
    coverage<-.bh_pf_record(v$coverage,paste0(path,".coverage"),c("divergent","depth","energy"))
    if(!identical(names(coverage),c("divergent","depth","energy"))) .bh_pf_fail(path,"HMC coverage ordering is malformed")
    for(n in names(coverage)) {
      p<-paste0(path,".coverage.",n)
      item<-.bh_pf_record(coverage[[n]],p,c("present","retained","finite","missing"))
      .bh_pf_logical(item$present,paste0(p,".present"))
      for(k in c("retained","finite","missing")) .bh_pf_number(item[[k]],paste0(p,".",k),0,(2^53 - 1),TRUE)
      if(item$retained!=L||item$finite!=(if(item$present)L else 0)||item$missing!=(if(item$present)0 else L))
        .bh_pf_fail(p,"HMC coverage contradicts its saved presence flag or row count")
    }
    depth<-.bh_pf_record(v$max_depth,paste0(path,".max_depth"),c("status","value","raw","reason"))
    .bh_pf_string(depth$status,paste0(path,".max_depth.status"),c("available","unavailable"))
    .bh_pf_nullable_numeric(depth$value,paste0(path,".max_depth.value"))
    .bh_pf_character(depth$raw,paste0(path,".max_depth.raw"))
    if(depth$status=="available") {
      .bh_pf_number(depth$value,paste0(path,".max_depth.value"),1,2147483647.0,TRUE)
      if(!identical(depth$reason,NA_character_)) .bh_pf_fail(path,"available max-depth metadata must have its original NA reason")
    } else {
      if(!is.na(depth$value)||is.nan(depth$value)) .bh_pf_fail(path,"unavailable max-depth metadata carries a usable value")
      .bh_pf_string(depth$reason,paste0(path,".max_depth.reason"),c("missing_max_depth","duplicate_max_depth","malformed_max_depth","contradictory_max_depth"))
    }
    if((v$engine=="static"&&mm$num_max_treedepth$status!="not_applicable")||
       (v$engine=="nuts"&&mm$num_max_treedepth$status=="not_applicable")||
       (v$engine=="nuts"&&depth$status!="available"&&mm$num_max_treedepth$status=="available"))
      .bh_pf_fail(path,"treedepth metric disagrees with the saved engine or max-depth availability")
    for(pair in list(c("divergent","num_divergent"),c("depth","num_max_treedepth"),c("energy","ebfmi")))
      if(!coverage[[pair[[1L]]]]$present&&mm[[pair[[2L]]]]$status=="available")
        .bh_pf_fail(path,"an available HMC metric lacks its retained source-column coverage")
    bfmi<-mm$ebfmi;present<-coverage$energy$present;constant<-v$energy_constant
    if((L<3||cc$thin!=1)&&bfmi$status=="available") .bh_pf_fail(path,"available BFMI lacks complete adjacent saved energies")
    if(!.bh_pf_plain(constant,"logical")||length(constant)!=1L||
       (present&&is.na(constant))||(!present&&!is.na(constant)))
      .bh_pf_fail(path,"energy-constant evidence disagrees with retained energy coverage")
    arithmetic_fields<-c("sum_squared_differences","numerator","variance","raw_ratio")
    arithmetic<-.bh_pf_record(v$arithmetic,paste0(path,".arithmetic"),arithmetic_fields)
    if(!identical(names(arithmetic),arithmetic_fields)) .bh_pf_fail(path,"BFMI arithmetic record order is malformed")
    for(n in arithmetic_fields) .bh_pf_nullable_numeric(arithmetic[[n]],paste0(path,".arithmetic.",n))
    if(!bfmi$evaluated) {
      if(!all(vapply(arithmetic,function(value)is.na(value)&&!is.nan(value),logical(1))))
        .bh_pf_fail(path,"unevaluated BFMI carries fabricated arithmetic evidence")
    } else {
      if(!present||L<3||cc$thin!=1) .bh_pf_fail(path,"evaluated BFMI lacks its saved energy prerequisites")
      squares<-arithmetic$sum_squared_differences;numerator<-arithmetic$numerator;variance<-arithmetic$variance;ratio<-arithmetic$raw_ratio
      if(!identical(as.double(numerator),as.double(squares/L))||
         !identical(as.double(ratio),as.double(numerator/variance))||!identical(as.double(ratio),as.double(bfmi$raw)))
        .bh_pf_fail(path,"saved BFMI arithmetic and raw metric disagree")
      if(bfmi$status=="available"&&(constant||!all(is.finite(c(squares,numerator,variance,ratio)))||any(c(squares,numerator,variance,ratio)<=0)))
        .bh_pf_fail(path,"available BFMI contradicts constant or nonpositive/nonfinite arithmetic evidence")
    }
  }
  .bh_pf_named(v$provenance,paste0(path,".provenance"))
  .bh_pf_open_metadata(v$provenance,paste0(path,".provenance"))
  invisible(TRUE)
}

.bh_pf_ppc_plan <- function(x,path) {
  fields<-c("version","level","law","replications","observations_per_rep","simulated_values","latent_values",
    "predictor_values_per_rep","scatter_values","geometry","roster","active_target_index","required_variables","required_columns",
    "budget","budget_origin","source","statistics","statistic_counts","model_identity","scientific_scope","cost_scope","assessment_status","screening_only")
  v<-.bh_pf_record(x,path,c(fields,"allocation_accounting"),fields,classes="bmeas_ppc_plan")
  .bh_pf_string(v$version,paste0(path,".version"),c("bmeas_basic_ppc_plan_v1","bmeas_basic_ppc_plan_v2","bmeas_basic_ppc_plan_v3",
    "bmeas_m3cs_ppc_plan_v1","bmeas_m3cs_ppc_plan_v2","bmeas_m3cs_ppc_plan_v3"))
  scalable<-v$version%in%c("bmeas_basic_ppc_plan_v3","bmeas_m3cs_ppc_plan_v3")
  if(!identical("allocation_accounting"%in%names(v),scalable))
    .bh_pf_fail(path,"allocation accounting must match the saved PPC-plan version")
  .bh_pf_string(v$level,paste0(path,".level"),c("population","conditional"))
  .bh_pf_string(v$law,paste0(path,".law"),c("iid_common_plus_parameter_residual","same_N_sample_centered_task_cholesky","conditional_response"))
  for(n in c("replications","observations_per_rep","simulated_values","latent_values","predictor_values_per_rep","scatter_values","required_columns"))
    .bh_pf_number(v[[n]],paste0(path,".",n),0,(2^53 - 1),TRUE)
  .bh_pf_character(v$required_variables,paste0(path,".required_variables"),TRUE,TRUE)
  if(length(v$required_variables)!=v$required_columns||!identical(v$assessment_status,"not_run")||!identical(v$screening_only,TRUE))
    .bh_pf_fail(path,"saved PPC-plan interpretation or required-column count is inconsistent")
  for(n in c("scientific_scope","cost_scope")) .bh_pf_string(v[[n]],paste0(path,".",n))
  # Historical versions retain their exact acquisition and budget schemas.
  # Version 3 records explicit work admission for either acquisition route.
  streaming<-v$version%in%c("bmeas_basic_ppc_plan_v2","bmeas_m3cs_ppc_plan_v2")||
    (scalable&&"posterior"%in%.bh_pf_names(v$budget))
  budget_fields<-c("max_draws","max_observations","max_simulated_values","max_latent_values","max_statistics","max_pair_terms",
    if(streaming)"posterior"else c("max_source_bytes","max_posterior_rows","max_draw_values"),if(scalable)"max_work_values")
  budget<-.bh_pf_record(v$budget,paste0(path,".budget"),budget_fields)
  for(n in .bh_pf_setdiff(names(budget),"posterior")) .bh_pf_number(budget[[n]],paste0(path,".budget.",n),0,(2^53 - 1),TRUE)
  if(streaming) .bh_pf_open_metadata(budget$posterior,paste0(path,".budget.posterior"))
  if(scalable) {
    for(n in .bh_pf_setdiff(names(budget),"posterior"))
      .bh_pf_number(budget[[n]],paste0(path,".budget.",n),0,2147483647.0,TRUE)
    .bh_pf_number(v$replications,paste0(path,".replications"),2,budget$max_draws,TRUE)
    .bh_pf_number(v$observations_per_rep,paste0(path,".observations_per_rep"),0,budget$max_observations,TRUE)
    .bh_pf_number(v$simulated_values,paste0(path,".simulated_values"),0,budget$max_simulated_values,TRUE)
    .bh_pf_number(v$latent_values,paste0(path,".latent_values"),0,budget$max_latent_values,TRUE)
    geometry<-.bh_pf_record(v$geometry,paste0(path,".geometry"),c("N","J","K","U","S","JT"))
    for(n in names(geometry)) .bh_pf_number(geometry[[n]],paste0(path,".geometry.",n),0,2147483647.0,TRUE)
    if(geometry$JT>geometry$J) .bh_pf_fail(path,"saved active task dimension exceeds the target dimension")
    sc<-.bh_pf_record(v$statistic_counts,paste0(path,".statistic_counts"),
      c("groups","marginal_statistics","pair_statistics","statistics","pair_overlap","pair_terms"))
    for(n in names(sc)) .bh_pf_number(sc[[n]],paste0(path,".statistic_counts.",n),0,2147483647.0,TRUE)
    if(sc$statistics>budget$max_statistics||sc$pair_terms>budget$max_pair_terms||
      sc$statistics!=as.double(sc$marginal_statistics)+sc$pair_statistics||
      sc$pair_terms!=(as.double(v$replications)+1)*sc$pair_overlap||
      !identical(.bh_pf_class(v$statistics),"data.frame")||
      .bh_pf_table_shape(v$statistics,paste0(path,".statistics"))!=sc$statistics)
      .bh_pf_fail(path,"saved statistic dimensions disagree with the predictive plan or budget")
    allocation_fields<-c("predictor_slots","generated_coordinate_slots","centered_coordinate_slots","loading_slots",
      "covariance_slots","scalar_name_slots","row_map_slots","statistic_slots")
    allocation<-.bh_pf_record(v$allocation_accounting,paste0(path,".allocation_accounting"),allocation_fields)
    for(n in allocation_fields)
      .bh_pf_number(allocation[[n]],paste0(path,".allocation_accounting.",n),0,2147483647.0,TRUE)
    # Recompute only saved integer arithmetic, never forward planning, source
    # acquisition, scientific checks, or any allocator/model callback.
    product<-function(...) {
      factors<-as.double(c(...))
      if(any(factors==0))return(0)
      value<-1
      for(factor in factors) {
        if(value>2147483647.0/factor)
          .bh_pf_fail(path,"saved allocation product exceeds representable work slots")
        value<-value*factor
      }
      value
    }
    N<-geometry$N;J<-geometry$J;K<-geometry$K;U<-geometry$U;JT<-geometry$JT
    q<-as.double(K)+U
    covariance<-sum(c(product(K,K),product(U,U),product(JT,JT)))
    expected_allocation<-c(predictor_slots=product(4,N,J),generated_coordinate_slots=product(2,N,q),
      centered_coordinate_slots=product(3,JT,N),loading_slots=product(J,q),covariance_slots=covariance,
      scalar_name_slots=sum(c(J,geometry$S,product(N,J),product(J,q),covariance)),
      row_map_slots=product(40,v$observations_per_rep),
      statistic_slots=sum(c(product(v$replications,sc$statistics),product(40,sc$statistics),
        product(N,sc$groups),product(8,v$observations_per_rep),product(sc$groups,sc$groups,N))))
    response_cells<-if(identical(v$version,"bmeas_m3cs_ppc_plan_v3"))10 else 1
    if(v$simulated_values!=product(v$replications,v$observations_per_rep,response_cells)||
      v$predictor_values_per_rep!=product(N,J)||
      v$scatter_values!=as.double(v$replications)*v$predictor_values_per_rep)
      .bh_pf_fail(path,"saved response or predictor work disagrees with the admitted dimensions")
    if(any(unlist(allocation[allocation_fields],use.names=FALSE)!=unname(expected_allocation)))
      .bh_pf_fail(path,"saved allocation counts disagree with their geometry or statistic dimensions")
    if(sum(unlist(allocation,use.names=FALSE))>budget$max_work_values)
      .bh_pf_fail(path,"saved allocation accounting exceeds its predictive work budget")
  }
  source_fields<-if(streaming)c("availability","required_reader","observed_rows","maximum_source_rows","recorded_path_count","expected_chains",
    "policy","policy_origin","numeric_buffer_known_lower_bound","numeric_buffer_scope","consumer_buffers","selection","validation_scope","original_fit_authentication")else
    c("availability","required_reader","observed_rows","maximum_admissible_source_rows","maximum_materialized_values","original_fit_authentication")
  .bh_pf_record(v$source,paste0(path,".source"),c(source_fields,"reader_execution"),source_fields)
  if(!is.null(v$source$reader_execution)) .bh_pf_reader_capability_check(v$source$reader_execution,v,path)
  .bh_pf_record(v$model_identity,paste0(path,".model_identity"),c("specification_fingerprint","saved_fit_model_fingerprint","data_identity"))
  for(n in c("geometry","roster","active_target_index","budget_origin","source","statistics","statistic_counts","model_identity"))
    .bh_pf_open_metadata(v[[n]],paste0(path,".",n))
  invisible(TRUE)
}
.bh_pf_ppc <- function(x,path,scientific_tables=NULL) {
  if(is.null(x))return(invisible(TRUE))
  success<-!is.null(.bh_pf_get(x,"table"))
  fields<-if(success)c("table","replicate_statistics","quantile_type","coverage","measurement_structure","conditional_association","interpretation","metadata")else
    c("table","replicate_statistics","metadata")
  v<-.bh_pf_record(x,path,fields)
  mf<-c("version","level","law","seed","rng_kind","R_version","requested_replications","completed_replications","plan","source_qualification",
    "upstream","screening_only","source_origin_scope","interpretation")
  optional<-c("source_receipt","posterior_selection","replicate_statistics_status","replicate_statistics_layout","quantile_type","actual_work","failure","simulator","draw_keys","stan_seeds","reader_execution")
  m<-.bh_pf_record(v$metadata,paste0(path,".metadata"),c(mf,optional),mf)
  .bh_pf_string(m$version,paste0(path,".metadata.version"),c("bmeas_basic_ppc_result_v1","bmeas_m3cs_ppc_result_v1",
    "bmeas_basic_ppc_result_v2","bmeas_m3cs_ppc_result_v2"))
  .bh_pf_ppc_plan(m$plan,paste0(path,".metadata.plan"))
  m$plan<-unclass(m$plan) # validated local copy; never dispatch saved plan accessors
  product_receipt<-identical(.bh_pf_get(.bh_pf_get(.bh_pf_get(m,"source_receipt"),"context_validation"),"context_profile"),
    "bmeas_iid_gaussian_product_context_v1")
  if(product_receipt&&!identical(m$reader_execution,.bh_pf_product_reader_capability()))
    .bh_pf_fail(path,"retained product context receipt requires its exact current-reader record","E_PORTABLE_CAPABILITY")
  if(!is.null(m$reader_execution)||!is.null(m$plan$source$reader_execution)) {
    if(!identical(m$reader_execution,m$plan$source$reader_execution)||
       !identical(m$level,m$plan$level)||!identical(m$law,m$plan$law))
      .bh_pf_fail(path,"saved reader result and plan capabilities, levels or laws disagree","E_PORTABLE_CAPABILITY")
    .bh_pf_reader_capability_check(m$reader_execution,m$plan,path)
    if(identical(m$reader_execution,.bh_pf_product_reader_capability())) {
      if(!is.null(scientific_tables)&&!.bh_pf_has_product(list(tables=scientific_tables)))
        .bh_pf_fail(path,"product reader result requires the product scientific context","E_PORTABLE_BINDING")
      if(!identical(.bh_pf_get(.bh_pf_get(m$upstream,"inference"),"status"),"not_assessed"))
        .bh_pf_fail(path,"product predictive reading cannot qualify inference","E_PORTABLE_CAPABILITY")
      source<-.bh_pf_get(m,"source_receipt")
      bound<-.bh_pf_get(source,"bound_model_identity")
      if((success&&is.null(bound))||(!is.null(bound)&&!identical(bound,m$plan$model_identity)))
        .bh_pf_fail(path,"product source receipt and plan model identities disagree","E_PORTABLE_BINDING")
      context<-.bh_pf_get(source,"context_validation")
      if(is.null(context)) .bh_pf_fail(path,"product result lacks its completed context receipt","E_PORTABLE_BINDING")
      .bh_pf_context_receipt_schema(context)
      portable_identity<-.bh_pf_get(source,"portable_bound_identity")
      if((success&&is.null(portable_identity))||
         (!is.null(portable_identity)&&!identical(portable_identity,context$identity)))
        .bh_pf_fail(path,"product source receipt and context identities disagree","E_PORTABLE_BINDING")
      if(!identical(context$operation,"assess_measurement_fit")||
         !identical(context$context_profile,"bmeas_iid_gaussian_product_context_v1"))
        .bh_pf_fail(path,"product result receipt names another operation or producer profile","E_PORTABLE_BINDING")
      if(success) {
        levels<-.bh_pf_get(v$table,"level")
        .bh_pf_character(levels,paste0(path,".table.level"))
        if(!identical(attr(v$table,"level",exact=TRUE),m$level)||any(levels!=m$level))
          .bh_pf_fail(path,"product result table level differs from its reader plan","E_PORTABLE_CAPABILITY")
      }
    }
  }
  scalable<-m$version%in%c("bmeas_basic_ppc_result_v2","bmeas_m3cs_ppc_result_v2")
  family<-if(m$version%in%c("bmeas_m3cs_ppc_result_v1","bmeas_m3cs_ppc_result_v2"))"m3cs"else"basic"
  expected_plan<-paste0("bmeas_",family,"_ppc_plan_v",if(scalable)3L else c(1L,2L))
  if(!.bh_pf_get(m$plan,"version")%in%expected_plan)
    .bh_pf_fail(path,"saved PPC result and plan schema versions disagree")
  draw_limit<-if(scalable).bh_pf_get(m$plan,"replications")else 20L
  statistic_limit<-if(scalable).bh_pf_get(.bh_pf_get(m$plan,"statistic_counts"),"statistics")else 200L
  .bh_pf_number(m$requested_replications,paste0(path,".metadata.requested_replications"),2,draw_limit,TRUE)
  .bh_pf_number(m$completed_replications,paste0(path,".metadata.completed_replications"),0,m$requested_replications,TRUE)
  if(scalable&&m$requested_replications!=draw_limit)
    .bh_pf_fail(path,"saved PPC result request differs from its admitted plan")
  if(!identical(m$screening_only,TRUE)) .bh_pf_fail(path,"PPC metadata cannot upgrade screening evidence")
  upstream<-.bh_pf_record(m$upstream,paste0(path,".metadata.upstream"),c("computation","inference","identification","posterior_propriety","posterior_normalizability"))
  for(n in names(upstream)) .bh_pf_evidence(upstream[[n]],paste0(path,".metadata.upstream.",n),n)
  if("failure"%in%names(m)) .bh_pf_record(m$failure,paste0(path,".metadata.failure"),c("phase","code","reason"))
  for(n in .bh_pf_setdiff(names(m),c("plan","upstream","failure"))) .bh_pf_open_metadata(m[[n]],paste0(path,".metadata.",n))
  if(success) {
    .bh_pf_table(v$table,paste0(path,".table"),c("statistic_id","domain","level","model","target","target1","target2",
      "group_index","pair_index","metric","unit","scale","definition","n_units","unit_type","n_rows","n_declared_rows","n_excluded_rows","n_people",
      "n_informative_rows","n_informative_people","n_zero_trial_rows","total_trials","overlap","rows_left","rows_right","trials_left","trials_right",
      "status","degenerate","low_information","observed","upper90","lower90","replicated_median","finite_replications","requested_replications","outside_interval","quantile_type","replication_coverage"))
    coverage_attr<-attr(v$table,"coverage",exact=TRUE)
    if(!is.null(coverage_attr)) .bh_pf_record(coverage_attr,paste0(path,".table@coverage"),c("planned","computed","unavailable","status"))
    level_attr<-attr(v$table,"level",exact=TRUE)
    if(!is.null(level_attr)) .bh_pf_string(level_attr,paste0(path,".table@level"),c("population","conditional"))
    if(!typeof(v$replicate_statistics)%in%c("integer","double")||length(attr(v$replicate_statistics,"dim",exact=TRUE))!=2L)
      .bh_pf_fail(path,"replicate statistics must retain a numeric matrix")
    table_rows<-.bh_pf_table_shape(v$table,paste0(path,".table"))
    if(table_rows>statistic_limit||
      (scalable&&(table_rows!=statistic_limit||m$completed_replications!=draw_limit||
        !identical(as.double(attr(v$replicate_statistics,"dim",exact=TRUE)),c(as.double(draw_limit),as.double(statistic_limit))))))
      .bh_pf_fail(path,"saved PPC result dimensions disagree with their admitted plan")
    if(!identical(v$quantile_type,7L)) .bh_pf_fail(path,"unsupported retained quantile convention")
    for(n in c("coverage","measurement_structure","conditional_association")) .bh_pf_history_status(v[[n]],paste0(path,".",n),
      c("status","planned","evaluable","complete","unavailable","reason"),c("status","planned","evaluable","complete","unavailable"))
    .bh_pf_string(v$interpretation,paste0(path,".interpretation"))
  } else if(!is.null(v$replicate_statistics)) .bh_pf_fail(path,"failed PPC table cannot carry unlabeled replicate results")
  invisible(TRUE)
}
.bh_pf_history_schema <- function(x,path,scientific_tables=NULL) {
  if(path=="run") {
    required<-c("chains","parallel_chains","threads_per_chain","iter_warmup","iter_sampling","seed","cpu_budget",
      "max_observations","max_subjects","max_saved_draws","reduction","grainsize","max_treedepth","model_budget","kind","wall_time_enforcement","memory_enforcement")
    v<-.bh_pf_record(x,path,c(required,"simultaneous_workers","requested_cores","csv_sig_figs"),required,classes="measurement_run_config")
    for(n in .bh_pf_setdiff(names(v),c("reduction","model_budget","kind","wall_time_enforcement","memory_enforcement","csv_sig_figs")))
      .bh_pf_number(v[[n]],paste0(path,".",n),0,(2^53 - 1),TRUE)
    if("csv_sig_figs"%in%names(v)) .bh_pf_number(v$csv_sig_figs,paste0(path,".csv_sig_figs"),1,18,TRUE)
    .bh_pf_string(v$reduction,paste0(path,".reduction"),c("serial","reduce_sum_static","reduce_sum"))
    for(n in c("kind","wall_time_enforcement","memory_enforcement")) .bh_pf_string(v[[n]],paste0(path,".",n))
    if(!is.null(v$model_budget)) .bh_pf_fail(path,"historical non-main model budget is outside this fit profile")
    return(invisible(TRUE))
  }
  if(path=="provenance") {
    required<-c("kind","started","finished","compile_key","cache_hit","executable_sha256","toolchain","initialization","model_fingerprint",
      "iter_total","execution_profile","exploratory_requested","exploratory_rank_override","save_log_lik","estimated_csv_bytes","output_files")
    optional<-c("resource_policy","resource_admission","output_estimate","cache_estimate","requested_run","adapt_delta","csv_sig_figs","plan_identity","scientific_checks","postprocessing_support","provider","provider_metadata")
    p<-.bh_pf_record(x,path,c(required,optional),required)
    for(n in c("kind","compile_key","executable_sha256","model_fingerprint")) .bh_pf_string(p[[n]],paste0(path,".",n))
    for(n in c("cache_hit","exploratory_requested","exploratory_rank_override","save_log_lik")) .bh_pf_logical(p[[n]],paste0(path,".",n))
    for(n in c("started","finished")) .bh_pf_timestamp(p[[n]],paste0(path,".",n))
    .bh_pf_character(p$output_files,paste0(path,".output_files"),TRUE,TRUE)
    tc<-c("cmdstan","cmdstan_path","cmdstanr","R","platform","compiler","compiler_version","environment","make_local","cpp_options","stanc_options")
    toolchain<-.bh_pf_record(p$toolchain,paste0(path,".toolchain"),tc)
    for(n in .bh_pf_setdiff(tc,c("cpp_options","stanc_options"))) .bh_pf_character(toolchain[[n]],paste0(path,".toolchain.",n),names=TRUE)
    for(n in c("cpp_options","stanc_options")) .bh_pf_open_metadata(toolchain[[n]],paste0(path,".toolchain.",n))
    .bh_pf_evidence(p$initialization,paste0(path,".initialization"),"initialization")
    .bh_pf_evidence(p$execution_profile,paste0(path,".execution_profile"),"execution_profile")
    if("csv_sig_figs"%in%names(p)) .bh_pf_number(p$csv_sig_figs,paste0(path,".csv_sig_figs"),1,18,TRUE)
    if("scientific_checks"%in%names(p)) .bh_pf_history_schema(p$scientific_checks,"spec.checks")
    for(n in .bh_pf_setdiff(optional,c("scientific_checks","csv_sig_figs")))if(n%in%names(p)) .bh_pf_open_metadata(p[[n]],paste0(path,".",n))
    return(invisible(TRUE))
  }
  if(path=="execution_receipt") {
    if(is.null(x))return(invisible(TRUE))
    v<-.bh_pf_record(x,path,c("version","sampling","output"))
    if(!identical(v$version,"bmeas_execution_receipt_v1")) .bh_pf_fail(path,"unsupported execution-receipt version")
    sf<-c("status","reason","return_codes","return_codes_availability","return_code_coverage","configured_chains","observed_csv_rows","observed_csv_rows_availability","started","finished")
    s<-.bh_pf_record(v$sampling,paste0(path,".sampling"),sf)
    for(n in c("started","finished")) .bh_pf_timestamp(s[[n]],paste0(path,".sampling.",n))
    for(n in c("status","reason","return_codes_availability","return_code_coverage","observed_csv_rows_availability")) .bh_pf_string(s[[n]],paste0(path,".sampling.",n))
    if(!is.null(s$return_codes)) .bh_pf_nullable_numeric(s$return_codes,paste0(path,".sampling.return_codes"),TRUE)
    .bh_pf_number(s$configured_chains,paste0(path,".sampling.configured_chains"),1,2147483647,TRUE)
    .bh_pf_nullable_numeric(s$observed_csv_rows,paste0(path,".sampling.observed_csv_rows"))
    of<-c("paths","paths_availability","path_coverage","configured_path_count","reported_path_count","original_file_hashes")
    o<-.bh_pf_record(v$output,paste0(path,".output"),of)
    .bh_pf_character(o$paths,paste0(path,".output.paths"),TRUE,TRUE)
    .bh_pf_history_status(o$original_file_hashes,paste0(path,".output.original_file_hashes"),c("status","reason","algorithm","hashes","paths","scope"),c("status","reason"))
    return(invisible(TRUE))
  }
  if(path=="resource_outcome") {
    if(is.null(x))return(invisible(TRUE))
    fields<-c("version","status","max_output_bytes","actual_output_bytes","output_files","path_coverage","files","reason","enforcement","original_file_hashes")
    v<-.bh_pf_record(x,path,fields)
    if(!identical(v$version,"bmeas_postfit_output_resource_v1")) .bh_pf_fail(path,"unsupported output-resource version")
    .bh_pf_table(v$files,paste0(path,".files"),c("path","bytes","status"))
    .bh_pf_character(v$output_files,paste0(path,".output_files"),TRUE,TRUE)
    return(invisible(TRUE))
  }
  if(path=="spec.checks") {
    if(is.null(x)) .bh_pf_fail(path,"scientific history is missing")
    required<-c("structure","correlation_feasibility","correlation_lowering","correlation_prior_geometry","identification","priors","gaussian_diagnostics",
      "posterior_propriety","posterior_normalizability","numerical_qualification","initialization","backend")
    v<-.bh_pf_record(x,path,c(required,"execution_profile"),required)
    .bh_pf_history_status(v$structure,paste0(path,".structure"),c("status","reason"),c("status","reason"))
    for(n in c("identification","gaussian_diagnostics","posterior_propriety","posterior_normalizability","initialization","execution_profile"))
      if(n%in%names(v)) .bh_pf_evidence(v[[n]],paste0(path,".",n),n)
    .bh_pf_record(v$priors,paste0(path,".priors"),c("status","invalid","missing"))
    .bh_pf_record(v$numerical_qualification,paste0(path,".numerical_qualification"),c("status","eta","reason"))
    .bh_pf_record(v$backend,paste0(path,".backend"),c("status","reason","support","qualification","evidence_version","evidence_scope","additional_evidence","compiler_policy","limitations"))
    for(n in c("correlation_feasibility","correlation_lowering","correlation_prior_geometry","priors","numerical_qualification","backend"))
      .bh_pf_open_metadata(v[[n]],paste0(path,".",n))
    return(invisible(TRUE))
  }
  if(path=="cache") {
    fields<-c("parameter_summary","fixed_parameters","parameter_roles","loading_cells","loading_primitives","loading_orientation","component_scales","components",
      "derived_marker_estimands","optional_reports","hmc","computation","numerical_evaluations","admissibility","posterior_propriety","posterior_normalizability",
      "gaussian_diagnostics","execution_profile","backend_qualification","identification","inference","predictive_fit","correlation_constraints","expanded_correlation_matrices",
      "reference_law","initialization","posterior_reporting","ppc","measurement_structure_fit","report_metadata","diagnostic_thresholds")
    v<-.bh_pf_record(x,path,fields,character())
    for(n in names(v)) .bh_pf_history_schema(v[[n]],paste0(path,".",n),scientific_tables=scientific_tables)
    .bh_pf_reporting_binding(.bh_pf_get(v,"posterior_reporting"),v)
    return(invisible(TRUE))
  }
  name<-substring(path,7L)
  if(!startsWith(path,"cache.")) .bh_pf_fail(path,"unknown historical schema position")
  if(is.null(x))return(invisible(TRUE))
  scientific<-c(fixed_parameters="fixed_parameters",parameter_roles="parameter_roles",loading_cells="loading_cells",loading_primitives="loading_primitives",
    loading_orientation="orientation",component_scales="component_scales",components="components")
  if(name%in%names(scientific))return(.bh_portable_scientific_schema(x,paste0("spec.tables.",scientific[[name]])))
  if(name%in%c("computation","inference","identification","posterior_propriety","posterior_normalizability","gaussian_diagnostics","execution_profile","backend_qualification","initialization","admissibility","predictive_fit","measurement_structure_fit"))
    return(.bh_pf_evidence(x,path,name))
  if(name=="numerical_evaluations")return(.bh_pf_numerical(x,path))
  if(name=="posterior_reporting")return(.bh_pf_reporting(x,path))
  if(name=="ppc")return(.bh_pf_ppc(x,path,scientific_tables))
  if(name=="parameter_summary") {
    ordinary<-c("variable","mean","median","sd","mad","q5","q95","rhat","ess_bulk","ess_tail","mcse_mean",
      "parameter_key","parameter_role","loading_aliases","target_variation_level","reference_target","reference_person_id","reference_basis_index",
      "reference_raw_block","reference_previous_whitened_coordinate","correlation_layer","correlation_computational_variable","correlation_computational_mapping","correlation_alias_cells")
    refreshed<-c("variable","parameter_key","parameter_role","label","layer","units","source_kind","prior_coordinate","mean","median","sd","q5","q95","rhat","ess_bulk","ess_tail","mcse_mean")
    n<-.bh_pf_names(x)
    .bh_pf_table(x,path,if("source_kind"%in%n)refreshed else ordinary,
      if("source_kind"%in%n)refreshed else c("variable","parameter_key","parameter_role"))
    return(invisible(TRUE))
  }
  if(name=="hmc") {
    .bh_pf_record(x,path,c("num_divergent","num_max_treedepth","ebfmi","chain_ids","chain_id","fraction_divergent","fraction_max_treedepth"),
      c("num_divergent","num_max_treedepth","ebfmi"))
    for(n in names(x)) .bh_pf_nullable_numeric(.bh_pf_get(x,n),paste0(path,".",n),TRUE)
    return(invisible(TRUE))
  }
  if(name=="optional_reports") {
    v<-.bh_pf_record(x,path,c("status","unavailable_values","flags","entries","log_magnitudes","log_metadata_semantics","reason"),c("status","unavailable_values"))
    .bh_pf_string(v$status,paste0(path,".status"));.bh_pf_nullable_numeric(v$unavailable_values,paste0(path,".unavailable_values"))
    full<-c("flags","entries","log_magnitudes","log_metadata_semantics")
    if(any(full%in%names(v))&&!all(full%in%names(v))) .bh_pf_fail(path,"retained optional-report payload is incomplete")
    if(all(full%in%names(v))) {
      .bh_pf_table(v$entries,paste0(path,".entries"),c("variable","unavailable_draws","total_draws"))
      if(!typeof(v$flags)%in%c("integer","double","logical")||length(attr(v$flags,"dim",exact=TRUE))!=2L) .bh_pf_fail(path,"availability flags need their numeric/logical matrix storage")
      if(typeof(v$log_magnitudes)=="list") .bh_pf_history_status(v$log_magnitudes,paste0(path,".log_magnitudes"),c("status","reason"),c("status","reason"))else
        if(!typeof(v$log_magnitudes)%in%c("integer","double")||length(attr(v$log_magnitudes,"dim",exact=TRUE))!=2L) .bh_pf_fail(path,"log magnitudes need a numeric matrix or unavailable record")
      .bh_pf_string(v$log_metadata_semantics,paste0(path,".log_metadata_semantics"))
    }
    return(invisible(TRUE))
  }
  if(name=="report_metadata") {
    v<-.bh_pf_record(x,path,c("observed_run","monitored","summary_method","interval_columns","previous_unavailable"),character())
    if("summary_method"%in%names(v)) .bh_pf_record(v$summary_method,paste0(path,".summary_method"),c("status","method","scope","parameter_keys"))
    if("interval_columns"%in%names(v)) .bh_pf_character(v$interval_columns,paste0(path,".interval_columns"),TRUE,TRUE)
    if("previous_unavailable"%in%names(v)) .bh_pf_string(v$previous_unavailable,paste0(path,".previous_unavailable"))
    for(n in .bh_pf_intersect(c("observed_run","monitored"),names(v))) .bh_pf_open_metadata(v[[n]],paste0(path,".",n))
    return(invisible(TRUE))
  }
  if(name=="diagnostic_thresholds") {
    .bh_pf_named(x,path)
    return(.bh_pf_open_metadata(x,path))
  }
  if(name=="reference_law") {
    fields<-c("person_law","person_roster","augmentation","task_target_domain","latent_coordinate_domains","correlation_coordinates",
      "ordered_raw_blocks","raw_correlation_coordinates","resolved_priors","summary_map","score_interpretation","white_output_identity","prior_measure")
    v<-.bh_pf_record(x,path,fields)
    aliases<-c(person_law="person_law",person_roster="person_roster",augmentation="augmentation",task_target_domain="task_target_domain",
      latent_coordinate_domains="task_latent_coordinates",correlation_coordinates="correlation_coordinate_system",ordered_raw_blocks="correlation_blocks",raw_correlation_coordinates="raw_correlation_coordinates",resolved_priors="priors")
    for(n in names(aliases)) .bh_portable_scientific_schema(v[[n]],paste0("spec.tables.",aliases[[n]]))
    .bh_pf_table(v$summary_map,paste0(path,".summary_map"),c("variable","parameter_key","parameter_role","target","person_id","basis_index","raw_block","previous_whitened_coordinate"))
    for(n in c("score_interpretation","white_output_identity","prior_measure")) .bh_pf_string(v[[n]],paste0(path,".",n))
    return(invisible(TRUE))
  }
  if(name=="correlation_constraints") {
    fields<-c("version","primitive_summary_map","primitives","cells","domains","prior_geometry","coordinate_scope","runtime_scope")
    v<-.bh_pf_record(x,path,fields)
    .bh_pf_string(v$version,paste0(path,".version"),c("centered_final_marginal_cache_v1","reference_affine_residual_cache_v1","unique_marginal_correlation_cache_v2","iid_gaussian_common_product_live_cache_v1","iid_gaussian_residual_dual_product_live_cache_v1"))
    if(v$version%in%c("iid_gaussian_common_product_live_cache_v1","iid_gaussian_residual_dual_product_live_cache_v1")&&
       (is.null(scientific_tables)||!.bh_pf_has_product(list(tables=scientific_tables))))
      .bh_pf_fail(path,"product cache producer requires the distinct product scientific profile")
    aliases<-c(primitives="correlation_primitives",cells="correlation_affine_cells",domains="correlation_domains",prior_geometry="correlation_prior_geometry")
    for(n in names(aliases)) .bh_portable_scientific_schema(v[[n]],paste0("spec.tables.",aliases[[n]]),
      allow_endpoint_free=n=="cells"&&!is.null(scientific_tables)&&
        identical(v$cells,.bh_pf_get(scientific_tables,"correlation_affine_cells")))
    .bh_pf_table(v$primitive_summary_map,paste0(path,".primitive_summary_map"),c("variable","parameter_key","parameter_role","layer","computational_variable","computational_mapping","alias_cells"))
    return(invisible(TRUE))
  }
  if(name=="expanded_correlation_matrices") {
    v<-.bh_pf_record(x,path,c("status","entries","scope","reason"),"status")
    .bh_pf_string(v$status,paste0(path,".status"))
    if("entries"%in%names(v)) {
      if(.bh_pf_length(v$entries)==0L) .bh_pf_table(v$entries,paste0(path,".entries"),character())else
        .bh_pf_table(v$entries,paste0(path,".entries"),c("variable","mean","median","sd","q5","q95","rhat","ess_bulk","ess_tail","mcse_mean","layer","row","col","parameter_key","role","fixed_value"))
    }
    return(invisible(TRUE))
  }
  if(name=="derived_marker_estimands") {
    .bh_pf_table(x,path,c("variable","parameter_key","parameter_role","target","component","component2","person","units","declared_beta_primitive","component_sd_parameter","component_sd_parameter2"))
    return(invisible(TRUE))
  }
  .bh_pf_fail(path,"unsupported cache record")
}
# Retained bmf records only. These checks do not rebuild declarations, execute
# selectors, recompute legacy seals, or invoke any model/numerical validator.
# Source: frontend-core/basis/residual/share/loading-constraints/marker/reference/
# migrate.R in bmeas 0.0.24.9007, SOURCE_SCHEMA_INVENTORY_v1_3.json.

.bh_pf_formula_dim <- function(x) base::attr(x, "dim", exact = TRUE)
.bh_pf_formula_get <- function(x, field) base::.subset2(base::unclass(x), field)
.bh_pf_formula_ncol <- function(x) base::.subset2(.bh_pf_formula_dim(x), 2L)
.bh_pf_formula_rownames <- function(x) base::.subset2(base::attr(x, "dimnames", exact = TRUE), 1L)
.bh_pf_formula_colnames <- function(x) base::.subset2(base::attr(x, "dimnames", exact = TRUE), 2L)
.bh_pf_formula_vector <- function(x) {
  y <- base::unclass(x)
  base::attributes(y) <- NULL
  y
}
.bh_pf_formula_setequal <- function(x, y) {
  all(!is.na(base::match(x, y))) && all(!is.na(base::match(y, x)))
}

.bh_pf_formula_string <- function(x, path, choices = NULL, empty = FALSE) {
  if (typeof(x) != "character" || !is.null(attr(x, "class", exact = TRUE)) ||
      !is.null(attr(x, "dim", exact = TRUE)) || length(x) != 1L || is.na(x) ||
      (!empty && !nzchar(x)) || (!is.null(choices) && !x %in% choices))
    .bh_pf_fail(path, "expected one admitted character value")
  invisible(TRUE)
}

.bh_pf_formula_strings <- function(x, path, minimum = 0L, unique = FALSE,
                                   missing = FALSE, empty = FALSE) {
  if (typeof(x) != "character" || !is.null(attr(x, "class", exact = TRUE)) ||
      !is.null(attr(x, "dim", exact = TRUE)) || length(x) < minimum ||
      (!missing && anyNA(x)) || (!empty && any(!is.na(x) & !nzchar(x))) ||
      (unique && base::anyDuplicated.default(x)))
    .bh_pf_fail(path, "malformed captured character vector")
  invisible(TRUE)
}

.bh_pf_formula_number <- function(x, path, minimum = -Inf, maximum = Inf,
                                  integer = FALSE, finite = TRUE) {
  if (!typeof(x) %in% c("integer", "double") || !is.null(attr(x, "class", exact = TRUE)) ||
      !is.null(attr(x, "dim", exact = TRUE)) || length(x) != 1L ||
      is.na(x) || (finite && !is.finite(x)) || x < minimum || x > maximum ||
      (integer && x != floor(x)))
    .bh_pf_fail(path, "malformed captured numeric scalar")
  invisible(TRUE)
}

.bh_pf_formula_flag <- function(x, path) {
  if (typeof(x) != "logical" || !is.null(attr(x, "class", exact = TRUE)) ||
      !is.null(attr(x, "dim", exact = TRUE)) || length(x) != 1L || is.na(x))
    .bh_pf_fail(path, "expected one nonmissing logical value")
  invisible(TRUE)
}

.bh_pf_formula_flags <- function(x, path) {
  if (typeof(x) != "logical" || !is.null(attr(x, "class", exact = TRUE)) ||
      !is.null(attr(x, "dim", exact = TRUE)) || anyNA(x))
    .bh_pf_fail(path, "expected a nonmissing logical vector")
  invisible(TRUE)
}

.bh_pf_formula_indices <- function(x, path, maximum = Inf, minimum = 1L,
                                   unique = FALSE, count = NULL, matrix = FALSE) {
  if (!typeof(x) %in% c("integer", "double") || !is.null(attr(x, "class", exact = TRUE)) || anyNA(x) ||
      (!matrix && !is.null(attr(x, "dim", exact = TRUE))) ||
      any(!is.finite(x)) || any(x != floor(x)) ||
      any(x < minimum | x > maximum) || (unique && base::anyDuplicated.default(x)) ||
      (!is.null(count) && length(x) != count))
    .bh_pf_fail(path, "malformed captured index vector")
  invisible(TRUE)
}

.bh_pf_formula_sequence <- function(x, path, minimum = 0L, named = FALSE, names_metadata = FALSE) {
  if (typeof(x) != "list" || !is.null(attr(x, "class", exact = TRUE)) ||
      any(!names(attributes(x)) %in% "names") || length(x) < minimum)
    .bh_pf_fail(path, "expected a captured list")
  if (!identical(names_metadata,FALSE) && !identical(names_metadata,TRUE))
    .bh_pf_fail(path, "names-metadata mode must be an explicit logical flag")
  n <- names(x)
  if (names_metadata) {
    if (named) .bh_pf_fail(path, "keyed and names-metadata sequence modes are distinct")
    if (!is.null(n)) {
      if (!.bh_pf_plain(n,"character") || length(n)!=length(x) || anyNA(n))
        .bh_pf_fail(path, "sequence names metadata must be nonmissing character values of the exact list length")
      .bh_pf_text_valid(n,path)
    }
  } else {
    if (named && length(x) &&
        (is.null(n) || anyNA(n) || any(!nzchar(n)) || base::anyDuplicated.default(n)))
      .bh_pf_fail(path, "expected distinct nonempty names")
    if (!named && !is.null(n))
      .bh_pf_fail(path, "this captured sequence must be unnamed")
  }
  invisible(TRUE)
}

.bh_pf_target_selector <- function(x, path, allow_empty = FALSE, names_metadata = FALSE) {
  .bh_pf_formula_sequence(x, path, minimum = if (allow_empty) 0L else 1L, names_metadata = names_metadata)
  ids <- character(length(x))
  for (i in seq_along(x)) {
    at <- paste0(path, "[[", i, "]]")
    z <- x[[i]]
    if (typeof(z) != "list" || is.null(.bh_pf_formula_get(z, "kind")))
      .bh_pf_fail(at, "missing captured target kind")
    fields <- if (identical(.bh_pf_formula_get(z, "kind"), "wide")) c("kind", "column") else
      if (identical(.bh_pf_formula_get(z, "kind"), "target")) c("kind", "block", "parameter", "task") else
        .bh_pf_fail(at, "unknown captured target kind")
    z <- .bh_pf_record(z, at, fields)
    for (field in fields) .bh_pf_formula_string(z[[field]], paste0(at, ".", field))
    if (any(vapply(z[.bh_ps_setdiff(fields, "kind")], function(v) grepl("::", v, fixed = TRUE), logical(1))))
      .bh_pf_fail(at, "target key components cannot contain the key delimiter")
    ids[i] <- if (z$kind == "wide") paste("wide", z$column, sep = "::") else
      paste("target", z$block, z$parameter, z$task, sep = "::")
  }
  if (base::anyDuplicated.default(ids)) .bh_pf_fail(path, "duplicate captured target selector")
  invisible(TRUE)
}

.bh_pf_formula_target_keys <- function(x) {
  vapply(x, function(z) if (z$kind == "wide") paste("scores", "mu", z$column, sep = "::") else
    paste(z$block, z$parameter, z$task, sep = "::"), character(1), USE.NAMES = FALSE)
}

.bh_pf_component_selectors <- function(x, path, components = NULL) {
  .bh_pf_formula_sequence(x, path, minimum = 1L, named = TRUE)
  if (!is.null(components) && any(!names(x) %in% components))
    .bh_pf_fail(path, "selector names an undeclared component")
  for (name in names(x)) .bh_pf_target_selector(x[[name]], paste0(path, ".", name))
  invisible(TRUE)
}

.bh_pf_formula_annotations <- function(x, path) {
  z <- .bh_pf_record(x, path, c("assign", "contrasts"), required = character())
  for (field in names(z)) .bh_pf_plain_tree(z[[field]], paste0(path, ".", field))
  invisible(TRUE)
}

.bh_pf_formula_axes <- function(x, path) {
  if (!is.null(x)) {
    .bh_pf_formula_strings(x, path, missing = TRUE, empty = TRUE)
    if (length(x) != 2L) .bh_pf_fail(path, "input axis names must be NULL or two captured character names")
  }
  invisible(TRUE)
}

.bh_pf_formula_matrix <- function(x, path, minimum_columns = 1L,
                                  rows = NULL, columns = NULL, labeled = TRUE) {
  d <- attr(x, "dim", exact = TRUE)
  if (!typeof(x) %in% c("integer", "double") || !is.null(attr(x, "class", exact = TRUE)) || length(d) != 2L ||
      anyNA(d) || any(d != floor(d)) || d[1L] < 1L || d[2L] < minimum_columns ||
      length(x) != prod(d) || any(!is.finite(x)) ||
      (!is.null(rows) && d[1L] != rows) || (!is.null(columns) && d[2L] != columns))
    .bh_pf_fail(path, "malformed finite captured matrix or dimensions")
  labels <- attr(x, "dimnames", exact = TRUE)
  if (labeled) {
    if (typeof(labels) != "list" || length(labels) != 2L)
      .bh_pf_fail(path, "captured matrix requires row and column labels")
    .bh_pf_formula_strings(labels[[1L]], paste0(path, ".row_labels"), unique = TRUE)
    if (length(labels[[1L]]) != d[1L]) .bh_pf_fail(path, "matrix row labels do not match dimensions")
    if (d[2L]) {
      .bh_pf_formula_strings(labels[[2L]], paste0(path, ".column_labels"), unique = TRUE)
      if (length(labels[[2L]]) != d[2L]) .bh_pf_fail(path, "matrix column labels do not match dimensions")
    } else if (!is.null(labels[[2L]])) .bh_pf_fail(path, "zero-column captured matrix has no column labels")
  }
  invisible(TRUE)
}

.bh_pf_formula_basis <- function(x, path) {
  fields <- c("targets", "basis", "input_row_labels", "input_row_index", "target_keys",
              "input_annotations", "input_axis_names", "loading", "diagnostics", "version", "fingerprint")
  z <- .bh_pf_record(x, path, fields, classes = "bhmm_basis_component")
  .bh_pf_target_selector(z$targets, paste0(path, ".targets"))
  n <- length(z$targets)
  .bh_pf_formula_matrix(z$basis, paste0(path, ".basis"), rows = n)
  .bh_pf_formula_strings(z$input_row_labels, paste0(path, ".input_row_labels"), unique = TRUE)
  .bh_pf_formula_indices(z$input_row_index, paste0(path, ".input_row_index"), n, unique = TRUE, count = n)
  if (length(z$input_row_labels) != n ||
      !identical(.bh_pf_formula_vector(z$input_row_labels[z$input_row_index]), attr(z$basis, "dimnames", exact = TRUE)[[1L]]))
    .bh_pf_fail(path, "basis input row labels/index do not describe the retained row order")
  .bh_pf_formula_strings(z$target_keys, paste0(path, ".target_keys"), unique = TRUE)
  if (!identical(z$target_keys, .bh_pf_formula_target_keys(z$targets)))
    .bh_pf_fail(path, "basis target keys do not match captured descriptors")
  kinds <- vapply(z$targets, function(a) a$kind, character(1))
  if (length(base::unique.default(kinds)) != 1L) .bh_pf_fail(path, "basis targets use mixed binding syntaxes")
  .bh_pf_formula_annotations(z$input_annotations, paste0(path, ".input_annotations"))
  .bh_pf_formula_axes(z$input_axis_names, paste0(path, ".input_axis_names"))
  .bh_pf_formula_string(z$loading, paste0(path, ".loading"), c("shared_positive_amplitude", "decomposition_only"))
  .bh_pf_formula_string(z$version, paste0(path, ".version"), "numeric_basis_shared_v1")
  .bh_pf_formula_string(z$fingerprint, paste0(path, ".fingerprint"))
  d <- z$diagnostics
  computed <- typeof(d) == "list" && identical(.bh_pf_formula_get(d, "status"), "computed")
  df <- c("status", "rank", "relative_tolerance", "condition", if (computed) "scope")
  d <- .bh_pf_record(d, paste0(path, ".diagnostics"), df)
  .bh_pf_formula_string(d$status, paste0(path, ".diagnostics.status"), c("computed", "not_assessed_size_limit"))
  if (!identical(d$relative_tolerance, 1e-8)) .bh_pf_fail(path, "unrecognized basis diagnostic tolerance")
  size_computed <- max(.bh_pf_formula_dim(z$basis)) <= 200L && length(z$basis) <= 20000L
  if (computed != size_computed) .bh_pf_fail(path, "basis diagnostic schema contradicts the captured size policy")
  if (computed) {
    .bh_pf_formula_number(d$rank, paste0(path, ".diagnostics.rank"), 0L, min(.bh_pf_formula_dim(z$basis)), integer = TRUE)
    .bh_pf_formula_number(d$condition, paste0(path, ".diagnostics.condition"), 0, finite = FALSE)
    .bh_pf_formula_string(d$scope, paste0(path, ".diagnostics.scope"),
      "diagnostic scaling only; captured numeric entries and structural zeros are unchanged")
  } else if (!identical(d$rank, NA_integer_) || !identical(d$condition, NA_real_))
    .bh_pf_fail(path, "unassessed basis diagnostics must retain missing rank and condition")
  invisible(TRUE)
}

.bh_pf_formula_matrix_collection <- function(x, path, role) {
  if (typeof(x) != "list" || is.null(.bh_pf_formula_get(x, "mode"))) .bh_pf_fail(path, "missing captured matrix mode")
  simple <- if (role == "design") "intercept" else "helmert"
  if (identical(.bh_pf_formula_get(x, "mode"), simple)) {
    .bh_pf_record(x, path, "mode")
    return(invisible(TRUE))
  }
  z <- .bh_pf_record(x, path, c("mode", "matrices"))
  .bh_pf_formula_string(z$mode, paste0(path, ".mode"), c("matrix", "strata"))
  .bh_pf_formula_sequence(z$matrices, paste0(path, ".matrices"), minimum = 1L, named = z$mode == "strata")
  if (z$mode == "matrix" && length(z$matrices) != 1L) .bh_pf_fail(path, "matrix mode stores exactly one matrix")
  for (i in seq_along(z$matrices)) {
    p <- paste0(path, ".matrices[[", i, "]]")
    m <- .bh_pf_record(z$matrices[[i]], p, c("value", "annotations", "axis_names"))
    .bh_pf_formula_matrix(m$value, paste0(p, ".value"), minimum_columns = if (role == "contrast") 0L else 1L)
    .bh_pf_formula_annotations(m$annotations, paste0(p, ".annotations"))
    .bh_pf_formula_axes(m$axis_names, paste0(p, ".axis_names"))
  }
  invisible(TRUE)
}

.bh_pf_formula_rank <- function(x, path, role, matrix) {
  d <- .bh_pf_formula_dim(matrix)
  if (typeof(x) != "list") .bh_pf_fail(path, "malformed captured rank diagnostic")
  if ("status" %in% attr(x, "names", exact = TRUE)) {
    z <- .bh_pf_record(x, path, c("rank", "status"))
    if (role != "design" || !identical(z$rank, NA_integer_) || !identical(z$status, "no_coefficients"))
      .bh_pf_fail(path, "unsupported unavailable rank diagnostic")
    return(invisible(TRUE))
  }
  if (.bh_pf_formula_setequal(attr(x, "names", exact = TRUE), c("rank", "relative_tolerance"))) {
    z <- .bh_pf_record(x, path, c("rank", "relative_tolerance"))
    .bh_pf_formula_number(z$rank, paste0(path, ".rank"), 0L, min(d), integer = TRUE)
    if (role != "contrast" || !identical(z$relative_tolerance, 1e-10) || z$rank != d[2L])
      .bh_pf_fail(path, "malformed deterministic contrast rank")
    return(invisible(TRUE))
  }
  fields <- c("rank", "singular_values", "relative_tolerance")
  if (d[2L]) fields <- c("rank", "singular_values", "diagnostic_scale", "relative_tolerance", "scope")
  z <- .bh_pf_record(x, path, fields)
  .bh_pf_formula_number(z$rank, paste0(path, ".rank"), 0L, min(d), integer = TRUE)
  if (z$rank != d[2L] || !identical(z$relative_tolerance, 1e-10) ||
      !typeof(z$singular_values) %in% c("double", "integer") ||
      length(z$singular_values) != min(d) || any(!is.finite(z$singular_values)) || any(z$singular_values < 0))
    .bh_pf_fail(path, "malformed successful captured rank diagnostic")
  if (d[2L]) {
    .bh_pf_formula_number(z$diagnostic_scale, paste0(path, ".diagnostic_scale"), 0)
    .bh_pf_formula_string(z$scope, paste0(path, ".scope"),
      "numerical conditioning threshold, not an exact algebraic invalidity claim")
  }
  invisible(TRUE)
}

.bh_pf_share_strata <- function(x, path) {
  .bh_pf_formula_sequence(x, path, minimum = 1L, named = TRUE)
  for (i in seq_along(x)) {
    p <- paste0(path, "[[", i, "]]")
    fields <- c("owner", "stratum", "targets", "parts", "part_labels", "X", "C",
                "design_diagnostic", "contrast_diagnostic", "coefficient_indices", "contrast_scope", "evaluation_gauge")
    z <- .bh_pf_record(x[[i]], p, fields)
    for (f in c("owner", "stratum")) .bh_pf_formula_string(z[[f]], paste0(p, ".", f))
    for (f in c("targets", "parts", "part_labels")) .bh_pf_formula_strings(z[[f]], paste0(p, ".", f), 1L, unique = f != "part_labels")
    if (length(z$parts) != length(z$part_labels)) .bh_pf_fail(p, "share part labels do not match parts")
    .bh_pf_formula_matrix(z$X, paste0(p, ".X"), rows = length(z$targets))
    .bh_pf_formula_matrix(z$C, paste0(p, ".C"), minimum_columns = 0L, rows = length(z$parts))
    if (.bh_pf_formula_ncol(z$C) > length(z$parts) - 1L ||
        !identical(.bh_pf_formula_rownames(z$X), z$targets) || !identical(.bh_pf_formula_rownames(z$C), z$parts))
      .bh_pf_fail(p, "share design/contrast labels or dimensions do not match their domains")
    .bh_pf_formula_rank(z$design_diagnostic, paste0(p, ".design_diagnostic"), "design", z$X)
    .bh_pf_formula_rank(z$contrast_diagnostic, paste0(p, ".contrast_diagnostic"), "contrast", z$C)
    no_coefficients <- .bh_pf_formula_ncol(z$C) == 0L
    if (no_coefficients != identical(z$design_diagnostic$status, "no_coefficients"))
      .bh_pf_fail(p, "design diagnostic availability contradicts the coefficient domain")
    ci <- z$coefficient_indices
    if (!typeof(ci) %in% c("integer", "double") || !identical(.bh_pf_formula_dim(ci), c(.bh_pf_formula_ncol(z$X), .bh_pf_formula_ncol(z$C))))
      .bh_pf_fail(p, "share coefficient-index matrix dimensions are inconsistent")
    .bh_pf_formula_indices(ci, paste0(p, ".coefficient_indices"), unique = TRUE, matrix = TRUE)
    .bh_pf_formula_string(z$contrast_scope, paste0(p, ".contrast_scope"),
      if (.bh_pf_formula_ncol(z$C) == length(z$parts) - 1L) "full_simplex" else "restricted_logit_subspace")
    .bh_pf_formula_string(z$evaluation_gauge, paste0(p, ".evaluation_gauge"),
      "column-mean offsets removed from C only when evaluating softmax; original C/B/prior preserved")
  }
  invisible(TRUE)
}

.bh_pf_formula_owner <- function(x, path, name, components) {
  fields <- c("owner", "targets", "target_keys", "groups", "residual", "design", "contrast", "contrast_keys", "support")
  z <- .bh_pf_record(x, path, fields)
  .bh_pf_formula_string(z$owner, paste0(path, ".owner"), name)
  .bh_pf_target_selector(z$targets, paste0(path, ".targets"))
  .bh_pf_formula_strings(z$target_keys, paste0(path, ".target_keys"), 1L, unique = TRUE)
  if (!identical(z$target_keys, .bh_pf_formula_target_keys(z$targets))) .bh_pf_fail(path, "owner target keys disagree with selectors")
  .bh_pf_formula_strings(z$groups, paste0(path, ".groups"), unique = TRUE)
  if (any(!z$groups %in% components)) .bh_pf_fail(path, "owner names an undeclared component")
  .bh_pf_formula_flag(z$residual, paste0(path, ".residual"))
  if (!length(z$groups) && !z$residual) .bh_pf_fail(path, "owner has no common or residual part")
  if (!is.null(z$support)) .bh_pf_formula_string(z$support, paste0(path, ".support"), "separate")
  .bh_pf_formula_string(z$contrast_keys, paste0(path, ".contrast_keys"), c("auto", "canonical"))
  .bh_pf_formula_matrix_collection(z$design, paste0(path, ".design"), "design")
  .bh_pf_formula_matrix_collection(z$contrast, paste0(path, ".contrast"), "contrast")
  invisible(TRUE)
}

.bh_pf_formula_decomposition <- function(x, path, components, coordinates) {
  fields <- c("owners", "owned", "parts", "strata", "coefficients", "cells", "required_zeros")
  z <- .bh_pf_record(x, path, fields)
  .bh_pf_formula_sequence(z$owners, paste0(path, ".owners"), 1L, named = TRUE)
  for (name in names(z$owners)) .bh_pf_formula_owner(z$owners[[name]], paste0(path, ".owners.", name), name, components)
  owned_keys <- unlist(lapply(z$owners, function(o) o$target_keys), use.names = FALSE)
  if (base::anyDuplicated.default(owned_keys)) .bh_pf_fail(path, "owner target domains overlap")
  .bh_pf_share_strata(z$strata, paste0(path, ".strata"))
  table_fields <- list(
    owned = c("owner", "target", "parameter", "stratum", "stratum_index", "design_row", "residual"),
    parts = c("owner", "target", "part", "group", "residual", "stratum_index", "part_index"),
    coefficients = c("parameter", "owner", "stratum", "term", "coordinate", "stratum_index"),
    cells = c("owner", "target", "group", "coordinate_index", "multiplier", "part"),
    required_zeros = c("row", "col", "owner"))
  tables <- list()
  for (field in names(table_fields)) {
    p <- paste0(path, ".", field)
    tab <- .bh_pf_table(z[[field]], p, table_fields[[field]])
    tables[[field]] <- tab
    for (column in .bh_ps_intersect(names(tab), c("owner", "target", "parameter", "stratum", "term", "coordinate", "part")))
      .bh_pf_formula_strings(tab[[column]], paste0(p, ".", column))
    if (any(!tab$owner %in% names(z$owners))) .bh_pf_fail(p, "share table refers to an unknown owner")
    if ("target" %in% names(tab) && any(!tab$target %in% owned_keys)) .bh_pf_fail(p, "share table refers to an unowned target")
    if ("stratum_index" %in% names(tab)) .bh_pf_formula_indices(tab$stratum_index, paste0(p, ".stratum_index"), length(z$strata))
    if ("residual" %in% names(tab)) .bh_pf_formula_flags(tab$residual, paste0(p, ".residual"))
  }
  owned <- tables$owned
  if (base::anyDuplicated.default(owned$target) || !.bh_pf_formula_setequal(owned$target, owned_keys) || base::anyDuplicated.default(owned$parameter))
    .bh_pf_fail(path, "owned table must cover each declared target once")
  .bh_pf_formula_indices(owned$design_row, paste0(path, ".owned.design_row"))
  part <- tables$parts
  .bh_pf_formula_strings(part$group, paste0(path, ".parts.group"), missing = TRUE)
  .bh_pf_formula_indices(part$part_index, paste0(path, ".parts.part_index"))
  if (any(part$residual != (part$part == "residual")) || any(is.na(part$group) != part$residual) ||
      any(!part$group[!part$residual] %in% components)) .bh_pf_fail(path, "share part/group/residual fields disagree")
  cell <- tables$cells
  .bh_pf_formula_strings(cell$group, paste0(path, ".cells.group"))
  .bh_pf_formula_indices(cell$coordinate_index, paste0(path, ".cells.coordinate_index"), length(coordinates$index))
  if (!typeof(cell$multiplier) %in% c("integer", "double") || !is.null(attr(cell$multiplier, "class", exact = TRUE)) ||
      !is.null(attr(cell$multiplier, "dim", exact = TRUE)) || any(!is.finite(cell$multiplier)) || any(cell$multiplier == 0) ||
      any(!cell$group %in% components) || any(cell$group != coordinates$group[cell$coordinate_index]))
    .bh_pf_fail(path, "share cell coordinates or nonzero multipliers are malformed")
  zeros <- tables$required_zeros
  .bh_pf_formula_indices(zeros$row, paste0(path, ".required_zeros.row"), length(coordinates$index))
  .bh_pf_formula_indices(zeros$col, paste0(path, ".required_zeros.col"), length(coordinates$index))
  if (any(zeros$row >= zeros$col)) .bh_pf_fail(path, "required-zero rows must describe distinct ordered coordinate pairs")
  coefficient <- tables$coefficients
  if (base::anyDuplicated.default(coefficient$parameter)) .bh_pf_fail(path, "share coefficient identities must be unique")
  for (name in names(z$owners)) {
    o <- z$owners[[name]]
    strata_for_owner <- z$strata[vapply(z$strata, function(s) identical(s$owner, name), logical(1))]
    ids <- vapply(strata_for_owner, function(s) s$stratum, character(1), USE.NAMES = FALSE)
    targets <- unlist(lapply(strata_for_owner, function(s) s$targets), use.names = FALSE)
    if (!length(ids) || base::anyDuplicated.default(ids) || base::anyDuplicated.default(targets) ||
        !.bh_pf_formula_setequal(targets, o$target_keys) || (length(ids) > 1L && !identical(o$support, "separate")))
      .bh_pf_fail(path, "owner strata do not partition the captured target domain under its support mode")
    for (role in c("design", "contrast")) {
      collection <- o[[role]]
      if (identical(collection$mode, "strata") && !.bh_pf_formula_setequal(names(collection$matrices), ids))
        .bh_pf_fail(path, "captured matrix collection does not cover its owner's strata")
    }
    if (identical(o$contrast$mode, "matrix") && length(ids) != 1L)
      .bh_pf_fail(path, "one unkeyed contrast matrix cannot describe multiple strata")
  }
  all_indices <- integer()
  for (i in seq_along(z$strata)) {
    s <- z$strata[[i]]
    if (!s$owner %in% names(z$owners)) .bh_pf_fail(path, "stratum refers to an unknown owner")
    o <- z$owners[[s$owner]]
    if (any(!s$targets %in% o$target_keys)) .bh_pf_fail(path, "stratum target falls outside its owner domain")
    if (identical(o$design$mode, "intercept") &&
        (.bh_pf_formula_ncol(s$X) != 1L || !identical(.bh_pf_formula_colnames(s$X), "Intercept") || any(s$X != 1)))
      .bh_pf_fail(path, "intercept share design must retain its one constant column")
    deterministic_contrast <- .bh_pf_formula_setequal(names(s$contrast_diagnostic), c("rank", "relative_tolerance"))
    if (deterministic_contrast != identical(o$contrast$mode, "helmert"))
      .bh_pf_fail(path, "contrast diagnostic variant disagrees with the owner's captured contrast mode")
    at <- which(owned$stratum_index == i)
    if (!identical(owned$target[at], s$targets) || any(owned$owner[at] != s$owner) ||
        any(owned$stratum[at] != s$stratum) || any(owned$residual[at] != o$residual) ||
        !identical(as.integer(owned$design_row[at]), seq_along(s$targets)))
      .bh_pf_fail(path, "owned rows disagree with their captured stratum")
    for (target in s$targets) {
      rows <- which(part$target == target)
      if (!identical(part$part[rows], s$parts) || any(part$owner[rows] != s$owner) ||
          any(part$stratum_index[rows] != i) || !identical(as.integer(part$part_index[rows]), seq_along(s$parts)))
        .bh_pf_fail(path, "share part rows do not cover each target's stratum parts in order")
    }
    indices <- .bh_pf_formula_vector(s$coefficient_indices)
    .bh_pf_formula_indices(indices, paste0(path, ".strata[[", i, "]].coefficient_indices"), length(coefficient$parameter))
    if (length(indices) && (any(coefficient$stratum_index[indices] != i) ||
        any(coefficient$owner[indices] != s$owner) || any(coefficient$stratum[indices] != s$stratum) ||
        !identical(coefficient$term[indices], rep(.bh_pf_formula_colnames(s$X), .bh_pf_formula_ncol(s$C))) ||
        !identical(coefficient$coordinate[indices], rep(.bh_pf_formula_colnames(s$C), each = .bh_pf_formula_ncol(s$X)))))
      .bh_pf_fail(path, "coefficient rows disagree with their retained matrix indices")
    all_indices <- c(all_indices, indices)
  }
  if (!identical(as.integer(all_indices), seq_along(coefficient$parameter)))
    .bh_pf_fail(path, "share coefficient indices must cover the retained rows once in producer order")
  invisible(TRUE)
}

.bh_pf_formula_loadings <- function(x, path, components) {
  z <- .bh_pf_record(x, path, c("fixed", "equal", "version"))
  .bh_pf_formula_string(z$version, paste0(path, ".version"), "ordinary_loading_constraints_v1")
  .bh_pf_formula_sequence(z$fixed, paste0(path, ".fixed"))
  .bh_pf_formula_sequence(z$equal, paste0(path, ".equal"), named = TRUE)
  if (!length(z$fixed) && !length(z$equal)) .bh_pf_fail(path, "captured loading constraints cannot be empty")
  for (i in seq_along(z$fixed)) {
    p <- paste0(path, ".fixed[[", i, "]]")
    r <- .bh_pf_record(z$fixed[[i]], p, c("value", "cells"))
    .bh_pf_formula_number(r$value, paste0(p, ".value"))
    .bh_pf_component_selectors(r$cells, paste0(p, ".cells"), components)
  }
  for (name in names(z$equal)) {
    .bh_pf_component_selectors(z$equal[[name]], paste0(path, ".equal.", name), components)
    if (sum(lengths(z$equal[[name]])) < 2L) .bh_pf_fail(path, "a loading equality class needs at least two cells")
  }
  invisible(TRUE)
}

.bh_pf_formula_migration <- function(x, path) {
  z <- .bh_pf_record(x, path, c("kind", "from", "to", "source_fingerprint", "operation"))
  .bh_pf_formula_string(z$kind, paste0(path, ".kind"), "formula")
  .bh_pf_formula_string(z$from, paste0(path, ".from"), c("scalar-components-v1", "scalar-components-residual-v1"))
  .bh_pf_formula_string(z$to, paste0(path, ".to"), "loading-expression-components-v2")
  .bh_pf_formula_string(z$source_fingerprint, paste0(path, ".source_fingerprint"))
  .bh_pf_formula_string(z$operation, paste0(path, ".operation"),
    "scalar coordinates added; target membership and resolved correlation graph unchanged")
  invisible(TRUE)
}

.bh_pf_formula_correlation <- function(x, path, count, labels = NULL, ownership = FALSE) {
  .bh_portable_scientific_schema(x, path)
  affine <- !is.null(attr(x, "affine_rules", exact = TRUE))
  absent_endpoints<-!c("component1","component2")%in%.bh_pf_names(x)
  if(any(absent_endpoints)&&(!all(absent_endpoints)||count!=0L))
    .bh_pf_fail(path,"endpoint columns may be absent only in the exact zero-coordinate producer table")
  fields <- c("row", "col", if(!all(absent_endpoints))c("component1", "component2"), "state", "source", "parameter",
    if (affine) c("constraint_group", "fixed_value", "equality_labels", "constraint_sources"),
    if (ownership) "required_by")
  z <- .bh_pf_table(x, path, fields)
  for (field in c("row", "col")) .bh_pf_formula_indices(z[[field]], paste0(path, ".", field), count)
  for (field in c(if(!all(absent_endpoints))c("component1", "component2"), "state", "source", "parameter"))
    .bh_pf_formula_strings(z[[field]], paste0(path, ".", field), missing = field == "parameter")
  if (length(z$row) != count * (count - 1) / 2)
    .bh_pf_fail(path, "formula correlation table does not cover its complete coordinate-pair domain")
  expected_row <- expected_col <- integer()
  if (count > 1L) {
    left <- seq_len(count - 1L)
    expected_row <- rep(left, count - left)
    expected_col <- unlist(lapply(left, function(i) seq.int(i + 1L, count)), use.names = FALSE)
  }
  if (!identical(as.integer(z$row), expected_row) || !identical(as.integer(z$col), expected_col))
    .bh_pf_fail(path, "formula correlation row/column pairs do not match the captured producer order")
  if (is.null(labels)) {
    # Residual endpoint labels are original opaque target-descriptor seals.
    # Check their positional consistency; never recompute those runtime seals.
    endpoints <- c(z$component1, z$component2)
    indices <- c(z$row, z$col)
    for (i in seq_len(count)) {
      values <- base::unique.default(endpoints[indices == i])
      if (count > 1L && (length(values) != 1L || is.na(values) || !nzchar(values)))
        .bh_pf_fail(path, "residual endpoint seals are inconsistent across pairs")
    }
    if (count > 1L && length(base::unique.default(endpoints)) != count)
      .bh_pf_fail(path, "residual endpoint seals must identify distinct captured targets")
  } else if (!all(absent_endpoints) && (!identical(z$component1, labels[z$row]) || !identical(z$component2, labels[z$col])))
    .bh_pf_fail(path, "formula correlation endpoint labels do not match coordinate order")
  if (affine) {
    base <- unclass(attr(x, "affine_rules", exact = TRUE)$base)
    for (field in c("row", "col", "component1", "component2"))
      if (!identical(base[[field]], z[[field]])) .bh_pf_fail(path, "affine base and resolved correlation pair domains disagree")
  }
  invisible(TRUE)
}

.bh_portable_formula_schema <- function(x, path) {
  core <- c("components", "correlations", "component_names", "coordinates", "person", "scale", "residual", "version", "fingerprint")
  optional <- c("decomposition", "loadings", "correlation_coordinates", "augmentation", "reference", "schema_migration")
  legacy_class <- identical(attr(x, "class", exact = TRUE), "bhmm_formula")
  z <- .bh_pf_record(x, path, c(core, optional), required = core,
    classes = if (legacy_class) "bhmm_formula" else c("bmeas_formula", "bhmm_formula"))
  if (legacy_class && !"schema_migration" %in% names(z))
    .bh_pf_fail(path, "a retained legacy formula class requires its explicit formula migration record")
  versions <- c("loading-expression-components-v2", "variance-share-components-v1", "ordinary-loading-constraints-v1",
                "marker-scale-components-v1", "reference-law-components-v1", "affine-correlation-components-v1")
  .bh_pf_formula_string(z$version, paste0(path, ".version"), versions)
  .bh_pf_formula_string(z$fingerprint, paste0(path, ".fingerprint"))
  for (field in .bh_ps_intersect(names(z), optional)) if (is.null(z[[field]]))
    .bh_pf_fail(paste0(path, ".", field), "this producer omits optional NULL fields")
  .bh_pf_formula_sequence(z$components, paste0(path, ".components"), named = TRUE)
  if (!identical(z$component_names, names(z$components))) .bh_pf_fail(path, "component_names disagree with component record names")
  component_targets <- list()
  for (name in names(z$components)) {
    p <- paste0(path, ".components.", name)
    c <- z$components[[name]]
    if (identical(attr(c, "class", exact = TRUE), "bhmm_basis_component")) {
      .bh_pf_formula_basis(c, p)
      component_targets[[name]] <- unclass(c)$targets
    } else {
      .bh_pf_target_selector(c, p)
      component_targets[[name]] <- c
    }
  }
  .bh_portable_scientific_schema(z$coordinates, paste0(path, ".coordinates"))
  coordinates <- .bh_pf_table(z$coordinates, paste0(path, ".coordinates"),
    c("group", "group_index", "kind", "coordinate", "display", "id", "index"))
  for (field in c("group", "kind", "coordinate", "display", "id"))
    .bh_pf_formula_strings(coordinates[[field]], paste0(path, ".coordinates.", field), missing = field == "coordinate")
  for (field in c("group_index", "index"))
    .bh_pf_formula_indices(coordinates[[field]], paste0(path, ".coordinates.", field))
  k <- length(coordinates$index)
  expected_group <- expected_kind <- expected_label <- character()
  expected_index <- integer()
  for (i in seq_along(z$components)) {
    c <- z$components[[i]]
    basis <- identical(attr(c, "class", exact = TRUE), "bhmm_basis_component")
    labels <- if (basis) .bh_pf_formula_colnames(unclass(c)$basis) else NA_character_
    expected_group <- c(expected_group, rep(names(z$components)[i], length(labels)))
    expected_index <- c(expected_index, rep(i, length(labels)))
    expected_kind <- c(expected_kind, rep(if (basis) "basis" else "scalar", length(labels)))
    expected_label <- c(expected_label, labels)
  }
  if (!identical(coordinates$group, expected_group) || !identical(as.integer(coordinates$group_index), expected_index) ||
      !identical(coordinates$kind, expected_kind) || !identical(coordinates$coordinate, expected_label) ||
      !identical(as.integer(coordinates$index), seq_len(k)))
    .bh_pf_fail(path, "coordinate rows do not match captured component shapes and order")
  common_affine <- !is.null(attr(z$correlations, "affine_rules", exact = TRUE))
  decomposition <- "decomposition" %in% names(z)
  .bh_pf_formula_correlation(z$correlations, paste0(path, ".correlations"), k,
    labels = coordinates$display, ownership = decomposition)
  if (decomposition) .bh_pf_formula_decomposition(z$decomposition, paste0(path, ".decomposition"), names(z$components), coordinates)
  if (("required_by" %in% attr(z$correlations, "names", exact = TRUE)) != decomposition)
    .bh_pf_fail(path, "common-correlation ownership fields disagree with decomposition presence")
  residual_affine <- FALSE
  if (!identical(z$residual, "none")) {
    fields <- c("targets", "correlations", if (decomposition) "provenance")
    r <- .bh_pf_record(z$residual, paste0(path, ".residual"), fields)
    .bh_pf_target_selector(r$targets, paste0(path, ".residual.targets"), names_metadata = decomposition)
    .bh_pf_formula_correlation(r$correlations, paste0(path, ".residual.correlations"), length(r$targets))
    residual_affine <- !is.null(attr(r$correlations, "affine_rules", exact = TRUE))
    if (decomposition) {
      p <- paste0(path, ".residual.provenance")
      fields <- c("target", "global_declared", "owner_declared", "owner", "correlation_background", "correlation_scope", "correlation_explicit", "declaration_source")
      provenance <- .bh_pf_table(r$provenance, p, fields)
      .bh_pf_formula_strings(provenance$target, paste0(p, ".target"), unique = TRUE)
      if (!identical(provenance$target, .bh_pf_formula_target_keys(r$targets))) .bh_pf_fail(p, "residual provenance target order differs from declaration")
      for (f in c("global_declared", "owner_declared", "correlation_explicit"))
        .bh_pf_formula_flags(provenance[[f]], paste0(p, ".", f))
      .bh_pf_formula_strings(provenance$owner, paste0(p, ".owner"), missing = TRUE)
      for (f in c("correlation_background", "correlation_scope", "declaration_source"))
        .bh_pf_formula_strings(provenance[[f]], paste0(p, ".", f))
      if (any(is.na(provenance$owner) == provenance$owner_declared) ||
          any(!provenance$owner[provenance$owner_declared] %in% names(z$decomposition$owners)) ||
          any(!provenance$correlation_background %in% c("zero", "free")) ||
          any(provenance$correlation_scope != "resolved_global_and_owner_residual_union"))
        .bh_pf_fail(p, "residual provenance ownership or correlation scope is malformed")
      expected <- ifelse(provenance$global_declared & provenance$owner_declared, "global_and_owner",
        ifelse(provenance$global_declared, "global", "owner"))
      if (any(!provenance$global_declared & !provenance$owner_declared) || !identical(provenance$declaration_source, expected))
        .bh_pf_fail(p, "residual declaration sources disagree with their indicators")
    }
  }
  marker <- !identical(z$scale, "unit_variance")
  if (marker) {
    .bh_pf_formula_sequence(z$scale, paste0(path, ".scale"), 1L, named = TRUE)
    if (any(!names(z$scale) %in% names(z$components))) .bh_pf_fail(path, "marker scale names an unknown component")
    for (name in names(z$scale)) {
      p <- paste0(path, ".scale.", name)
      rule <- .bh_pf_record(z$scale[[name]], p, c("marker", "value"))
      .bh_pf_target_selector(rule$marker, paste0(p, ".marker"))
      .bh_pf_formula_number(rule$value, paste0(p, ".value"))
      if (length(rule$marker) != 1L || rule$value == 0 ||
          !.bh_pf_formula_target_keys(rule$marker) %in% .bh_pf_formula_target_keys(component_targets[[name]]) ||
          identical(attr(z$components[[name]], "class", exact = TRUE), "bhmm_basis_component"))
        .bh_pf_fail(p, "marker must name one declared ordinary scalar cell with nonzero value")
    }
  }
  if ("loadings" %in% names(z)) .bh_pf_formula_loadings(z$loadings, paste0(path, ".loadings"), names(z$components))
  centered <- !identical(z$person, "iid")
  if (centered) {
    person <- .bh_pf_record(z$person, paste0(path, ".person"), c("distribution", "ids"))
    .bh_pf_formula_string(person$distribution, paste0(path, ".person.distribution"), "sample_centered")
    .bh_pf_formula_strings(person$ids, paste0(path, ".person.ids"), 2L, unique = TRUE)
    if (any(grepl("::", person$ids, fixed = TRUE))) .bh_pf_fail(path, "person IDs contain the key delimiter")
  }
  augmented <- "augmentation" %in% names(z)
  raw <- "correlation_coordinates" %in% names(z)
  reference <- "reference" %in% names(z)
  if (centered != augmented || reference != (raw || augmented))
    .bh_pf_fail(path, "person/correlation-coordinate/augmentation fields form an unsupported combination")
  if (reference) .bh_pf_formula_string(z$reference, paste0(path, ".reference"), "explicit_person_correlation_augmentation_v1")
  if (augmented) {
    a <- .bh_pf_record(z$augmentation, paste0(path, ".augmentation"), c("type", "targets", "order_source"))
    .bh_pf_formula_string(a$type, paste0(path, ".augmentation.type"), "task_cholesky")
    source <- if (is.null(a$targets)) "canonical_symbolic_support" else if (!length(a$targets)) "explicit_empty" else "explicit_target_order"
    .bh_pf_formula_string(a$order_source, paste0(path, ".augmentation.order_source"), source)
    if (!is.null(a$targets)) .bh_pf_target_selector(a$targets, paste0(path, ".augmentation.targets"), allow_empty = TRUE)
  }
  if (raw) {
    p <- paste0(path, ".correlation_coordinates")
    r <- .bh_pf_record(z$correlation_coordinates, p, c("type", "blocks", "ordered_indices"))
    .bh_pf_formula_string(r$type, paste0(p, ".type"), "ordered_blocks")
    .bh_pf_formula_sequence(r$blocks, paste0(p, ".blocks"), named = TRUE)
    for (name in names(r$blocks)) {
      .bh_pf_formula_indices(r$blocks[[name]], paste0(p, ".blocks.", name), k, unique = TRUE)
      if (!length(r$blocks[[name]])) .bh_pf_fail(p, "ordered correlation blocks cannot be empty")
    }
    .bh_pf_formula_indices(r$ordered_indices, paste0(p, ".ordered_indices"), k, unique = TRUE, count = k)
    expected <- as.integer(unlist(r$blocks, use.names = FALSE))
    if (!identical(r$ordered_indices, expected) || !.bh_pf_formula_setequal(r$ordered_indices, seq_len(k)) || common_affine)
      .bh_pf_fail(p, "ordered correlation blocks must partition coordinates and cannot accompany common affine rules")
    cor <- unclass(z$correlations)
    membership <- integer(k)
    for (i in seq_along(r$blocks)) membership[r$blocks[[i]]] <- i
    if (any(cor$state != ifelse(membership[cor$row] == membership[cor$col], "zero", "free")))
      .bh_pf_fail(p, "ordered correlation blocks disagree with the retained common graph")
  }
  expected_version <- if (common_affine || residual_affine) "affine-correlation-components-v1" else
    if (reference) "reference-law-components-v1" else if (marker) "marker-scale-components-v1" else
      if ("loadings" %in% names(z)) "ordinary-loading-constraints-v1" else
        if (decomposition) "variance-share-components-v1" else "loading-expression-components-v2"
  if (!identical(z$version, expected_version)) .bh_pf_fail(path, "formula version contradicts its retained feature records")
  if ("schema_migration" %in% names(z)) {
    .bh_pf_formula_migration(z$schema_migration, paste0(path, ".schema_migration"))
    if (z$version != "loading-expression-components-v2" || any(coordinates$kind != "scalar"))
      .bh_pf_fail(path, "scalar formula migration record accompanies an incompatible formula schema")
  }
  invisible(TRUE)
}
# Static schemas traced from retained R producers, checkpoint-release-2.
# These validators inspect stored records only: no construction, normalization,
# hashing, graph solving, density evaluation, or scientific reassessment occurs.
.bh_ps_dim <- function(x) attr(x,"dim",exact=TRUE)
.bh_ps_setequal <- function(x,y) all(match(x,y,nomatch=0L)>0L)&&all(match(y,x,nomatch=0L)>0L)
.bh_ps_intersect <- function(x,y) {x<-base::unique.default(x);x[match(x,y,nomatch=0L)>0L]}
.bh_ps_setdiff <- function(x,y) {x<-base::unique.default(x);x[match(x,y,nomatch=0L)==0L]}
.bh_ps_vec <- function(x, path, type="c", size=NULL) {
  good <- switch(type, c=typeof(x)=="character", C=typeof(x)=="character",
    n=typeof(x)%in%c("integer","double"), N=typeof(x)%in%c("integer","double"),
    i=typeof(x)%in%c("integer","double"), I=typeof(x)%in%c("integer","double"),
    b=typeof(x)=="logical", B=typeof(x)=="logical", z=typeof(x)=="logical"&&length(x)==0L, FALSE)
  if(!good || !is.null(attr(x,"class",exact=TRUE)) || !is.null(attr(x,"dim",exact=TRUE)) ||
     (!is.null(size)&&length(x)!=size)) .bh_pf_fail(path,"invalid scientific scalar/vector shape or storage type")
  if(type%in%c("c","n","i","b") && anyNA(x)) .bh_pf_fail(path,"missing scientific value is not admitted here")
  if(type%in%c("n","i") && any(!is.finite(x))) .bh_pf_fail(path,"expected finite scientific numeric values")
  if(type%in%c("i","I") && any(!is.na(x)&(!is.finite(x)|x!=floor(x)))) .bh_pf_fail(path,"expected integral scientific indices")
  invisible(TRUE)
}
.bh_ps_matrix <- function(x,path,type="n",square=FALSE,binary=FALSE) {
  d<-attr(x,"dim",exact=TRUE)
  if(!typeof(x)%in%(if(type=="c")"character"else c("integer","double")) || length(d)!=2L || anyNA(d) || any(d<0L) ||
     (square&&d[1L]!=d[2L])) .bh_pf_fail(path,"invalid scientific matrix shape or storage type")
  x<-unclass(x)
  if(type=="n"&&any(!is.finite(x))) .bh_pf_fail(path,"scientific matrix must contain finite numeric values")
  if(type=="c"&&anyNA(x)) .bh_pf_fail(path,"scientific character matrix cannot contain missing cells")
  if(binary&&any(!x%in%c(0,1))) .bh_pf_fail(path,"expected binary sign-system matrix")
  invisible(TRUE)
}
.bh_ps_fields <- function(x,path,schemas) {
  n<-attr(x,"names",exact=TRUE)
  if(!any(vapply(schemas,function(s)length(n)==length(s)&&.bh_ps_setequal(n,names(s)),logical(1))))
    .bh_pf_fail(path,"unsupported scientific field/column combination")
  schema<-schemas[[which(vapply(schemas,function(s)length(n)==length(s)&&.bh_ps_setequal(n,names(s)),logical(1)))[1L]]]
  z<-.bh_pf_table(x,path,names(schema))
  for(k in names(schema))if(schema[[k]]!="l") .bh_ps_vec(z[[k]],paste0(path,".",k),schema[[k]])
  z
}
.bh_ps_enum <- function(x,values,path,missing=FALSE) {
  if((!missing&&anyNA(x))||any(!is.na(x)&!x%in%values)) .bh_pf_fail(path,"unsupported scientific category")
  invisible(TRUE)
}
.bh_ps_positive_index <- function(x,path,zero=FALSE,missing=FALSE) {
  .bh_ps_vec(x,path,if(missing)"I"else"i")
  if(any(!is.na(x)&x<if(zero)0 else 1)) .bh_pf_fail(path,"scientific index is outside its declared range")
  invisible(TRUE)
}
.bh_ps_unique <- function(x,path) {
  if(anyNA(x)||base::anyDuplicated.default(x)) .bh_pf_fail(path,"scientific key is missing or duplicated")
  invisible(TRUE)
}
.bh_ps_list <- function(x,path,named=FALSE) {
  if(typeof(x)!="list") .bh_pf_fail(path,"expected a scientific list")
  y<-unclass(x);n<-names(y)
  if(named&&length(y)&&(is.null(n)||anyNA(n)||any(!nzchar(n))||base::anyDuplicated.default(n))) .bh_pf_fail(path,"expected distinct named scientific records")
  y
}
.bh_ps_string_list <- function(x,path,integer=FALSE) {
  y<-.bh_ps_list(x,path)
  for(i in seq_along(y)) .bh_ps_vec(y[[i]],paste0(path,"[[",i,"]]"),if(integer)"i"else"c")
  invisible(TRUE)
}
.bh_ps_pair_capture <- function(x,path) {
  z<-.bh_pf_record(x,path,c("expression_text","endpoints","indices","capture_mode"))
  .bh_ps_vec(z$expression_text,paste0(path,".expression_text"),"c",1L)
  .bh_ps_vec(z$endpoints,paste0(path,".endpoints"),"c",2L)
  .bh_ps_positive_index(z$indices,paste0(path,".indices"))
  if(length(z$indices)!=2L||z$indices[1L]>=z$indices[2L]||base::anyDuplicated.default(z$endpoints)||
     !identical(z$capture_mode,"resolved_symbolic_text_without_environment_or_AST_annotations")) .bh_pf_fail(path,"malformed resolved correlation selector")
  invisible(TRUE)
}
.bh_ps_affine_rules <- function(x,path) {
  z<-.bh_pf_record(x,path,c("base","equal","pins","explicit_free","rules"))
  if(!is.null(attr(z$base,"affine_rules",exact=TRUE))) .bh_pf_fail(path,"affine base cannot recursively contain affine rules")
  .bh_portable_scientific_schema(z$base,paste0(path,".correlations"))
  b<-unclass(z$base);n<-length(b$row)
  equal<-.bh_ps_list(z$equal,paste0(path,".equal"))
  for(i in seq_along(equal)) {
    p<-paste0(path,".equal[[",i,"]]");q<-.bh_pf_record(equal[[i]],p,c("label","edges"))
    .bh_ps_vec(q$label,paste0(p,".label"),"c",1L);.bh_ps_positive_index(q$edges,paste0(p,".edges"))
    if(length(q$edges)<2L||base::anyDuplicated.default(q$edges)||any(q$edges>n)) .bh_pf_fail(p,"correlation equality needs distinct existing edges")
  }
  pins<-.bh_ps_fields(z$pins,paste0(path,".pins"),list(c(edge="i",value="n",source="c")))
  .bh_ps_positive_index(pins$edge,paste0(path,".pins.edge"))
  if(any(pins$edge>n)||any(abs(pins$value)>1)) .bh_pf_fail(path,"invalid fixed correlation edge or value")
  .bh_ps_positive_index(z$explicit_free,paste0(path,".explicit_free"))
  if(any(z$explicit_free>n)||base::anyDuplicated.default(z$explicit_free)) .bh_pf_fail(path,"invalid explicit free correlation edges")
  a<-c(rule="c",kind="c",label="C",value="N",edge="i")
  rules<-.bh_ps_fields(z$rules,paste0(path,".rules"),list(a,c(a,captured_selector="l")))
  if(!"captured_selector"%in%names(rules)&&length(rules$rule)) .bh_pf_fail(path,"nonempty correlation rules need captured selectors")
  .bh_ps_enum(rules$kind,c("equal","fixed"),paste0(path,".rules.kind"))
  if(any(rules$edge<1L|rules$edge>n)) .bh_pf_fail(path,"correlation rules reference absent edges")
  if("captured_selector"%in%names(rules))for(i in seq_along(rules$captured_selector)) .bh_ps_pair_capture(rules$captured_selector[[i]],paste0(path,".rules.captured_selector[[",i,"]]"))
  invisible(TRUE)
}

.bh_ps_table_schemas <- function() list(
  parameter_roles=list(c("model"="c","parameter"="c","task"="c","key"="c","predictor"="c","receives"="c","support"="c","units"="c","observation_units"="c","predictor_units"="c","role"="c","level"="c","level_source"="c","received_units"="c","predictor_representation"="c","natural_representation"="c","supplied_scale"="C","supplied_units"="C","supplied_value"="N","fixed_received"="N","fixed_predictor"="N","fixed_natural"="N","free_parameter"="b","individual_variation"="b")),
  fixed_parameters=list(c("model"="c","parameter"="c","task"="c","key"="c","predictor"="c","receives"="c","support"="c","units"="c","observation_units"="c","predictor_units"="c","role"="c","level"="c","level_source"="c","received_units"="c","predictor_representation"="c","natural_representation"="c","supplied_scale"="C","supplied_units"="C","supplied_value"="N","fixed_received"="N","fixed_predictor"="N","fixed_natural"="N","free_parameter"="b","individual_variation"="b")),
  observation_parameters=list(c("model"="c","parameter"="c","task"="c","key"="c","predictor"="c","receives"="c","support"="c","units"="c","observation_units"="c","predictor_units"="c","role"="c","level"="c","level_source"="c","received_units"="c","predictor_representation"="c","natural_representation"="c","supplied_scale"="C","supplied_units"="C","supplied_value"="N","fixed_received"="N","fixed_predictor"="N","fixed_natural"="N","free_parameter"="b","individual_variation"="b")),
  targets=list(c("model"="c","parameter"="c","task"="c","key"="c","predictor"="c","receives"="c","support"="c","units"="c","observation_units"="c","predictor_units"="c","role"="c","level"="c","level_source"="c","received_units"="c","predictor_representation"="c","natural_representation"="c","supplied_scale"="C","supplied_units"="C","supplied_value"="N","fixed_received"="N","fixed_predictor"="N","fixed_natural"="N","free_parameter"="b","individual_variation"="b","target_index"="i")),
  individual_targets=list(c("model"="c","parameter"="c","task"="c","key"="c","predictor"="c","receives"="c","support"="c","units"="c","observation_units"="c","predictor_units"="c","role"="c","level"="c","level_source"="c","received_units"="c","predictor_representation"="c","natural_representation"="c","supplied_scale"="C","supplied_units"="C","supplied_value"="N","fixed_received"="N","fixed_predictor"="N","fixed_natural"="N","free_parameter"="b","individual_variation"="b","target_index"="i")),
  components=list(c("component"="c","orientation"="c","variance_expression"="c","score_definition"="c","mean"="n","standardized_variance"="n","variance"="N","sd_parameter"="C")),
  loadings=list(c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C")),
  free_loadings=list(c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","units"="c","members"="c","support"="c","orientation_reason"="c"),c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","units"="c","members"="c","support"="c","orientation_reason"="c","sign_character"="l")),
  loading_primitives=list(c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","units"="c","members"="c","support"="c","orientation_reason"="c"),c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","units"="c","members"="c","support"="c","orientation_reason"="c","sign_character"="l")),
  amplitudes=list(c("component"="c","parameter"="c","units"="c","amplitude_index"="i")),
  loading_coefficients=list(c("parameter"="c","role"="c","component"="c","support"="c","target"="C","prior_source"="C","coefficient_index"="i")),
  loading_map=list(c("target"="c","group"="c","coordinate"="c","coordinate_id"="c","role"="c","effective_expression"="c","target_index"="i","coordinate_index"="i","coefficient"="C","component_sd_parameter"="C","multiplier"="n","coefficient_index"="I","fixed_value"="N"),c("target"="c","group"="c","coordinate"="c","coordinate_id"="c","role"="c","effective_expression"="c","target_index"="i","coordinate_index"="i","coefficient"="C","component_sd_parameter"="C","multiplier"="n","coefficient_index"="I","fixed_value"="N","owner"="C","part"="C","stratum"="C","expression"="c")),
  loading_cells=list(c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","constraint_class"="C","state"="c","equality_labels"="c","constraint_provenance"="c","units"="c","group"="c","expression"="c","effective_expression"="c","fixed_value"="N","primitive_index"="I","effective_support"="b","owner"="C","component_sd_parameter"="C")),
  ordinary_loading_map=list(c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","constraint_class"="C","state"="c","equality_labels"="c","constraint_provenance"="c","units"="c","group"="c","expression"="c","effective_expression"="c","fixed_value"="N","primitive_index"="I","effective_support"="b","owner"="C","component_sd_parameter"="C")),
  loading_prior_aliases=list(c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","constraint_class"="C","state"="c","equality_labels"="c","constraint_provenance"="c","units"="c","group"="c","expression"="c","effective_expression"="c","fixed_value"="N","primitive_index"="I","effective_support"="b","owner"="C","component_sd_parameter"="C"),c("target"="c","component"="c","sign"="c","target_index"="i","component_index"="i","parameter"="C","cell_id"="c","constraint_class"="C","state"="c","equality_labels"="c","constraint_provenance"="c","units"="c","group"="c","expression"="c","effective_expression"="c","fixed_value"="N","primitive_index"="I","effective_support"="b","owner"="C","component_sd_parameter"="C","prior"="C","prior_source"="c")),
  loading_constraint_rules=list(c("rule"="c","kind"="c","cell_id"="c","state"="c","label"="C","primitive"="C","value"="N","resolved_fixed_value"="N","captured_selector"="l")),
  orientation=list(c("coordinate"="c","coordinate_id"="c","pin_witness"="c","sign_group"="I","pinned"="b","allowed_simultaneous_flip"="b","latent_only"="b","anchor"="C"),c("coordinate"="c","coordinate_id"="c","pin_witness"="c","sign_group"="I","pinned"="b","allowed_simultaneous_flip"="b","latent_only"="b","anchor"="C","sign_character"="l","representation"="c")),
  component_groups=list(c("component"="c","kind"="c","dimensions"="i")),
  coordinates=list(c("group"="c","kind"="c","display"="c","id"="c","group_index"="i","index"="i","coordinate"="C")),
  basis_rows=list(c("component"="c","target"="c","input_row_label"="c","target_index"="i","input_row_index"="i","zero_row"="b")),
  correlations=list(c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C"),c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C","required_by"="C"),c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C","estimand"="c","primitive_parameter"="C"),c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C","required_by"="C","primitive_parameter"="C","estimand"="c"),c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C","constraint_group"="i","fixed_value"="N","equality_labels"="l","constraint_sources"="l"),c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C","constraint_group"="i","fixed_value"="N","equality_labels"="l","constraint_sources"="l","required_by"="C"),c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C","constraint_group"="i","fixed_value"="N","equality_labels"="l","constraint_sources"="l","estimand"="c","primitive_parameter"="C"),c("row"="i","col"="i","component1"="c","component2"="c","state"="c","source"="c","parameter"="C","constraint_group"="i","fixed_value"="N","equality_labels"="l","constraint_sources"="l","required_by"="C","primitive_parameter"="C","estimand"="c")),
  parameter_correlations=list(c("row"="i","col"="i","target1"="c","target2"="c","state"="c","source"="c","parameter"="C"),c("row"="i","col"="i","target1"="c","target2"="c","state"="c","source"="c","parameter"="C","constraint_group"="i","fixed_value"="N","equality_labels"="l","constraint_sources"="l")),
  parameter_residuals=list(c("target"="c","units"="c","target_index"="i","residual_index"="i","parameter"="C"),c("target"="c","units"="c","target_index"="i","residual_index"="i","parameter"="C","expression"="c","owner"="C","total_sd_parameter"="C","free_parameter_index"="I")),
  information=list(c("block"="c","observation_id"="c","policy"="c","informative"="b")),
  exclusions=list(c(".block"="c",".observation_id"="c",".person_key"="c")),
  partial_response_exclusions=list(structure(character(),names=character()),c("block"="c","observation_id"="c","missing_response_cells"="i","observed_response_cells_discarded"="i")),
  component_scales=list(c("component"="c","coordinate_id"="c","state"="c","units"="c","support"="c","affected_class_members"="c","expression"="c","component_index"="i","primitive_index"="i","parameter"="C","marker_target"="C","marker_cell"="C","marker_units"="C","fixed_sd"="N","marker_value"="N","captured_selector"="l")),
  response_domains=list(c("model"="c","parameter"="c","task"="c","key"="c","predictor"="c","receives"="c","support"="c","units"="c","observation_units"="c","predictor_units"="c","role"="c","level"="c","level_source"="c","received_units"="c","predictor_representation"="c","natural_representation"="c","supplied_scale"="C","supplied_units"="C","supplied_value"="N","fixed_received"="N","fixed_predictor"="N","fixed_natural"="N","free_parameter"="b","individual_variation"="b","eta_index"="I","mean_level"="c","noise_key"="c","fixed_mean"="N","fixed_sigma"="N","noise_free"="b","response_index"="i")),
  observation_operands=list(c("block"="c","observation_id"="c","argument"="c","role"="c","level"="c","receives"="c","predictor"="c","operand"="c","value_source"="c","route"="C","target"="C","literal_predictor"="C","literal"="b","literal_value"="N","received_constant"="N","eta_index"="I","observation_scale_index"="I")),
  target_coverage=list(c("target"="c","declared"="b","nonzero_common"="b","parameter_residual"="b","mean_only"="b")),
  priors=list(c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i"),c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i","coordinate_measure"="C","constraint_domain"="C","normalization_scope"="C"),c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i","decomposition"="C","stratum"="C","term"="C","coordinate"="C"),c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i","decomposition"="C","stratum"="C","term"="C","coordinate"="C","coordinate_measure"="C","constraint_domain"="C","normalization_scope"="C"),c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i","block"="C"),c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i","block"="C","coordinate_measure"="C","constraint_domain"="C","normalization_scope"="C"),c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i","block"="C","decomposition"="C","stratum"="C","term"="C","coordinate"="C"),c("parameter"="c","class"="c","layer"="c","source"="c","status"="c","scope"="c","target"="C","component"="C","prior"="C","validation_error"="C","calibration_status"="C","calibration_location"="N","calibration_scale"="N","parameter_count"="i","block"="C","decomposition"="C","stratum"="C","term"="C","coordinate"="C","coordinate_measure"="C","constraint_domain"="C","normalization_scope"="C")),
  loading_prior_calibration=list(structure(character(),names=character()),c("parameter"="c","cell_id"="c","target"="c","source"="c","observations"="c","original_source"="c","proposal_status"="c","proposal_reason"="c","policy"="c","marker_component"="C","proposed_scale"="N","original_target_scale"="N","marker_value"="N","marker_calibration_scale"="N","marker_sd_proposal"="N","marker_log_sd_proposal"="N","log_beta_proposal"="N")),
  component_sd_calibration=list(structure(character(),names=character()),c("parameter"="c","component"="c","marker_target"="c","status"="c","source"="c","observations"="c","policy"="c","component_index"="i","marker_value"="N","marker_calibration"="N","log_sd_proposal"="N","proposed_scale"="N")),
  prior_resolution=list(structure(character(),names=character()),c("parameter"="c","prior"="c","component"="c","targets"="c","rule_index"="i","maximal"="b")),
  parameters=list(c("parameter"="c","class"="c","layer"="c")),
  latent_coordinates=list(c("component"="c","noncentered_symbol"="c","prior"="c","scientific_score"="c","count"="i")),
  parameter_latent_coordinates=list(c("target"="c","noncentered_symbol"="c","prior"="c","scientific_score"="c","count"="i")),
  share_targets=list(c("owner"="c","target"="c","parameter"="c","stratum"="c","units"="c","stratum_index"="i","design_row"="i","target_index"="i","total_sd_index"="i","residual"="b")),
  share_parts=list(c("owner"="c","target"="c","part"="c","group"="C","residual"="b","stratum_index"="i","part_index"="i","target_index"="i","total_sd_index"="i")),
  share_cells=list(c("owner"="c","target"="c","group"="c","part"="c","coordinate_index"="i","target_index"="i","total_sd_index"="i","multiplier"="n")),
  share_coefficients=list(c("parameter"="c","owner"="c","stratum"="c","term"="c","coordinate"="c","stratum_index"="i")),
  required_component_zeros=list(c("row"="i","col"="i","owner"="c")),
  residual_declarations=list(structure(character(),names=character()),c("target"="c","correlation_background"="c","correlation_scope"="c","declaration_source"="c","owner"="C","global_declared"="b","owner_declared"="b","correlation_explicit"="b")),
  decompositions=list(c("owner"="c","population"="c","correlation_prior"="c","residual"="b")),
  person_roster=list(c("person_index"="i","person_id"="c","person_key"="c","observed"="b")),
  correlation_blocks=list(c("block"="c","component"="c","coordinate_id"="c","block_index"="i","within_block_index"="i","component_index"="i","ordered_index"="i")),
  raw_correlation_coordinates=list(c("parameter"="c","block"="c","destination"="c","previous_whitened_coordinate"="c","block_index"="i","row"="i","col"="i","destination_index"="i","previous_index"="i","primitive_index"="i")),
  task_target_domain=list(c("target"="c","level"="c","reason"="c","has_symbolic_support"="b","task_white_target"="b")),
  task_latent_coordinates=list(c("target"="c","raw_coordinate_domain"="c","score_domain"="c","target_index"="i","unconstrained_count"="i","constrained_count"="i")),
  correlation_primitives=list(c("parameter"="c","layer"="c","primitive_index"="i","members"="i","edge_indices"="l")),
  correlation_affine_cells=list(c("layer"="c","endpoint1"="c","endpoint2"="c","state"="c","source"="c","row"="i","col"="i","parameter"="C","primitive_index"="I","fixed_value"="N")),
  correlation_constraint_rules=list(c("rule"="c","kind"="c","layer"="c","endpoint1"="c","endpoint2"="c","resolved_state"="c","label"="C","resolved_parameter"="C","value"="N","resolved_fixed_value"="N","edge"="i","captured_selector"="l"))
)

.bh_ps_certificate <- function(x,path,n) {
  if(typeof(x)!="list") .bh_pf_fail(path,"expected stored correlation certificate record")
  z<-unclass(x);s<-z$status;m<-z$method
  fields<-if(identical(s,"strictly_positive_definite")&&identical(m,"empty_matrix")) c("status","method","margin","error_bound","factor") else
    if(identical(s,"strictly_positive_definite")&&identical(m,"inverse_factor_congruence_Gershgorin_with_roundoff_bound")) c("status","method","margin","error_bound","factor","eigenvalues") else
    if(identical(s,"proven_indefinite")&&identical(m,"negative_quadratic_form")) c("status","method","quadratic","error_bound","witness") else
    if(identical(s,"unknown")&&z$reason%in%c("nonfinite or nonsymmetric numerical matrix","symmetric eigensolver failed","inverse-factor congruence is not representable")) c("status","reason") else
    if(identical(s,"unknown")&&identical(z$reason,"Cholesky failed without a verified negative direction")) c("status","reason","eigenvalues") else
    if(identical(s,"unknown")&&identical(z$reason,"the inverse-factor Gershgorin margin does not exceed its arithmetic bound")) c("status","reason","margin","error_bound","eigenvalues") else
      .bh_pf_fail(path,"unknown stored correlation certificate variant")
  z<-.bh_pf_record(x,path,fields)
  for(k in .bh_ps_intersect(c("status","method","reason"),fields)) .bh_ps_vec(z[[k]],paste0(path,".",k),"c",1L)
  if("factor"%in%fields) {
    .bh_ps_matrix(z$factor,paste0(path,".factor"),square=TRUE)
    if(!identical(as.integer(attr(z$factor,"dim",exact=TRUE)),rep(as.integer(n),2L))) .bh_pf_fail(path,"certificate factor dimension does not match its stored domain")
  }
  for(k in .bh_ps_intersect(c("eigenvalues","witness"),fields)) .bh_ps_vec(z[[k]],paste0(path,".",k),"n",n)
  if("quadratic"%in%fields) .bh_ps_vec(z$quadratic,paste0(path,".quadratic"),"n",1L)
  if("margin"%in%fields) .bh_ps_vec(z$margin,paste0(path,".margin"),"N",if(identical(s,"unknown"))n else 1L)
  if("error_bound"%in%fields) {
    if(identical(s,"unknown")) {
      .bh_ps_matrix(z$error_bound,paste0(path,".error_bound"),type="N",square=TRUE)
      if(!identical(as.integer(attr(z$error_bound,"dim",exact=TRUE)),rep(as.integer(n),2L))) .bh_pf_fail(path,"certificate error-bound matrix has wrong dimensions")
    } else .bh_ps_vec(z$error_bound,paste0(path,".error_bound"),"n",1L)
  }
  invisible(TRUE)
}
.bh_ps_correlation_domain <- function(x,path) {
  if(typeof(x)=="list"&&"product_status"%in%.bh_pf_names(x))return(.bh_ps_product_domain(x,path))
  base<-c("dimension","status","constant","coordinate_measure","method","reason","affine_chart_scope","runtime_scope")
  attached<-c("parameter","primitive_role","coordinates")
  if(typeof(x)!="list") .bh_pf_fail(path,"expected stored correlation domain")
  q<-unclass(x);a<-.bh_ps_intersect(names(q),attached)
  if(length(a)&&!.bh_ps_setequal(a,attached)) .bh_pf_fail(path,"incomplete attached correlation-domain metadata")
  # Enumerate producer exit branches, rather than admitting an arbitrary subset.
  candidates<-list(base,c(base,"interval_status"),c(base,"interior","interval_status"),c(base,"interior","factor","interval_status"),
    c(base,"certificate","interior"),c(base,"certificate","interior","factor"))
  bounds<-c("whitening_error_bound","direction_error_bound","eigensystem_error_bound","endpoint_enclosures")
  for(search in list(character(),"interior_search")) {
    candidates<-c(candidates,list(c(base,"certificate",search),c(base,"certificate","interior",search),
      c(base,"certificate","interior",search,"interval_status","interval_reason"),
      c(base,"certificate","interior",search,bounds,"interval_status","interval_reason","endpoint_error_bound"),
      c(base,"certificate","interior",search,bounds,"interval_status","lower","upper","midpoint","half_width","interval_method","endpoint_error_bound","direction_eigenvalues","direction")))
  }
  matches<-vapply(candidates,function(v).bh_ps_setequal(names(q),c(v,a))&&length(q)==length(c(v,a)),logical(1))
  if(!any(matches)) .bh_pf_fail(path,"unsupported stored correlation-domain exit schema")
  z<-.bh_pf_record(x,path,c(candidates[[which(matches)[1L]]],a))
  .bh_ps_positive_index(z$dimension,paste0(path,".dimension"),zero=TRUE)
  if(length(z$dimension)!=1L) .bh_pf_fail(path,"correlation-domain dimension must be scalar")
  .bh_ps_matrix(z$constant,paste0(path,".constant"),square=TRUE);n<-attr(z$constant,"dim",exact=TRUE)[1L]
  .bh_ps_enum(z$status,c("unknown","feasible","proven_infeasible_strict_PD"),paste0(path,".status"))
  for(k in .bh_ps_intersect(c("status","coordinate_measure","method","reason","affine_chart_scope","runtime_scope","primitive_role","coordinates","interval_status","interval_reason","interval_method"),names(z))) .bh_ps_vec(z[[k]],paste0(path,".",k),"c",1L)
  if(!identical(z$affine_chart_scope,if(z$dimension==0L)"fully_fixed"else if(z$dimension==1L)"one_unique_label"else"multilabel_inspection_only")) .bh_pf_fail(path,"stored correlation chart scope contradicts dimension")
  if("parameter"%in%names(z)) {.bh_ps_vec(z$parameter,paste0(path,".parameter"));.bh_ps_unique(z$parameter,paste0(path,".parameter"))}
  if("certificate"%in%names(z)) .bh_ps_certificate(z$certificate,paste0(path,".certificate"),n)
  if("factor"%in%names(z)) {.bh_ps_matrix(z$factor,paste0(path,".factor"),square=TRUE);if(!identical(.bh_ps_dim(z$factor),.bh_ps_dim(z$constant))) .bh_pf_fail(path,"stored domain factor dimensions differ")}
  if("interior"%in%names(z)) .bh_ps_vec(z$interior,paste0(path,".interior"),"n",z$dimension)
  if("interior_search"%in%names(z)) {
    r<-.bh_pf_record(z$interior_search,paste0(path,".interior_search"),c("method","candidate"))
    if(z$dimension!=1L||!identical(r$method,"concave_minimum_eigenvalue_search_candidate_only")) .bh_pf_fail(path,"invalid recorded one-dimensional interior search")
    .bh_ps_vec(r$candidate,paste0(path,".interior_search.candidate"),"n",1L)
  }
  if("endpoint_enclosures"%in%names(z)) {
    r<-.bh_pf_record(z$endpoint_enclosures,paste0(path,".endpoint_enclosures"),c("lower","upper"))
    .bh_ps_vec(r$lower,paste0(path,".endpoint_enclosures.lower"),if(identical(z$interval_status,"available"))"n"else"N",2L);.bh_ps_vec(r$upper,paste0(path,".endpoint_enclosures.upper"),if(identical(z$interval_status,"available"))"n"else"N",2L)
  }
  for(k in .bh_ps_intersect(c("whitening_error_bound","direction_error_bound","eigensystem_error_bound","endpoint_error_bound","lower","upper","midpoint","half_width"),names(z))) .bh_ps_vec(z[[k]],paste0(path,".",k),"N",1L)
  if("direction_eigenvalues"%in%names(z)) .bh_ps_vec(z$direction_eigenvalues,paste0(path,".direction_eigenvalues"),"n",n)
  if("direction"%in%names(z)) {.bh_ps_matrix(z$direction,paste0(path,".direction"),square=TRUE);if(!identical(.bh_ps_dim(z$direction),.bh_ps_dim(z$constant))) .bh_pf_fail(path,"domain direction shape differs from its constant")}
  if("interval_status"%in%names(z)) {
    .bh_ps_enum(z$interval_status,c("not_assessed","available"),paste0(path,".interval_status"))
    if(identical(z$interval_status,"available")&&(!identical(z$status,"feasible")||z$dimension!=1L||!all(c("lower","upper","half_width","midpoint","direction")%in%names(z))||!is.finite(z$lower)||!is.finite(z$upper)||z$lower>=z$upper||z$half_width<=0)) .bh_pf_fail(path,"available interval has inconsistent stored shape or bounds")
  }
  if("factor"%in%names(z)&&(!identical(z$status,"feasible")||z$dimension!=0L)) .bh_pf_fail(path,"a complete stored factor requires a fixed feasible domain")
  invisible(TRUE)
}
.bh_ps_prior_geometry <- function(x,path) {
  z<-.bh_pf_record(x,path,c("component","parameter"))
  for(layer in names(z)) {
    p<-paste0(path,".",layer);q<-z[[layer]]
    if(typeof(q)!="list") .bh_pf_fail(p,"expected correlation-prior geometry record")
    q<-unclass(q)
    .bh_ps_vec(q$status,paste0(p,".status"),"c",1L)
    if(q$status%in%c("separate_raw_B_law","not_applicable_no_free_correlations")) {
      q<-.bh_pf_record(q,p,c("status","measure"));.bh_ps_vec(q$measure,paste0(p,".measure"),"c",1L)
    }else {
      f<-c("status","eta","unique_parameters","measure","kernel","normalization","posterior_propriety","reason")
      extra<-c("proper_iff_eta_greater_than","complete_block_sizes")
      if(any(extra%in%names(q)))f<-c(f,extra)
      q<-.bh_pf_record(q,p,f)
      .bh_ps_enum(q$status,c("unassessed","proven_proper_prior","existing_zero_free_prior_scope","proven_improper_prior"),paste0(p,".status"))
      for(k in .bh_ps_setdiff(f,c("eta","unique_parameters",extra))) .bh_ps_vec(q[[k]],paste0(p,".",k),"c",1L)
      .bh_ps_vec(q$eta,paste0(p,".eta"),"N",1L);.bh_ps_positive_index(q$unique_parameters,paste0(p,".unique_parameters"))
      if(length(q$unique_parameters)!=1L) .bh_pf_fail(p,"unique parameter count is not scalar")
      if(any(extra%in%f)) {
        .bh_ps_vec(q$proper_iff_eta_greater_than,paste0(p,".proper_iff_eta_greater_than"),"n",1L)
        .bh_ps_positive_index(q$complete_block_sizes,paste0(p,".complete_block_sizes"))
        if(q$unique_parameters!=1L||!length(q$complete_block_sizes)||any(q$complete_block_sizes<2L)) .bh_pf_fail(p,"invalid complete-block certificate shape")
      }
    }
  }
  invisible(TRUE)
}

.bh_ps_scientific_record <- function(x,path,name) {
  if(name=="person_law") {
    z<-.bh_pf_record(x,path,c("distribution","roster_size","observed_people","covariance_law","covariance_rank","roster_source","dense_covariance_stored"))
    .bh_ps_enum(z$distribution,c("iid","sample_centered"),paste0(path,".distribution"))
    for(k in c("distribution","covariance_law","roster_source")) .bh_ps_vec(z[[k]],paste0(path,".",k),"c",1L)
    for(k in c("roster_size","observed_people","covariance_rank")) {.bh_ps_positive_index(z[[k]],paste0(path,".",k),zero=TRUE);if(length(z[[k]])!=1L).bh_pf_fail(path,"person-law counts must be scalar")}
    if(z$observed_people>z$roster_size||z$covariance_rank!=z$roster_size-as.integer(z$distribution=="sample_centered")||!identical(z$dense_covariance_stored,FALSE)) .bh_pf_fail(path,"inconsistent stored person-law dimensions")
  }else if(name=="correlation_coordinate_system") {
    z<-.bh_pf_record(x,path,c("type","ordered_global_indices","prior_measure","applied_prior_coordinate_jacobian","geometric_B_to_R_jacobian"))
    .bh_ps_enum(z$type,c("ordered_blocks","final_marginal_mask","affine_final_marginal_labels"),paste0(path,".type"))
    for(k in .bh_ps_setdiff(names(z),"ordered_global_indices")) .bh_ps_vec(z[[k]],paste0(path,".",k),"c",1L)
    .bh_ps_positive_index(z$ordered_global_indices,paste0(path,".ordered_global_indices"))
    if(!.bh_ps_setequal(z$ordered_global_indices,seq_along(z$ordered_global_indices))) .bh_pf_fail(path,"global coordinate order must be a permutation")
  }else if(name=="augmentation") {
    z<-.bh_pf_record(x,path,c("type","target","target_index","order_source","common_covariance_coordinates","parameter_covariance_coordinates","sampled_common_coordinates","sampled_parameter_coordinates","task_white_coordinates","task_constrained_scores","raw_coordinate_variance","isometric_basis","normalized_subspace_log_adjustment","common_scores","parameter_scores","numerical_factorization"))
    .bh_ps_enum(z$type,c("task_cholesky","common_plus_parameter_residual"),paste0(path,".type"))
    for(k in c("type","order_source","isometric_basis","common_scores","parameter_scores","numerical_factorization")) .bh_ps_vec(z[[k]],paste0(path,".",k),"c",1L)
    .bh_ps_vec(z$target,paste0(path,".target"));.bh_ps_unique(z$target,paste0(path,".target"))
    .bh_ps_positive_index(z$target_index,paste0(path,".target_index"));.bh_ps_unique(z$target_index,paste0(path,".target_index"))
    if(length(z$target)!=length(z$target_index)) .bh_pf_fail(path,"augmentation targets and indices differ in length")
    for(k in c("common_covariance_coordinates","parameter_covariance_coordinates","sampled_common_coordinates","sampled_parameter_coordinates","task_white_coordinates","task_constrained_scores")) {.bh_ps_positive_index(z[[k]],paste0(path,".",k),zero=TRUE);if(length(z[[k]])!=1L).bh_pf_fail(path,"augmentation counts must be scalar")}
    .bh_ps_vec(z$raw_coordinate_variance,paste0(path,".raw_coordinate_variance"),"N",1L)
    .bh_ps_vec(z$normalized_subspace_log_adjustment,paste0(path,".normalized_subspace_log_adjustment"),"n",1L)
    if(identical(z$type,"common_plus_parameter_residual")&&(length(z$target)||z$task_white_coordinates!=0L||z$task_constrained_scores!=0L)) .bh_pf_fail(path,"common/residual augmentation has unexpected task coordinates")
    if(identical(z$type,"task_cholesky")&&(z$sampled_common_coordinates!=0L||z$sampled_parameter_coordinates!=0L)) .bh_pf_fail(path,"task augmentation cannot retain sampled common/residual counts")
  }else if(name=="orientation_sign_system") {
    z<-.bh_pf_record(x,path,c("equations","equation_sources","rref","allowed_binary_basis","anchor_parameters","anchor_characters","correlation_label_characters","unanchored_binary_basis","scope"))
    for(k in c("equations","rref","allowed_binary_basis","anchor_characters","unanchored_binary_basis")) .bh_ps_matrix(z[[k]],paste0(path,".",k),binary=TRUE)
    .bh_ps_vec(z$equation_sources,paste0(path,".equation_sources"),"c",.bh_ps_dim(z$equations)[1L])
    .bh_ps_vec(z$anchor_parameters,paste0(path,".anchor_parameters"),"c",.bh_ps_dim(z$anchor_characters)[1L]);.bh_ps_unique(z$anchor_parameters,paste0(path,".anchor_parameters"))
    .bh_ps_vec(z$scope,paste0(path,".scope"),"c",1L)
    K<-.bh_ps_dim(z$equations)[2L];D<-.bh_ps_dim(z$allowed_binary_basis)[2L]
    if(!identical(.bh_ps_dim(z$rref),.bh_ps_dim(z$equations))||.bh_ps_dim(z$allowed_binary_basis)[1L]!=K||.bh_ps_dim(z$anchor_characters)[2L]!=D||.bh_ps_dim(z$unanchored_binary_basis)[1L]!=K) .bh_pf_fail(path,"stored sign-system matrix dimensions disagree")
    labels<-.bh_ps_list(z$correlation_label_characters,paste0(path,".correlation_label_characters"),named=TRUE)
    for(k in seq_along(labels)) {.bh_ps_vec(labels[[k]],paste0(path,".correlation_label_characters[[",k,"]]"),"i",D);if(any(!labels[[k]]%in%c(0,1))).bh_pf_fail(path,"correlation sign characters must be binary")}
  }else if(name=="correlation_domains") {
    z<-.bh_pf_record(x,path,c("component","parameter"))
    for(k in names(z)) .bh_ps_correlation_domain(z[[k]],paste0(path,".",k))
  }else if(name=="correlation_affine_constants") {
    z<-.bh_pf_record(x,path,c("component","parameter"))
    for(k in names(z)) .bh_ps_matrix(z[[k]],paste0(path,".",k),square=TRUE)
  }else if(name=="correlation_prior_geometry") {
    .bh_ps_prior_geometry(x,path)
  }else if(name=="share_strata") {
    .bh_pf_share_strata(x,path)
  }else .bh_pf_fail(path,"unsupported non-tabular scientific node")
  invisible(TRUE)
}
.bh_ps_prior_attributes <- function(x,path) {
  aliases<-c(loading_calibration="loading_prior_calibration",loading_aliases="loading_prior_aliases",component_sd_calibration="component_sd_calibration")
  for(k in names(aliases)) {
    a<-attr(x,k,exact=TRUE)
    if(!is.null(a)) .bh_portable_scientific_schema(a,paste0(path,".@",aliases[[k]]))
  }
  for(k in c("correlation_measure","selection_semantics","parameterization")) {
    a<-attr(x,k,exact=TRUE);if(!is.null(a)) .bh_ps_vec(a,paste0(path,".@",k),"c",1L)
  }
  a<-attr(x,"calibration",exact=TRUE)
  if(!is.null(a)) {
    z<-.bh_ps_list(a,paste0(path,".@calibration"),named=TRUE)
    for(i in seq_along(z))if(!is.null(z[[i]])) {
      p<-paste0(path,".@calibration[[",i,"]]");q<-z[[i]]
      fields<-c("location","scale","source","n")
      if(any(c("observations","input_scale")%in%names(q)))fields<-c(fields,"observations","input_scale")
      q<-.bh_pf_record(q,p,fields)
      for(k in c("location","scale")) .bh_ps_vec(q[[k]],paste0(p,".",k),"n",1L)
      .bh_ps_vec(q$source,paste0(p,".source"),"c",1L);.bh_ps_positive_index(q$n,paste0(p,".n"))
      if(length(q$n)!=1L||q$scale<=0) .bh_pf_fail(p,"invalid calibration count or scale")
      if("observations"%in%fields) {.bh_ps_vec(q$observations,paste0(p,".observations"),"c",q$n);.bh_ps_enum(q$input_scale,c("response","log_response"),paste0(p,".input_scale"))}
    }
  }
  invisible(TRUE)
}

.bh_portable_scientific_schema <- function(x,path,allow_endpoint_free=FALSE) {
  name<-sub("^.*[.$@]","",path)
  if(name=="formula")return(.bh_portable_formula_schema(x,path))
  if(name=="prior")return(.bh_ps_prior(x,path))
  if(name=="schema_migration")return(.bh_ps_migration(x,path))
  if(name=="parameter_keys") {
    schema<-rep("c",10L);names(schema)<-c("model","parameter","task","key","predictor","receives","support","units","observation_units","predictor_units")
    z<-.bh_ps_fields(x,path,list(schema));.bh_ps_unique(z$key,paste0(path,".key"));return(invisible(TRUE))
  }
  if(name=="tables") {
    admitted<-c(names(.bh_ps_table_schemas()),"ordinary_fixed_matrix","basis_multiplier","basis_amplitude_index","orientation_sign_system","correlation_prior_geometry","correlation_mask","parameter_correlation_mask","share_strata","person_law","correlation_coordinate_system","augmentation","correlation_affine_constants","correlation_domains")
    optional<-c("orientation_sign_system","share_targets","share_parts","share_cells","share_coefficients","share_strata","required_component_zeros","residual_declarations","decompositions")
    z<-.bh_pf_record(x,path,admitted,required=.bh_ps_setdiff(admitted,optional))
    for(k in names(z)) .bh_portable_scientific_schema(z[[k]],paste0(path,".",k),
      allow_endpoint_free=k=="correlation_affine_cells")
    .bh_pf_scientific_relations(z,path);return(invisible(TRUE))
  }
  if(name=="observation_operands"&&is.null(x))return(invisible(TRUE))
  if(name%in%c("ordinary_fixed_matrix","basis_multiplier"))return(.bh_ps_matrix(x,path))
  if(name=="basis_amplitude_index")return(.bh_ps_positive_index(x,path,zero=TRUE))
  if(name%in%c("correlation_mask","parameter_correlation_mask")) {
    .bh_ps_matrix(x,path,"c",square=TRUE)
    values<-unclass(x);attributes(values)<-NULL
    .bh_ps_enum(values,c("zero","free","fixed","fixed_one"),path)
    d<-attr(x,"dim",exact=TRUE);diagonal<-if(d[1L])1L+(seq_len(d[1L])-1L)*(d[1L]+1L)else integer()
    if(length(values)&&any(values[diagonal]!="fixed_one")) .bh_pf_fail(path,"correlation-mask diagonal must retain fixed-one labels")
    size<-as.double(d[[1L]])
    for(col in seq_len(size)) {
      row<-seq_len(size)
      if(!identical(values[as.double(row)+(as.double(col)-1.0)*size],values[as.double(col)+(as.double(row)-1.0)*size]))
        .bh_pf_fail(path,"saved correlation mask is not symmetric")
    }
    return(invisible(TRUE))
  }
  schemas<-.bh_ps_table_schemas()
  if(!name%in%names(schemas))return(.bh_ps_scientific_record(x,path,name))
  variants<-schemas[[name]]
  # Exact empty text columns emitted by the retained producers. This changes
  # the validation schema only; stored vectors are never coerced or replaced.
  zero_text<-switch(name,
    free_loadings="support",loading_primitives="support",
    ordinary_loading_map=c("state","sign","expression"),
    loading_cells=c("state","sign","expression"),
    loading_prior_aliases=c("state","sign","expression"),
    loading_constraint_rules="state",loadings="sign",loading_coefficients="support",
    loading_map="expression",parameter_residuals="expression",character())
  if((length(zero_text)||name%in%c("components","correlations","task_target_domain","priors"))&&.bh_pf_table_shape(x,path)==0.0) {
    for(field in zero_text)if(typeof(.bh_pf_get(x,field))=="logical")
      for(i in seq_along(variants))if(field%in%names(variants[[i]]))variants[[i]][field]<-"z"
    if(name=="components"&&typeof(.bh_pf_get(x,"orientation"))=="logical")for(i in seq_along(variants))variants[[i]]["orientation"]<-"z"
    if(name=="task_target_domain"&&typeof(.bh_pf_get(x,"reason"))=="logical")for(i in seq_along(variants))variants[[i]]["reason"]<-"z"
    if(name=="priors"&&typeof(.bh_pf_get(x,"status"))=="logical"&&typeof(.bh_pf_get(x,"scope"))=="logical")
      for(i in seq_along(variants))variants[[i]][c("status","scope")]<-"z"
    if(name=="correlations") {
      if(typeof(.bh_pf_get(x,"estimand"))=="logical")
        for(i in seq_along(variants))if("estimand"%in%names(variants[[i]]))variants[[i]]["estimand"]<-"z"
      absent<-!c("component1","component2")%in%.bh_pf_names(x)
      if(any(absent)&&!all(absent)) .bh_pf_fail(path,"an empty correlation table cannot lose only one endpoint column")
      if(all(absent))for(i in seq_along(variants))variants[[i]]<-variants[[i]][!.bh_pf_names(variants[[i]])%in%c("component1","component2")]
    }
  }
  # Historical zero-component assembly drops both NULL endpoint columns.
  # Admit only that exact typed-empty shape, without rewriting stored evidence.
  if(name=="correlation_affine_cells"&&isTRUE(allow_endpoint_free)&&.bh_pf_table_shape(x,path)==0.0) {
    absent<-!c("endpoint1","endpoint2")%in%.bh_pf_names(x)
    if(any(absent)&&!all(absent)) .bh_pf_fail(path,"empty affine cells cannot lose only one endpoint column")
    if(all(absent))for(i in seq_along(variants))variants[[i]]<-variants[[i]][!names(variants[[i]])%in%c("endpoint1","endpoint2")]
  }
  schemas[[name]]<-variants
  z<-.bh_ps_fields(x,path,variants)
  if(!length(z))return(invisible(TRUE))
  n<-length(z[[1L]])
  # Integral indices are checked even when producers emitted double storage.
  schema<-schemas[[name]][[which(vapply(schemas[[name]],function(s).bh_ps_setequal(names(s),names(z))&&length(s)==length(z),logical(1)))[1L]]]
  zero_indices<-c("count","parameter_count","primitive_index","unconstrained_count","constrained_count","missing_response_cells","observed_response_cells_discarded")
  for(k in names(schema)[schema%in%c("i","I")]) .bh_ps_positive_index(z[[k]],paste0(path,".",k),zero=k%in%zero_indices,missing=schema[[k]]=="I")
  key_field<-switch(name,targets="key",parameter_roles="key",fixed_parameters="key",individual_targets="key",observation_parameters="key",components="component",free_loadings="parameter",loading_primitives="parameter",amplitudes="parameter",loading_coefficients="parameter",loading_cells="cell_id",ordinary_loading_map="cell_id",loading_prior_aliases="cell_id",component_groups="component",coordinates="id",parameter_residuals="target",component_scales="component",response_domains="key",target_coverage="target",priors="parameter",parameters="parameter",latent_coordinates="component",parameter_latent_coordinates="target",share_targets="target",share_coefficients="parameter",decompositions="owner",person_roster="person_key",raw_correlation_coordinates="parameter",task_target_domain="target",task_latent_coordinates="target",NULL)
  if(!is.null(key_field)) .bh_ps_unique(z[[key_field]],paste0(path,".",key_field))
  if(name%in%c("targets","parameter_roles","fixed_parameters","individual_targets","observation_parameters","response_domains")) {
    .bh_ps_enum(z$level,c("subject_task","task","fixed"),paste0(path,".level"));.bh_ps_enum(z$role,c("predictor","observation_scale"),paste0(path,".role"))
    .bh_ps_enum(z$level_source,c("model_default","block_override"),paste0(path,".level_source"))
    if(any(z$free_parameter!=(z$level!="fixed"))||any(z$individual_variation!=(z$role=="predictor"&z$level=="subject_task"))) .bh_pf_fail(path,"parameter role/variation flags disagree")
    if(name=="fixed_parameters"&&any(z$level!="fixed")||name=="individual_targets"&&any(z$level!="subject_task")||name=="observation_parameters"&&any(z$role!="observation_scale"|z$level=="fixed")||name=="targets"&&any(z$role!="predictor"|z$level=="fixed")) .bh_pf_fail(path,"parameter subset has incompatible roles")
  }
  if(name=="components"&&(any(z$mean!=0)|any(z$standardized_variance!=1)|any(!is.na(z$variance)&z$variance!=1))) .bh_pf_fail(path,"captured component moments contradict their standardized representation")
  if(name=="coordinates") {
    .bh_ps_enum(z$kind,c("scalar","basis"),paste0(path,".kind"));.bh_ps_unique(z$display,paste0(path,".display"))
    if(!identical(as.integer(z$index),seq_len(n))||any(is.na(z$coordinate[z$kind=="basis"]))||any(!is.na(z$coordinate[z$kind=="scalar"]))) .bh_pf_fail(path,"coordinate indices or scalar/basis labels disagree")
  }
  if(name%in%c("correlations","parameter_correlations")) {
    .bh_ps_enum(z$state,c("zero","free","fixed"),paste0(path,".state"))
    if(any(z$row>=z$col)||base::anyDuplicated.default(paste(z$row,z$col,sep=":"))) .bh_pf_fail(path,"correlation pairs must be distinct upper-triangle edges")
    if(any(is.na(z$parameter[z$state=="free"]))||any(!is.na(z$parameter[z$state!="free"]))) .bh_pf_fail(path,"correlation primitive presence disagrees with edge state")
    affine<-all(c("constraint_group","fixed_value","equality_labels","constraint_sources")%in%names(z))
    if(affine) {
      if(any(!is.na(z$fixed_value[z$state=="free"]))||any(!is.finite(z$fixed_value[z$state!="free"]))||any(abs(z$fixed_value[z$state!="free"])>1)||any(z$fixed_value[z$state=="zero"]!=0)) .bh_pf_fail(path,"stored affine edge states and fixed values disagree")
      .bh_ps_string_list(z$equality_labels,paste0(path,".equality_labels"));.bh_ps_string_list(z$constraint_sources,paste0(path,".constraint_sources"))
    } else if(any(z$state=="fixed")) .bh_pf_fail(path,"fixed correlation states require the affine columns")
    a<-attr(x,"affine_rules",exact=TRUE)
    if(affine!=!is.null(a)) .bh_pf_fail(path,"affine correlation columns and declaration attribute must occur together")
    if(!is.null(a)) .bh_ps_affine_rules(a,paste0(path,".@affine_rules"))
  }
  if(name%in%c("free_loadings","loading_primitives","orientation")&&"sign_character"%in%names(z)) {
    .bh_ps_string_list(z$sign_character,paste0(path,".sign_character"),integer=TRUE)
    for(v in unclass(z$sign_character))if(any(!v%in%c(0,1))) .bh_pf_fail(path,"sign characters must be binary vectors")
  }
  if(name=="orientation") {
    if(any(z$allowed_simultaneous_flip==z$pinned)) .bh_pf_fail(path,"orientation flip and pin flags disagree")
    if("representation"%in%names(z)&&any(z$representation!="GF2_binary_sign_basis")) .bh_pf_fail(path,"unknown orientation representation")
  }
  if(name=="loading_constraint_rules") {
    .bh_ps_enum(z$kind,c("equal","fixed","marker"),paste0(path,".kind"))
    z$captured_selector<-.bh_ps_list(z$captured_selector,paste0(path,".captured_selector"))
    for(i in seq_len(n)) {
      p<-paste0(path,".captured_selector[[",i,"]]")
      if(z$kind[i]=="marker") .bh_pf_target_selector(z$captured_selector[[i]],p) else .bh_pf_component_selectors(z$captured_selector[[i]],p)
    }
  }
  if(name=="component_scales") {
    .bh_ps_enum(z$state,c("fixed","free"),paste0(path,".state"))
    z$captured_selector<-.bh_ps_list(z$captured_selector,paste0(path,".captured_selector"))
    for(i in seq_len(n)) {
      if(z$state[i]=="fixed") {
        if(!isTRUE(z$fixed_sd[i]==1)||!is.na(z$parameter[i])||z$primitive_index[i]!=0L||!is.null(z$captured_selector[[i]])) .bh_pf_fail(path,"fixed component scale has inconsistent operands")
      } else {
        if(!is.na(z$fixed_sd[i])||is.na(z$parameter[i])||z$primitive_index[i]<1L||!is.finite(z$marker_value[i])||z$marker_value[i]==0) .bh_pf_fail(path,"free component scale lacks a valid marker")
        .bh_pf_target_selector(z$captured_selector[[i]],paste0(path,".captured_selector[[",i,"]]"))
      }
    }
  }
  if(name=="correlation_constraint_rules") {
    .bh_ps_enum(z$kind,c("equal","fixed"),paste0(path,".kind"))
    z$captured_selector<-.bh_ps_list(z$captured_selector,paste0(path,".captured_selector"))
    for(i in seq_len(n)) .bh_ps_pair_capture(z$captured_selector[[i]],paste0(path,".captured_selector[[",i,"]]"))
  }
  if(name=="correlation_primitives") {
    .bh_ps_string_list(z$edge_indices,paste0(path,".edge_indices"),integer=TRUE)
    if(any(lengths(unclass(z$edge_indices))!=z$members)||any(z$members<1L)) .bh_pf_fail(path,"correlation primitive member counts disagree")
    for(v in unclass(z$edge_indices))if(any(v<1L)||base::anyDuplicated.default(v)) .bh_pf_fail(path,"correlation primitive has invalid member edges")
  }
  if(name=="loading_prior_aliases"&&n>0L&&!all(c("prior","prior_source")%in%names(z))) .bh_pf_fail(path,"nonempty retained loading aliases require resolved prior metadata")
  if(name=="required_component_zeros"&&any(z$row>=z$col)) .bh_pf_fail(path,"required-zero edges must be upper-triangle coordinate pairs")
  if(name=="priors") {
    .bh_ps_enum(z$status,c("missing","invalid","specified"),paste0(path,".status"))
    .bh_ps_prior_attributes(x,path)
  }
  invisible(TRUE)
}

.bh_pf_scientific_relations <- function(tables,path="spec.tables") {
  .bh_pf_product_tables(tables,path)
  cells<-.bh_pf_get(tables,"correlation_affine_cells")
  if(!all(c("endpoint1","endpoint2")%in%.bh_pf_names(cells))) {
    for(n in c("components","correlations","parameter_correlations"))
      if(.bh_pf_table_shape(.bh_pf_get(tables,n),paste0(path,".",n))!=0.0)
        .bh_pf_fail(paste0(path,".correlation_affine_cells"),"endpoint-free cells require zero components and no declared correlation cells")
  }
  t<-.bh_ps_list(tables,path,named=TRUE)
  get<-function(k)if(k%in%names(t))unclass(t[[k]])else NULL
  need<-c("targets","components","coordinates","parameter_residuals","free_loadings","amplitudes")
  if(!all(need%in%names(t))) .bh_pf_fail(path,"missing coordinate-map structural tables")
  target<-get("targets");component<-get("components");coord<-get("coordinates");residual<-get("parameter_residuals")
  J<-length(target$key);K<-length(component$component);U<-length(residual$target)
  if(!identical(as.integer(target$target_index),seq_len(J))||!identical(as.integer(residual$residual_index),seq_len(U))||
     !identical(component$component,coord$display)) .bh_pf_fail(path,"stored target, component, or residual row identities disagree")
  if(any(residual$target_index>J)||!identical(residual$target,target$key[residual$target_index])) .bh_pf_fail(path,"parameter residuals reference different target rows")
  for(name in .bh_ps_intersect(c("basis_multiplier","ordinary_fixed_matrix"),names(t)))if(!identical(as.integer(attr(t[[name]],"dim",exact=TRUE)),as.integer(c(J,K)))) .bh_pf_fail(paste0(path,".",name),"loading matrix dimensions disagree with target/coordinate tables")
  if("basis_amplitude_index"%in%names(t)&&(length(t$basis_amplitude_index)!=K||any(t$basis_amplitude_index>length(get("amplitudes")$parameter)))) .bh_pf_fail(path,"basis amplitude indices exceed captured dimensions")
  for(name in .bh_ps_intersect(c("loadings","free_loadings","loading_primitives","loading_cells","ordinary_loading_map","loading_prior_aliases"),names(t))) {
    z<-get(name)
    if(any(z$target_index>J)||any(z$component_index>K)||!identical(z$target,target$key[z$target_index])||!identical(z$component,component$component[z$component_index])) .bh_pf_fail(paste0(path,".",name),"loading row indices do not match captured targets/coordinates")
  }
  if("loading_map"%in%names(t)) {
    z<-get("loading_map")
    if(any(z$target_index>J)||any(z$coordinate_index>K)||!identical(z$target,target$key[z$target_index])||!identical(z$coordinate_id,coord$id[z$coordinate_index])) .bh_pf_fail(path,"loading map indices disagree with target or coordinate identities")
  }
  for(layer in c("component","parameter")) {
    name<-if(layer=="component")"correlations"else"parameter_correlations"
    z<-get(name);size<-if(layer=="component")K else U
    if(!is.null(z)) {
      a<-if(layer=="component")z$component1 else z$target1;b<-if(layer=="component")z$component2 else z$target2
      labels<-if(layer=="component")component$component else residual$target
      absent_endpoints<-layer=="component"&&!any(c("component1","component2")%in%names(z))
      if(length(z$row)!=as.double(size)*(size-1)/2||any(z$col>size)||
         (absent_endpoints&&size!=0L)||(!absent_endpoints&&(!identical(a,labels[z$row])||!identical(b,labels[z$col]))))
        .bh_pf_fail(paste0(path,".",name),"correlation edges do not cover their captured coordinate domain")
    }
    mask<-if(layer=="component")"correlation_mask"else"parameter_correlation_mask"
    if(mask%in%names(t)&&!identical(as.integer(attr(t[[mask]],"dim",exact=TRUE)),rep(as.integer(size),2L))) .bh_pf_fail(paste0(path,".",mask),"correlation-mask dimensions disagree with captured coordinate domain")
    if(mask%in%names(t)&&!is.null(z)) {
      cells<-unclass(t[[mask]]);attributes(cells)<-NULL
      at<-as.double(z$row)+(as.double(z$col)-1.0)*as.double(size)
      if(!identical(cells[at],z$state)) .bh_pf_fail(paste0(path,".",mask),"saved mask cells disagree with the corresponding edge states")
    }
    if("correlation_affine_constants"%in%names(t)&&!identical(as.integer(attr(t$correlation_affine_constants[[layer]],"dim",exact=TRUE)),rep(as.integer(size),2L))) .bh_pf_fail(path,"stored correlation constant dimensions disagree with captured coordinate domain")
    if("correlation_domains"%in%names(t)&&!identical(as.integer(attr(t$correlation_domains[[layer]]$constant,"dim",exact=TRUE)),rep(as.integer(size),2L))) .bh_pf_fail(path,"stored correlation domain dimensions disagree with captured coordinate domain")
  }
  if("component_scales"%in%names(t)) {
    z<-get("component_scales")
    if(!identical(as.integer(z$component_index),seq_len(K))||!identical(z$component,component$component)||!identical(z$coordinate_id,coord$id)) .bh_pf_fail(path,"component-scale rows do not cover captured coordinates")
  }
  share<-c("share_targets","share_parts","share_cells","share_coefficients","share_strata","required_component_zeros","residual_declarations","decompositions")
  if(any(share%in%names(t))&&!all(share%in%names(t))) .bh_pf_fail(path,"variance-share tables must be retained together")
  has_share<-all(share%in%names(t))
  raw<-identical(get("correlation_coordinate_system")$type,"ordered_blocks")
  .bh_pf_raw_maps(t,coord,component,path)
  has_affine<-function(q)any(q$state=="fixed")||base::anyDuplicated.default(q$parameter[q$state=="free"])>0L
  common_affine<-has_affine(get("correlations"));any_affine<-common_affine||has_affine(get("parameter_correlations"))
  pri<-get("priors")
  if(("block"%in%names(pri))!=raw || all(c("decomposition","stratum","term","coordinate")%in%names(pri))!=has_share ||
     all(c("coordinate_measure","constraint_domain","normalization_scope")%in%names(pri))!=any_affine) .bh_pf_fail(path,"prior metadata columns disagree with captured coordinate representation")
  if(("required_by"%in%names(get("correlations")))!=has_share || all(c("estimand","primitive_parameter")%in%names(get("correlations")))!=raw ||
     all(c("owner","part","stratum","expression")%in%names(get("loading_map")))!=has_share ||
     ("expression"%in%names(residual))!=has_share) .bh_pf_fail(path,"share/raw-correlation companion columns disagree with captured representation")
  if(("orientation_sign_system"%in%names(t))!=common_affine || ("sign_character"%in%names(get("free_loadings")))!=common_affine ||
     ("sign_character"%in%names(get("orientation")))!=common_affine) .bh_pf_fail(path,"affine sign-system records must be retained with their coordinate representation")
  if(has_share) {
    owned<-get("share_targets")
    if(any(owned$target_index>J)||!identical(owned$target,target$key[owned$target_index])||!identical(as.integer(owned$total_sd_index),seq_along(owned$target))) .bh_pf_fail(path,"owned target rows disagree with captured targets")
    for(name in c("share_parts","share_cells")) {
      z<-get(name)
      if(any(z$total_sd_index>length(owned$target))||any(z$target_index>J)||!identical(z$target,target$key[z$target_index])||!identical(z$target,owned$target[z$total_sd_index])) .bh_pf_fail(paste0(path,".",name),"share cells/parts reference different captured target rows")
    }
    if(any(get("share_cells")$coordinate_index>K)||any(get("required_component_zeros")$col>K)) .bh_pf_fail(path,"share correlation/loading references exceed captured coordinates")
  }
  if(all(c("person_roster","person_law","augmentation")%in%names(t))) {
    roster<-get("person_roster");law<-get("person_law");a<-get("augmentation");N<-length(roster$person_key)
    if(!identical(as.integer(roster$person_index),seq_len(N))||law$roster_size!=N||law$observed_people!=sum(roster$observed)||a$common_covariance_coordinates!=K||a$parameter_covariance_coordinates!=U) .bh_pf_fail(path,"person-law and augmentation counts disagree with captured dimensions")
    if(any(a$target_index>J)||!identical(a$target,target$key[a$target_index])) .bh_pf_fail(path,"augmentation target indices disagree")
    if(a$type=="task_cholesky") {
      if(a$task_white_coordinates!=length(a$target)*(N-1)||a$task_constrained_scores!=length(a$target)*N) .bh_pf_fail(path,"task augmentation dimensions disagree with captured roster")
    }else if(a$sampled_common_coordinates!=K*N||a$sampled_parameter_coordinates!=U*N) .bh_pf_fail(path,"sampled common/residual dimensions disagree with captured roster")
  }
  invisible(TRUE)
}

.bh_ps_prior <- function(x,path) {
  if(is.null(x))return(invisible(TRUE))
  if(typeof(x)!="list"||!identical(attr(x,"class",exact=TRUE),"bhmm_prior")) .bh_pf_fail(path,"expected captured bhmm_prior rules")
  rules<-unclass(x)
  if(!length(rules)) .bh_pf_fail(path,"captured prior rule list cannot be empty")
  for(i in seq_along(rules)) {
    p<-paste0(path,"[[",i,"]]");r<-rules[[i]]
    if(typeof(r)!="list") .bh_pf_fail(p,"expected a captured prior-rule record")
    r<-unclass(r);.bh_ps_vec(r$class,paste0(p,".class"),"c",1L)
    .bh_ps_enum(r$class,c("Intercept","loading","amplitude","sigma","sdlog","observation_sd","parameter_sd","component_sd","cor","cor_raw","total_sd","share"),paste0(p,".class"))
    f<-c("prior","class","component","target","layer")
    if(r$class%in%c("share","total_sd"))f<-c(f,"decomposition","stratum","term","coordinate")
    if(r$class=="cor_raw"&&"block"%in%names(r))f<-c(f,"block")
    r<-.bh_pf_record(r,p,f)
    .bh_ps_vec(r$layer,paste0(p,".layer"),"c",1L);.bh_ps_enum(r$layer,c("component","parameter"),paste0(p,".layer"))
    if(r$class!="cor"&&r$layer!="component") .bh_pf_fail(p,"only joint correlation priors may select the parameter layer")
    if(!is.null(r$component)) {
      .bh_ps_vec(r$component,paste0(p,".component"),"c",1L)
      if(!r$class%in%c("loading","amplitude","component_sd")) .bh_pf_fail(p,"prior component selector has the wrong role")
    }
    if(!is.null(r$target)) {
      if(typeof(r$target)=="character") .bh_ps_vec(r$target,paste0(p,".target"),"c",1L) else {
        a<-.bh_pf_record(r$target,paste0(p,".target"),c("targets","fingerprint"),classes="bhmm_prior_targets")
        .bh_pf_target_selector(a$targets,paste0(p,".target.targets"));.bh_ps_vec(a$fingerprint,paste0(p,".target.fingerprint"),"c",1L)
      }
      if(r$class%in%c("amplitude","component_sd","cor","cor_raw","share")) .bh_pf_fail(p,"this prior role cannot have a target selector")
    }
    for(k in .bh_ps_intersect(c("decomposition","stratum","term","coordinate","block"),names(r)))if(!is.null(r[[k]])) {
      .bh_ps_vec(r[[k]],paste0(p,".",k));.bh_ps_unique(r[[k]],paste0(p,".",k))
      if(!length(r[[k]])||any(!nzchar(r[[k]]))) .bh_pf_fail(p,"captured prior selectors must be nonempty distinct strings")
      if(r$class=="total_sd"&&k!="decomposition") .bh_pf_fail(p,"total-SD priors can select only a decomposition")
    }
    d<-.bh_pf_record(r$prior,paste0(p,".prior"),c("distribution","values","text"))
    .bh_ps_vec(d$distribution,paste0(p,".prior.distribution"),"c",1L)
    arity<-c(normal=2L,logistic=2L,student_t=3L,half_normal=1L,half_student_t=3L,lkj=1L)
    if(!d$distribution%in%names(arity)) .bh_pf_fail(p,"unsupported retained prior distribution")
    .bh_ps_vec(d$values,paste0(p,".prior.values"),"n",arity[[d$distribution]])
    .bh_ps_vec(d$text,paste0(p,".prior.text"),"c",1L)
    if(grepl("[[:space:]]",d$text)||!grepl("^[a-z_]+\\([0-9eE+.,-]+\\)$",d$text)) .bh_pf_fail(p,"malformed captured prior text")
    opening<-regexpr("(",d$text,fixed=TRUE)[1L]
    text_name<-substring(d$text,1L,opening-1L)
    numeric_text<-substring(d$text,opening+1L,nchar(d$text)-1L)
    vals<-suppressWarnings(as.double(strsplit(numeric_text,",",fixed=TRUE)[[1L]]))
    if(!identical(text_name,d$distribution)||length(vals)!=length(d$values)||anyNA(vals)||any(vals!=d$values)) .bh_pf_fail(p,"captured prior text and stored numeric operands disagree")
    v<-d$values;dist<-d$distribution
    if((dist%in%c("normal","logistic")&&v[2L]<=0)||(dist%in%c("student_t","half_student_t")&&(v[1L]<=0||v[3L]<=0))||(dist%in%c("half_normal","lkj")&&v[1L]<=0)||(dist=="half_student_t"&&v[2L]!=0)) .bh_pf_fail(p,"captured prior has invalid distribution operands")
    if(r$class%in%c("loading","cor_raw")&&(dist!="normal"||v[1L]!=0)||r$class%in%c("sigma","sdlog","observation_sd","parameter_sd","component_sd","amplitude","total_sd")&&!dist%in%c("half_normal","half_student_t")||r$class=="Intercept"&&!dist%in%c("normal","logistic","student_t")||r$class=="cor"&&dist!="lkj"||r$class=="share"&&dist!="normal") .bh_pf_fail(p,"captured prior distribution does not match its declared role")
  }
  invisible(TRUE)
}
.bh_ps_migration <- function(x,path) {
  if(identical(.bh_pf_get(x,"kind"),"prepared_context")) {
    v<-.bh_pf_record(x,path,c("kind","from","to","source_fingerprint","source_formula_fingerprint","operation"))
    .bh_pf_string(v$from,paste0(path,".from"),"scalar-components-residual-v1")
    .bh_pf_string(v$to,paste0(path,".to"),"coefficient-map-spec-v1")
    .bh_pf_string(v$operation,paste0(path,".operation"),"B=0 scalar coordinate/coefficient map added; data, likelihoods, existing loading roles and prior values unchanged")
    .bh_pf_md5(v$source_fingerprint,paste0(path,".source_fingerprint"))
    .bh_pf_md5(v$source_formula_fingerprint,paste0(path,".source_formula_fingerprint"))
    return(invisible(TRUE))
  }
  .bh_pf_formula_migration(x,path)
}

.bh_pf_raw_maps <- function(tables,coordinates,components,path) {
  system<-unclass(.bh_pf_get(tables,"correlation_coordinate_system"))
  blocks<-unclass(.bh_pf_get(tables,"correlation_blocks"))
  cells<-unclass(.bh_pf_get(tables,"raw_correlation_coordinates"))
  K<-as.double(length(components$component));ordered<-system$ordered_global_indices
  .bh_pf_string(system$type,paste0(path,".correlation_coordinate_system.type"),c("ordered_blocks","final_marginal_mask","affine_final_marginal_labels"))
  .bh_ps_positive_index(ordered,paste0(path,".correlation_coordinate_system.ordered_global_indices"))
  if(length(ordered)!=K||any(ordered>K)||base::anyDuplicated.default(ordered))
    .bh_pf_fail(path,"raw/global coordinate order must cover the finite captured coordinate domain exactly once")
  if(system$type!="ordered_blocks") {
    if(!identical(as.double(ordered),as.double(seq_len(K)))||length(blocks$block)||length(cells$parameter))
      .bh_pf_fail(path,"final-marginal coordinates cannot carry an ordered raw-block map")
    return(invisible(TRUE))
  }
  if(length(blocks$block)!=K||!identical(as.double(blocks$ordered_index),as.double(seq_len(K)))||
     !identical(as.double(blocks$component_index),as.double(ordered))||any(blocks$component_index>K)||
     !identical(blocks$component,components$component[blocks$component_index])||
     !identical(blocks$coordinate_id,coordinates$id[blocks$component_index]))
    .bh_pf_fail(path,"raw-block coordinate labels, indices, and saved order disagree")
  block_names<-base::unique.default(blocks$block)
  B<-as.double(length(block_names));previous<-0.0;visited<-0.0
  if(any(blocks$block_index>B)||any(blocks$within_block_index>K)||any(cells$block_index>B)||
     any(cells$destination_index>K)||any(cells$previous_index>K)||
     !identical(as.double(cells$primitive_index),as.double(seq_along(cells$parameter))))
    .bh_pf_fail(path,"raw-coordinate/block indices exceed their finite captured domain")
  for(i in seq_along(block_names)) {
    at<-which(blocks$block_index==i);width<-as.double(length(at))
    if(!width||!identical(as.double(at),previous+as.double(seq_len(width)))||
       any(blocks$block[at]!=block_names[[i]])||
       !identical(as.double(blocks$within_block_index[at]),as.double(seq_len(width))))
      .bh_pf_fail(path,"raw blocks must retain one contiguous ordered group with complete local indices")
    row_at<-which(cells$block_index==i)
    count<-.bh_pf_exact_product(width,previous,path,"raw-coordinate count exceeds exact representation")
    if(length(row_at)!=count||!identical(as.double(row_at),visited+as.double(seq_len(count))))
      .bh_pf_fail(path,"raw-coordinate rows do not cover their saved destination/predecessor block")
    if(count) {
      local<-as.double(seq_len(count))-1.0
      row<-local%%width+1.0;col<-floor(local/width)+1.0
      if(!identical(as.double(cells$row[row_at]),row)||!identical(as.double(cells$col[row_at]),col)||
         any(cells$block[row_at]!=block_names[[i]])||
         !identical(as.double(cells$destination_index[row_at]),as.double(blocks$component_index[at[row]]))||
         !identical(as.double(cells$previous_index[row_at]),as.double(ordered[col]))||
         !identical(cells$destination[row_at],coordinates$id[cells$destination_index[row_at]])||
         !identical(cells$previous_whitened_coordinate[row_at],coordinates$id[cells$previous_index[row_at]]))
        .bh_pf_fail(path,"raw-coordinate local indices, global indices, and labels disagree")
    }
    previous<-previous+width;visited<-visited+count
  }
  if(visited!=length(cells$parameter)) .bh_pf_fail(path,"raw-coordinate map contains rows outside declared blocks")
  invisible(TRUE)
}
.bh_pf_raw_declaration_binding <- function(formula,tables) {
  declaration<-.bh_pf_get(formula,"correlation_coordinates")
  system<-.bh_pf_get(tables,"correlation_coordinate_system")
  raw<-identical(.bh_pf_get(system,"type"),"ordered_blocks")
  if(raw!=!is.null(declaration)) .bh_pf_fail("spec.tables","saved raw-coordinate declaration and table profile disagree")
  if(!raw)return(invisible(TRUE))
  if(!identical(as.double(.bh_pf_get(declaration,"ordered_indices")),as.double(.bh_pf_get(system,"ordered_global_indices"))))
    .bh_pf_fail("spec.tables","saved raw-coordinate order differs from its declaration")
  declared<-.bh_pf_get(declaration,"blocks");stored<-unclass(.bh_pf_get(tables,"correlation_blocks"))
  if(length(declared)!=length(base::unique.default(stored$block))||(length(declared)&&!identical(.bh_pf_names(declared),base::unique.default(stored$block))))
    .bh_pf_fail("spec.tables","saved raw-block names/order differ from their declaration")
  for(i in seq_along(declared))if(!identical(as.double(.subset2(declared,i)),as.double(stored$component_index[stored$block_index==i])))
    .bh_pf_fail("spec.tables","saved raw-block membership differs from its declaration")
  invisible(TRUE)
}

.bh_pf_reporting_binding <- function(reporting,cache,path="cache.posterior_reporting") {
  if(!identical(.bh_pf_get(reporting,"version"),"bmeas_posterior_reporting_v2"))return(invisible(TRUE))
  r<-unclass(reporting);selected<-r$selection$selected_keys;observed<-r$observed
  parameters<-.bh_pf_get(cache,"parameter_summary");hmc<-.bh_pf_get(cache,"hmc")
  if(!identical(.bh_pf_class(parameters),"data.frame")) .bh_pf_fail(path,"explicit diagnostic reporting requires its retained parameter-summary table")
  p<-unclass(parameters);M<-length(selected);C<-observed$chains
  .bh_pf_character(p$parameter_key,paste0(path,".selected_parameter_keys"),TRUE,TRUE)
  if(.bh_pf_table_shape(parameters,path)!=M||!identical(unname(p$parameter_key),unname(selected)))
    .bh_pf_fail(path,"selected-key coverage differs from the retained parameter table")
  parameter_records<-r$parameter_metrics;hmc_records<-r$hmc_metrics
  if(length(parameter_records)!=M||(M&&!identical(names(parameter_records),selected))||
     length(hmc_records)!=C||!identical(names(hmc_records),base::as.character.default(observed$chain_ids)))
    .bh_pf_fail(path,"metric records do not retain the exact selected-key/original-chain ordering")
  parameter_fields<-c("mean","median","sd","q5","q95","rhat","ess_bulk","ess_tail","mcse_mean")
  parameter_statuses<-character()
  for(n in parameter_fields) {
    .bh_pf_nullable_numeric(p[[n]],paste0(path,".parameter_summary.",n),TRUE)
    if(length(p[[n]])!=M) .bh_pf_fail(path,"parameter summary metric column has the wrong length")
  }
  for(i in seq_along(parameter_records)) {
    item<-.subset2(parameter_records,i)
    for(n in parameter_fields)if(!identical(as.double(p[[n]][i]),as.double(item$values[[n]])))
      .bh_pf_fail(path,"saved per-parameter metrics disagree with the retained parameter table")
    parameter_statuses<-c(parameter_statuses,vapply(item$metrics,function(m)m$status,character(1)))
  }
  expected<-if(!M)"not_requested"else if(all(parameter_statuses=="available"))"computed"else"computed_partial"
  if(!identical(r$phases$parameter_summary$status,expected)) .bh_pf_fail(path,"parameter-summary phase status overstates its retained metrics")
  h<-.bh_pf_record(hmc,"cache.hmc",c("chain_id","num_divergent","fraction_divergent","num_max_treedepth","fraction_max_treedepth","ebfmi"))
  .bh_pf_nullable_numeric(h$chain_id,"cache.hmc.chain_id",TRUE)
  if(length(h$chain_id)!=C||!identical(as.double(h$chain_id),as.double(observed$chain_ids)))
    .bh_pf_fail(path,"retained HMC chains differ from the recorded original-chain IDs")
  hmc_fields<-c("num_divergent","fraction_divergent","num_max_treedepth","fraction_max_treedepth","ebfmi")
  for(n in hmc_fields) {
    .bh_pf_nullable_numeric(h[[n]],paste0("cache.hmc.",n),TRUE)
    if(length(h[[n]])!=C) .bh_pf_fail(path,"retained HMC metric column does not cover every original chain")
  }
  hmc_statuses<-character()
  for(i in seq_along(hmc_records)) {
    item<-.subset2(hmc_records,i)
    for(n in hmc_fields)if(!identical(as.double(h[[n]][i]),as.double(item$values[[n]])))
      .bh_pf_fail(path,"saved per-chain metrics disagree with the retained HMC cache")
    hmc_statuses<-c(hmc_statuses,vapply(item$metrics,function(m)m$status,character(1)))
  }
  expected<-if(all(hmc_statuses=="not_applicable"))"not_applicable"else if(all(hmc_statuses%in%c("available","not_applicable")))"computed"else
    if(any(hmc_statuses=="available"))"partial"else"unavailable"
  if(!identical(r$phases$hmc$status,expected)) .bh_pf_fail(path,"HMC phase status overstates its retained per-chain metrics")
  fields<-c(conditioning="admissibility",availability_and_covariance="optional_reports",expanded_summary="expanded_correlation_matrices",numerical_messages="numerical_evaluations")
  for(n in names(fields)) {
    phase<-r$phases[[n]];current<-.bh_pf_get(cache,fields[[n]])
    if(!identical(phase$source,paste0("cache$",fields[[n]]))) .bh_pf_fail(path,"saved phase source differs from its producer cache field")
    provenance<-.bh_pf_named(phase$provenance,paste0(path,".phases.",n,".provenance"))
    if(!is.null(phase$record)&&!identical(phase$record,current)) .bh_pf_fail(path,"saved phase evidence differs from its current retained cache record")
    if(identical(phase$status,"retained_evidence")&&is.null(phase$record)) .bh_pf_fail(path,"retained phase status has no saved evidence")
    node<-.bh_pf_get(provenance,"posterior_reporting")
    history<-if(typeof(node)=="list"&&identical(.bh_pf_get(node,"availability"),"retained")) .bh_pf_get(node,"record") else NULL
    expected<-.bh_pf_retained_phase_state(current,n,history)
    if(!identical(phase$status,expected)) .bh_pf_fail(path,"saved phase status contradicts its current evidence or historical-v1 availability")
  }
  invisible(TRUE)
}


# Qualified passive producer rules, applied only to saved values. Neither
# helper creates counts, refreshes evidence, or calls numerical estimators.
.bh_pf_retained_phase_state <- function(value,name,history=NULL) {
  if(is.null(value)) {
    historical<-typeof(history)=="list"&&identical(.bh_pf_get(history,"version"),"bmeas_posterior_reporting_v1")&&
      isTRUE(tryCatch({.bh_pf_reporting(history,"saved historical-v1 phase provenance");TRUE},error=function(e)FALSE))
    return(if(historical).bh_pf_get(history,"status")else"unavailable")
  }
  status<-if(typeof(value)=="list").bh_pf_get(value,"status")else NULL
  if(typeof(status)=="character"&&!is.object(status)&&length(status)==1L&&!is.na(status)&&
     status%in%c("deferred_by_policy","uncomputed_output_resource","uncomputed"))return(status)
  if(name=="numerical_messages")
    return(if(.bh_pf_recognized_lines_retained(value))"retained_evidence"else"unavailable")
  computed<-switch(name,conditioning=c("screened_not_proven","warning"),
    availability_and_covariance=c("available","unavailable_values"),expanded_summary="available",character())
  if(typeof(status)=="character"&&!is.object(status)&&length(status)==1L&&!is.na(status)&&status%in%computed)
    "retained_evidence"else"unavailable"
}
.bh_pf_recognized_lines_retained <- function(record) {
  if(isS4(record)||typeof(record)!="list"||is.object(record))return(FALSE)
  count<-function(x)typeof(x)%in%c("integer","double")&&!isS4(x)&&!is.object(x)&&
    is.null(attr(x,"dim",exact=TRUE))&&length(x)==1L&&!is.na(x)&&is.finite(x)&&x>=0&&x==floor(x)
  string<-function(x)typeof(x)=="character"&&!isS4(x)&&!is.object(x)&&is.null(attr(x,"dim",exact=TRUE))&&
    length(x)==1L&&!is.na(x)&&validEnc(x)&&nzchar(x)
  text<-function(x)typeof(x)=="character"&&!isS4(x)&&!is.object(x)&&is.null(attr(x,"dim",exact=TRUE))&&!anyNA(x)&&all(validEnc(x))
  n<-.bh_pf_get(record,"observed_failure_messages");retained<-count(n)
  version<-.bh_pf_get(record,"schema_version")
  # The original historical schema has no capture/category semantics. A
  # retained recognized-line count alone is reported as retained evidence.
  if(is.null(version)||!identical(version,"bmeas_numerical_messages_v1"))return(retained)
  messages<-.bh_pf_get(record,"messages")
  if(retained&&text(messages)&&length(messages)!=n)retained<-FALSE
  categories<-.bh_pf_get(record,"category_counts")
  if(typeof(categories)=="list"&&!is.object(categories)) {
    nms<-.bh_pf_names(categories)
    complete<-length(categories)==2L&&!base::anyDuplicated.default(nms)&&
      .bh_pf_setequal(nms,c("structural_numerical","stan_exception"))
    if(complete) {
      values<-list(.bh_pf_get(categories,"structural_numerical"),.bh_pf_get(categories,"stan_exception"))
      if(all(vapply(values,count,logical(1)))) {
        values<-unlist(values,use.names=FALSE)
        if(count(n)&&(!is.finite(sum(values))||max(values)>n||sum(values)<n))retained<-FALSE
      }
    }
  }
  capture<-.bh_pf_get(record,"output_capture");capture_status<-NULL
  if(typeof(capture)=="list"&&!is.object(capture)) {
    candidate<-.bh_pf_get(capture,"status");reason<-.bh_pf_get(capture,"reason")
    if(string(candidate)&&candidate%in%c("succeeded","failed","unusable_output")&&string(reason))capture_status<-candidate
  }
  if(identical(capture_status,"succeeded")&&!count(n))retained<-FALSE
  if(isTRUE(capture_status%in%c("failed","unusable_output")))retained<-FALSE
  retained
}
