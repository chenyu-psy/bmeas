# Passive interpretation of optional, already-retained numerical messages.
# The original domain record is never changed or completed from other sources.
.bh_report_numerical <- function(domain) {
  plain_list <- function(x) {
    if(typeof(x)!="list"||is.object(x)||!is.null(dim(x)))return(FALSE)
    n<-names(x)
    !length(x)||(is.character(n)&&!is.object(n)&&is.null(dim(n))&&length(n)==length(x)&&!anyNA(n)&&all(validEnc(n))&&
      !anyDuplicated(n)&&all(nzchar(n)))
  }
  string <- function(x) typeof(x)=="character"&&!is.object(x)&&is.null(dim(x))&&
    length(x)==1L&&!is.na(x)&&validEnc(x)&&nzchar(x)
  count <- function(x) typeof(x)%in%c("integer","double")&&!is.object(x)&&
    is.null(dim(x))&&length(x)==1L&&!is.na(x)&&is.finite(x)&&x>=0&&x==floor(x)
  unavailable_count <- function(x) typeof(x)%in%c("integer","double")&&!is.object(x)&&
    is.null(dim(x))&&length(x)==1L&&is.na(x)&&!is.nan(x)
  text <- function(x) typeof(x)=="character"&&!is.object(x)&&is.null(dim(x))&&!anyNA(x)&&all(validEnc(x))
  slot <- function(x,n) .subset2(x,n)
  absent <- function(reason)list(availability="unavailable_in_cache",reason=reason)
  invalid <- function(reason)list(availability="unavailable_invalid_metadata",reason=reason)
  out<-list(version="bmeas_numerical_message_report_v1",
    recognized_lines=c(list(value=NA_real_),absent("A recognized captured-line count was not retained")),
    output_capture=c(list(status="unknown"),absent("Capture provenance was not retained")),
    recognition=c(list(rule=NA_character_),absent("Recognition provenance was not retained")),
    category_counts=c(list(values=NULL),absent("Message categories were not retained")),
    message_text=absent("Recognized message text was not retained"),
    count_unit="captured_text_line_occurrence",category_counts_may_overlap=TRUE,
    coverage="incomplete",total_failed_evaluations=list(availability="unknown",value=NA_real_,
      reason="Captured message lines do not enumerate all failed numerical evaluations"),
    provenance="unavailable_in_historical_schema",issues=character())
  issue <- function(reason)out$issues<<-c(out$issues,reason)
  bad_count <- function(reason) {
    out$recognized_lines<<-c(list(value=NA_real_),invalid(reason));issue(reason)
  }
  if(is.null(domain))return(out)
  if(!plain_list(domain)) {
    bad_count("Optional numerical-evidence domain is not an unambiguous plain list")
    return(out)
  }
  availability<-slot(domain,"availability")
  if(identical(availability,"unavailable_in_cache"))return(out)
  if(identical(availability,"unavailable_invalid_metadata")) {
    reason<-slot(domain,"reason")
    bad_count(if(string(reason))reason else "Optional numerical evidence has invalid metadata")
    return(out)
  }
  if(!string(availability)||!identical(availability,"retained")) {
    bad_count("Optional numerical-evidence availability metadata is malformed")
    return(out)
  }
  record<-slot(domain,"record")
  if(is.null(record))return(out)
  if(!plain_list(record)) {
    bad_count("Optional numerical-evidence record is not an unambiguous plain list")
    return(out)
  }
  n<-slot(record,"observed_failure_messages")
  if(count(n))out$recognized_lines<-list(availability="retained",value=n,
    reason="Retained recognized-message line count; not a total failure or proposal count")else
    if(!is.null(n)&&!unavailable_count(n))bad_count("Recognized-message count is not a plain nonnegative integer-valued scalar")
  messages<-slot(record,"messages")
  if(!is.null(messages)) {
    if(text(messages))out$message_text<-list(availability="retained",reason="Original recognized text remains in the unchanged domain record")else {
      out$message_text<-invalid("Optional recognized-message text is not a plain character vector")
      issue(out$message_text$reason)
    }
  }
  version<-slot(record,"schema_version")
  if(is.null(version))return(out)
  if(!string(version)||!identical(version,"bmeas_numerical_messages_v1")) {
    out$provenance<-"unavailable_unrecognized_schema"
    issue("Optional numerical-message schema is malformed or unrecognized; no modern capture or recognition rule is inferred")
    return(out)
  }
  out$provenance<-"versioned_record"
  rule<-slot(record,"recognition_rule")
  if(string(rule)&&identical(rule,"structural_or_literal_Stan_Exception_v1"))
    out$recognition<-list(availability="retained",rule=rule,
      reason="Positional union of the structural patterns and literal Exception:; repeated occurrences are retained")else {
    out$recognition<-c(list(rule=NA_character_),invalid("Optional recognition rule is absent, malformed or unrecognized"))
    issue(out$recognition$reason)
  }
  capture<-slot(record,"output_capture")
  capture_status<-NULL
  if(plain_list(capture)) {
    candidate<-slot(capture,"status");reason<-slot(capture,"reason")
    if(string(candidate)&&candidate%in%c("succeeded","failed","unusable_output")&&string(reason)) {
      capture_status<-candidate
      out$output_capture<-list(availability="retained",status=candidate,reason=reason)
    }
  }
  if(is.null(capture_status)) {
    out$output_capture<-c(list(status="unknown"),invalid("Optional output-capture metadata is absent, ambiguous or malformed"))
    issue(out$output_capture$reason)
  }
  categories<-slot(record,"category_counts")
  category_values<-NULL
  category_names<-c("structural_numerical","stan_exception")
  if(plain_list(categories)&&length(categories)==2L&&setequal(names(categories),category_names)) {
    values<-lapply(category_names,function(name)slot(categories,name));names(values)<-category_names
    if(all(vapply(values,count,logical(1))))category_values<-values
  }
  if(!is.null(category_values)) {
    out$category_counts<-list(availability="retained",values=category_values,
      reason="Category line counts may overlap; do not add them as disjoint events")
  }else if(isTRUE(capture_status%in%c("failed","unusable_output"))&&plain_list(categories)&&
           length(categories)==2L&&setequal(names(categories),category_names)&&
           all(vapply(categories,unavailable_count,logical(1)))) {
    out$category_counts<-list(availability="unavailable_capture",values=NULL,
      reason="Capture was unavailable; no category count was manufactured")
  }else {
    out$category_counts<-c(list(values=NULL),invalid("Optional category counts are absent, ambiguous or malformed"))
    issue(out$category_counts$reason)
  }
  unit<-slot(record,"count_unit");overlap<-slot(record,"category_counts_may_overlap")
  if(!string(unit)||!identical(unit,"captured_text_line_occurrence")||!identical(overlap,TRUE)) {
    issue("Optional count-unit or category-overlap metadata contradicts the recognized schema")
    out$recognition<-c(list(rule=NA_character_),invalid(tail(out$issues,1L)))
    out$category_counts<-c(list(values=NULL),invalid(tail(out$issues,1L)))
  }
  # These arithmetic contradictions are independent of optional capture metadata.
  # Missing/malformed provenance never makes incompatible retained counts agree.
  if(count(n)&&text(messages)&&length(messages)!=n)
    bad_count("Recognized-line count contradicts its retained message occurrences")
  if(count(n)&&!is.null(category_values)) {
    values<-unlist(category_values,use.names=FALSE)
    if(!is.finite(sum(values))||max(values)>n||sum(values)<n) {
      bad_count("Recognized-line count and overlapping category counts are contradictory")
      out$category_counts<-c(list(values=NULL),invalid("Category counts contradict the positional-union count"))
    }
  }
  if(identical(capture_status,"succeeded")) {
    if(!count(n))bad_count("Successful capture has no valid recognized-line count; no zero is inferred")
  }else if(isTRUE(capture_status%in%c("failed","unusable_output"))) {
    if(count(n)||(!is.null(n)&&!unavailable_count(n))) {
      bad_count("Unavailable capture contradicts a supplied recognized-line count")
    }else out$recognized_lines<-list(availability="unavailable_capture",value=NA_real_,
      reason="Capture was unavailable; no recognized-line count was manufactured")
    if(!is.null(category_values)) {
      out$category_counts<-c(list(values=NULL),invalid("Unavailable capture contradicts supplied category counts"))
      issue(out$category_counts$reason)
    }
    if(text(messages)&&length(messages)) {
      out$message_text<-invalid("Unavailable capture has contradictory recognized-message text")
      issue(out$message_text$reason)
    }
  }
  coverage<-slot(record,"coverage");total<-slot(record,"total_failed_evaluations")
  if(!identical(coverage,"incomplete")||!unavailable_count(total))
    issue("Optional coverage/total metadata cannot establish complete evaluation counts; total remains unknown")
  out
}

.bh_print_numerical_report <- function(x) {
  cat("Numerical evaluation evidence: incomplete | total failed evaluations: unknown\n")
  cat("  Recognized cached numerical-message lines:",.bh_report_display(x$recognized_lines$value),
    "|",x$recognized_lines$availability,"\n")
  cat("  Output capture:",x$output_capture$status,"| recognition provenance:",x$recognition$availability,"\n")
  if(identical(x$category_counts$availability,"retained"))
    cat("  Captured-line categories (may overlap): Stan Exception:",x$category_counts$values$stan_exception,
      "| structural numerical:",x$category_counts$values$structural_numerical,"\n")
  if(length(x$issues))cat("  Optional numerical metadata:",paste(unique(x$issues),collapse="; "),"\n")
  invisible(x)
}
