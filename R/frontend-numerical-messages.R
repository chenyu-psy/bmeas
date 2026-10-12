.bh_capture_numerical_messages <- function(backend) {
  record<-list(schema_version="bmeas_numerical_messages_v1",
    observed_failure_messages=NA_integer_,messages=character(),
    output_capture=list(status="failed",reason="backend output capture failed; recognized numerical-message lines are unavailable"),
    recognition_rule="structural_or_literal_Stan_Exception_v1",
    category_counts=list(structural_numerical=NA_integer_,stan_exception=NA_integer_),
    category_counts_may_overlap=TRUE,count_unit="captured_text_line_occurrence",
    coverage="incomplete",total_failed_evaluations=NA_integer_,
    count_scope="recognized captured text-line occurrences across printed and returned output, including repeated text and channel duplication; not distinct failed evaluations; backend output may omit chains or suppressed messages")
  captured<-tryCatch({
    returned<-NULL
    printed<-utils::capture.output(returned<-backend$output())
    list(printed=printed,returned=returned)
  },interrupt=function(e)stop(e),error=function(e)NULL)
  # Keep error metadata plain: unusual conditions must not dispatch conditionMessage().
  if(is.null(captured))return(record)
  plain_text<-function(x)typeof(x)=="character"&&!is.object(x)&&!isS4(x)&&
    all(names(attributes(x))%in%"names")&&!anyNA(x)&&all(validEnc(x))
  returned<-captured$returned
  usable<-is.null(returned)||plain_text(returned)||
    (typeof(returned)=="list"&&!is.object(returned)&&!isS4(returned)&&
      all(names(attributes(returned))%in%"names")&&all(vapply(returned,plain_text,logical(1))))
  if(!plain_text(captured$printed)||!usable) {
    record$output_capture<-list(status="unusable_output",
      reason="backend output returned unusable text; expected NULL, a plain character vector, or a plain one-level list of character vectors with valid encodings and no missing values")
    return(record)
  }
  # Split both channels alike; each retained occurrence remains distinct, even if equal.
  output<-c(captured$printed,unlist(returned,use.names=FALSE))
  output<-c(character(),unlist(strsplit(output,"\r\n|\n|\r",perl=TRUE),use.names=FALSE))
  structural<-grepl("correlation chart|correlation basis|cholesky_decompose|share logit|share normalization|active share|derived share|derived residual|component SD cannot be represented|effective loading cannot be represented|required task",output)
  stan_exception<-grepl("Exception:",output,fixed=TRUE)
  recognized<-structural|stan_exception
  record$observed_failure_messages<-as.integer(sum(recognized))
  record$messages<-output[recognized]
  record$output_capture<-list(status="succeeded",
    reason="the existing backend output method returned usable text; coverage remains incomplete")
  record$category_counts<-list(structural_numerical=as.integer(sum(structural)),
    stan_exception=as.integer(sum(stan_exception)))
  record
}
