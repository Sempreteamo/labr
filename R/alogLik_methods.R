#' @export
logLikVec <- function(object, ...) {
  UseMethod("logLikVec")
}

#' @export
alogLik <- function(x, cluster = NULL, use_vcov = TRUE, ...) {
  UseMethod("alogLik")
}

#' Sum loglikelihood contributions from individual observations
#'
#' S3 logLik method for logLikVec objects
#'
#' @param object An object of class \code{"logLikVec"} return from a
#'   \code{logLikVec} method.
#' @param ... Further arguments.
#' @export
logLik.logLikVec <- function(object, ...) {
  save_attributes <- attributes(object)
  object <- sum(object)
  attributes(object) <- save_attributes
  class(object) <- "logLik"
  return(object)
}
