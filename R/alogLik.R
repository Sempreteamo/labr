# Loglikelihood adjustment for beta regression fits

#' @export
alogLik.default <- function(x, cluster = NULL, use_vcov = TRUE, ...) {
  # list of betareg objects supported
  supported_by_labr <- list(betareg = c("betareg"))

  # determine whether the betareg object x has a supported class
  is_supported <- NULL
  for (i in 1:length(supported_by_labr)) {
    is_supported[i] <- identical(class(x), unlist(supported_by_labr[i], use.names = FALSE))
  }
  if (!any(is_supported)) {
    stop(paste("x's class", deparse(class(x)), "is not supported"))
  }

  # set the class
  name_of_class <- names(supported_by_labr)[which(is_supported)]
  class(x) <- name_of_class

  # call adj_oject() to adjust the loglikelihood
  res <- adj_object(x, cluster = cluster, use_vcov = use_vcov, ...)
  class(res) <- c("labr", "chandwich", "betareg")
  return(res)
}
