# Methods for class betareg

#' @export
logLikVec.betareg <- function(object, pars = NULL, ...) {
  if (!missing(...)) {
    warning("extra arguments discarded")
  }

  # Extract the estimated parameters from the fitted object
  all_pars <- coef(object, phi = TRUE)

  # If pars is provided, then use its value to define parameters
  if (!is.null(pars)) {
    object$coefficients$mean <- pars[-length(pars)]
    object$coefficients$precision <- pars[length(pars)]
  }

  # Calculate the loglikelihood contributions from specific observations
  mu <- suppressWarnings(betareg::predict(object, newdata = object$model, type = "response"))
  phi <- betareg::predict(object, newdata = object$model, type = "precision")
  if (any(phi <= 0)) {
    val <- -Inf
  } else {
    val <- stats::dbeta(as.numeric(object$y), shape1 = mu * phi, shape2 = phi - mu * phi, log = TRUE)
  }

  # Some attributes for the loglikelihood object
  attr(val, "nobs") <- nobs(object)
  attr(val, "df") <- length(all_pars)
  class(val) <- "logLikVec"
  return(val)
}

