#' @keywords internal
adj_object <- function(x, cluster = NULL, use_vcov = TRUE, ...) {

  # all available methods of fitted model object
  fn_findmethods <- function(i) as.vector(utils::methods(class = class(x)[i]))
  all_methods <- unlist(sapply(1:length(class(x)), fn_findmethods))

  # set logLikVec
  logLikVec_exist <- paste0("logLikVec.", class(x)) %in% all_methods
  if (!any(logLikVec_exist)) {
    stop("A logLikVec method should be available for x")
  }
  fn_loglik <- function(fitted_model, pars, ...){
    return(logLikVec(fitted_model, pars = pars))
  }

  # set H
  # when use_vcov = FALSE or there exists no vcov method, H = NULL
  if (!use_vcov) {
    H <- NULL
  } else {
    vcov_exist <- paste0("vcov.", class(x)) %in% all_methods
    if (any(vcov_exist)) {
      H <- -solve(vcov(x))
    } else {
      H <- NULL
    }
  }

  # set mle and nobs
  mle <- coef(x)
  n_obs <- nobs(x)

  # set V
  if (is.null(cluster)) {
    V <- sandwich::meat(x, fitted_object = x, loglik_fn = loglik_fn, ...) * n_obs
  } else {
    V <- sandwich::meatCL(x, cluster = cluster, fitted_object = x, loglik_fn = loglik_fn, ...) * n_obs
  }

  # use chandwich package to perform loglikelihood adjustment
  res <- chandwich::adjust_loglik(loglik = fn_loglik, fitted_model = x,
                                  p = length(mle), par_names = names(mle),
                                  name = paste(class(x), collapse = "_"),
                                  mle = mle, H = H, V = V)
  class(res) <- c("labr", "chandwich", class(x))
  return(res)
}
