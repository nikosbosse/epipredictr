#' Bayesian linear regression with Stan
#'
#' @export
#' @param x Numeric vector of input values.
#' @param y Numberic vector of output values.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'

linear_regression <- function(x, y, num_pred = 10, ...) {
  standata <- list(x = x, 
  	               y = y, 
  	               N = length(y), 
  	               num_pred = num_pred)
  out <- rstan::sampling(stanmodels$linear_regression, data = standata, ...)
  return(out)
}
