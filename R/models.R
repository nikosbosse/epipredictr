#' Bayesian linear regression with Stan
#'
#' @export
#' @param x Numeric vector of input values.
#' @param y Numberic vector of output values.
#' @param num_pred number of time points to forecast 
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'

linear_regression <- function(y, x = NULL, num_pred = 10, ...) {
	if (is.null(x)) x <- 1:length(y)
  standata <- list(x = x, 
  	               y = y, 
  	               N = length(y), 
  	               num_pred = num_pred)
  out <- rstan::sampling(stanmodels$linear_regression, data = standata, ...)
  return(out)
}
