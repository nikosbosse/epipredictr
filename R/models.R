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


#' BSTS with Stan
#'
#' @export
#' @param y Numeric vector of output values, i.e. vector of a series of R_t values
#' @param num_pred number of time points to forecast 
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'

bsts <- function(y, num_pred = 10, ...) {
  standata <- list(y = y, 
  	               N = length(y), 
  	               n_pred = num_pred, 
                   prior_var_phi = 0.1)

  out <- rstan::sampling(stanmodels$bsts, data = standata, ...)
  return(out)
}
