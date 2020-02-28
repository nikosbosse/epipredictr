#' @title Load Wuhan Data
#'
#' @description
#' wrapper around the nCov2019 pacakge to load the Wuhan data and calculates
#' daily cases
#'
#' @return
#' a data.frame with the data
#'
#' @importFrom nCov2019 load_nCov2019
#'
#' @examples
#' NULL
#' @export 
#' @references

get_data <- function() {
  x <- nCov2019::load_nCov2019(lang = 'en')
  tmp <- subset(x['Hubei'], city == "Wuhan")
  tmp$time <- as.Date(tmp$time)
  y <- tmp[order(tmp$time), ]
  y$daily <- c(y$cum_confirm[1], diff(y$cum_confirm, 1))

  return(y)
}



#' @title Extract Predictive Samples From Stanobject
#'
#' @description
#' Extract preditive samples and bring them in a format that 
#' can be used for scoring. Function needs to be updated to 
#' deal with more than one predictive parameter?
#'
#' @param stanfitobject a stanfitobject
#' @param predictive TRUE indicates that you want to extract
#' predictive values, FALSE returns fitted values
#' 
#' @return
#' a matrix of dimension nxN with predictive samples.
#' n is the number of predicted values, N is the number of
#' MCMC samples for each value to be predicted. 
#'
#' @importFrom rstan extract
#'
#' @examples
#' NULL
#' @export 

extract_samples <- function(stanfitobject, 
							predictive = TRUE) {

	samples <- rstan::extract(stanfitobject)
	params <- names(samples)

	if (predictive == TRUE) {
		samples <- samples[grep("_pred", params)]
	} else if (predictive == FALSE) {
		samples <- samples[grep("_fit", params)]
	}
	
	if (length(samples) > 1) {}

	return(t(samples[[1]]))
}




#' @title Iteratively fit a model to the data
#'
#' @description
#' 
#' @param incidences Vector of length n with the past incidences used to 
#' fit the model and make predictions. 
#' @param n_pred prediction horizon, i.e. number of days to predict into the 
#' future
#' @param interval interval between predictions. Maybe delete?
#' @param max_n_past_obs maximum number of past observations to take into 
#' account
#'
#' @return
#' a data.frame with the data
#'
#' @importFrom nCov2019 load_nCov2019
#'
#' @examples
#' NULL
#' @export 
#' @references




fit_iteratively <- function(incidences, 
							n_pred = 14, 
							interval = NULL,
							start_period = 15,
							max_n_past_obs = Inf,
							model = NULL,
							...) {

	## track time
	time <- Sys.time()

	if (is.null(interval)) interval <- n_pred

	## calculate number of fits to do
	total_n <- (length(incidences))
	n_runs <- ceiling((total_n - (start_period - 1)) / interval)

	stanfitobjects <- list()
	predictive_samples <- list()

	## do fitting
	i <- 0
	current_last_obs <- start_period - 1

	while (current_last_obs < total_n){
		cat("run ", as.character(i + 1), "of ", as.character(n_runs), "\n")
		cat(as.character(current_last_obs))
		## determine current indices and make data
		index <- 1:current_last_obs
		if ((length(index)) > total_n) { 
			index <- 1:total_n
		}
		if (length(index) > max_n_past_obs) {
			current_earliest_obs <- length(index) - max_n_past_obs + 1
			index <- index[current_earliest_obs:current_last_obs]
		}

		y <- incidences[index]

		stanfit <- epipredictr::linear_regression(y = y, 
												  num_pred = n_pred, 
												  x = 1:length(y))

		i <- i + 1
		stanfitobjects[[i]] <- stanfit
		predictive_samples[[i]] <- epipredictr::extract_samples(stanfit)
		current_last_obs <- current_last_obs + interval
	}
    predictive_samples <- do.call("rbind", predictive_samples)

    last_n_to_display <- nrow(predictive_samples) - (current_last_obs - total_n)
	print(time - Sys.time())
	return(predictive_samples[1:last_n_to_display, ])
}