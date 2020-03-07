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
#' NULL


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
							type = "predictive") {

	samples <- rstan::extract(stanfitobject)
	params <- names(samples)

	if (type == "predictive") {
		samples <- samples[grep("_pred", params)]
	} else if (type == "prior") {
		samples <- samples[grep("_prior", params)]
	} else if (type == "fit") {
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
#' NULL




fit_iteratively <- function(incidences, 
							n_pred = 14, 
							interval = NULL,
							start_period = 15,
							max_n_past_obs = Inf,
							model = "bsts",
							n_samples = 4000,
							vb = FALSE,
							length_local_trend = 7,
							...) {
	## track time
	time <- Sys.time()

	if (is.null(interval)) interval <- n_pred

	## calculate number of fits to do
	total_n <- (length(incidences))
	n_runs <- ceiling((total_n - (start_period - 1)) / interval)

	## initialize empty lists to hold the results
	stanfitobjects <- list()
	predictive_samples <- list()
	forecast_run <-list()

	## do iterative fitting
	i <- 0
	current_last_obs <- start_period - 1
	if (current_last_obs > total_n) stop("start_period later than length of series")

	while (current_last_obs < total_n){
		cat("run ", as.character(i + 1), "of ", as.character(n_runs), "\n")

		## determine current indices and make data
		index <- 1:current_last_obs
		if (length(index) > max_n_past_obs) {
			current_earliest_obs <- length(index) - max_n_past_obs + 1
			index <- index[current_earliest_obs:current_last_obs]
		}
		y <- incidences[index]

		stanfit <- fit_stan_model(y, model, n_pred = n_pred, vb = vb)

		## store results
		i <- i + 1
		stanfitobjects[[i]] <- stanfit
		predictive_samples[[i]] <- extract_samples(stanfit)
		forecast_run[[i]] <- rep(i, nrow(predictive_samples[[i]]))
										 
		current_last_obs <- current_last_obs + interval
	}


    predictive_samples <- do.call("rbind", predictive_samples)
    forecast_run <- do.call("c", forecast_run)

    ## cap predictions to match the length of the 
    ## true_values minus the starting data
    last_n_to_display <- nrow(predictive_samples) - (current_last_obs - total_n)
	predictive_samples <- predictive_samples[1:last_n_to_display, ]
    forecast_run <- forecast_run[1:last_n_to_display]

	## do another last prediction into the future and store results
	index <- 1:total_n
		if (length(index) > max_n_past_obs) {
			current_earliest_obs <- length(index) - max_n_past_obs + 1
			index <- index[current_earliest_obs:total_n]
		}
	y <- incidences[index]

	stanfit <- fit_stan_model(y, model, 
							  n_pred = n_pred, 
							  vb = vb, 
							  length_local_trend = length_local_trend)
	i <- i + 1
	stanfitobjects[[i]] <- stanfit
	predictive_samples <- rbind(predictive_samples, 
								extract_samples(stanfit))
	forecast_run <- c(forecast_run, rep(i, n_pred))


    ## add NAs to the predictions and the true_values
    predictive_samples <- rbind(matrix(NA, nrow = start_period - 1, 
    									   ncol = ncol(predictive_samples)), 
								predictive_samples)
    y <- c(incidences, rep(NA, n_pred))
    forecast_run <- c(rep(NA, start_period - 1), forecast_run)


	print(Sys.time() - time)
	return(list(predictive_samples = predictive_samples, 
		        forecast_run = forecast_run, 
		        stanfitobjects = stanfitobjects, 
		    	y = y)) 

}



#' @title Fit Stan model
#'
#' @description
#' Wrapper around different lower level fit functions
#' @param y Vector of length n with the true values
#' fit the model and make predictions. 
#' @param model Missing
#' 
#' @return
#' Missing
#' @examples
#' NULL
#' @export 

fit_stan_model <-function(y, model, n_pred, vb, length_local_trend, ...) {

	if (class(model) == "character" && model == "lin_reg") {
		stanfit <- epipredictr::linear_regression(y = y, 
												  num_pred = n_pred, 
												  x = 1:length(y))
		} else if (class(model) == "character" && model == "bsts") {
			stanfit <- epipredictr::bsts(y = y,
							num_pred = n_pred, 
							prior_var_phi = 0.5)
		} else if (class(model) == "character" && model == "bsts_local_trend") {
			stanfit <- bsts_local_trend(y = y,
							num_pred = n_pred, 
							length_local_trend = length_local_trend,
							prior_var_phi = 0.5)
		} else {
			l <- list(y = y, N = length(y), 
					  n_pred = n_pred, prior_var_phi = 0.5, x = 1:length(y),
					  length_local_trend = 3, num_pred = n_pred)
			stanfit <- rstan::sampling(model, data = l, 
                        	iter = 4000, warmup = 800, thin = 1, 
                        	control = list(adapt_delta = 0.97))
		}
}





#' @title Plot predictive samples vs. true values
#'
#' @description
#' Missing. 
#' Also Todo: add an option for point estimates
#' get the forecast_run thing tidy
#' @param y_true Vector of length n with the true values
#' fit the model and make predictions. 
#' @param y_pred_samples predictive samples
#' @param forecast_run vector indicating which forecast run an 
#' esimtate belongs to
#' 
#' @return
#' Missing
#' @examples
#' NULL
#' @export 



plot_pred_vs_true <- function(y_true, 
							  y_pred_samples,
							  forecast_run = NULL){
	pred_mean <- rowMeans(y_pred_samples)
	pred_median <- apply(y_pred_samples, median, MARGIN = 1)
	pred_quantiles <- t(apply(y_pred_samples, 
								MARGIN = 1, 
								FUN = quantile, 
								probs = c(0.025, 0.25, 0.75, 0.975), 
							    na.rm = TRUE))

	df <- as.data.frame(cbind(y_true, pred_median, 
							  pred_mean, pred_quantiles, forecast_run))
	colnames(df) <- c("true", "median", "mean", "ci2.5", 
					  "ci25", "ci75", "ci97.5", "forecast_run")

	start_data <- sum(is.na(forecast_run))
	interval <- sum(forecast_run == 1, na.rm = TRUE)

	## make vertical lines to display
	x <- unique(forecast_run)
	seq <- rep(NA, length(x) - 1)
	for (i in 1:(length(x) - 1)) {
		seq[i] <- which(forecast_run == x[i + 1] )[1] - 1
	}
	vlines <- rep(NA, nrow(df))
	vlines[seq] <- seq


	plot <- ggplot(df, aes(x = 1:nrow(df)), group = forecast_run) +
			geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5), alpha = 0.3, 
						fill = "lightblue") +
			geom_ribbon(aes(ymin = ci25, ymax = ci75), alpha = 0.9, 
						fill = "lightblue") + 
			geom_line(aes(y = median), color = "blue") +
			geom_line(aes(y = true)) +
			geom_vline(aes(xintercept = vlines))
	return(plot)
}



