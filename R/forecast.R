#' @title Do the full analysis
#'
#' @description
#' Missing.
#' Also Todo: handling for only one item
#' @param y
#'
#' @return
#' Missing
#' @examples
#' NULL
#' @export

full_analysis <- function(data) {

	inputdata <- data$inputdata
	regions <- as.character(unique(inputdata$region))
	models <- data$models

	# analysis by region ============================================= #
	## collect results country-wise
	all_region_results <- furrr::future_map(seq_along(regions),
		function(i) {
				  	out <- tryCatch(
				  		{
				  			cat("start analysis ", 
				  				as.character(i), " (", regions[i],
				  				") ",  
				  				"of ", as.character(length(regions)), 
				  				"\n", sep = "")
				  			analysis_one_region(region = regions[i],
				  								data = data)
				  		}, 
				  		error = function(cond) {
				  			warning(cond)
				  			warning(paste("Issue with region", regions[i]))
				  			return(NULL)
				  		}
				  	)
				  	return(out)
				  }, .progress = TRUE)

	failure <- lapply(all_region_results, is.null)
	failure <- do.call(c, failure)

	all_region_results <- all_region_results[!failure]
	regions <- regions[!failure]
	names(all_region_results) <- regions
	## ================================================================ #

	## aggregate all region results into one data.frame
	full_results <- lapply(all_region_results, 
						   function(x) {
						 	  return(x[["complete_region_results"]])
						   })
	full_results <- do.call(rbind, full_results)
	rownames(full_results) <- NULL


		out <- list(regions = regions,
				inputdata = inputdata,
				all_region_results = all_region_results,
				full_predictive_samples = full_results)

	return(out)
}



#' @title Do entire analysis for one country
#'
#' @description
#' Missing.
#' Also Todo: handling for only one item
#' @param y
#'
#' @return
#' Missing
#' @examples
#' NULL
#' @export
#'

analysis_one_region <- function(data, region = NULL, plot = F) {

	models <- data$models
	inputdata <- data$inputdata
	dates = inputdata[inputdata$region == region,	
			      colnames(inputdata) == "date"]	

	## do forecasting
	# if (isTRUE(include_stan)) {
	# }
	region_results <- list()
	for (model in models) {
		name <- paste("bsts_", model, sep = "")
		region_results[[name]] <- fit_iteratively(data, 
												  region = region, 
									      		  model = model, 
									      		  fit_type = "bsts_package")
	}

	# region_results <- add_average_model(region_results)

	##aggregate region results into one data.frame
	complete_region_results <- lapply(region_results, 
				 					  function(x) {
									  	 return(x[["predictive_samples"]])
									  })

	complete_region_results <- do.call(rbind, complete_region_results)

	rownames(complete_region_results) <- NULL
	# eventually the add_average_model should be moved behind this probably

	# if(isTRUE(plot)) {
	# 	y <- inputdata[inputdata$region == country, 
	# 			   colnames(inputdata) == "median"]
	# 	compare_bsts_models(y)
	# }




	return(list(region = region, 
			    region_results = region_results,
				#forecast_plot_region = forecast_plot_region, 
			    complete_region_results = complete_region_results))
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


fit_iteratively <- function(data,
							region,
							in_between_pred = T,
							max_n_past_obs = Inf,
							model = "local",
							n_samples = 4000,
							vb = FALSE,
							fit_type = "bsts_package",
							length_local_trend = 7,
							iter = 4000,
							...) {

	inputdata <- data$inputdata
	y = inputdata$y[inputdata$region == region]
	dates = inputdata$date[inputdata$region == region]
	n_pred = data$n_pred
	start_period = data$start_period
	total_n <- length(y)

	## initialize empty lists to hold the results
	predictive_samples <- list()
	forecast_run <-list()
	stanfitobjects <- list()

	current_last_obs <- start_period - 1
	if (current_last_obs > total_n) {
		stop("start_period later than length of series")
	}

	run <- 1
	for (curr_last_data_point in current_last_obs:total_n) {
		
		## determine current indices
		index <- 1:curr_last_data_point
		if (length(index) > max_n_past_obs) {
			current_earliest_obs <- length(index) - max_n_past_obs + 1
			index <- index[current_earliest_obs:current_last_obs]
		}

		# get current data and corresponding dates
		y_curr <- y[index]
		curr_dates <- dates[index]

		if (fit_type == "stan") {
			# stanfit <- fit_stan_model(y_curr, model, n_pred = n_pred, vb = vb,
			# 					      length_local_trend = length_local_trend,
			# 					      iter = iter)
			# stanfitobjects[[i]] <- stanfit
			# predictive_samples[[i]] <- extract_samples(stanfit)
		} else {
			predictive_samples[[run]] <- bsts_wrapper(y = y_curr,
													dates = curr_dates,
													model = model,
													num_pred  = n_pred)

		}
		forecast_run[[run]] <- rep(run, n_pred)

		run <- run + 1
	}

    predictive_samples <- do.call("rbind", predictive_samples)

    forecast_run <- do.call("c", forecast_run)

    predictive_samples <- cbind(forecast_run = forecast_run, 
    							region = region, 
								model = model, 
								type = "predicted",
								predictive_samples)

	return(list(predictive_samples = predictive_samples,
		        forecast_run = forecast_run,
		        stanfitobjects = stanfitobjects,
		    	y = y))

}



#' @title Fit model
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

predict_with_model <- function(y, model, num_pred, stan = F) {

	if (isTRUE(stan)) {

	} else {
		return(bsts_wrapper(y, model, num_pred = num_pred))
	}
}




#' @title Wrapper around the functions from the bsts package
#'
#' @description
#' Missing.
#' Also Todo: handling for only one item
#' @param dates The dates for the data that is uses for the prediction.
#' We need the last date in order to specify the date for which the predictions
#' are made
#'
#' @return
#' Missing
#' @examples
#' NULL
#' @export

bsts_wrapper <- function(y, model,
						 num_pred = NULL,
						 dates = dates,
						 n_iter = 2000) {
	if (model == "semilocal") {
		ss <- AddSemilocalLinearTrend(list(), y)
	} else if (model == "local"){
		ss <- AddLocalLinearTrend(list(), y)
	} else if (model == "local_student"){
		ss <- AddStudentLocalLinearTrend(list(), y)
	} else if (model == "ar1"){
		ss <- AddAr(list(), y, lags = 1)
	} else if (model == "ar2"){
		ss <- AddAr(list(), y, lags = 2)
	}

	bsts.model <- bsts::bsts(y, state.specification = ss, niter = 1000, ping=0)
	#burn <- SuggestBurn(0.1, bsts.model)
	p <- predict.bsts(bsts.model, horizon = num_pred, burn = 100, quantiles = c(.025, .975))
	
	predictive_samples <- as.data.frame(t(p$distribution))
	colnames(predictive_samples) <- paste("sample", 1:ncol(predictive_samples))
	
	days_ahead <- 1:nrow(predictive_samples)
	last_date <- dates[length(dates)]
	predicted_date <- last_date + days_ahead
	predictive_samples <- cbind(date = predicted_date, 
	                            days_ahead = days_ahead, 
	                            predictive_samples)
	
	return(predictive_samples)

}









#' @title Do very basic model averaging
#'
#' @description
#' Missing. 
#' Also Todo: handling for only one item 
#' @param y 
#' 
#' @return
#' Missing
#' @examples
#' NULL
#' @export 



add_average_model <- function(region_results) {
	tmp <- lapply(seq_along(region_results), 
				  FUN = function(i) {
				  	p <- region_results[[i]]
				  	p <- p$predictive_samples
				  	p <- p[, grep("sample", colnames(p))]
				  	return(p)
				  })
	pred <- tmp[[1]]
	for (i in 2:length(region_results)) {
		pred <- pred + tmp[[i]] 
	}
	avg <- pred / length(region_results)

	## add columns with region, model, date and days ahead
	avg <- cbind(region_results[[1]]$predictive_samples$forecast_run, 
				 region_results[[1]]$predictive_samples$region, 
				 region_results[[1]]$predictive_samples$model, 
				 region_results[[1]]$predictive_samples$predicted_date,
				 region_results[[1]]$predictive_samples$days_ahead,  	
				 avg)
	colnames(avg)[1:5] <- c("forecast_run", "region", 
							"model", "predicted_date", "days_ahead")
    							

	region_results$average$predictive_samples <- avg
	region_results$average$y <- region_results[[1]]$y
	region_results$average$forecast_run <- region_results[[1]]$forecast_run
	return(region_results)
}


# add_stacked_model <- function(region_results) {


# 	models <- names(region_results)

# 	scores <- sapply()


# 	model <- "bsts_local"

# 	# get scores across all models
# 	scores <- 0
# 	for (model in models) {

# 		model_results <- region_results[[model]]
# 		forecast_run <- model_results$forecast_run
# 		predictive_samples <- model_results$predictive_samples
# 		y <- model_results$y

# 		runs <- unique(forecast_run[!is.na(forecast_run)])
# 		index <- forecast_run == runs[1]
# 		index[is.na(index)] <- FALSE
		
# 		crps <- scoringutils::crps(y[index], predictive_samples[index,])
		
# 		scores <- cbind(scores, crps)
# 	} 
# 	colnames(scores) <- c("y", models)
# 	scores <- as.data.frame(scores)

# 	lm(y ~ . , data = scores)

# 	lm(scores)
	

# }





