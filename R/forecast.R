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
#	timeseries <- data$timeseries
	countries <- as.character(unique(inputdata$region))
	models <- data$models


	# analysis by country ============================================= #
	## collect results country-wise
	res <- lapply(seq_along(countries),
				  FUN = function(i) {
				  	out <- tryCatch(
				  		{
				  			analysis_one_country(country = countries[i],
				  								data = data)
				  		}, 
				  		error = function(cond) {
				  			warning(cond)
				  			warning(paste("Issue with region", countries[i]))
				  			return(NULL)

				  		}
				  	)
				  	return(out)
				  })

	failure <- lapply(res, is.null)
	failure <- do.call(c, failure)

	res <- res[!failure]
	countries <- countries[!failure]
	names(res) <- countries

	

	## ================================================================ #

	## scoring ================================================== #
	## do aggregated scoring
	scoring <- list()

	tables <- lapply(res, '[[', 3)
	tables <- lapply(seq_along(tables),
 					 FUN = function(i) {
 					 	cbind(country = countries[i], tables[[i]])
 					 })
	table <- do.call(rbind, tables)

	scoring$across_country_scores <- table

	## rank scores by mean and median
	table_mean <- aggregate(mean ~ method + model, table, mean)
	table_mean <- table_mean[order(table_mean$method, table_mean$mean), ]
	scoring$scores_ranked_mean <- table_mean

	table_median <- aggregate(mean ~ method + model, table, mean)
	table_median <- table_median[order(table_mean$method, table_mean$mean), ]
	scoring$scores_ranked_median <- table_median

	## plot scores across countries
	scoring$lineplot <- ggplot(data = table,
		   	aes(y = mean, color = (model), x = country, group = model)) +
	  		geom_point() +
	  		geom_line() +
	  		facet_wrap(~ method, ncol = 1, scales = "free_y") +
	  		theme(text = element_text(family = 'Serif'))

	scoring$boxplot <- ggplot(data = table,
		   	aes(y = mean, color = (model), x = country, group = model)) +
	  		geom_boxplot() +
	  		facet_wrap(~ method, ncol = 1, scales = "free_y") +
	  		theme(text = element_text(family = 'Serif'))
	## ========================================================== #




	## find best model and make separate plots ================== #
	best <- as.character(table_mean[1, colnames(table_mean) == "model"])

	predictions_best <- list()
	for (country in countries) {

		predictive_samples <- res[[country]]$forecast_res[[best]]$predictive_samples
		y <- res[[country]]$forecast_res[[best]]$y

		forecast_run <- res[[country]]$forecast_res[[best]]$forecast_run

		predictive_samples[!is.na(y), ] <- NA

		t <- paste(country, best, sep = "_")
		p <- plot_pred_vs_true(y_true = y,
							   y_pred_samples = predictive_samples,
							   forecast_run = forecast_run, vlines = F,
						  	   plottitle = t)

		predictions_best[[country]] <- p
	}


	## ========================================================== #



	out <- list(countries = countries,
				inputdata = inputdata,
				country_results = res,
				scoring = scoring,
				predictions_best = predictions_best)

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

analysis_one_country <- function(data, country = "country", plot = F) {
	
	analysis <- list()
	inputdata <- data$inputdata
	y <- inputdata[inputdata$region == country, 
				   colnames(inputdata) == "median"]
	analysis$country <- country
	models <- data$models

	## do forecasting
	analysis$forecast_res <- forecast_one_country(data = data, country = country, include_stan = F)
	analysis$forecast_res <- add_average_model(analysis$forecast_res)


	## do scoring
	analysis$scoring_table <- compare_forecasts(analysis$forecast_res)

	if(isTRUE(plot)) {
		compare_bsts_models(y)
	}

	analysis$forecast_plot <- plot_forecast_compare(analysis$forecast_res)
	return(analysis)
}






#' @title Wrapper to do all the fits that I have
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


forecast_one_country <- function(data = NULL, 
								 country = NULL, 
								 y = NULL, 
								 include_stan = FALSE, 
								 models = NULL) {

	if (is.null(y)) {
		inputdata <- data$inputdata
		y <- inputdata[inputdata$region == country, 
			   		   colnames(inputdata) == "median"]
	}
	if (is.null(data)) {
		start_period <- 8
	} else {
		start_period <- data$start_period
	}

	res <- list()
	stanfitlist <- list()
	scores <-list()

	# if (isTRUE(include_stan)) {
	# 	## lin_reg_stan
	# 	model_lin_reg <- models[[1]]
	# 	res$lin_reg_stan <- fit_iteratively(incidences = y, model = model_lin_reg,
	# 						   n_pred = 7, iter = 5000,
	#  					       max_n_past_obs = 7, vb = FALSE)

	# 	model_bsts <- models[[2]]
	# 	res$bsts_stan <- fit_iteratively(incidences = y,
	# 							model = model_bsts, n_pred = 7, iter = 5000,
	# 							prior_var_phi = 0.8, mean_phi = 1,
	# 							max_n_past_obs = Inf, vb = FALSE)

	# 	model_bsts_local <- models[[3]]
	# 	res$bsts_local_stan <- fit_iteratively(incidences = y,
	# 								  model = model_bsts_local,
	# 								  n_pred = 7, iter = 5000,
	# 								  max_n_past_obs = 7, vb = FALSE)
	# }

	res$bsts_local <- fit_iteratively(incidences = y,
					  model = "local", n_pred = 7,
					  start_period = start_period,
					  fit_type = "bsts_package")

	res$bsts_semilocal <- fit_iteratively(incidences = y,
					  model = "semilocal", n_pred = 7,
					  start_period = start_period,
					  fit_type = "bsts_package")

	res$bsts_local_student <- fit_iteratively(incidences = y,
					  model = "local_student", n_pred = 7,
					  start_period = start_period,
					  fit_type = "bsts_package")

	res$bsts_ar1 <- fit_iteratively(incidences = y,
					  model = "ar1", n_pred = 7,
					  start_period = start_period,
					  fit_type = "bsts_package")

	res$bsts_ar2 <- fit_iteratively(incidences = y,
					  model = "ar2", n_pred = 7,
					  start_period = start_period,
					  fit_type = "bsts_package")


	return(res)
}

