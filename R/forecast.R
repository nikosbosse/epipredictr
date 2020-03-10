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

	timeseries <- data$timeseries
	countries <- data$countries
	models <- data$models

	# analysis by country ============================================= #
	## collect results country-wise
	res <- lapply(seq_along(timeseries),
				  FUN = function(i) {
				  	return(analysis_one_country(y = timeseries[[i]],
				  								country = countries[i],
				  								data = data))
				  })
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
				timeseries = timeseries,
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

analysis_one_country <- function(y, country = "country",
								 data, plot = F) {
	analysis <- list()
	analysis$country <- country

	models <- data$models

	# res <- list()

	# for (model in models) {
	# 	res[[model]] <- list()
	# }


	# i <- 1
	# current_last_obs <- data$start_period - 1

	# while (current_last_obs < total_n){
	# 	## determine current indices and make data
	# 	index <- 1:current_last_obs
	# 	if (length(index) > max_n_past_obs) {
	# 		current_earliest_obs <- length(index) - max_n_past_obs + 1
	# 		index <- index[current_earliest_obs:current_last_obs]
	# 	}
	# 	current_y <- y[index]

	# 	for (model in models) {
	# 		res[[model]][[forecast_run]] <- i
	# 		res[[model]][[predictive_samples]] <- fit()

	# 	}




	# 	i <- i + 1

	# 	if (fit_type == "stan") {
	# 		stanfit <- fit_stan_model(y, model, n_pred = n_pred, vb = vb,
	# 							      length_local_trend = length_local_trend,
	# 							      iter = iter)

	# 		## store results
	# 		stanfitobjects[[i]] <- stanfit
	# 		predictive_samples[[i]] <- extract_samples(stanfit)
	# 	} else {
	# 		predictive_samples[[i]] <- bsts_wrapper(y, model,
	# 								   num_pred  = n_pred)
	# 	}

	# 	forecast_run[[i]] <- rep(i, nrow(predictive_samples[[i]]))

	# 	current_last_obs <- current_last_obs + interval
	# }



	## do forecasting
	analysis$forecast_res <- forecast_one_country(y, include_stan = F)
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


forecast_one_country <- function(y, include_stan = FALSE, models = NULL) {
	res <- list()
	stanfitlist <- list()
	scores <-list()

	if (isTRUE(include_stan)) {
		## lin_reg_stan
		model_lin_reg <- models[[1]]
		res$lin_reg_stan <- fit_iteratively(incidences = y, model = model_lin_reg,
							   n_pred = 7, iter = 5000,
	 					       max_n_past_obs = 7, vb = FALSE)

		model_bsts <- models[[2]]
		res$bsts_stan <- fit_iteratively(incidences = y,
								model = model_bsts, n_pred = 7, iter = 5000,
								prior_var_phi = 0.8, mean_phi = 1,
								max_n_past_obs = Inf, vb = FALSE)

		model_bsts_local <- models[[3]]
		res$bsts_local_stan <- fit_iteratively(incidences = y,
									  model = model_bsts_local,
									  n_pred = 7, iter = 5000,
									  max_n_past_obs = 7, vb = FALSE)
	}

	res$bsts_local <- fit_iteratively(incidences = y,
					  model = "local", n_pred = 7,
					  fit_type = "bsts_package")

	res$bsts_semilocal <- fit_iteratively(incidences = y,
					  model = "semilocal", n_pred = 7,
					  fit_type = "bsts_package")

	res$bsts_local_student <- fit_iteratively(incidences = y,
					  model = "local_student", n_pred = 7,
					  fit_type = "bsts_package")

	res$bsts_ar1 <- fit_iteratively(incidences = y,
					  model = "ar1", n_pred = 7,
					  fit_type = "bsts_package")

	res$bsts_ar2 <- fit_iteratively(incidences = y,
					  model = "ar2", n_pred = 7,
					  fit_type = "bsts_package")


	return(res)
}

