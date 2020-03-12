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
	countries <- as.character(unique(inputdata$region))
	models <- data$models


	# analysis by country ============================================= #
	## collect results country-wise
	all_region_results <- lapply(seq_along(countries),
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

	failure <- lapply(all_region_results, is.null)
	failure <- do.call(c, failure)

	all_region_results <- all_region_results[!failure]
	countries <- countries[!failure]
	names(all_region_results) <- countries
	## ================================================================ #


	## scoring ================================================== #
	## do aggregated scoring
	scoring <- list()

	tables <- lapply(all_region_results, '[[', 3)
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

		predictive_samples <- all_region_results[[country]]$region_results[[best]]$predictive_samples
		y <- all_region_results[[country]]$region_results[[best]]$y

		forecast_run <- all_region_results[[country]]$region_results[[best]]$forecast_run

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
				all_region_results = all_region_results,
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

	models <- data$models
	start_period <- data$start_period
	n_pred <- data$n_pred

	inputdata <- data$inputdata
	y <- inputdata[inputdata$region == country, 
				   colnames(inputdata) == "median"]
	
	out <- list()
	out$country <- country


	## do forecasting

	# if (isTRUE(include_stan)) {
	# }

	out$region_results <- list()
	for (model in models) {
		name <- paste("bsts_", model, sep = "")
		out$region_results[[name]] <- fit_iteratively(incidences = y,
									      model = model, n_pred = n_pred,
									      start_period = start_period,
									      fit_type = "bsts_package")
	}

	out$region_results <- add_average_model(out$region_results)


	## do scoring
	out$scoring_table <- compare_forecasts(out$region_results)

	if(isTRUE(plot)) {
		compare_bsts_models(y)
	}

	out$forecast_plot <- plot_forecast_compare(out$region_results)
	return(out)
}



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
				  	return(p)
				  })

	pred <- tmp[[1]]
	for (i in 2:length(region_results)) {
		pred <- pred + tmp[[i]] 
	}
	avg <- pred / length(region_results)
	region_results$average$predictive_samples <- avg
	region_results$average$y <- region_results[[1]]$y
	region_results$average$forecast_run <- region_results[[1]]$forecast_run
	return(region_results)
}