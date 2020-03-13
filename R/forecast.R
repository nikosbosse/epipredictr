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

	## aggregate all country results into one data.frame
	full_results <- lapply(all_region_results, 
						   function(x) {
						 	  return(x[["complete_region_results"]])
						   })
	full_results <- do.call(rbind, full_results)
	rownames(full_results) <- NULL

	## scoring ================================================== #
	## do aggregated scoring
	# scoring <- list()
	# tables <- lapply(all_region_results, 
	# 				 function(x) {
	# 				 	return(x[["scoring_table_region"]])
	# 				 })
	# tables <- lapply(seq_along(tables),
 # 					 FUN = function(i) {
 # 					 	cbind(country = countries[i], tables[[i]])
 # 					 })
	# table <- do.call(rbind, tables)

	# scoring$across_country_scores <- table

	# ## rank scores by mean and median
	# table_mean <- aggregate(mean ~ method + model, table, mean)
	# table_mean <- table_mean[order(table_mean$method, table_mean$mean), ]
	# scoring$scores_ranked_mean <- table_mean

	# table_median <- aggregate(mean ~ method + model, table, mean)
	# table_median <- table_median[order(table_mean$method, table_mean$mean), ]
	# scoring$scores_ranked_median <- table_median

	# ## plot scores across countries
	# scoring$lineplot <- ggplot(data = table,
	# 	   	aes(y = mean, color = (model), x = country, group = model)) +
	#   		geom_point() +
	#   		geom_line() +
	#   		facet_wrap(~ method, ncol = 1, scales = "free_y") +
	#   		theme(text = element_text(family = 'Serif'))

	# scoring$boxplot <- ggplot(data = table,
	# 	   	aes(y = mean, color = (model), x = country, group = model)) +
	#   		geom_boxplot() +
	#   		facet_wrap(~ method, ncol = 1, scales = "free_y") +
	#   		theme(text = element_text(family = 'Serif'))
	## ========================================================== #




	## find best model and make separate predictin plots ============ #
	# best <- as.character(table_mean[1, colnames(table_mean) == "model"])

	# predictions_best <- list()
	# for (country in countries) {

	# 	predictive_samples <- all_region_results[[country]]$region_results[[best]]$predictive_samples
	# 	y <- all_region_results[[country]]$region_results[[best]]$y

	# 	forecast_run <- all_region_results[[country]]$region_results[[best]]$forecast_run

	# 	predictive_samples[!is.na(y), ] <- NA

	# 	t <- paste(country, best, sep = "_")
	# 	p <- plot_pred_vs_true(y_true = y,
	# 						   y_pred_samples = predictive_samples,
	# 						   forecast_run = forecast_run, vlines = F,
	# 					  	   plottitle = t)
	# 	predictions_best[[country]] <- p
	# }
	## ========================================================== #

	## make case predictions with best model ============ #

	out <- list(countries = countries,
				inputdata = inputdata,
				all_region_results = all_region_results,
				# scoring = scoring,
				# predictions_best = predictions_best, 
				full_results = full_results)

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
	inputdata <- data$inputdata
	dates = inputdata[inputdata$region == country,	
			      colnames(inputdata) == "date"]	

	## do forecasting
	# if (isTRUE(include_stan)) {
	# }
	region_results <- list()
	for (model in models) {
		name <- paste("bsts_", model, sep = "")
		region_results[[name]] <- fit_iteratively(data, 
												  country = country, 
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

	## do scoring
	# scoring_table_region <- compare_forecasts(inputdata, region_results)

	# if(isTRUE(plot)) {
	# 	y <- inputdata[inputdata$region == country, 
	# 			   colnames(inputdata) == "median"]
	# 	compare_bsts_models(y)
	# }

	
	## make plots with the predictive performance of all models in that region
	# titles <- names(region_results)
	# plots <- lapply(seq_along(region_results),
	# 				FUN = function (i) {
	# 					plot_pred_vs_true(
	# 					 y_pred_samples = region_results[[i]]$predictive_samples, 
	# 					 y_true = region_results[[i]]$y,
	# 					 forecast_run = region_results[[i]]$forecast_run,
	# 					 plottitle = titles[i], 
	# 					 dates = dates
	# 					)
	# 		        })

	# forecast_plot_region <- wrap_plots(plots, ncol = 1)



	return(list(country = country, 
			    region_results = region_results,
				#scoring_table_region = scoring_table_region, 
				#forecast_plot_region = forecast_plot_region, 
			    complete_region_results = complete_region_results))
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
				  	p <- p[, grep("sample", colnames(p))]
				  	return(p)
				  })
	pred <- tmp[[1]]
	for (i in 2:length(region_results)) {
		pred <- pred + tmp[[i]] 
	}
	avg <- pred / length(region_results)

	## add columns with country, model, date and days ahead
	avg <- cbind(region_results[[1]]$predictive_samples$forecast_run, 
				 region_results[[1]]$predictive_samples$country, 
				 region_results[[1]]$predictive_samples$model, 
				 region_results[[1]]$predictive_samples$predicted_date,
				 region_results[[1]]$predictive_samples$days_ahead,  	
				 avg)
	colnames(avg)[1:5] <- c("forecast_run", "country", 
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
