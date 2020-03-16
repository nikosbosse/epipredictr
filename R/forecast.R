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
	all_region_results <- lapply(seq_along(regions),
				  FUN = function(i) {
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
				  })

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



#' @title Make incidence predictions
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

predict_incidences <- function (data, full_predictive_samples) {

	cat("predict incidences ", 
	as.character(i), " (", regions[i],
	") ",  
	"of ", as.character(length(regions)), 
	"\n", sep = "")

	regions <- unique(data$inputdata$region)

	inc_pred <- lapply(seq_along(regions), 
					   function(i) {
					   		predict_incidences_one_region(full_predictive_samples, regions[i])
					   })
	return(do.call(rbind, inc_pred))

}



predict_incidences_one_region <- function(full_predictive_samples, region) {

	region = "austria"
	pred <- full_predictive_samples[full_predictive_samples$region == region, ]
	max_days_ahead <- max(full_predictive_samples$days_ahead)
	predicted_incidences <- list()
	
	for (days_ahead in 1:max_days_ahead) {
		pred_x_ahead <- pred[pred$days_ahead == days_ahead, ]
		dates <- unique(pred_x_ahead$date)
		cat("step", as.character(days_ahead), "of", as.character(max_days_ahead), "\n")
		

		inc <- lapply(seq_along(dates), 
					FUN = function(i) {
						# cat(as.character(i), "of", as.character(length(dates)))
						## select current predictive R0 samples 
						curr_r_pred <- pred_x_ahead[pred_x_ahead$date == dates[i], ]
						select_cols <- grepl("sample", colnames(curr_r_pred))	

						## infectiousness from observed data
						inf_data <- infectiousness_from_true_data(days_ahead_forecast = days_ahead, data, 
						region, date_of_prediction = dates[i])
						
						## infectiousness from predicted data
						last_date_of_observed_data <- as.Date(dates[i]) - days_ahead
						prev_pred_dates <- last_date_of_observed_data + 1:days_ahead
						prev_pred_dates <- prev_pred_dates[-length(prev_pred_dates)]

						if (length(prev_pred_dates) == 0) {
							infectiousness <- inf_data
						} else {
							inf <- list()
							for (curr_prev_date in prev_pred_dates) {
								
								curr_prev_date <- as.Date(curr_prev_date)
								diff_to_curr <- as.numeric(as.Date(dates[i]) - curr_prev_date)
								predictions_to_select <- days_ahead - diff_to_curr

								prev_pred_inc <- predicted_incidences[[predictions_to_select]]
								prev_pred_inc <- prev_pred_inc[prev_pred_inc$date == curr_prev_date, ]

								inf[[predictions_to_select]] <- prev_pred_inc[, select_cols] * weight_case_x_days_ago(diff_to_curr)
							}
							infectiousness <- Reduce("+", inf) + inf_data
						}

						pred_inc <- cbind(curr_r_pred[, !select_cols], 
										  curr_r_pred[, select_cols] * infectiousness)
						return(pred_inc)
					})

		predicted_incidences[[days_ahead]] <- do.call(rbind, inc)
	}

	return(do.call(rbind, predicted_incidences))

}





infectiousness_from_true_data <- function(days_ahead_forecast, 
										  data, region, date_of_prediction,
										  mean_si = NULL, sd_si = NULL) {

		## a one day ahead forecast means that the last data point is one
		## day away from the current date
		last_date_of_observed_data <- as.Date(date_of_prediction) - (days_ahead)	

		select_obs <- incidences$region == region & incidences$date <= last_date_of_observed_data
		y <- incidences$y[select_obs]
		num_obs <- length(y)
		dates <- incidences$date[incidences$region == region]




		w <- sapply(num_obs:1, weight_case_x_days_ago)
		return (sum(w * y))
}




weight_case_x_days_ago <- function(num_days_ago, 
											mean_si = NULL, 
											sd_si = NULL) {
	alpha_gamma = 2.706556
	beta_gamma = 0.1768991
	return(pgamma(num_days_ago + 0.5, alpha_gamma, beta_gamma) -
     pgamma(num_days_ago - 0.5, alpha_gamma, beta_gamma))
}







#' @title Do the scoring
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


scoring <- function(data, full_predictive_samples) {

	inputdata <- data$inputdata
	models <- data$models
	regions <- as.character(unique(inputdata$region))

	score_model_in_region <- function(data, full_predictive_samples, region, model) {
		inputdata <- data$inputdata

		# select observations and predictions
		index <- full_predictive_samples$model == model & full_predictive_samples$region == region
		observations <- inputdata[inputdata$region == region, ] 
		predictions <- full_predictive_samples[index, ]

		df <- merge(observations, predictions)
		pred <- df[, grepl("sample", colnames(df))]

		dss <- scoringRules::dss_sample(y = df$y, dat = as.matrix(pred))
		crps <- scoringRules::crps_sample(y = df$y, dat = as.matrix(pred))
		pit <- pit_cont(y = df$y, as.matrix(pred))
		sharpness <- apply(pred, MARGIN = 1, mad)
		bias <- 1 - pit


		scores <- data.frame(date = df$date, 
				   model = model, 
				   region = region,
				   days_ahead = df$days_ahead, 
				   crps = crps, 
				   dss = dss, 
				   pit = pit, 
				   sharpness = sharpness,
				   bias = bias)

		return(scores)
	}


	all_scores <- list()
	for (region in regions) {
		tmp <- lapply(seq_along(models), 
					  FUN = function(i) {
					  	score_model_in_region(data, 
					  						  full_predictive_samples, 
					  						  region, 
					  						  models[i])
					  })
		scores_one_region <- do.call(rbind, tmp)
		all_scores[[region]] <- scores_one_region
	}	
	all_scores <- do.call(rbind, all_scores)

	return(all_scores)
}




#' @title aggregate scores
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


aggregate_scores <- function(all_scores) {

	## aggregate scores in one region over all predictions made by 
	## that model for given days ahead
	n_pred <- max(all_scores$days_ahead)
	overall_region_scores <- aggregate(. ~ model + days_ahead + region,
									   all_scores, 
									   mean)

	scores_model_in_region <- subset(overall_region_scores, select = -c(date))
	

	## aggregate to find the model that performs best in every region 
	## no matter the time horizon
	mean_model_scores_region <- aggregate(. ~ model + region, 
										  scores_model_in_region, mean)
	mean_model_scores_region <- subset(mean_model_scores_region, 
									   select = -days_ahead)


	## aggregate to find the model that performs best for a given time horizon 
	## no matter the region
	mean_model_scores_horizon <- aggregate(. ~ model + days_ahead, 
										  scores_model_in_region, mean)
	mean_model_scores_horizon <- subset(mean_model_scores_horizon, 
									   select = -days_ahead)

	## aggregate the scores (that were already averaged over all predictions
	## made by one model in one country) over all the regions to get the 
	## model that performs best for given days ahead
	model_scores <- list()
	for (i in 1:n_pred) {
		index <- scores_model_in_region$days_ahead == i
		tmp <- aggregate(. ~ model, 
								  scores_model_in_region[index, ], 
								  mean)
		tmp <- subset(tmp, select = -c(region))

		model_scores[[i]] <- tmp
	}
	model_scores <- do.call(rbind, model_scores)


	## aggregate to find the model that performs best overall no 
	## matter the time horizon
	mean_model_scores <- aggregate(. ~ model, model_scores, mean)
	mean_model_scores <- subset(mean_model_scores, select = -days_ahead)

	return(list(scores_model_in_region = scores_model_in_region,
				mean_model_scores_region = mean_model_scores_region,
				mean_model_scores_region = mean_model_scores_region,
				model_scores = model_scores, 
				mean_model_scores = mean_model_scores))
}




#' @title Do the plotting for scores
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

plot_scoring <- function(data, aggregate_scores) {

	model_scores <- aggregate_scores$model_scores
	mean_model_scores_region <- aggregate_scores$mean_model_scores_region


	model_scores_plot <- ggplot(data = model_scores,
		   	aes(y = crps, x = model, color = model)) +
	  		geom_boxplot() +
	  		theme(text = element_text(family = 'Sans Serif')) +
	  		ggtitle("Aggregated performance for different horizons")

	 mean_scores_plot <- ggplot(data = mean_model_scores_region,
		   	aes(y = crps, x = model, group = model, color = model)) +
	  		geom_boxplot() +
	  		theme(text = element_text(family = 'Sans Serif')) +
	  		ggtitle("Performance averaged over days ahead and regions")


	score_days_ahead <- ggplot(data = scores_model_in_region,
		   	aes(y = crps, x = model, 
		   		group = model, color = model)) +
	  		geom_boxplot() +
	  		facet_wrap(~ days_ahead) +
	  		theme(text = element_text(family = 'Sans Serif')) +
	  		ggtitle("Performance for different horizons across all countries")

	score_days_ahead2 <-  ggplot(data = scores_model_in_region,
		   	aes(y = crps, x = days_ahead, 
		   		group = days_ahead, color = model)) +
	  		geom_boxplot() +
	  		facet_wrap(~ model) +
	  		theme(text = element_text(family = 'Sans Serif')) +
	  		ggtitle("Performance for different horizons across all countries")


	 return(list(model_scores_plot = model_scores_plot, 
	 			 score_days_ahead = score_days_ahead, 
				 score_days_ahead2 = score_days_ahead2, 
				 mean_scores_plot = mean_scores_plot))
}




plot_predictions <- function(data, full_predictive_samples) {

	inputdata <- data$inputdata

	## make df for observations
	obs <- cbind(inputdata, 
				 type = "observed", 
				 days_ahead = 0,
				 model = NA,
				 ci2.5 = NA, 
				 ci25 = NA, 
			     ci75 = NA, 
			     ci97.5 = NA, 
			     forecast_run = NA)

	## make df for predictions
	select_cols <- grepl("sample", colnames(full_predictive_samples))
	y_pred_samples <- full_predictive_samples[, select_cols]	

	pred_mean <- rowMeans(y_pred_samples)
	pred_median <- apply(y_pred_samples, median, MARGIN = 1)
	pred_quantiles <- t(apply(y_pred_samples,
								MARGIN = 1,
								FUN = quantile,
								probs = c(0.025, 0.25, 0.75, 0.975),
							    na.rm = TRUE))

	pred <- data.frame(date = full_predictive_samples$date,
					   days_ahead = full_predictive_samples$days_ahead,
					   model = full_predictive_samples$model,
				       y = pred_median, 
				       region = full_predictive_samples$region,
				       type = "predicted", 
				       ci2.5 = pred_quantiles[,1], 
				       ci25 = pred_quantiles[,2], 
				       ci75 = pred_quantiles[,3], 
				       ci97.5 = pred_quantiles[,4], 
				       forecast_run = full_predictive_samples$forecast_run)

	df <- (rbind(obs, pred))


	## make predictions for a single model

	index <- df$region == region
	dfcurr <- df[index, ]
	index <- is.na(dfcurr$forecast_run) | dfcurr$forecast_run == max(dfcurr$forecast_run, na.rm = T) 
	index <- index & (is.na(dfcurr$model) | dfcurr$model == model )
	dfcurr <- dfcurr[index, ]

	ggplot(data = dfcurr, aes(x = date, group = type, color = type)) + 
			geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5), alpha = 0.5) +
			geom_ribbon(aes(ymin = ci25, ymax = ci75), alpha = 0.7) +
		geom_line(aes(y = y)) +
		geom_point(aes(y = y), size=1) +
	  		theme(text = element_text(family = 'Sans Serif')) 


	## compare accuracy for different days ahead 	  
	index <- df$region == region
	dfcurr <- df[index, ]

		ggplot(data = dfcurr, aes(x = date, group = type, color = type)) + 
			geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5), alpha = 0.5) +
			geom_ribbon(aes(ymin = ci25, ymax = ci75), alpha = 0.7) +
		geom_line(aes(y = y)) +
		geom_point(aes(y = y), size=1) +
		facet_wrap(~ days_ahead) +
	  		theme(text = element_text(family = 'Sans Serif')) 


# dfcurr

}






#
#
# predictions = list(dat1 = replicate(5000, rpois(n = 100, lambda = 1:100)),
#                    dat2 = replicate(5000, rpois(n = 100, lambda = 1:100)))
#
#
# call = eval_forecasts(y, predictions)
#
#
# eval_forecasts_prob_int(true_values = y, predictions = predictions)
#












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



