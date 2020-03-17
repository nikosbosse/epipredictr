



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

plot_scoring <- function(data, aggregate_scores, all_scores) {

	model_scores <- aggregate_scores$model_scores
	mean_model_scores_region <- aggregate_scores$mean_model_scores_region

	scores_model_in_region <- aggregate_scores$scores_model_in_region

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


	score_days_ahead3 <- ggplot(data = scores_model_in_region,
		   	aes(y = crps, x = days_ahead, group = days_ahead, color = model, fill = model)) +
	  		geom_violin(alpha = 0.4) +
	  		facet_wrap(~ model) +
	  		theme(text = element_text(family = 'Sans Serif'), legend.position = "bottom") +
	  		ggtitle("Performance for different horizons across all countries")


	 return(list(model_scores_plot = model_scores_plot, 
	 			 score_days_ahead = score_days_ahead, 
				 score_days_ahead2 = score_days_ahead2, 
				 mean_scores_plot = mean_scores_plot))
}


plot_predictions <- function(data, full_predictive_samples, best_model. 
							 incidences = NULL, type = "R",
							 predicted_incidences == NULL) {

	make_plot_dataframe <- function(data, full_predictive_samples, incidences, 
									type) {

		if (type == "R") {
			y_values <- data$inputdata	
		} else {
			## select appropriate dates
			regions <- unique(incidences$region)
			select_dates <- sapply(regions, 
								   function(region) {
								   	 pred_dates <- predicted_incidences$date[predicted_incidences$region == region]
								   	 incidences$region == region & incidences$date %in% 
								   })

			dim(full_predictive_samples)
		}
 		

		## make df for observations
		obs <- cbind(y_values, 
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

		return(rbind(obs, pred))
	}

	plot_df <- make_plot_dataframe(data, full_predictive_samples)
	

	## make predictions for the best model =======================
	plot_predictions_best_model <- function(model, region, plot_df) {
		#select appropriate data: region, model, and last forecast_run
		index <- plot_df$region == region
		dfcurr <- plot_df[index, ]
		index <- is.na(dfcurr$forecast_run) | dfcurr$forecast_run == max(dfcurr$forecast_run, na.rm = T) 
		index <- index & (is.na(dfcurr$model) | dfcurr$model == model )
		dfcurr <- dfcurr[index, ]
		df_obs <- dfcurr[dfcurr$type == "observed", ]
		df_pred <- dfcurr[dfcurr$type == "predicted", ]

		out <- ggplot(data = df_pred, aes(x = date, group = type, color = type)) + 
				geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5), alpha = 0.3, fill = "blue") +
				geom_ribbon(aes(ymin = ci25, ymax = ci75), alpha = 0.7, fill = "blue") +
			geom_line(data = df_obs, aes(y = y, group = type), color = "red") +
			geom_point(data = df_obs, aes(y = y), size=1) +
			theme_cowplot() +
			theme(legend.position="bottom", text = element_text(family = 'Sans Serif')) 

		return(out)
	}

	regions <- unique(data$inputdata$region)
	pred_best_model <- lapply(seq_along(regions), 
							  FUN = function(i) {
							  	plot_predictions_best_model(best_model, regions[i], plot_df)	 
							  })

	names(pred_best_model) <- paste("prediction", regions, sep = "")

	## plot accuracy for model x in region y for different days ahead 	  
	plot_forecast_vs_true <- function(region, model, plot_df) {
		
		index <- plot_df$region == region & (is.na(plot_df$model) | plot_df$model == model)
		dfcurr <- plot_df[index, ]
		df_obs <- dfcurr[dfcurr$type == "observed", ]
		df_obs <- subset(df_obs, select = -days_ahead)
		df_pred <- dfcurr[dfcurr$type == "predicted", ]
		df_pred <- df_pred[df_pred$date <= max(df_obs$date), ]

		out <- ggplot(data = df_pred, aes(x = date)) + 
				geom_line(data = df_obs, aes(y = y), color = "blue") +
				geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5, group = days_ahead), alpha = 0.2) +
				geom_ribbon(aes(ymin = ci25, ymax = ci75, group = days_ahead), alpha = 0.5) +
				facet_wrap(~ days_ahead) +
				coord_cartesian(ylim = c(0, NA)) +
		  		theme(text = element_text(family = 'Sans Serif')) 		
		return(out)
	}

	
	plot_pred_vs_true_one_region <- function(data, region, plot_df) {
		models <- data$models

		out <- lapply(seq_along(models), 
					  FUN = function (i) {
					  	plot_forecast_vs_true(region, models[i], plot_df)
					  })
		names(out) <- models
		return(out)
	}



	all_plot_pred_vs_true <- function(data, plot_df) {
		
		regions <- unique(data$inputdata$region)
		out <- lapply(seq_along(regions), 
					  FUN = function (i) {
					  	plot_pred_vs_true_one_region(data, regions[i], plot_df)
					  })
		names(out) <- paste("pred_vs_true", regions, sep = "")
		return(out)
	}
	 
	all_plots_pred_vs_true <- all_plot_pred_vs_true(data, plot_df)


	return(list(predictions_best = pred_best_model, 
				all_plot_pred_vs_true = all_plots_pred_vs_true))

}




