



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



	regions <- unique(data$inputdata$region)

	inc_pred <- lapply(seq_along(regions), 
					   function(i) {
					   		cat("predict incidences ", 
							as.character(i), " (", regions[i],
							") ",  
							"of ", as.character(length(regions)), 
							"\n", sep = "")
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
						inf_data <- infectiousness_from_true_data(days_ahead = days_ahead, data, 
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





infectiousness_from_true_data <- function(days_ahead, 
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


