

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


scoring <- function(data, full_predictive_samples, incidences = NULL, scoringtype = "R") {

	inputdata <- data$inputdata
	models <- data$models
	regions <- as.character(unique(inputdata$region))

	score_model_in_region <- function(data, full_predictive_samples, 
								      region, model, incidences = NULL, 
								      predicted_incidences = NULL,
								      scoringtype) {

		inputdata <- data$inputdata

		# select observations and predictions
		if (scoringtype == "R") {
			index <- full_predictive_samples$model == model & full_predictive_samples$region == region
			predictions <- full_predictive_samples[index, ]
			observations <- inputdata[inputdata$region == region, ] 
		} else {
			index <- predicted_incidences$model == model & predicted_incidences$region == region
			predictions <- predicted_incidences[index, ]

			observations <- incidences[incidences$region == region, ]
			select_dates <- observations$date %in% unique(predictions$date)
			observations <- observations[select_dates, ]	
		}

		df <- merge(observations, predictions)
		pred <- df[, grepl("sample", colnames(df))]

		dss <- scoringRules::dss_sample(y = df$y, dat = as.matrix(pred))
		crps <- scoringRules::crps_sample(y = df$y, dat = as.matrix(pred))
		logS <- scoringRules::logs_sample(y = df$y, dat = as.matrix(pred))
		pit <- pit_cont(y = df$y, as.matrix(pred))
		sharpness <- apply(pred, MARGIN = 1, mad)
		bias <- 1 - pit


		scores <- data.frame(date = df$date, 
				   model = model, 
				   region = region,
				   days_ahead = df$days_ahead, 
				   crps = crps, 
				   logS = logS,
				   dss = dss, 
				   pit = pit, 
				   sharpness = sharpness,
				   bias = bias)

		return(scores)
	}


	all_scores <- list()
	for (region in regions) {
		tmp <- furrr::future_map(seq_along(models), 
					  .f = function(i) {
					  	score_model_in_region(data, 
					  						  full_predictive_samples, 
					  						  region,
					  						  incidences = incidences, 
					  						  scoringtype = scoringtype, 
					  						  models[i])
					  }, .progress = TRUE)
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




pit_cont <- function (y, dat) {

	n_pred <- ncol(dat)
	P_x <- vapply(seq_along(y),
              function(i) {
                sum(dat[i,] <= y[i]) / n_pred
              },
              .0)	
	return(P_x)
}


# num_bins <- round(sqrt(length(P_x)))
# hist_PIT <- ggplot(as.data.frame(P_x), aes(x = P_x)) + 
# 			geom_histogram(color = 'darkblue', 
# 						   fill = 'lightblue', bins = num_bins)

calibration <- function(true_values, predictive_samples) {
	P_x <- pit(true_values, predictive_samples)
	goftest::ad.test(P_x)$p.value	
}



bias <- function(true_values, predictive_samples) {
	n_pred <- ncol(predictive_samples)
	## empirical cdf
	P_x <- vapply(seq_along(true_values),
              function(i) {
                sum(predictions[i,] <= true_values[i]) / n_pred
              },
              .0)	
	return(1 - P_x)
}

