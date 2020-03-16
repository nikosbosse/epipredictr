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



#' @title Load Sam's R_t timeseries
#'
#' @description
#'
#' @return
#' a data.frame with the data
#'
#' @examples
#' NULL
#' @export
#' @references
#' NULL

load_all_timeseries <- function(base_dir = NULL, 
								date = NULL, 
								ts_type = "R", 
								min_req_data = 6) {

	if (is.null(base_dir)) {
		base_dir <- "data/results"
	}

	regions <- list.files(base_dir)

	dfs <- lapply(seq_along(regions),
				  FUN = function(i) {
				  	tryCatch(
				  	{ load_single_timeseries(base_dir,
				  							   regions[i],
				  							   date,
				  							   min_req_data = min_req_data,
				  							 ts_type = ts_type)
				  	},
				  	error=function(cond) {return(NULL)}
				  	)
				  })
	dfs <- do.call(rbind, dfs)

	return(dfs)
}



#' @title Load Sam's R_t timeseries for a single region
#'
#' @description
#'
#' @return
#' a data.frame with the data
#'
#' @examples
#' NULL
#' @export
#' @references
#' NULL

load_single_timeseries <- function(base_dir, region, date = NULL,
								   ts_type = NULL, min_req_data) {
	
	file_dir <- file.path(base_dir, region)

	if (is.null(date)) {
		## find latest date
		date <-  as.Date(list.files(file_dir))
		date <- max(date)
	}

	file_dir <- file.path(base_dir, region, date)

	if (ts_type == "R") {
		file_path <- file.path(file_dir, "time_varying_params.rds")

		df <- readRDS(file_path)[[1]]
		df <- df[, colnames(df) %in% c("date", "median")]
		df <- cbind(df, region = region)
		colnames(df)[colnames(df) == "median"] <- "y"

		if (nrow(df) < min_req_data) return(NULL)
		return(df)
	} else if (ts_type == "incidences") {

		file_path <- file.path(file_dir, "summarised_nowcast.rds")
		df <- readRDS(file_path)
		df <- df[df$type == "nowcast",
			     colnames(df) %in% c("date", "median")]
		df <- cbind(df, region = region)
	}
	colnames(df)[colnames(df) == "median"] <- "y"
	return(df)
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

fit_stan_model <-function(y, model, n_pred, vb,
						  iter = 4000,
	                      length_local_trend, ...) {

	if (class(model) == "character" && model == "lin_reg") {
		stanfit <- epipredictr::linear_regression(y = y,
												  num_pred = n_pred,
												  x = 1:length(y))
		} else if (class(model) == "character" && model == "bsts") {
			stanfit <- bsts(y = y,
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
					  length_local_trend = 5, num_pred = n_pred,
					  mean_phi = 1)
			stanfit <- rstan::sampling(model, data = l,
                        	iter = iter, thin = 1,
                        	control = list(adapt_delta = 0.99))
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

plot_pred_vs_true <- function(full_results,
							  inputdata, 
							  region,
							  model,
							  vlines = F,
							  dates = NULL,
							  plottitle = "Pred vs. True"){
	
	plottitle <- paste("predictions for model", model, "in", region)

	## make df with observed values
	y_true <- inputdata$y[inputdata$region == region]
	inputdates <- inputdata$date[inputdata$region == region]
	obs <- data.frame(date = inputdates, 
					  type = "observed", 
					  y = y_true, 
					  ci2.5 = NA, 
				      ci25 = NA, 
				      ci75 = NA, 
				      ci97.5 = NA, 
				      forecast_run = NA)

	## make df to plot predicted values
	index <- full_results$region == region & full_results$model == model
	y_pred_samples <- full_results[index, 
								   grep("sample", colnames(full_results))]	
	
	preddates <- full_results$date[index]
	forecast_run <- full_results$forecast_run[index]

	pred_mean <- rowMeans(y_pred_samples)
	pred_median <- apply(y_pred_samples, median, MARGIN = 1)
	pred_quantiles <- t(apply(y_pred_samples,
								MARGIN = 1,
								FUN = quantile,
								probs = c(0.025, 0.25, 0.75, 0.975),
							    na.rm = TRUE))

	pred <- data.frame(date = preddates, 
				       type = "predicted", 
				       y = pred_median, 
				       ci2.5 = pred_quantiles[,1], 
				       ci25 = pred_quantiles[,2], 
				       ci75 = pred_quantiles[,3], 
				       ci97.5 = pred_quantiles[,4], 
				       forecast_run = forecast_run)

	df <- (rbind(obs, pred))

	# start_data <- sum(is.na(forecast_run))
	# interval <- sum(forecast_run == 1, na.rm = TRUE)

	## make vertical lines to display
	if (isTRUE(vlines)) {
		x <- unique(forecast_run)
		seq <- rep(NA, length(x) - 1)
		for (i in 1:(length(x) - 1)) {
			seq[i] <- which(forecast_run == x[i + 1] )[1] - 1
		}
		vlines <- rep(NA, nrow(df))
		vlines[seq] <- seq
	} else {
		vlines <- 0
	}

	plot <- ggplot(df, aes(x = date, group = type)) +
			geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5), alpha = 0.5,
						fill = "gray") +
			geom_ribbon(aes(ymin = ci25, ymax = ci75), alpha = 0.7,
						fill = "gray") +
			geom_line(aes(y = y, group = type, color = type), size=1) +
			geom_point(aes(y = y), color = "black", size=1) +
			ggtitle(plottitle) +
			theme(text = element_text(family = 'Sans Serif'))

	if (isTRUE(vlines)) {
		plot <- plot + geom_vline(aes(xintercept = vlines))
	}
	return(plot)
}




# plot_pred_vs_true <- function(y_true,
# 							  y_pred_samples,
# 							  forecast_run,
# 							  vlines = F,
# 							  dates = NULL,
# 							  plottitle = "Pred vs. True"){

# # y_pred_samples <- (analysis$all_region_results$austria$region_results$bsts_local$predictive_samples)

# # y_true <- (analysis$all_region_results$austria$region_results$bsts_local$y)
# # forecast_run <- (analysis$all_region_results$austria$region_results$bsts_local$forecast_run)
# 	dates <- y_pred_samples$predicted_date
# 	y_pred_samples <- y_pred_samples[, grep("sample", colnames(y_pred_samples))]

# 	pred_mean <- rowMeans(y_pred_samples)
# 	pred_median <- apply(y_pred_samples, median, MARGIN = 1)
# 	pred_quantiles <- t(apply(y_pred_samples,
# 								MARGIN = 1,
# 								FUN = quantile,
# 								probs = c(0.025, 0.25, 0.75, 0.975),
# 							    na.rm = TRUE))

# 	df <- (cbind(as.data.frame(dates), y_true, pred_median,
# 							  pred_mean, pred_quantiles, forecast_run))
# 	colnames(df) <- c("date", "true", "median", "mean", "ci2.5",
# 					  "ci25", "ci75", "ci97.5", "forecast_run")

# 	start_data <- sum(is.na(forecast_run))
# 	interval <- sum(forecast_run == 1, na.rm = TRUE)

# 	## make vertical lines to display
# 	if (isTRUE(vlines)) {
# 		x <- unique(forecast_run)
# 		seq <- rep(NA, length(x) - 1)
# 		for (i in 1:(length(x) - 1)) {
# 			seq[i] <- which(forecast_run == x[i + 1] )[1] - 1
# 		}
# 		vlines <- rep(NA, nrow(df))
# 		vlines[seq] <- seq
# 	} else {
# 		vlines <- 0
# 	}

# 	plot <- ggplot(df, aes(x = dates), group = forecast_run) +
# 			geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5), alpha = 0.5,
# 						fill = "gray") +
# 			geom_ribbon(aes(ymin = ci25, ymax = ci75), alpha = 0.7,
# 						fill = "gray") +
# 			geom_line(aes(y = median), color = "darkgray", size=1) +
# 			geom_point(aes(y = median), color = "black", size=6) +
# 			geom_line(aes(y = true), size=1, color = "darkgray") +
# 			geom_point(aes(y = true), size=6, color = "black") +
# 			ggtitle(plottitle) +
# 			theme(text = element_text(family = 'Sans Serif'))

# 	if (isTRUE(vlines)) {
# 		plot <- plot + geom_vline(aes(xintercept = vlines))
# 	}
# 	return(plot)
# }


#' @title Plot prior vs posterior distribution of parameters
#'
#' @description
#' Missing.
#' Also Todo: handling for only one item
#' @param stanfitobjects list of stanfit objects
#'
#' @return
#' Missing
#' @examples
#' NULL
#' @export

plot_prior_vs_posterior <- function(stanfitobjects) {
	if (is.list(stanfitobjects)) {

	}

	make_prior_post_df <- function (stanfit, run = 1) {
		samples <- rstan::extract(stanfit)
		params <- names(samples)
		prior_params <- params[grep("_prior", params)]
		post_params <- sub("_prior", "", prior_params)

		prior_samples <- samples[prior_params]
		post_samples <- samples[post_params]

		df_prior <- lapply(seq_along(prior_samples),
                   FUN = function(i) {
                     data.frame(samples = as.vector(prior_samples[[i]]),
		   						type = c("prior"),
		   						name = post_params[i],
		   					    run = run)
                   })

		df_post <- lapply(seq_along(post_samples),
                   FUN = function(i) {
                     data.frame(samples = as.vector(post_samples[[i]]),
		   						type = c("posterior"),
		   						name = post_params[i],
		   						run = run)
                   })

		df_prior <- do.call(rbind, df_prior)
		df_post <- do.call(rbind, df_post)
		df <- rbind(df_prior, df_post)
		return(df)
	}

	dataframes <- lapply(seq_along(stanfitobjects),
						 FUN = function(i) {
						 	make_prior_post_df(stanfitobjects[[i]], run = i)
						 })

	df <- do.call(rbind, dataframes)

	minx <- min(df[df$type == "posterior", ]$samples)
	maxx <- max(df[df$type == "posterior", ]$samples)
	plot <- ggplot(df, mapping = aes(x = samples, fill = type)) +
				geom_density(alpha = 0.7, position = "identity") +
				facet_grid(run ~ name, scales = "free_y") +
				xlim(c(minx, maxx)) +
				theme(text = element_text(family = 'Serif'))

	return(list(plot = plot, df = df))

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
	p <- predict.bsts(bsts.model, horizon = 7, burn = 100, quantiles = c(.025, .975))
	
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






#' @title Wrapper to compare forecasts
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


compare_forecasts <- function(inputdata = NULL, region_results = NULL) {
	titles <- names(region_results)
	scores <- lapply(seq_along(region_results),
					 FUN = function (i) {
					 	y <- inputdata
					 	pred <- region_results[[i]]$predictive_samples
					 	pred <- pred[, grep("sample", colnames(pred))]
					 	forecast_run <-region_results[[i]]$forecast_run
					 	ind <- (!is.na(y) & !is.na(forecast_run))
					 	y <- y[ind]
					 	pred <- pred[ind, ]


					 	tmp <- scoringutils::eval_forecasts(
					 			true_values = y,
					 			predictions = pred,
					 			prediction_type = "probabilistic",
					 			outcome_type = "continuous")
					 	return(cbind(tmp, model = titles[i]))
					 })
	scores <- do.call(rbind, scores)
	scores <- scores[order(scores$mean, scores$model), ]

	rn <- rownames(scores)

	# countries <- gsub("\\..*","",rn)

	method <- gsub(".*\\.","",rn)
	method <- gsub('[[:digit:]]+', '', method)

	scores <- cbind(method = method, scores)


	return(scores)
}

#' @title Wrapper to select the best bsts model from the package
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



compare_bsts_models <- function(y) {
	bsts <- list()

	ss1 <- AddSemilocalLinearTrend(list(), y)
	ss2 <- AddLocalLinearTrend(list(), y)
	ss3 <- AddStudentLocalLinearTrend(list(), y)
	ss4 <- AddAr(list(), y, lags = 1)
	ss5 <- AddAr(list(), y, lags = 2)

	bsts$semilocal <- bsts::bsts(y, state.specification = ss1, niter = 1000, ping=0)
	bsts$local <- bsts::bsts(y, state.specification = ss2, niter = 1000, ping=0)
	bsts$local_student <- bsts::bsts(y, state.specification = ss3, niter = 1000, ping=0)
	bsts$ar1 <- bsts::bsts(y, state.specification = ss4, niter = 1000, ping=0)
	bsts$ar2 <- bsts::bsts(y, state.specification = ss5, niter = 1000, ping=0)

	CompareBstsModels(bsts)

}



#' @title Extract summary data.frame from forecasts
#'
#' @description
#' Missing.
#' Also Todo: handling for only one item
#' @param results_region_model results from the best model for a particular
#' region
#'
#' @return
#' Missing
#' @examples
#' NULL
#' @export

forecast_table <- function(results_region_model, region = NULL) {

	pred <- results_region_model$predictive_samples[is.na(results_region_model$y), ]

	pred <- pred[, grep("sample", colnames(pred))]

	median_3 <- median(as.numeric(pred[3, ]))
	mean_3 <- median(as.numeric(pred[3, ]))
	quantiles_3 <- quantile(as.numeric(pred[3, ]), c(0.025, 0.25, 0.75, 0.975))

	df <- data.frame(region = region,
					 median_3 = median_3,
					 mean_3 = mean_3,
					 "quantile_2.5" = quantiles_3[1],
					 "quantile_25" = quantiles_3[2],
					 "quantile_75" = quantiles_3[3],
					 "quantile_97.5" = quantiles_3[4])
	rownames(df) <- region
	return(df)
}






#' @title Predict Cases implied by the R_t predictions
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

# make_case_prediction <- function(region_results, data = data, region = NULL) {

# 	n_pred <- data$n_pred
# 	incidences <- data$incidences
# 	incidences <- incidences[incidences$region == region, ]
# 	models <- names(region_results)

# 	forecast_run <- region_results[[1]]$forecast_run
# 	r_true <- region_results[[1]]$y

# 	runs <- unique(forecast_run[!is.na(forecast_run)])

# 	## find parts of the incidence time series that are relevant

# 	nrow(incidences[(n_pred +1):nrow(incidences), ])





# 	for (run in runs) {
# 		for (model in models) {

# 			model_results <- region_results[[model]]
# 			predictive_samples <- model_results$predictive_samples
# 			index <- forecast_run == run
# 			index[is.na(index)] <- FALSE
# 		}

# 	}


# }





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

