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



fit_iteratively <- function(incidences, 
							n_pred = 14, 
							interval = NULL,
							start_period = 8,
							max_n_past_obs = Inf,
							model = "bsts",
							n_samples = 4000,
							vb = FALSE,
							fit_type = "stan",
							length_local_trend = 7,
							iter = 4000,
							...) {
	## track time
	time <- Sys.time()

	if (is.null(interval)) interval <- n_pred

	## calculate number of fits to do
	total_n <- (length(incidences))
	n_runs <- ceiling((total_n - (start_period - 1)) / interval)

	## initialize empty lists to hold the results
	stanfitobjects <- list()
	predictive_samples <- list()
	forecast_run <-list()

	## do iterative fitting
	i <- 0
	current_last_obs <- start_period - 1
	if (current_last_obs > total_n) stop("start_period later than length of series")

	while (current_last_obs < total_n){
		cat("run ", as.character(i + 1), "of ", as.character(n_runs), "\n")

		## determine current indices and make data
		index <- 1:current_last_obs
		if (length(index) > max_n_past_obs) {
			current_earliest_obs <- length(index) - max_n_past_obs + 1
			index <- index[current_earliest_obs:current_last_obs]
		}
		y <- incidences[index]
		i <- i + 1

		if (fit_type == "stan") {
			stanfit <- fit_stan_model(y, model, n_pred = n_pred, vb = vb, 
								      length_local_trend = length_local_trend, 
								      iter = iter)

			## store results
			stanfitobjects[[i]] <- stanfit
			predictive_samples[[i]] <- extract_samples(stanfit)
		} else {
			predictive_samples[[i]] <- bsts_wrapper(y, model, 
									   num_pred  = n_pred)
		}

		forecast_run[[i]] <- rep(i, nrow(predictive_samples[[i]]))
										 
		current_last_obs <- current_last_obs + interval
	}


    predictive_samples <- do.call("rbind", predictive_samples)
    forecast_run <- do.call("c", forecast_run)

    ## cap predictions to match the length of the 
    ## true_values minus the starting data
    last_n_to_display <- nrow(predictive_samples) - (current_last_obs - total_n)
	predictive_samples <- predictive_samples[1:last_n_to_display, ]
    forecast_run <- forecast_run[1:last_n_to_display]

	## do another last prediction into the future and store results
	index <- 1:total_n
		if (length(index) > max_n_past_obs) {
			current_earliest_obs <- length(index) - max_n_past_obs + 1
			index <- index[current_earliest_obs:total_n]
		}
	y <- incidences[index]
	i <- i + 1
	if (fit_type == "stan") {
			stanfit <- fit_stan_model(y, model, 
							  n_pred = n_pred, 
							  vb = vb, 
							  iter = iter,
							  length_local_trend = length_local_trend)

			stanfitobjects[[i]] <- stanfit
			predictive_samples <- rbind(predictive_samples, 
								     	extract_samples(stanfit))
		} else {
			predictive_samples <- rbind(predictive_samples,			
										bsts_wrapper(y, model,
												     num_pred  = n_pred))
		}

	forecast_run <- c(forecast_run, rep(i, n_pred))

    ## add NAs to the predictions and the true_values
    predictive_samples <- rbind(matrix(NA, nrow = start_period - 1, 
    									   ncol = ncol(predictive_samples)), 
								predictive_samples)
    y <- c(incidences, rep(NA, n_pred))
    forecast_run <- c(rep(NA, start_period - 1), forecast_run)


	print(Sys.time() - time)
	return(list(predictive_samples = predictive_samples, 
		        forecast_run = forecast_run, 
		        stanfitobjects = stanfitobjects, 
		    	y = y)) 

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



plot_pred_vs_true <- function(y_true, 
							  y_pred_samples,
							  forecast_run = NULL, 
							  plottitle = "Pred vs. True"){
	pred_mean <- rowMeans(y_pred_samples)
	pred_median <- apply(y_pred_samples, median, MARGIN = 1)
	pred_quantiles <- t(apply(y_pred_samples, 
								MARGIN = 1, 
								FUN = quantile, 
								probs = c(0.025, 0.25, 0.75, 0.975), 
							    na.rm = TRUE))

	df <- as.data.frame(cbind(y_true, pred_median, 
							  pred_mean, pred_quantiles, forecast_run))
	colnames(df) <- c("true", "median", "mean", "ci2.5", 
					  "ci25", "ci75", "ci97.5", "forecast_run")

	start_data <- sum(is.na(forecast_run))
	interval <- sum(forecast_run == 1, na.rm = TRUE)

	## make vertical lines to display
	x <- unique(forecast_run)
	seq <- rep(NA, length(x) - 1)
	for (i in 1:(length(x) - 1)) {
		seq[i] <- which(forecast_run == x[i + 1] )[1] - 1
	}
	vlines <- rep(NA, nrow(df))
	vlines[seq] <- seq


	plot <- ggplot(df, aes(x = 1:nrow(df)), group = forecast_run) +
			geom_ribbon(aes(ymin =ci2.5, ymax = ci97.5), alpha = 0.3, 
						fill = "lightblue") +
			geom_ribbon(aes(ymin = ci25, ymax = ci75), alpha = 0.9, 
						fill = "lightblue") + 
			geom_line(aes(y = median), color = "blue") +
			geom_line(aes(y = true)) +
			geom_vline(aes(xintercept = vlines)) +
			ggtitle(plottitle) +
			theme(text = element_text(family = 'Serif'))
	return(plot)
}


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
#' @param y 
#' 
#' @return
#' Missing
#' @examples
#' NULL
#' @export 

bsts_wrapper <- function(y, model,
						 num_pred = 7, 
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
	return(t(p$distribution))

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


#' @title Wrapper to make a visual plot to compare the outputs of do_all_fits
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


plot_forecast_compare <- function(pred_results) {
	titles <- names(pred_results)
	plots <- lapply(seq_along(pred_results), 
					FUN = function (i) {
						plot_pred_vs_true(
						 y_pred_samples = pred_results[[i]]$predictive_samples, 
						 y_true = pred_results[[i]]$y, 
						 forecast_run = pred_results[[i]]$forecast_run, 
						 plottitle = titles[i]
						)
			        })

	(p <- wrap_plots(plots, ncol = 1))
	return(p)

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


compare_forecasts <- function (pred_results) {
	titles <- names(pred_results)
	scores <- lapply(seq_along(pred_results), 
					 FUN = function (i) {
					 	y <- pred_results[[i]]$y
					 	pred <- pred_results[[i]]$predictive_samples
					 	forecast_run <- pred_results[[i]]$forecast_run
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


add_average_model <- function(pred_results) {
	tmp <- lapply(seq_along(pred_results), 
				  FUN = function(i) {
				  	p <- pred_results[[i]]
				  	p <- p$predictive_samples
				  	return(p)
				  })

	pred <- tmp[[1]]
	for (i in 2:length(pred_results)) {
		pred <- pred + tmp[[i]] 
	}
	avg <- pred / length(pred_results)
	pred_results$average$predictive_samples <- avg
	pred_results$average$y <- pred_results[[1]]$y
	pred_results$average$forecast_run <- pred_results[[1]]$forecast_run
	return(pred_results)
}

#' @title Extract summary data.frame from forecasts
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

forecast_table <- function(pred_result, country = "country") {
	pred <- pred_result$predictive_samples[is.na(pred_result$y), ]

	median_3 <- median(pred[3, ])
	mean_3 <- median(pred[3, ])
	quantiles_3 <- quantile(pred[3, ], c(0.025, 0.25, 0.75, 0.975))	

	df <- data.frame(country = country, 
					 median_3 = median_3, 
					 mean_3 = mean_3, 
					 "quantile_2.5" = quantiles_3[1], 
					 "quantile_25" = quantiles_3[2], 
					 "quantile_75" = quantiles_3[3],
					 "quantile_97.5" = quantiles_3[4])
	rownames(df) <- country
	return(df)
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

analysis_one_country <- function(y, country = "country", plot = F) {
	analysis <- list()
	analysis$country <- country
	analysis$forecast_res <- forecast_one_country(y, include_stan = F)
	analysis$forecast_res <- add_average_model(analysis$forecast_res)
	analysis$forecast_table <- compare_forecasts(analysis$forecast_res)
	
	if(isTRUE(plot)) compare_bsts_models(y)

	analysis$forecast_plot <- plot_forecast_compare(analysis$forecast_res)
	return(analysis)
}




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

full_analysis <- function(timeseries, countries) {
	res <- lapply(seq_along(timeseries), 
				  FUN = function(i) {				  	
				  	return(analysis_one_country(timeseries[[i]], countries[i]))
				  })

	names(res) <- countries 

	tables <- lapply(res, '[[', 3)
	tables <- lapply(seq_along(tables), 
 					 FUN = function(i) {
 					 	cbind(country = countries[i], tables[[i]])
 					 })
	table <- do.call(rbind, tables)

	lineplot <- ggplot(data = table, 
		   aes(y = mean, color = (model), x = country, group = model)) +
	  geom_point() +
	  geom_line() +
	  facet_wrap(~ method, ncol = 1, scales = "free_y") +
	  theme(text = element_text(family = 'Serif'))

	boxplot <- ggplot(data = table, 
		   aes(y = mean, color = (model), x = country, group = model)) +
	  geom_boxplot() +
	  facet_wrap(~ method, ncol = 1, scales = "free_y") +
	  theme(text = element_text(family = 'Serif'))	

	table_mean <- aggregate(mean ~ method + model, df, mean)
	table_mean <- table_mean[order(table_mean$method, table_mean$mean), ]	

	table_mean <- aggregate(mean ~ method + model, df, mean)
	table_median <- table_median[order(table_mean$method, table_mean$mean), ]	


	out <- list(analysis_results = res, 
				analysis_table = table, 
				lineplot = lineplot, 
				boxplot = boxplot. 
				table_mean = table_mean, 
				table_median = table_median)

	return(out)
}
