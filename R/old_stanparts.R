

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


## add to analysis_one_region

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


