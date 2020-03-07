library(epipredictr)
library(rstan)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
library(scoringutils)
options(width=as.integer(160))
library(dplyr)
library(cowplot)

inc <- epipredictr::get_data()
ts <- inc$daily
fit <- epipredictr::linear_regression(y = ts) 
p <- extract_samples(fit, predictive = F)
scoringutils::eval_forecasts(true_values = ts, predictions = p)
a <- fit_iteratively(ts)



source("R/utilities.R")
source("R/models.R")
source("R/stanmodels.R")


# ======================================================== #
# try estimates for R0 values
# ======================================================== #
d <- readRDS("data/time_varying_params.rds")[[1]]
y_true <- d$mean


# =======================================================
res <- epipredictr::bsts(y = d$mean, iter = 4000)

model_lin_reg <- stan_model(file = "./inst/stan/linear_regression.stan")
res_lin <- fit_iteratively(incidences = y_true, model = "lin_reg", n_pred = 7, 
					   max_n_past_obs = 7, vb = FALSE)

res_lin <- fit_iteratively(incidences = y_true, model = model_lin_reg, 
						   n_pred = 7, 
 					       max_n_past_obs = 7, vb = FALSE)

res_bsts <- fit_iteratively(incidences = y_true, 
							model = "bsts_local_trend", n_pred = 7, 
							max_n_past_obs = 7, vb = FALSE)


model_bsts <- stan_model(file = "./inst/stan/bsts.stan")
res_bsts <- fit_iteratively(incidences = y_true, 
							model = model_bsts, n_pred = 7, 
							max_n_past_obs = Inf, vb = FALSE)


model_bsts_local <- stan_model(file = "./inst/stan/bsts_local_trend.stan")
res_bsts_local <- fit_iteratively(incidences = y_true, 
								  model = model_bsts_local, 
								  n_pred = 7, 
								  max_n_past_obs = 7, vb = FALSE)


# =======================================================
## do plots
p_reg <- plot_pred_vs_true(y_pred_samples = res_lin$predictive_samples, 
						y_true = res_lin$y, 
						forecast_run = res_lin$forecast_run)


p_bsts <- plot_pred_vs_true(y_pred_samples = res_bsts$predictive_samples, 
						y_true = res_bsts$y, 
						forecast_run = res_bsts$forecast_run)

p_bsts_local <- plot_pred_vs_true(y_pred_samples = res_bsts_local$predictive_samples, 
						y_true = res_bsts_local$y, 
						forecast_run = res_bsts_local$forecast_run)


plot_grid(p_reg, p_bsts, p_bsts_local, labels = "AUTO", ncol = 1)

# =======================================================


# =======================================================
# do checking of predictions

## todo: get log_likelihood implemented in stan
# library(loo)
# loo::waic(a)
# a <- res_bsts$stanfitobjects[[2]]
# b <- extract_log_lik(a)

scoringutils::eval_forecasts(true_values = y[15:76], 
							 predictions = res_lin$predictive_samples[15:76, ])


# =======================================================
# plot prior vs. posterior 

posterior_samples <- rnorm(100000)
prior_function = rnorm

plot_prior_vs_posterior <- function(posterior_samples, 
							        prior_function, 
							        params_prior_function,
							        ...) {

	
	posterior <- as.vector(rnorm(1000000))
	posterior <- posterior[!is.na(posterior)]

	prior <- unlist(do.call(prior_function, 
						 	args = params_prior_function))  +1


	df <- data.frame(prior = prior, posterior = posterior)
	samples <- as.data.frame(as.vector(res$predictive_samples))

	plot <- ggplot2::ggplot(df, aes(x = posterior)) + 
							geom_histogram(fill = 'lightblue',
										   bins = 100, alpha = 0.3) + 
							geom_histogram(fill = 'red',
										   bins = 100, alpha = 0.3, 
										   aes (x = prior)) +
							stat_function(fun=dnorm)




# ggplot(posterior, aes(sample, variable, fill = variable)) +
#   ggridges::geom_density_ridges() +
#   geom_vline(xintercept = simple_reg.data$beta, linetype = 'dashed', colour = 'red') +
#   facet_wrap(~type) +
#   coord_cartesian(xlim = c(-4, 4)) +
#   guides(fill = F) +
#   labs(
#     title = 'Density of the prior and posterior',
#     x = '',
#     y = ''
#   )






my_plot_two_histograms <- function(vector1, vector2, 
								   breaks = 100, 
								   upper_limit = NULL){
	if(!is.null(upper_limit)){
		vector1 <- vector1[vector1 < upper_limit]
		vector2 <- vector2[vector2 < upper_limit]
	}

	## set breakpoints and define minimum 
	## breakpoint a and maximum breakpoint b
	a <- min(c(vector1, vector2)) 
	b <- max(c(vector1, vector2)) 

	## define axis
	ax <- pretty(a:b, n = breaks)

	while(min(ax) > a | max(ax) < b){
		if (min(ax) > a){
		a <- a - (ax[2] - ax[1])
		}
		if (max(ax) < b){
		b <- b + (ax[2] - ax[1])
		}
		ax <- pretty(a:b, n = breaks)
	}

	

	## make histogram A and B
	plot1 <- hist(vector1, breaks = ax, plot = FALSE)
	plot1$density = plot1$counts/sum(plot1$counts)
	plot2 <- hist(vector2, breaks = ax, plot = FALSE)
	plot2$density = plot2$counts/sum(plot2$counts)

	## set correct font
	par(family = "Serif")

	## define two colors
	col1 <- rgb(168,209,225,max = 255, alpha = 75)
	col2 <- rgb(248,183,193, max = 255, alpha = 75)

	## plot and add 2nd plot to first
	plot(plot1, col = col1, xlab = "vec1 is blue, vec2 is pink", xlim = c(a, b)) 
	plot(plot2, col = col2, add = TRUE) 
}






































# ======================
# try stuff with samples instead of point estimates for R0
beta_mu <- d$mean / (d$std)^2
alpha_mu <- d$mean * beta_mu
n <- length(beta_mu)

samples <- replicate(n = 2000, rgamma(n, shape = alpha_mu, rate = beta_mu))

t <- nrow(samples)
l <- list(N = t, 
		  y = samples, 
		  x = 1:t, 
		  n_samples = ncol(samples),
		  num_pred = 1)



stanfit1 <- rstan::stan(file = "./inst/stan/lin_reg_sampled_y.stan",
                        data = l,
                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))




library(EpiEstim)
library(dplyr)
library(tidyverse)

# ================================= #
# Load and prepare Data
# ================================= #

inc <- epipredictr::get_data()
ts <- pmax(0, inc$daily)


# ================================= #
# Rebuild EpiEstim in Stan
# ================================= #

t <- length(ts)
l <- list(t = t, obs_inc = ts, tau = 7, num_pred = 10)

stanfit2 <- rstan::stan(file = "./inst/stan/epiestim.stan",
                        data = l,
                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))
launch_shinystan(stanfit2)


# ================================= #
# Run EpiEstim in Stan
# ================================= #

# EpiEstim analysis for comparison
r <- estimate_R(ts, config = make_config(list(
                        cv_posterior=2,
                        t_start=2:(length(ts)-6),
                        t_end=8:length(ts),
                        mean_si= 15.3,
                        std_si= 9.3)),
                      method="parametric_si")

# ================================= #
# compare EpiEstim with Stan estimates
# ================================= #

s1 <- summary(stanfit2)$summary %>%
	as.data.frame() %>%
	rownames_to_column("var") %>%
	filter(grepl("^R", var)) %>%
	dplyr::select(-var) %>%
	mutate(estimate="stan") %>%
	dplyr::select(c(1,4,8,11)) %>%
	mutate(id=1:n())

s1 <- s1[15:nrow(s1),] %>%
	mutate(id=1:n())
colnames(s1) <- c("mean", "low", "high", "estimate", "id")

s2 <- r$R %>%
    dplyr::select(c(3,5,11)) %>%
    mutate(estimate="epiestim") %>%
    mutate(id=1:n())

s2 <- s2[8:nrow(s2),]%>%
	mutate(id=1:n())

colnames(s2) <- c("mean", "low", "high", "estimate", "id")

df <- bind_rows(s1, s2)

theme_set(theme_get() + theme(text = element_text(family = 'Serif')))

ggplot(df, aes(x=id, y=mean, ymin=low, ymax=high, color=estimate, fill=estimate)) +
	geom_line() +
	geom_ribbon(alpha=0.5) +
	coord_cartesian(ylim=c(0, 7.5))





nrow(s2)
nrow(s1)
length(ts)


























my_pred_vs_true_inc_plot <- function(y_true, 
									 y_pred, 
									 vert_lines = NULL){
	ymin <- min(c(y_true, y_pred))
	ymax <- max(c(y_true, y_pred))

	plot(y_true, type = "l", col = "grey", family = "Serif", ylim = c(ymin, ymax))
	lines(y_pred, col = "red", lwd = 3)

	if (!is.null(vert_lines) && vert_lines > 0){
		abline(v = vert_lines, col = "blue", lty = 2)
	}
}



vert_lines <- seq(from = interval, 
			  	  to = n_total, 
				  by = interval)

my_pred_vs_true_inc_plot(ts[15:86], rowMeans(a), vert_lines)