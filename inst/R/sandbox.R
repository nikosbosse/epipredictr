library(epipredictr)
library(rstan)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
library(scoringutils)
options(width=as.integer(160))
library(dplyr)
library(cowplot)
library(patchwork)
library(bsts)
par(family = "Serif")

# inc <- epipredictr::get_data()
# ts <- inc$daily
# fit <- epipredictr::linear_regression(y = ts) 
# p <- extract_samples(fit, predictive = F)
# scoringutils::eval_forecasts(true_values = ts, predictions = p)
# a <- fit_iteratively(ts)

source("R/utilities.R")
source("R/models.R")



# ======================================================== #
# 
# ======================================================== #

sk <- readRDS("data/time_varying_params_south_korea.rds")[[1]]
it <- readRDS("data/time_varying_params_italy.rds")[[1]]
# uk <- readRDS("data/time_varying_params_uk.rds")[[1]]
jp <- readRDS("data/time_varying_params_japan.rds")[[1]]
sp <- readRDS("data/time_varying_params_singapore.rds")[[1]]

# model_lin_reg <- stan_model(file = "./inst/stan/linear_regression.stan")
# model_bsts <- stan_model(file = "./inst/stan/bsts.stan")
# model_bsts_local_trend <- stan_model(file = "./inst/stan/bsts_local_trend.stan")
# models <- list(model_lin_reg, model_bsts, model_bsts_local_trend, "local", "semilocal")

y_sk <- sk$median
y_jp <- jp$median
y_sp <- sp$median
y_it <- it$median
timeseries <- list(y_sk, y_jp, y_sp, y_it)
countries <- c("South_Korea", "Japan", "Singapore", "Italy")

c <- analysis_one_country(y_sk, "South Korea", plot = T)
c$forecast_plot

analysis <- full_analysis(timeseries, countries)

(analysis$analysis_results$Japan$forecast_plot)





df




aggregate(df$mean, list(df$method, df$model), mean)


tmp <- list()
i = 1
for (model in c("CRPS", "logs")) {
	for (country in countries)
		index <- df$country == country & df$method == method
		tmp[[i]] <- mean(df[df])
}




compare_forecasts(analysis$analysis_results$Japan$forecast_res)


cbind(analysis$analysis_table, country = countries, method = method)



forecast_table(res_sk[[1]])

y_sk <- sk$median
res_sk <- forecast_one_country(y_sk, models, include_stan = F)
res_sk <- add_average_model(res_sk)
(plot_sk <- plot_forecast_compare(res_sk))
compare_forecasts(res_sk2)
compare_bsts_models(y_sk)
ggsave("vignettes/figure/south_korea.png", plot_sk)

y_jp <- jp$median
res_jp <- do_all_fits(y_jp, models, include_stan = F)
plot_jp <- plot_forecast_compare(res_jp)
compare_forecasts(res_jp)
ggsave("./vignettes/figure/japan.png", plot_jp)

y_sp <- sp$median
res_sp <- do_all_fits(y_sp, models, include_stan = F)
plot_sp <- plot_forecast_compare(res_sp)
compare_forecasts(res_sp)
ggsave("vignettes/figure/singapore.png", plot_sp)

y_it <- it$median
res_it <- do_all_fits(y_it, models, include_stan = F)
plot_it <- plot_forecast_compare(res_it)
compare_forecasts(res_it)
ggsave("./vignettes/figure/italy.png", plot_it)




















# =======================================================






# =======================================================
## do plots
p_reg <- plot_pred_vs_true(y_pred_samples = res_lin$predictive_samples, 
						y_true = res_lin$y, 
						forecast_run = res_lin$forecast_run)


p_bsts <- plot_pred_vs_true(y_pred_samples = res_bsts$predictive_samples, 
						y_true = res_bsts$y, 
						forecast_run = res_bsts$forecast_run)
p_bsts

p_bsts_local <- plot_pred_vs_true(y_pred_samples = res_bsts_local$predictive_samples, 
						y_true = res_bsts_local$y, 
						forecast_run = res_bsts_local$forecast_run)
p_bsts_local

plot_grid(p_reg, p_bsts, p_bsts_local, labels = "AUTO", ncol = 1)





# =======================================================

## do checks for a single time series

bsts_stanfit


prior <- SdPrior(sigma.guess = sdy,
                       sample.size = .01,
                       upper.limit = sigma.upper.limit)

str(prior)
# =======================================================
# do checking of predictions

## todo: get log_likelihood implemented in stan
# library(loo)
# loo::waic(a)
# a <- res_bsts$stanfitobjects[[2]]
# b <- extract_log_lik(a)


# =======================================================
# plot prior vs. posterior 

prior_post <- plot_prior_vs_posterior(res_bsts$stanfitobjects)
prior_post$plot












































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