library(epipredictr)
library(rstan)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
library(scoringutils)
options(width=as.integer(160))
library(dplyr)


inc <- epipredictr::get_data()
ts <- inc$daily
fit <- epipredictr::linear_regression(y = ts) 
p <- extract_samples(fit, predictive = F)
scoringutils::eval_forecasts(true_values = ts, predictions = p)
a <- fit_iteratively(ts)





# ======================================================== #
# try estimates for R0 values
# ======================================================== #
d <- readRDS("data/time_varying_params.rds")[[1]]

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


# =====================
# try bsts


l <- list(N = n, 
		  y = d$mean,
		  n_pred = 1)


stanfit2 <- rstan::stan(file = "./inst/stan/bsts.stan",
                        data = l,
                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))



my_stan_bsts <- function(y, n_pred = 10){
	n <- length(y)
	l <- list(N = n, y = y, n_pred = n_pred)

	stanfit2 <- rstan::stan(file = "./inst/stan/bsts.stan" ,
	                        data = l,
	                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))

	sum <- summary(stanfit2)$summary
	sum <- sum %>% as.data.frame(rownames(sum)) %>% mutate(var = rownames(sum)) 

	params <- sum %>% filter(sum$var %in% c("sigma_epsilon", "sigma_eta", "phi", "D"))
	rownames(params) <- params$var

	predicted <- sum %>% filter(grepl("^y_pred", var))
	rownames(predicted) <- predicted$var

	res <- list(params = params, predicted = predicted, stanfit = stanfit2)

	r <- res$predicted
	r <- r[,c(1,4,8)]
	colnames(r) <- c("mean", "low", "high")


	res$plot <- ggplot(r, aes(x = 1:n_pred, y = mean, ymin = low, ymax = high)) + geom_line() + geom_ribbon(alpha = 0.5)

	return(res)
}

res <- my_stan_bsts(d$mean)















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