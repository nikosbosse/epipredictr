library(ggplot2)
library(scoringutils)
options(width=as.integer(160))
library(dplyr)
library(cowplot)
library(patchwork)
library(bsts)
library(furrr)
library(future)
par(family = "Serif")

source("R/utilities.R")
source("R/forecast.R")
source("R/incidence_prediction.R")
source("R/scoring.R")
source("R/plotting.R")

future::plan("multiprocess", workers = future::availableCores() / 2)

# ======================================================== #
#
# ======================================================== #


models <- c("local", "semilocal", "local_student", "ar1", "ar2")
inputdata <- load_all_timeseries()
incidences <- load_all_timeseries(ts_type = "incidences")

data <- list(inputdata = inputdata, 
			 last_date = max(inputdata$date),
             models = models,
             incidences = incidences,
             n_pred = 14,
             start_period = 4)

analysis <- full_analysis(data)
saveRDS(analysis, file = "data/analysis/analysis.rds")

full_predictive_samples <- analysis$full_predictive_samples
# 
all_scores <- scoring(data, analysis$full_predictive_samples)
saveRDS(all_scores, file = "data/analysis/all_scores.rds")

aggregate_scores <- aggregate_scores(all_scores)
saveRDS(aggregate_scores, file = "data/analysis/aggregate_scores.rds")

plot_scoring <- plot_scoring(data, aggregate_scores, all_scores)
saveRDS(plot_scoring, file = "data/analysis/plot_scoring.rds")

plot_predictions <- plot_predictions(data, full_predictive_samples, best_model = "semilocal")
saveRDS(plot_predictions, file = "data/analysis/plot_predictions.rds")





## do everything for incidences as well
predicted_incidences <- predict_incidences(data, full_predictive_samples)

saveRDS(predicted_incidences, file = "data/analysis/predicted_incidences.rds")

all_scored_incidences <- scoring(data, analysis$full_predictive_samples, 
								 incidences = incidences, scoringtype = "Inc", 
								 predicted_incidences = predicted_incidences)
saveRDS(all_scored_incidences, file = "data/analysis/all_scored_incidences.rds")

aggregated_incidence_scores <- aggregate_scores(all_scored_incidences)
saveRDS(aggregated_incidence_scores, file = "data/analysis/aggregated_incidence_scores.rds")

plot_predictions_incidences <- plot_predictions(data, full_predictive_samples, 
									 best_model = "semilocal", incidences, 
									 type = "inc")
saveRDS(plot_predictions, file = "data/analysis/plot_predictions.rds")






























# # ======================
# # try stuff with samples instead of point estimates for R0
# beta_mu <- d$mean / (d$std)^2
# alpha_mu <- d$mean * beta_mu
# n <- length(beta_mu)

# samples <- replicate(n = 2000, rgamma(n, shape = alpha_mu, rate = beta_mu))

# t <- nrow(samples)
# l <- list(N = t,
# 		  y = samples,
# 		  x = 1:t,
# 		  n_samples = ncol(samples),
# 		  num_pred = 1)



# stanfit1 <- rstan::stan(file = "./inst/stan/lin_reg_sampled_y.stan",
#                         data = l,
#                         iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))





# library(EpiEstim)
# library(dplyr)
# library(tidyverse)

# # ================================= #
# # Load and prepare Data
# # ================================= #

# inc <- epipredictr::get_data()
# ts <- pmax(0, inc$daily)


# # ================================= #
# # Rebuild EpiEstim in Stan
# # ================================= #

# t <- length(ts)
# l <- list(t = t, obs_inc = ts, tau = 7, num_pred = 10)

# stanfit2 <- rstan::stan(file = "./inst/stan/epiestim.stan",
#                         data = l,
#                         iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))
# launch_shinystan(stanfit2)


# # ================================= #
# # Run EpiEstim in Stan
# # ================================= #

# # EpiEstim analysis for comparison
# r <- estimate_R(ts, config = make_config(list(
#                         cv_posterior=2,
#                         t_start=2:(length(ts)-6),
#                         t_end=8:length(ts),
#                         mean_si= 15.3,
#                         std_si= 9.3)),
#                       method="parametric_si")

# # ================================= #
# # compare EpiEstim with Stan estimates
# # ================================= #

# s1 <- summary(stanfit2)$summary %>%
# 	as.data.frame() %>%
# 	rownames_to_column("var") %>%
# 	filter(grepl("^R", var)) %>%
# 	dplyr::select(-var) %>%
# 	mutate(estimate="stan") %>%
# 	dplyr::select(c(1,4,8,11)) %>%
# 	mutate(id=1:n())

# s1 <- s1[15:nrow(s1),] %>%
# 	mutate(id=1:n())
# colnames(s1) <- c("mean", "low", "high", "estimate", "id")

# s2 <- r$R %>%
#     dplyr::select(c(3,5,11)) %>%
#     mutate(estimate="epiestim") %>%
#     mutate(id=1:n())

# s2 <- s2[8:nrow(s2),]%>%
# 	mutate(id=1:n())

# colnames(s2) <- c("mean", "low", "high", "estimate", "id")

# df <- bind_rows(s1, s2)

# theme_set(theme_get() + theme(text = element_text(family = 'Serif')))

# ggplot(df, aes(x=id, y=mean, ymin=low, ymax=high, color=estimate, fill=estimate)) +
# 	geom_line() +
# 	geom_ribbon(alpha=0.5) +
# 	coord_cartesian(ylim=c(0, 7.5))





# nrow(s2)
# nrow(s1)
# length(ts)





















