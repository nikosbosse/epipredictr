# inc <- get_data()
#
# ggplot(inc, aes(time, daily)) + geom_line()
# ggplot(inc, aes(time, cum_confirm)) + geom_line()
#
#

# library(epipredictr)

# inc <- epipredictr::get_data()

# ts <- inc$daily

# fit <- epipredictr::linear_regression(x = 1:length(ts), 
# 									  num_pred = 10, 
# 									  y = ts) 


# samples <- stan::extract(fit)
