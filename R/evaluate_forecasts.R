# ================================================= # 
# ================================================= # 
## empirical cumulative distribution function
my_F <- function(predictions, k){
	return(sum(predictions <= k) / length(predictions))
}


##' Performs an Anderson-Darling test for uniformity for a randomised PIT histogram using predictive Monte-Carlo samples
##'
##' See Eqs. 1 and 2 in Czado, Geniting and Held (2009), Predictive Model Assessment for Count Data, Biometrics Vol. 65, No. 4 (Dec., 2009), pp. 1254-1261
##' @param y vector of data (length n)
##' @param dat nxN matrix of predictive samples, n (number of rows) being the number of data points and N (number of columns) the number of Monte Carlo samples
##' @param J the number of bins in the PIT histogram. If not given, will use the square root of n
##' @param N the number of tests to perform, each time re-randomising the PIT
##' @importFrom goftest ad.test
##' @return the n p-values from the tests
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @export

pit_test_sample <- function(y, dat, J = NULL, N=10) {
    if (is.null(J)) J <- as.integer(round(sqrt(length(y))))

    f <- lapply(seq_along(y), function(i) ecdf(dat[i, ]))
    P_x <- vapply(seq_along(y), function(i) f[[i]](y[i]), .0)
    P_xm1 <- vapply(seq_along(y), function(i) f[[i]](y[i]-1), .0)
   
   (pvalues <-
        replicate(N, goftest::ad.test(P_xm1 + runif(length(P_x)) * (P_x - P_xm1))$p.value))

    test <-
        replicate(N, (P_xm1 + runif(length(P_x)) * (P_x - P_xm1)))

    u = P_xm1 + runif(length(P_x)) * (P_x - P_xm1)

    hist(rowMeans(test), breaks = J, family = "Serif")

    hist(u, family = "Serif")

    return(pvalues)
}


## Alternative Reimplementation that gives the same results
my_PIT <- function(true_values, samples){
	n <- length(true_values)
	n_pred <- ncol(samples)

	P_x <- vapply(seq_along(true_values), 
				  function(i) { sum(samples[i,] <= true_values[i]) / n_pred }, 
				  .0)

    P_xm1 <- vapply(seq_along(true_values), 
    				function(i) sum(samples[i,] <= true_values[i] - 1) / n_pred, 
    				.0)

	u <- P_xm1 + runif(n) * (P_x - P_xm1)
}


## simulate true values and samples
sim_true = y <- rnorm(100, mean = 1:100)
sim_estim = dat = replicate(5000, rnorm(n = 100, mean = 1:100))

## sanity check
dim(dat)
apply(dat, 1, mean)
dat = t(dat)


u_test <- my_PIT(sim_true, sim_estim)
hist(u_test, breaks = 10, family = "Serif")



##' Determines sharpness of an incidence forecast with an Anderson-Darling test of the randomised PIT histogram.
##'
##' @inheritParams pit_test_sample
##' @return data frame with mean and standard deviation of the resulting p values
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @seealso pit_test_sample
##' @param ... other arguments for \code{\link{pit_test_sample}}
calibration_sample <- function (y, dat, ...) {
    pvalues <- pit_test_sample(y, dat, ...)
    return(data.frame(mean=mean(pvalues), sd=sd(pvalues)))
}


sharpness_sample <- function (y, dat) {
    sharpness <- apply(dat, 1, mad)
    return(sharpness)
    return(data.frame(date=as.Date(rownames(dat)), sharpness=sharpness))
}

s = sharpness_sample(y, dat)

my_sharpness <- function(forecasts){
	#forecasts is a matrix with n columns where
	# every column [,i] has all the draws that correspond 
	# to one true_value[i]

	S <- function(predictions){
		1/0.675 * median(abs(predictions - median(predictions)))
	}
	res <- apply(forecasts, MARGIN = 2, FUN = S)
	return(res)
}

my_sharpness(t(dat)) - s

1/0.675