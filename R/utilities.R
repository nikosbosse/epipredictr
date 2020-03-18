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








# #' @title Wrapper to select the best bsts model from the package
# #'
# #' @description
# #' Missing.
# #' Also Todo: handling for only one item
# #' @param y
# #'
# #' @return
# #' Missing
# #' @examples
# #' NULL
# #' @export



# compare_bsts_models <- function(y) {
# 	bsts <- list()

# 	ss1 <- AddSemilocalLinearTrend(list(), y)
# 	ss2 <- AddLocalLinearTrend(list(), y)
# 	ss3 <- AddStudentLocalLinearTrend(list(), y)
# 	ss4 <- AddAr(list(), y, lags = 1)
# 	ss5 <- AddAr(list(), y, lags = 2)

# 	bsts$semilocal <- bsts::bsts(y, state.specification = ss1, niter = 1000, ping=0)
# 	bsts$local <- bsts::bsts(y, state.specification = ss2, niter = 1000, ping=0)
# 	bsts$local_student <- bsts::bsts(y, state.specification = ss3, niter = 1000, ping=0)
# 	bsts$ar1 <- bsts::bsts(y, state.specification = ss4, niter = 1000, ping=0)
# 	bsts$ar2 <- bsts::bsts(y, state.specification = ss5, niter = 1000, ping=0)

# 	CompareBstsModels(bsts)

# }


