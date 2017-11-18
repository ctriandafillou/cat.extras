#' Calculate rolling mean of a generic column in a dataframe (timeseries)
#' 
#' Takes a subset of a dataframe that represents a single experiment as input and calculates a rolling mean on the data with binwidth k.
#'
#' @param dat A dataframe or dataframe subset to act on.
#' @param data.column a string representing the name of the column containing the timeseries observations
#' @param time.column a string representign the name of the column containing the time; default is "timepoint"
#' @param k Indicates method to use for binwidth. If k is an integer, this will be the number of samples to include in the averaging bin. If k is "auto", the binwidth will be determined for each subset individually, size will be determined by k_val
#' @param k_val Only used if k = "auto"; the percentage of the total number of observations that will be averaged over for each subset
#' @export
#' @return a dataframe with columns "Time" and "fit" representing the rolling mean at each timepoint.


gen_rolling_mean <- function(dat, data.column, time.column = "timepoint", k="auto", k_val=0.05) {
  y <- dat[data.column]
  if (k == "auto") {
    auto_k <- round(nrow(dat)*k_val)
    if (auto_k %% 2 == 0) {
      auto_k <- auto_k + 1
    }
    print(auto_k)
    fit <- rollmean(y, auto_k, fill = NA)
    x <- dat$timepoint
  } else {
    fit <- rollmean(y, k, fill = NA)
    x <- dat$timepoint
  }
  #x <- ifelse(is.integer(k/2), dat$Time[round(k/2):(length(dat$Time)-round(k/2)-1)], dat$Time[round(k/2):(length(dat$Time)-round(k/2))])
  setNames(data.frame(x, fit), c("timepoint", "fit"))
}