#' pHluorin Calibration Curve Analysis
#' 
#' This function takes raw data from a calibration curve experiment (yeast expressing pHluorin, flow cytometry data) and does all processing steps to construct a calibration curve that maps fluorescence ratio to pH. Takes *raw* (untransformed) FITC and BV510 channels as inputs!
#' @param params character vector of parameters c(a, b, c, d); a is the max value, b is the steepness, c is the location of the inflection point, and d is the min value
#' @param x x values to generate the sigmoid for
#' @export
#' @return vector of sigmoid function values for each x point given

sigmoid <- function(params, x) {
  (params[1] / (1 + exp(-params[2] * (x-params[3])))) + params[4]
}