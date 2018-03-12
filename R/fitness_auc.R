#' Generate area under the curve for fitness data
#' 
#' Data should be of the form x = recovery time and y = log ratio of stressed to unstressed.
#' 
#' @param dat A dataframe or dataframe subset to act on
#' @param normalize Boolean; whether or not to normalize the data. Default is FALSE
#' @export
#' @return dataframe of area under the fitness curve


fitness_auc <- function(dat, normalize = FALSE) {
  if(normalize){
    mutate(dat, norm.growth.ratio = -(growth.ratio - max(growth.ratio, na.rm = T)))
  }
  
  auc <- Bolstad2::sintegral(dat$timepoint, dat$norm.growth.ratio)$int
  timerange <- max(dat$timepoint, na.rm = T) - min(dat$timepoint, na.rm = T)
  setNames(data.frame(auc, timerange), c("auc", "time.range"))
}