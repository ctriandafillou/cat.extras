#' Fit induction curve data with a sigmoid
#' 
#' Fit log-transformed induction data
#' 
#' @param df Dataframe or dataframe subset with columns "timepoint", "med.red", "shock.pH.f", and "treatment" (last two are grouping variables if a dataframe subset)
#' @param start.list Edit the starting fitting parameters; default is c(a=1.8, b=0.03, c=120) (keep this form but change numbers)
#' @param x.cutoff maximum timepoint to consider in fitting; default is "none" (fits on all timepoints)
#' @param pr.params logical, whether or not to print the final parameters of the fitted model to screen
#' @param xmax maxiumum x value to fit to
#' @export
#' @return dataframe with columns "treatment", "shock.pH.f", "timepoint", and "med.red" for plotting



fit_induction_sigmoid_base <- function(df, start.list = c(a=1.8, b=0.03, c=120), x.cutoff = "none", pr.params = F,
                                       xmax = F, constrain.base = F) {
  #dataframe must have columns "timepoint", "med.red", "shock.pH.f" (grouping variable)
  xe = filter(df, timepoint < x.cutoff)$timepoint
  ye = filter(df, timepoint < x.cutoff)$med.red
  
  if(constrain.base) {
    d = 0
    names(d) = "d"
    start.list <- c(start.list, d)
    fitmodel <- nls(ye~a/(1 + exp(-b * (xe-c))) + d, start = start.list, algorithm = "port", lower = c(0, 0, 0, 0), control = c(maxiter = 200))
  } else{
    fitmodel <- nls(ye~a/(1 + exp(-b * (xe-c))) + 0, start=start.list, control = c(maxiter=200))
  }
  
  params = coef(fitmodel)
  
  #xt <- seq(min(xe), max(xe), by = 0.5)
  if(xmax) {
    xt <- seq(0, xmax, by = 0.5)
  } else{
    xt <- seq(0, 180, by = 0.5)
  }
  yt <- sigmoid(c(params, 0), xt)
  
  
  
  
  
  if(pr.params == T) {print(params)}
  
  setNames(data.frame(xt, yt), c("timepoint", "med.red")) %>% mutate(shock.pH.f = unique(df$shock.pH.f), treatment = unique(df$treatment))
}

