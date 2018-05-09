#' General polynomial fitting function
#' 
#' Operates on a dataframe or dataframe subset and does a polynomial fit between two of the columns.
#' 
#' @param df dataframe or dataframe group
#' @param x.col name of the x column to be fit; must be a string
#' @param y.col name of the y column to be fit; must be a string
#' @param poly.degree optional; use to specify the degree of the polynomial model. Default is 2. Available options are 2 or 3.
#' @return Returns a one-row dataframe with all grouping variables, and the parameters of the polynomial model as columns of the dataframe.
#' @export


general.poly.fit <- function(df, x.col, y.col, poly.degree = 2) {
  x <- df[[x.col]]
  y <- df[[y.col]]
  fit <- lm(df[[y.col]] ~ poly(df[[x.col]], poly.degree, raw = T))
  
  if (poly.degree == 2) {
    intercept <- fit$coefficients[1]
    first <- fit$coefficients[2]
    second <- fit$coefficients[3]
    return(data.frame(intercept, first, second, fit.to = y.col))
  } else if (poly.degree == 3) {
    intercept <- fit$coefficients[1]
    first <- fit$coefficients[2]
    second <- fit$coefficients[3]
    third <- fit$coefficients[4]
    return(data.frame(intercept, first, second, third, fit.to = y.col))
  } else {
    print("poly.degree must be 2 or 3")
  }
  
}