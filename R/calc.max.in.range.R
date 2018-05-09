#' Calculate the x and y values in a model where y is maximum
#' 
#' This function is designed to evaluate a polynomial model (either calculating it from scratch using general.poly.fit or with a dataframe that already contains the output of general.poly.fit) between two variables, specified by the user, and then returns the (x,y) value where y is maximal in a given range. Since the model is a polynomial function, there should only ever be one max y.
#' 
#' @param df dataframe or dataframe group
#' @param poly.degree degree of the polynomial model; default is 2. Only 2 and 3 are currently available.
#' @param compute.model logical; whether or not to fit the underlying model. Default is F. If T, x.col and y.col must be specified.
#' @param y.col y column to fit in the model; required if compute.model = T
#' @param x.col x column to fit in the model; required if compute.model = T
#' @param x.min Minimum value of the range over which the maximum is to be computed
#' @param x.max Maximum value of the range over which the maximum is to be computed
#' @return Returns a one-row dataframe with all grouping variables and the (x,y) values corresponding to the value at which y is maximal in the range (xmin, xmax).
#' @export




calc.max.in.range <- function(df, poly.degree=2, compute.model = F, y.col = NULL, x.col = NULL,
                              x.min = 30, x.max = 200) {
  if (poly.degree == 2) {
    if (compute.model) {
      if (missing(y.col) | missing(x.col)) {
        stop("If computing the model you must specify the columns to fit with x.col and y.col")
      }
      model <- general.poly.fit(df, x.col = x.col, y.col = y.col, poly.degree = poly.degree)
      x <- seq(x.min, x.max, by = 0.01)
      y <- model$intercept + model$first*x + model$second*x^2
      predictions <- data.frame(max.y.x = x, max.y = y)
      return(filter(predictions, y == max(y)))
    } else {
      x <- seq(x.min, x.max, by = 0.01)
      y <- df$intercept + df$first*x + df$second*x^2
      predictions <- data.frame(max.y.x = x, max.y = y)
      return(filter(predictions, y == max(y)))
    }
  } else if (poly.degree == 3) {
    if (compute.model) {
      if (missing(y.col) | missing(x.col)) {
        stop("If computing the model you must specify the columns to fit with x.col and y.col")
      }
      model <- general.poly.fit(df, x.col = x.col, y.col = y.col, poly.degree = poly.degree)
      x <- seq(x.min, x.max, by = 0.01)
      y <- model$intercept + model$first*x + model$second*x^2 + model$third*x^3
      predictions <- data.frame(max.y.x = x, max.y = y)
      return(filter(predictions, y == max(y)))
    } else {
      x <- seq(x.min, x.max, by = 0.01)
      y <- df$intercept + df$first*x + df$second*x^2 + df$third*x^3
      predictions <- data.frame(max.y.x = x, max.y = y)
      return(filter(predictions, y == max(y)))
    }
  } else {
    stop("poly.degree must be 2 or 3")
  }
}