#' Scientific log2 scale
#'
#' Creates a nice log 2 scale for plots (goes with scales package and ggplot2).
#'
#' @examples ggplot(dat, aes(...)) + scale_x_loglog2()
#' @export

scale_loglog2 <- function(...) {
    list(scale_x_log2nice(...),scale_y_log2nice(...))
}