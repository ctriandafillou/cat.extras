#' Scientific log-log scale
#'
#' Creates a nice log 10 scale for plots (goes with scales package and ggplot2).
#'
#' @examples ggplot(dat, aes(...)) + scale_loglog()
#' @export

scale_loglog <- function(...) {
    list(scale_x_log10nice(...),scale_y_log10nice(...))
}