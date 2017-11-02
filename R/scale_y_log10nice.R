#' Scientific log scale for y
#'
#' Creates a nice log 10 scale for plots (goes with scales package and ggplot2).
#'
#' @examples ggplot(dat, aes(...)) + scale_y_log10nice()
#' @export

scale_y_log10nice <- function(name=waiver(),omag=seq(-10,20),...) {
    breaks10 <- 10^omag
    scale_y_log10(name,breaks=breaks10,labels=scientific_10(breaks10),...)
}