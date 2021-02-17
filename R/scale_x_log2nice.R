#' Scientific log2 scale for x
#'
#' Creates a nice log 2 scale for plots (goes with scales package and ggplot2).
#'
#' @examples ggplot(dat, aes(...)) + scale_x_log2nice()
#' @export

scale_x_log2nice <- function(name=waiver(),omag=seq(-10,20),...) {
  breaks2 <- 2^omag
  scale_x_log10(name,breaks=breaks2,
		labels = parse(text=paste("2^{",omag,"}")),...)
}
