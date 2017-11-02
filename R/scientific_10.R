#' Scientific 10
#'
#' This is a helper function used to construct nice scales for plots (goes with scales package and ggplot2).
#'
#' @export

scientific_10 <- function(x) {
    xout <- gsub("1e", "10^{", format(x),fixed=TRUE)
    xout <- gsub("{-0", "{-", xout,fixed=TRUE)
    xout <- gsub("{+", "{", xout,fixed=TRUE)
    xout <- gsub("{0", "{", xout,fixed=TRUE)
    xout <- paste(xout,"}",sep="")
    return(parse(text=xout))
}