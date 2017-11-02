#' Density plotting theme
#' 
#' Adding this function to a ggplot changes the theme
#' @export



theme_density <- function(...) {
    list(scale_y_continuous(expand=c(0.01,0.01)),
         theme(panel.border=element_blank(),
               panel.background=element_blank(),
               axis.text.y=element_blank(),
               axis.title.y=element_blank(),
               axis.ticks.y=element_blank()),...)
}