#' Paper plotting theme
#' 
#' Adding this function to a ggplot changes the theme
#' @param base_size text size; default is 10
#' @export


theme_ct_stresspH <- function(base_size=10) {
  theme_grey(base_size=base_size, base_family = "") %+replace% 
  theme(panel.grid=element_blank(),
  		  axis.text = element_text(size = 11),
        strip.text = element_text(size = 10),
        panel.background=element_blank(),
        axis.ticks=element_line(colour="grey20"),
        panel.border=element_rect(fill=NA),
        legend.background = element_blank(),
        legend.key.height = unit(5, "mm"),
        legend.key = element_blank(),
        strip.background = element_blank(),
        legend.box.spacing = unit(1, "mm")) 
}