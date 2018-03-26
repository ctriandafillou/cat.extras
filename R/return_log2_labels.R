#' Helper function to make log2 labels for axes
#'
#' @param expo A list of exponents; can be a numeric or character vector 
#' @return A list of expressions of the form 2^
#'
#' @export

return_log2_labels <- function(expo) {
  lapply(expo, function(x) bquote(2^.(x)))
}