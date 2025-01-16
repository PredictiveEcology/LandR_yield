#' Prepare RIA layer from BC_TSA layer
#'
#' @param x BC_TSA layer
#'
#' @return a single terra Polygon
getRIA <- function(x) {
  x <- terra::vect(x)
  ria <- x[x$TSA_NUMBER %in% c('08', '16', '24', '40', '41'),]
  ria <- terra::aggregate(ria)
  return(ria)
}
