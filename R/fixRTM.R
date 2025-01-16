#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
fixRTM <- function(x) {
  x <- terra::rast(x)
  x[!is.na(x[])] <- 1
  RIArtm3 <- terra::rast(x)
  aaa <- terra::focal(RIArtm3, fun = "sum", na.rm = TRUE, w = 5)
  RIArtm2 <- raster::raster(x)
  RIArtm2[aaa[] > 0] <- 1
  RIArtm4 <- terra::rast(RIArtm2)
  bbb <- terra::focal(RIArtm4, fun = "sum", na.rm = TRUE, w = 5)
  ccc <- raster::raster(bbb)[] > 0 & !is.na(x[])
  RIArtm2[ccc] <- 1
  RIArtm2[!ccc & !is.na(x[])] <- 0
  sa <- sf::st_as_sf(stars::st_as_stars(RIArtm2), as_points = FALSE, merge = TRUE)
  sa <- sf::st_buffer(sa, 0)
  sa <- sf::as_Spatial(sa)
  sa <- terra::vect(sa)
  return(sa)
}
