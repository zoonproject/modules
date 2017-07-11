#' @name Bioclim
#'
#' @title Covariate module: Bioclim
#'
#' @description Get worldclim environment data. Downloads then stores locally.
#'
#' @details 
#'
#' @param extent Either a length 4 numeric vector giving min longitude, max longitude, min latitude, max latitude; or an object of class Extent
#'
#' @param resolution Resolution in minutes. Must be one of 2.5, 5 or 10. Default is 10.
#'
#' @param layers Which bioclim layers to obtain, a vector of integers between 1 and 19.
#'
#' @family covariate
#'
#' @author ZOON Developers, \email{zoonproject@@@@gmail.com}
#'
#' @section Version: 1.1
#'
#' @section Date submitted:  2017-07-11
Bioclim <- function(extent = c(-180, 180, -90, 90), resolution = 10, layers = 1:5) {
    
    if(!(resolution %in% c(2.5, 5, 10))){
      stop('only 2.5, 5 and 10 degree resolutions are supported currently')
    }
    
    if (length(layers) < 1 |
        length(layers) > 19 |
        !all(layers %in% 1:19)) {
      stop ('layers must be a vector of integers between 1 and 19 indicating layers to obtain')
    }
    
  stopifnot(class(extent) %in% c("Extent", "numeric"))
  if(class(extent) == "numeric"){
    stopifnot(length(extent) == 4)
    stopifnot(all(is.numeric(extent)))
  }
    
    
    world <- raster::getData('worldclim', var = 'bio', res = resolution)
    world <- world[[layers]]
    cropped <- raster::crop(world, extent(extent))
    return (cropped)
  }
