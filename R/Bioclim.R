#' @title Covariate module: Bioclim
#'
#' @description Get worldclim environment data. Downloads then stores locally.
#'
#' @param extent Length 4 numeric vector giving min longitude, max longitude, min latitude, max latitude. 
#'
#' @param resolution Resolution in minutes. Must be one of 2.5, 5 or 10. Default is 5. 
#'
#' @param layers which bioclim layers to obtain, a vector of integers between 1 and 19.
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @name Bioclim
#' @family covariate
Bioclim <-
function(extent = c(-180, 180, -90, 90), resolution = 10, layers = 1:19) {
    
    if(!(resolution %in% c(2.5, 5, 10))){
      stop('only 2.5, 5 and 10 degree resolutions are supported currently')
    }
  
  if (length(layers) < 1 |
      length(layers) > 19 |
      !all(layers %in% 1:19)) {
    stop ('layers must be a vector of integers between 1 and 19 indicating layers to obtain')
  }

    stopifnot(length(extent) == 4)
    stopifnot(all(is.numeric(extent)))


    world <- getData('worldclim', var = 'bio', res = resolution)
    world <- world[[layers]]
    cropped <- crop(world, extent(extent))
    return (cropped)
  }
