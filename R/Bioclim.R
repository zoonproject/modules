#'Covariate module: Bioclim
#'
#'Get worldclim environment data. Downloads then stores locally.
#'
#'@param extent Length 4 numeric vector giving min longitude, max longitude, min latitude, max latitude. 
#'
#'@param resolution Resolution in minutes. Must be one of 2.5, 5 or 10. Default is 5. 
#'
#'@name Bioclim
#'@family covariate
Bioclim <-
function(extent, resolution = 5) {
    
    if(!(resolution %in% c(2.5, 5, 10))){
      stop('only 2.5, 5 and 10 degree resolutions are supported currently')
    }

    stopifnot(length(extent) == 4)
    stopifnot(all(is.numeric(extent)))


    world <- getData('worldclim', var = 'bio', res = resolution)
    cropped <- crop(world, extent(extent))
    return (cropped)
  }
