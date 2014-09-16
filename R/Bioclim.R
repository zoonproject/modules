#'Covariate module: Bioclim
#'
#'Get worldclim environment data. Downloads then stores locally.
#'
#'@param extent Length 4 character vector giving min lat, max lat, min long, max long. 
#'
#'@param resolution Resolution in minutes. Must be one of 2.5, 5 or 10. Default is 5. 
#'
#'@name Bioclim
Bioclim <-
function(extent, resolution = 5) {
    
    if(!(res %in% c(2.5, 5, 10))){
      stop('only 2.5, 5 and 10 degree resolutions are supported currently')
    }

    assert_that(length(extent) == 4)
    assert_that(all(is.numeric(extent)))


    world <- getData('worldclim', var = 'bio', res = resolution)
    cropped <- crop(world, extent(extent))
    return (coppred)
  }
