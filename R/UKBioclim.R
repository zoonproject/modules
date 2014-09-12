#'Covariate module: UKBioclim
#' Load Bioclim rasters at 5 degree resolution for the UK
#' (a region bounded by longitudes -12 and 4 and by latitudes 50 and 60).
#' The first time this module is used the data are downloaded from the web,
#' subsequently a local copy is used so it's much faster.
#' This is essentially a wrapper around the `raster` function `getData`,
#' see `?getData' for more details of what that function does.

#'@name UKBioclim
UKBioclim <-
  function() {
    world <- getData('worldclim', var = 'bio', res = 5)
    uk <- crop(world, extent(-10, 10, 45, 65))
    return (uk)
  }
