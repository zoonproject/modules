#' @title Covariate module: LocalRaster
#'
#' @description LocalRaster: Read in a single raster or a list of rasters and stack them.
#'
#' @details Read in a single raster or a list of rasters and stack them.
#'
#' @param rasters Either a string of the filename of the raster layer, a list or vector of strings of filenames to rasters that will be stacked, a RasterLayer or a RasterStack object. 
#'
#' @name LocalRaster
#' @family covariate
#' @author Tim Lucas \email{timcdlucas@@gmail.com} and Samuel Bosch \email{mail@samuelbosch.com}
#' @section Version: 1.1
#' @section Date submitted: 2016-03-03
LocalRaster <-
function(rasters='myRaster'){

  if(is.string(rasters)){
    raster <- raster(rasters)
  } else if(is.list(rasters)) {
    rasterList <- lapply(rasters, raster)
    raster <- stack(rasterList)
  } else if(is.vector(rasters) & typeof(rasters) == "character") {
    raster <- stack(rasters)
  } else if (is(rasters, "RasterLayer") || is(rasters, "RasterStack")) {
    raster <- rasters
  } else {
    stop("LocalRaster: rasters type is not supported (currently supported types: string, list of strings, vector of strings, RasterLayer and RasterStack)")
  }
  return(raster)
}
