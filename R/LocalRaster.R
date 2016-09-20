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
#' @author Tim Lucas, Samuel Bosch, \email{timcdlucas@@gmail.com}
#' @section Version: 1.1
#' @section Date submitted: 2016-03-04
LocalRaster <-
function(rasters = 'myRaster'){

  zoon::GetPackage('assertthat')
  if (is.string(rasters)){
    ## Check if file exists
    if (length(list.files(dirname(rasters), paste('^', basename(rasters), '\\.', sep = ''))) > 0){
      ## Load raster from file
      raster <- raster(rasters)
    } else {
      ## Load raster from global environment (Reads: raster, rasterstack, or rasterbrick)
      raster <- eval(parse(text = rasters), envir = globalenv())
    }
  } else if (is.list(rasters)) {
    rasterList <- lapply(rasters, raster)
    raster <- stack(rasterList)
  } else if (is.vector(rasters) & typeof(rasters) == "character") {
    raster <- stack(rasters)
  } else if (is(rasters, "RasterLayer") || is(rasters, "RasterStack")) {
    raster <- rasters
  } else {
    stop("LocalRaster: rasters type is not supported (currently supported types: string, list of strings, vector of strings, RasterLayer and RasterStack)")
  }
  return(raster)
}
