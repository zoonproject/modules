#' @name CarolinaWrenRasters
#'
#' @title Covariate rasters for the USA used in the Carolina Wren analysis
#'
#' @description Land cover and coordinate covariate data for the USA, used in the Carolina Wren MaxLike analysis. These data are made available in the maxlike R package. See \code{\link[maxlike]{carw.data}} for more details, and see the modules \code{CarolinaWrenPA} and \code{CarolinaWrenPO} for corresponding occurrence data. This module loads the data, converts them into a \code{RasterStack} object and transforms the coordinates to lat/longs.
#'
#' @details The six covariates are: Percentage cover of mixed forest, deciduous forest, coniferous forest and grassland, and the latitude and longitude of each cell. After converting to latlongs, the pixels have dimensions 0.29x0.223 decimal degrees and the extent is \code{c(-138.71, -52.58, 18.15, 54.95)}
#'

#'
#' @family covariate
#'
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-05-21
CarolinaWrenRasters <- function () 
{
    zoon::GetPackage("maxlike")
    data(carw, envir = environment())
    rl <- lapply(carw.data$raster.data, function(x) {
        m <- matrix(x, nrow = carw.data$dim[1], ncol = carw.data$dim[2], 
            byrow = TRUE)
        raster(m)
    })
    rs <- stack(rl)
    extent(rs) <- carw.data$ext
    names(rs) <- names(carw.data$raster.data)
    projection(rs) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    rs_wgs84 <- raster::projectRaster(rs, crs = "+init=epsg:4326")
    return(rs_wgs84)
}
