#' @name CarolinaWrenPO
#'
#' @title Presence-only data for the Carolina Wren in the USA
#'
#' @description Occurrence locations for the Carolina Wren \emph{Thryothorus ludovicianus} at Breeding Bird Survey transects in 2006. These data are made available in the maxlike R package. See \code{\link[maxlike]{carw.data}} for more details, and see the module \code{CarolinaWrenPA} for the presence-absence version. This module loads the data, drops absence data, transforms the coordinates to lat/longs and formats the data for use in a zoon workflow.
#'
#' @details 
#'

#'
#' @family occurrence
#'
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#'
#' @section Data type: presence-only
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-05-21
CarolinaWrenPO <- function () 
{
    zoon::GetPackage("maxlike")
    data(carw, envir = environment())
    occ <- na.omit(carw.data$pa.data)
    occ <- occ[occ$y == 1, ]
    coords_raw <- SpatialPoints(occ[, c("X", "Y")], proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
    coords <- sp::spTransform(coords_raw, "+init=epsg:4326")
    data.frame(longitude = coords@coords[, "X"], latitude = coords@coords[, 
        "Y"], value = occ$y, type = "presence", fold = 1, stringsAsFactors = FALSE)
}
