#' @name NaiveRandomPresence
#'
#' @title Naive Random Presence
#'
#' @description Creates a random occurrence dataset within the extent given
#'
#' @details Points are created entirely at random using runif
#'
#' @param extent A numeric vector of length 4 giving the coordinates of the rectangular region within which to create the random points. order: xmin, xmax, ymin, ymax. By default the extent of the UK.
#' 
#' @param n Numeric - the number of points to create
#' 
#' @param seed Used with set.seed to set a seed. Default NULL, no seed is used
#' 
#' @param projection The projection for your points (as a proj4string)
#'
#' @family occurrence
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#' @section Data type: presence-only
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-05-31
NaiveRandomPresence <- function (extent = c(-10, 10, 45, 65), n = 1000,
                                 seed = NULL, projection = NULL) 
{
    if (!is.null(seed)) 
        set.seed(seed)
    long <- runif(n = n, min = extent[1], max = extent[2])
    lat <- runif(n = n, min = extent[3], max = extent[4])
    occurrence <- data.frame(longitude = long, latitude = lat, 
        value = 1, type = "presence", fold = 1, stringsAsFactors = FALSE)
    if(!is.null(projection)) occurrence$crs <- projection
    return(occurrence)
}
