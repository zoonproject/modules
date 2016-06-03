#' @name NaiveRandomRaster
#'
#' @title Naive Random Raster
#'
#' @description Creates a random raster layer within the extent given
#'
#' @details 
#'
#' @param extent A numeric vector of length 4 giving the coordinates of the rectangular region within which to create the raaster. order: xmin, xmax, ymin, ymax. By default the extent of the UK
#' 
#' @param res Numeric giving the resolution of the raster to produce. 1 is one cell per degree, 0.5 means one cell per half degree.
#' 
#' @param seed Used with set.seed to set a seed. Default NULL, no seed is used
#'
#' @family covariate
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-02
NaiveRandomRaster <- function (extent = c(-10, 10, 45, 65), res = 0.5, seed = NULL) 
{
    if (length(extent) != 4 | !inherits(extent, "numeric")) 
        stop("extent must be a numeric vector of length 4")
    if (extent[1] >= extent[2]) 
        stop("in extent - min x must be smaller than max x")
    if (extent[3] >= extent[4]) 
        stop("in extent - min y must be smaller than max y")
    nrow <- (extent[2] - extent[1])/res
    ncol <- (extent[4] - extent[3])/res
    if (!is.null(seed)) 
        set.seed(seed)
    rasMat <- matrix(data = runif(n = nrow * ncol, min = 0, max = 100), 
        nrow = nrow, ncol = ncol)
    ras <- raster::raster(x = rasMat, xmn = extent[1], xmx = extent[2], 
        ymn = extent[3], ymx = extent[4], crs = "+proj=longlat +datum=WGS84")
    return(ras)
}
