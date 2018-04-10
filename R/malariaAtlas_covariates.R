#' @name malariaAtlas_covariate
#'
#' @title Covariate module: malariaAtlas_covariates
#'
#' @description Get coovariate rasters from the Malaria Atlas Project
#'   database.
#'
#' @details 
#' 
#' See the data explorer \link{www.map.ox.ac.uk/explorer} to view 
#'   available rasters.
#' 
#' @param surface The surface name. See the title column in 
#'  \link{\code{malariaAtlas::listRaster}} for options.
#' @param extent Either a length 4 numeric vector giving min longitude, max longitude, min latitude, max latitude; or an object of class Extent
#' @param year Numeric vector of same lengthh as \code{surface}. Which year to use for time varying rasters.
#'
#' @seealso \code{\link{malariaAtlas::getraster}} \code{\link{malariaAtlas::listRaster}}
#' @family covariate
#'
#' @author Tim Lucas, \email{timcdlucas@@gmail.com}
#'


malariaAtlas_covariates <- function(surface = 'ACTs', 
                                    extent = c(32, 48, 3, 14),
                                    year = rep(NA, length(surface))) {
  
  stopifnot(length(surface) == length(year))
  stopifnot(is.numeric(year) | is.na(year))
  
  
  extent <- matrix(extent, nrow = 2, byrow = TRUE)

  layer <- malariaAtlas::getRaster(surface = surface, 
                                   extent = extent, 
                                   year = year)
  
  return (layer)
}