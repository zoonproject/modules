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
#'  \code{\link{malariaAtlas::listRaster}} for available raster names.
#' @param extent Either a length 4 numeric vector giving min longitude, max longitude, min latitude, max latitude; or an object of class Extent
#' @param year Numeric vector of same lengthh as \code{surface}. Which year to use for time varying rasters. NA for the latest year or for static rasters.
#'
#'
#' Unlike \code{\link{malariaAtlas::getRaster}} you cannot get multiple years at once. To get multiple years of the same surface you would have to do
#'   \code{surface = c('Plasmodium falciparum PR2-10', 'Plasmodium falciparum PR2-10'), year = c(2010, 2011)}.
#'
#' @seealso \code{\link{malariaAtlas::getraster}} \code{\link{malariaAtlas::listRaster}}
#' @family covariate
#'
#' @author Tim Lucas, \email{timcdlucas@@gmail.com}
#'


malariaAtlas_covariates <- function(surface = 'Plasmodium falciparum PR2-10', 
                                    extent = c(32, 48, 3, 14),
                                    year = rep(NA, length(surface))) {
  
  zoon::GetPackage('malariaAtlas')

  stopifnot(length(surface) == length(year))
  stopifnot(is.numeric(year) | is.na(year))
  
  
  extent <- matrix(extent, nrow = 2, byrow = TRUE)

  layer <- malariaAtlas::getRaster(surface = surface, 
                                   extent = extent, 
                                   year = year)

  if(!is.list(layer)) layer <- list(layer)
  
  layers <- zoon::CombineRasters(layer)

  cov_stack <- stack(layers)

  return (cov_stack)
}
