#' @name LonLatToCovariates
#'
#' @title Create new covariates using latitude and longitude values
#'
#' @description Create two new covariates, Longitudecov and Latitudecov that are simply the latitude and longitude for each cell. These covariates can be used in otherwise non-spatial models. Some specifically spatial models such as INLA require these covariates and will use the specifically named covariates once implemented.
#'
#' @details 
#'
#' @param lat Logical, return the latitude column?
#' @param lon Logical, return the longitude column?
#' @param .data \strong{Internal parameter, do not use in the workflow function.} \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. .data is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @family process
#'
#' @author Tim Lucas, \email{timcdlucas@@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, presence/background, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-04-25
LonLatToCovariates <- function (.data, lat = TRUE, lon = TRUE) {
    
    df <- .data$df
    ras <- .data$ras
    
    if(!lat & !lon) stop('You must chose to return at least one of lat and lon.')

    # create copy of rasters
    lonlat <- raster::stack(ras[[1]], ras[[1]])
    
    # Add new values to layers
    values(lonlat) <- sp::coordinates(lonlat)
    
    # rename layerse
    names(lonlat) <- c('Longitudecov', 'Latitudecov')
    
    # Stack
    ras <- raster::stack(ras, lonlat)
    
    
    vals <- raster::extract(lonlat, df[, c("longitude", "latitude")])
    df <- zoon:::cbindZoon(df, vals)
    attr(df, 'covCols') <- c(attr(df, 'covCols'), c('Longitudecov', 'Latitudecov'))
    
    return(list(df = df, ras = ras))
}
