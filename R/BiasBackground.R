#'BiasBackground: Generate background data using a bias grid
#'
#'This process module genereates a sample of background records according to some spatial model of recording bias,indicated by a raster layer.
#'
#'Module type: Process
#'
#'@param .data \strong{Internal parameter, do not use in the workflow function}.
#' \code{.data} is a list of a data frame and a raster object returned from 
#' occurrence modules and covariate modules respectively. \code{.data} is a list
#' of a data frame and a raster object returned from occurrence modules and 
#' covariate modules respectively. \code{.data} is passed automatically in 
#' workflow from the occurrence and covariate modules to the process module(s)
#' and should not be passed by the user.
#'
#'@param bias Raster with cells giving the relative probability of a background record being sampled there.
#'
#'@param n Positive integer giving the number of background points required.
#'
#'@author Nick Golding
#'@name BiasBackground
BiasBackground <- function (.data, bias, n = 100) {
  
  # get the internal data
  occurrence <- .data$df
  ras <- .data$ras
  
  if (!all(occurrence$type == 'presence')) {
    stop ('this function only works for presence-only data')
  }
  
  # generate pseudo-absence data biased according to the grid
  # suppressing warnings when the number is restricted
  suppressWarnings(pa <- dismo::randomPoints(bias,
                                             n,
                                             prob = TRUE))
  
  npres <- nrow(occurrence)
  
  npabs <- nrow(pa)
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('longitude', 'latitude')]))
  
  pa_covs <- as.matrix(extract(ras, pa))
  
  covs <- rbind(occ_covs, pa_covs)
  
  # combine with the occurrence data
  df <- data.frame(value = rep(c(1, 0),
                               c(npres, npabs)),
                   type = rep(c('presence', 'background'),
                              c(npres, npabs)),
                   fold = rep(1, npres + npabs),
                   longitude = c(occurrence$lon, pa[, 1]),
                   latitude = c(occurrence$lat, pa[, 2]),
                   covs)
  
  names(df)[6:ncol(df)] <- names(ras)
  
  # remove missing values
  if(NROW(na.omit(df)) > 0){
    df <- na.omit(df)
  }
  
  return(list(df=df, ras=ras))
  
}
