#' @title Process module: OneThousandBackground
#'
#' @description Process module to generate up to 1000 background records at random in
#'      cells of the covariate raster and return these along with the occurrence data.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#' @param seed Numeric used with \code{\link[base]{set.seed}}
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @section Data type: presence-only
#'
#' @name OneThousandBackground
#' @family process

OneThousandBackground <- function (.data, seed = NULL) {
  

  occurrence <- .data$df
  ras <- .data$ras
 
  if (!all(occurrence$type == 'presence')) {
    stop ('this function only works for presence-only data')
  }
  
  # set seed if specified
  if(!is.null(seed)){
    if(inherits(x = seed, what = c('numeric', 'integer'))){
      set.seed(seed)
    } else {
      stop("'seed' must be numeric or NULL")
    }
  }
  
  # generate pseudo-absence data
  # suppressing warnings when the number is restricted
  suppressWarnings(pa <- randomPoints(ras, 1000))
  
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
