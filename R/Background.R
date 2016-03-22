#' @title Process module: Background
#'   
#' @description Process module to generate random (optionally biased) background
#'   records in cells of the covariate raster and return these along with the
#'   presence only data.
#'   
#' @param .data \strong{Internal parameter, do not use in the workflow
#'   function}. \code{.data} is a list of a data frame and a raster object
#'   returned from occurrence modules and covariate modules respectively.
#'   \code{.data} is passed automatically in workflow from the occurrence and
#'   covariate modules to the process module(s) and should not be passed by the
#'   user.
#'   
#' @param n the number of background points to sample
#'   
#' @param bias optional \code{RasterLayer} with cells giving the relative
#'   probability of a background record being sampled there. Alternatively,
#'   a length one numeric giving a radius (in KM) around presence points to take 
#'   background points from. if \code{bias = NULL} (the default) then no 
#'   biasing is applied, and all non-missing cells are equally likely to be selected.
#'   
#' @param seed Numeric used with \code{\link[base]{set.seed}}
#' 
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13 
#' @section Data type: presence-only
#'   
#' @name Background
#' @family process
Background <- function (.data, n = 100, bias = NULL, seed = NULL) {
  
  zoon:::GetPackage(dismo)
  
  occurrence <- .data$df
  
  if (!all(occurrence$type == 'presence')) {
    stop ('"Background" module only works for presence-only data')
  }
  
  # if no bias grid is provided
  if (is.null(bias)) {
    ras <- .data$ras
    prob <- FALSE
  } else if(inherits(bias, 'RasterLayer')) {
    ras <- bias
    prob <- TRUE
  } else {
    # If it's not a raster, it should be a length one numeric
    if (!inherits(bias, 'numeric') | !length(bias) == 1) {
      stop ('bias must be either NULL or a RasterLayer object from the raster package')
    }

    ras <- .data$ras
    xypoints <- SpatialPoints(.data$df[, c('longitude', 'latitude')], CRS('+proj=longlat +ellps=WGS84'))

    transpoints <- sp::spTransform(xypoints, ras@crs)
    r2 <- rasterize(transpoints, ras[[1]], field = 1)
    ras <- raster::buffer(r2, width = bias * 1000)

    prob <- TRUE
    # Set cells whose centre point is further than `bias` from an occurrence point to 0.
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
  points <- n
  
  # check the number
  if (sum(!is.na(getValues(ras))) < n) {
    # find the number of non-na cells in ras
    points <- length(na.omit(getValues(ras)))
    warning(sprintf('There are fewer than %i cells in the covariate raster.\nUsing all available cells (%i) instead',
                    n,
                    points))
  }

  # generate pseudo-absence data on the grid, possibly biased,
  # suppressing warnings when the number is restricted
  suppressWarnings(pa <- dismo::randomPoints(ras,
                                             n,
                                             prob = prob))
  
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

