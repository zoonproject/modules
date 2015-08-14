#'Model module: QuickGRaF
#'
#'Model module to fit a quick GRaF model, without parameter optimisation.
#'@param l lengthscale parameter, a positive number controlling the complexity
#' of the fitted model with respect to each covariate.
#' Can either be a single number (in which case it is used for all covariates),
#' a vector of numbers of the same length as the number of covariates,
#' or \code{NULL} in which case GRaF will calculate a fixed approximation to
#' a sensible lengthscale.
#'@name QuickGRaF


QuickGRaF <-
function (df, l = NULL) {
  
  zoon:::GetPackage('GRaF')
  
  if (!all(df$type %in% c('presence', 'absence', 'background'))) {
    stop ('only for presence/absence or presence/background data')
  }
  
  # get the covariates
  covs <- as.data.frame(df[, 6:ncol(df)])
  names(covs) <- names(df)[6:ncol(df)]
  
  # set up l
  if (!is.null(l)) {
    
    # duplicate if necessary
    if (length(l) == 1) {
      l <- rep(l, ncol(covs))
    }

    # check l has the correct length
    if (length(l) != ncol(covs)) {
      stop (sprintf('l has %i elements, but there are %i covariates', length(l), ncol(covs)))
    }
  
    # check l is of the correct value
    if (any(l <= 0)) {
      stop(sprintf('l must be positive, but the values provided were: %s',
                   paste(format(l, digits = 3), collapse = ', ')))
    }
  }
  # fit the model
  m <- graf(df$value,
            covs,
            l = l)
  
  return (m)
}
