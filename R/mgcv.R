#' @title Model module:mgcv
#'
#' @description  Model module to fit a generalized additive model using generalized
#' crossvalidation via the mgcv R package.
#' 
#'@param .df \strong{Internal parameter, do not use in the workflow function}.
#' \code{.df} is data frame that combines the occurrence
#'
#'@param k the dimension of the basis used to represent the smooth term.
#' The default depends on the number of variables that the smooth is a
#'  function of. k should not be less than the dimension of the null space
#'   of the penalty for the term (see \code{\link{mgcv::null.space.dimension}}),
#'   but will be reset if it is. See \code{\link{mgcv::choose.k}} for further
#'   information
#'
#'@param bs a two letter character string indicating the (penalized)
#' smoothing basis to use. (eg "tp" for thin plate regression spline,
#'  "cr" for cubic regression spline). See \code{\link{mgcv::smooth.terms}}
#'  for an overview of what is available.
#'
#'@author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#'@name mgcv
#'@family model
mgcv <-
  function (.df,
            k = -1,
            bs = 'tp') {
    
    zoon:::GetPackage('mgcv')
    
    if (!all(.df$type %in% c('presence', 'absence', 'background'))) {
      stop ('only for presence/absence or presence/background data')
    }
    
    # get the covariates
    covs <- as.data.frame(.df[, 6:ncol(.df)])
    names(covs) <- names(.df)[6:ncol(.df)]
    
    # build the formula
    param_string <- sprintf(', k = %s, bs = "%s")',
                               k, bs)
    f_start <- 'value ~ '
    f_covs <- paste0('s(',
                       paste0(names(covs),
                              collapse = paste0(param_string, ' + s(')),
                       param_string)
    f <- formula(paste0(f_start, f_covs))
    
    # assign the response to the environment
    value <- .df$value
    
    # fit the model with the maximum number of trees
    m <- mgcv::gam(f,
                  data = covs,
                  family = binomial)

    # create a ZoonModel object and return it
    ZoonModel(model = m,
              code = {
                p <- mgcv::predict.gam(model,
                                  newdata,
                                  type = 'response')
                as.vector(p)
              },
              packages = 'mgcv')
  
}
