#' @title Model module: MyMaxLike
#'   
#' @description Model module to fit a simple MyMaxLike model
#'   
#' @details The mymaxlike method aims to infer the parameters of a 
#'   presence-absence logistic regression model using only presence-background 
#'   data, in a manner somewhat similar to MaxEnt. The practical utility of this
#'   model has been questioned (see references), since it makes very strong 
#'   assumptions about the underlying model which are unlikely to be met in 
#'   practice. A more feature-rich version of the model is implemented in the 
#'   \code{\link{mymaxlike::mymaxlike}} package, but that implementation only 
#'   accepts raster/point data as input. This module implements a simple version
#'   of the model using the logistic link, in a format that interacts with the 
#'   rest of the zoon system.
#' 
#' @references J.A. Royle, R.B. Chandler, C. Yackulic, and J.D. Nichols (2012). 
#'   Likelihood analysis of species occurrence probability from presence-only 
#'   data for modelling species distributions. Methods in Ecology and Evolution,
#'   3, 545-554.
#'   
#'   Hastie, T. & Fithian, W. (2013) Inference from presence-only data; the 
#'   ongoing controversy. Ecography, 36, 864-867.
#'   
#'   Phillips, S.J. & Elith, J. (2013) On estimating probability of presence 
#'   from use-availability or presence-background data. Ecology, 94, 1409-1419.
#'   
#'   Merow, C. & Silander, J.A. (2014) A comparison of MyMaxlike and Maxent for
#'   modelling species distributions. Methods in Ecology and Evolution, 5,
#'   215-225.
#'   
#' @param .df \strong{Internal parameter, do not use in the workflow function}. 
#'   \code{.df} is data frame that combines the occurrence data and covariate 
#'   data. \code{.df} is passed automatically in workflow from the process 
#'   module(s) to the model module(s) and should not be passed by the user.
#'   
#'   
#' @seealso \code{\link{mymaxlike::mymaxlike}}
#'   
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @section Data type: presence/background
#'   
#' @name MyMaxLike
#' @family model
MyMaxLike <- function (.df) {

  # define the negative log-likelihood function
  nll <- function (par, x, z) {
    
    # log probability of presence across observed locations
    log_psi_obs <- plogis(x %*% par, log.p = TRUE)
    
    # logged, summed probability of presence across whole dataset
    log_psi_ref <- log(sum(plogis(z %*% par)))
    
    # relative log-likelihood
    ll <- sum(log_psi_obs - log_psi_ref)
    
    # return the negative relative log-likelihood
    return (-ll)
  }
  
  # extract the required elements
  response <- as.numeric(.df$value)
  covs <- as.data.frame(.df[, attr(.df, 'covCols')])

  # add an intercept
  covs <- cbind(intercept = 1,
                covs)

  # get covariate values at presence locations
  x <- as.matrix(covs[response == 1, ])
  
  # get covariate values at reference locations
  z <- as.matrix(covs[response == 0, ])

  # define starting parameters
  start <- rep(0, ncol(covs))
  
  # run the optimisation
  m <- optim(start,
             nll,
             x = x,
             z = z)
  
  # set the parameter names
  names(m$par) <- names(covs)
  
  # define a ZoonModel object with prediction code
  ZoonModel(model = m,
            code = {
              
              # need to handle matching parameters with covariates, and adding an intercept
              # add an intercept to newdata
              newdata <- cbind(intercept = 1,
                               newdata)
              
              # extract parameter estimates
              par <- model$par
              
              # line up the covariates with the parameters
              o <- match (names(par), names(newdata))
              newdata <- newdata[, o]
              
              # get predictions on the logit scale
              mu <- as.numeric(as.matrix(newdata) %*% par)
              
              # convert to probability scale & return
              p <- plogis(mu)
              
              return (p)
              
            },
            packages = 'base')

}
