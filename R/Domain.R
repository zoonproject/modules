#' @title Model module: Domain
#'
#' @description Model module to fit the Domain algorithm.
#' 
#' @param .df \strong{Internal parameter, do not use in the workflow function}. \code{.df} is data frame that combines the occurrence data and covariate data. \code{.df} is passed automatically in workflow from the process module(s) to the model module(s) and should not be passed by the user.
#'
#' @return A model object with a valid predict method
#' 
#' @seealso \code{\link{dismo::domain}}
#'
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2017-04-05
#' @section Data type: presence-only
#' @name Domain
#' @family model


Domain <- function (.df) {
  
  zoon:::GetPackage('dismo')
  
  if (!all(.df$type %in% c('presence'))) {
   stop ('only for presence-only data')
  }
  
  covs <- as.data.frame(.df[, attr(.df, 'covCols')])
  names(covs) <- attr(.df, 'covCols')
  
  m <- domain(covs)
  
  ZoonModel(model = m,
            code = {predict(model, newdata)},
            packages = 'dismo')
  
}

