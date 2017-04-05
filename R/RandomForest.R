#' @title Model module: RandomForest
#'
#' @description Model module to fit a simple RandomForest classification model
#'
#' @param .df \strong{Internal parameter, do not use in the workflow function}. \code{.df} is data frame that combines the occurrence data and covariate data. \code{.df} is passed automatically in workflow from the process module(s) to the model module(s) and should not be passed by the user.
#'
#' @return A model object with a valid predict method
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @section Data type: presence/absence, abundance 
#' @name RandomForest
#' @family model


RandomForest <- function (.df) {
  
  zoon:::GetPackage('randomForest')
  
  #if (!all(.df$type %in% c('presence', 'absence', 'background'))) {
  #  stop ('only for presence/absence or presence/background data')
  #}
  
  covs <- as.data.frame(.df[, attr(.df, 'covCols')])
  names(covs) <- attr(.df, 'covCols')

  m <- randomForest(factor(.df$value) ~ .,
                    data = covs,
                    weights = rep(1, nrow(covs)),
                    size = 1)

  ZoonModel(model = m,
            code = {
              randomForest:::predict.randomForest(model,
                                newdata,
                                type = 'prob')[, 2]
            },
            packages = 'randomForest')
}

