#' @name AIC
#'
#' @title Returns the AIC value for LogisticRegession models
#'
#' @description Returns the AIC value for LogisticRegession models
#'
#' @details Returns the AIC value for LogisticRegession models. Cannot work with other model classes. Prints if print is TRUE
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param print If TRUE, will print to the console.
#'
#' @family output
#'
#' @author Liz Martin, \email{lizmartinresearch@@gmail.com}
#'
#' @section Data type: presence/absence, presence/background
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2018-02-28
AIC <- function (.model, .ras, print = TRUE) {
  
  model_obj <- .model$model$model
  classes <- c('glm') # can expand to include other parametric model class
  
  if (!inherits(model_obj, classes)) {
    stop ('AIC only works with model objects of class',
          paste(classes, sep = ', '),
          ', but this model object had class(es): ',
          paste(class(model_obj)[1], sep = ', ')
    )
  }
  if ( class(model_obj)[1] == "glm" ){
    AIC <- model_obj$aic
    
  } else{
    stop("AIC is only available for LogisticRegression")
  }
  if (print == TRUE) print(AIC)
  return(AIC)
}
