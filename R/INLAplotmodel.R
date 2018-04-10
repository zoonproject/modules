#' @title Output module: INLAplotmodel
#'
#' @description Plot posterior distributions for covariates and hyperparameters from an INLA model.
#'
#' 
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @author Tim Lucas, \email{timcdlucas@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2017-04-27
#' @section Data type: presence/absence, presence/background
#'
#' @name INLAplotmodel
#' @family output
INLAplotmodel <-
function(.model, .ras){

  if(!inherits(.model$model$model$model, 'inla')){
    message('INLAplotmodel only works for INLA models. Returning NULL')
    return(NULL)
  }
  
  p <- autoplot(.model$model$model$model, which = c(1, 2), CI = TRUE)
  print(p)
  
  return (p)

}
