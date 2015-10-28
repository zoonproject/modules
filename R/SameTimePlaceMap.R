#' @title Output module: SameTimePlaceMap
#'
#' @description Output module. A function for outputing the raster of the predictions from an analysis
#'
#'@param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#'@param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#'@return A Raster object giving the probabilistic model predictions for each
#'      cell of ras
#'
#'@author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#'@name SameTimePlaceMap
#'@family output

SameTimePlaceMap <- function (.model, .ras) {
  
  vals <- data.frame(getValues(.ras))
  colnames(vals) <- names(.ras)
  
  pred <- ZoonPredict(.model$model,
                      newdata = vals)
 
  pred_ras <- .ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)
  
  return(pred_ras)
  
}

