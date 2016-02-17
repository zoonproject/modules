#' @title Output module: PredictNewAreaMap
#'
#' @description Output module. Predict a model across a new area.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param extent Numeric vector of length 4 giving (in this order) the min x, max x, min y, max y, the the new area to be mapped. Must be within the covariate data collected in the covariate module.
#'
#' @param plot Logical, if \code{TRUE} (default) the prediction map is plotted.
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @name PredictNewAreaMap
#' @family output

PredictNewAreaMap <- function (.model, .ras, extent = as.vector(.ras@extent),
                               plot = TRUE) {
  
  ras <- crop(.ras, extent)
  vals <- data.frame(getValues(ras))
  colnames(vals) <- names(ras)
  
  pred <- ZoonPredict(.model$model,
                      newdata = vals)

  pred_ras <- ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)
  
  if(plot) plot(pred_ras)
  
  return(pred_ras)
  
}
