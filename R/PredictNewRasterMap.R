#' @name PredictNewRasterMap
#'
#' @title Output module: PredictNewRasterMap
#'
#' @description Output module. Predict a model across a new area or time period.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param newraster Raster* object to make predictions onto.
#'
#' @param extent Numeric vector of length 4 giving (in this order) the min x, max x, min y, max y, the the new area to be mapped. Must be within the covariate data collected in the covariate module.
#'
#' @param plot Logical, if \code{TRUE} (default) the prediction map is plotted.
#'
#' @param ... further arguments to be passed to \code{\link[raster]{plot}}.
#'
#' @family output
#'
#' @author Zoon developers (modified by F. Rodriguez-Sanchez), \email{zoonproject@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 1.0.1
#'
#' @section Date submitted:  2016-06-15
PredictNewRasterMap <- function (.model, .ras, newraster = .ras, extent = as.vector(.ras@extent),
    plot = TRUE, ...)
{
    ras <- crop(newraster, extent)
    vals <- data.frame(getValues(newraster))
    colnames(vals) <- names(newraster)
    pred <- ZoonPredict(.model$model, newdata = vals)
    pred_ras <- newraster[[1]]
    names(pred_ras) <- 'prediction'
    pred_ras <- setValues(pred_ras, pred)
    if (plot)
        plot(pred_ras, ...)
    return(pred_ras)
}
