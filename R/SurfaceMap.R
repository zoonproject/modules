#' @title Output module: SurfaceMap
#'
#' @description Make a png of a map of predicted surface.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param dir Directory where plots are saved. If NULL (default) then plots are not saved. 
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @name SurfaceMap
#' @family output
SurfaceMap <-
function (.model, .ras, dir=NULL) {
  
  zoon::GetPackage('raster')
  
  vals <- data.frame(getValues(.ras))
  colnames(vals) <- names(.ras)
  
  pred <- ZoonPredict(.model$model,
                      newdata = vals)

  pred_ras <- .ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)

  if(!is.null(dir)){
    png(paste0(dir,"/ZoonMap", 
      format(Sys.time(), "%Y_%m_%d-%H%M"), ".png"))
    plot(pred_ras) 
    dev.off()
  } else {
    plot(pred_ras)
  }
  
  return(NULL)
  
}
