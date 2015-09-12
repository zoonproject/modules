#'Output module: PrintMap
#'
#'Plot a map of predicted surface.
#'
#'@param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#'@param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user. 
#'@name PrintMap
#'@family output
PrintMap <-
function (.model, .ras) {
  
  vals <- data.frame(getValues(.ras))
  colnames(vals) <- names(.ras)
  
  pred <- ZoonPredict(.model$model,
                      newdata = vals)

  pred_ras <- .ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)

  plot(pred_ras) 
  
  return(NULL)
  
}
