#'Output module: InteractiveMap
#'
#'Plot an interactive map of the predicted distribution.
#'
#'@param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.#'
#'
#'@param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#'@name InteractiveMap
InteractiveMap <-
function (.model, .ras) {
 
  # unload the leafletR package if it's installed
  # as it conflicts with leaflet
  
  zoon:::GetPackage('environmentalinformatics-marburg/Rsenal', github = TRUE)
 
  vals <- data.frame(getValues(.ras))
  colnames(vals) <- names(.ras)
  
  pred <- predict(.model$model,
                  newdata = vals,
                  type = 'response')
  
  # if pred is a matrix/dataframe, take only the first column
  if(!is.null(dim(pred))) {
    pred <- pred[, 1]
  }

  pred_ras <- .ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)
  names(pred_ras) <- 'prediction'

  htmlwidgets:::print.htmlwidget(mapView(pred_ras))
  
  return (NULL)
  
}
