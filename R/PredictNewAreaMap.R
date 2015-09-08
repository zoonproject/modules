#'Output module: PredictNewAreaMap
#'
#'Output module. Predict a model across a new area.
#'
#'@param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#'@param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#'@param extent Length 4 character vector giving the new extent to be predicted over. Must be within the covariate data collected in the covariate module.
#'
#'@name PredictNewAreaMap


PredictNewAreaMap <- function (.model, .ras, extent) {
  

  ras <- crop(.ras, extent)
  vals <- data.frame(getValues(ras))
  colnames(vals) <- names(ras)
  
  pred <- predict(.model$model,
                  newdata = vals,
                  type = 'response')
  
  # if pred is a matrix/dataframe, take only the first column
  # and coerce to a vector
  if(NCOL(pred) > 1) {
    pred <- pred[, 1]
  }
  pred <- as.vector(pred)
 
  pred_ras <- ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)
  
  return(pred_ras)
  
}

