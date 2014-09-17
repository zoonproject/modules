#'Output module: PredictNewAreaMap
#'
#'Output module. Predict a model across a new area.
#'
#'@param extent Length 4 character vector giving the new extent to be predicted over. Must be within the covariate data collected in the covariate module.
#'
#'@name PredictNewAreaMap


PredictNewAreaMap <- function (model, ras, extent) {
  

  ras <- crop(ras, extent)
  vals <- data.frame(getValues(ras))
  colnames(vals) <- names(ras)
  
  pred <- predict(model$model,
                  newdata = vals,
                  type = 'response')
  
  # if pred is a matrix/dataframe, take only the first column
  if(!is.null(dim(pred))) {
    pred <- pred[, 1]
  }
  
  pred_ras <- ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)
  
  return(pred_ras)
  
}

