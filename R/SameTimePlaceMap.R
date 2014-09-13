#'Output module: SameTimePlaceMap
#'
#'Output module. A function for outputing the raster of the predictions from an analysis
#'
#'@return A Raster object giving the probabilistic model predictions for each
#'      cell of ras
#'
#'@name SameTimePlaceMap


SameTimePlaceMap <- function (model, ras) {
  
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

