
#'Output module. A function for outputing the raster of the predictions from an analysis
#'
#'@param model A model object, the output from a model module
#'@param ras A Raster* object, the output from a covariate module
#'
#'@return A Raster object giving the probabilistic model predictions for each
#'      cell of ras
#'
#'@name sameTimePlaceMap


sameTimePlaceMap <- function (model, ras) {
  
  vals <- data.frame(getValues(ras))
  colnames(vals) <- names(ras)
  
  pred <- predict(model,
                  newdata = vals,
                  type = 'response')
  
  pred_ras <- ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)
  
  return(pred_ras)
  
}

