#'Output module: InteractiveMap
#'
#'Plot an interactive map of the predicted distribution.
#'
#'@param model  
#'
#'@param ras  

#'@name InteractiveMap
InteractiveMap <-
function (model, ras) {
 
  zoon::GetPackage('latticeExtra')
  zoon::GetPackage('leaflet')
  zoon::GetPackage('satellite')
  zoon::GetPackage('environmentalinformatics-marburg/Rsenal')
 
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

  mapView(pred_ras) 

  
  
  return(NULL)
  
}
