#'Output module: PrintMap
#'
#'Plot a map of predicted surface.
#'
#'@param model  
#'
#'@param ras  

#'@name PrintMap
PrintMap <-
function (model, ras) {
  
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

  plot(pred_ras) 

  
  
  return(NULL)
  
}
