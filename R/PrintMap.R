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
  
  pred_ras <- ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)

  plot(ras) 
  
  
  return(NULL)
  
}
