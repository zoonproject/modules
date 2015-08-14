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
 
  # unload the leafletR package if it's installed
  # as it conflicts with leaflet
  unload('spocc')
  unload('ecoengine')
  unload('AntWeb')
  unload('leafletR')
  
  zoon:::GetPackage('leaflet')
  zoon:::GetPackage('latticeExtra')
  zoon:::GetPackage('satellite')
  zoon:::GetPackage('environmentalinformatics-marburg/Rsenal', github = TRUE)
 
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
  names(pred_ras) <- 'prediction'

  print(mapView(pred_ras))

  
  
  return(NULL)
  
}

unload <- function(pkg) {
 
  # vectorise
  if (length(pkg) > 1) {
    lapply(pkg, unload)
  } else {
    # remove attached packages
    if(pkg %in% names(sessionInfo()$otherPkgs)) {
      pkg <- paste0('package:',pkg)
      detach(pkg,
             character.only = TRUE,
             force = TRUE,
             
             unload = TRUE)
    }
    # and loaded namespaces
    if (pkg %in% loadedNamespaces()) {
      unloadNamespace(pkg)
    }
  }
}
