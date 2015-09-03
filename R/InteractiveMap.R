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
    
    # This function draws inspiration from a previous version of
    # the Rsenal package: https://github.com/environmentalinformatics-marburg/Rsenal
    # and of course relies heavily on the wonderful leaflet package whose
    # functions it relies on
    
    # load required packages
    zoon:::GetPackage('leaflet')
    zoon:::GetPackage('htmlwidgets')
    zoon:::GetPackage('viridis')
    
    # Make the prediction
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
    
    # set up a map with background layers
    m <- leaflet::leaflet()
    m <- leaflet::addTiles(m, group = 'OpenStreetMap')
    m <- leaflet::addProviderTiles(m,
                                   provider = 'Esri.WorldImagery',
                                   group = 'Esri.WorldImagery')
    
    # get legend values
    values <- round(seq(0, 1, length.out = 10), 3)
    
    # get colour palette
    pal <- leaflet::colorNumeric(viridis(10), 
                                 domain = values, 
                                 na.color = 'transparent')
    
    # add the raster
    m <- leaflet::addRasterImage(map = m,
                                 x = pred_ras,
                                 colors = pal,
                                 project = TRUE,
                                 opacity = 0.8,
                                 group = 'Predicted distribution')
    
    # add a legend
    m <- leaflet::addLegend(map = m,
                            pal = pal,
                            opacity = 0.8, 
                            values = values, 
                            title = 'Predicted distribution')
    
    # add toggle for the layers
    m <- leaflet::addLayersControl(map = m,
                                   position = "topleft",
                                   baseGroups = c('OpenStreetMap',
                                                  'Esri.WorldImagery'),
                                   overlayGroups = 'Predicted distribution')
    
    htmlwidgets:::print.htmlwidget(m)
    
    return (NULL)
    
  }
