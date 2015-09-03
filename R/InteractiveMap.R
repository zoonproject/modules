#'Output module: InteractiveMap
#'
#'Plot an zoomable and scrollable map of the predicted distribution
#' and training data.
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
    m <- leaflet::addTiles(map = m, group = 'OpenStreetMap')
    m <- leaflet::addProviderTiles(map = m,
                                   provider = 'Esri.WorldImagery',
                                   group = 'Esri.WorldImagery')
    
    # get legend values
    legend_values <- round(seq(0, 1, length.out = 10), 3)
    
    # get prediction colour palette
    pred_pal <- leaflet::colorNumeric(viridis(10), 
                                 domain = legend_values, 
                                 na.color = 'transparent')
    
    # add the prediction raster
    m <- leaflet::addRasterImage(map = m,
                                 x = pred_ras,
                                 colors = pred_pal,
                                 project = TRUE,
                                 opacity = 0.8,
                                 group = 'Predicted distribution')
    
    # add to the overlay groups list
    overlay_groups <- 'Predicted distribution'
    
    
    # add legend
    m <- leaflet::addLegend(map = m,
                            pal = pred_pal,
                            opacity = 0.8, 
                            values = legend_values, 
                            title = 'Predicted distribution')

    # add training data
    df <- model$data
    
    # color palettes for circles
    fill_pal <- colorFactor(grey(c(1, 0, 0.2)),
                            domain = c('presence',
                                       'absence',
                                       'background'))

    border_pal <- colorFactor(grey(c(0, 1, 1)),
                            domain = c('presence',
                                       'absence',
                                       'background'))
    
    for (type in c('background', 'absence', 'presence')) {
      if (any(df$type == type)) {
        idx <- df$type == type
        group_name <- paste(type, 'data')
        overlay_groups <- c(overlay_groups, group_name)
        m <- leaflet::addCircleMarkers(map = m,
                                 lng = df$longitude[idx],
                                 lat = df$latitude[idx],
                                 color = grey(0.4),
                                 fillColor = fill_pal(type),
                                 weight = 1,
                                 opacity = 1,
                                 fillOpacity = 1,
                                 radius = 5,
                                 group = group_name)
        
      }
    }
        
    # add toggle for the layers
    m <- leaflet::addLayersControl(map = m,
                                   position = "topleft",
                                   baseGroups = c('OpenStreetMap',
                                                  'Esri.WorldImagery'),
                                   overlayGroups = overlay_groups)
    
    htmlwidgets:::print.htmlwidget(x = m)
    
    return (NULL)
    
  }
