#' @name InteractiveOccurrenceMap
#'
#' @title Interactive Occurrence Map
#'
#' @description This output module creates an interactive occurrence map of the species data.
#'
#' @details The module creates an html widget that displays in  the Viewer panel
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s) to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param maxBytes The maximum number of bytes to allow for the projected image (before base64 encoding); defaults to 4MB.
#'
#' @family output
#'
#' @author David Wilkinson, \email{davidpw@@student.unimelb.edu.au}
#'
#' @section Data type: presence-only, presence/absence
#'
#' @section Version: 0.3
#'
#' @section Date submitted:  2018-04-10
InteractiveOccurrenceMap <- function(.model, .ras, maxBytes = 4.2e6){
  
  ## required packages
  
  zoon::GetPackage("mapr")
  zoon::GetPackage("htmlwidgets")
  zoon::GetPackage("leaflet")
  zoon::GetPackage("viridis")
  zoon::GetPackage("RColorBrewer")
  
  ## Make temp version of data. To make it easier to update when/if zoon internal structure changes
  
  .tmp <- .model$data
  
  ## Point colour palette
  
  fill_pal <- colorFactor(c("#FFFFFF", "#808080", "#080808"),
                          domain = c('presence', 'background', 'absence'),
                          ordered = TRUE)
  
  border_pal <- colorFactor(c("#F0F0F0", "#808080", "#080808"),
                            domain = c('presence', 'background', 'absence'),
                            ordered = TRUE)
  
  ## Set up map + base layers
  
  map <- leaflet(.tmp) %>%
    # Base layers
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(provider = 'Esri.WorldImagery', group = 'Esri.WorldImagery')
  
  ## Set up group layers
  
  overlay_groups <- c() # layers to "print"
  hide_groups <- c() # layers to not have ticked by default
  
  ### Data points
  
  for (type in c('absence', 'background', 'presence')) {
    
    if (any(.tmp$type == type)) {
      
      idx <- .tmp$type == type  # logical test for subsetting
      
      group_name <- paste(type, 'data')  # give layer name
      overlay_groups <- c(overlay_groups, group_name)  # add to overlay group
      
      map <- leaflet::addCircleMarkers(map = map,  # add circles to map
                                       lng = .tmp$longitude[idx],  # longitude values
                                       lat = .tmp$latitude[idx],  # latitude values
                                       color = grey(0.4),  # border colour
                                       fillColor = fill_pal(type),  # fill colour
                                       weight = 1,
                                       opacity = 1,
                                       fillOpacity = 1,
                                       radius = 3.5,
                                       group = group_name,
                                       popup = paste('<b>',
                                                     paste(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), sep=""),
                                                     '</b>',
                                                     '<br>Longitude:', .tmp$longitude[idx],
                                                     '<br>Latitude:', .tmp$latitude[idx],
                                                     '<br>Fold:', .tmp$fold[idx],
                                                     '<br>Value:', .tmp$value[idx]))
    }
  }
  
  ## add points legend
  
  map <- leaflet::addLegend(map = map,
                            pal = fill_pal,
                            opacity = 0.8,
                            values = factor(c('presence', 'absence', 'background'),
                                            levels = c('presence', 'absence', 'background'),
                                            ordered = TRUE),
                            title = 'Data points')
  
  ### Covariate Rasters
  
  for(i in seq(nlayers(.ras))){
    
    ## raster layer name
    
    layer_name_tmp <- sprintf('names(.ras)[%s]', i)  # get raster layer name
    layer_name <- eval(parse(text = layer_name_tmp))
    overlay_groups <- c(overlay_groups, layer_name)  # add layer to overlay group
    
    ## get raster colour palette
    
    ras_pal <- colorNumeric(viridis(10),    # set colour palette by viridis
                            domain = seq(minValue(.ras[[i]]), maxValue(.ras[[i]]), length.out = 10),
                            na.color = 'transparent')
    
    ## reproject .ras[[i]], suppressing warnings
    
    suppressWarnings(ext <- raster::projectExtent(.ras[[i]], crs = sp::CRS('+init=epsg:3857')))
    suppressWarnings(tmp_ras <- raster::projectRaster(.ras[[i]], ext))
    
    ## add layer
    
    map <- addRasterImage(map = map,  # add layer to map
                          x = tmp_ras,
                          colors = ras_pal,
                          project = FALSE,
                          opacity = 0.8,
                          group = layer_name,
                          maxBytes = maxBytes)
    
    ## add all bar first raster layer to hide group so default display is one raster
    
    if(i != 1){
      hide_groups <- c(hide_groups, layer_name)
    }
  }
  
  ## add toggle for the layers
  
  map <- addLayersControl(map = map,   # add toggle switch for layers
                          position = "topleft",
                          baseGroups = c('OpenStreetMap',
                                         'Esri.WorldImagery'),
                          overlayGroups = overlay_groups,
                          options = layersControlOptions(collapsed = FALSE))
  
  map <- hideGroup(map = map,  # untick all raster layers except first
                   hide_groups)
  
  ## output map as widget
  
  htmlwidgets:::print.htmlwidget(x = map)
  
  return (invisible(map))
}
