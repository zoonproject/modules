#' @name InteractiveOccurrenceMap
#'
#' @title Interactive Occurrence Map
#'
#' @description This output module creates an interactive occurrence map of the species data.
#'
#' @details The module creates an html widget that displays in  the Viewer panel
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author David Wilkinson, \email{davidpw@@student.unimelb.edu.au}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-04-28
InteractiveOccurrenceMap <- function(.model, .ras){
  
  # required packages
  
  zoon::GetPackage("mapr")
  zoon::GetPackage("htmlwidgets")
  
  # build data.frame of required format
  
  tmp <- data.frame(name = .model$data$type,
                    longitude = .model$data$longitude,
                    latitude = .model$data$latitude)
  
  # set colour scheme for points
  
  if(length(unique(.model$data$type)) == 1){
    col <- c("red")
  } else {
    if(length(unique(.model$data$type)) == 2){
      col <- c("red", "blue")
    } else {
      if(length(unique(.model$data$type)) ==3){
        col <- c("red", "blue", "green")
      }
    }
    }
  
  # create map
  
  tmp2 <- map_leaflet(tmp, size = 2, color = col)
  
  # output map as widget
  
  htmlwidgets:::print.htmlwidget(x = tmp2)
  
  return (invisible(tmp2))
}
