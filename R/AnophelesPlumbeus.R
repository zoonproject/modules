#' @title Occurrence module: AnophelesPlumbeus
#'
#' @description Occurrence module to grab *Anopheles plumbeus* (a mosquito) presence
#'       (presence-only) data from GBIF, in the area bounded by extent.
#'
#' @param extent A numeric vector of length 4 giving the coordinates of the 
#'       rectangular region within which to carry out the analysis, in the 
#'       order: xmin, xmax, ymin, ymax. By default the extent of the UK.
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @section Data type: presence-only
#'
#' @name AnophelesPlumbeus
#' @family occurrence


AnophelesPlumbeus <- function(extent = c(-10, 10, 45, 65)){

  zoon:::GetPackage('dismo')
   
  # create extent object
  extentObj <- extent(matrix(data = extent, nrow = 2, byrow = TRUE))
  
  raw <- gbif(genus = 'Anopheles',
              species = 'plumbeus',
              ext = extentObj)
  
  occurrence <- raw[, c('lon', 'lat')]
  colnames(occurrence) <-  c('longitude', 'latitude')
  
  occurrence$value <- 1
  
  occurrence$type <- 'presence'

  occurrence$fold <- 1

  return(occurrence)
}

