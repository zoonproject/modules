#'Occurrence module: AnophelesPlumbeus
#'
#'Occurrence module to grab *Anopheles plumbeus* (a mosquito) presence
#'       (presence-only) data from GBIF, in the area bounded by extent.
#'
#'@param extent A numeric vector of length 4 giving the coordinates of the 
#'       rectangular region within which to carry out the analysis, in the 
#'       order: xmin, xmax, ymin, ymax. By default the extent of the UK.
#'
#'
#'@name AnophelesPlumbeus



AnophelesPlumbeus <- function(extent = c(-10, 10, 45, 65)){

  zoon:::GetPackage(dismo)
   
  raw <- gbif(genus = 'Anopheles',
              species = 'plumbeus',
              ext = extent)
  
  occurrence <- raw[, c('lon', 'lat')]
  colnames(occurrence) <-  c('longitude', 'latitude')
  
  occurrence$value <- 1
  
  occurrence$type <- 'presence'

  occurrence$fold <- 1

  return(occurrence)
}

