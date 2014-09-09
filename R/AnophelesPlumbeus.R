
#'Occurrence module to grab *Anopheles plumbeus* (a mosquito) occurrence (i.e.
#'       presence-only) data from GBIF, in the area bounded by extent.
#'       Perhaps this should have temporal interval too for future-proofing?
#'
#'@param extent A numeric vetor of length 4 giving the coordinates of the 
#'       rectangular region within which to carry out the analysis, in the 
#'       order: xmin, xmax, ymin, ymax.
#'
#'@return a dataframe with four columns:
#'       value - a numeric value which may give 1 for presences, 0 for absences 
#'       or a positive integer for count data
#'       type - a character value saying what is in the value column
#'       lon - the longitude of the record
#'       lat - the latitutude of the record
#'
#'@name AnophelesPlumbeus



AnophelesPlumbeus <- function(extent){
  require (dismo)

  
  extent <- uk.extent <- c(xmin = -10,
              xmax = 10,
              ymin = 45,
              ymax = 65)
  
  raw <- gbif(genus = 'Anopheles',
              species = 'plumbeus',
              ext = extent)
  
  occurrence <- raw[, c('lon', 'lat')]
  
  occurrence$value <- 1
  
  occurrence$type <- 'presence'
  
  return(occurrence)
}

