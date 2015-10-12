#'Occurrence module: SpOcc
#'
#'Occurrence module to collect occurrence data from a number of data bases.
#'
#'@param species A character string giving the species name.
#'  e.g. 'Anopheles plumbeus'.
#'  
#'@param extent A numeric vector of length 4 giving the coordinates of the 
#'  rectangular region within which to carry out the analysis, in the 
#'  order: xmin, xmax, ymin, ymax.
#'
#'@param databases A character vector giving the databases to use.
#'  Choose from "gbif", "bison", "inat", "ebird", "ecoengine", "antweb".
#'  Defaults to gbif. NB I have had some problems with databases other
#'  than gbif.
#'@seealso \code{\link{spocc::occ}}
#'
#'
#'@name SpOcc


SpOcc <-
function(species, extent, databases = 'gbif'){

  zoon::GetPackage(spocc)
  zoon::GetPackage(MASS)
  
  # reorder the extent to match the xmin, ymin, xmax, yamx order that spocc expects now
  reorderExtent <- extent[c(1, 3, 2, 4)]

  # Get the data
  raw <- occ2df(occ(query = species, geometry = reorderExtent, from = databases, limit=1e5))
  
  # If no data is returned issue an error
  if(nrow(raw) == 0){
    stop(paste('No data was found for', species, 'in',
               paste(databases, collapse = ','),
               'for the given extent'))
  }
  occurrence <- raw[,c('longitude', 'latitude')]
  occurrence$value <- 1
  occurrence$type <- 'presence'
  occurrence$fold <- 1
  return(occurrence) 
}
