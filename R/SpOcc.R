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
#'
#'@param type A string giving the type of occurrence data to treat these records as.
#'  Must be one of \code{'presence'} (the default and most common use case),
#'  \code{'absence'} or \code{'background'} (which may be used to implement
#'  a 'target background group' approach for overcoming observation bias).
#'
#'@param limit The maximum number of occurrence records to return.
#'
#'@seealso \code{\link{spocc::occ}}
#'
#'
#'@name SpOcc
#'@family occurrence

SpOcc <-
function(species, extent, databases = 'gbif', type = 'presence', limit = 10000){

  if (!(type %in% c('presence', 'absence', 'background'))) {
    stop(paste('occurrence type', type, 'cannot be used, type must be one of:',
               "'presence', 'absence' or 'background'"))
  }

  zoon::GetPackage(MASS)
  zoon::GetPackage(spocc)
  
  # reorder the extent to match the xmin, ymin, xmax, yamx order that spocc expects now
  reorderExtent <- extent[c(1, 3, 2, 4)]

  # Get the data
  raw <- occ2df(occ(query = species, geometry = reorderExtent, from = databases, limit = limit))
  
  # If no data is returned issue an error
  if(nrow(raw) == 0){
    stop(paste('No data was found for', species, 'in',
               paste(databases, collapse = ','),
               'for the given extent'))
  }
  occurrence <- data.frame(longitude = as.numeric(raw$longitude),
                           latitude = as.numeric(raw$latitude))
  if (type == 'presence') {
    occurrence$value <- 1 
  } else {
    occurrence$value <- 0 
  }
  occurrence$type <- type
  occurrence$fold <- 1
  return(occurrence) 
}
