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

  if(!require(spocc)){
    install.packages('spocc')
    library(spocc)
  }
  raw <- occ2df(occ(query = species, geometry = extent, from = databases, limit=10e5))
  occurrence <- raw[,c('longitude', 'latitude')]
  occurrence$value <- 1
  occurrence$type <- 'presence'
  occurrence$fold <- 1
  return(occurrence) 
}
