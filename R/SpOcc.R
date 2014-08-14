# A zoon module
# @occurrence
SpOcc <-
function(species, extent, databases = 'gbif'){
  require(spocc)
  raw <- occ2df(occ(query = species, geometry = extent, from = databases, limit=10e5))
  occurrence <- raw[,c('longitude', 'latitude')]
  occurrence$value <- 1
  occurrence$type <- 'presence'
  return(occurrence) 
}
