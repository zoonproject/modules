library(zoon)



# Just a function to read local data in. 

LocalData <- function(filename, occurrenceType){
  type <- tolower(occurrenceType)
  #assert_that(type %in% c('presence', 'presence/absence', 'abundance'))
  occurrence <- read.csv(filename, header=TRUE)
  
  if(type == 'presence/absence') {
    occurrence$type <- ifelse(occurrence[,3]==1, 'presence', 'absence')
  } else {
    occurrence$type <- type
  }

  colnames(occurrence) <- c('lon', 'lat', 'value', 'type')

  return(occurrence)
}

BuildModule(LocalData, 'occurrence', dir='~/Dropbox/zoon/modules/R')





NoProcess <- function(occurrence, ras){

  
  noccurrence <- nrow(occurrence)
  
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('lon', 'lat')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   occ_covs)
  
  names(df)[5:ncol(df)] <- names(ras)
  
  return(df)
  
}

BuildModule(NoProcess, 'process', dir='~/Dropbox/zoon/modules/R')




# spocc module

SpOcc <- function(species, extent, databases = 'gbif'){
  require(spocc)
  raw <- occ2df(occ(query = species, geometry = extent, from = databases, limit=10e5))
  occurrence <- raw[,c('longitude', 'latitude')]
  occurrence$value <- 1
  occurrence$type <- 'presence'
  return(occurrence) 
}

BuildModule(SpOcc, 'occurrence', dir = '~/Dropbox/zoon/modules/R')




