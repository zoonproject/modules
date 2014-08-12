

AnophelesPlumbeus <- function(extent){
  require (dismo)
  
  raw <- gbif(genus = 'Anopheles',
              species = 'plumbeus',
              ext = extent)
  
  occurrence <- raw[, c('lon', 'lat')]
  
  occurrence$value <- 1
  
  occurrence$type <- 'presence'
  
  return(occurrence)
}


# Just a function to read local data in. 

LocalData <- function(filename, occurrenceType){
  type <- tolower(occurrenceType)
  assert_that(type %in% c('presence', 'presence/absence', 'abundance'))
  occurrence <- read.csv(filename, header=TRUE)
  
  if(type == 'presence/absence') {
    occurrence$type <- ifelse(occurrence[,3]==1, 'presence', 'absence')
  } else {
    occurrence$type <- type
  }

  colnames(occurrence) <- c('lon', 'lat', 'value', 'type')

  return(occurrence)
}



NoProcess <- function(occurrence, ras){

  require (dismo)
  
  noccurrence <- nrow(occurrence)
  
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('lon', 'lat')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   covs)
  
  names(df)[5:ncol(df)] <- names(ras)
  
  return(df)
  
}





