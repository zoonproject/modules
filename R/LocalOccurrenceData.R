#'Occurrence module: LocalOccurrenceData
#'
#'Occurrence module to format local occurrence data to be used with zoon.
#'
#'@param filename The path to the file. Currently assumes a .csv file. 
#'
#'@param occurrenceType What type data is it? One of 'presence', 'presence/absence', 'abundance' 
#'
#'@param externalValidation Logical indicating whether this data is external validation data. Cross validation is handled elsewhere and this argument should be left as FALSE for data that will be used in cross validation. 
#'
#'@name LocalOccurrenceData
LocalOccurrenceData <-
function(filename, occurrenceType, externalValidation=FALSE){

  assert_that(is.logical(externalValidation))
  type <- tolower(occurrenceType)
  #assert_that(type %in% c('presence', 'presence/absence', 'abundance'))
  occurrence <- read.csv(filename, header=TRUE)
  
  if(type == 'presence/absence') {
    occurrence$type <- ifelse(occurrence[,3]==1, 'presence', 'absence')
  } else {
    occurrence$type <- type
  }

  colnames(occurrence) <- c('lon', 'lat', 'value', 'type')

  if (externalValidation){
    occurrence$fold <- 0
  } else {
    occurrence$fold <- 1
  }                     

  return(occurrence)
}
