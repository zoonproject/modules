#'Occurrence module: LocalData
#'
#'Occurrence module to format local occurrence data to be used with zoon. 
#'
#'@param filename The path to the file. Currently assumed a .csv file.
#'@param occurrenceType What type data is it? 
#'  One of 'presence', 'presence/absence', 'abundance')
#'
#'@seealso \code{\link{read.csv}}
#'@name LocalData
LocalData <-
function(filename, occurrenceType){
  type <- tolower(occurrenceType)
  #assert_that(type %in% c('presence', 'presence/absence', 'abundance'))
  occurrence <- read.csv(filename, header=TRUE)
  
  if(type == 'presence/absence') {
    occurrence$type <- ifelse(occurrence[,3]==1, 'presence', 'absence')
  } else {
    occurrence$type <- type
  }

  colnames(occurrence) <- c('lon', 'lat', 'value', 'type')

  occurrence$fold <- 1

  return(occurrence)
}


