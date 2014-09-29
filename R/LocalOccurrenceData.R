#'Occurrence module: LocalOccurrenceData
#'
#'Occurrence module to format local occurrence data to be used with zoon. 
#'  Must be a .csv file with three columns longitude, latitude and value 
#'  in that order.
#'
#'@param filename The path to the file. Should have columns named long, lat 
#'  and value.
#'
#'@param occurrenceType What type data is it? One of 'presence', 
#' 'presence/absence', 'abundance', 'probability' 
#'
#'@param externalValidation Logical indicating whether this data is external
#'  validation data. Cross validation is handled elsewhere and this argument 
#'  should be left as FALSE for data that will be used in cross validation. 
#'
#'@name LocalOccurrenceData
LocalOccurrenceData <-
function(filename, occurrenceType, externalValidation = FALSE){

  
  assert_that(is.logical(externalValidation))
  type <- tolower(occurrenceType)

  # Fuzzy match to find presnce/absence variations.
  # But also be sure that 'presence' doesn't fuzzy match as presence/absence
  if(agrepl('presence/absence', type,
                  max.distance = list(insertions = 4)) & 
      !type == 'presence' &
      !type == 'absence'){
    
    type <- 'presence/absence'
  }

  if(!type %in% c('presence', 'presence/absence', 'abundance', 'probability')){
    stop("occurrenceType must but one of 'presence', 'presence/absence', 'abundance' or 'probability'.")
  }

  
  # Read in the data  
  # Try and do a vaguley thorough job of checking format.
  # Cheers @klmr
  # https://gist.github.com/klmr/4f093bb49dcf1aa72b7a

  extension <- gsub(pattern = "(.*\\.)(.*)($)", replacement = "\\2", x = filename)
  
  separators <- list('csv' = ',',
                    'tsv' = '\t',
                    'tab' = '\t')
  if (extension %in% names(separators)){
    sep <- separators[[extension]]
    data <- read.table(filename, header = TRUE, stringsAsFactors = FALSE, sep = sep)
  } else if (extension %in% c('xls', 'xlsx')) {
    GetPackages(xlsx)
    read.xlsx("myfile.xlsx", sheetName = "Sheet1")
    read.xlsx2("myfile.xlsx", sheetName = "Sheet1")
  }



  }
  






  

  if(type == 'presence/absence') {
    occurrence$type <- ifelse(occurrence[,3] == 1, 'presence', 'absence')
  } else {
    occurrence$type <- type
  }

  colnames(occurrence) <- c('longitude', 'latitude', 'value', 'type')

  if (!externalValidation){
    occurrence$fold <- 1
  } else {
    occurrence$fold <- 0
  }                     

  return(occurrence)
}
