#' @title Occurrence module: LocalOccurrenceData
#'
#' @description Occurrence module to format local occurrence data to be used with zoon. 
#'  Must be a .csv file with three columns longitude, latitude and value 
#'  in that order.
#'
#' @param filename The path to the spreadsheet. The spreadsheet should have a 
#'  header giving column names. .csv, .tab, .tsv handled. The xlsx
#'  package is used for excel files.
#'
#' @param occurrenceType What type data is it? One of 'presence', 
#' 'presence/absence', 'abundance', 'probability' 
#'
#' @param externalValidation Logical indicating whether this data is external
#'  validation data. Cross validation is handled elsewhere and this argument 
#'  should be left as FALSE for data that will be used in cross validation. 
#'
#' @param columns Which columns in the spreadsheet relate to longitude, latitude
#'  and response value? Takes a named character vector e.g.
#'  c(long = 'longitude', lat = 'latitude', value = 'responseVals'). If an
#'  unnamed character vector is given, the order is assumed to be 
#'  long, lat, value.
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @section Data type: presence-only, presence/background, presence/absence, abundance, proportion
#' @name LocalOccurrenceData
#' @family occurrence
LocalOccurrenceData <-
function(filename='myData.csv',
         occurrenceType='presence',
         columns=c(long = 'longitude', lat = 'latitude', value = 'value'),
         externalValidation = FALSE){

  zoon:::GetPackage('assertthat')
  
  # If occurrence type is not a string, turn it into one.
  if(!is.string(occurrenceType)){
    occurrenceType <- deparse(substitute(occurrenceType))
  }
  
  assert_that(is.logical(externalValidation))
  type <- tolower(occurrenceType)

  # Fuzzy match to find presence/absence variations.
  # But also be sure that 'presence' doesn't fuzzy match as presence/absence
  if(agrepl('presence/absence', type,
                  max.distance = list(insertions = 4)) & 
      !type == 'presence' &
      !type == 'absence'){
    
    type <- 'presence/absence'
  }

  if(!type %in% c('presence', 'presence/absence', 'abundance', 'probability')){
    stop("occurrenceType must be one of 'presence', 'presence/absence', 'abundance' or 'probability'.")
  }

  
  # Read in the data  
  # Try and do a vaguley thorough job of checking format.
  # Cheers @klmr
  # https://gist.github.com/klmr/4f093bb49dcf1aa72b7a

  extension <- gsub(pattern = "(.*\\.)(.*)($)", replacement = "\\2", x = filename)
  
  separators <- list('csv' = ',',
                    'tsv' = '\t',
                    'tab' = '\t')

  
  loadComplete <- FALSE
  if (extension %in% names(separators)){
    try({
      sep <- separators[[extension]]
      data <- read.table(filename, header = TRUE, stringsAsFactors = FALSE, sep = sep)
      loadComplete <- TRUE
    }, silent = TRUE)
    # If above block didn't work, try csv2
    if (!loadComplete){
      data <- read.csv2(filename, header = TRUE, stringsAsFactors = FALSE)
      loadComplete <- TRUE
    }
    # If it still isn't right, load data.table and use fread.
    if (ncol(data) < 3 | !loadComplete){
      zoon::GetPackage('data.table')
      data <- as.data.frame(fread(filename))
    }
  } else if (extension %in% c('xls', 'xlsx')) {
    zoon::GetPackage('xlsx')
    data <- read.xlsx(filename, header = TRUE, sheetIndex = 1)
  } else if (extension == 'dbf') {
    zoon::GetPackage('foreign')
    data <- read.dbf(filename)
  } else if (exists(filename, envir = globalenv())) {
    data <- eval(parse(text = filename), envir = globalenv())
  } else {
    stop("Can't open spreadsheet. Is it one of supported formats?")
  }

  
  # Get column names from args
  if(is.null(names(columns))){
    longName <- columns[1]
    latName <- columns[2]
    valName <- columns[3]
  } else {
    longName <- columns['long']
    latName <- columns['lat']
    valName <- columns['value']
  }

  df <- data.frame(longitude = data[,longName],
                   latitude = data[,latName],
                   value = data[,valName])

  # If presence absence, give correct type name
  # Otherwise type is just type.
  if(type == 'presence/absence') {
    df$type <- ifelse(df[,3] == 1, 'presence', 'absence')
  } else {
    df$type <- type
  }

  # Fold gets 1 unless this is external validation data.
  if(externalValidation){
    df$fold <- 0
  } else {
    df$fold <- 1
  }

  return(df)

  }
  




  
