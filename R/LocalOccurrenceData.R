#' @name LocalOccurrenceData
#'
#' @title Occurrence module: LocalOccurrenceData
#'
#' @description Occurrence module to format local occurrence data to be used with zoon. Must be a .csv file with three columns longitude, latitude and value in that order. Other columns will be kept. A column named CRS can be supplied if the coordinates are not lat/long. CRS should contain the proj4string for the coordinate system of the data (for example "+init=epsg:27700" for easting/northing data). If a CRS column is supplied longitude is taken to be the X coordinate and latitude the Y coordinate.
#'
#' @details 
#'
#' @param filename The path to the spreadsheet. The spreadsheet should have a header giving column names. The column names should be "longitude", latitude, and "value". "value" is a numeric such as a proportion, count or presence/absence (1/0). The file can be .csv, .tab, .tsv, or .xlsx
#'
#' @param occurrenceType What type data is it? One of "presence", "presence/absence", "abundance", "probability"
#'
#' @param columns Which columns in the spreadsheet relate to longitude, latitude and response value? Takes a named character vector e.g. c(long = "longitude", lat = "latitude" value = "responseVals"). If an unnamed character vector is given, the order is assumed to be long, lat, value.
#'
#' @param subsetSpecies Do you need to subset your spreadsheet by species? Supply a character vector of species name and the column name it is specified in (in that order)
#'
#' @param externalValidation Logical indicating whether this data is external validation data. Cross validation is handled elsewhere and this argument should be left as FALSE for data that will be used in cross validation. This is ignored if the user supplies a "fold" column.
#'
#' @family occurrence
#'
#' @author ZOON Developers, David Wilkinson, \email{zoonproject@@@@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 1.3
#'
#' @section Date submitted:  2017-12-17
LocalOccurrenceData <- function(filename='myData.csv',
           occurrenceType='presence',
           columns=c(long = 'longitude', lat = 'latitude', value = 'value'),
           subsetSpecies = NULL,
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
    
    if(!is.null(subsetSpecies)){
      if(!is.character(subsetSpecies[1]) & !is.character(subsetSpecies[2])){
        stop("subsetSpecies must be a character vector of 'species name' and 'column name'.")
      }
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
    
    # Get subset variables from args
    
    if(!is.null(subsetSpecies)){
      targetSpecies <- subsetSpecies[1]
      targetColumn <- subsetSpecies[2]
      
      command <- sprintf('data[data$%s == "%s", ]', targetColumn, targetSpecies)
      data <- eval(parse(text = command))
    }
    
    # command <- sprintf('data[ , c("%s", "%s", "%s")]', longName, latName, valName)
    # data <- eval(parse(text = command))
    
    names(data)[names(data) == longName] <- 'longitude'
    names(data)[names(data) == latName] <- 'latitude'
    names(data)[names(data) == valName] <- 'value'
    
    # If presence absence, give correct type name
    # Otherwise type is just type.
    if(type == 'presence/absence') {
      data$type <- ifelse(data[,'value'] == 1, 'presence', 'absence')
    } else {
      data$type <- type
    }
    
    # Fold gets 1 unless this is external validation data.
    if(!'fold' %in% tolower(names(data))){
      if(externalValidation){
        data$fold <- 0
      } else {
        data$fold <- 1
      }
    }
    
    return(data)
  }
