#' @name LocalOccurrenceDataFrame
#'
#' @title Occurrence module: LocalOccurrenceDataFrame
#'
#' @description Occurrence module to format local occurrence data to be used with zoon. Must be a data.frame with three columns containing values for longitude, latitude and value. A column named CRS can be supplied if the coordinates are not lat/long. CRS should contain the proj4string for the coordinate system of the data (for example "+init=epsg:27700" for easting/northing data). If a CRS column is supplied longitude is taken to be the X coordinate and latitude the Y coordinate.
#'
#' @details 
#'
#' @param dataFrame A data.frame in your environment. Must have columns recording values for "longitude", "latitude", and "value". "value" is a numeric such as a proportion, count or presence/absence (1/0).
#'
#' @param occurrenceType What type data is it? One of "presence", "presence/absence", "abundance", "probability"
#'
#' @param columns Which columns in the data.frame relate to longitude, latitude and response value? Takes a named character vector e.g. c(long = "longitude", lat = "latitude" value = "responseVals"). If an unnamed character vector is given, the order is assumed to be long, lat, value.
#'
#' @param subsetSpecies Do you need to subset your spreadsheet by species? Supply a character vector of species name and the column name it is specified in (in that order)
#'
#' @param externalValidation Logical indicating whether this data is external validation data. Cross validation is handled elsewhere and this argument should be left as FALSE for data that will be used in cross validation. This is ignored if the user supplies a "fold" column.
#'
#' @family occurrence
#'
#' @author David Wilkinson, ZOON Developers, \email{davidpw@@student.unimelb.edu.au}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 1.1
#'
#' @section Date submitted:  2017-12-17
LocalOccurrenceDataFrame <- function(dataFrame=read.csv("myData.csv"),  # is there a data.frame in zoon's namespace that could go here?
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
    
    # Read in the data.frame
    
    data <- dataFrame
    
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
