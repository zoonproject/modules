#' @title Occurrence module: malariaAtlas_PR
#'
#' @description Occurrence module to collect malaria parasite rate data
#'   from the Malaria Atlas Project database. 
#'
#' @param country	Character vector containing names of desired countries, 
#'   e.g. c("Country1", "Country2", ...) OR = "ALL" (use exactly one of 
#'   country, ISO and extent).
#' @param ISO	Character vector containing ISO3 code for desired country, 
#'  e.g. c("XXX", "YYY", ...) OR = "ALL" (use exactly one of 
#'   country, ISO and extent).
#' @param extent	Numeric vector containing bounding box values for desired
#'  area. In the order xmin, xman, ymin, ymax. (use exactly one of 
#'   country, ISO and extent).
#' @param species 'Pf' or 'Pv' for P. falciparum or P. vivax respectively.
#'
#' @param standardise Length two integer vector giving the age range to
#'   standardise all the points to. The original surveys were conducted
#'   on different age ranges but this affects PR, so typically they should
#'   be standardised. If PA is TRUE, the sample size will be kept the 
#'   same and the number of positives will be rounded. If NULL, no 
#'   standardisation will be performed.
#'
#' @param PA Should the data be transformed from parasite rate and examined  
#'  data to presence absence data? 
#'  Many modules only work with presence absence data so this TRUE aids 
#'  compatibility. However, the amount of data will often be very large
#'  with FALSE. A single country may have 50,000 presence absence data 
#'  points.
#'
#' @param year Vector of years for which to keep data. NULL keeps all years.
#' 
#' @param fold Control the validation. If 0, all data is validation data.
#'   If 1, all data is training data. 
#'
#' @seealso \code{\link{malariaAtlas::getPR}}
#'
#' @author Tim Lucas, \email{timcdlucas@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2018-04-19
#' @section Data type: presence/absence, proportion
#' @references 
#' 
#'   Smith, D. L. et al. Standardizing estimates of the
#'   Plasmodium falciparum parasite rate. Malaria Journal 6, 131 (2007).
#'
#'   Gething, Peter W., et al. "A long neglected world malaria map:
#'   Plasmodium vivax endemicity in 2010." PLoS neglected tropical
#'   diseases 6.9 (2012): e1814.
#' @name malariaAtlas_PR
#' @family occurrence





malariaAtlas_PR <- function(country = NULL, 
                            ISO = 'IDN', 
                            extent = NULL,
                            species = 'Pf', 
                            standardise = c(2, 10),
                            PA = TRUE, 
                            year = NULL,
                            fold = 1) 
{
  
  install.packages('malariaAtlas')
  library('malariaAtlas')
  
  # Makes things simpler by always going to lower case.
  species <- tolower(species)
  
  stopifnot(length(fold) == 1)
  stopifnot(length(species) == 1)
  
  
  # Prevent using both species together.
  if(species == 'both'){
    stop('Cannot select P. falciparum and P. vivax together because the data does not distinguish coinfections.')
  }
  
  # Get the data
  d <- malariaAtlas::getPR(country = country,
                           ISO = ISO, 
                           species = species)
  
  d <- d[!is.na(d$examined) & !is.na(d$positive), ]
      
  
  if(!is.null(year)){
    d <- d[d$year_start %in% year, ]
  }
  
  
  if(!is.null(standardise)){
    parset <- ifelse(species == 'Pf', 'Pf_Smith2007', 'Pv_Gething2012')
    new_pr <- malariaAtlas::convertPrevalence(d$positive / d$examined,
                                              d$lower_age,
                                              d$upper_age,
                                              rep(standardise[1], nrow(d)),
                                              rep(standardise[2], nrow(d)),
                                              parameters = parset)
    if(PA){
      d$positive <- round(new_pr * d$examined)
      if(any(d$positive > d$examined)){
        # I don't think this should ever happen, but just in case.
        warning(paste0('After standardisation, some surveys had', 
                'positive > examined. Setting positive = examined.'))
      }
    }
    
  } else {
    new_pr <- d$positive / d$examined
  }
  
  
  if(PA){
    # Copy d and take NOT positive
    d_absence <- d
    d_absence$positive <- d_absence$examined - d_absence$positive
    
    # Remove NA and empty rows
    d <- d[!is.na(d$positive) & d$positive > 0, ]
    d_absence <- d_absence[!is.na(d_absence$positive) & d_absence$positive > 0, ]
    
    # Make a row index
    d$index <- 1:nrow(d)
    d_absence$index <- 1:nrow(d_absence)
    
    # Create presence absence values
    d$value <- 1
    d_absence$value <- 0
    d$type <- 'presence'
    d_absence$type <- 'absence'
    
    # Replicate rows
    d <- d[rep(d$index, d$positive), ]
    d_absence <- d_absence[rep(d_absence$index, d_absence$positive), ]
    
    d <- rbind(d, d_absence)

  } else {
    d$value <- new_pr
    d$type <- 'probability'
  }
  
  
  
  occ <- data.frame(longitude = d$longitude, 
                    latitude = d$latitude, 
                    value = d$value,
                    type = d$type, 
                    fold = fold, 
                    stringsAsFactors = FALSE)
  
  if(!PA) occ$weight <- d$examined
  
  occ <- occ[stats::complete.cases(occ), ]
  return(occ)
}





