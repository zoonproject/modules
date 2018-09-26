#' @title Occurrence module: malariaAtlas_VecOcc
#' #
#' @description Occurrence module to collect malaria vector occurrence data
#'  from the Malaria Atlas Project database. 
#'  
#'  @param country	Character vector containing names of desired countries, 
#'   e.g. c("Country1", "Country2", ...) OR = "ALL" (use exactly one of 
#'   country, ISO and extent).
#' @param ISO	Character vector containing ISO3 code for desired country, 
#'  e.g. c("XXX", "YYY", ...) OR = "ALL" (use exactly one of 
#'   country, ISO and extent).
#' @param extent	Numeric vector containing bounding box values for desired
#'  area. In the order xmin, xman, ymin, ymax. (use exactly one of 
#'   country, ISO and extent).
#' @param species character string containing name of desired species,
#'  e.g. "Species1". 
#'  
#' @param year Vector of years for which to keep data. NULL keeps all years.
#'  
#'  @param fold Control the validation. If 0, all data is validation data.
#'   If 1, all data is training data.
#'   
#' @seealso \code{\link{malariaAtlas::getVecOcc}}
#' 
#' @author Suzanne Keddie, \email{suzanne.keddie@@bdi.ox.ac.uk}
#' @section Version: 1.0
#' @section Date submitted: 2018-09-26
#' @section Data type: presence-only   
#' 
#' @name malariaAtlas_VecOcc
#' @family occurrence





malariaAtlas_VecOcc <- function(country = NULL,
                                ISO = "MMR",
                                extent = NULL,
                                species,       
                                year = NULL,
                                fold = 1)
{
  
  zoon::GetPackage('malariaAtlas')
  
  stopifnot(length(fold) == 1)
  
  # Prevent null or more than one species
  if(is.null(species)){
    stop('Please select a malaria vector species')
  }
  
  if(length(species) >1){
    stop('Cannot select more than one species at once')
  }
  
  if(!is.null(extent)){
    extent <- matrix(extent, nrow = 2, byrow = TRUE)
  }
  
  
  # Get the data
  d <- malariaAtlas::getVecOcc(country = country,
                           ISO = ISO, 
                           extent = extent,
                           species = species)
  
  
  if(!is.null(year)){
    d <- d[d$year_start %in% year, ]
  }
  
  
  occ <- data.frame(longitude = d$longitude, 
                    latitude = d$latitude,
                    fold = fold,
                    stringsAsFactors = FALSE)
  
  occ$value <- 1
  occ$type <- 'presence'
  
  occ <- occ[stats::complete.cases(occ), ]
  return(occ)
}
