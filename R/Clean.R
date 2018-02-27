
#' @title Process module: Clean
#'
#' @description Do some data cleaning on occurrence points
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#' @param which A numeric or character vector describing which data cleaning processes to use (see details).
#'
#'@seealso \code{\link{scrubr::coord_incomplete}}
#'  
#'@details A number of data cleaning steps are implemented. \code{which} takes a (case insensitive) character or numeric vector
#'  declaring which should be used:
#' \itemize{
#'  \item{"1 - Impossible"}{Remove rows with imposisble latitude or longitude values}
#'  \item{"2 - Incomplete"}{Remove rows with missing data for either latitude of longitude}
#'  \item{"3 - Unlikely"}{Remove rows with unlikely data such as (0, 0)}
#' }
#' 
#' @author ZOON Developers, David Wilkinson \email{zoonproject@@gmail.com}
#' @section Version: 1.1
#' @section Date submitted: 2015-03-21
#' @section Date updated: 2017-08-09
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @name Clean
#' @family process


Clean <- function (.data, which = c(1, 2, 3, 4)) {
  
  # Make lower case clean argument to make comparisons easier  
  if(is.character(which)) which <- tolower(which)
  
  # The list of possible entries in clean (as a character vector).
  listOfPossible <- c('impossible', 'incomplete', 'unlikely')
  
  # If any items in clean aren't available, throw an error.
  if(!all(which %in% 1:4)){
    if(!all(which %in% listOfPossible)){
      stop('In Clean module you have selected cleaning processes that do not exist. Please alter "which" argument')
    }
  }
  
  # Extract point data
  
  occurrence <- .data$df
  
  # Number of data points pre-Clean
  
  numPoints_start <- nrow(occurrence)
  numPointsPres_start <- nrow(occurrence[occurrence$type == "presence", ])
  numPointsAbs_start <- nrow(occurrence[occurrence$type == "absence", ])
  numPointsBack_start <- nrow(occurrence[occurrence$type == "background", ])
  
  # Load the scrubr package for data cleaning
  zoon::GetPackage('scrubr')  
  
  # Remove impossible lat lon rows.
  if (1 %in% which | 'impossible' %in% which){
    occurrence <- coord_impossible(occurrence, lat = 'latitude', lon = 'longitude')
  }
  
  # remove incomplete data rows
  if (2 %in% which | 'incomplete' %in% which){
    occurrence <- coord_incomplete(occurrence, lat = 'latitude', lon = 'longitude')
  }
  
  # Remove unlikely datapoints
  if (3 %in% which | 'unlikely' %in% which){
    occurrence <- coord_unlikely(occurrence, lat = 'latitude', lon = 'longitude')
  }
  
  #  # Remove data points close to city (and maybe country?) centroids
  #  if (4 %in% clean | 'pol_centroids' %in% clean){
  #    occurrence <- coord_pol_centroids(occurrence)
  #  }
  
  # Number of data points post-Clean
  
  numPoints_end <- nrow(occurrence)
  numPointsPres_end <- nrow(occurrence[occurrence$type == "presence", ])
  numPointsAbs_end <- nrow(occurrence[occurrence$type == "absence", ])
  numPointsBack_end <- nrow(occurrence[occurrence$type == "background", ]) 
  
  # Number of data points removed
  
  points_removed <- numPoints_start - numPoints_end
  pointsPres_removed <- numPointsPres_start - numPointsPres_end
  pointsAbs_removed <- numPointsAbs_start - numPointsAbs_end
  pointsBack_removed <- numPointsBack_start - numPointsBack_end
  
  if(is.null(pointsPres_removed)){
    pointsPres_removed <- 0
  }
  if(is.null(pointsAbs_removed)){
    pointsAbs_removed <- 0
  }
  if(is.null(pointsBack_removed)){
    pointsBack_removed <- 0
  }
  
  # Generate message to print to screen
  
  points_rem_message <- paste(
    "The Clean module has removed", points_removed, "data points.",
    "This includes", pointsPres_removed, "presences,", pointsAbs_removed,
    "absences, and", pointsBack_removed, "background points.",
    collapse = " ")
  
  # print message to screen
  
  message(points_rem_message)
  
  # return object
  
  return(list(df = occurrence, ras = .data$ras))
  
}
