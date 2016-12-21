#' @name CarolinaWrenValidation
#'   
#' @title Presence/absence validation data for the Carolina Wren in the USA
#'   
#' @description Presence/absence of the Carolina Wren \emph{Thryothorus 
#'   ludovicianus} at Breeding Bird Survey transects in 2006. These data are 
#'   made available in the maxlike R package. See 
#'   \code{\link[maxlike]{carw.data}} for more details. This module loads the 
#'   data, transforms the coordinates to lat/longs and formats the data for use 
#'   in a zoon workflow. This module is identical to \code{CarolinaWrenPO}, 
#'   except that the data are added as external validation data (fold = 0), 
#'   rather than training data (fold = 1).
#'   
#' @param .data \strong{Internal parameter, do not use in the workflow
#'   function}. \code{.data} is a list of a data frame and a raster object
#'   returned from occurrence modules and covariate modules respectively.
#'   \code{.data} is passed automatically in workflow from the occurrence and
#'   covariate modules to the process module(s) and should not be passed by the
#'   user.
#' 
#' @family process
#'   
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#'   
#' @section Data type: presence/absence
#'   
#' @section Version: 0.1
#'   
#' @section Date submitted:  2016-12-21
CarolinaWrenValidation <- function (.data) {
  
  # load the data from maxlike
  zoon::GetPackage("maxlike")
  data(carw, envir = environment())
  occ <- na.omit(carw.data$pa.data)
  
  #transform the coordinate system form Albers to lat/long
  coords_raw <- sp::SpatialPoints(occ[, c("X", "Y")],
                                  proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  coords <- sp::spTransform(coords_raw, "+init=epsg:4326")
  
  # create a dataframe
  eval_data <- data.frame(value = occ$y,
                          type = ifelse(occ$y, "presence", "absence"),
                          fold = 0,
                          longitude = coords@coords[, "X"],
                          latitude = coords@coords[, "Y"],
                          stringsAsFactors = FALSE)
  
  # extract and append covariate values
  eval_covs <- as.matrix(extract(.data$ras, eval_data[, c('longitude', 'latitude')]))
  colnames(eval_covs) <- attr(.data$df, 'covCols')
  eval_data <- cbind(eval_data, eval_covs)
  
  # combine the evaluation data with the training data and return
  .data$df <- rbind(.data$df, eval_data)
  .data
  
}
