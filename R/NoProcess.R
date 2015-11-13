#' @title Process module: NoProcess
#'
#' @description Process module that does nothing. A place holder for if nothing should be
#' done to the data before modelling.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @return a Raster object (class from the raster package) with the gridded
#'      covariates used to train and predict from the SDM.
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @section Data type: presence-only, presence/absence
#'
#' @name NoProcess
#' @family process

NoProcess <-
function (.data) {
  

  occurrence <- .data$df
  ras <- .data$ras

  noccurrence <- nrow(occurrence)
  
  df <- occurrence
  names(df)[6:ncol(df)] <- names(ras)
  
  return(list(df=df, ras=ras))
  
}


