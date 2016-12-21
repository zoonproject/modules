#' @title Process module: SubsampleOccurrence
#'   
#' @description Process module to sample a random subset of the occurrence data.
#'   
#' @param .data \strong{Internal parameter, do not use in the workflow 
#'   function}. \code{.data} is a list of a data frame and a raster object 
#'   returned from occurrence modules and covariate modules respectively. 
#'   \code{.data} is passed automatically in workflow from the occurrence and 
#'   covariate modules to the process module(s) and should not be passed by the 
#'   user.
#'   
#' @param n the number of points to sample. Defaults to the same number as the
#'   dataset
#'   
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2016-12-21
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'   
#' @name SubsampleOccurrence
#' @family process
SubsampleOccurrence <- function (.data, n = 100) {
  # randomly retain a subsample of occurrence records
  choose <- sample(seq_len(nrow(.data$df)), n)
  .data$df <- .data$df[choose, ]
  .data
}