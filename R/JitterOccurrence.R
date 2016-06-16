#' @name JitterOccurrence
#'
#' @title Add noise to occurrence coordinates
#'
#' @description Often the geographic coordinates of occurrence records are subject to uncertainty. One approach ot accounting for that uncertainty in an SDM analysis is to run several analyses, each time sampling a different location from a probability distribution over the likely "true" coordinates. \code{JitterOccurrence} generates one such sample, under an assumption that the distribution is an isotropic Gaussian with standard deviation \code{sd}
#'
#' @details Currently only a single standard deviation is provided for all occurrence records. Future iterations of this module will enable more control over the uncertainty distributions
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @param sd standard deviation of the gaussian noise to add the the coordinates of each ocurrence record
#'
#' @family process
#'
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, presence/background, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2016-06-16
JitterOccurrence <- function (.data, sd = 0) 
{
    df <- .data$df
    ras <- .data$ras
    n <- nrow(df)
    df$latitude <- df$latitude + rnorm(n, 0, sd)
    df$longitude <- df$longitude + rnorm(n, 0, sd)
    vals <- extract(ras, df[, c("longitude", "latitude")])
    df[, attr(df, "covCols")] <- vals
    df <- na.omit(df)
    return(list(df = df, ras = ras))
}
