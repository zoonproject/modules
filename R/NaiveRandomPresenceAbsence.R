#' @name NaiveRandomPresenceAbsence
#'
#' @title Naive Random Presence/Absence
#'
#' @description Creates a random occurrence dataset within the extent given
#'
#' @details Points are created entirely at random using runif
#'
#' @param extent A numeric vector of length 4 giving the coordinates of the rectangular region within which to create the random points. order: xmin, xmax, ymin, ymax. By default the extent of the UK.#'
#' @param pAbs The proportion of records that should be absences (default: 0.5)#'
#' @param n Numeric - the number of points to create#'
#' @param seed Used with set.seed to set a seed. Default NULL, no seed is used
#'
#' @family occurrence
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#' @section Data type: presence/absence
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-02
NaiveRandomPresenceAbsence <- function (extent = c(-10, 10, 45, 65), pAbs = 0.5, n = 100, seed = NULL) 
{
    if (!is.null(seed)) 
        set.seed(seed)
    if (pAbs <= 0) 
        stop("pAbs cannot be 0, use module NaiveRandomPresence instead")
    long <- runif(n = n, min = extent[1], max = extent[2])
    lat <- runif(n = n, min = extent[3], max = extent[4])
    abs_index <- sample(x = 1:n, size = floor(n * pAbs), replace = FALSE)
    value <- rep(1, n)
    value[abs_index] <- 0
    type <- rep("presence", n)
    type[abs_index] <- "absence"
    occurrence <- data.frame(longitude = long, latitude = lat, 
        value = value, type = type, fold = 1, stringsAsFactors = FALSE)
    return(occurrence)
}
