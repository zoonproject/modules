#' @name Lorem_ipsum_UK
#'
#' @title A dataset of Lorem ipsum occurrences
#'
#' @description The module retrieves a dataset ofLorem ipsum records from figshare. This dataset containsprecence only data and was collected between 1990 and2000 by members of to Lorem ipsum appreciation society
#'
#' @details This dataset is fake, Lorem ipsum does not exist
#'
#' @family occurrence
#'
#' @author A.B. Ceidi, \email{ABCD@@anemail.com}
#'
#' @section Data type: presence-only
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-13
Lorem_ipsum_UK <- function () 
{
    URL <- "https://ndownloader.figshare.com/files/2519918"
    out <- read.csv(URL)
    out <- out[, c("longitude", "latitude")]
    out$value <- 1
    out$type <- "presence"
    out$fold <- 1
    return(out)
} 
