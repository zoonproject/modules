#' @name NoOutput
#'
#' @title Do not create any output
#'
#' @description No output is created
#'
#' @details 
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author Tim Lucas, \email{timcdlucas@@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, presence/background, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2016-06-15
NoOutput <- function (.model, .ras) 
{
    return(NULL)
}
