#' @name ROCcurve
#'
#' @title an ROC-curve with the AUC value plotted in the title
#'
#' @description The module retrieves output from a presence only, or presence-absence model and outputs an ROC-curve with the AUC-value above it  it uses the evaluate function from dismo.
#'
#' @details The module writes the ROC-plot to whatever is opened by dev.new() this can be changed to plot in the current output device by setting the option newwin=FALSE
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param newwin logical indicator (default: true) to specify whether the plot should be made in a new window
#'
#' @family output
#'
#' @author E.E. van Loon, \email{e.e.vanloonD@@uva.nl}
#'
#' @section Data type: presence-only, presence/absence, presence/background
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2016-06-15
ROCcurve <- function (.model, .ras, newwin = FALSE) 
{
    if (newwin) {
        dev.new()
    }
  
    # Extract model
    mod <- .model$model$model
  
    presidx <- mod$model$.df == 1
    absidx <- mod$model$.df == 0
    presprob <- mod$fitted.values[presidx]
    absprob <- mod$fitted.values[absidx]
    e <- dismo::evaluate(p = presprob, a = absprob)
    plot(e, "ROC")
}
