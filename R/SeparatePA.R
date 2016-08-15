#' @name SeparatePA
#'
#' @title two graphs showing the probabilies for presence and absence points
#'
#' @description The module retrieves output from a presence-absence model and outputs a simple figure with kernel-density estimated histrograms (left) and a boxplot (right).
#'
#' @details Based on code in the dismo vignette
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author E.E. van Loon, \email{e.e.vanloonD@@uva.nl}
#'
#' @section Data type: presence-only, presence/absence, presence/background
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-15
SeparatePA <- function (.model, .ras) 
{
  presidx <- mod$model$model$.df == 1
  absidx <- mod$model$model$.df == 0
  presprob <- mod$model$fitted.values[presidx]
  absprob <- mod$model$fitted.values[absidx]
  e <- dismo::evaluate(p = presprob, a = absprob)
  par(mfrow = c(1, 2))
  density(e)
  boxplot(e, col = c("blue", "red"))
}
