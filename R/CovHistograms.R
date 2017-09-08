#' @name CovHistograms
#'
#' @title Histograms of Covariate data
#'
#' @description This output module provides a histogram for each covariate.
#'
#' @details The frequency of each value is plotted for each covariate.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author Saras Windecker, \email{saras.windecker@@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-09-08
CovHistograms <- function (.model, .ras) {
  relPlot <- function (cov) {
    par  <- par(mfrow = c(ncol(cov)/2, 2))
    for (i in names(cov)) {
      hist(x = cov[,i], xlab = i, main = '')
    }
  }

  CovariateData <- as.data.frame(.model$data[,attributes(.model$data)$covCols])
  if (ncol(CovariateData) == 1) {
    print('Histograms not possible, only one covariate')
  } else {
    return(relPlot(CovariateData))
  }
}
