#' @name PairPlot
#'
#' @title Pair plot of covariates with correlation
#'
#' @description This output module provides a pair plot of covariate data that includes correlation statistics.
#'
#' @details The data are plotted in the bottom left-hand panel and on the top-right are displayed the correlation statistics. Covariate names run diagonally down from top left to bottom right. For original code, see: https://gist.github.com/arsalvacion/1ba2373bbe89b2d3c023#file-pairscor-r
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
#' @section Version: 0.2
#'
#' @section Date submitted:  2017-05-05
PairPlot <- function (.model, .ras) {

  panel.cor <- function(x, y, digits = 2, cex.cor, ...) {

    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))

    # correlation coefficient
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste("r= ", txt, sep = "")
    text(0.5, 0.6, txt)

    # p-value calculation
    p <- cor.test(x, y)$p.value
    txt2 <- format(c(p, 0.123456789), digits = digits)[1]
    txt2 <- paste("p= ", txt2, sep = "")
    if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
    text(0.5, 0.4, txt2)

  }

  CovariateData <- as.data.frame(.model$data[,attributes(.model$data)$covCols])
  if (ncol(CovariateData) == 1) {
    print('Pair plot not possible, only one covariate')
  } else {
    return(pairs(CovariateData, upper.panel = panel.cor))
  } 

}
