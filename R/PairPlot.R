#' @name PairPlot
#'
#' @title Pair plot of covariates with correlation
#'
#' @description This output module provides a pair plot of covariate data that includes correlation statistics.
#'
#' @details The data are plotted in the upper panel and the lower panel displays correlation statistics. Covariate names run diagonally. Modified from: http://wresch.github.io/2012/11/30/modified-splom.html
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
#' @section Version: 0.3
#'
#' @section Date submitted:  2017-09-07
PairPlot <- function (.model, .ras) {

  zoon::GetPackage(c('lattice', 'latticeExtra', 'hexbin'))
  
  pairsFunction <- function (df) {

    ct <- custom.theme(
      symbol = c("black", brewer.pal(n = 8, name = "Dark2")),
      fill = brewer.pal(n = 12, name = "Set3"),
      region = brewer.pal(n = 11, name = "Spectral"),
      reference = "#e8e8e8",
      bg = "transparent", fg = "black",
      lwd=2, pch=16
    )
    ct$axis.text$cex = 1.4
    ct$par.xlab.text$cex = 1.4
    ct$par.ylab.text$cex = 1.4

    cr <- colorRampPalette(c('grey80', 'grey0'))
    splom(~df,
          pscales = 0, #don't show axes,
          par.settings = ct,
          upper.panel = panel.hexbinplot,  # use hexbinplot
          xbins = 15,                     # number of bins
          trans = log10, inv=function(x) 10^x, # density color scale transformation
          colramp = cr,
          # show correlation coefficient in lower panel
          diag.panel = function(x, ...){
            yrng <- current.panel.limits()$ylim
            d <- density(x, na.rm=TRUE)
            d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
            panel.lines(d)
            diag.panel.splom(x, ...)
          },
          lower.panel = function(x,  y, ...) {
            panel.fill(col = brewer.pal(10, "RdBu")[round(cor(x, y) *  4 + 5)])
            panel.text(sum(range(x))/2, sum(range(y))/2, round(cor(x, y), 2), font = 2)
          },
          varname.cex = 0.9
    )
  }

  CovariateData <- as.data.frame(.model$data[,attributes(.model$data)$covCols])
  if (ncol(CovariateData) == 1) {
    print('Pair plot not possible, only one covariate')
  } else {
    return(pairsFunction(CovariateData))
  }

}
