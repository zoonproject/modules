#' @name GenerateCovariateReport
#'
#' @title Generate a data profiling report
#'
#' @description This output module generates a report outlining the results of data profiling. This includes data structure, missing values, and correlation testing.
#'
#' @details The results are presented as an html output generated using Rmarkdown that opens in your internet browser
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author David Wilkinson, \email{davidpw@@student.unimelb.edu.au}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-04-24
GenerateCovariateReport <- function(.model, .ras){
  
  zoon::GetPackage("DataExplorer")
  zoon::GetPackage("raster")
  zoon::GetPackage("data.table")
  
  GenerateReport(data.table(as.data.frame(.ras)))
  
}
