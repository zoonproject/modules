#' @title Process module: RemoveNAs
#'
#' @description Process module that removes data containing NAs. 
#' Remove either data with NAs in any column or only data with NAs
#' in the response. As a number of model modules cannot handle NAs, this
#' may be necessary.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @param responseOnly Logical indicating whether to only remove rows with NAs in the response column.
#' @param ignore Character vector of columns to ignore i.e. columns that can have NAs without being removed. 
#'
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2018-04-11
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @name RemoveNAs
#' @family process

RemoveNAs <-
  function (.data, responseOnly = FALSE, ignore = NULL) {
    
    stopifnot(is.logical(responseOnly))

    df <- .data$df
    
    if(responseOnly){
      df <- df[!is.na(df$value), ]
    } else {
      df <- df[complete.cases(df[, !names(df) %in% ignore]), ]
    }
    
    
    return(list(df=df, ras=.data$ras))
    
  }


