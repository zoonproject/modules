#' @title Process module: Boostrap
#'   
#' @description Process module to generate random bootstraps of the 
#'   data.
#'   
#' @param .data \strong{Internal parameter, do not use in the workflow
#'   function}. \code{.data} is a list of a data frame and a raster object
#'   returned from occurrence modules and covariate modules respectively.
#'   \code{.data} is passed automatically in workflow from the occurrence and
#'   covariate modules to the process module(s) and should not be passed by the
#'   user.
#'   
#' @param n the number of points to sample. Defaults to the same number as the dataset
#'   
#' @param seed Numeric used with \code{\link[base]{set.seed}}
#' 
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2016-03-23 
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'   
#' @name Bootstrap
#' @family process

Bootstrap <- function (.data, n = NULL, seed = NULL) {
  
  
  # set seed if specified
  if(!is.null(seed)){
    if(inherits(x = seed, what = c('numeric', 'integer'))){
      set.seed(seed)
    } else {
      stop("'seed' must be numeric or NULL")
    }
  }

  if(is.null(n)){
    n <- NROW(.data$df)
  }

  whichData <- sample(seq_len(NROW(.data$df)), n, replace = TRUE)
  df <- .data$df[whichData, ]

  return(list(df = df, ras = .data$ras))
  
}

