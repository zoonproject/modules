#' @title Process module: MESSMask
#'   
#' @description Calculate the multivariate environmental similarity surface 
#'   and mask areas with values outside the range of variability in the 
#'   training region.
#'   
#' @param .data \strong{Internal parameter, do not use in the workflow
#'   function}. \code{.data} is a list of a data frame and a raster object
#'   returned from occurrence modules and covariate modules respectively.
#'   \code{.data} is passed automatically in workflow from the occurrence and
#'   covariate modules to the process module(s) and should not be passed by the
#'   user.
#'   
#' @seealso \code{\link{dismo::mess}}
#' 
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2016-03-23 
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'   
#' @name MESSMask
#' @family process

MESSMask <- function (.data) {
  
  zoon:::GetPackage('dismo')
 
  # Ignore warnings because they're just a bunch of NA issues
  suppressWarnings(
    mess <- dismo::mess(.data$ras, v = .data$df[, attr(.data$df, 'covCols')], full = FALSE)
  )

  # Extreme values become NA
  values(mess)[values(mess) < 0] <- NA

  .data$ras <- mask(.data$ras, mess)

  return(.data)
  
}

