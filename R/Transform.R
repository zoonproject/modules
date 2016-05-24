#' @name Transform
#'
#' @title Apply Transformations to Covariates
#'
#' @description Apply a tranformation function (e.g. square, log, something else) to one or more covariates, pixelwise. These can either overwrite the named covariates, or be added to the covariate set.
#'
#' @details  This module is essentially a wrapper around the \code{raster} function \code{\link[raster]{calc}}.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.#'
#' @param trans a function defining a transformation to apply to each pixel in the covariate(s) raster. This can be a definition (e.g. \code{function(x) x^2} for a square) or a function defined in base R (e.g. \code{log1p} for the log(x + 1) transformation). The default transformation does nothing.#'
#' @param which_cov a character vector naming all of the covariates in the RasterStack to which the new transformation should be applied.#'
#' @param replace a logical, determining whether to overwrite the values of the named layers with the transformed values (if \code{replace = TRUE}), or to add extra layers with the transformed covariates. If additional layers are added, they will be renamed accoring to the \code{raster} package's renaming conventions. This normally involves adding a '.2' at the end of the name.
#'
#' @family process
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, presence/background, abundance, proportion
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-05-25
Transform <- function (.data, trans = function(x) {
    x
}, which_cov = NULL, replace = TRUE) 
{
    df <- .data$df
    ras <- .data$ras
    if (is.null(which_cov)) 
        which_cov <- names(ras)
    stopifnot(all(which_cov %in% names(ras)))
    if (!replace) {
        new_idx <- seq_along(which_cov) + nlayers(ras)
        ras <- stack(ras, ras[[which_cov]])
        which_cov <- names(ras)[new_idx]
    }
    if (nlayers(ras) == 1) {
        ras <- calc(ras, trans)
    }
    else {
        for (cov in which_cov) {
            ras[[cov]] <- calc(ras[[cov]], trans)
        }
    }
    vals <- extract(ras, df[, c("longitude", "latitude")])
    df <- cbind(df[, 1:5], vals)
    return(list(df = df, ras = ras))
}
