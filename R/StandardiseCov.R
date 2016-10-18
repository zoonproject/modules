#' @name StandardiseCov
#'
#' @title Scale numeric covariates
#'
#' @description The module by default scales all numeric covariates. By default conducts a regular standardisation: minus the mean and divide by the standard deviation. If Gelman = TRUE then divides by 2 sd as suggested by Gelman. Excluded covariates are not standardised.
#'
#' @details 
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @param Gelman Option to divide by two rather than one standard deviation.
#'
#' @param exclude Variable names of numeric variables to exclude from standardisation
#'
#' @family process
#'
#' @author Alison Johnston & Carsten F. Dormann, \email{alison.johnston@@bto.org}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-16
StandardiseCov <- function (.data, Gelman = FALSE, exclude = NULL) 
{
    df <- .data$df
    ras <- .data$ras
    # Keep attributes
    Atts <- attributes(df)[!names(attributes(df)) %in% c('names', 'class', 'row.names')]
    numericLayer <- NA
    for (i in 1:nlayers(ras)) numericLayer[i] <- is.numeric(ras[i]) & 
        !names(ras)[i] %in% exclude
    if (sum(numericLayer) == 0) 
        stop("StandardiseCov not used, as no numeric covariates")
    numericNames <- names(ras)[which(as.logical(numericLayer))]
    sd_mult <- ifelse(Gelman, 2, 1)
    for (cov in numericNames) {
        m <- cellStats(ras[[cov]], "mean", na.rm = TRUE)
        s <- cellStats(ras[[cov]], "sd", na.rm = TRUE) * sd_mult
        if (nlayers(ras) == 1) {
            ras <- (ras - m)/s
        }
        else {
            ras[[cov]] <- (ras[[cov]] - m)/s
        }
    }
    layer <- extract(ras, df[, c("longitude", "latitude")])
    names(layer) <- names(ras)
    df <- cbind(df[, !(names(df) %in% attr(df, 'covCols'))], layer)
    attributes(df) <- c(attributes(df), Atts)
    attr(df, which = 'covCols') <- names(ras)
    return(list(df = df, ras = ras))
}
