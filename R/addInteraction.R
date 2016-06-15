#' @name addInteraction
#'
#' @title Set up interactions among covariates
#'
#' @description The module constructs new covariates torepresent interactions (and possibly polynomial terms).
#'
#' @details Statistical interactions are (mathematically) the product of covariates. This module allows the user to pre-specify which interactions to add to the data. If a four-way interaction between A, B, C and D is desired, it automatically adds all possible two- and three-way interactions as well (obeying the marginality theorem). If the same variable name is provided several times in a vector (e.g. c(A, A, A)), this will lead to a cubic polynomial.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @param which_covs List with vector(s) of names of covariates to interact, e.g. list(c('A', 'C', 'D')), or 'pairs' for all pairwise interactions.
#'
#' @param exclude For option 'pairs', exclude the covariates named from interactions.
#'
#' @family process
#'
#' @author Alison Johnston & Carsten F. Dormann, \email{alison.johnston@@bto.org}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-15
addInteraction <- function (.data, which_covs = "pairs", exclude = NULL) 
{
    df <- .data$df
    ras <- .data$ras
    if (is.null(which_covs) | length(names(names(ras)[!(names(ras) %in% 
        exclude)])) < 2) {
        return(list(df = df, ras = ras))
    }
    if (!is.null(exclude) & which_covs[1] != "pairs") {
        warning("The exclude-argument has been ignored because which_covs is not 'pairs'.")
    }
    if (which_covs[1] == "pairs") {
        which_covs <- combn(names(ras)[!(names(ras) %in% exclude)], 
            2, simplify = FALSE)
    }
    namesOfVars <- unique(c(unlist(which_covs), exclude))
    if (any(!(namesOfVars %in% names(ras)))) 
        stop("A variable name in which_covs or exclude does not match covariate names.")
    check4lowerOrderInts <- which_covs[sapply(which_covs, length) > 
        2]
    if (length(check4lowerOrderInts) != 0) {
        for (i in 1:length(check4lowerOrderInts)) {
            ints <- lapply(2:(length(check4lowerOrderInts[[i]]) - 
                1), function(x) unlist(combn(check4lowerOrderInts[[i]], 
                x)))
        }
        for (k in 1:length(ints)) {
            for (l in 1:NCOL(ints[[k]])) {
                which_covs[[length(which_covs) + 1]] <- as.vector(ints[[k]][, 
                  l])
            }
        }
    }
    which_covs <- lapply(which_covs, sort)
    which_covs <- unique(which_covs)
    for (i in 1:length(which_covs)) {
        vars2comb <- which_covs[[i]]
        positionOfVariables2combine <- sapply(1:length(vars2comb), 
            function(x) grep(vars2comb[x], names(ras))[1])
        newint <- calc(ras[[positionOfVariables2combine]], fun = prod)
        names(newint) <- paste0(vars2comb, collapse = "_X_")
        ras <- stack(ras, newint)
    }
    vals <- extract(ras, df[, c("longitude", "latitude")])
    df <- cbind(df[, 1:5], vals)
    return(list(df = df, ras = ras))
}
