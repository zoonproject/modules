#' @name ResponseCurveViz
#'
#' @title Conditional Plots
#'
#' @description Basic conditional plots in one column
#'
#' @details Just starting out, much more to do
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param cov Covariates
#'
#' @family output
#'
#' @author G. McInerny, \email{gmcinerny@@hotmail.com}
#'
#' @section Data type: presence-only
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-15
ResponseCurveViz <- function (.model, .ras, cov = NULL) {
    def.par <- par(no.readonly = TRUE)
    params <- colnames(.model$data)[-(1:6)]
    nparams <- length(params)
    if (is.null(cov)) {
        covs <- params
    }
    else {
        covs <- params[cov]
    }
    rescale <- function(x) {
        x <- x - min(x)
        x/max(x)
    }
    lay <- matrix(seq_along(covs), nrow = length(covs), ncol = 1)
    layout(lay)
    par(mar = c(0, 0, 0, 0))
    cvs <- 0
    for (cov in covs) {
        cvs <- cvs + 1
        covars <- .model$data[, params, drop = FALSE]
        name <- cov
        covar <- covars[, cov]
        Ntestpoints = 100
        Ncoff <- ncol(covars)
        Epred <- as.data.frame(matrix(0, nrow = Ntestpoints, 
            ncol = Ncoff))
        colnames(Epred) <- colnames(covars)
        Eres <- Emeds <- Epred
        for (jj in 1:Ncoff) {
            Emeds[jj] <- rep(median(covars[[jj]], na.rm = TRUE), 
                dim(Emeds)[1])
            Emin <- min(covars[[jj]], na.rm = TRUE)
            Emax <- max(covars[[jj]], na.rm = TRUE)
            Epred[jj] <- seq(Emin, Emax, by = (Emax - Emin)/(Ntestpoints - 
                1))
        }
        for (kk in 1:Ncoff) {
            Etest <- Emeds
            Etest[kk] <- Epred[kk]
            p <- ZoonPredict(.model$model, newdata = Etest)
            if (!is.null(dim(p))) 
                p <- p[, 1]
            Eres[kk] <- p
        }
        cexlab = 1.3
        cexlab2 = 1.8
        cexlab3 = 1.2
        coltitles = c("grey10")
        coltitles2 = c("grey30")
        colaxes = c("grey50")
        colpres = c("grey10")
        colpseu = c("grey10")
        covar_scale <- rescale(covar)
        covar_min <- min(covar)
        covar_max <- max(covar)
        pres_idx <- which(.model$data$type == "presence")
        if (any(.model$data$type == "absence")) {
            back_idx <- which(.model$data$type == "absence")
            back_name <- "Absence"
        }
        else if (any(.model$data$type == "background")) {
            back_idx <- which(.model$data$type == "background")
            back_name <- "Background"
        }
        else {
            stop("no background or absence records present")
        }
        e1 <- Epred[, cov]
        p1 <- Eres[, cov]
        plot(e1, p1, cex = 0, axes = FALSE, xlab = "", ylab = "", 
            ylim = 0:1)
        text(max(e1), 0.5, labels = name, col = "grey80", cex = 6, 
            srt = 0, font = 1, adj = 1)
        lines(e1, p1, col = "grey10", lwd = 4)
        lines(e1, p1, col = "skyblue2", lwd = 2)
        axis(3, at = c(covar_min, covar_max), las = 1, col = colaxes, 
            line = -3)
        box(col = colaxes)
    }
    par(def.par)
    return(invisible())
}
