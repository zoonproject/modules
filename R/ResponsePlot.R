#' @name ResponsePlot
#'
#' @title Plot predicted response curves along covariate gradients
#'
#' @description This module plots predicted response curves along covariate gradients for all or specified covariates
#'
#' @details The module outputs a simple line graph of probability of occupancy (relative for presence-only and presence background data, absolute for presence-absence data). When the covariate is specified, a single line graph is returned. When cov = NULL, the module plots line graphs for each covariate in the zoon workflow iteratively. Currently only implemented for presence-only, presence-background, and presence-absence data.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param cov covariate name
#'
#' @family output
#'
#' @author Liz Martin, \email{emartin@@student.unimelb.edu.au}
#'
#' @section Data type: presence-only, presence/absence
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-04-28
ResponsePlot <- function (.model, .ras, cov = NULL) {
  
  cov_names <- attr(.model$data, 'covCols')
  
  # by default, plot all covariates
  if (is.null(cov)) {
    for (i in seq_along(cov_names)) {
      ResponsePlot(.model, .ras, cov = i)
    }
    return (invisible())
  }
  
  rescale <- function (x) {
    # scale to 0/1 for colour schemes
    x <- x - min(x)
    x / max(x)
  }
  
  # extract covariates matrix
  covars <- .model$data[, cov_names, drop = FALSE]
  
  # name of key covariate
  name <- colnames(covars)[cov]
  
  # get covariate of interest
  covar <- covars[, cov]
  
  # ~~~~~~~~~~~~~~~~~~~~
  # make predictions for all covariates
  
  
  #select how many pred points to use
  n_test_points <- 500
  
  #get the number of coefficients in the model
  n_coeff <- ncol(covars)
  
  # create a dummy data frame
  pred_to <- as.data.frame( matrix(NA, nrow = n_test_points , ncol = n_coeff) )
  
  colnames(pred_to) <- colnames(covars)
  
  # make a copy where the value is held for all others, and one for predictions
  pred_out <- meds <- pred_to
  
  # for each variable
  for( j in 1:n_coeff ){
    # fill in the median value for all varaibles
    meds[j] <- rep(median(covars[[j]], na.rm = TRUE), dim(meds)[1])
    
    # create a sequence form low to high for the varaibale of interest
    min <- min(covars[[j]], na.rm = TRUE)
    max <- max(covars[[j]], na.rm = TRUE)
    pred_to[j] <- seq( min, max, length.out = n_test_points )
  }
  
  for(k in 1:n_coeff){
    # select which predictor variable 
    test <- meds
    # Add in variation for variable of focus
    test[k] <- pred_to[k]
    # make predictions
    p <- ZoonPredict(.model$model,
                     newdata = test)
    
    # make sure it's a vector
    if (!is.null(dim(p))) p <- p[, 1]
    
    pred_out[k] <- p
    
  }	
  
  # plotting
  # save default, for resetting...
  def.par <- par(no.readonly = TRUE) 
  
  # get range of values for var
  covar_min <- min( covar)
  covar_max <- max( covar )
  
  # get subsets for presences
  pres_idx <- which(.model$data$type=="presence")
  
  # Plot response function
  colaxes="grey20"
  colabs="grey20"
  if (any(.model$data$type == 'absence')) {
    y_lab <- "Probability of Occupancy"
  } else {
    y_lab <- "Relative Probability of Occupancy"
  }
  e1 <- pred_to[, cov]
  p1 <- pred_out[, cov]
  plot(e1,p1, cex=0, axes = FALSE, ylim = 0:1, type="l", col="darkblue", lwd=2, xlab=name, ylab=y_lab, col.lab=colabs)
  axis(1, col = colaxes, lwd = 0, lwd.tick = 1, col.axis = colabs)#, at= seq( covar_min, covar_max, by=10), las=3, col=colaxes)
  axis(2, col = colaxes, las = 2, lwd = 0, lwd.tick = 1, col.axis = colabs)
  box(bty="l",col=colaxes)
  #points(x=covar[pres_idx], y=jitter(rep(1, length(pres_idx))), pch=20, cex=0.5)
  # if (any(.model$data$type == 'absence')) {
  #   abs_idx <- which(.model$data$type == 'absence')
  #   points(x=covar[abs_idx], y=rep(0, length(abs_idx)), pch=20)
  # }
  
  
  # Reset to default
  par(def.par)  
  
}
