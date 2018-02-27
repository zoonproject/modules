#' @name ReliabilityPlot
#'
#' @title Produces are relaibility plot with associate linear regression analysis
#'
#' @description Returns a reliability plot of the discritised predictions versus obserations as well as the a vector of the intercept, gradient, and r-squared for further interpretation
#'
#' @details We can analyse the linear model of predictions versus observations for bias in our predictions. A gradient on 1 and and intercept of 0 indicate a perfect fit - the predictions equal the observations. An intercept less than 0 indicated that, on average, our model underpredicts. Greater than 0 indicates over-prediction, on average.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param n_bins An integer greater than 1 indicating the number of bins for predictions and obsertions
#'
#' @family output
#'
#' @author Liz Martin, \email{lizmartinresearch@@gmail.com}
#'
#' @section Data type: presence-only, presence/absence, presence/background
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2018-02-27
ReliabilityPlot <- function(.model, .ras, n_bins = 20){
  
  zoon:::GetPackage('SDMTools')
    
    
  if (n_bins < 2){
    stop ('You need two or more bins')
  }
  
  
  n_breaks <- n_bins+1
  
  colaxes <- "grey20"
  colabs <- "grey20"
  colfunc <- colorRampPalette(c("dodgerblue1", "darkblue"))
  coldots <- colfunc(n_bins)
  
  if (all(.model$data$predictions %in% c(0,1))){
    warning('The model has predicted presence/absence rather than probabilities. This module will not work')
  }
  
  if (all(.model$data$fold == 1)){ #if no cross-validation folds
    
    warning('You have no cross-validation folds, calibration results may be misleading.')
    
    # make predictions for the model
    covs <- .model$data[, 7:NCOL(.model$data), drop = FALSE]
    any(is.na(covs))
    p <- ZoonPredict(zoonModel = .model$model,
                     newdata = covs)
    o <- .model$data$value
    
    
    
  } else if (all(.model$data$fold >= 1)){ # if more than one cross-validation fold
    
    p <- .model$data$predictions
    o <- .model$data$value
    
    
  } else if (all(.model$data$fold %in% c(0,1))){ # if internal or external cross-validation 
    
    p <- .model$data$predictions
    o <- .model$data$value
    
  }
  
  #  message('Model performance measures:')
  #  for(i in 1:length(performance)) {
  #    line <- paste0(names(performance)[i],
  #                   ' :  ',
  #                   performance[i])
  #    message(line)
  #  }
  #  message(' ')
  bins <- .bincode(x = p, breaks = seq(0,1, length.out = n_breaks))
  
  n_obs_in_bins <- sapply(1:max(bins), function(x) length(p[which(bins == x)]))
  cex_sizes <- 1+(n_obs_in_bins - min(n_obs_in_bins))/(max(n_obs_in_bins) - min(n_obs_in_bins))*5
  
  bins_p <- sapply(1:max(bins), function(x) mean(p[which(bins == x)]))
  bins_o <- sapply(1:max(bins), function(x) mean(o[which(bins == x)]))
  
  max_bins_o<-max(bins_o, na.rm = TRUE)
  max_bins_p<-max(bins_p, na.rm = TRUE)
  
  axis_max <- ifelse(round(max(max_bins_o, max_bins_p),1) < max(max_bins_o, max_bins_p), round(max(max_bins_o, max_bins_p),1)+.1, round(max(max_bins_o, max_bins_p),1))

  layout(matrix(1:2,nrow=1),widths=c(0.9,0.1))
  par(mar=c(5.1,4.1,2.1,2.1))
  plot(x=bins_p, y=bins_o, xlim=c(0,axis_max), ylim=c(0,axis_max), pch=19,
       ylab="Observed occurrence (proportion of sites)",
       xlab="Predicted probability of presence",
       col=coldots, axes=FALSE, col.lab=colabs, cex=cex_sizes)
  axis(1, col = colaxes, lwd = 0, lwd.tick = 1, col.axis = colabs)
  axis(2, col = colaxes, las = 2, lwd = 0, lwd.tick = 1, col.axis = colabs)
  abline(a=0, b=1, col=colaxes, lty="dashed")
  box(bty="l", col=colaxes)
  calib <- lm(bins_p ~ bins_o)
  calib_output <- c(coefficients(calib), summary(calib)$r.squared)
  names(calib_output) <- c("intercept", "gradient", "r-squared")
  text(x=0, y=axis_max-0.05, paste("y =", round(calib_output[2],2), "x +", round(calib_output[1],2)), pos=4)
  text(x=0, y=axis_max-0.15, paste("r-sq =", round(calib_output[which(names(calib_output) == "r-squared")],2)), pos=4)
  
  par(mar=c(5.1,0.5,4.1,0.5))
  plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n", col.axis=colaxes)
  rect(
    xl,
    head(seq(yb,yt,(yt-yb)/n_bins),-1),
    xr,
    tail(seq(yb,yt,(yt-yb)/n_bins),-1),
    col=colfunc(n_bins)
  )
  
  mtext(c(round(seq(0,1, length.out=n_breaks),2), "Bins"), side=2,
        at = c(1, tail(seq(yb,yt,(yt-yb)/n_bins),-1), 2.05),
        las = 2,cex=0.7, col=colabs)

  
  return(calib_output)
  
}
