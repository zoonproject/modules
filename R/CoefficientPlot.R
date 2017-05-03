#' @name CoefficientPlot
#'
#' @title Plot estimated coeffcients from LogisticRegression
#'
#' @description This module plots the estimated coefficients from the LogisticRegression module and returns the summarised model object
#'
#' @details The module outputs a point plot of coefficient estimates from the LogisticRegression module. Coefficients with a p-value <= 0.05 are identified on the graph. The summary glm object is returned in the console. This module is only implemented for the LogisticRegression model module.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author Liz Martin, \email{emartin@@student.unimelb.edu.au}
#'
#' @section Data type: presence/absence
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-05-03
CoefficientPlot <- function (.model, .ras) {
  # this module was modifed from Tom August's VariableImportance module
  
  colabs <-"grey20"
  colaxes <- "grey20"
  cov_names <- attr(.model$data, 'covCols')
  dev.par <- par(no.readonly = TRUE)
  
  model_obj <- .model$model$model
  class_model_obj <- class(model_obj)
  
  if (length(class_model_obj) > 1) {
    message("Class of model object (from model module) has length > 1, using first value")
    class_model_obj <- class_model_obj[1]
  }
  
  if (!inherits(class_modelObj, "character")) 
    stop("CoefficientPlot: model class is not a character")
  
  if ( class_model_obj == "glm" ){
    sum_model_obj <- model_obj$coefficients
    
    signif_0.05_index <- which(coef(summary(model_obj))[,4]<=0.05)
    colours <- rep("darkblue", length(sum_model_obj))
    colours[signif_0.05_index] <- "darkorange"  
    
    par(mar=c(4,6,1,1))
    plot(x=sum_model_obj, y=1:length(sum_model_obj), pch=19, col=colours, axes=FALSE, ylab="", xlab="Coefficient estimate", col.lab=colabs, ylim=c(0.5, length(sum_model_obj) + 0.5))
    axis(1, col = colaxes, lwd = 0, lwd.tick = 1, col.axis = colabs)
    axis(2, col = colaxes, las = 2, lwd = 0, lwd.tick = 1, col.axis = colabs, labels=c("Intercept", cov_names), at=1:length(sum_model_obj))
    box(bty="l",col=colaxes)
    abline(v=0, col=colaxes, lty="dashed")
    legend(x= "topleft", legend = "p-value <= 0.05", col ="darkorange", pch=19, border = NULL, text.col = "grey20", bty="n")
  
    } else{
      stop("CoefficientPlot output module only implemented for LogisticRegression model module")
    }
  return(summary(model_obj))
  par(dev.par)    
}
