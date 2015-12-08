#' @title Output module: PrintMap
#'
#' @description Plot a map of predicted surface.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user. 
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @name PrintMap
#' @family output
PrintMap <-
function (.model, .ras) {
  
  zoon::GetPackage(raster)
  
  vals <- data.frame(getValues(.ras))
  colnames(vals) <- names(.ras)

  pred <- ZoonPredict(.model$model,
                      newdata = vals)

  pred_ras <- .ras[[1]]
  
  pred_ras <- setValues(pred_ras, pred)

  cls <- colorRampPalette(c('#e0f3db', '#a8ddb5', '#4eb3d3', '#08589e'))(10)

  par(mar = c(4, 4, 0, 2) + 0.1)
  plot(pred_ras, col = cls, xlab = 'Longitude2', ylab = 'Latitude') 
  if(any(.model$data$value == 0)){
    points(.model$data$longitude[.model$data$value == 0], .model$data$latitude[.model$data$value == 0], 
      pch = 16, col = '#00000055')
  } 
  if(any(.model$data$value == 1)){
    points(.model$data$longitude[.model$data$value == 1], .model$data$latitude[.model$data$value == 1], 
      pch = 16, col = '#e41a1c55')
  }
  legend('topright', legend = c('Presence', 'Absence'), fill = c( '#e41a1c', '#000000'), 
    bty = 'n', border = NA, inset=c(-0.23, 0.1), xpd = TRUE)

  
  
  return(NULL)
  
}
