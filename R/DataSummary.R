#' @name DataSummary
#'
#' @title Output module: DataSummary
#'
#' @description The DataSummary module generates summaries of both the point data (non-background) and the raster data for the purposes of data exploration. The module outputs a list of two summaries to the console.
#'
#' @details 
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author David Wilkinson, \email{davidpw@@student.unimelb.edu.au}
#'
#' @section Data type: presence-only, presence/background, presence/absence
#'
#' @section Version: 1.1
#'
#' @section Date submitted:  2017-10-05
DataSummary <- function(.model, .ras){
  
  # Extract data frames
  
  data_df <- .model$data
  data_df <- data_df[data_df$type != "background", ]
  
  ras_df <- as.data.frame(.ras)
  
  # For purpose of summary, convert type to factor
  
  data_df$type <- as.factor(data_df$type)
  
  # Generate summaries
  
    ## Data
  
    data_df_sum <- summary(data_df[, !names(data_df) %in% c("type", "value", "fold", "predictions")])
  
    ## Raster
    
    ras_df_sum <- summary(ras_df)
    
    names(ras_df_sum) <- names(.ras)
    
  # Create single output
    
  output <- list(PointData = data_df_sum,
                 RasterData = ras_df_sum)  
  
  # Print to screen
  
  print(output)
}
