#' @title Output module: PrintMap
#'
#' @description Plot a map of predicted surface.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user. 
#'
#' @param plot If \code{TRUE} the plot will be displayed in the device
#' 
#' @param points If \code{TRUE} the training points will be plotted over the prediction surface
#'
#' @param dir Directory where plots are saved. If both \code{dir} and \code{filename} are \code{NULL} (default) then plots are
#' not saved. 
#' 
#' @param filename The name to be given to the output as a character, don't include a file extension. If both \code{dir} and \code{filename} are \code{NULL} (default) then plots are
#' not saved. 
#' 
#' @param size A vector containing the width and height of the output figure when writing to a png file. Example: c(800,600).
#' 
#' @param res The output resolution in ppi when writing to a png file.
#'
#' @param threshold The threshold value to use to convert probabilities to binary 1's and 0's. Default is NULL ie not used.
#' 
#' @param thresholdmethod The method used to calculate probability threshold. One of 'probability', 'quantile', 'falsepositive', 'falsenegative'.
#'   See Details for specifics.
#' 
#' @param ... Parameters passed to sp::spplot, useful for setting title and axis labels e.g. \code{xlab = 'Axis Label', main = 'My Plot Title'}
#'
#' @details For creating maps with only presence absence values, there are a number of options for setting a threshold.
#' The \code{threshold} argument sets the value for the threshold while \code{thresholdmethod} selects the methods used to set the threshold.
#' \enumerate{
#'   \item `probability' (default) Any pixels with predicted probability (or relative probability, depending on the model) greater than the threshold are set to presence
#'   \item `quantile' \code{threshold} gives the proportion of pixels that should be absense. The threshold value is selected so that this is true.
#'   \item `falsepositive' \code{threshold} sets the proportion of absense data points (not pixels) that should be misclassified as presence.
#'   \item `falsenegative' \code{threshold} sets the proportion of presence data points (not pixels) that should be misclassified as absense
#' }
#' @return A Raster object giving the probabilistic model predictions for each
#' cell of covariate raster layer
#'
#' @author ZOON Developers, James Campbell, \email{zoonproject@@gmail.com}
#' @section Version: 1.1
#' @section Date submitted: 2016-04-02
#'
#' @name PrintMap
#' @family output
PrintMap <-
  function (.model, 
            .ras, 
            plot = TRUE,
            points = TRUE, 
            dir = NULL,
            filename = NULL,
            size = c(480, 480), 
            res = 72,
            threshold = NULL, 
            thresholdmethod = c('probability', 'quantile', 'falsepositive', 'falsenegative'),
            ...) {
    
    vals <- data.frame(getValues(.ras))
    colnames(vals) <- names(.ras)
    
    pred <- ZoonPredict(.model$model,
                        newdata = vals)
    
    # use threshold if needed
    if(!is.null(threshold)){
      
      # Use correct thresholdmethod
      #   In each case, 
      if(thresholdmethod == 'probability'){
        stopifnot(threshold <= 1 & threshold >= 0)
        probthreshold <- threshold
      } else if(thresholdmethod == 'quantile'){
          stopifnot(threshold <= 1 & threshold >= 0)
          probthreshold <- quantile(pred, probs = threshold, na.rm = TRUE)
        
      } else if(thresholdmethod %in% c('falsepositive', 'falsenegative')){
          stopifnot(threshold <= 1 & threshold >= 0)
          if(!all(.model$data$value %in% c(0, 1))){
            stop('false positive and false negative methods require presence, absence or background points only.')
          }
        
          # Predict known points using full model.
          covs <- .model$data[.model$data$fold != 0, 7:NCOL(.model$data), drop = FALSE]
          
          p <- ZoonPredict(zoonModel = .model$model,
                           newdata = covs)
          combdf <- cbind(.model$data[.model$data$fold != 0, ], p)
          
          target <- ifelse(thresholdmethod == 'falsepositive', 0, 1)

          # If we want "threshold" proportion of positives to be misclassified as negative,
          #   Find "threshold" proportion through sorted vector of probabilities 
          
          midpoint <- threshold * sum(.model$data$value == target)
          whichindices <- c(floor(midpoint), ceiling(midpoint))
          
          # Actually want mean of probabilities either side.
          sortedp <- sort(p[.model$data$value == target], decreasing = as.logical(!target))
          probthreshold <- sum(sortedp[whichindices]) / 2
      } else {
        stop("thresholdmethod not recognised. Should be one of 'probability', 'quantile', 'falsepositive', 'falsenegative'.")
      }
          
          
      
      # Now binarise predictions. 
      pred[pred >= probthreshold] <- 1
      pred[pred < probthreshold] <- 0
    }
    
    pred_ras <- setValues(.ras[[1]], pred)
    
    ## Define color palette
    cls <- colorRampPalette(c('#e0f3db', '#a8ddb5', '#4eb3d3', '#08589e'))(10)
    
    par(mar = c(4, 4, 0, 2) + 0.1)

    ## Create plot object
    
    if(any(.model$data$value != 0 & .model$data$value != 1)){
      ## Define abundance colors and key
      
      cls.abundance <- colorRampPalette(c('#ff0000', '#ffff00', '#00ff00'))(10)
      points.abundance <- SpatialPoints(coords = .model$data[.model$data$value == 0,c('longitude','latitude')])
      pl.abundance <- list('sp.points',
                           points.abundance, 
                           pch=21, cex=1, 
                           fill=cls.abundance, 
                           col = '#00000055', 
                           alpha = 0.5)
      # Color from red to green for o to max abundance values
      key <- list(corner = c(0,0),
                  space = 'inside',
                  text = list(as.character(c(max(.model$data$value), max(.model$data$value)/2,min(.model$data$value)))), 
                  points = list(pch = 21,fill = c('#00ff00','#ffff00','#ff0000'),col = '#000000'))
      points.list <- list(pl.abundance)
    }else{
      ## Define presence/absence colores and key
      
      if(any(.model$data$value == 0)){
        ## Define absence points
        points.absence <- SpatialPoints(coords = .model$data[.model$data$value == 0,c('longitude','latitude')])
        pl.absence <- list('sp.points', points.absence, pch=16, cex=1, col='#00000055')
      }else{
        pl.absence <- list()
      }
      # Define presence points
      if(any(.model$data$value == 1)){
        ## Define presence points
        points.presence <- SpatialPoints(coords = .model$data[.model$data$value == 1,c('longitude','latitude')])
        pl.presence <- list('sp.points', points.presence, pch=16, cex=1, col='#e41a1c55')      
      }else{
        pl.presence <- list()
      }
      points.list <- list(pl.absence,pl.presence)
      key <- list(corner = c(0,0),
                  space = 'inside',
                  text = list(c('Presence', 'Absence')),
                  points = list(pch = 21,fill = c('#e41a1c','#000000')))
    }
    
    ## Create plotting object
    # corner: between 0 and 1 for x and y, defines which corner to place the legend in. 
    
    if (points) {
      plot.object <- spplot(pred_ras,
                            sp.layout=points.list,
                            col.regions=cls,cuts=length(cls)-1,
                            scales = list(draw = TRUE),
                            key = key,
                            maxpixels = Inf,
                            ... = ...)
    } else {
      plot.object <- spplot(pred_ras,
                            col.regions=cls,cuts=length(cls)-1,
                            scales = list(draw = TRUE),
                            maxpixels = Inf,
                            ... = ...)
      
    }
    
    ## Plot the model on graphics device
    if(plot){
      print(plot.object)
    }
    
    ## Save to .png if output directory is specified
    if(!is.null(dir) | !is.null(filename)){
    
      if(is.null(dir)) dir <- getwd()
      if(is.null(filename)) filename <- format(Sys.time(), "%Y_%m_%d-%H%M")

      # Create the filename
      preferred_name <- paste(filename, ".png", sep = '')
      
      ## Check for prexisting file with same name as preferred_name
      if(!file.exists(file.path(dir, preferred_name))){
        plotname <- preferred_name
      } else {
        ## Create new file name with enumerated suffix (to aviod overwriting prexisting files)
        ex_files <- list.files(path = dir, pattern = paste('^', filename, sep = ''))
        plotname <- paste(filename, '_',as.character(length(ex_files)),'.png', sep = '')
      }
      
      ## Initiate png graphics device
      png(filename = ifelse(test = !is.null(dir),
                            yes = file.path(dir, plotname),
                            no = plotname),
          res = res,
          width = size[1],
          height = size[2])
      
      ## Push plot object to graphics device
      print(plot.object)
      ## Close graphics device (write image to file)
      dev.off()
    }
    
    return (pred_ras)
    
  }
