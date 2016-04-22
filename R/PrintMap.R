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
#' @param dir Directory where plots are saved. If \code{NULL} (default) then plots are
#' not saved. 
#' 
#' @param size A vector containing the width and height of the output figure when writing to a png file. Example: c(800,600).
#' 
#' @param res The output resolution in ppi when writing to a png file.
#'
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
  function (.model, .ras, plot = TRUE, dir = NULL, size = c(480,480), res = 72) {
    
    zoon::GetPackage(raster)
    
    vals <- data.frame(getValues(.ras))
    colnames(vals) <- names(.ras)
    
    pred <- ZoonPredict(.model$model,
                        newdata = vals)
    
    pred_ras <- .ras[[1]]
    
    pred_ras <- setValues(pred_ras, pred)
    
    ## Define color pallete
    cls <- colorRampPalette(c('#e0f3db', '#a8ddb5', '#4eb3d3', '#08589e'))(10)
    
    par(mar = c(4, 4, 0, 2) + 0.1)
    ST <- format(Sys.time(), "%Y_%m_%d-%H%M")
    
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
    plot.object <- spplot(pred_ras,
                          sp.layout=points.list,
                          col.regions=cls,cuts=length(cls)-1,
                          scales = list(draw = TRUE),
                          key = key)
    
    ## Plot the model on graphics device
    if(plot){
      print(plot.object)
    }
    
    ## Save to .png if output directory is specified
    if(!is.null(dir)){
      # Create the filename
      preferred_name <- paste(ST, ".png", sep = '')
      
      ## Check for prexisting file with same name as preferred_name
      if(!file.exists(file.path(dir, preferred_name))){
        plotname <- preferred_name
      } else {
        ## Create new file name with enumerated suffix (to aviod overwriting prexisting files)
        ex_files <- list.files(path = dir, pattern = paste('^', ST, sep = ''))
        plotname <- paste(ST, '_',as.character(length(ex_files)),'.png', sep = '')
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
    
    return(pred_ras)
    
  }
