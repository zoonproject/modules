#' @title Output module: PrintOccurrenceMap
#'
#' @description Plot a map of only occurrence data over the study area.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param plot If \code{TRUE} the plot will be displayed in the device
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
#' @param ... Parameters passed to sp::spplot, useful for setting title and axis labels e.g. \code{xlab = 'Axis Label', main = 'My Plot Title'}
#'
#' @return A Raster object giving the study area
#' 
#' @details Based on the PrintMap module
#'
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2017-03-07
#'
#' @name PrintOccurrenceMap
#' @family output
PrintMap <-
  function (.model,
            .ras,
            plot = TRUE,
            dir = NULL,
            filename = NULL,
            size = c(480, 480),
            res = 72,
            ...) {

    base_ras <- .ras[[1]] * 0

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

    plot.object <- spplot(base_ras,
                          sp.layout=points.list,
                          col.regions=grey(0.9),
                          cuts=0,
                          scales = list(draw = TRUE),
                          key = key,
                          colorkey = FALSE,
                          maxpixels = Inf,
                          ... = ...)

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

    return (base_ras)

  }
