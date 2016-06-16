#' @title Process module: AddRandomUniformPredictors
#'
#' @description Process module which adds a random uniform covariate to the dataset.  This new covariate can be scaled to an existing covariate.
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#' @param name Optional argument specifying the name of the new covariate layer.  If not specified, it will automatically named. 
#' @param scaleTo Optional argument specifying the name of an existing covariate layer which the new random uniform covariate will be scaled to.
#'
#' @return a Raster object with the appended random uniform covariate.
#'
#' @author James Campbell, \email{jamesadamcampbell@@gmail.com}
#' @section Version: 1.01
#' @section Date submitted: 2016-06-15
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @examples 
#' work1 <- workflow(occurrence = UKAnophelesPlumbeus,
#'    covariate  = UKBioclim,
#'    process    = Chain(OneHundredBackground,
#'                    AddRandomUniformPredictors(scaleTo = 'bio1',name = 'Random.bio1'),
#'                    AddRandomUniformPredictors(scaleTo = 'bio2'),
#'                    AddRandomUniformPredictors),
#'    model      = LogisticRegression,
#'    output     = PerformanceMeasures)
#'    
#'### Dsiplay resulting covariate maps from each workflow
#'spplot(work1$process.output[[1]]$ras$bio1)
#'spplot(work1$process.output[[1]]$ras$bio2)
#'spplot(work1$process.output[[1]]$ras$Random.bio1.1)
#'spplot(work1$process.output[[1]]$ras$RandUnif.1)
#'spplot(work1$process.output[[1]]$ras$RandUnif.2)
#'
#'### Show resulting model
#'work1$model.output[[1]]$model$model
#'
#' @name AddRandomUniformPredictors
#' @family process

AddRandomUniformPredictors <-
  function (.data, name = 'RandUnif', scaleTo = NULL) {
    
    df <- .data$df
    ras <- .data$ras
    ## Preserve attributes from data.
    df.attributes <- attributes(df)
    
    ## Define new raster layer name
    if(missing(name)){
      nameTemplate <- 'RandUnif';
    }else{
      nameTemplate <- name
    }
    
    i <- 0
    repeat{  ## Make sure previously used names are not overwritte
      i <-i + 1
      newName <- paste0(nameTemplate,'.',i)
      if(!newName %in% names(ras)){break}
    }
    
    ## Create new raster layer
    # Make template from existing layer
    newLayer <- raster(ras)
    ## Set range for new values
    if(!missing(scaleTo)){
      ## Error catching
      if(!(scaleTo %in% names(ras))){
        stop(paste0('There is no layer named: ',scaleTo, '.  Change the scaleTo argument to point to an existing covariate layer'))
      }
      ## Scale data if specified
      randUnif.range <- range(df[,scaleTo])
      
      ## Get indicies of NA values (will copy NA values)
      idx.NA <- which(is.na(values(subset(ras,subset = scaleTo))))
    }else{
      randUnif.range <- c(0,1)
      idx.NA <- c()
    }
      
    # Set random uniform values
    values(newLayer) <- runif(length(values(newLayer)),min = randUnif.range[1],max = randUnif.range[2])
    ## Copy NA points from scaleTo layer
    values(newLayer)[idx.NA] <- NA
    names(newLayer) <-newName 
    
    ## Append new random uniform layer to ras object
    ras.appended <- addLayer(ras,newLayer)
    
    ## Extract data from new raster layer and add to data.frame
    newLayer.extracted <- data.frame(extract(newLayer,df[,c('longitude','latitude')]))
    
    ## Append new data to df
    df[,newName] <- newLayer.extracted
    
    ## Redefine covCols attributes
    attr(df, 'covCols') <- names(ras.appended)
    
    return(list(df=df, ras=ras.appended))
  }
