#' @name TargetGroupBackground
#'
#' @title Target Group Background Data
#'
#' @description This module takes as one of its arguements a call to an occurrence module, these occurrence points are then used as background points. This follows the methods outlined by Phillips et al (Phillips, S. J., Dudík, M., Elith, J., Graham, C. H., Lehmann, A., Leathwick, J. and Ferrier, S. (2009), Sample selection bias and presence-only distribution models: implications for background and pseudo-absence data. Ecological Applications, 19: 181–197. doi:10.1890/07-2153.1) as a method to choose background points with similar bias to the target species presence points.
#'
#' @details 
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @param OccurrenceCall A call to an occurrence module in the same format as would be given to the "Occurrence" argument in workflow. Can be a Chain but cannot be a list (instead list TargetGroupBackground).
#'
#' @param naOmit Should NA containing rows be removed from the dataset (occurrence and background points combine). Defaults to TRUE
#'
#' @family process
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#' @section Data type: presence-only
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2016-08-22
TargetGroupBackground <- function(.data,
                                  OccurrenceCall = NaiveRandomPresence,
                                  naOmit = TRUE){
  
  df <- .data$df
  ras <- .data$ras
  
  # Subsitute the occurrence call
  occMod <- substitute(OccurrenceCall)
  occCall <- zoon:::CheckModList(occMod)
  
  # Don't allow lists
  if(length(occCall) > 1){
    if('chain' %in% names(attributes(occCall))){
      if(attr(occCall, 'chain') != TRUE){
        stop('OccurrenceCall cannot be a list, instead list the module TargetGroupBackground')
      }
    } else {
      stop('OccurrenceCall cannot be a list, instead list the module TargetGroupBackground')
    }
  }
  
  # break down the call
  occurrenceName <- zoon:::LapplyGetModule(occCall, forceReproducible = FALSE)
 
  # Use the same code as in the main workflow function
  # to run the occurrence module
  e <- environment() 

  occurrence.output <- lapply(occurrenceName, FUN = zoon:::DoOccurrenceModule, e = e)
  if(is.null(occurrence.output)) stop ('In TargetGroupBackground: Call to retrieve occurrence data for background returned NULL')

  # Then bind together if the occurrence modules were chained
  if (identical(attr(occCall, 'chain'), TRUE)){
    occurrence.output <- list(do.call(rbind, occurrence.output))
    attr(occurrence.output[[1]], 'call_path') <- list(occurrence = paste('Chain(',
                                                                         paste(lapply(occurrenceName, function(x) x$module),
                                                                               collapse = ', '),
                                                                         ')', sep = ''))
  }
  
  # Error if no data to add
  if(!inherits(occurrence.output[[1]], what = 'data.frame')) stop(paste('In TargetGroupBackground: Occurrence data returned for use as background is',
                                                             'not a data.frame:\n',
                                                             paste(invisible(capture.output(str(occurrence.output[[1]]))),
                                                                         collapse = '\n')))
  if(nrow(occurrence.output[[1]]) < 1) stop('In TargetGroupBackground: There are no rows in the occurrence data to be used as background')
  
  # Check they dont fall outsie raster
  occurrence <- occurrence.output[[1]]
  bad.coords <- is.na(cellFromXY(ras,
                                 occurrence[,c('longitude', 'latitude')]))
  if(any(bad.coords)){
    nr_before <- nrow(occurrence)
    occurrence <- occurrence[!bad.coords, ]
    nr_after <- nrow(occurrence)
    
    if(nr_after > 0){
      warning (paste('In TargetGroupBackground:',
                     nr_before - nr_after,
                     'background points are outside the raster extent and have been removed before modelling leaving',
                     nr_after, 'background points'))
      occurrence.output[[1]] <- occurrence.output[[1]][!bad.coords,]
    } else if(nr_after == 0) {
      warning(paste('In TargetGroupBackground: All background points are outside the raster extent. Try changing your raster.'))
    }
  }
  
  # Make the new data into background
  occurrence.output[[1]]$value <- 0 
  occurrence.output[[1]]$type <- 'background' 
  
  # Add the raster data to this layer
  ras.values <- raster::extract(ras, occurrence.output[[1]][, c('longitude', 'latitude')])
  if(is.null(ras.values)){
    occurrenceCovariates <- NULL
    warning('Locations in the background data did not match your raster so no covariate data were extracted. This is only a good idea if you are creating simulated data in a process module')
  }else{
    occurrenceCovariates <- as.matrix(ras.values)
    colnames(occurrenceCovariates) <- names(ras)  
  }
  
  occurrence.output[[1]] <- cbindZoon(occurrence.output[[1]], occurrenceCovariates)

  # Bind this to the original data
  # Note this should fail gracefully if the columns don't match
  df <- try(rbind(df, occurrence.output[[1]]), silent = TRUE)

  # This is the graceful fail  
  if(grepl('error', class(df))){
    stop('\nIn TargetGroupBackground: failed to add background data to occurrence data. Ensure that the columns are the same in both datasets.') 
  }
 
  if(length(is.na(df)) > 0){
    before <- nrow(df)
    after <- nrow(na.omit(df))
    if(naOmit){
      df <- na.omit(df)
      if((before - after) > 0){
        warning(paste('In TargetGroupBackground:',
                      before - after,
                      'rows of the combined background and occurrence data points (from a total of',
                      before,
                      ') have been removed as they contain NAs'))
      }
    } else {
      warning(paste('In TargetGroupBackground:',
                    before - after,
                    'rows of data contain NAs and may cause problems in your model'))
    }
  }
  
  # Now return the modified data and raster
  return(list(df = df, ras = ras))
   
}
