#'PartitionDisc: Leave-one-disc-out cross-validation
#'
#'This process module partitions the sample into training and tests set by selecting circular test areas (possibly surrounded by an exclusion buffer) and using the remaining samples as training samples. See function partition.disc in package sperrorest for more details
#'
#'Module type: Process
#'
#'@param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#'@param radius Numeric the radius of the disc, in degrees 
#'
#'@param buffer Numeric, the size of the buffer zone in degrees 
#'
#'@author Tom August'
#'@author tomaug@@ceh.ac.uk'
#'@name PartitionDisc
#'@family process
PartitionDisc <-
  function(.data, radius = 2, buffer = 1){
    
    # Get the package we need
    zoon:::GetPackage('sperrorest')
    
    # warning if we are going to overwrite existing folds
    if(length(unique(.data$fold)) > 1) warning('PartitionDisc (Process module) will overwrite existing folds')
    
    # get coordinate columns, accounting for naming mismatch
    coord_names <- c(grep('^lon', colnames(.data$df), value = TRUE),
                     grep('^lat', colnames(.data$df), value = TRUE))
    
    parti <- partition.disc(.data$df, coords = coord_names,
                            radius = radius, buffer = buffer, ndisc = 1,
                            repetition = 1)
    
    # Assign classifications back to the data
    .data$df$fold <- NA 
    .data$df$fold[parti[[1]][[1]]$test] <- 0
    .data$df$fold[parti[[1]][[1]]$train] <- 1
    
    # Drop the NAs (these are records that fall in the buffer zone)
    .data$df <- .data$df[!is.na(.data$df$fold),]
    
    return(.data)
    
  }
