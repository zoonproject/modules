#'Process module: NoProcess
#'
#'Process module that does nothing. A place holder for if nothing should be
#'done to the data before modelling.
#'
#'
#'@return a Raster* object (class from the raster package) with the gridded
#'      covariates used to train and predict from the SDM.
#'
#'@name NoProcess


NoProcess <-
function (data) {
  

  occurrence <- data$df
  ras <- data$ras

  noccurrence <- nrow(occurrence)
  
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('longitude', 'latitude')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   occ_covs)
  
  names(df)[6:ncol(df)] <- names(ras)
  
  return(list(df=df, ras=ras))
  
}


