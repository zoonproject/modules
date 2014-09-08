
#'Process module that does nothing. A place holder for if nothing should be
#'done to the data before modelling.
#'
#'
#'@return a Raster* object (class from the raster package) with the gridded
#'      covariates used to train and predict from the SDM.
#'
#'@name NoProcess


NoProcess <-
function(occurrence, ras){

  
  noccurrence <- nrow(occurrence)
  
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('lon', 'lat')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   occ_covs)
  
  names(df)[5:ncol(df)] <- names(ras)
  
  return(df)
  
}
