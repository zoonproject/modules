# A zoon module
# @process
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
