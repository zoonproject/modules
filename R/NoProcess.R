# A zoon module
# @process
NoProcess <-
function(occurrence, ras){

  require (dismo)
  
  noccurrence <- nrow(occurrence)
  
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('lon', 'lat')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   covs)
  
  names(df)[5:ncol(df)] <- names(ras)
  
  return(df)
  
}
