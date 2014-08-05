

#'process module to generate up to 100 background records at random in
#'      cells of ras and return these along with the presence only data.
#'
#'@param occ Occurrence data, the output from an occurrence module
#'@param ras Covariate data, the output from a covariate module
#'
#'@return Dataframe with at least 5 columns
#'       value - a numeric value which may give 1 for presences, 0 for absences 
#'       or a positive integer for count data
#'       type - a character value saying what is in the value column
#'       lon - the longitude of the record
#'       lat - the latitutude of the record
#'       columns 5-n - the values of the covariates for each records (the names of
#'               these columns should correspond exactly to the names of the 
#'               layers in ras)
#'@name processA



processA <- function (occ, ras) {
  
  require (dismo)
  
  if (!all(occ$type == 'presence')) {
    stop ('this function only works for presence-only data')
  }
  
  # generate pseudo-absence data
  pa <- randomPoints(ras,
                     100)
  
  
  npres <- nrow(occ)
  
  npabs <- nrow(pa)
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occ[, c('lon', 'lat')]))
  
  pa_covs <- as.matrix(extract(ras, pa))
  
  covs <- rbind(occ_covs,
                pa_covs)
  
  # combine with the occurrence data
  df <- data.frame(value = rep(c(1, 0),
                               c(npres, npabs)),
                   type = rep(c('presence', 'background'),
                              c(npres, npabs)),
                   lon = c(occ$lon, pa[, 1]),
                   lat = c(occ$lat, pa[, 2]),
                   covs)
  
  names(df)[5:ncol(df)] <- names(ras)
  
  return(df)
  
}

