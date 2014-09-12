
#'Process module: BackgroundAndCrossvalid
#'
#'Process module to generate up to 100 background records at random in
#'      cells of ras and split all data in k folds for cross validation.
#'
#'@param k The number of folds you wish to have. Will later implement a 'leaveoneout' opt
#'
#'@name BackgroundAndCrossvalid



BackgroundAndCrossvalid <- function (occurrence, ras, k=5) {
  
  zoon:::GetPackage(dismo)
  
  if (!all(occurrence$type == 'presence')) {
    stop ('"BackgroundAndCrossvalid" module only works for presence-only data')
  }
  
  # generate pseudo-absence data
  points <- 100
  if(ncell(ras) < 100){
    points <- ncell(ras)
    warning(paste0('There are fewer than 100 cells in the environmental raster.', 
      '\nUsing all available cells (', ncell(ras), ') instead'))
  }
  pa <- randomPoints(ras, points)
  
  
  npres <- nrow(occurrence)
  
  npabs <- nrow(pa)
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('longitude', 'latitude')]))
  
  pa_covs <- as.matrix(extract(ras, pa))
  
  covs <- rbind(occ_covs, pa_covs)
  
  # combine with the occurrence data
  df <- data.frame(value = rep(c(1, 0),
                               c(npres, npabs)),
                   type = rep(c('presence', 'background'),
                              c(npres, npabs)),
                   fold = c(sample(1:k, npres, replace=TRUE), sample(1:k, npabs, replace=TRUE)),
                   lon = c(occurrence$lon, pa[, 1]),
                   lat = c(occurrence$lat, pa[, 2]),
                   covs)
  
  names(df)[6:ncol(df)] <- names(ras)
  
  df <- na.omit(df)

  return(df)
  
}

