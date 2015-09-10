#'Process module: HundredBackground
#'
#'Process module to generate background records at random in
#'      cells of the covariate raster and return these along with the presence only data.
#'
#'@param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#'@param n the number of background points to sample
#'
#'@name Background
Background <- function (.data, n = 100) {
  
  zoon:::GetPackage(dismo)
  
  occurrence <- .data$df
  ras <- .data$ras
 
  if (!all(occurrence$type == 'presence')) {
    stop ('"Background" module only works for presence-only data')
  }
  
  # generate pseudo-absence data
  points <- n
  if(ncell(ras) < n){
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
                   fold = rep(1, npres + npabs),
                   longitude = c(occurrence$lon, pa[, 1]),
                   latitude = c(occurrence$lat, pa[, 2]),
                   covs)
  
  names(df)[6:ncol(df)] <- names(ras)
  
  # remove missing values
  if(NROW(na.omit(df)) > 0){
    df <- na.omit(df)
  }

  return(list(df=df, ras=ras))
  
}

