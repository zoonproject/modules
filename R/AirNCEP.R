
#'covariate module to grab a coarse resolution mean air temperature raster from
#'       January-February 2001-2002 for the given extent.
#'
#'@param extent A numeric vector of length 4 giving the coordinates of the 
#'      rectangular region within which to carry out the analysis, in the 
#'      order: xmin, xmax, ymin, ymax.
#'
#'@return a Raster* object (class from the raster package) with the gridded
#'      covariates used to train and predict from the SDM.
#'
#'@name AirNCEP



AirNCEP <- function(){
  require(RNCEP)
  require(raster)
  
  extent <- uk.extent <- c(xmin = -10,
              xmax = 10,
              ymin = 45,
              ymax = 65)

  c1 <- NCEP.gather(variable = 'air',
                    level = 850,
                    months.minmax = c(1:2),
                    years.minmax = c(2000,2001),
                    lat.southnorth = extent[3:4],
                    lon.westeast = extent[1:2],
                    reanalysis2 = FALSE,
                    return.units = TRUE)
  
  avg <- apply(c1, c(1, 2), mean)
  
  ras <- raster(avg)
  
  extent(ras) <- c(extent)
  
  return (ras)  
  
}

