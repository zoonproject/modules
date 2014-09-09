#'Covariate module: AirNCEP
#'
#'Covariate module to grab a coarse resolution mean air temperature raster from
#'  January-February 2001-2002 for the given extent.
#'
#'@seealso \code{\link{RNCEP::NCEP.gather}}
#'@name AirNCEP
#'


AirNCEP <- function(){
  require(RNCEP)
  
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

