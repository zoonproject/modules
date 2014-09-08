#'covariate module to grab coarse resolution environmental data from NCEP
#'
#'@param extent A numeric vector of length 4 giving the coordinates of the 
#'  rectangular region within which to carry out the analysis, in the 
#'  order: xmin, xmax, ymin, ymax.
#'@param variables Character vector of which variables to select.
#'See NCEP.gather for more options but basic options are
#'       *‘air’*    Air Temperature                 deg K    
#'       *‘hgt’*    Geopotential Height             m        
#'       *‘rhum’*   Relative Humidity               %        
#'       *‘shum’*   Specific Humidity               kg/kg    
#'       *‘omega’*  Omega [Vertical Velocity]       Pascal/s 
#'       *‘uwnd’*   U-Wind Component [East/West]    m/s      
#'       *‘vwnd’*   V-Wind Component [North/South]  m/s      
#'
#'@return a Raster* object (class from the raster package) with the gridded
#'  covariates used to train and predict from the SDM.
#'
#'@seealso \code{\link{RNCEP::NCEP.gather}}
#'@name NCEP
NCEP <-
function(extent, variables){
  
  require(RNCEP)
  layers <- list()

  for(i in 1:length(variables)){
  data <- NCEP.gather(variable = variables[i],
                    level = 850,
                    months.minmax = c(1:2),
                    years.minmax = c(2000,2001),
                    lat.southnorth = extent[3:4],
                    lon.westeast = extent[1:2],
                    reanalysis2 = FALSE,
                    return.units = TRUE)
  
  avg <- apply(data, c(1, 2), mean)
  
  layers[[i]] <- raster(avg)
  names(layers[[i]]) <- variables[i]  

  extent(layers[[i]]) <- c(extent)
  }

  ras <- do.call(stack, layers)
  
  return (ras)  
}
