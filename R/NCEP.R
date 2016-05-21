#' @title Covariate module: NCEP
#'
#' @description Covariate module to grab coarse resolution environmental data from NCEP
#'
#' @param extent A numeric vector of length 4 giving the coordinates of the 
#'  rectangular region within which to carry out the analysis, in the 
#'  order: xmin, xmax, ymin, ymax.
#' @param variables Character vector of which variables to select.
#' See NCEP.gather for more options but basic options are
#'       *'air'*    Air Temperature                 deg K    
#'       *'hgt'*    Geopotential Height             m        
#'       *'rhum'*   Relative Humidity               %        
#'       *'shum'*   Specific Humidity               kg/kg    
#'       *'omega'*  Omega [Vertical Velocity]       Pascal/s 
#'       *'uwnd'*   U-Wind Component [East/West]    m/s      
#'       *'vwnd'*   V-Wind Component [North/South]  m/s    
#' @param status.bar Logical. Should a status bar be shown indicating the percentage of completion? 
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#'
#' @seealso \code{\link{RNCEP::NCEP.gather}}
#' @name NCEP
#' @family covariate
NCEP <-
function(extent = c(-5, 5, 50, 60),
         variables = 'hgt',
         status.bar = FALSE){
  

  zoon:::GetPackage('RNCEP')

  layers <- list()

  for(i in 1:length(variables)){
    
    suppressWarnings({
    data <- NCEP.gather(variable = variables[i],
                      level = 850,
                      months.minmax = c(1:2),
                      years.minmax = c(2000,2001),
                      lat.southnorth = extent[3:4],
                      lon.westeast = extent[1:2],
                      reanalysis2 = FALSE,
                      return.units = TRUE)
    })
    
    avg <- apply(data, c(1, 2), mean)
    
    layers[[i]] <- raster(avg)
    names(layers[[i]]) <- variables[i]  
  
    extent(layers[[i]]) <- c(extent)
  }

  ras <- do.call(raster::stack, layers)
  
  projection(ras) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  return (ras)  
}
