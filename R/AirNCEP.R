#' @title Covariate module: AirNCEP
#'
#' @description Covariate module to grab a coarse resolution mean air temperature raster from
#'  January-February 2001-2002 for the UK.
#'  
#' @param status.bar Logical. Should a status bar be shown indicating the percentage of completion? 
#'
#' @seealso \code{\link{RNCEP::NCEP.gather}}
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @name AirNCEP
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @family covariate
AirNCEP <- function(status.bar = FALSE){
  zoon:::GetPackage(RNCEP)
  
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
                    return.units = TRUE,
                    status.bar = status.bar)
  
  avg <- apply(c1, c(1, 2), mean)
  
  ras <- raster(avg)
  
  extent(ras) <- c(extent)
  
  projection(ras) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  return(ras)  
  
}
