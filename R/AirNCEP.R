#' @title Covariate module: AirNCEP
#'
#' @description Covariate module to grab a coarse resolution mean air temperature raster from
#'  January-February 2001-2002 for the UK.
#'  
#' @param quiet Logical. If TRUE (default) the progress of downloads is not shown 
#'
#' @seealso \code{\link{RNCEP::NCEP.gather}}
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @name AirNCEP
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @family covariate
AirNCEP <- function(quiet = TRUE){
  
  # zoon:::GetPackage('foreign')
  # zoon:::GetPackage('shapefiles')
  # zoon:::GetPackage('maptree')
  zoon:::GetPackage('RNCEP')
  
  extent <- uk.extent <- c(xmin = -10,
              xmax = 10,
              ymin = 45,
              ymax = 65)
  
  if(quiet) formals(fun = download.file)$quiet <- TRUE

  c1 <- NCEP.gather(variable = 'air',
                    level = 850,
                    months.minmax = c(1:2),
                    years.minmax = c(2000,2001),
                    lat.southnorth = extent[3:4],
                    lon.westeast = extent[1:2],
                    reanalysis2 = FALSE,
                    return.units = TRUE,
                    status.bar = TRUE)

  if(quiet) formals(download.file)$quiet <- FALSE
  
  avg <- apply(c1, c(1, 2), mean)
  
  ras <- raster(avg)
  
  extent(ras) <- c(extent)
  
  projection(ras) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  return(ras)  
  
}
