#' @name NATrees
#'
#' @title Occurrences for 22 tree species from North America
#'
#' @description Retrieves occurrences for 22 trees from eastern North America. Data originally sourced from multiple forestinventory databases, collected in the QUICC-FOR project ($URL). This dataset was originally published in $REF. Data extent is c(-97, -57, 25, 53)
#'
#' @details 
#'
#' @param localRDSFilePath Path to the local data file. Defaults to null, in which case random data are returned.
#'
#' @param dateRange Optional 2-item vector giving the years to use; default is the full range of the data (1960-2013)
#'
#' @param spCode Which species should be extracted
#'
#' @family occurrence
#'
#' @author Matt Talluto, \email{mtalluto@@gmail.com}
#'
#' @section Data type: presence/absence
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-15
NATrees <- function (localRDSFilePath = NULL, dateRange = NULL, 
    spCode = "28757-ACE-SAC") 
{
	if(is.null(localRDSFilePath)) {
		n=1000
		spCode = 'random'
		prDat = data.frame(
			lon = runif(n, -97, -57),
			lat = runif(n, 25, 53),
			random = rbinom(n, 1, 0.3)
		)
	} else {
	    dat = readRDS(localRDSFilePath)
  	  if (is.null(dateRange)) dateRange = range(dat$presAbs$year_measured)
    	prDat = with(dat$presAbs, dat$presAbs[year_measured >= dateRange[1] & 
      	  year_measured <= dateRange[2], c("lon", "lat", spCode)])
	}
    out = data.frame(longitude = prDat$lon, latitude = prDat$lat, 
        value = prDat[, spCode], type = ifelse(prDat[, spCode], 
            "presence", "absence"), fold = 1, stringsAsFactors = FALSE)
    out
}
