#' @name NATrees
#'
#' @title Occurrences for 21 tree species from North America
#'
#' @description Retrieves occurrences for 21 trees from eastern North America. Data originally sourced from USGS FIA forest inventory database, collected in the QUICC-FOR project ($URL). This dataset was originally published in $REF. Extent for this dataset is c(-97,-67,25,50)
#'
#' @details 
#'
#' @param dateRange Optional 2-item numeric vector giving the years to use; default is the full range of the data (1960-2013)
#'
#' @param species Which species should be extracted, with format genus_species (all lowercase); allowable values are: abies_balsamea acer_rubrum acer_saccharum betula_alleghaniensis betula_papyrifera fagus_grandifolia fraxinus_americana fraxinus_nigra larix_laricina picea_glauca picea_mariana picea_rubens pinus_banksiana pinus_resinosa pinus_strobus populus_grandidentata populous_tremuloides quercus_macrocarpa quercus_rubra thuja_occidentalis tsuga_canadensis
#'
#' @family occurrence
#'
#' @author Matt Talluto, \email{mtalluto@@gmail.com}
#'
#' @section Data type: presence/absence
#'
#' @section Version: 0.5
#'
#' @section Date submitted:  2016-06-16
NATrees <- function (dateRange = NULL, species = "acer_saccharum") 
{
    dat = read.csv(file = "https://ndownloader.figshare.com/files/5398307?private_link=374047f2ad50e45433e5")
    if (is.null(dateRange)) 
        dateRange = range(dat$year_measured)
    prDat = tryCatch(dat[dat$year_measured >= dateRange[1] & 
        dat$year_measured <= dateRange[2], c("longitude", "latitude", 
        species)], error = function(e) {
        stop(paste0("Error retrieving data for ", species, "\nIs the species name correct?"))
    })
    data.frame(longitude = prDat$longitude, latitude = prDat$latitude, 
        value = prDat[, species], type = ifelse(prDat[, species], 
            "presence", "absence"), fold = 1, stringsAsFactors = FALSE)
}
