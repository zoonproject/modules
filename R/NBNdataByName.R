#' @name NBNdataByName
#'
#' @title NBN Data by Common Name
#'
#' @description This module supplies occurrence data from the UK NBN gateway. To get access to data you must first register at https://data.nbn.org.uk/User/Register. You will need your username and password when running this module. You can specify the data to retrieve by dataset, species, time, location and/or group.
#'
#' @details For more detials see the rnbn package
#'
#' @param species The latin name of a species to search for, or a vector of such names.
#'
#' @param username your NBN username
#'
#' @param password your NBN password
#'
#' @param datasets a list of dataset keys which are strings of 8 alphanumeric characters. Look up datasets here: https://data.nbn.org.uk/Datasets
#'
#' @param startYear a 4 digit integer year
#'
#' @param endYear a 4 digit integer year
#'
#' @param VC a string giving a vice-county name (see listVCs in the rnbn package)
#'
#' @param group a string giving the name of a group (see listGroups in the rnbn package). Using group will retireve data for all TVKs in this group. for example using group "reptile" will search using over 150 TVKs including TVKs for higher taxonomic groups such families within reptiles. Therefore it may be preferrable to search using a vector of species
#'
#' @param gridRef a string giving a UK gridreference in which to search for occurrences
#'
#' @param acceptTandC if set to TRUE you accept the NBN gateway terms and conditions and privacy policy. These can be found at https://data.nbn.org.uk/Terms. Accepting the terms and conditions supresses the corresponding warning message.
#'
#' @param silent if TRUE information returned to console is reduced
#'
#' @param attributes if FALSE then attribute data is not returned, this may improve the speed of large requests.
#'
#' @family occurrence
#'
#' @author Tom August, \email{tomaug@@ceh.ac.uk}
#'
#' @section Data type: presence-only
#'
#' @section Version: 0
#'
#' @section Date submitted:  2016-06-16
NBNdataByName <- function (species = "Myotis daubentonii", username = "tom_test", 
    password = "HelloWorld", datasets = NULL, startYear = 2000, 
    endYear = 2000, VC = NULL, group = NULL, gridRef = NULL, 
    acceptTandC = FALSE, silent = FALSE, attributes = FALSE) 
{
    zoon:::GetPackage("rnbn")
    nbnLogin(username = username, password = password)
    rawSp <- unlist(lapply(species, FUN = function(x) getTVKQuery(query = x, 
        top = TRUE)$taxonVersionKey))
    if (!silent) 
        cat(paste("Searching for TVKs", paste(rawSp, collapse = ", "), 
            "\n"))
    rawOC <- getOccurrences(tvks = rawSp, datasets = datasets, 
        startYear = startYear, endYear = endYear, VC = VC, group = group, 
        gridRef = gridRef, latLong = TRUE, acceptTandC = acceptTandC, 
        silent = silent, attributes = attributes)
    rawOC$value <- 1
    rawOC$value[rawOC$absence == "TRUE"] <- 0
    rawOC$type[rawOC$absence == "TRUE"] <- "absence"
    rawOC$type[!rawOC$absence == "TRUE"] <- "presence"
    occurrence <- rawOC[, c("longitude", "latitude", "value", 
        "type")]
    occurrence$fold <- 1
    attr(occurrence, "NBNDataProviders") <- attr(rawOC, "providers")
    attr(occurrence, "NBNTVKs") <- rawSp
    return(occurrence)
}
