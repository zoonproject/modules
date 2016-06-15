#' @name SugarMaple
#'
#' @title Sugar Maple distribution in North America
#'
#' @description Presence/absence of Sugar Maple (\emph{Acer saccharum}) in North America. Each record corresponds to the centroid of a 0.5 degree cell.
#'
#' @details The extent of the occurrence data is \\code{c(-170, -20, 10, 80)}. These data were used as part of a wider analysis by Morin & Thuiller (2009), and were subsequently analysed and made available by Talluto \emph{et al.}(2016). \n Morin, X., & Thuiller, W. (2009). Comparing niche-and process-based models to reduce prediction uncertainty in species range shifts under climate change. Ecology, 90(5), 1301–1313. http://doi.org/10.1890/08-0134.1 \n Talluto, M. V., Boulangeat, I., Ameztegui, A., Aubin, I., Berteaux, D., Butler, A., … Gravel, D. (2016). Cross-scale integration of knowledge for predicting species ranges: A metamodelling framework. Global Ecology and Biogeography, 25(2), 238–249. http://doi.org/10.1111/geb.12395
#'
#' @family occurrence
#'
#' @author Matt Talluto & Nick Golding, \email{mtalluto@@gmail.com}
#'
#' @section Data type: presence/absence
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2016-06-15
SugarMaple <- function () 
{
    url <- "https://raw.githubusercontent.com/QUICC-FOR/Cross-Scale-Model-Integration-Examples/master/example_2/dat/raw/AceSac.csv"
    dat <- read.csv(url, stringsAsFactors = FALSE)
    occ <- data.frame(longitude = dat$long, latitude = dat$lat, 
        value = dat$PresObs, type = ifelse(dat$PresObs == 1, 
            "presence", "absence"), fold = 1, stringsAsFactors = FALSE)
    return(occ)
}
