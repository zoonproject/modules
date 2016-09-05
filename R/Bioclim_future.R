#' @title Covariate module: Bioclim_future
#'
#' @description Get worldclim data for the future (CMIP5). Downloads then stores locally.
#'
#' @param extent Length 4 numeric vector giving min longitude, max longitude, min latitude, max latitude.
#'
#' @param resolution Resolution in minutes. Must be one of 2.5, 5 or 10. Default is 10.
#'
#' @param layers which bioclim layers to obtain, a vector of integers between 1 and 19. See \url{http://www.worldclim.org/bioclim}.
#'
#' @param rcp numeric. Representative Concentration Pathways, or greenhouse gases concentration trajectories (see \url{https://en.wikipedia.org/wiki/Representative_Concentration_Pathways}. Should be one of 26, 45, 60, or 85.
#'
#' @param model character. General Circulation Model. Should be one of "AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", or "NO".
#'
#' @param year numeric. Time period. Should be either 50 (which represents the period 2041-2060) or 70 (which represents the period 2061-2080).
#'
#' @seealso \url{http://www.worldclim.org} and \code{\link[raster]{getData}}.
#'
#'
#' @author F. Rodriguez-Sanchez & E. Van Loon, \email{zoonproject@@gmail.com}
#' @section Version: 1.0.0
#' @section Date submitted: 2016-06-15
#'
#' @name Bioclim_future
#' @family covariate
Bioclim_future <-
  function(extent = c(-10, 10, 45, 65), resolution = 10, layers = 1:19, rcp = 45, model = 'AC', year = 70) {

    if(!(resolution %in% c(2.5, 5, 10))){
      stop('only 2.5, 5 and 10 degree resolutions are supported currently')
    }

    if (length(layers) < 1 |
        length(layers) > 19 |
        !all(layers %in% 1:19)) {
      stop ('layers must be a vector of integers between 1 and 19 indicating layers to obtain')
    }

    stopifnot(length(extent) == 4)
    stopifnot(all(is.numeric(extent)))

    world <- raster::getData('CMIP5', var = 'bio', res = resolution, rcp = rcp, model = model, year = year)
    world <- world[[layers]]
    cropped <- crop(world, extent(extent))

    # need to rename layer names in order to be able to make predictions later
    # (layer names must be the same as present bioclim layers: bio1, bio2, ... bio19)
    names(cropped) <- paste0('bio',
                             as.numeric(substr(names(cropped), nchar(names(cropped))-1, nchar(names(cropped)))))

    return (cropped)



  }
