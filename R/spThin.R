#' @name spThin
#'
#' @title Spatial thinning of Presence-only Data
#'
#' @description Apply the stochastic spatial thinning algorithm implemented in the spThin package to presence data in a presence-background dataset
#'
#' @details Full details of the algorithm are available in the open-access article by Aiello-Lammens et al. (2015): dx.doi.org/10.1111/ecog.01132
#'
#' @param .data \strong{Internal parameter, do not use in the workflow function}. \code{.data} is a list of a data frame and a raster object returned from occurrence modules and covariate modules respectively. \code{.data} is passed automatically in workflow from the occurrence and covariate modules to the process module(s) and should not be passed by the user.
#'
#' @param thin Thinning parameter - the required minimum distance (in kilometres) between points after applying the thinning procedure
#'
#' @family process
#'
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#'
#' @section Data type: presence-only
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-11-23
spThin <- function (.data, thin = 50) {
  # check these are presence-background data
  stopifnot(all(.data$df$type %in% c('presence', 'background')))
  # install & load the package
  zoon::GetPackage('spThin')
  # get dataframe & index to presence data
  df <- na.omit(.data$df)
  pres_idx <- which(df$type == 'presence')
  # prepare presence data subset and apply thinning
  sub_df <- data.frame(LAT = df$latitude[pres_idx],
                       LONG = df$longitude[pres_idx],
                       SPEC = NA)
  th <- thin(loc.data = sub_df,
             thin.par = thin,
             reps = 1,
             locs.thinned.list.return = TRUE,
             write.files = FALSE,
             write.log.file = FALSE)
  # get index to rows in sub_df, update the full dataset and return
  pres_keep_idx <- as.numeric(rownames(th[[1]]))
  .data$df <- rbind(df[pres_idx,][pres_keep_idx, ],
                    df[-pres_idx, ])
  return (.data)
}
