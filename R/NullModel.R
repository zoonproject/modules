#' @name NullModel
#'
#' @title Null Model
#'
#' @description A simple null model for predicting species distributions. Predictions are simply the mean of the occurrence values column. This only make sense for some data combination, see details.
#'
#' @details The predictions can be interpreted as the landscape-level prevalence (for presence & absence), a metric of suitability (for presence & background), a mean abundance (for abundance) or a site-level prevalence (for proportion). The module will error if the occurrence data types are not one of these types
#'
#' @param .df \strong{Internal parameter, do not use in the workflow function}. \code{.df} is data frame that combines the occurrence data and covariate data. \code{.df} is passed automatically in workflow from the process module(s) to the model module(s) and should not be passed by the user.
#'
#' @family model
#'
#' @author Nick Golding, \email{nick.golding.research@@gmail.com}
#'
#' @section Data type: presence/absence, presence/background, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-05-03
NullModel <- function(.df){
  
  # list the types of occurrence data present
  types <- sort(unique(as.character(.df$type)))
  values <- .df$value
  
  # calculating the null prediction as the mean of values only makes sense for
  # these types of model, so error if users do something different
  options <- list(c('absence', 'presence'),
                  c('background', 'presence'),
                  c('proportion'),
                  c('abundance'))
  
  matches <- vapply(options, identical, FUN.VALUE = FALSE, types)
  if (!any(matches)) {
    
    stop ("Occurrence data had types: ", paste(types, collapse = ' & '),
          " but NullModel can only calculate the null value when occurrence ",
          "data types are all: presence & absence; presence & background; ",
          "abundance; or proportion.")
    
  }
  
  pred <- mean(values)
  
  # create a ZoonModel object and return it
  ZoonModel(model = pred,
            code = {
              n <- nrow(newdata)
              p <- rep(model, n)
              
              # find missing values
              newdata_clean <- na.omit(newdata)
              na_idx <- attr(newdata_clean, 'na.action')
              if (!is.null(na_idx))
                p[na_idx] <- NA
              
              p
              
            },
            packages = 'zoon')
  
}
