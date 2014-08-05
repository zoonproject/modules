
#' model module to fit a simple RandomForest classification model
#'
#'@param df A dataframe, the output from a process module
#'
#'@return A model object with a valid predict method
#'
#'@name modelRF



modelRF <- function (df) {
  
  require ('randomForest')
  
  if (!all(df$type %in% c('presence', 'absence', 'background'))) {
    stop ('only for presence/absence or presence/background data')
  }
  
  covs <- as.data.frame(df[, 5:ncol(df)])
  names(covs) <- names(df)[5:ncol(df)]
  m <- randomForest(df$value ~ .,
                    data = covs,
                    weights = rep(1, nrow(covs)),
                    size = 1)
  
  return (m)
}

