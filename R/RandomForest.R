#'Model module: RandomForest
#'
#'Model module to fit a simple RandomForest classification model
#'
#'@return A model object with a valid predict method
#'
#'@name RandomForest



RandomForest <- function (df) {
  
  zoon:::GetPackage(randomForest)
  
  #if (!all(df$type %in% c('presence', 'absence', 'background'))) {
  #  stop ('only for presence/absence or presence/background data')
  #}
  
  covs <- as.data.frame(df[, 6:ncol(df)])
  names(covs) <- names(df)[6:ncol(df)]

  # set the response to a factor to make sure it's regression not classification
  df$value <- factor(df$value)

  m <- randomForest(df$value ~ .,
                    data = covs,
                    weights = rep(1, nrow(covs)),
                    size = 1)

  return (m)
}

