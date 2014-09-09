
#'Model module to fit a simple logistic regression model
#'
#'@param df A dataframe, the output from a process module
#'
#'@return A model object with a valid predict method
#'
#'@name LogisticRegression


LogisticRegression <- function(df){
  if (!all(df$type %in% c('presence', 'absence', 'background'))) {
    stop ('only for presence/absence or presence/background data')
  }
  
  covs <- as.data.frame(df[, 5:ncol(df)])
  names(covs) <- names(df)[5:ncol(df)]
  m <- glm(df$value ~ .,
           data = covs,
           family = binomial)
  
  return (m)
}
