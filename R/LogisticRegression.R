#'Model module: LogisticRegression
#'
#'Model module to fit a simple logistic regression model
#'
#'@seealso \code{\link{glm}}
#'
#'@name LogisticRegression


LogisticRegression <- function(df){
  #if (!all(df$type %in% c('presence', 'absence', 'background'))) {
  #  stop ('only for presence/absence or presence/background data')
  #}
  
  covs <- as.data.frame(df[, 6:ncol(df)])
  names(covs) <- names(df)[6:ncol(df)]
  m <- glm(df$value ~ .,
           data = covs,
           family = 'binomial')
  
  return (m)
}
