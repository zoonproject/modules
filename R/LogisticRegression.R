#'Model module: LogisticRegression
#'
#'Model module to fit a simple logistic regression model
#'
#'@param .df \strong{Internal parameter, do not use in the workflow function}. \code{.df} is data frame that combines the occurrence data and covariate data. \code{.df} is passed automatically in workflow from the process module(s) to the model module(s) and should not be passed by the user.
#'
#'@seealso \code{\link{glm}}
#'
#'@name LogisticRegression


LogisticRegression <- function(.df){
  #if (!all(df$type %in% c('presence', 'absence', 'background'))) {
  #  stop ('only for presence/absence or presence/background data')
  #}
  
  covs <- as.data.frame(.df[, 6:ncol(.df)])
  names(covs) <- names(.df)[6:ncol(.df)]
  m <- glm(.df$value ~ .,
           data = covs,
           family = 'binomial')
  
  return (m)
}
