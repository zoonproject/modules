#'Model module: LASSO
#'
#'Model module to fit LASSO or ridge regression.
#'
#'@param alpha Elasticnet mixing parameter. 1 (default) is LASSO. 0 is ridge regression.
#'
#'@seealso \code{\link{glmnet}}
#'
#'@name LASSO

LASSO <- function(df, alpha=1){

  GetPackage("glmnet")
  
  covs <- as.data.frame(df[, 6:ncol(df)])
  names(covs) <- names(df)[6:ncol(df)]
  m <- gam::gam(formula = df$value ~ .,
         data = covs,
         family = binomial, 
         alpha=alpha)

# Output a model object. The object class must have a predict method available.
# If it doesn't, define one here (see BiomodModel at link below for example)
# https://github.com/zoonproject/modules/blob/master/R/BiomodModel.R
  return (m)
}




