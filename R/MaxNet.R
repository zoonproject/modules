#'@title Model module: MaxNet
#'  
#'@description Model module to fit MaxEnt models using the maxnet package
#'  
#'@details The maxnet R package fits MaxEnt models using the glmnet package, 
#'  which enables efficient fitting of glms with regularization. Unlike MaxEnt, 
#'  MaxNet does not require the user to download and install the MaxEnt java 
#'  executable.
#'  
#'  \code{features} should be a string including an 'l' for linear features, 'q'
#'  for quadratic features, 'h' for hinge features, 'p' for pairwise 
#'  interactions and 't' for threshold features. E.g. to use only linear 
#'  features and their interactions, either \code{features = 'lp'} or 
#'  \code{features = 'pl'} would work. the default value of \code{'default'} 
#'  uses maxnet's default settings, adjusting the set up based on the number of 
#'  occurrence records \emph{np}: 'l' if np < 10; 'lq' if np < 15; 'lqh' if np <
#'  80; or 'lqph' if np >= 80. I.e. the default never uses threshold features.
#'  
#'  \code{prediction_type} corresponds to the \code{type} argument in the 
#'  \code{maxnet} predict function. \code{maxnet} enables types: 'link', 
#'  'exponential', 'cloglog' and 'logistic'. However most output modules expect 
#'  predictions to be made on the probability scale, for which only 'cloglog'
#'  and 'logistic' are guaranteed to work.
#'  
#'@param .df \strong{Internal parameter, do not use in the workflow function}. 
#'  \code{.df} is data frame that combines the occurrence data and covariate 
#'  data. \code{.df} is passed automatically in workflow from the process 
#'  module(s) to the model module(s) and should not be passed by the user.
#'  
#'@param features A length-one character vector (i.e. a string) defining the 
#'  types of features to use for all covariates (see details).
#'  
#'@param regmult A positive scalar giving the multiplier for the degree of 
#'  regularisation. Higher values mean more regularisation.
#'  
#'@param clamp_predictions Whether to clamp predictions when extrapolating.
#'  
#'@param prediction_type The scale on which to make predictions (see details for
#'  options).
#'  
#'@seealso \code{\link{maxnet::maxnet}}
#'  
#'@author Nick Golding, \email{nick.golding.research@@gmail.com}
#'@section Version: 1.0
#'@section Date submitted: 2016-12-21
#'@section Data type: presence/background
#'  
#'@name MaxNet
#'@family model
MaxNet <- function (.df,
                    features = 'default',
                    regmult = 1,
                    clamp_predictions = TRUE,
                    prediction_type = 'logistic') {
  
  zoon::GetPackage('maxnet')
  
  # get teh covariate values
  covs <- as.data.frame(.df[, attr(.df, 'covCols')])
  names(covs) <- attr(.df, 'covCols')
  
  # check the features argument is valid
  if (!(is.character(features) && is.vector(features) && length(features == 1)))
    stop ("'features' must be a length 1 character vector, see ModuleHelp('maxnet')")
  
  # create the formula (which features to use)
  # l = linear, q = quadratic, h = hinge, t = thresholds, p = pairwise (product)
  # interactions
  f <- maxnet::maxnet.formula(p = .df$value,
                              data = covs,
                              classes = features)
  
  # fit the model  
  m <- maxnet::maxnet(p = .df$value,
                      data = covs,
                      f = f,
                      regmult = regmult)
  
  # add predict arguments to m
  m$predict_args <- list(clamp = clamp_predictions,
                         type = prediction_type)
  
  # create a ZoonModel object and return it
  ZoonModel(model = m,
            code = {
              # create empty vector
              p <- rep(NA, nrow(newdata))
              # omit NAs in new data
              newdata_clean <- na.omit(newdata)
              # get their indices
              na_idx <- attr(newdata_clean, 'na.action')
              if (is.null(na_idx)) {
                idx <- 1:nrow(newdata)
              } else {
                idx <- -na_idx
              }
              p[idx] <- predict(model,
                                newdata_clean,
                                clamp = model$predict_args$clamp,
                                type = model$predict_args$type)
              return (p)
            },
            packages = 'maxnet')
}