#'Model module: GBM
#'
#'Model module to fit a generalized boosted regression (aka boosted regression
#' trees) model
#' 
#'@param .df \strong{Internal parameter, do not use in the workflow function}.
#' \code{.df} is data frame that combines the occurrence
#'
#'@param max.trees The maximum number of trees to fit.
#' The number of trees is equivalent to the number of iterations and
#'  the number of basis functions in the additive expansion. The optimal
#'  number will be selected by cross-validation, but this sets an upper limit.
#'
#'@param interaction.depth The maximum depth of variable interactions.
#' 1 implies an additive model, 2 implies a model with up to 2-way interactions,
#'  etc.
#'
#'@param shrinkage a shrinkage parameter applied to each tree in the expansion.
#' Also known as the learning rate or step-size reduction.
#'
#'@param cv.folds Number of cross-validation folds to perform when selecting
#' the optimal number of trees and calculating an internal estimate of
#'  generalization error, returned in cv.error.
#'
#'@name GBM
GBM <-
  function (.df,
            max.trees = 1000,
            interaction.depth = 5,
            shrinkage = 0.001,
            cv.folds = 3) {
    
    zoon:::GetPackage('gbm')
    
    if (!all(.df$type %in% c('presence', 'absence', 'background'))) {
      stop ('only for presence/absence or presence/background data')
    }
    
    # get the covariates
    covs <- as.data.frame(.df[, 6:ncol(.df)])
    names(covs) <- names(.df)[6:ncol(.df)]
    
    # fit the model with the maximum number of trees
    m <- gbm::gbm(.df$value ~ .,
                  data = covs,
                  distribution = 'bernoulli',
                  n.trees = max.trees,
                  interaction.depth = interaction.depth,
                  shrinkage = shrinkage,
                  cv.folds = cv.folds)
    
    # get the optimum number of trees
    n.trees <- gbm.perf(m,
                        plot.it = FALSE,
                        method = 'cv')
    
    # set this in m
    m$n.trees <- n.trees
    
    # note this won't work without a bespoke prediction method
    # passed out from this function (using n.trees) since that
    # argument is required by predict.gbm
    ZoonModel(model = m,
              code = {
                gbm::predict.gbm(model,
                                  newdata,
                                  n.trees = model$n.trees,
                                  type = 'response')
              },
              packages = 'gbm')
  }