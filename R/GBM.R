#' @title Model module: GBM
#'
#' @description Model module to fit a generalized boosted regression (aka boosted regression
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
#'@author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#'@name GBM
#'@family model
GBM <-
  function (.df,
            max.trees = 1000,
            interaction.depth = 5,
            shrinkage = 0.001) {
    
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
                  shrinkage = shrinkage)
    
    # get the optimum number of trees
    n.trees <- gbm.perf(m,
                        plot.it = FALSE,
                        method = 'OOB')
    
    # set this in m
    m$n.trees <- n.trees
    
    # note this won't work without a bespoke prediction method
    # passed out from this function (using n.trees) since that
    # argument is required by predict.gbm
    ZoonModel(model = m,
              code = {
                # create empty vector
                p <- rep(NA, nrow(newdata))
                # omit NAs in new data
                newdata_clean <- na.omit(newdata)
                # get their indices
                na_idx <- attr(newdata_clean, 'na.action')
                if (is.null(na_idx)) idx <- 1:nrow(newdata)
		else idx <- -na_idx
		p[idx] <- gbm::predict.gbm(model,
                                               newdata_clean,
                                               n.trees = model$n.trees,
                                               type = 'response')
                return (p)
              },
              packages = 'gbm')
}
