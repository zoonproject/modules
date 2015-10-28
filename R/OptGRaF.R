#' @title Model module: OptGRaF
#'
#' @description Model module to fit a (slow) GRaF model, with parameter optimisation.
#'
#'@param .df \strong{Internal parameter, do not use in the workflow function}. \code{.df} is data frame that combines the occurrence data and covariate data. \code{.df} is passed automatically in workflow from the process module(s) to the model module(s) and should not be passed by the user.
#'
#'@author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#'@name OptGRaF
#'@family model

OptGRaF <-
  function (.df) {
    
    zoon:::GetPackage('GRaF')
    
    if (!all(.df$type %in% c('presence', 'absence', 'background'))) {
      stop ('only for presence/absence or presence/background data')
    }
    
    covs <- as.data.frame(.df[, 6:ncol(.df)])
    names(covs) <- names(.df)[6:ncol(.df)]
    m <- graf(.df$value,
              covs,
              opt.l = TRUE)
    
    # create a ZoonModel object and return it
    ZoonModel(model = m,
              code = {
                GRaF::predict.graf(model,
                                  newdata,
                                  type = 'response')[, 1]
              },
              packages = 'GRaF')
  }
