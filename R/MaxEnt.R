#'Model module: MaxEnt
#'
#'Model module to fit MaxEnt model via the dismo package
#'
#'@details \strong{In order to fit a MaxEnt model, you must first download the
#'MaxEnt executable file \code{maxent.jar} from:
#'\url{http://www.cs.princeton.edu/~schapire/maxent/} and save it under the
#' java directory in the dismo package in your R package library. You can find
#' the correct location with this command:
#' \code{paste0(system.file(package = 'dismo'), '/java/')}}.
#' Running MaxEnt also requires an up-to-date version of java and the java
#' development kit (which you may already have installed).
#'
#'@param .df \strong{Internal parameter, do not use in the workflow function}.
#' \code{.df} is data frame that combines the occurrence data and covariate
#' data. \code{.df} is passed automatically in workflow from the process
#' module(s) to the model module(s) and should not be passed by the user.
#'
#'@seealso \code{\link{dismo::maxent}}
#'
#'@name MaxEnt
MaxEnt <- function(.df){
  
  zoon:::GetPackage('dismo')
  
  covs <- as.data.frame(.df[, 6:ncol(.df)])
  names(covs) <- names(.df)[6:ncol(.df)]
  
  m <- maxent(x = covs,
              p = .df$value)
  
  # create a ZoonModel object and return it
  ZoonModel(model = m,
            code = {
              # create empty vector
              p <- rep(NA, nrow(newdata))
              # omit NAs in new data
              newdata_clean <- na.omit(newdata)
              # get their indices
              na_idx <- attr(newdata_clean, 'na.action')
              p[-na_idx] <- dismo::predict(model,
                             newdata_clean,
                             type = 'response')
              return (p)
            },
            packages = 'dismo')
}
