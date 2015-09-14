#'Model module: MaxEnt
#'
#'Model module to fit MaxEnt model via he dismo package
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
#' @param args Additional arguments to pass to MaxEnt to modify model fitting,
#' in the form of flags to the java executable.
#' Unfortunately these are a bit cryptic and the maxent
#' help files, which list all of the options, aren't available on the web.
#' You could try the MaxEnt graphical user interface, otherwise there is a list
#' (which may or may not be up-to-date) in this forum:
#' \url{https://groups.google.com/d/msg/maxent/yRBlvZ1_9rQ/Fj8Two0lmHIJ}.
#'
#'@seealso \code{\link{dismo::maxent}}
#'
#'@name MaxEnt
#'@family model
MaxEnt <- function(.df, args = ''){
  
  zoon:::GetPackage('dismo')
  zoon:::GetPackage('rJava')
  
  covs <- as.data.frame(.df[, 6:ncol(.df)])
  names(covs) <- names(.df)[6:ncol(.df)]
  
  m <- maxent(x = covs,
              p = .df$value,
              args = args)
  
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
