#' @title Model module: BiomodModel
#'
#' @description Model module wrapper for the biomod2 function BIOMOD_Modeling()
#'
#' @details In order to fit a MaxEnt model, you must first download the
#' MaxEnt executable file \code{maxent.jar} and save it in the correct location.
#' The zoon function \code{GetMaxEnt} can orchestrate this for you.
#' Running MaxEnt also requires an up-to-date version of java
#' (which you may already have installed).
#'
#' @param .df \strong{Internal parameter, do not use in the workflow function}.
#' \code{.df} is data frame that combines the occurrence data and covariate
#'  data. \code{.df} is passed automatically in workflow from the process
#'  module(s) to the model module(s) and should not be passed by the user.
#'
#' @param modelType A character to describe models to use. Select from
#' 'GLM','GBM','GAM','CTA','ANN','SRE','FDA','MARS','RF','MAXENT' 
#'
#' @author ZOON Developers, \email{zoonproject@@gmail.com}
#' @section Version: 1.0
#' @section Date submitted: 2015-11-13
#' @section Data type: presence/background, presence/absence, abundance, proportion
#' @name BiomodModel
#' @family model
#' @seealso \code{\link{biomod2::BIOMOD_ModelingOptions}}


BiomodModel <- function(.df, modelType = 'GLM'){
  
  zoon:::GetPackage('biomod2')
  
  # Only one model type allowed
  if(length(modelType) > 1){
    stop(paste('In BiomodModel: Only one model type can be run at once. If you want to run more than one model type use "list()",',
               "for example list(BiomodModel(modelType = 'GLM'), BiomodModel(modelType = 'FDA'))"))
  }
  
  # If our response in an integer, convert to numeric
  if(class(.df$value) == 'integer') {
    .df$value <- as.numeric(.df$value)
  }
  
  coords <- data.frame(.df$lon, .df$lat)
  
  biomodData <- BIOMOD_FormatingData(resp.var = .df$value, 
                                     expl.var = .df[,6:NCOL(.df), drop = FALSE],
                                     resp.xy = coords, 
                                     resp.name = 'Species')
  
  myBiomodOptions <- BIOMOD_ModelingOptions(GLM = list(test = 'none'))
  
  id <- as.character(format(Sys.time(), '%s'))
  
  myBiomodModelOut <- BIOMOD_Modeling(biomodData,
                                      models = modelType,
                                      models.options = myBiomodOptions,
                                      NbRunEval = 1,
                                      DataSplit = 100,
                                      Prevalence = 0.5,
                                      VarImport = 0,
                                      SaveObj = TRUE,
                                      rescal.all.models = TRUE,
                                      do.full.models = FALSE,
                                      modeling.id = id,
                                      silent = TRUE)
  
  ZoonModel(model = myBiomodModelOut,
            code = {
              p <- BIOMOD_Projection(modeling.output = model,
                                     new.env = newdata,
                                     proj.name = 'current',
                                     selected.models = 'all',
                                     clamping.mask = FALSE,
                                     output.format = '.RData',
                                     silent = TRUE)
              p <- as.vector(get_predictions(p))
              # convert to 0-1 scale
              pmin(1, pmax(0, p / 1000))
            },
            packages = 'biomod2')
  
}
