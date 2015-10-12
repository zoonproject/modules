#'Output module: PerformanceMeasures
#'
#'Calculate a suite of performance metrics on either crossvalidation, external validation data or (at your own risk) in-sample validation.
#'
#'@param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#'@param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#'@param threshold A chosen threshold value for measures that need 0/1 predictions 
#'
#'@name PerformanceMeasures
#'@family output
PerformanceMeasures <-
function(.model, .ras, threshold = 0.5){

  zoon:::GetPackage(SDMTools)

  if (all(.model$data$predictions %in% c(0,1))){
    warning('The model has predicted presence/absence rather than probabilities. Some measures may not work')
  }

  if (all(.model$data$fold == 1)){
    warning('You have no cross-validation folds, validation statistics may be misleading')

    # make predictions for the model
    covs <- .model$data[, 7:NCOL(.model$data), drop = FALSE]
    p <- ZoonPredict(zoonModel = .model$model,
                     newdata = covs)
    
    confusion <- SDMTools::confusion.matrix(.model$data$value,
                                            p,
                                            threshold)
    
    performance <- list(
      auc = SDMTools::auc(.model$data$value, p),
      kappa = Kappa(confusion),
      omissions = omission(confusion),
      sensitivity = sensitivity(confusion),
      specificity = specificity(confusion),
      proportionCorrect = prop.correct(confusion)
    )
  } else if (all(.model$data$fold >= 1)){
    
    confusion <- SDMTools::confusion.matrix(.model$data$value, .model$data$predictions)

    performance <- list(
      auc = SDMTools::auc(.model$data$value, .model$data$predictions),
      kappa = Kappa(confusion),
      omissions = omission(confusion),
      sensitivity = sensitivity(confusion),
      specificity = specificity(confusion),
      proportionCorrect = prop.correct(confusion)
    )
    
  } else if (all(.model$data$fold %in% c(0,1))){

    data <- .model$data[.model$data$fold == 0,]
    confusion <- SDMTools::confusion.matrix(data$value, data$predictions, threshold)

    performance <- list(
      auc = SDMTools::auc(data$value, data$predictions),
      kappa = Kappa(confusion),
      omissions = omission(confusion),
      sensitivity = sensitivity(confusion),
      specificity = specificity(confusion),
      proportionCorrect = prop.correct(confusion)
    )
  }
  
  message('Model performance measures:')
  for(i in 1:length(performance)) {
    line <- paste0(names(performance)[i],
                   ' :  ',
                   performance[i])
    message(line)
  }
  message(' ')

  return (performance)

}
