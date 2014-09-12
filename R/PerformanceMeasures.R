#'Output module: PerformanceMeasures
#'
#'Calculate a suite of performance metrics on either crossvalidation or external validation data
#'
#'@param threshold A chosen threshold value for measures that need 0/1 predictions 
#'
#'@name PerformanceMeasures
PerformanceMeasures <-
function(model, ras, threshold=0.5){

  zoon:::GetPackage(SDMTools)

  if (all(model$data$predictions %in% c(0,1))){
    warning('The model has predicted presence/absence rather than probabilities. Some measures may not work')
  }

  if (all(model$data$fold == 1)){
    stop('You have no validation data to apply PerformanceMeasures module to')
  }

  if (all(model$data$fold != 0)){
    
    confusion <- SDMTools::confusion.matrix(model$data$value, model$data$predictions)

    performance <- list(
      auc = SDMTools::auc(model$data$value, model$data$predictions),
      kappa = Kappa(confusion),
      omissions = omission(confusion),
      sensitivity = sensitivity(confusion),
      specificity = specificity(confusion),
      proportionCorrect = prop.correct(confusion)
    )
  } else if (all(model$data$fold %in% c(0,1))){

    data <- model$data[model$data$fold == 0,]
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

  return(performance)

}
