#' @name AUC
#'
#' @title Plot estimated coefficients from LogisticRegression
#'
#' @description Returns AUC value for SDM
#'
#' @details Returns the the Area Under the Curve of the Receiver operating characteristic using a Mann-Whitney U statistic
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @family output
#'
#' @author Liz Martin, \email{emartin@@student.unimelb.edu.au}
#'
#' @section Data type: presence/absence, presence/background
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-11-22
AUC <- function(.model, .ras){

  zoon:::GetPackage('SDMTools')

  if (all(.model$data$predictions %in% c(0,1))){
    warning('The model has predicted presence/absence rather than probabilities. Some measures may not work')
  }

  if (all(.model$data$fold == 1)){ #if no cross-validation folds

    warning('You have no cross-validation folds, AUC may be misleading')

    # make predictions for the model
    covs <- .model$data[, 7:NCOL(.model$data), drop = FALSE]
    
    p <- ZoonPredict(zoonModel = .model$model,
                     newdata = covs)
    
    auc = SDMTools::auc(.model$data$value, p)
    
  } else if (all(.model$data$fold >= 1)){ # if more than one cross-validation fold

    auc = SDMTools::auc(.model$data$value, .model$data$predictions)

  } else if (all(.model$data$fold %in% c(0,1))){ # if internal or external cross-validation 

    auc = SDMTools::auc(.model$data$value, .model$data$predictions)

  }
  
#  message('Model performance measures:')
#  for(i in 1:length(performance)) {
#    line <- paste0(names(performance)[i],
#                   ' :  ',
#                   performance[i])
#    message(line)
#  }
#  message(' ')

  print(auc)

}
