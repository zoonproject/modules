#' @name VariableImportance
#'
#' @title Variable Importance
#'
#' @description This module outputs a simple report of the coefficents/importance measures from the model
#'
#' @details This module identifies the class of the model object and if it has a known return method for this class it will be applied. If the class is not known it will try the default which is to return the summary of the object.
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param print If true then the data are returned to console as well as outputted
#'
#' @family output
#'
#' @author Tom August, \email{tomaug@@ceh.ac.uk}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2016-06-15
VariableImportance <- function (.model, .ras, print = TRUE) 
{
    modObj <- .model$model$model
    class_modelObj <- class(modObj)
    if (length(class_modelObj) > 1) {
        message("ImportanceValues:  Class of model object (from model module) has length > 1, using first value")
        class_modelObj <- class_modelObj[1]
    }
    if (!inherits(class_modelObj, "character")) 
        stop("ImportanceValues: model class is not a character")
    switch(class_modelObj, train = {
        zoon::GetPackage("caret")
        sum_modObj <- caret::varImp(modObj)
        if (print == TRUE) cat("Class train, importance values:\n\n", 
            sum_modObj)
        return(sum_modObj)
    }, glm = {
        sum_modObj <- modObj$coefficients
        if (print == TRUE) cat("Class glm, coefficients:\n\n", 
            sum_modObj)
        return(sum_modObj)
    }, graf = {
        sum_modObj <- modObj$ls
        names(sum_modObj) <- names(modObj$obsx)
        if (print == TRUE) cat("Class graf, lapse values:\n\n", 
            sum_modObj)
        return(sum_modObj)
    }, {
        cat("I dont know how to work with class", class_modelObj, 
            "\nI will try to return a summary\n\n")
        if (print == TRUE) print(modObj)
        return(summary(modObj))
    })
}
