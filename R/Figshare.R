#' @title Output module: Figshare
#'
#' @description Upload your workflow call, model, occurrance and covariate data to figshare as a RData file. Uploads privately but be aware I cannot assure the privacy of any data. This module is a work in progress, and will obviously want to upload the output from the output modules.
#'
#'@param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.#'
#'
#'@param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#'@param title String giving the title 
#'
#'@param description  String describing the workflow 
#'
#'@param authors Charactor vector of full authors names 
#'
#'@param categories Character vector of figshare categories e.g. ecology. 
#'
#'@param tags Character vector of searchable tags. 
#'
#'@author ZOON Developers, \email{zoonproject@@gmail.com}
#'
#'@name Figshare
#'@family output
Figshare <-
function (.model, .ras, title, description='zoon workflow', authors='zoon', categories='SDM', tags='zoon'){

  zoon:::GetPackage(rfigshare)

  bits <- sys.call(1)

  call <- paste0(bits[1],

                 '(',

                 paste(bits[-1],

                       collapse = ', '),

                 ')')


  save(call, .model, .ras, file = 'zoonTMPfigshare.RData')
  

  id <- fs_new_article(title = title, description = description, 

                       type = "dataset", authors = authors, tags = tags, categories = 'ecology',

                       files = "zoonTMPfigshare.RData", visibility = "private")

  

  file.remove('zoonTMPfigshare.RData')  


  return(NULL)

}
