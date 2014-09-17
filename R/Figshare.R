#'Output module: Figshare
#'
#'Upload your workflow call, model, occurrance and covariate data to figshare as a RData file. Uploads privately but be aware I cannot assure the privacy of any data. This module is a work in progress, and will obviously want to upload the output from the output modules.
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
#'@name Figshare
Figshare <-
function (model, ras, title, description='zoon workflow', authors='zoon', categories='SDM', tags='zoon'){

  GetPackage(rfigshare)

  


  bits <- sys.call(1)

  call <- paste0(bits[1],

                 '(',

                 paste(bits[-1],

                       collapse = ', '),

                 ')')


  save(call, model, ras, file = 'zoonTMPfigshare.RData')
  

  id <- fs_new_article(title = title, description = description, 

                       type = "dataset", authors = authors, tags = tags, categories = 'ecology',

                       files = "zoonTMPfigshare.RData", visibility = "private")

  

  file.remove('zoonTMPfigshare.RData')  


  return(NULL)

}
