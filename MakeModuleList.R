


FindModules <- function(dir = '~/Dropbox/zoon/modules'){
  wd <- getwd()
  setwd(dir)
  x <- system('git ls-tree --full-tree -r HEAD', intern=TRUE)

  short <- gsub(".*\t", '',  x) 

  modules <- short[grep('^R/', short)]
  module.names <- gsub('^R/', '', modules)
  module.names <- gsub('.R$', '', module.names)
  setwd(wd)
  return(module.names)
}


WriteModules <- function(dir = '~/Dropbox/zoon/modules'){
  x <- FindModules(dir)
  write.table(x, file = paste0(dir, '/ModuleList.csv'), row.names = FALSE, sep = ',', col.names = 'Module names')
}


