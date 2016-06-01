### General module tests ###
library(zoon, quietly = TRUE)
library(roxygen2)

# Get our list of modules
modulePaths <- list.files('../../R', pattern = '.R$', full.names = TRUE)

# A vector of R scripts to ignore
ignoreModules <- c('ModulesDocumentation.R')#, 'NCEP.R', 'AirNCEP.R', 'Bioclim.R', 'UKBioclim.R')
modulePaths <- modulePaths[!basename(modulePaths) %in% ignoreModules]

capture.output({

  # loop through each module
  for(modulePath in modulePaths){
    
    zoon:::test_module(modulePath) 

  }
  
}, file = 'tests.txt', split = TRUE)