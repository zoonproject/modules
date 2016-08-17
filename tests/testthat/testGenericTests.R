### General module tests ###
library(zoon, quietly = TRUE)
library(roxygen2)
if(grepl("ZoonModules.Rcheck", getwd())) setwd('../../00_pkg_src/ZoonModules/tests/testthat')
# Get our list of modules
if(basename(getwd()) == 'tests'){
  modulePaths <- list.files('../R', pattern = '.R$', full.names = TRUE)
} else if(basename(getwd()) == 'testthat'){
  modulePaths <- list.files('../../R', pattern = '.R$', full.names = TRUE)
} else {
  stop(paste('executing tests from', getwd()))
}

if(length(modulePaths) == 0) stop(paste('No modules found for testing\n',
                                        'Current path:', getwd(), '\n\n',
                                        'Dir above:', paste(dir('..'), collapse = ', '),
                                        '\n\nDir 2 above:', paste(dir('../..'), collapse = ', ')))

# A vector of R scripts to ignore
ignoreModules <- c('ModulesDocumentation.R')#, 'NCEP.R', 'AirNCEP.R', 'Bioclim.R', 'UKBioclim.R')
modulePaths <- modulePaths[!basename(modulePaths) %in% ignoreModules]

capture.output({

  # loop through each module
  for(modulePath in modulePaths){
    
    zoon:::test_module(modulePath) 

  }
  
}, file = 'tests.txt', split = TRUE)