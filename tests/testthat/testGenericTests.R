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
# Modules should only be ignored here if:
# 1) They take a LONG time (>1min)
# 2) They require 3rd party software
# 3) They fail due to 3rd party server issues
# They should ofcourse be tested locally by commenting the
# line below
ignoreModules <- 'ModulesDocumentation.R'
ignoreModules <- c(ignoreModules, 'Bioclim.R', 'UKBioclim.R', 'NBNdataByName.R',
                   'OptGRaF.R', 'BiomodModel.R', 'NATrees.R', 'MaxEnt.R', 'Bioclim_future.R')
modulePaths <- modulePaths[!basename(modulePaths) %in% ignoreModules][1:5]

capture.output({

  # loop through each module
  for(modulePath in modulePaths){
    
    zoon:::test_module(modulePath) 

  }
  
}, file = 'tests.txt', split = TRUE)