### General module tests ###
library(zoon, quietly = TRUE)
library(roxygen2)
source('test_parameters.R')
source('test_module.R')
source('test_Outputs.R')

# Get our list of modules
modulePaths <- list.files('../../R', pattern = '.R$', full.names = TRUE)

# A vector of R scripts to ignore
ignoreModules <- c('ModulesDocumentation.R')#, 'NCEP.R', 'AirNCEP.R')
modulePaths <- modulePaths[!basename(modulePaths) %in% ignoreModules]

# loop through each module
for(modulePath in modulePaths){
  
  context(paste('Testing module', basename(gsub('.R$', '', modulePath))))
  
  time <- system.time({
  test_module(modulePath) 
  })
  
  expect_true(time['elapsed'] < 30,
              info = 'Module tests should not take longer than 30 seconds, please change your defualt values so that test workflow runs do not take too long')
}