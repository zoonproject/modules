### Test Model modules ###
context('Testing for Model modules')

if (!capabilities('libcurl')) skip('skipping as libcurl not supported')  

library(zoon, quietly = TRUE)

# Get a list of Model modules
mod_modules <- GetModuleList()$model

baseURL <- 'https://raw.githubusercontent.com/zoonproject/modules/master/R/'

library(roxygen2)
library(RCurl)

# loop through each module
for(moduleURL in paste(baseURL, mod_modules, '.R', sep = '')){
  
  roxy_parse <- roxygen2:::parse_file(moduleURL, environment())[[1]]
  
  test_that(paste('Check arguements -', basename(gsub('.R', '', moduleURL))), {
    
    expect_equal(roxy_parse$family, 'model')
    
  })
  
}