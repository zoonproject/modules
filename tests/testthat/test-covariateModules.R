### Test Covariate modules ###
context('Testing for Covariate modules')

if (!capabilities('libcurl')) skip('skipping as libcurl not supported')  

library(zoon, quietly = TRUE)

# Get a list of Covariate modules
cov_modules <- GetModuleList()$covariate

baseURL <- 'https://raw.githubusercontent.com/zoonproject/modules/master/R/'

library(roxygen2)
library(RCurl)

# loop through each module
for(moduleURL in paste(baseURL, cov_modules, '.R', sep = '')){
  
  roxy_parse <- roxygen2:::parse_file(moduleURL, environment())[[1]]
  
  test_that(paste('Check arguements -', basename(gsub('.R', '', moduleURL))), {
    
    expect_equal(roxy_parse$family, 'covariate')
    
  })
  
}