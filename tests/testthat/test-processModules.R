### Test Process modules ###
context('Testing for Process modules')

library(zoon, quietly = TRUE)

# Get a list of Process modules
pro_modules <- GetModuleList()$process

baseURL <- 'https://raw.githubusercontent.com/zoonproject/modules/master/R/'

library(roxygen2)
library(RCurl)

# loop through each module
for(moduleURL in paste(baseURL, pro_modules, '.R', sep = '')){
  
  roxy_parse <- roxygen2:::parse_file(moduleURL, environment())[[1]]
  
  test_that(paste('Check arguements -', basename(gsub('.R', '', moduleURL))), {
    
    expect_equal(roxy_parse$family, 'process')
    
  })
  
}