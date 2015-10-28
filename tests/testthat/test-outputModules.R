### Test Output modules ###
context('Testing for Output modules')

library(zoon, quietly = TRUE)

# Get a list of Output modules
out_modules <- GetModuleList()$output

baseURL <- 'https://raw.githubusercontent.com/zoonproject/modules/master/R/'

library(roxygen2)
library(RCurl)

# loop through each module
for(moduleURL in paste(baseURL, out_modules, '.R', sep = '')){
  
  roxy_parse <- roxygen2:::parse_file(moduleURL, environment())[[1]]
  
  test_that(paste('Check arguements -', basename(gsub('.R', '', moduleURL))), {
    
    expect_equal(roxy_parse$family, 'output')
    
  })
  
}