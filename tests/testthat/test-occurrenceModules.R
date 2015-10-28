### Test Occurrence modules ###
context('Testing for Occurrence modules')

library(zoon, quietly = TRUE)

# Get a list of occurrence modules
occ_modules <- GetModuleList()$occurrence

baseURL <- 'https://raw.githubusercontent.com/zoonproject/modules/master/R/'

library(roxygen2)
library(RCurl)

# loop through each module
for(moduleURL in paste(baseURL, occ_modules, '.R', sep = '')){

  roxy_parse <- roxygen2:::parse_file(moduleURL, environment())[[1]]
  
  test_that(paste('Check arguements -', basename(gsub('.R', '', moduleURL))), {

    expect_equal(roxy_parse$family, 'occurrence')
    
  })
  
}