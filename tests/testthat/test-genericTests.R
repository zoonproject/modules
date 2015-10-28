### General module tests ###
context('General module tests')

library(zoon, quietly = TRUE)

# These tests are generic and do not depend on module type
modules <- unlist(GetModuleList())

baseURL <- 'https://raw.githubusercontent.com/zoonproject/modules/master/R/'

library(roxygen2)
library(RCurl)

# loop through each module
for(moduleURL in paste(baseURL, modules, '.R', sep = '')){
  
  test_that(paste('Check urls exist -', basename(gsub('.R', '', moduleURL))),{
    
    # expect that the URL exists
    expect_true(url.exists(moduleURL))
    closeAllConnections()
    
  })
  
  test_that(paste('Check for generic tags', basename(gsub('.R', '', moduleURL))),{
    
    roxy_parse <- roxygen2:::parse_file(moduleURL, environment())[[1]]
    closeAllConnections()
    
    # Check for the required generic tags
    expect_true('title' %in% names(roxy_parse))
    expect_true('description' %in% names(roxy_parse))
    expect_true('name' %in% names(roxy_parse))
    expect_true('family' %in% names(roxy_parse))
    expect_true('author' %in% names(roxy_parse))
    
  })
  
}