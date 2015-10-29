### General module tests ###
context('General module tests')

library(zoon, quietly = TRUE)
library(roxygen2)

# Get our list of modules
modulePaths <- list.files('../../R', pattern = '.R$', full.names = TRUE)

# A vector of R scripts to ignore
ignoreModules <- c('ModulesDocumentation.R')
modulePaths <- modulePaths[!basename(modulePaths) %in% ignoreModules]

test_module <- function(modulePath){
  
  roxy_parse <- roxygen2:::parse_file(modulePath, environment())[[1]]
  
  ## GENERIC TESTS
  test_that(paste('Check roxy_parse', basename(gsub('.R', '', modulePath))),{
    
    # Check roxy_parse is good
    expect_is(roxy_parse, 'list')
    
  })
  
  test_that(paste('Check for generic tags', basename(gsub('.R', '', modulePath))),{
    
    # Check for the required generic tags
    expect_true('title' %in% names(roxy_parse))
    expect_true('description' %in% names(roxy_parse))
    expect_true('name' %in% names(roxy_parse))
    expect_true('family' %in% names(roxy_parse))
    expect_true('author' %in% names(roxy_parse))
    
  })
  
  # If the family tag is present continue (this error will be reported above)
  if('family' %in% names(roxy_parse)){
    
    ## OCCURRENCE TESTS
    if(roxy_parse$family == 'occurrence'){
      
      test_that(paste('Check occurrence module -', basename(gsub('.R', '', modulePath))), {
        
        expect_equal(roxy_parse$family, 'occurrence')
        
      })
      
    }
    
    ## COVARIATE TESTS
    if(roxy_parse$family == 'covariate'){
      
      test_that(paste('Check covariate module -', basename(gsub('.R', '', modulePath))), {
        
        expect_equal(roxy_parse$family, 'covariate')
        
      })
      
    }  
    
    ## PROCESS TESTS
    if(roxy_parse$family == 'process'){
      
      test_that(paste('Check process module -', basename(gsub('.R', '', modulePath))), {
        
        expect_equal(roxy_parse$family, 'process')
        
      })
      
    }  
    
    ## MODEL TESTS
    if(roxy_parse$family == 'model'){
      
      test_that(paste('Check model module -', basename(gsub('.R', '', modulePath))), {
        
        expect_equal(roxy_parse$family, 'model')
        
      })
      
    }
    
    ## OUTPUT TESTS
    if(roxy_parse$family == 'output'){
      
      test_that(paste('Check output module -', basename(gsub('.R', '', modulePath))), {
        
        expect_equal(roxy_parse$family, 'output')
        
      })
      
    }
  }
  
}

# loop through each module
for(modulePath in modulePaths){
  
  test_module(modulePath) 
  
}