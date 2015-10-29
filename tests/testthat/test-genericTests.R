### General module tests ###
library(zoon, quietly = TRUE)
library(roxygen2)

# Get our list of modules
modulePaths <- list.files('../../R', pattern = '.R$', full.names = TRUE)

# A vector of R scripts to ignore
ignoreModules <- c('ModulesDocumentation.R')
modulePaths <- modulePaths[!basename(modulePaths) %in% ignoreModules]

# A function for testing parameters (within a context)
test_paramaters <- function(roxy_parse, defaultParams = NULL, modulePath){

  # Extract the parameters
  params <- formals(source(modulePath)$value)
    
  test_that('Check parameter names', {
    
    # Extract names from tags
    paramNames <- unlist(lapply(roxy_parse[names(roxy_parse) == 'param'],
                                function(x) x$name))
    
    # Check for defult parameter
    if(!is.null(defaultParams)) expect_true(all(defaultParams %in% paramNames),
                                            info = 'Default parameter is not documented')
    
    # Check all parameters are documented
    expect_true(all(names(params) %in% paramNames),
                info = 'Parameters are missing documentation')
    
  })
  
  test_that('Check default values', {
    
    # Expect that all non-default parameters have defaults
    paramClasses <- lapply(params, function(x) class(x))
    
    # Remove ellipsis
    paramClasses <- paramClasses[!names(paramClasses) %in% '...']
    
    # remove defaults
    if(!is.null(defaultParams)) paramClasses <- paramClasses[!names(paramClasses) %in% defaultParams]
    
    expect_true(all(paramClasses != 'name'),
                info = 'Parameters are missing default values')
    
  })
}

test_module <- function(modulePath){
  
  moduleName <- basename(gsub('.R$', '', modulePath))
  
  roxy_parse <- roxygen2:::parse_file(modulePath, environment())[[1]]
  
  ## GENERIC TESTS
  test_that(paste('Check roxy_parse', moduleName),{
    
    # Check roxy_parse is good
    expect_is(roxy_parse, 'list')
    
  })
  
  test_that(paste('Check for generic tags', moduleName),{
    
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
      
      test_paramaters(roxy_parse, modulePath = modulePath)
      
    }
    
    ## COVARIATE TESTS
    if(roxy_parse$family == 'covariate'){
      
      test_paramaters(roxy_parse, modulePath = modulePath)
      
    }  
    
    ## PROCESS TESTS
    if(roxy_parse$family == 'process'){
      
      test_paramaters(roxy_parse, defaultParams = '.data',
                      modulePath = modulePath)

    }  
    
    ## MODEL TESTS
    if(roxy_parse$family == 'model'){
      
      test_paramaters(roxy_parse, defaultParams = '.df',
                      modulePath = modulePath)
      
    }
    
    ## OUTPUT TESTS
    if(roxy_parse$family == 'output'){
      
      test_paramaters(roxy_parse, defaultParams = c('.model', '.ras'),
                      modulePath = modulePath)

    }
    
  }
  
}

# loop through each module
for(modulePath in modulePaths){
  
  context(paste('Testing module', basename(gsub('.R$', '', modulePath))))
  
  test_module(modulePath) 
  
}