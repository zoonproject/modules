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
    
    ## PARAMETER TESTS
    if(roxy_parse$family == 'occurrence'){
      
      test_parameters(roxy_parse, modulePath = modulePath)
        
    }
    
    if(roxy_parse$family == 'covariate'){
      
      test_parameters(roxy_parse, modulePath = modulePath)
      
    }  
    
    if(roxy_parse$family == 'process'){
      
      test_parameters(roxy_parse, defaultParams = '.data',
                      modulePath = modulePath)
      
    }  
    
    if(roxy_parse$family == 'model'){
      
      test_parameters(roxy_parse, defaultParams = '.df',
                      modulePath = modulePath)
      
    }
    
    if(roxy_parse$family == 'output'){
      
      test_parameters(roxy_parse, defaultParams = c('.model', '.ras'),
                      modulePath = modulePath)
      
    }
    
    ## FUNCTION OUTPUT TESTS
    test_outputs(roxy_parse, modulePath)
    
  }
  
}