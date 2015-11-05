# A function for testing the outputs conform to the expected
# (within a context)
test_outputs <- function(roxy_parse, modulePath){

  test_that('Check output formats',{ 
  
    ## OCCURRENCE MODULES ##
    if(roxy_parse$family == 'occurrence'){
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      occ_return <- do.call(roxy_parse$name, args = list())

      # Check the data.frame returned is as expected
      expect_is(occ_return, 'data.frame', info = 'Occurrence modules must return a data.frame')
      expect_named(occ_return, expected = c('longitude',
                                            'latitude',
                                            'value',
                                            'type',
                                            'fold'), 
                   info = "Some of the required columns are missing ('longitude', 'latitude', 'value', 'type', 'fold')")
      expect_is(occ_return$longitude, c('numeric', 'integer'), info = 'longitude must be a numeric or integer')
      expect_is(occ_return$latitude, c('numeric', 'integer'), info = 'latitude must be a numeric or integer')
      expect_is(occ_return$value, c('numeric', 'integer'), info = 'value must be a numeric or integer')
      expect_is(occ_return$type, 'character', info = 'type must be a character')
      expect_is(occ_return$fold, c('numeric', 'integer'), info = 'info must be a numeric or integer')

    }
    
    ## COVARIATE MODULES ##
    if(roxy_parse$family == 'covariate'){
      
      # Load the script
      source(modulePath) 
      
      # Run the module with defaults
      cov_return <- do.call(roxy_parse$name, args = list())

      # Check projection
      expect_true(all(grepl("+proj=longlat", projection(cov_return)),
                      grepl("+ellps=WGS84", projection(cov_return))),
                  info = 'Covariate module output must have WGS84 projection: proj4 string is expected to contain the elements "+proj=longlat" and "+ellps=WGS84"')
      
      # Check raster returned is as expected
      expect_is(cov_return, c('RasterLayer', 'RasterStack', 'RasterBrick'), info = 'Covariate module output must be either a RasterLayer or a RasterStack')
      
    }
  
  })
}
