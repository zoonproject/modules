# A zoon module
# @covariate
NCEP <-
function(extent, variables){
  
  require(RNCEP)
  layers <- list()

  for(i in 1:length(variables)){
  data <- NCEP.gather(variable = variables[i],
                    level = 850,
                    months.minmax = c(1:2),
                    years.minmax = c(2000,2001),
                    lat.southnorth = extent[3:4],
                    lon.westeast = extent[1:2],
                    reanalysis2 = FALSE,
                    return.units = TRUE)
  
  avg <- apply(data, c(1, 2), mean)
  
  layers[[i]] <- raster(avg)
  names(layers[[i]]) <- variables[i]  

  extent(layers[[i]]) <- c(extent)
  }

  ras <- do.call(stack, layers)
  
  return (ras)  
}
