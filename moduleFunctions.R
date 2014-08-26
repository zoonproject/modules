# 
#
#
#  Occurence: lon, lat, type, value  --
#                                      | --> process: df lon, lat, type, value, covar --> model: predict method --> output
#                    Covariate: ras  --
#
#
#
#



library(zoon)



# Just a function to read local data in. 

LocalData <- function(filename, occurrenceType){
  type <- tolower(occurrenceType)
  #assert_that(type %in% c('presence', 'presence/absence', 'abundance'))
  occurrence <- read.csv(filename, header=TRUE)
  
  if(type == 'presence/absence') {
    occurrence$type <- ifelse(occurrence[,3]==1, 'presence', 'absence')
  } else {
    occurrence$type <- type
  }

  colnames(occurrence) <- c('lon', 'lat', 'value', 'type')

  return(occurrence)
}

BuildModule(LocalData, 'occurrence', dir='~/Dropbox/zoon/modules/R')





NoProcess <- function(occurrence, ras){

  
  noccurrence <- nrow(occurrence)
  
  
  # extract covariates
  occ_covs <- as.matrix(extract(ras, occurrence[, c('lon', 'lat')]))
  
  # combine with the occurrence data
  df <- cbind(occurrence,
                   occ_covs)
  
  names(df)[5:ncol(df)] <- names(ras)
  
  return(df)
  
}

BuildModule(NoProcess, 'process', dir='~/Dropbox/zoon/modules/R')




# spocc module

SpOcc <- function(species, extent, databases = 'gbif'){
  require(spocc)
  raw <- occ2df(occ(query = species, geometry = extent, from = databases, limit=10e5))
  occurrence <- raw[,c('longitude', 'latitude')]
  occurrence$value <- 1
  occurrence$type <- 'presence'
  return(occurrence) 
}

BuildModule(SpOcc, 'occurrence', dir = '~/Dropbox/zoon/modules/R')





# A module for uk air data saved in package.

UKAir <- function(){
  return(UKAirRas)
}
BuildModule(UKAir, 'covariate', dir = '~/Dropbox/zoon/modules/R')


# A module for uk anopheles plumbeus saved in package.
UKAnophelesPlumbeus <- function(){
  return(AplumbeusOcc)
}
BuildModule(UKAnophelesPlumbeus, 'occurrence', dir='~/Dropbox/zoon/modules/R')


# Wrapper for biomod2 model function

BiomodModel <- function(df, modelType){
  require(biomod2)
 
  biomodData <- BIOMOD_FormatingData(resp.var = df$value, expl.var = df[,5:NCOL(df), drop=FALSE], resp.xy = df[,c('lon', 'lat')], resp.name = 'Species')

  myBiomodOptions <- BIOMOD_ModelingOptions()

  
  myBiomodModelOut <- BIOMOD_Modeling(
    biomodData,
    models = modelType,
    models.options = myBiomodOptions,
    NbRunEval=1,
    DataSplit=100,
    Prevalence=0.5,
    VarImport=0,
    SaveObj = TRUE,
    rescal.all.models = TRUE,
    do.full.models = FALSE,
    modeling.id = paste('zoon',Sys.time(),sep=" "),
    silent=TRUE
  )

  # Create a global predict method
  biomodPredictMethod <- function(object, newdata, type='response'){
  #  predict.BIOMOD.models.out <<- function(object, newdata, type='response'){

    assertthat::assert_that(class(newdata) == 'RasterLayer' || class(newdata) == 'RasterStack' || class(newdata) == 'data.frame')

    if(class(newdata) == 'RasterLayer'){
      new.data.stack <- stack(newdata)
    } else if (class(newdata) == 'RasterStack') {
      new.data.stack <- newdata
    } else {
      if(!all(names(newdata) %in% object@expl.var.names) ){
        stop('Variable names in newdata and the model object do not match')
      }
      new.data.df <- newdata
    }

    if(class(newdata) == 'RasterLayer' || class(newdata) == 'RasterStack'){
      biomodProject <- BIOMOD_Projection(
        modeling.output = object,
        new.env = new.data.stack,
        proj.name ='current',
        selected.models ='all',
        clamping.mask = F,
        output.format ='.grd',
        silent=TRUE
      )
    } else {
      biomodProject <- BIOMOD_Projection(
        modeling.output = object,
        new.env = new.data.df,
        proj.name ='current',
        selected.models ='all',
        clamping.mask = F,
        output.format ='.RData',
        silent=TRUE
      )
    }


    preds <- get_predictions(biomodProject)
    
    return(preds)  

  }


  assign('predict.BIOMOD.models.out', biomodPredictMethod, envir = .GlobalEnv)

  return(myBiomodModelOut)

}


BuildModule(BiomodModel, 'model', dir='~/Dropbox/zoon/modules/R')
rm(BiomodModel, biomodPredictMethod, predict.BIOMOD.models.out)

# NCEP data

NCEP <- function(extent, variables){
  
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

BuildModule(NCEP, 'covariate', dir='~/Dropbox/zoon/modules/R')	








# Get world clim data
#  library(raster)
# w = getData('worldclim', var='tmin', res=0.5, lon=5, lat=45)







#  
# 
#
#




