# A zoon module
# @model
BiomodModel <-
function(df, modelType){
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

  assign('predict.BIOMOD.models.out', biomodPredictMethod)

  print('HEllo')
  try(paste('infunc1', print(where('biomodPredictMethod'))))
  try(paste('infunc2',print(where('predict.BIOMOD.models.out'))))

  return(myBiomodModelOut)

}
