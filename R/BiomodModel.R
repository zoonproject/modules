# A zoon module
# @model
BiomodModel <-
function(df, modeltype){
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
  predict.BIOMOD.models.out <<- function(object, newdata){

    assert_that(class(newdata) == 'RasterLayer' || class(newdata) == 'RasterStack')

    if(class(newdata)=='RasterLayer'){
      new.data.stack <- stack(newdata)
    } else {
      new.data.stack <- newdata
    }


    biomodProject <- BIOMOD_Projection(
      modeling.output = object,
      new.env = new.data.stack,
      proj.name ='current',
      selected.models ='all',
      clamping.mask = F,
      output.format ='.grd',
      silent=TRUE
    )

    preds <- get_predictions(biomodProject)
    
    return(preds)  

  }

  return(myBiomodModelOut)

}
