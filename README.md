[![Build Status](https://travis-ci.org/zoonproject/modules.svg)](https://travis-ci.org/zoonproject/modules)
[![codecov.io](https://codecov.io/github/zoonproject/modules/coverage.svg?branch=master)](https://codecov.io/github/zoonproject/modules?branch=master)

Modules
=======

This repository contains the modules for the [Zoon](https://github.com/zoonproject/zoon) species distribution modelling package.

A module is a self contained part of a SDM analysis. We conceptualise a SDM analysis as containing five parts.

### Occurrence data

Collect occurrence data. This could be from a [local file](https://github.com/zoonproject/modules/blob/master/R/LocalOccurrenceData.R) or an [online database](https://github.com/zoonproject/modules/blob/master/R/SpOcc.R) and might include presence only data, presence/absence data, abundance data etc.

### Covariate data

Collect data on predictor variables for the model. While commonly [environmental](https://github.com/zoonproject/modules/blob/master/R/Bioclim.R) measurements, this could include habitat data, distributions of other species, anthropogenic variables or anything else.

### Processes

The are maany things that need to be done to the data before actual modelling takes place. This might include taking [background pseudoabsences](https://github.com/zoonproject/modules/blob/master/R/OneHundredBackground.R), splitting the data for [crossvalidation](https://github.com/zoonproject/modules/blob/master/R/Crossvalidate.R), runnings a PCA on environmental variables, data cleaning or a million other things.


### Models

This is the area most other SDM packages focus on. Common examples include [logistic regression](https://github.com/zoonproject/modules/blob/master/R/LogisticRegression.R) and [random forests](https://github.com/zoonproject/modules/blob/master/R/RandomForest.R). But also wrapping [whole packages](https://github.com/zoonproject/modules/blob/master/R/BiomodModel.R) means there's a lot of models available.


### Output

Good species distribution analyses should provide a variety of outputs. Typically we want to use our trained models to [map](https://github.com/zoonproject/modules/blob/master/R/SurfaceMap.R) where a species exists. We should also be checking how well our [models work](https://github.com/zoonproject/modules/blob/master/R/PerformanceMeasures.R) and the response shape and importance of each covariate. Perhaps we also want to [upload](https://github.com/zoonproject/modules/blob/master/R/Figshare.R) our entire analysis to an online repository. Who knows.



## Contributing modules

Modules are simple R scripts containing a single function and some metadata. The inputs and outputs of each module type are controlled. A brief description can be found [here](https://github.com/zoonproject/modules/blob/master/vignettes/Module_IO_for_devs.Rmd). The function `BuildModule` from the Zoon package is used to turn a function in an R session into a module. 

Please note, Zoon is still being developed. We would love you to contribute modules, but can't yet guarantee that there won't be major changes that might break modules. We will try to fix user submitted modules if we break them. 









