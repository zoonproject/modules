context('Test the MachineLearn module.')

test_that('basic caret MachineLearn works (including a list of popular models)', {

skip('not run')

m1 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = MachineLearn(method = 'nnet'),
                  output = SameTimePlaceMap)

expect_true(exists('m1'))
expect_is(m1$report[[1]], 'RasterLayer')
expect_is(m1$model.output[[1]][[1]]$model, 'train')


m2 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = MachineLearn(method = 'glm'),
                  output = SameTimePlaceMap)

expect_true(exists('m2'))
expect_is(m2$report[[1]], 'RasterLayer')
expect_is(m2$model.output[[1]][[1]]$model, 'train')


m3 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = MachineLearn(method = 'glmnet'),
                  output = SameTimePlaceMap)

expect_true(exists('m3'))
expect_is(m3$report[[1]], 'RasterLayer')
expect_is(m3$model.output[[1]][[1]]$model, 'train')


m4 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = MachineLearn(method = 'rf'),
                  output = SameTimePlaceMap)

expect_true(exists('m4'))
expect_is(m4$report[[1]], 'RasterLayer')
expect_is(m4$model.output[[1]][[1]]$model, 'train')


m5 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = MachineLearn(method = 'gbm'),
                  output = SameTimePlaceMap)

expect_true(exists('m5'))
expect_is(m5$report[[1]], 'RasterLayer')
expect_is(m5$model.output[[1]][[1]]$model, 'train')

})





test_that('MachineLearn works with some other output modules.', {

skip('not run')

m6 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = MachineLearn(method = 'nnet'),
                  output = ResponseCurve)

expect_true(exists('m6'))
expect_is(m6$report[[1]], 'NULL')
expect_is(m6$model.output[[1]][[1]]$model, 'train')


m7 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = OneHundredBackground,
                  model = list(MachineLearn(method = 'nnet'), MachineLearn(method = 'glm')),
                  output = SameTimePlaceMap)

expect_true(exists('m7'))
expect_is(m7$report[[1]], 'RasterLayer')
expect_is(m7$model.output[[1]][[1]]$model, 'train')
expect_true(length(m7$model.output) == 2)
expect_true(length(m7$report) == 2)




m8 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKBioclim,
                  process = Chain(OneHundredBackground, Crossvalidate(k = 2)),
                  model = MachineLearn(method = 'glm'),
                  output = PerformanceMeasures)

expect_true(exists('m8'))
expect_is(m8$report[[1]], 'list')
expect_is(m8$model.output[[1]][[1]]$model, 'train')
expect_true(all(names(m8$report[[1]]) == c("auc", "kappa", "omissions", "sensitivity", "specificity", "proportionCorrect")))


})


