context('Soft test each module in turn. Only expect object to exists i.e. throw no errors.')

test_that('Occurrence modules all work', {

  
  o1 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap')
  o2 <- workflow('AnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap')
  o3 <- workflow(ModuleOptions('SpOcc', species='Anopheles plumbeus', extent= c(-10, 10, 45, 65)), 
          'UKAir', 'OneHundredBackground', 'LogisticRegression', 'SameTimePlaceMap')

  set.seed(12)
  dt <- data.frame(longitude = runif(10), latitude = runif(10), value = 1)
  write.csv(dt, 'zoonLocalDataTest.csv', row.names=FALSE)
  o4 <- workflow(ModuleOptions('LocalData', filename = 'zoonLocalDataTest.csv', occurrenceType='presence'), 
          'UKAir', 'OneHundredBackground', 'LogisticRegression', 'SameTimePlaceMap')

  o5 <- workflow(
          ModuleOptions('LocalOccurrenceData', filename = 'zoonLocalDataTest.csv', 
            occurrenceType='presence', externalValidation=FALSE), 
          'UKAir', 'OneHundredBackground', 'LogisticRegression', 'SameTimePlaceMap')

  expect_true(exists('o1'))
  expect_true(exists('o2'))
  expect_true(exists('o3'))
  expect_true(exists('o4'))
  expect_true(exists('o5'))


  file.remove('zoonLocalDataTest.csv')

})


test_that('Covariate modules all work', {
  c1 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap')
  c2 <- workflow('UKAnophelesPlumbeus', 'AirNCEP', 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap') #docs for airncep are bad.
  c3 <- workflow('UKAnophelesPlumbeus', ModuleOptions('NCEP',  extent= c(-10, 10, 45, 65), variables='air'), 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap')
  c4 <- workflow('UKAnophelesPlumbeus', 'UKBioclim', 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap')

  expect_true(exists('c1'))
  expect_true(exists('c2'))
  expect_true(exists('c3'))
  expect_true(exists('c4'))


})


test_that('Process modules all work', {
  p1 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap')
  p2 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneThousandBackground', 
          'LogisticRegression', 'SameTimePlaceMap')
  p3 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'BackgroundAndCrossvalid', 
          'LogisticRegression', 'SameTimePlaceMap')

  set.seed(12)
  dt <- data.frame(longitude = runif(10, 0,10), latitude = runif(10, 50, 60), value = rep(c(0,1), 5))
  write.csv(dt, 'zoonLocalDataTest.csv', row.names=FALSE)
  p4 <- workflow(ModuleOptions('LocalData', filename = 'zoonLocalDataTest.csv', occurrenceType='presence/absence'), 'UKAir', 'NoProcess', 
          'LogisticRegression', 'SameTimePlaceMap')

  expect_true(exists('p1'))
  expect_true(exists('p2'))
  expect_true(exists('p3'))
  expect_true(exists('p4'))


})

test_that('Model modules all work', {
  m1 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'LogisticRegression', 'SameTimePlaceMap')
  m2 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'RandomForest', 'SameTimePlaceMap')
  m3 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'QuickGRaF', 'SameTimePlaceMap')
  m4 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          'OptGRaF', 'SameTimePlaceMap')
  m5 <- workflow('UKAnophelesPlumbeus', 'UKAir', 'OneHundredBackground', 
          ModuleOptions('BiomodModel', modelType='GAM'), 'SameTimePlaceMap')

  expect_true(exists('m1'))
  expect_true(exists('m2'))
  expect_true(exists('m3'))
  expect_true(exists('m4'))
  expect_true(exists('m5'))
})
