context('Test PrintMap.')


test_that('Presence-absence works', {

w1 <- workflow(occurrence = CWBZimbabwe,
                  covariate = Bioclim(extent = c(28, 38, -25, -15),
                                      res = 2.5),
                  process = NoProcess,
                  model = LogisticRegression,
                  output = PrintMap)

expect_true(exists('w1'))
expect_true(length(dev.list() == 1))
expect_true(is.null(w1$report[[1]]))

# Turn off graphics devices so next test can check.
graphics.off()

})


test_that('Presence only works', {


w2 <- workflow(occurrence = SpOcc(species = 'Eresus kollari', 
                                       extent = c(-10, 10, 45, 65)),
                  covariate  = UKAir,
                  process    = BackgroundAndCrossvalid(k = 2),
                  model      = list(LogisticRegression, RandomForest),
                  output     = PrintMap
         )


expect_true(exists('w2'))
expect_true(length(dev.list() == 1))
expect_true(is.null(w2$report[[1]]))

# Turn off graphics devices so next test can check.
graphics.off()
})

test_that('Previous bugs work', {

w3 <- workflow(occurrence = UKAnophelesPlumbeus,
                  covariate = UKAir,
                  process = OneHundredBackground,
                  model = LogisticRegression,
                  output = PrintMap)


expect_true(exists('w3'))
expect_true(length(dev.list() == 1))
expect_true(is.null(w3$report[[1]]))

# Turn off graphics devices so next test can check.
graphics.off()

}) 




test_that('Abundance works', {





}) 


