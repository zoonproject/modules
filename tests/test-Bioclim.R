context('Test the Bioclim module')



test_that('Default global extent works', {

set.seed(1)

w1 <- workflow(occurrence = CWBZimbabwe,
                  covariate = Bioclim(res = 10),
                  process = NoProcess,
                  model = LogisticRegression,
                  output = SameTimePlaceMap)


set.seed(1) 

w2 <- workflow(occurrence = CWBZimbabwe,
                  covariate = Bioclim(extent = c(-180, 180, -90, 90), res = 10),
                  process = NoProcess,
                  model = LogisticRegression,
                  output = SameTimePlaceMap)

expect_true(identical(w1[2], w2[2]))

})
