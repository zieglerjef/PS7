####################
### Unit testing ###
####################

# test1: correct numeric integration output

context("Correct output")

test_that(paste('Numeric integral not correct.'),{
  # create test function (found in adaptIntegrate help file), integral from 0 to 1 = 0.09714416
  testFn0 <- function(x) {
    prod(sin(x))
  }
  correctAnswer <- adaptIntegrate(testFn0, rep(0, 3), rep(1, 3), tol=1e-4)
  expect_equal(round(as.numeric(correctAnswer$integral), 5), round(as.numeric(sg.int(testFn0, lower=rep(0,3), upper=rep(1,3))), 5))
})