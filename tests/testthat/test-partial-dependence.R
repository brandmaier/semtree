#'
#' Testing whether partial dependence plots work as expected
#' based on a default forest built from an OpenMx model
#' 
#' The subsequent tests depend on the first test, which does
#' the expensive forest computation once and then writes
#' to the global environment (which is a bit wonky)
#'

# skip long running tests on CRAN
testthat::skip_on_cran()


#
# test numeric predictors
#

test_that("partial dependence works for numeric predictors", {
  
  
  library(lavaan)

  # draw predictors randomly
  set.seed(325)
  N <- 200
  pred1 <- factor(sample(c("red", "green", "blue"), N, replace = TRUE))
  pred2 <- ordered(sample(c(0, 1, 2), N, replace = TRUE))
  pred3 <- as.numeric(sample(1:20, size = N, replace = TRUE))
  
  x <- rnorm(N) + ifelse(pred2 == "1", 10, 0)
  x <- x + pred3 / 10
  df <- data.frame(x, pred1, pred2, pred3)
  
  model = "x ~~ var*x; x~ mu*0"
  fitted_model <- lavaan(model, df)
  tree = semtree(fitted_model, df, 
        control = semtree_control(verbose = FALSE, report.level = 99))
  plot(tree)
  
  
  forest <<- semforest(fitted_model, df, control = semforest.control(num.trees =
                                                                     10))
  
  pd = partialDependence(forest, reference.var = "pred3")
  plot(pd, parameter="mu")
  
  expect_equal(class(forest), "semforest")
  expect_s3_class(pd, "partialDependence")
  expect_equal(dim(pd$samples), c(20, 3))
  expect_true(median(pd$samples$mu[pd$samples$pred3 < 11]) < 
                median(pd$samples$mu[pd$samples$pred3 >=
                                                                                   11]))
})


test_that("partial dependence works for numeric predictors", {
  

#
# testing unordered factors
#
 #                                                                 10))
pd = partialDependence(forest, reference.var = "pred1")
plot(pd, parameter="mu")
test_that("partial dependence works for numeric predictors", {
  testthat::expect_equal(class(forest), "semforest")
  testthat::expect_s3_class(pd, "partialDependence")
  testthat::expect_equal(dim(pd$samples), c(3, 3))
})

})

test_that("partial dependence works for ordered predictors", {
  
#
# testing ordered factors
#                                                  

pd = partialDependence(forest, reference.var = "pred2")
plot(pd,parameter="mu")

testthat::expect_equal(class(forest), "semforest")
testthat::expect_s3_class(pd, "partialDependence")
testthat::expect_equal(dim(pd$samples), c(3, 3))
testthat::expect_true(pd$samples[pd$samples$pred2==2,"mu"] < pd$samples[pd$samples$pred2==1,"mu"] )
testthat::expect_true(pd$samples[pd$samples$pred2==0,"mu"] < pd$samples[pd$samples$pred2==1,"mu"] )

})

#
# expect error here
#
test_that("missing reference variable is detected", {
  testthat::expect_error(pd = partialDependence(forest, reference.var = "reallyweirdPredictor958Qz6"))
})
