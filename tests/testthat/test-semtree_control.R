skip_on_cran()


testthat::test_that("control object is created and checked correctly", {
  ctrl <- semtree::semtree_control(min.N = NULL)
  chck <- semtree:::check.semtree.control(ctrl)
  expect_true(chck)
}  )


# testthat::test_that("", {
#   
#   library(semtree)
#   library(lavaan)
#   
#   n <- 1000
#   
#   x <- rnorm(n)
#   
#   # data frame has only a dummy predictor
#   df <- data.frame(x=x)
#   
#   fit <- lavaan("x~~x",df)
#   
#   ctrl <- semforest_control(mtry=NULL)
#   
#   semforest(model = fit, data = df, control=ctrl)
# })
