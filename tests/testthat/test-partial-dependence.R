#context("test partial dependence")

library(lavaan)

#
# test numeric predictors
#

# skip long running tests on CRAN
skip_on_cran()

# draw predictors randomly
set.seed(325)
N <- 200
pred1 <- factor(sample(c("red","green","blue"),N,replace=TRUE))
pred2 <- ordered(sample(c(0,1,2),N,replace=TRUE))
pred3 <- as.numeric(sample(1:20,size = N,replace=TRUE))

x <- rnorm(N)+ifelse(pred2=="1",10,0)
x <- x + pred3/10
df <- data.frame(x, pred3)

model = "x ~~ var*x; x~ mu*0"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(verbose=TRUE,report.level = 99))
plot(tree)


forest = semforest(fitted_model, df, control=semforest.control(num.trees=10))

pd = partialDependence(forest, reference.var="pred3")
plot(pd, "mu")
test_that("partial dependence works for numeric predictors", {
  expect_equal(class(forest),"semforest")
  expect_equal(class(pd),"partialDependence")
  expect_equal(dim(pd$samples),c(20,3))
  expect_true(median(pd$samples$mu[pd$samples$pred3<1]) > median(pd$samples$mu[pd$samples$pred3>=1]))
})

plot(pd, "mu")

pd = partialDependence(forest, reference.var="pred2")

#
# testing unordered factors
#
df <- data.frame(x, pred1)
forest = semforest(fitted_model, df, control=semforest.control(num.trees=10))
pd = partialDependence(forest, reference.var="pred1")
plot(pd, "mu")
test_that("partial dependence works for numeric predictors", {
  expect_equal(class(forest),"semforest")
  expect_equal(class(pd),"partialDependence")
  expect_equal(dim(pd$samples),c(3,3))
})

#
# testing ordered factors
#
df <- data.frame(x, pred2)
forest = semforest(fitted_model, df, control=semforest.control(num.trees=10))
pd = partialDependence(forest, reference.var="pred2")
test_that("partial dependence works for numeric predictors", {
  expect_equal(class(forest),"semforest")
  expect_equal(class(pd),"partialDependence")
})

#
# expect error here
#
test_that("missing reference variable is detected", {
  expect_error(
 pd = partialDependence(forest, reference.var="reallyweirdPredictor958Qz6")
)
})
