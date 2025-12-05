#context("test basic splitting based on level of covariate with score tests")

# skip long running tests on CRAN
testthat::skip_on_cran()

library(lavaan)
library(semtree)

# generate observations of an ordered factor with labels
set.seed(458)
n <- 1000
var_unordered <- factor(sample(c("lightning","rain","sunshine","snow"),n,TRUE))
x <- rnorm(n)+ifelse(var_unordered=="rain",20,0)
x <- x+ifelse(var_unordered=="sunshine",20,0)

df <- data.frame(x, var_unordered)
model = "x ~~ x; x ~mu*1"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree_control(method="score"))
test_that("optimal split is chosen", {
  expect_true(all(tree$rule$value==c("lightning","snow")))
  expect_equal(tree$rule$relation,"%in%")
})

plot(tree)


# 
n <- 100
var_ordered <- ordered(sample(c("A","B","C","D"), n, replace=TRUE))
x <- rnorm(n) + ifelse(var_ordered>="C",10,0)

df <- data.frame(x, var_ordered)
model = "x ~~ x; x ~mu*1"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree_control(method="score"))
plot(tree)
test_that("optimal split is chosen", {
  expect_equal(tree$rule$value,"B")
  expect_equal(tree$rule$relation,">")
})

tree = semtree(fitted_model, df, control=semtree_control(method="score",min.bucket = 50))


# generate observations of an ordered factor with labels
set.seed(458)
n <- 1000
var_metric <- runif(n,-10,+10)
x <- rnorm(n)+var_metric

df <- data.frame(x, var_metric)
model = "x ~~ x; x ~mu*1"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree_control(method="score"))
test_that("optimal split is chosen", {
#  expect_true(all(tree$rule$value==c("lightning","snow")))
  expect_equal(tree$rule$relation,">=")
})

plot(tree)
