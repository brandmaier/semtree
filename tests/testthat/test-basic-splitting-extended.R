
# skip long running tests on CRAN
skip_on_cran()

library(lavaan)

#
# test basic splitting
#
n <- 500
var_numeric <- 1:n
var_ordered <- ordered( sample(c(1,2,3,4), 
                               size=n, prob=rep(.25,4), 
                               replace = TRUE))
var_ordered_named <- ordered( sample(c(1,2,3,4), 
                                     size=n, prob=rep(.25,4),
                                     replace = TRUE),
                              labels=c("one","two","three","four"))

var_unordered_named <- factor(sample(c("red","green","blue","teal with a little bit of rosé"),
                                     size=n, prob=rep(.25,4), replace = TRUE))

var_unordered <- factor(sample(c(10713, 10720, 81247, 80337),
                               size=n, prob=rep(.25,4), replace = TRUE))


# just a single observed value in ordered factor, that is,
# no split is possible, should return only root
var_ordered_named_single <- var_ordered_named
var_ordered_named_single[1:n] <- "two"

set.seed(233453)
x = rnorm(n)
df <- data.frame(x, var_ordered_named_single)
model = "x ~~ x"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree_control())
test_that("return object is a valid tree", {expect_equal(class(tree),"semtree")})
test_that("tree depth is correct", {expect_equal(getDepth(tree),0)})

tree = semtree(fitted_model, df, control=semtree_control(method="score"))
test_that("return object is a valid tree", {expect_equal(class(tree),"semtree")})
test_that("tree depth is correct", {expect_equal(getDepth(tree),0)})


# just a single observed value in an unordered factor, that is,
# no split is possible, should return only root
var_unordered_named_single <- var_unordered_named
var_unordered_named_single[1:n] <- "red"
df <- data.frame(x, var_unordered_named_single)
tree = semtree(fitted_model, df, control=semtree_control())
test_that("return object is a valid tree", {expect_equal(class(tree),"semtree")})
test_that("tree depth is correct", {expect_equal(getDepth(tree),0)})

# NA values and still no effect of any predictor
var_numeric_NA <- var_numeric
var_numeric_NA[c(1,10,11,12,16,100,400,401,402)]<-NA
var_ordered_NA <- var_ordered
var_ordered_NA[c(1,2,3,4,100,101,200,202,204,303)]<-NA
var_unordered_NA <- var_ordered
var_unordered_NA[c(1,2,3,4,100,101,200,202,204,303,420,421,422)]<-NA
df <- data.frame(x, var_numeric_NA, var_ordered_NA, var_unordered_NA)
tree = semtree(fitted_model, df, control=semtree_control(max.depth=3))
test_that("return object is a valid tree", {
  expect_equal(class(tree),"semtree")})
