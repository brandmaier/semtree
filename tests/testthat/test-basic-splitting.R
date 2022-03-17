context("test basic splitting based on level of covariate")

library(lavaan)
# skip long running tests on CRAN
skip_on_cran()

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


# testing ordered, named factors

set.seed(233453)
x = rnorm(n)
x <- x * ifelse( (var_ordered_named=="one"), .5, 10) 
df <- data.frame(x, var_ordered_named)
model = "x ~~ x"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(verbose=TRUE,report.level = 99))
test_that("result is a tree",{ expect_equal(class(tree),"semtree")})
test_that("tree depth is 3", { expect_equal(getDepth(tree),3) })

# testing unordered, named factors
set.seed(3490843)
x <- rnorm(n)
x <- x * ifelse( var_unordered_named=="green" , 1, 10)
df <- data.frame(x, var_unordered_named)
tree = semtree(fitted_model, df, control=semtree.control(verbose=TRUE,report.level = 99))
plot(tree)
test_that("result is a tree",{ expect_equal(class(tree),"semtree")})
test_that("tree depth is at least 2", { expect_gt(getDepth(tree),1) })
test_that("split is optimal", { expect_equal(tree$caption, "var_unordered_named  in [ green ]")})

# testing ordered, numeric
set.seed(23334653)
x = rnorm(n)
x <- x * ifelse( (var_ordered <= 2), .5, 10) 
df <- data.frame(x, var_ordered)
model = "x ~~ x"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(verbose=TRUE,report.level = 99))
plot(tree)
test_that("result is a tree",{ expect_equal(class(tree),"semtree")})
test_that("tree depth is 2", { expect_equal(getDepth(tree),2) })
test_that("split is optimal", { expect_equal(tree$caption, "var_ordered > 2")})

# testing numeric
set.seed(23334653)
x = rnorm(n)
x <- x * ifelse( (var_numeric < mean(var_numeric)), .5, 10) 
df <- data.frame(x, var_numeric)
model = "x ~~ x"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(max.depth = 3))
plot(tree)
test_that("split is optimal", { expect_equal(tree$caption, "var_numeric >= 251.5")})

# all of them
df <- data.frame(x,  var_ordered, var_ordered_named, var_unordered_named)
set.seed(23334653)
x = rnorm(n)
x <- x * ifelse( (var_ordered <= 2), .5, 10) 
df <- data.frame(x, var_ordered, var_numeric, var_unordered_named)
model = "x ~~ x"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(verbose=TRUE,report.level = 99))
plot(tree)
test_that("result is a tree",{ expect_equal(class(tree),"semtree")})
test_that("tree depth is 6", { expect_equal(getDepth(tree),6) })