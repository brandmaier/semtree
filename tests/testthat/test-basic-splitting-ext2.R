# skip long running tests on CRAN
testthat::skip_on_cran()

testthat::test_that("Trees worked",{
  
set.seed(345)
N <- 2000
p1 <- factor(sample( c("retired","in workforce","unemployed"), N, TRUE))
p2 <- factor(sample( c("north","south"),N, TRUE))
p3 <- ordered(sample( c(5,9,10,11,12,13,14,15), N, TRUE))
xdat <- MASS::mvrnorm(n=N, mu=rep(0,4), Sigma=
                        matrix(c(1.0,0.8,0.8,0.8,
                                 0.8,1.0,0.8,0.8,
                                 0.8,0.8,1.0,0.8,
                                 0.8,0.8,0.8,1.0),nrow=4))
x1 <- xdat[,1]
x2 <- xdat[,2]
x3 <- xdat[,3]
x4 <- xdat[,4] + ifelse(p1=="retired",1,0)

df<-data.frame(x1,x2,x3,x4, p1, p2,p3)
model <- "L =~ 1*x1+x2+x3+x4"

fit_model <- lavaan::cfa(model, df)

tree <- semtree(fit_model, df, semtree_control(bonferroni = FALSE))
tree_bf <- semtree(fit_model, df, semtree_control(bonferroni = TRUE))
tree_score <- semtree(fit_model, df, semtree_control(method="score"))
plot(tree)
plot(tree_bf)
plot(tree_score)

plot(prune(tree,2))


  testthat::expect_equal(tree$rule$name,"p1") # p1 is strongest predictor
  testthat::expect_equal(tree_bf$rule$name,"p1") # p1 is strongest predictor
  testthat::expect_equal(tree_score$rule$name,"p1") # p1 is strongest predictor
  
})
