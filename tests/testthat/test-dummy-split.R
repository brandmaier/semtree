library(lavaan)


set.seed(458)
n <- 500
var_unordered <- factor(sample(c("lightning","rain","sunshine","snow"),n,TRUE))
var_grp <- factor((var_unordered %in% c("rain","sunshine"))) 
x <- rnorm(n)+ifelse(var_grp==TRUE,20,0)

# data frame has only a dummy predictor
df <- data.frame(x=x, var_grp)

fit <- lavaan("x~~x",df)

tree <- semtree(fit, df, control = semtree.control(method="score"))

testthat::expect_equal(tree$rule$name, "var_grp")
testthat::expect_equal(tree$rule$value, "FALSE")
testthat::expect_equal(tree$left_child$N, 271)

