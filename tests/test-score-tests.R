context("test basic splitting based on level of covariate with score tests")

library(lavaan)

# skip long running tests on CRAN
skip_on_cran()

# generate observations of an ordered factor with labels
n <- 1000
var_unordered <- factor(sample(c("lightning","rain","sunshine","snow"),n,TRUE))
x <- rnorm(n)+ifelse(var_unordered=="rain",20,0)
x <- x+ifelse(var_unordered=="sunshine",40,0)

df <- data.frame(x, var_unordered)
model = "x ~~ x; x ~mu*1"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(method="score"))

plot(tree)


# 
n <- 1000
var_ordered <- ordered(sample(c("A","B","C","D"), n, replace=TRUE))
x <- rnorm(n) + ifelse(var_ordered>"C",10,0)

df <- data.frame(x, var_ordered)
model = "x ~~ x; x ~mu*1"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(method="score"))

