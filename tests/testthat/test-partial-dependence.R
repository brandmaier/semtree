context("test partial dependence")

# skip long running tests on CRAN
skip_on_cran()

# draw predictors randomly
N <- 200
pred1 <- factor(sample(c("red","green","blue"),N,replace=TRUE))
pred2 <- ordered(sample(c(0,1,2),N,replace=TRUE))

x <- rnorm(N)+ifelse(pred2=="1",10,0)
df <- data.frame(x,pred1, pred2)

model = "x ~~ var*x; x~ mu*0"
fitted_model <- lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree.control(verbose=TRUE,report.level = 99))
plot(tree)


forest = semforest(fitted_model, df)

pd = partialDependence(forest, reference.var="pred1")
