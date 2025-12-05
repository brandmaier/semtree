skip_on_cran()

library(lavaan)

n <- 100
var_unordered <- factor(sample(c("rain","sunshine","thunderstorm"),n,TRUE))
x <- rnorm(n)+ ifelse(var_unordered=="rain",10,0)

df <- data.frame(x, var_unordered)


model = "x ~~ x; x~mu*0"
fitted_model <- lavaan(model, df)

tree <- semtree(fitted_model, df, control=semtree_control(verbose=TRUE, report.level=99))

plot(tree)



sets <- semtree:::recodeAllSubsets(var_unordered)
