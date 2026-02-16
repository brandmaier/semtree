n <- 500
set.seed(233453)
x = rnorm(n)
var_ordered <- ordered( sample(c(1,2,3,4), 
                               size=n, prob=rep(.25,4), 
                               replace = TRUE))
var_ordered_named <- ordered( sample(c(1,2,3,4), 
                                     size=n, prob=rep(.25,4),
                                     replace = TRUE),
                              labels=c("one","two","three","four"))
x <- x * ifelse( (var_ordered_named=="one"), .5, 10) 
df <- data.frame(x, var_ordered_named)
model = "x ~~ x"
fitted_model <- lavaan::lavaan(model, df)
tree = semtree(fitted_model, df, control=semtree_control())

forest = semforest(fitted_model, df, control=semforest_control(logfile=FALSE))
forest = semforest(fitted_model, df, control=semforest_control(logfile=TRUE))
forest = semforest(fitted_model, df, control=semforest_control(logfile=))
