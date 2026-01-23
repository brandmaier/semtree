library(lavaan)

N <- 1000
pred <- sample(c(0,1),N,TRUE)
pred2 <- sample(c(0,1),N,TRUE)
x <- rnorm(N) 
z <- rnorm(N)
y <- (1-pred)*x+z+x*z+rnorm(N,0,0.1)
Data = data.frame(x= x,y=y,z=z,pred=pred,pred2=pred2)
lav_obj <- 'y ~ x +z + x:z'
run_lav_obj <- sem(model = lav_obj , data = Data)
summary(run_lav_obj)

tree <- semtree(model=run_lav_obj , data=Data,
                verbose = FALSE, predictors=c('pred','pred2'))

plot(tree)
