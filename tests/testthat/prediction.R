skip_on_cran()

library(semtree)
# generate data with two observed variables x,y
# and two predictors
#
N<-1000
x<-rnorm(N)
y<-rnorm(N)
pred1 <- ordered( sample(c(0,1,2),N,replace=TRUE) )
pred2 <- sample(0:10, N, replace=TRUE)

y2 <- ifelse(pred2>5,0.2*y+0.8*x,x)              

# define a fully saturated model
model <- "x~~y; x~~x;y~~y"

sim_data <- data.frame(x,y=y2,pred1,pred2)

fitted_model <- lavaan::lavaan(model,sim_data)
tree <- semtree(fitted_model, sim_data, control = semtree_control(method="score",verbose=TRUE))
plot(tree)

forest <- semforest(fitted_model, sim_data)

pars_pred <- predict(forest, sim_data, "pars")


#pars_pred <- predict(strip(forest), sim_data, "pars")

