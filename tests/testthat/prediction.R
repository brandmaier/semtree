skip_on_cran()

N<-100
x<-rnorm(N)
y<-rnorm(N)
pred1 <- ordered( sample(c(0,1,2),N,replace=TRUE) )
pred2 <- sample(0:10, N, replace=TRUE)

y2 <- ifelse(pred2>5,0.2*y+0.8*x,x)              

model <- "x~~y; x~~x;y~~y"

df <- data.frame(x,y=y2,pred1,pred2)

fitted_model <- lavaan(model,df)
tree <- semtree(fitted_model, df)
plot(tree)

forest <- semforest(fitted_model, df)

pars_pred <- predict(forest, df, "pars")


#pars_pred <- predict(strip(forest), df, "pars")

