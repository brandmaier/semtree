skip_on_cran()

library(future)
library(lavaan)
library(semtree)

# generate data
# generate observations of an ordered factor with labels
set.seed(458)
n <- 1000
var_unordered <- factor(sample(c("lightning","rain","sunshine","snow"),n,TRUE))
x <- rnorm(n)+ifelse(var_unordered=="rain",20,0)
x <- x+ifelse(var_unordered=="sunshine",40,0)

df <- data.frame(x, var_unordered, p1=rnorm(N),p2=rnorm(N))
model = "x ~~ x; x ~mu*1"
fitted_model <- lavaan(model, df)

tree<-semtree(fitted_model, df,control = semtree.control(method="score"))

testrun <- function(mode="sequential"){

  if (mode=="sequential") {
    future::plan(sequential) }
  else {
    future::plan(multisession, workers=5)
  }
  
  sf<-semforest(fitted_model, df, with.error.handler=FALSE, control=semforest.control(num.trees=5, control=semtree.control(method="score")))
  
  return(sf)
}


#result1<-testrun("sequential")
result2<-testrun("parallel")



