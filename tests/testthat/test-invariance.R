#
# test invariance
# 
library(semtree)
library(lavaan)

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
eff<-10
x1 <- xdat[,1]+ ifelse(p1=="retired",eff,0)
x2 <- xdat[,2]+ ifelse(p1=="retired",eff,0)
x3 <- xdat[,3]+ ifelse(p1=="retired",eff,0)
x4 <- xdat[,4] + ifelse(p1=="retired",eff,0)
x4 <- ifelse(p3>13, x4, (x4+x3)/2)
df<-data.frame(x1,x2,x3,x4, p1, p2,p3)
model <- "L =~ 1*x1+a*x2+b*x3+c*x4"

fit_model <- lavaan::cfa(model, df)

tree<-semtree(fit_model, df, control = semtree_control(verbose=TRUE),
              constraints = semtree.constraints(local.invariance="a") )

tree
plot(tree)