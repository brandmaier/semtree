library(lavaan)
library(semtree)
set.seed(1238)

N <- 500

# simulate data
da <- data.frame(y = c(rnorm(N/2, mean = -1), rnorm(N/2, mean = 1)),
                 z = factor(rep(c(0,1),each=N/2)),k=rnorm(N),m=rnorm(N) )

m_lav <- '
y ~~ y
y ~ 1
'

fit_lav <- lavaan(model = m_lav, data = da)


tree = semtree(model=fit_lav, data=da, 
               control = semtree_control(method="score"),
               forced_splits=NULL)



tree_forced_m = semtree(model=fit_lav, data=da, 
               control = semtree_control(method="score"),
               forced_splits=c("m"))

