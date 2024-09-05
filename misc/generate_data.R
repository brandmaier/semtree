#
# generate noise variables
#
N <- 1000
noise1 <- sample(c(0,1),size=N, replace=TRUE)
noise2 <- sample(c(0,1,2,3),size=N, replace=TRUE)
noise3 <- sample(1:10,size=N, replace=TRUE)
noise4 <- rnorm(N)

pred1 <- rnorm(N)
pred2 <- ifelse(pred1+rnorm(N,0,0.1)>0,1,0)
pred3 <- rnorm(N)

latvar <- ifelse(pred1>0, 0, 5)+rnorm(N)

obs <- latvar %*% t(c(0.8,0.9,0.7))
obs <- obs + rnorm(N*3,0,0.1)
