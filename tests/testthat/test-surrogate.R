
N <- 1000
y <- rnorm(N)

modelData <- data.frame(y)

for (i in 1:10) {
  modelData[,paste0("x",i)] <- 
    factor(sample(c(0,1,2,3,4),N,replace=TRUE),ordered=TRUE)
}

modelData$x2[c(1,5,10)]<-NA
modelData$x3[c(1,2,3,4)]<-NA
rule <- list(name="x2", relation=">", value=1)

surrogate(rule = rule, mydata = modelData)    

mydata <- modelData
