
# simulate uninformative covariates

z1 <- sample(c(1:2),size = N,replace=TRUE)
z2 <- sample(c(1:4),size = N,replace=TRUE)
z3 <- sample(c(1:10),size = N,replace=TRUE)
z4 <- rnorm(N)

# simulate CFA
lat <- rnorm(N)+ifelse(z>0,1,0)+ifelse(z>0.5,3,0)
loadings <- c(.5,.7,.9,.3)
obs <- t(matrix(loadings) %*% t(lat)) + rnorm(N*length(loadings),0,err)


modelData <- data.frame( cbind(obs,z1,z2,z3,z4))
names(modelData) <- c(paste0("x",1:4), paste0("z",1:4))

require("OpenMx");
manifests<-c("x2","x3","x1","x4")
latents<-c("YY")
model <- mxModel("Unnamed_Model", 
                 type="RAM",
                 manifestVars = manifests,
                 latentVars = latents,
                 mxPath(from="YY",to=c("x2","x3","x1","x4"), free=c(TRUE,TRUE,TRUE,TRUE), value=c(0.0,0.0,0.1,.5) , arrows=1, label=c("YY__x2","YY__x3","YY__x1","YY__x4") ),
                 mxPath(from="YY",to=c("YY"), free=c(FALSE), value=c(1.0) , arrows=2, label=c("VAR_YY") ),
                 mxPath(from="x2",to=c("x2"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x2") ),
                 mxPath(from="x3",to=c("x3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x3") ),
                 mxPath(from="x1",to=c("x1"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x1") ),
                 
                 mxPath(from="x4",to=c("x4"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x4") ),
                 mxPath(from="one",to=c("x2","x3","x1","x4","YY"), free=c(F,F,F,F,T), 
                        value=0, arrows=1,label=c("m2","m3","m1","m4","mYY")),
                 mxData(modelData, type = "raw")
);

result <- mxRun(model)
summary(result)

omxGetParameters(result)
# create SEM tree


controlOptions <- semtree.control(method = "naive",
                                  test.type="dm",
                                  min.bucket = 1,
                                  alpha=1,
                                  max.depth = 2)

# grow tree and plot

start_time <- Sys.time()
trees <- replicate(semtree(model=model, 
                           data=modelData, control = controlOptions),n = 10,
                   simplify = FALSE)
end_time <- Sys.time()
plot(tree)