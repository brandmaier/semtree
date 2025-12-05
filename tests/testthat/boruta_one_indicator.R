influence <- c(1, 0)
nPred <- length(influence)
cutbreaks <- 3
make_categorical <- TRUE


genModel <- mxModel(type="RAM", manifestVars=c("Y", paste0("X", 1:nPred)),
                    mxPath(paste0("X", 1:nPred), "Y", values=influence),
                    mxPath(c("Y", paste0("X", 1:nPred)), arrows=2, values=1))
simpleData <- mxGenerateData(genModel, N)
if(make_categorical) {
  for(i in paste0("X", 1:nPred)) {
    simpleData[[i]] <- cut(simpleData[[i]], cutbreaks)
    simpleData[[i]] <- mxFactor(simpleData[[i]], levels(simpleData[[i]]))
  }
}

testModel <- mxModel("SimplisticModel", type="RAM", manifestVars="Y", 
                     mxPath("Y", arrows=2, values=1, free=TRUE, labels=c("Var")),
                     mxPath("one", "Y", values=0, free=TRUE, labels=c("mu")), 
                     mxData(simpleData, type="raw"))

control <- semforest_control(control=semtree_control(method="score", alpha=1))

output <- boruta(testModel, simpleData, verbose=TRUE, control = control)
output