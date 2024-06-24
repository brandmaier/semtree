# Tests a simple boruta model on a basic one-indicator SEM

library(OpenMx)

N <- 1000
influence <- c(1, 0, 0, 0)
nPred <- length(influence)


genModel <- mxModel(type="RAM", manifestVars=c("Y", paste0("X", 1:nPred)),
                    mxPath(paste0("X", 1:nPred), "Y", values=influence),
                    mxPath(c("Y", paste0("X", 1:nPred)), arrows=2, values=1))
simpleData <- mxGenerateData(genModel, 1000)

testModel <- mxModel("SimplisticModel", type="RAM", manifestVars="Y", 
                     mxPath("Y", arrows=2, values=1, free=TRUE),
                     mxPath("one", "Y", values=0, free=TRUE), 
                     mxData(simpleData, type="raw"))

boruta(testModel, simpleData, verbose=TRUE)