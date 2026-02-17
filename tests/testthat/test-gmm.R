library(OpenMx)

set.seed(98325)

data(myGrowthMixtureData)
names(myGrowthMixtureData)

nms <- names(myGrowthMixtureData)

# add a fake predictor
myGrowthMixtureData$pred <- sample(c(0,1),nrow(myGrowthMixtureData),replace=TRUE)

matrA        <- mxMatrix( type="Full", nrow=7, ncol=7,
                          free=F, values=rbind(cbind(matrix(0,5,5),
                                                     matrix(c(rep(1,5),0:4),5,2)),matrix(0,2,7)),
                          byrow=TRUE, name="A" )
labelsS      <- matrix(NA,5,5); diag(labelsS) <- "residual"
matrS        <- mxMatrix( type="Symm", nrow=7, ncol=7,
                          free=rbind(cbind(matrix(as.logical(diag(5)),5,5),
                                           matrix(F,5,2)),cbind(matrix(F,2,5),matrix(T,2,2))),
                          values=rbind(cbind(matrix((diag(5)),5,5),
                                             matrix(0,5,2)),cbind(matrix(0,2,5),matrix(c(1,.4,.4,1),2,2))),
                          labels=rbind(cbind(labelsS, matrix(NA,5,2)),cbind(matrix(NA,2,5),
                                                                            matrix(c("vari1","cov1","cov1","vars1"),2,2))),
                          byrow= TRUE, name="S" )
matrF        <- mxMatrix( type="Full", nrow=5, ncol=7,
                          free=F, values=cbind(diag(5),matrix(0,5,2)),
                          byrow=T, name="F" )
matrM        <- mxMatrix( type="Full", nrow=1, ncol=7,
                          free=c(F,F,F,F,F,T,T),
                          values=c(0,0,0,0,0,0,-1),
                          labels=c(NA,NA,NA,NA,NA,"meani1","means1"), name="M" )
exp          <- mxExpectationRAM("A","S","F","M",
                                 dimnames=c(nms,"intercept","slope"))
funML        <- mxFitFunctionML(vector=TRUE)
class1       <- mxModel("Class1", matrA, matrS, matrF, matrM, exp, funML)

matrS2       <- mxMatrix( type="Symm", nrow=7, ncol=7,
                          free=rbind(cbind(matrix(as.logical(diag(5)),5,5),
                                           matrix(F,5,2)),cbind(matrix(F,2,5),matrix(T,2,2))),
                          values=rbind(cbind(matrix((diag(5)),5,5),
                                             matrix(0,5,2)),cbind(matrix(0,2,5),matrix(c(1,.5,.5,1),2,2))),
                          labels=rbind(cbind(labelsS, matrix(NA,5,2)),cbind(matrix(NA,2,5),
                                                                            matrix(c("vari2","cov2","cov2","vars2"),2,2))),
                          byrow= TRUE, name="S2" )
matrM2       <- mxMatrix( type="Full", nrow=1, ncol=7,
                          free=c(F,F,F,F,F,T,T),
                          values=c(0,0,0,0,0,5,1),
                          labels=c(NA,NA,NA,NA,NA,"meani2","means2"), name="M2" )
exp          <- mxExpectationRAM("A","S2","F","M2",
                                 dimnames=c(nms,"intercept","slope"))

class2       <- mxModel( class1, name="Class2", matrS2, matrM2, exp )


class1@fitfunction@vector <- TRUE
class2@fitfunction@vector <- TRUE

classQ       <- mxMatrix( type="Full", nrow=2, ncol=1,
                          free=c(TRUE, FALSE), values=1, lbound=0.001,
                          labels=c("p1","p2"), name="classQuant" )

classP       <- mxAlgebra( classQuant %x% (1/sum(classQuant)), name="classProbs" )

algFit       <- mxAlgebra( -2*sum(log(classProbs[1,1] %x% Class1.fitfunction
                                      + classProbs[2,1] %x% Class2.fitfunction)),
                           name="mixtureObj")
fit          <- mxFitFunctionAlgebra("mixtureObj")
dataRaw      <- mxData( observed=myGrowthMixtureData, type="raw" )

gmm          <- mxModel("Growth Mixture Model",
                        dataRaw, class1, class2, classP, classQ, algFit, fit )

gmmFit <- mxRun(gmm)

summary(gmmFit)

library(semtree)

ctrl <- semtree_control(check.convergence = FALSE, progress.bar = TRUE,max.depth = 1)

tree <- semtree(model = gmmFit, 
                data = myGrowthMixtureData, 
                control=ctrl, 
                predictors = "pred")

plot(tree)