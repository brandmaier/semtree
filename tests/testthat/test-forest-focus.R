# skip parallel tests on CRAN
skip_on_cran()


require("semtree")
data(lgcm)

library(future)
future::plan(multisession, workers=5)

lgcm$agegroup <- ordered(lgcm$agegroup)
lgcm$training <- factor(lgcm$training)
lgcm$noise <- as.numeric(lgcm$noise)

# LOAD IN OPENMX MODEL.
# A SIMPLE LINEAR GROWTH MODEL WITH 5 TIME POINTS FROM SIMULATED DATA

manifests <- names(lgcm)[1:5]
lgcModel <- mxModel("Linear Growth Curve Model Path Specification",
                    type="RAM",
                    manifestVars=manifests,
                    latentVars=c("intercept","slope"),
                    # residual variances
                    mxPath(
                      from=manifests,
                      arrows=2,
                      free=TRUE,
                      values = c(1, 1, 1, 1, 1),
                      labels=c("residual1","residual2","residual3","residual4","residual5")
                    ),
                    # latent variances and covariance
                    mxPath(
                      from=c("intercept","slope"),
                      connect="unique.pairs",
                      arrows=2,
                      free=TRUE,
                      values=c(1, 1, 1),
                      labels=c("vari", "cov", "vars")
                    ),
                    # intercept loadings
                    mxPath(
                      from="intercept",
                      to=manifests,
                      arrows=1,
                      free=FALSE,
                      values=c(1, 1, 1, 1, 1)
                    ),
                    # slope loadings
                    mxPath(
                      from="slope",
                      to=manifests,
                      arrows=1,
                      free=FALSE,
                      values=c(0, 1, 2, 3, 4)
                    ),
                    # manifest means
                    mxPath(
                      from="one",
                      to=manifests,
                      arrows=1,
                      free=FALSE,
                      values=c(0, 0, 0, 0, 0)
                    ),
                    # latent means
                    mxPath(
                      from="one",
                      to=c("intercept", "slope"),
                      arrows=1,
                      free=TRUE,
                      values=c(1, 1),
                      labels=c("meani", "means")
                    ),
                    mxData(lgcm,type="raw")
)

#tree <- semtree(model=lgcModel, data=lgcm)

#lgcModel=mxRun(lgcModel)

# TREE CONTROL OPTIONS.
# TO OBTAIN BASIC/DEFAULT SMETREE OPTIONS, SIMPLY TPYE THE FOLLOWING:

ctrl <- semtree.control(method = "score", verbose = TRUE)

# RUN TREE.

forest <- semforest(model=lgcModel, data=lgcm, control = 
                      semforest.control(num.trees = 30, control=semtree.control(alpha=1,method="score")),
                    constraints=semtree.constraints(focus.parameter="meani"))

vim_naive <- varimp(forest)

vim <- varimp(forest, method = "permutationFocus")

#plot(vim)
#plot(vim_naive)

aggregate()