## SEM Forest demo

# INSTALL THE PACKAGE
require("semtree")

# ORGANIZE DATA BY TYPE FOR COVARIATES.
# SOME COVARIATES ARE ORGANIZED IN THE DATA. MODEL VARIABLES AND 
# CAOVARIATES DO NOT NEED TO BE ORDERED IN THE DATASET. ONLY VARIABLE
# TYPE NEEDS TO BE DEFINED FOR THE COVARIATES (FACTOR/ORDERED/NUMERIC)
# BE AWARE OF THE EXPONENTIAL GROWTH OF MODEL COMPARISON WITH INCREASES
# IN NUMBER OF FACTORS PER A COVARIATE.

data(lgcm)

lgcm$agegroup <- as.ordered(lgcm$agegroup)
lgcm$training <- as.factor(lgcm$training)
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


# create forest (default = only 5 trees for testing purposes, use control object to create much more)

forest <- semforest(model=lgcModel, data=lgcm)

# create second, larger forest with 20 trees (still a small forest)

control <- semforest.control(num.trees = 20)

forest2 <- semforest(model=lgcModel, data=lgcm, control=control)

# merge trees from both forest
bigforest <- merge(forest, forest2)

# compute variable importance

vim <- varimp(bigforest)

# plot importance
print(vim)
plot(vim)

# diversity matrix
df <- diversityMatrix(forest = forest)
plot(df)