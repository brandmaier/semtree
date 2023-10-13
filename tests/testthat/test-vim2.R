skip_on_cran()

set.seed(789)
require("semtree")
data(lgcm)

lgcm$agegroup <- ordered(lgcm$agegroup)
lgcm$training <- factor(lgcm$training)
lgcm$noise <- factor(lgcm$noise)
lgcm$noise2 <- factor(sample(c(0,1,2), nrow(lgcm),TRUE))

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

ctrl <-  semforest.control(num.trees = 10)
ctrl$semtree.control$method <- "score"

fr <- semforest(lgcModel, lgcm, control = ctrl)


vimp <- varimp(fr)

print(vimp)
print(vimp, na.omit=TRUE)

# reorder dataset

lgcm <- lgcm[, sample(1:ncol(lgcm),ncol(lgcm),FALSE)]
fr2 <- semforest(lgcModel, lgcm, control = ctrl)
vimp2 <- varimp(fr2)

print(vim, scale="relative.baseline")
