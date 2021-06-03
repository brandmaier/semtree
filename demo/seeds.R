#
# Reproducibility with semtree and parallel processing
# using multisession future strategy
#
require("semtree")
require("future")

#
# (0) set up a dummy model and dataset. Scroll further down for the 
# important stuff
#

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

# compute a simple forest

forest <- semforest(model=lgcModel, data=lgcm)

#
# (1) setting seeds for local computations to make
#   variable importance computation reproducible
#
plan(sequential)

# compute variable importance
set.seed(123)
vim <- varimp(forest)

# compute variable importance again (with same seed)
set.seed(123)
vim2 <- varimp(forest)

# check whether results are identical between vim and vim2
# they really should be given the same seed
all(vim$importance==vim2$importance,na.rm = TRUE)


#
# (2) Future functions in this packages are configured
# with future.seed = T and should also return the same
# variable importance

plan(multisession)

set.seed(123)
vim <- varimp(forest)
set.seed(123)
vim2 <- varimp(forest)

all(vim$importance==vim2$importance,na.rm = TRUE)