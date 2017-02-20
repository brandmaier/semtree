#
# Reproducibility with semtree and parallel processing
# using seeds and seeds on clusters
#
require("semtree")

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
# (2) This is how to *NOT* set seeds when using parallel computations
#

# compute variable importance using cluster
cl <- parallel::makeCluster(4)
set.seed(123)
vim <- varimp(forest, cluster=cl)

set.seed(123)
vim2 <- varimp(forest, cluster=cl)

# check whether results are identical between vim and vim2
# they will with probability of almost 1 be not identical
# since seeds were set only on the local process but not on
# the cluster processes
all(vim$importance==vim2$importance,na.rm = TRUE)


#
# (3) This is how to set seeds when using parallel computations
#
parallel::clusterSetRNGStream(cl, 123)
vim <- varimp(forest, cluster=cl)

parallel::clusterSetRNGStream(cl, 123)
vim2 <- varimp(forest, cluster=cl)

all(vim$importance==vim2$importance,na.rm = TRUE)