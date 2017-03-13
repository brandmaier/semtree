## SEM Forest demo using parallel package

require("semtree")

# ORGANIZE DATA BY TYPE FOR COVARIATES.
# SOME COVARIATES ARE ORGANIZED IN THE DATA. MODEL VARIABLES AND 
# CAOVARIATES DO NOT NEED TO BE ORDERED IN THE DATASET. ONLY VARIABLE
# TYPE NEEDS TO BE DEFINED FOR THE COVARIATES (FACTOR/ORDERED/NUMERIC)
# BE AWARE OF THE EXPONENTIAL GROWTH OF MODEL COMPARISON WITH INCREASES
# IN NUMBER OF FACTORS PER A COVARIATE.

data(lgcm)

lgcm$agegroup <- as.ordered(lgcm$agegroup)
#lgcm$training <- factor(lgcm$training, levels=c("0","1"),labels=c("YES","NO"))
lgcm$training <- factor(lgcm$training)
lgcm$noise <- as.numeric(lgcm$noise)

#resample noise, add missingness
lgcm$noise <- sample(1:5, size = length(lgcm$noise), replace = TRUE)
lgcm$noise <- ifelse(
  rnorm(n = length(lgcm$noise),0,1)>1
  ,NA,1) * lgcm$noise

#lgcm$agegroup <- ifelse(
#  rnorm(n = length(lgcm$agegroup),0,1)>1
#  ,NA,1) * lgcm$agegroup

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

tree <- semtree(lgcModel, lgcm)

# create a tiny forest

control <- semforest.control(num.trees = 7)

cl <- parallel::makeCluster(7)

forest <- semforest(model=lgcModel, data=lgcm, control=control, cluster=cl)

#tdep <- partialDependence(forest, reference.var="training", reference.par="means")
depnoise <- partialDependence(forest, reference.var="noise", reference.par="means")
depage <- partialDependence(forest, reference.var="agegroup", reference.par="means")


opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(depnoise)
plot(depnoise, lwd=3,ylim=c(-0.5,0), type="b", ylab="Mean Slope")
plot(depage)
plot(depage, lwd=3,ylim=c(-0.5,0), type="b", ylab="Mean Slope")
par(opar)

reference.var <- "training"
reference.param <- "means"
support <- 10
