set.seed(123789)

require("semtree")

data(lgcm)

lgcm$agegroup <- as.ordered(lgcm$agegroup)
lgcm$training <- as.factor(lgcm$training)
lgcm$noise <- as.numeric(lgcm$noise)
lgcm$noise2 <- rnorm(nrow(lgcm))

# randomize
lgcm$agegroup <- as.numeric(lgcm$agegroup)+rnorm(400,0,.01)
lgcm$training <- as.numeric(lgcm$training)+rnorm(400,0,0.01)
lgcm$noise <- as.numeric(lgcm$noise)+rnorm(400)

#lgcm <- lgcm[,-which(names(lgcm)%in%c("training","noise"))]
head(lgcm)

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

lgcModel <- mxRun(lgcModel)

# tree control options
# naive splitting (biased) with DM test statistic

controlOptions <- semtree.control(method = "naive",
                                  test.type="dm")

# grow tree and plot

start_time <- Sys.time()
tree <- semtree(model=lgcModel, data=lgcm, control = controlOptions)
end_time <- Sys.time()
plot(tree)

start_time2 <- Sys.time()
controlOptions$test.type<-"ml"
tree2 <- semtree(model=lgcModel, data=lgcm, control = controlOptions)
end_time2 <- Sys.time()

end_time-start_time
end_time2-start_time2