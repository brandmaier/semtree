
# skip long running tests on CRAN
skip_on_cran()

require("semtree")
data(lgcm)

lgcm$agegroup <- ordered(lgcm$agegroup)
lgcm$training <- factor(lgcm$training)
lgcm$noise <- as.numeric(lgcm$noise)
lgcm$noise2 <- factor(sample(c(0,1,2),size=length(lgcm$agegroup),replace=TRUE))
lgcm$noise3 <- factor(sample(c(0,1,2,3),size=length(lgcm$agegroup),replace=TRUE))
lgcm$noise4 <- ordered(sample(c(0,1,2,3,4,5),size=length(lgcm$agegroup),replace=TRUE))

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

#lgcModel=mxRun(lgcModel)

# TREE CONTROL OPTIONS.
# TO OBTAIN BASIC/DEFAULT SMETREE OPTIONS, SIMPLY TPYE THE FOLLOWING:

ctrl <- semtree.control(method = "score", verbose = TRUE)

# RUN TREE.
min.N = 30

forest <- semforest(model=lgcModel, data=lgcm, control = 
                      semforest.control(num.trees = 20, control=semtree.control(method="score",
                                                                min.N = min.N
                                                                )))


find_smallest_node <- function(tree) {
  if (tree$caption == "TERMINAL") return ( tree$N)
  else {
    return(
      min(
        find_smallest_node(tree$left_child),
        find_smallest_node(tree$right_child)
      )
    )
  }
}

observed.min.N <- sapply(FUN=find_smallest_node, forest$forest)

testthat::expect_true(
  all(observed.min.N >= min.N)
)
