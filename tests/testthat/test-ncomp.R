context("number of comparisons")

# skip long running tests on CRAN
skip_on_cran()

set.seed(789)
library("semtree")
data(lgcm)

lgcm$agegroup <- as.ordered(lgcm$agegroup) # 1 split
lgcm$training <- as.factor(lgcm$training) # 1 split
lgcm$noise <- as.ordered(sample(c(0,1,2,3),size=nrow(lgcm),replace=TRUE)) # 3 splits

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


test_that("number of comparisons in naive split with factors (type=0)", {
  
  lgcm$noise2 <- as.factor(sample(c(0,1,2),size=nrow(lgcm),replace=TRUE)) # 3 splits
  
  control = semtree.control(method="naive",naive.bonferroni.type=0, max.depth=1)
  
  tree <- semtree(model=lgcModel, data=lgcm, control = control)
  
  expect_equal( tree$result$n.comp, 8 )
  
})



test_that("number of comparisons in naive split with factors (type=1)", {
  
  lgcm$noise2 <- as.factor(sample(c(0,1,2),size=nrow(lgcm),replace=TRUE)) # 3 splits
  
  control = semtree.control(method="naive",naive.bonferroni.type=1, max.depth=1)
  
  tree <- semtree(model=lgcModel, data=lgcm, control = control)
  
  expect_equal( tree$result$n.comp, 4 )
  
})


test_that("number of comparisons in naive split with numeric (type=0)", {
  
  data(lgcm)
  
  lgcm$agegroup <- as.ordered(lgcm$agegroup) # 1 split
  lgcm$training <- as.factor(lgcm$training) # 1 split
  lgcm$noise <- rep(1:50,8) # 5 splits
  
  control = semtree.control(method="naive",naive.bonferroni.type=0,min.bucket = 2,
                            min.N = 2, verbose=TRUE,exclude.heywood = FALSE,
                            max.depth=1)
  
  tree <- semtree(model=lgcModel, data=lgcm, control = control)
  
  expect_equal( tree$result$n.comp, 49+1+1 )
  
})


test_that("number of comparisons in naive split with numeric (type=1)", {
  
  data(lgcm)
  
  lgcm$agegroup <- as.ordered(lgcm$agegroup) # 1 split
  lgcm$training <- as.factor(lgcm$training) # 1 split
  lgcm$noise <- rep(1:50,8) # 5 splits
  
  control = semtree.control(method="naive",naive.bonferroni.type=1,min.bucket = 2,
                            min.N = 2, verbose=TRUE,exclude.heywood = FALSE,
                            max.depth=1)
  
  tree <- semtree(model=lgcModel, data=lgcm, control = control)
  
  expect_equal( tree$result$n.comp, 3 )
  
})
