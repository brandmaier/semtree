 testthat::skip_on_cran()

 test_that("varimp returns expected structure", {
   set.seed(789)
   data(lgcm)
   lgcm$agegroup <- ordered(lgcm$agegroup)
   lgcm$training <- factor(lgcm$training)
   lgcm$noise <- factor(lgcm$noise)
   lgcm$noise2 <- factor(sample(c(0,1,2), nrow(lgcm), TRUE))

   manifests <- names(lgcm)[1:5]
   lgcModel <- mxModel(
     "Linear Growth Curve Model Path Specification",
     type = "RAM",
     manifestVars = manifests,
     latentVars = c("intercept", "slope"),
     mxPath(from = manifests, arrows = 2, free = TRUE,
            values = c(1, 1, 1, 1, 1),
            labels = c("residual1", "residual2", "residual3", "residual4", "residual5")),
     mxPath(from = c("intercept", "slope"), connect = "unique.pairs",
            arrows = 2, free = TRUE, values = c(1, 1, 1),
            labels = c("vari", "cov", "vars")),
     mxPath(from = "intercept", to = manifests,
            arrows = 1, free = FALSE, values = c(1, 1, 1, 1, 1)),
     mxPath(from = "slope", to = manifests,
            arrows = 1, free = FALSE, values = c(0, 1, 2, 3, 4)),
     mxPath(from = "one", to = manifests,
            arrows = 1, free = FALSE, values = c(0, 0, 0, 0, 0)),
     mxPath(from = "one", to = c("intercept", "slope"),
            arrows = 1, free = TRUE, values = c(1, 1),
            labels = c("meani", "means")),
     mxData(lgcm, type = "raw")
   )

   ctrl <- semforest.control(num.trees = 10)
   ctrl$semtree.control$method <- "score"

   fr <- semforest(lgcModel, lgcm, control = ctrl)
   vimp <- varimp(fr)

   expect_true(is.list(vimp))
   expect_true(all(c("var.names", "importance") %in% names(vimp)))

   lgcm <- lgcm[, sample(1:ncol(lgcm), ncol(lgcm), FALSE)]
   fr2 <- semforest(lgcModel, lgcm, control = ctrl)
   vimp2 <- varimp(fr2)

   expect_true(is.list(vimp2))
   expect_true(all(c("var.names", "importance") %in% names(vimp2)))
 })
