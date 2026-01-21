#
# Test to ensure that likelihood estimates with focus parameters
# are identical across OpenMx and lavaan
#
# written by Andreas Brandmaier, 2025
#

testthat::test_that("Likelihood estimates with focus parameters are identical in lavaan and OpenMx SEM trees", {
library(semtree)
library(lavaan)

# load demo ata file
data(lgcm)

# create LGCM in OpenMx & run it
model = semtree:::omx_lgcm_helper(lgcm, homoscedastic_errors = FALSE)
omx_fitted = mxRun(model)
summary(omx_fitted)

# run tree in OpenMx with focus parameters on slope mean

omx_tree = semtree(model = omx_fitted,
                data = lgcm,
                constraints=semtree.constraints(focus.parameters = c("means")))

# create LGCM in lavaan and run it
lav_model = lav_lgcm_helper(homoscedastic_errors = FALSE)
lav_fitted <- lavaan(lav_model, lgcm, do.fit=TRUE)

lav_tree = semtree(model = lav_fitted,
                data = lgcm,
                control=semtree_control(verbose=TRUE),
                constraints=semtree.constraints(focus.parameters = c("mus")))

subset1 <- lgcm[lgcm$agegroup==1, ]
subset2 <- lgcm[lgcm$agegroup!=1, ]
invariance <- "mus"
model = lav_fitted
raw_model <- lav_model
#
# check identity of model estimates of OpenMx and lavaan
# template models
#

lav_ests <- parameterEstimates(lav_fitted)
omx_ests <- OpenMx::omxGetParameters(omx_fitted)

testthat::expect_equal(lav_ests$est[lav_ests$label=="mui"], as.numeric(omx_ests["meani"]), 0.0001 )
testthat::expect_equal(lav_ests$est[lav_ests$label=="mus"], as.numeric(omx_ests["means"]), 0.0001 )

#
# check identity of tree test statistics
#

testthat::expect_equal( omx_tree$lr, lav_tree$lr, tolerance = 0.00001)

testthat::expect_equal(omx_tree$right_child$lr, lav_tree$right_child$lr, tollerance = 0.00001)


})