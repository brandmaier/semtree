# DEMO FOR SEMTREE
require("semtree")
require("lavaan")
#require("future")
#plan(sequential)
data(lgcm)

lgcm$agegroup <- ordered(lgcm$agegroup)
lgcm$training <- factor(lgcm$training)
lgcm$noise <- as.numeric(lgcm$noise)

# LOAD IN LAVAAN MODEL.
# A SIMPLE LINEAR GROWTH MODEL WITH 5 TIME POINTS FROM SIMULATED DATA

lgcModelstr <- '
g0 =~ 1*o1 + 1*o2 + 1*o3 + 1*o4 + 1*o5;
g1 =~ 0*o1 + 1*o2 + 2*o3 + 3*o4 + 4*o5;
g0 ~~ g0; g0 ~ 1;
g1 ~~ g1; g1 ~ 1;
g0 ~~ g1;
o1 ~~ o1; o1 ~ 0*1;
o2 ~~ o2; o2 ~ 0*1;
o3 ~~ o3; o3 ~ 0*1;
o4 ~~ o4; o4 ~ 0*1;
o5 ~~ o5; o5 ~ 0*1;
'
lgcModel <- lavaan(lgcModelstr, lgcm, do.fit=TRUE)

# RUN TREE.

controlOptions = semforest.control()

forest <- semforest(model=lgcModel, data=lgcm,control = controlOptions)
stripped_forest <- strip(forest)


test_that("partialDependence works for semforest", {
  expect_error({partialDependence3 <- partialDependence(forest, reference.var = "agegroup")}, NA)
})

test_that("partialDependence works for interactions", {
  
  expect_error({partialDependence3 <- partialDependence(stripped_forest, lgcm, reference.var = c("agegroup", "training"), mc = 10)}, NA)
  partialDependence3 <- partialDependence(stripped_forest, lgcm, reference.var = c("agegroup", "training"), mc = 10)
  expect_true(nrow(partialDependence3$samples) == 4)
})
