# DEMO FOR SEMTREE
require("semtree")
require("lavaan")
require("future")
plan(sequential)
data(lgcm)
# Note: Can't use as.ordered, because this is handled incorrectly in semtree.
lgcm$agegroup <- as.factor(lgcm$agegroup)
lgcm$training <- as.factor(lgcm$training)
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

forest <- semforest(model=lgcModel, data=lgcm,control = controlOptions)
f_light <- clear_underbrush(forest)

test_that("pd is equivalent to partialDepencence", {
  ptm <- proc.time()
  pd <- partialDependence(forest, "agegroup", "g0~1", support = 10)  # g0 ~1
  time_old <- proc.time() - ptm
  df <- forest$data
  #df$agegroup <- as.numeric(as.character(df$agegroup))
  ptm <- proc.time()
  pd2 <- pd(f_light, data = df, reference.var = "agegroup", support = 10)
  time_new <- proc.time() - ptm
  expect_equivalent(pd2$`g0~1`, unlist(pd$dict), tolerance = .02)
  #expect_true(time_old[1] > time_new[1])
})

test_that("pd works for semforest", {
  expect_error({pd3 <- pd(forest, reference.var = "agegroup")}, NA)
})

test_that("pd works for interactions", {
  
  expect_error({pd3 <- pd(f_light, lgcm, reference.var = c("agegroup", "training"), mc = 10)}, NA)
  pd3 <- pd(f_light, lgcm, reference.var = c("agegroup", "training"), mc = 10)
  expect_true(nrow(pd3) == 4)
})
