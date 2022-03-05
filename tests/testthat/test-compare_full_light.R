if(FALSE){
  # DEMO FOR SEMTREE
  require("semtree")
  require("lavaan")
  require("future")
  plan(sequential)
  data(lgcm)
  
  lgcm$agegroup <- as.ordered(lgcm$agegroup)
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
  
  # TREE CONTROL OPTIONS.
  # TO OBTAIN BASIC/DEFAULT SMETREE OPTIONS, SIMPLY TPYE THE FOLLOWING:
  
  controlOptions <- semforest.control()
  
  # THE CONTENTS OF THE DEFAULT CONTROLS CAN THEN BE VIEWED.
  
  controlOptions
  
  # AND MODEL OPTIONS CAN BE CHANGED BY REDEFINING ELEMENTS OF THE 
  # CONTROL OBJECT.
  
  
  
  # RUN TREE.
  
  forest <- semforest(model=lgcModel, data=lgcm,control = controlOptions)
 
  # PARTIAL DEPENDENCE
  
  # Make light forest
  f_light <- clear_underbrush(forest)

  ptm <- proc.time()
  pd <- partialDependence(forest, "agegroup", "g0~1", support = 10)  # g0 ~1
  time_old <- proc.time() - ptm
  df <- forest$data
  df$agegroup <- as.numeric(as.character(df$agegroup))
  ptm <- proc.time()
  f_light <- clear_underbrush(forest)
  pd2 <- pd(f_light, data = df, reference.var = "agegroup", point = list(agegroup = c(0, 1)))
  time_new <- proc.time() - ptm
  expect_equivalent(pd2$`g0~1`, unlist(pd$dict), tolerance = .02)
  #expect_true(time_old[1] > time_new[1])
  
}
