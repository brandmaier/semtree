# DEMO FOR SEMTREE!

# INSTALL THE PACKAGE
require("semtree")
require("future")
plan(multisession)

# ORGANIZE DATA BY TYPE FOR COVARIATES.
# SOME COVARIATES ARE ORGANIZED IN THE DATA. MODEL VARIABLES AND 
# CAOVARIATES DO NOT NEED TO BE ORDERED IN THE DATASET. ONLY VARIABLE
# TYPE NEEDS TO BE DEFINED FOR THE COVARIATES (FACTOR/ORDERED/NUMERIC)
# BE AWARE OF THE EXPONENTIAL GROWTH OF MODEL COMPARISON WITH INCREASES
# IN NUMBER OF FACTORS PER A COVARIATE.

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


# TREE CONTROL OPTIONS.
# TO OBTAIN BASIC/DEFAULT SMETREE OPTIONS, SIMPLY TPYE THE FOLLOWING:

controlOptions <- semtree.control()

# THE CONTENTS OF THE DEFAULT CONTROLS CAN THEN BE VIEWED.

controlOptions

# AND MODEL OPTIONS CAN BE CHANGED BY REDEFINING ELEMENTS OF THE 
# CONTROL OBJECT.

controlOptions$alpha <- 0.01

# RUN TREE.

tree <- semtree(model=lgcModel, data=lgcm, control = controlOptions)

# RERUN TREE WITH MODEL CONSTRAINTS.
# MODEL CONSTRAINTS CAN BE ADDED BY IDENTIFYING THE PARAMETERS TO BE
# CONSTRAINED IN EVERY NODE. ONLY UNCONSTRAINED PARAMETERS ARE THEN
# TESTED AT EACH NODE FOR GROUP DIFFERENCES. IN THIS EXAMPLE THE MODEL 
# RESIDUALS ARE CONSTRAINED OVER THE NODES.

constraints <- semtree.constraints(global.invariance = names(omxGetParameters(lgcModel))[1:5])

treeConstrained <- semtree(model=lgcModel, data=lgcm, control = controlOptions,
                           constraints=constraints)

# SEE PLOT.
# THE PLOT FUNCTION WILL SHOW ALL FREE PARAMETERS AT EACH TERMINAL NODE.
# THIS CAN CREATE UNREADABLE FIGURES FOR MODELS WITH MANY FREE PARAMETERS.

plot(tree)

# SEE SUMMARY.
# OVERALL MODEL FACTS ARE GIVEN WITH THE SUMMARY FUNCTION.

summary(tree)

summary(treeConstrained)

# USING THE PRINT FUNCTION WILL PROVIDE AN OVERVIEW OF THE MODEL
# SPLITS, LR TESTS WITH DF, AND SUBGROUP SIZES (N).

print(tree)

# GET NODE PARAMETERS (leafs only as default).
# THE DEFAULT IS ONLY DISPLAYING LEAF NODE PARAMETER ESTIMATES.

parameters(tree)

# TO GET ALL PARAMETER ESTIMATES FOR EVERY NODE CHANGE THE DEFAULT TO.

parameters(tree, leafs.only=FALSE)

# SUBTREE FUNCTION.
# IF ONLY A SUBSET OF THE TREE IS TO BE EXAMINED, THEN THE TREE CAN 
# BE PARED. THIS IS DONE WITH A SUBSET FUNCTION WHICH STORES THE TREE
# FROM A GIVEN NODE ON. IF THE TREE FROM NODE 2 ON IS DESIRED, THEN:

treeSub <- subtree(tree, startNode=3)

# ALL OF THE PREVIOUS FUNCTIONS CAN THEN BE APPLIED TO THE SUBTREE 
# FOR FURTHER EXAMINATION.

plot(treeSub)

