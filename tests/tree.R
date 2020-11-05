set.seed(789)
require("semtree")
data(lgcm)

lgcm$agegroup <- as.ordered(lgcm$agegroup)
lgcm$training <- as.factor(lgcm$training)
lgcm$noise <- as.factor(lgcm$noise)

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

controlOptions <- semtree.control(method = "naive")
controlOptions$alpha <- 0.05

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

summary(tree)

summary(treeConstrained)

print(tree)

parameters(tree)

parameters(tree, leafs.only=FALSE)

treeSub <- subtree(tree, startNode=3)

if (!is.null(treeSub))
	plot(treeSub)

toTable(tree)


controlOptions <- semtree.control(method = "fair")

tree2 <- semtree(lgcModel, lgcm, control=controlOptions)


controlOptions <- semtree.control(method = "score")
tree3 <- semtree(lgcModel, lgcm, control=controlOptions)