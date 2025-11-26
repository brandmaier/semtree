skip_on_cran()

library(lavaan)

set.seed(123)

N <- 1000
grp1 <- ordered(sample(x = c(0,1), size=N, replace=TRUE))
grp2 <- ordered(sample(x = c(0,1), size=N, replace=TRUE))
grp3 <- ifelse(grp1=="0", rnorm(N,-10), rnorm(N,+10))
Sigma <- matrix(byrow=TRUE,
                nrow=2,c(2,0.2,
                         0.2,1))
obs <- MASS::mvrnorm(N,mu=c(0,0),
                     Sigma=Sigma)
obs[,1] <- obs[,1] + ifelse(grp1=="1",3,0)
obs[,2] <- obs[,2] + ifelse(grp2=="1",3,0)
df.biv <- data.frame(obs, grp1, grp2)
df.biv2 <- data.frame(obs, grp3)
names(df.biv)[1:2] <- paste0("x",1:2)
names(df.biv2)[1:2] <- paste0("x",1:2)

lav_model <- "
x1 ~~ x2
x1~~x1
x2~~x2
x1 ~ m1*0
x2 ~ m2*0"

fit_lav_model <- lavaan(lav_model, df.biv)

manifests<-c("x1","x2")
mx_model <- mxModel("Bivariate_Model", 
                     type="RAM",
                     manifestVars = manifests,
                     latentVars = c(),
                     mxPath(from="x1",to=c("x1","x2"), 
                            free=c(TRUE,TRUE), value=c(1.0,.2) , 
                            arrows=2, label=c("VAR_x1","COV_x1_x2") ),
                     mxPath(from="x2",to=c("x2"), free=c(TRUE), 
                            value=c(1.0) , arrows=2, label=c("VAR_x2") ),
                     mxPath(from="one",to=c("x1","x2"), label=c("mu1","mu2"),
                            free=TRUE, value=0, arrows=1),
                     mxData(df.biv, type = "raw")
);

tree.biv <- semtree(fit_lav_model, data=df.biv)


plot(tree.biv)

#tree.biv2 <- semtree(fit_lav_model, df.biv, constraints=
#                       semtree.constraints(focus.parameters = "mu1"))


tree.biv <- semtree(fit_lav_model, data=df.biv, control=semtree.control(method="score"))
plot(tree.biv)


testthat::expect_error(
tree.biv <- semtree(fit_lav_model, data=df.biv,
                    control=semtree.control(method="score"),
                    constraints = semtree.constraints(focus.parameters = "mu1"))
)

tree_mx_score_focus <- semtree(mx_model, data=df.biv,
                    control=semtree.control(method="score"),
                    constraints = semtree.constraints(focus.parameters = "mu1"))

test_that("return object is a valid tree", {expect_equal(class(tree_mx_score_focus),"semtree")})
test_that("tree depth is correct", {expect_equal(getDepth(tree_mx_score_focus),2)})
test_that("first split is optimal", {expect_equal(tree_mx_score_focus$rule$name,"grp1")})
test_that("first split is optimal", {expect_equal(tree_mx_score_focus$rule$value,"0")})

tree_mx_score_focus <- semtree(mx_model, data=df.biv2,
                               control=semtree.control(method="score"),
                               constraints = semtree.constraints(focus.parameters = "mu1"))
