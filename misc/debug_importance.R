#
# this is code to debug variable importance estimates for
# focus parameters
#
# here, there are three predictors and two model parameters
# x1 and x2 predict differences in mux
# x3 predicts differences in varx
#
# the effect of x1 is half the size of x2
#
# if additional variables (x4, x5, ...) are generated,
# they have no effect on the outcome
#


library(partykit)
library(semtree)
library(tictoc)

num_noise <- 1

set.seed(123)
N <- 400
x1 <- rbinom(N, size = 1, prob=.5)
x2 <- rbinom(N, size = 1, prob=.5)
x3 <- rbinom(N, size = 1, prob=.5)

y <- rnorm(N, x1+0.5*x2, sd=(2*x3+1))

x1 <- factor(x1)
x2 <- factor(x2)
x3 <- factor(x3)

sim.data <- data.frame(y,x1,x2,x3)

if (num_noise>0)
for (i in 1:num_noise) {
  xnoi <- factor(rbinom(N, size = 1, prob=.5))
  sim.data <- cbind(sim.data, xnoi)
}

names(sim.data) <- c("y", paste0("x",1:(ncol(sim.data)-1)))

manifests <- "y"
observed.model <- mxModel("Linear Growth Curve Model Path Specification",
                          type="RAM",
                          mxData(
                            sim.data,
                            type="raw"
                          ),
                          manifestVars=manifests,
                          latentVars=c(),
                          # variance
                          mxPath(
                            from=manifests,
                            arrows=2,
                            free=TRUE,
                            values = c(1),
                            labels=c("varx")
                          ),
                          
                          # means
                          mxPath(
                            from="one",
                            to=manifests,
                            arrows=1,
                            free=TRUE,
                            values=c(0),
                            labels=c("mux")
                          )
) # close model

#tree <- semtree(model = observed.model, data=sim.data,control = semtree.control(method="score"))

ctrl <- semforest_score_control(num.trees=200)

ctrl <- semforest.control(num.trees=200)
ctrl$semtree.control$method <- "score"

tic()

#parallel::makeCluster(7)
library(future)
plan(multisession, workers=15)
#plan(sequential)
frst <- semforest(model = observed.model, data=sim.data,
                  control = ctrl,
                  constraints = semtree.constraints(focus.parameters = "mux"))
#parallel::stopCluster()

vim <- semtree::varimp(frst)
vimp <- semtree::varimp(frst,method="permutationFocus")

plot(vimp)

frst2 <- semforest(model = observed.model, data=sim.data,
                  control = ctrl,
                  constraints = semtree.constraints(focus.parameters = "varx"))
vimp2 <- semtree::varimp(frst2,method="permutationFocus")

toc()

plot(vimp2)
plot(vimp)

semtree:::varimpTree( frst$forest[[5]], frst$forest.data[[5]],method = "permutationFocus",var.names = "x2" )

#semtree:::varimpFocus( frst$forest[[5]], frst$forest.data[[5]], cov.name = "x1")

#debug(semtree:::varimpFocus)

# plot(vimp) should show that there is an effect of x1 and (with half the effect size) an
# effect of x2
plot(vimp)

# plot(vimp2) should show that there is only an effect of x3
plot(vimp2)
