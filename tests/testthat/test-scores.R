library(MASS)
library(semtree)
library(lavaan)

N <- 1000

d <- data.frame(MASS::mvrnorm(n = N,
                              Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
                              mu=c(0,0)))
d$cov <- rnorm(n = N)
man_vars <- c("y1", "y2")
colnames(d) <- man_vars

m <- mxModel(manifestVars = man_vars,
             type = "RAM",
             mxPath(from = man_vars, arrows = 2, connect = "unique.pairs",
                    free = TRUE, values = c(1, 0.5, 1)),
             mxPath(from = "one", to = man_vars, arrows = 1, free = TRUE,
                    values = 0),
             mxData(observed = d, type = "raw"))

fit <- mxRun(m)

ctrl <- semtree_control(method = "score")
ctrl <- c(ctrl, list(scores_info = semtree:::OpenMx_scores_input(
  x = fit,
  control = ctrl
)))

scores_analytically <- semtree:::mxScores(x = fit, control = ctrl)
scores_numerically <- imxRowGradients(fit)


(scores_analytically)[1,]
(scores_numerically)[1,]

