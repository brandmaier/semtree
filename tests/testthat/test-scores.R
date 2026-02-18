set.seed(4653)

N <- 1000

d <- data.frame(MASS::mvrnorm(n = N,
                              Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
                              mu = c(0, 0)))
d$cov <- rnorm(n = N)
man_vars <- c("y1", "y2")
colnames(d) <- man_vars

m_mx <- OpenMx::mxModel(
  manifestVars = man_vars,
  type = "RAM",
  OpenMx::mxPath(from = man_vars, arrows = 2, connect = "unique.pairs",
                 free = TRUE, values = c(1, 0.5, 1)),
  OpenMx::mxPath(from = "one", to = man_vars, arrows = 1, free = TRUE,
                 values = 0),
  OpenMx::mxData(observed = d, type = "raw")
)

m_lav <- '
y1 ~~ y1
y2 ~~ y1
y2 ~~ y2

y1 ~ 1
y2 ~ 1
'

fit_mx <- OpenMx::mxRun(m_mx)

fit_lav <- lavaan::lavaan(model = m_lav, data = d)

ctrl <- semtree::semtree_control(method = "score")
ctrl <- c(ctrl, list(scores_info = semtree:::OpenMx_scores_input(
  x = fit_mx,
  control = ctrl
)))

scores_mx_analytically <- semtree:::mxScores(x = fit_mx, control = ctrl)
scores_mx_numerically <- -0.5 * OpenMx::imxRowGradients(fit_mx)

scores_lav <- lavaan::lavScores(fit_lav)

(scores_mx_analytically)[1,]
(scores_mx_numerically)[1,]
(scores_lav)[1, ]
