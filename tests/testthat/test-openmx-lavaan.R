#context("Test equivalence of lavaan and OpenMx results in a simple model")

skip_on_cran()

library(semtree)
library(lavaan)
library(OpenMx)

set.seed(1808)

Data <- data.frame(y = c(rnorm(150, mean = -1), rnorm(150, mean = 1)),
                   z = sort(rnorm(300)))

m_lav <- '
y ~~ y
y ~ 1
'

####Testing semtree with lavaan models ####
fit_lav <- lavaan(model = m_lav, data = Data)

tree_lav <- semtree::semtree(model = fit_lav, data = Data,
                             control = semtree.control(use.maxlr = TRUE, method = "naive"))


tree_lav <- semtree::semtree(model = fit_lav, data = Data)


plot(tree_lav)



m_mx <- mxModel(manifestVars = "y", type = "RAM",
mxData(observed = Data, type = "raw"),
mxPath(from = "y", arrows = 2, free = TRUE, values = 1, labels = "sigma2"),
mxPath(from = "one", to = "y", arrows = 1, free = TRUE, values = 0, labels = "mu"))

fit_mx <- mxTryHard(model = m_mx)

tree_mx <- semtree::semtree(model = fit_mx, data = Data,
control = semtree.control(use.maxlm = TRUE, method = "naive"))
plot(tree_mx)

test_that("results are identical", {
  expect_equal(tree_mx$rule, tree_lav$rule)
})
