library(lavaan)

example(cfa)


HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
            '

fit <- cfa(HS.model, data = HolzingerSwineford1939, meanstructure = TRUE)

data <- HolzingerSwineford1939[1:10, -(1:6)]

variant1  <- evaluateDataLikelihood(fit, data, loglik="model")

variant2 <- sum(evaluateDataLikelihood(fit, data, loglik="mvn"))

test_that("multivariate normal likelihood is correct (with means)", {expect_equal(variant1, variant2)})

# revert back to default
options(semtree.lavaan_loglik_computation = "model")
# should also be identical to:
# sigma<- fitted(fit)$cov
# mean<-fitted(fit)$mean
# -2*sum(mvtnorm::dmvnorm(data, mean=mean,sigma=sigma, log=TRUE))


#
# covariance only
#


HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
            '

fit <- cfa(HS.model, data = HolzingerSwineford1939, meanstructure = FALSE)

data <- HolzingerSwineford1939[1:10,]

variant1  <- evaluateDataLikelihood(fit, data,loglik = "model")

variant2 <- sum(evaluateDataLikelihood(fit, data, loglik="mvn"))

#-2*sum(mvtnorm::dmvnorm(data, mean=,sigma=sigma, log=TRUE))
test_that("multivariate normal likelihood is correct (no means)", {expect_equal(variant1, variant2)})
