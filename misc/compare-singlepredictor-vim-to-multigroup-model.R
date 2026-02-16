
library(lavaan)
library(semtree)
set.seed(1238)

N <- 500

# simulate data with Cohen's d = 2
Data <- data.frame(y = c(rnorm(N/2, mean = -1), rnorm(N/2, mean = 1)),
                   z = factor(rep(c(0,1),each=N/2) ))

m_lav <- '
y ~~ y
y ~ 1
'

m_lav_constrained <- '
y ~~ y
y ~ c(a,a)*1
'

####Testing semtree with lavaan models ####
fit_lav <- lavaan(model = m_lav, data = Data)

forest <- semforest(model=fit_lav, data = Data, 
          control = semforest.control(
            num.trees = 25, control=semtree_control(method="score")))

vim <- varimp(forest)

plot(vim)

zimp <- semtree:::aggregateVarimp(vim)

fit_lav_multigroup <- lavaan(model = m_lav, data = Data,group = "z")
fit_lav_multigroup2 <- lavaan(model = m_lav_constrained, data = Data,group = "z")
lrt <- anova(fit_lav_multigroup,fit_lav_multigroup2)
chi2 <- lrt$`Chisq diff`[2]


cat("Importance: ", zimp,"\n")
cat("MG Chi^2: ",chi2,"\n")


