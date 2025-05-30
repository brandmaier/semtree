testthat::skip_on_cran()

library(semtree)

imps <- c(
  1:50,
  rep(c(10,20),each=25),
  rep(c(1,2,3,4,5),each=10)
)

imps[c(1,10,55,56)] <- NA

set.seed(234)
vim <- list(
  var.names = paste0("x",1:3),
  importance = matrix(data=imps, nrow=50, ncol=3, byrow = FALSE),
  ll.baseline = c(100,110,120),
  elapsed = NA
  
)
class(vim) <- "semforest.varimp"

vim

plot(vim)

varimpConvergencePlot(vim)
varimpConvergencePlot(vim, aggregate = "mean")
varimpConvergencePlot(vim, aggregate = "median")

varimpConvergencePlot(vim, aggregate = "mean",na.omit = TRUE)
varimpConvergencePlot(vim, aggregate = "mean",na.omit = FALSE)


print(vim)
