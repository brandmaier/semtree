testthat::skip_on_cran()

if (require(future)) {

N <- 2000

library(semtree)

library(future)
future::plan(multisession, workers=5)

observed <- MASS::mvrnorm(n=N, mu=c(0,0), Sigma=matrix(c(1,0.2,0.2,1),nrow=2))

pred1 <- rnorm(N)
pred2 <- rnorm(N)
noise <- rnorm(N)

observed[,1] <- observed[,1]+ifelse(pred1>0,2,0)
observed[,2] <- observed[,2]+ifelse(pred2>0,2,0)

datf <- data.frame(x1=observed[,1], x2=observed[,2],pred1, pred2, noise)


#
library(lavaan)

model <- "
x1~~x1
x2~~x2
x1~~x2
x1~1
x2~1"

model_lav <- lavaan(model, datf)

# ---

manifests<-c("x1","x2")
model.biv <- mxModel("Bivariate_Model", 
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
                     mxData(datf, type = "raw")
);
model_omx <- mxRun(model.biv)

sim <- function(model) {


forest <- semforest(model=model, data=datf, 
                    constraints = semtree.constraints(focus.parameters="mu2"),
                    control = 
                      semforest_control(num.trees = 30,
                                        control=semtree_control(min.N = 100, min.bucket = 50,alpha=1,
                                                                method="score")))

vim_naive <- varimp(forest)

vim_focus <- varimp(forest, method="permutationFocus")

vimdat <- data.frame( vim=rep(c("naive","focus"),each=3),
                      param=rep(c("pred1","pred2","noise"),2),
                      vals=c( semtree:::aggregateVarimp(vim_naive),semtree:::aggregateVarimp(vim_focus)))
return(vimdat)
}

#vimdat %>% ggplot(aes(x=vim,y=vals,group=param,fill=param))+geom_col(position="dodge")
rs <- sapply( c( model_lav, model_omx), FUN=sim)

}