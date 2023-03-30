## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi=300,
  out.width="50%"
)



library(ggplot2)

## -----------------------------------------------------------------------------

library(semtree)
set.seed(123)
N <- 1000
grp1 <- factor(sample(x = c(0,1), size=N, replace=TRUE))
grp2 <- factor(sample(x = c(0,1), size=N, replace=TRUE))
noise <- factor(sample(x = c(0,1),size=N, replace=TRUE))
Sigma <- matrix(byrow=TRUE,
                nrow=2,c(2,0.2,
                         0.2,1))
obs <- MASS::mvrnorm(N,mu=c(0,0),
                     Sigma=Sigma)
obs[,1] <- obs[,1] + ifelse(grp1==1,3,0)
obs[,2] <- obs[,2] + ifelse(grp2==1,3,0)
df.biv <- data.frame(obs, grp1, grp2, noise)
names(df.biv)[1:2] <- paste0("x",1:2)
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
                     mxData(df.biv, type = "raw")
);
result <- mxRun(model.biv)
summary(result)

## -----------------------------------------------------------------------------
df.biv.pred <- data.frame(df.biv, 
  leaf=factor(as.numeric(df.biv$grp2)*2+as.numeric(df.biv$grp1)))
  ggplot(data = df.biv.pred, aes(x=x1, y=x2, group=leaf))+ 
  geom_density_2d(aes(colour=leaf))+ 
  viridis::scale_color_viridis(discrete=TRUE)+
  theme_classic()

## -----------------------------------------------------------------------------
fp <- "mu2" # predicted by grp2
#fp <- "mu1" # predicted by grp1

tree.biv <- semtree(model.biv, data=df.biv, constraints = list(focus.parameters=fp))

plot(tree.biv)


