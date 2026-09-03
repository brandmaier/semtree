N<-500

set.seed(3459)
observed <- MASS::mvrnorm(n=N, mu=c(0,0), Sigma=matrix(c(1,0.2,0.2,1),nrow=2))

pred1 <- rnorm(N)
pred2 <- rnorm(N)
noise <- rnorm(N)

observed[,1] <- observed[,1]+ifelse(pred1>0,2,0)
observed[,2] <- observed[,2]+ifelse(pred2>0,2,0)

datf <- data.frame(x1=observed[,1], x2=observed[,2],pred1, pred2, noise)


#
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
result <- mxRun(model.biv)
summary(result)

# ---

tree <- semtree(model=model.biv, data=datf, 
                    constraints = semtree.constraints(focus.parameters=c("mu1","VAR_x1")),
                    control = semtree_control(  method="score"))
