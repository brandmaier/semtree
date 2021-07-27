#
# testing invariance 
#

# simulation:
# factor structure holds over SES but not over age
# both SES and age predict a mean difference in cognitive outcome

set.seed(123)

require("semtree")
require("lavaan")
#
lambda <- list()
age <- c(0,0,1,1) # 0=young, 1 =old
ses <- c(0,1,0,1) # 0=low, 1=high
lambda[[1]] <- c(1,0.9,0.8,0.8)
lambda[[2]] <- c(1,0.9,0.8,0.8)
lambda[[3]] <- c(1,0.4,0.2,0.9)
lambda[[4]] <- c(1,0.4,0.2,0.9)
cogmean <- 80-age*30 + ses*20
cogsd <- 1
errsd <- 1

Nsub <- 200 # persons per agexSES group

cbind(age,ses,cogmean)

# simulate data from 4 groups
data <- c()
for (i in 1:4) {
  cogsim <- rnorm(n = Nsub,mean = cogmean[i],cogsd)
  scores <- as.matrix(t(outer(lambda[[i]],cogsim))) + rnorm(Nsub*4,0,errsd)
  data <- rbind(data,scores)
}
data <- data.frame(data)
names(data) <- paste0("x",1:4)

fulldata <- cbind(data, age=factor(rep(age,each=Nsub)),ses=factor(rep(ses,each=Nsub)))
#
model<-"
! regressions 
F=~1.0*x1
F=~l2*x2
F=~l3*x3
F=~l4*x4
! residuals, variances and covariances
x1 ~~ VAR_x1*x1
x2 ~~ VAR_x2*x2
x3 ~~ VAR_x3*x3
x4 ~~ VAR_x4*x4
F ~~ 1.0*F
! means
F~1
x1~0*1;
x2~0*1;
x3~0*1;
x4~0*1;
";
result<-lavaan(model, data=data, fixed.x=FALSE, missing="FIML");

manifests<-c("x1","x2","x3","x4")
latents<-c("F")
model <- mxModel("Unnamed_Model", 
                 type="RAM",
                 manifestVars = manifests,
                 latentVars = latents,
                 mxPath(from="F",to=c("x1","x2","x3","x4"), free=c(FALSE,TRUE,TRUE,TRUE),
                        value=c(1.0,1.0,1.0,1.0) , arrows=1, label=c("F__x1","l2","l3","l4") ),
                 mxPath(from="one",to=c("F"), free=c(TRUE), value=c(1.0) , arrows=1, label=c("const__F") ),
                 mxPath(from="one",to=c("x2","x3","x4"), free=c(TRUE,TRUE,TRUE), value=c(1.0,1.0,1.0) , arrows=1, label=c("const__x2","const__x3","const__x4") ),
                 mxPath(from="x1",to=c("x1"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x1") ),
                 mxPath(from="x2",to=c("x2"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x2") ),
                 mxPath(from="x3",to=c("x3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x3") ),
                 mxPath(from="x4",to=c("x4"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_x4") ),
                 mxPath(from="F",to=c("F"), free=c(FALSE), value=c(1.0) , arrows=2, label=c("VAR_F") ),
                 mxPath(from="one",to=c("x1"), free=F, value=0, arrows=1),
                 mxData(data[1:50,], type = "raw")
);

result <- mxRun(model)
summary(result)

subset <- data[1:50, ]


ctr <- semtree.control(verbose=TRUE)
ctr$exclude.heywood <- FALSE
# naive tree should find both effects, age & ses, with age having the stronger effect 
tree <- semtree(model = result, data=fulldata, control=ctr)
#plot(tree)

# invariance tree should exclude splits wrt age and only splir wrt to ses
ctr$report.level <- 99
ctr$alpha.invariance <- 0.01

cnst <- semtree.constraints(local.invariance = c("l2","l3","l4"))
#cnst <- semtree.constraints(local.invariance=)
tree2 <- semtree(model = result, data=fulldata, control=ctr, constraints = cnst )

#plot(tree2)
