
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


raw_model<-"x1~~a*x1
x2~~b*x2
x1~~c*x2
x1~e*1
x2~d*1"

library(lavaan)
fitted <- lavaan(raw_model, df.biv)
summary(fitted)

tree1<-semtree(fitted, df.biv)
tree2<-semtree(fitted, df.biv, control=semtree_control(verbose=TRUE),
              constraints = semtree.constraints(focus.parameters = "e"))
tree3<-semtree(fitted, df.biv, control=semtree_control(verbose=TRUE),
              constraints = semtree.constraints(focus.parameters = "d"))


frst <- semforest(fitted, df.biv,constraints = semtree.constraints(focus.parameters = "e"))


testthat::expect_identical(tree2$rule$name, "grp1")
testthat::expect_identical(tree3$rule$name, "grp2")



#model=fitted
#df.biv
#constraints = semtree.constraints(focus.parameters = "e")
#subset1 <- df.biv[df.biv$grp1==1,]
#subset2 <- df.biv[df.biv$grp1!=1,]
#lav_multigroup(model, subset1, subset2, constraints)
