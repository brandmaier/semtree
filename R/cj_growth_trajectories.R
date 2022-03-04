if(FALSE){
dotsize=1
scores <- data.frame(restsne$Y)
p <- ggplot(data = scores, aes(x = X1, y = X2)) + geom_point(colour = "skyblue") + theme_bw()

respc <- princomp(M_dist)

p <- ggplot(data = as.data.frame(respc$scores[, 1:2]), aes(x = Comp.1, y = Comp.2)) + geom_point(colour = "skyblue") + theme_bw()

tmp <- M_dist
class(tmp) <- "matrix"
psych::fa.parallel(tmp)
# 3 classes
res_k <- kmeans(M_dist, 3)
table(res_k$cluster)

mmg <- m0
mmg$data <- NULL
mmg <- lapply(1:max(res_k$cluster), function(i){
  mod <- mmg
  mod$S$labels[which(mod$S$labels == "vres")] <- paste0("vres", i)
  mod$M$labels[which(grepl("^mean", mod$M$labels))] <- paste0(mod$M$labels[which(grepl("^mean", mod$M$labels))], i)
  mxModel(name = paste0("group ", i), mod,
          mxData(df_anal[res_k$cluster == i, paste0("de", 2:6)], type = "raw"),
          mxFitFunctionML())
})
mmg <- do.call(mxModel, c(list(model = "mg_starts", mxFitFunctionMultigroup(paste0("group ", 1:max(res_k$cluster))), mmg)))
mxRun(mmg) -> res_mmg
summary(res_mmg)



library(semtree)
#> Loading required package: OpenMx
#> OpenMx may run faster if it is compiled to take advantage of multiple cores.
#> Registered S3 method overwritten by 'sets':
#>   method        from   
#>   print.element ggplot2
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


fp <- "mu2" # predicted by grp2
#fp <- "mu1" # predicted by grp1

tree.biv <- semtree(model.biv, data=df.biv, constraints = list(focus.parameters=fp))
#> ✔ Tree construction finished [took 1s].

semtree:::plot.semtree(tree.biv)

fp <- "mu2" # predicted by grp2
forest <- semforest(model.biv, data=df.biv,
                    constraints = list(focus.parameters=fp),
                    control=semforest.control(num.trees=10, control=semtree.control(method="score",alpha=1)))


vim <- varimp(forest, method="permutationFocus")

plot(vim, main="Variable Importance")



plot_growth <- function(x, ...){
  UseMethod("plot_growth", x)
}

plot_growth.default <- function(x, ...){
  if(ncol(x) == 2) x$group <- 1
  ggplot(x, aes_string(x = names(x[1]), y = names(x[2]), colour = names(x[3]))) + 
    geom_point() + 
    geom_path()
}

plot_growth.MxModel <- function(x, lv = NULL, obs = NULL, timelv = NULL, ...){
  tab <- table_results(x, columns = NULL)
  if(is.null(lv)) lv <- unique(tab$col[tab$matrix == "A"])
  if(is.null(obs)) obs <- unique(tab$row[tab$matrix == "A"])
  if(is.null(timelv)) timelv <- lv[2]
  if(!is.null(names(x@submodels))){
    tmp <- lapply(names(x@submodels), function(i){
      list(x[[i]]$M$values[1, lv],
           x[[i]]$A$values[obs, lv])
    })
  } else {
    tmp <- list(x$M$values[1, lv],
                x$A$values[obs, lv])
  }
  df_plot <- do.call(rbind, lapply(1:length(tmp), function(gnum){
    i = tmp[[gnum]]
    data.frame(
      Time = i[[2]][, timelv],
      Expected = .trajectory(i[[1]], i[[2]]),
      group = paste0(x[[names(x@submodels)[gnum]]]$name,
                     ", N = ",
                     x[[names(x@submodels)[gnum]]]$data$numObs)
    )
  }))
  plot_growth(df_plot) + theme_bw()
}


plot_growth.semtree <- function(x, lv = NULL, obs = NULL, timelv = NULL, ...){
  if(is.null(lv)) lv <- x$model$latentVars
  if(is.null(obs)) obs <- x$model$manifestVars
  if(is.null(timelv)) timelv <- lv[2]
  mean_vals <- x$model$M$labels[1, lv]
  params <- parameters(x)
  mean_vals <- params[mean_vals, ]
  loadings <- x$model$A$values[obs, lv]
  expected <- apply(mean_vals, 2, .trajectory, L = loadings)
  df_plot <- as.data.frame.table(expected)
  df_plot$Time <- loadings[, timelv][df_plot$Var1]
  names(df_plot)[c(2, 3)] <- c("group", "Expected")
  df_plot <- df_plot[c("Time", "Expected", "group")]
  therange <- range(x$model$data$observed[obs], na.rm = TRUE)
  plot_growth(df_plot) +
    scale_y_continuous(limits = therange) +
    theme_bw()
}
}
.trajectory <- function(m, L){
  colSums(m * t(L))
}
