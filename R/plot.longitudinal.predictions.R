#nodes <- c(5,6,11)
#plot.predictions(tree,nodes)

plot.predictions<-function(tree, nodes, lty=NULL, col=NULL, time=NULL, labels=NULL)
{
  N <- length(nodes)
  M <- length(tree$model@manifestVars)
  
  preds <- matrix(NA, ncol=M, nrow=N)
  for (i in 1:N) {
    node <- getNodeById(tree, nodes[i])
    preds[i,] <- simplify2array(node$model@matrices$F@values %*% (node$model@matrices$A@values %*% t(node$model@matrices$M@values)) )
  }
  
  ylim <- c(min(preds)*0.9,max(preds)*1.1)
  
  if(is.null(time)) {
    time <- 1:M
  }
  
  plot(time,type="n",ylim=ylim,xlim=c(1,M),xlab="time",ylab="measurement")

  if (is.null(lty)) {
    lty <- 1:N
  }
  
  if (is.null(col)) {
    col <- rep("black",N)
  }
  
  
  for (i in 1:N) {
    lines(1:M,preds[i,],lty=lty[i],col=col[i],lwd=8)
#    lines(1:M,preds[i,],lw=8)
  }
  
  if (is.null(labels)) {
 labels<-paste("Node ",nodes)
 }

  legend("topright",legend=labels,lty=lty,lwd=8,col=col)
  
}