plot.semforest.proximity <- function(x, main = "Principal Coordinates", xlim=NULL, ylim=NULL, 
                           col="black",pch=NULL,  axes=c(1,2), projection.type="mds",cex=2, ...) 
{

	prox <- x

  type <- projection.type
    
  d <- as.dist(1-prox)
  
  if (type=="mds") {
    fit <- cmdscale(d,eig=T, k=max(axes))  
    #x <- fit$points[,axis.x.num]
    #y <- fit$points[,axis.y.num]
    eig <- fit$eig
    points <- fit$points
  } else if (type == "pca") {
    fit <- eigen(prox)
    eig <- fit$values
    points <- fit$vectors
   # x <- fit$vectors[,axis.x.num ]
    #y <- fit$vectors[,axis.y.num ]
  } else {
    stop("Unknown type. Use 'mds' or 'pca'")
  }
  
  if (length(axes)==2) {
  x <- points[,axes[1]]
  y <- points[,axes[2]]
  plot(x, y,  xlab=paste("Principal Axis",axes[1]), ylab=paste("Principal Axis",
                                                                  axes[2]), 
       main=main, type="n", xlim=xlim, ylim=ylim)
  if (!is.null(pch)) {
    points(x,y,pch=pch,cex=cex,col=col)
  } else {
    text(x,y,1:length(x),cex=cex, col=col)
  }
  
  } else {
    
    if (is.null(pch)) pch<-1
    
    pairs(points[, axes],labels=paste("Dim ",axes), col=col, pch=pch, cex=cex, ...)
  }
  
  # Variance explained by three components
  sum(abs(eig[1:2])) / sum(abs(eig))
  
  return ( fit  )
}