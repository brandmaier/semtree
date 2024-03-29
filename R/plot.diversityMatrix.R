#' @exportS3Method plot diversityMatrix
plot.diversityMatrix <- function(x, num.cluster=2, col.area = "gray",
                     col.medoids = "blue", type="mds", 
                     show.cluster.center=TRUE,...)
{
  if (type=="mds") {
  
  distmat <- x
  
  result <- cluster::pam(dist(distmat), num.cluster, TRUE, "euclidean")
  
  mds <- stats::cmdscale(distmat, eig = TRUE, k = 2)
  x <- mds$points[, 1]
  y <- mds$points[, 2]
  plot(x, y, xlab = "Coordinate #1", ylab = "Coordinate #2", 
       type = "n", main = "Multidimensional Scaling")
  
  if (length(col.area) < length(unique(result$clustering))) {
    col.area <- rep(col.area, 
                    length(unique(result$clustering))) # just repeat to often; we don't care..
  }
  
  if (length(col.medoids) < length(unique(result$clustering))) {
    col.medoids <- rep(col.medoids, num.cluster)
  }
  
  # draw convex hulls
  for (cn in unique(result$clustering)) {
    rng <- which(result$clustering == cn)
    idx <- grDevices::chull(x[rng], y[rng])
    graphics::polygon(mds$points[c(rng[idx], rng[idx[1]]), ],
                      col = col.area[which(unique(result$clustering) == 
                                                                           cn)])
  }
  
  # draw cluster IDs
  text(x,y,labels=1:nrow(distmat))
  
  # draw cluster centers
  if (show.cluster.center) {
    for (i in 1:length(result$medoids)) {
      rad <- (max(x)-min(x))/20
      
      graphics::symbols(x=x[result$medoids][i], 
                        y= y[result$medoids][i],
                        circles = rad, add=TRUE, bg=col.medoids[i], inches=FALSE)
      text(x[result$medoids][i], 
           y[result$medoids][i], labels = i)
    }
  }
  
  
  #points(x[result$medoids], y[result$medoids],pch="o",col="green")

  
  } else if (type=="hist") {
     xlab="Diversity"
     main="Forest diversities"
     values <- x[lower.tri(x)]
    hist(x,main=main,xlab=xlab,...)
  } else {
    stop("Type not supported!")
  }
  
}


#plot.diversityMatrix <- function(x, xlab="Diversity", main="Forest diversities", ...) {
#  values <- x[lower.tri(x)]
#  hist(x,main=main,xlab=xlab,...)
#}
