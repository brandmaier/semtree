plot.diversityMatrix <- function(x, num.cluster=2, col.area = "gray",
                     col.medoids = "blue", ...)
{
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
  
  for (cn in unique(result$clustering)) {
    rng <- which(result$clustering == cn)
    idx <- grDevices::chull(x[rng], y[rng])
    graphics::polygon(mds$points[c(rng[idx], rng[idx[1]]), ],
                      col = col.area[which(unique(result$clustering) == 
                                                                           cn)])
  }
  
  for (i in 1:length(result$medoids)) {
    rad <- (max(x)-min(x))/20
    plotrix::draw.circle(x[result$medoids][i], 
                         y[result$medoids][i],radius=rad,col=col.medoids[i])
  }
  
  
  #points(x[result$medoids], y[result$medoids],pch="o",col="green")
  text(x,y,labels=1:nrow(distmat))
  
}