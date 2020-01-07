plot.contributions <- function(x, normalize=TRUE, ...)
{

	# this is Option #2 from https://www.r-bloggers.com/no-visible-binding-for-global-variable/
	# to stop CRAN complaining about undefined variables
	percentage <- id <- parameter <- NULL

  x <- x$dat
  xdat <- tidyr::gather(x,key="parameter",value="percentage",-id)
  p4 <- ggplot2::ggplot() + ggplot2::geom_bar(ggplot2::aes(y = percentage, 
                                x = id, fill = parameter), 
                            data = xdat,
                            stat="identity")+
    ggplot2::xlab("Node ID")+
    ggplot2::ylab("Contribution [%]")+
    ggplot2::theme_light()
  
  (p4)
  return(p4)
}
  
  
contributions <- function(tree, normalize=TRUE) 
  {
    x <- NULL
    nodes <- getNodeList(tree)
    for (node in nodes) {
      if (node$caption == "TERMINAL") next;
      print(node$caption)
      x <- rbind(x, c(node$result$contrib.max,id=node$node_id))
    }
    
    x <- abs(x)
    # x <- t(x)
    if (normalize)
      x <- cbind(
        t(apply(x[,-which(colnames(x)=="id")], 1, function(x) { x<-x/sum(x)})),
        id=x[,"id"])
    
    x <- data.frame(x)
    x[,"id"] <- as.factor(x[,"id"])

        return.obj <- list(dat=x)
        class(return.obj) <- c("contributions")
    return(return.obj)
  }
