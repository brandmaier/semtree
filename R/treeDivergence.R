
treeDivergence <- function(tree1, tree2, data, divergence=klsym) {
  
  # similarity of two trees, sampling-based
  
  trav1<-traverse(tree1, data)
  trav2<-traverse(tree2, data)
  

  
  precomputeDist <- list()
  
  dds <- rep(NA, nrow(data))
  
  for (i in 1:nrow(data)) {
    
    nodeid1 <- trav1[i]
    nodeid2 <- trav2[i]
    
    key <- paste0(nodeid1,";",nodeid2)
    
    # obtain difference of models (independent of the sample value?)
    if (!is.null(precomputeDist[[key]])) {
      diff <- precomputeDist[[key]]
    } else {
      
      model1 <- semtree::getNodeById(tree1, nodeid1)$model
      model2 <- semtree::getNodeById(tree2, nodeid2)$model
      
      cov1 <- getExpectedCovariance(model1)
      cov2 <- getExpectedCovariance(model2)
      
      mu1 <- t(getExpectedMean(model1))
      mu2 <- t(getExpectedMean(model2))
      
            
      diff <- divergence(mu1, cov1, mu2, cov2)
      precomputeDist[[key]] <- diff
    }
    dds[i] <- diff
    
    
  }
  
  return(mean(dds))
  
}


treeDivergence.alt <- function(tree1, tree2, data, divergence=klsym) {
  
  # similarity of two trees, sampling-based
  
  trav1<-traverse(tree1, data)
  trav2<-traverse(tree2, data)
  
  nodes1 <- getNodeList(tree1)
  nodes2 <- getNodeList(tree2)
  
  precomputeDist <- list()
  
  dds <- rep(NA, nrow(data))
  
  for (i in 1:nrow(data)) {
    
    nodeid1 <- trav1[i]
    nodeid2 <- trav2[i]
    
    key <- paste0(nodeid1,";",nodeid2)
    
    # obtain difference of models (independent of the sample value?)
    if (!is.null(precomputeDist[[key]])) {
      diff <- precomputeDist[[key]]
    } else {
      
      model1 <- nodes1[[nodeid1]]$model
      model2 <- nodes2[[nodeid2]]$model
      
      cov1 <- getExpectedCovariance(model1)
      cov2 <- getExpectedCovariance(model2)
      
      mu1 <- t(getExpectedMean(model1))
      mu2 <- t(getExpectedMean(model2))
      
      
      diff <- divergence(mu1, cov1, mu2, cov2)
      precomputeDist[[key]] <- diff
    }
    dds[i] <- diff
    
    
  }
  
  return(mean(dds))
  
}


