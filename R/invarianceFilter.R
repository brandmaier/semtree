invarianceFilter <- function(model=NULL, mydata=NULL, btn.matrix=NULL, LL.baseline=NULL, invariance, control, ...) {
  
  # Test for invariance of user selected parameters
  # Runs invariance on selected best splits for each covariate
  # Can be a list of multiple vectors for ordered invariance testing
  # Returns a list of filter vectors (1 for each of the invariance conditions)
  
  # Submodel Fits with model parameters allowed to vary
  # If Model is significant return 1, else 0
  filter <- list()
  #browser()
  # btn.matrix <- btn.matrix[,order(-as.numeric(btn.matrix[1,]))]
  # for(i in 1:6){
  for(i in 1:ncol(btn.matrix)){
    # select subsets for variable split i
    if(is.factor(mydata[,as.numeric(btn.matrix[3,i])])){
      if(is.ordered(mydata[,as.numeric(btn.matrix[3,i])])){
        subset1 <- subset (mydata, as.numeric(mydata[,as.numeric(btn.matrix[3,i])]) > as.numeric(btn.matrix[4,i]))
        subset2 <- subset (mydata, as.numeric(mydata[,as.numeric(btn.matrix[3,i])]) < as.numeric(btn.matrix[4,i]))
      }
      else{
        
        result <- recodeAllSubsets(mydata[,as.numeric(btn.matrix[3,i])],colnames(mydata)[as.numeric(btn.matrix[3,i])])
        test1 <- c()
        test2 <- rep(NA, length(mydata[,as.numeric(btn.matrix[3,i])]))
        
        for(q in 1:ncol(result$columns)) {
          for(p in 1:length(mydata[,as.numeric(btn.matrix[3,i])])) {
            if(isTRUE(result$columns[p,q])) {test1[p] <- 1}
            else if(!is.na(result$columns[p,q])){test1[p] <- 0}
            else{test1[p]<-NA}
          }
          test1 <- as.factor(test1)
          test2 <- data.frame(test2, test1)
        }
        test2 <- test2[,-1]
        
        if(result$num_sets==1) {
          subset1 <- subset (mydata, as.numeric(test2) == 2)
          subset2 <- subset (mydata, as.numeric(test2) == 1)
        }
        else {
          subset1 <- subset (mydata, as.numeric(test2[[as.numeric(btn.matrix[4,i])]]) == 2)  
          subset2 <- subset (mydata, as.numeric(test2[[as.numeric(btn.matrix[4,i])]]) == 1)
        }
      }
    }
    else{
      subset1 <- subset (mydata, as.numeric(mydata[,as.numeric(btn.matrix[3,i])]) > as.numeric(btn.matrix[4,i]))
      subset2 <- subset (mydata, as.numeric(mydata[,as.numeric(btn.matrix[3,i])]) < as.numeric(btn.matrix[4,i]))
    }
    
    # initialize empty vectors for storing scores and split info
    inv.filter <- 0
    df <- 0
    pvalue <- 0
    # the most free model, add constraints to test invariance between groups
    LRtest <- fitSubmodels(model, subset1, subset2, control)
    for(j in 1:length(invariance)){
      #browser()
      #constrain model from relaxed state -> put constrained model in place for new comparison
      LR <- fitSubmodels(model, subset1, subset2, control, invariance[[j]])
      
      # create matrix of stats for comparing filters over variables (testing purposes)
      LRtest <- cbind(LRtest, LR)
      df <- cbind(df, length(invariance[[j]]))
      pvalue <- cbind(pvalue, pchisq(LRtest[j+1]-LRtest[j],df=length(invariance[[j]]), lower.tail=F))
      # calc if var is invariant for each step in invariance testing (number of lists passed for number of invariance tests)
      # the first one is versus completely invariant ( or whatever global constraints exist)
      if(!is.na(LR) & (pchisq(LRtest[j+1]-LRtest[j],df=length(invariance[[j]]), lower.tail=F) <= control$alpha.invariance)) { 
        inv.filter <- cbind(inv.filter, 0)
      }
      else { 
        inv.filter <- cbind(inv.filter, 1)
      }
    }
    filter[[i]] <- rbind(inv.filter,LRtest,df,pvalue)
  }
  # sum up filters for each variable.
  # if the number of invariant models equla the number of lists, then the selected split is valid
  by.var.filter <- rep(NA, ncol(btn.matrix))
  for(i in 1:ncol(btn.matrix)){
    if(sum(filter[[i]][1,])==length(invariance)) { by.var.filter[i] <- 1}
    else { by.var.filter[i] <- 0}
  }
  
  for(i in 1:ncol(btn.matrix)){
    if(by.var.filter[i]==0){
      btn.matrix[1,i] <- NA
    }
  }
  #browser()
  # returns a vector of booleans, 1 for valid split by invariance testing, 0 for invalid
  return(btn.matrix)
}