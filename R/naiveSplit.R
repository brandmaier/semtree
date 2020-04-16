naiveSplit <- function(model=NULL, mydata=NULL, control=NULL, invariance=NULL, meta=NULL, 
                       pp=FALSE, constraints=NULL, ...) {
	#mvars <- length(model@manifestVars)
	n.comp <- 0
	LL.btn <- c()
	split.btn <- c()
	cov.btn.names <- c()
	cov.btn.cols <- c()
	cov.type <- c()
	cov.col <- c()
	cov.name <- c()
	
	LL.within <- c()
	within.split <- c()
	#Baseline model to compare subgroup fits to
	###########################################################
	###               OPENMX USED HERE                      ###
	###########################################################
	if(control$sem.prog == 'OpenMx'){
	  modelnew <- mxAddNewModelData(model,mydata,name="BASE MODEL")
	  LL.overall <- safeRunAndEvaluate(modelnew) 
	  suppressWarnings(if (is.na(LL.overall)) return(NULL))
	}
	###########################################################
	###               lavaan USED HERE                      ###
	###########################################################
	if(control$sem.prog == 'lavaan'){
	  #if (control$verbose) {message("Assessing overall model")}
	  modelnew <- eval(parse(text=paste(model@Options$model.type,'(parTable(model),data=mydata,missing=\'',model@Options$missing,'\',do.fit=F)',sep="")))
	  #modelnew <- lavaan(parTable(model),data=mydata,model.type=model@Options$model.type,do.fit=FALSE)
	  LL.overall <- safeRunAndEvaluate(modelnew) 
	  suppressWarnings(if (is.na(LL.overall)) return(NULL))
	}
  #firstCol <- 1
	#for(c in (mvars+1):ncol(mydata)) {
  if(pp) {comparedData <- max(meta$model.ids+1)}
  else {comparedData <- meta$covariate.ids}
  for (cur_col in comparedData) {
    
    # parent model's likelihood (LL.baseline) is adjusted
    # for each split if there is missing data
    LL.baseline <- LL.overall
	    missingModel <- missingDataModel(modelnew, mydata, cur_col)
	    if(!is.null(missingModel)){ LL.baseline <- safeRunAndEvaluate(missingModel)}
	    if (control$report.level > 10) {
	      report(paste("Estimating baseline likelihood: ",LL.baseline),1)
	    }
	    
	  
	  # tell the user a little bit about where we are right now
	  if (control$verbose){
	    ui_message("Testing Predictor: ",
	            colnames(mydata)[cur_col])
	 }
    ############################################################
	  #case for factored covariates##############################
	  if(is.factor(mydata[,cur_col])) {
	    #unordered factors#####################################
	    if(!is.ordered(mydata[,cur_col])) {
	      var.type = 1
	      v <- as.numeric(mydata[,cur_col])
	      #v <- as.numeric(mydata[,cur_col])
	      val.sets <- sort(union(v,v))
	      if(length(val.sets) > 1) {
	        
	        #create binaries for comparison of all combinations
	        result <- recodeAllSubsets(mydata[,cur_col],colnames(mydata)[cur_col])
	        test1 <- c()
	        test2 <- rep(NA, length(mydata[,cur_col]))
	        
	        for(j in 1:ncol(result$columns)) {
	          for(i in 1:length(mydata[,cur_col])) {
	            if(isTRUE(result$columns[i,j])) {test1[i] <- 1}
	            else if(!is.na(result$columns[i,j])){test1[i] <- 0}
	            else{test1[i]<-NA}
	          }
	          test1 <- as.factor(test1)
	          test2 <- data.frame(test2, test1)
	        }
	        test2 <- test2[,-1]
	        for(i in 1:(result$num_sets)) {
	          LL.temp <- c()
	          #subset data for chosen value and store LL
	          if(result$num_sets==1) {
	            subset1 <- subset (mydata, as.numeric(test2) == 2)
	            subset2 <- subset (mydata, as.numeric(test2) == 1)
	          }
	          else {
	            subset1 <- subset (mydata, as.numeric(test2[[i]]) == 2)	
	            subset2 <- subset (mydata, as.numeric(test2[[i]]) == 1)
	          }
	          
	          # refit baseline model with focus parameters @TAGX
	          if (!is.null(constraints) & (!is.null(constraints$focus.parameters))) {
	            LL.baseline <- fitSubmodels(model, subset1, subset2, 
	                                        control, invariance=constraints$focus.parameters)
	            if (control$report.level > 10) {
	              report(paste("Reestimating baseline likelihood with focus parameters: ",LL.baseline),1)
	            }
	          }
	          
	          LL.return <- fitSubmodels(model, subset1, subset2, control, invariance=NULL)
	          if(!is.na(LL.return)){
	            LL.within <- cbind(LL.within, (LL.baseline-LL.return))
	            within.split <- cbind(within.split, i)
	            cov.col <- cbind(cov.col, cur_col)
	            cov.name <- cbind(cov.name, colnames(mydata[cur_col]))
	            cov.type <- cbind(cov.type, var.type)
	            n.comp <- n.comp + 1
	          }
	        }
	      }
	    }
	    #ordered factors#########################################
	    if(is.ordered(mydata[,cur_col])) {
	      var.type = 2
	      v <- as.numeric(as.character(mydata[,cur_col]))
	      val.sets <- sort(union(v,v))
        #browser()
        #cat("number of categories:",length(val.sets),"\n")
	      #cat("number of comparisons:",(length(val.sets)-1),"\n")
	      if(length(val.sets) > 1) {
	        for(i in 2:(length(val.sets))) {
	          LL.temp <- c()
	          #subset data for chosen value and store LL
	          cond1 <- as.numeric(as.character(mydata[,cur_col])) > (val.sets[i]+val.sets[(i-1)])/2
	          cond2 <- as.numeric(as.character(mydata[,cur_col])) < (val.sets[i]+val.sets[(i-1)])/2
	          subset1 <- subset (mydata, cond1)
	          subset2 <- subset (mydata, cond2)

	          # refit baseline model with focus parameters @TAGX
	          if (!is.null(constraints) & (!is.null(constraints$focus.parameters))) {
	            LL.baseline <- fitSubmodels(model, subset1, subset2, 
	                                        control, invariance=constraints$focus.parameters)
	            if (control$report.level > 10) {
	              report(paste("Reestimating baseline likelihood with focus parameters: ",LL.baseline),1)
	            }
	          }
	          
	          LL.return <- fitSubmodels(model, subset1, subset2, control, invariance=NULL)
	          if(!is.na(LL.return)){
	            LL.within <- cbind(LL.within, (LL.baseline-LL.return))
	            within.split <- cbind(within.split, (val.sets[i]+val.sets[(i-1)])/2)
	            cov.col <- cbind(cov.col, cur_col)
	            cov.name <- cbind(cov.name, colnames(mydata[cur_col]))
	            cov.type <- cbind(cov.type, var.type)
	            n.comp <- n.comp + 1
	          }
	        }
	      }
	    }
	  }
	  #cat("length of comparison LL:",length(LL.within),"\n")
	  #numeric (continuous) covariates################################
	  if(is.numeric(mydata[,cur_col])) {
	    var.type = 2
	    v <- as.numeric(mydata[,cur_col])
	    val.sets <- sort(union(v,v))
	
	      if(length(val.sets) > 1) {
      
	        for(i in 2:(length(val.sets))) {
	          LL.temp <- c()
	          #subset data for chosen value and store LL
	          cond1 <- as.numeric(mydata[,cur_col]) > (val.sets[i]+val.sets[(i-1)])/2
	          cond2 <- as.numeric(mydata[,cur_col]) < (val.sets[i]+val.sets[(i-1)])/2
	          subset1 <- subset (mydata, cond1)
	          subset2 <- subset (mydata, cond2)
	          
	          #catch LLR for each comparison
	          
	          # refit baseline model with focus parameters @TAGX
	          if (!is.null(constraints) & (!is.null(constraints$focus.parameters))) {
	            LL.baseline <- fitSubmodels(model, subset1, subset2, 
	                                        control, invariance=constraints$focus.parameters)
	            if (control$report.level > 10) {
	              report(paste("Reestimating baseline likelihood with focus parameters: ",LL.baseline),1)
	            }
	          }
	          
	          LL.return<-fitSubmodels(model, subset1, subset2, control, invariance=NULL)
            # cat("LLreturn:",LL.return," and value split at:",(val.sets[i]+val.sets[(i-1)])/2,"\n")
	          if(!is.na(LL.return)){
	            LL.within <- cbind(LL.within, (LL.baseline-LL.return))
	            within.split <- cbind(within.split, (val.sets[i]+val.sets[(i-1)])/2)
	            cov.col <- cbind(cov.col, cur_col)
	            cov.name <- cbind(cov.name, colnames(mydata[cur_col]))
	            cov.type <- cbind(cov.type, var.type)
	            n.comp <- n.comp + 1
	          }
            #browser()
	        }
	      }

	  }
	}

	if(is.null(LL.within)) {return(NULL)}

	btn.matrix <- rbind(LL.within,cov.name,cov.col,within.split)
	colnames(btn.matrix) <- c(paste("var",seq(1,ncol(btn.matrix)),sep=""))
	rownames(btn.matrix) <- c("LR","variable","column","split val")

  filter <- c()
  if(!is.null(invariance)){ 
    if (control$verbose){
      ui_message("Filtering possible splits by invariance")
    }
    filter <- invarianceFilter(model,mydata,btn.matrix,LL.baseline,invariance,control)
  }  

	LL.max <- NA
	split.max <- NA
	name.max <- NA
	col.max <-NA
	type.max <- NA
	for(cur_col in 1:ncol(LL.within)) {
	  if(!is.null(filter)){
	    if(!is.na(filter[1,cur_col])) {
	      if(is.na(LL.max)){
	        LL.max <- LL.within[cur_col]
	        split.max <- within.split[cur_col]
	        name.max <- cov.name[cur_col]
	        col.max <-cov.col[cur_col]
	        type.max <- cov.type[cur_col]
	      }
	      else if(LL.within[cur_col]>LL.max){
	        LL.max <- LL.within[cur_col]
	        split.max <- within.split[cur_col]
	        name.max <- cov.name[cur_col]
	        col.max <-cov.col[cur_col]
	        type.max <- cov.type[cur_col]
	      }
	    }
	  }
    else {
      if(!is.na(LL.within[cur_col])) {
        if(is.na(LL.max)){
          LL.max <- LL.within[cur_col]
          split.max <- within.split[cur_col]
          name.max <- cov.name[cur_col]
          col.max <-cov.col[cur_col]
          type.max <- cov.type[cur_col]
        }
        else if(LL.within[cur_col]>LL.max){
          LL.max <- LL.within[cur_col]
          split.max <- within.split[cur_col]
          name.max <- cov.name[cur_col]
          col.max <-cov.col[cur_col]
          type.max <- cov.type[cur_col]
        }
      }
    }
	}
	
	# alternative way of counting the number of comparisons
	# count the number of variables instead of tests
	if (control$naive.bonferroni.type==1) {
	  n.comp <- length(comparedData)
	}
	
	
  if(is.na(LL.max)){return(NULL)}
  else(
    return(list(LL.max=LL.max,split.max=split.max,name.max=name.max,
                col.max=col.max, type.max=type.max, n.comp=n.comp, btn.matrix=btn.matrix, 
                invariance.filter=filter ))
  )
}