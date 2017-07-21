plot.semtree<-function(x, type=2, no.plot=FALSE, ...) {
 	
 	if (is.null(x)) {
 		cat("Argument is not a SEM tree!");
 		return(NULL);
 	}
 	
 	tree <- x
 	
 	to.rpart.rec<-function(x, xx){
	
	if (is.null(xx)) {
		num <- 0
		level <- 1
	} else {
		num <- sum( 2^(length(xx):1-1) * xx )
		level <- 2**length(xx)
	}
	
	num <- num+level
	
	data <- c();
	# var   n  wt dev yval complexity ncompete nsurrogate
	if (x$caption=="TERMINAL") {
		#
		# create a data row for a terminal node
		#
		row <- c("<leaf>",x$N,x$N,NA,x$node_id,0,0,0, num, paste(paste(x$param_names,"=" ,round(x$params,3)) ,collapse="\n"),"")
		data <- rbind(data,row)
		
	} else {
		#
		# create data row for a decision node
		#
		
		# -- prepare statistic and/or p-value
		#crit <- "?"
   	# 	options("scipen"=5)
		#if (tree$p.values.valid) {
    #  	  	if(x$p<.001){crit <- paste("p<0.001")}
    #  		else{crit <- paste("p=",round(x$p,3),sep="")}
		#} else {
			crit <- paste( "LR=",round(x$lr,1),"(df=",x$df,")",sep="")
		#}
		
		row <- c(x$caption,x$N,x$N,0,x$node_id,0,0,0, num, NA,crit)
#		row <- c(x$node_id,x$N,x$N,0,x$caption,0,0,0, num, NA,crit)
	#	cat("b",paste(row),"\n")
		data <- rbind(data, row)
		row <- to.rpart.rec(x$left_child, append(xx,0))
	#	cat("c",paste(row),"\n")
		data <- rbind(data, row)
		row <- to.rpart.rec(x$right_child, append(xx,1))
	#	cat("d",paste(row),"\n")
		data <- rbind(data, row)
		#cat(paste(data),"\n")
	}
	return(data);
}

 	
	l <- list()
	data <- to.rpart.rec(x, NULL);
	
	l$frame <- data
	l$frame <- data.frame(l$frame, row.names=l$frame[,9])
	names(l$frame) <- c("var","n","wt","dev","yval","complexity","ncompete","nsurrogate","label","estimates","crit")



l$frame[,1] <- as.factor(l$frame[,1])
l$frame$dev <- as.numeric(as.character(l$frame$dev))
#l$frame$yval <- as.numeric(as.character(l$frame$yval))
l$frame$yval <- as.numeric(as.character(l$frame$yval))
l$frame$wt <- as.numeric(as.character(l$frame$wt))
l$frame$complexity <- as.numeric(as.character(l$frame$complexity))
l$frame$n <- as.numeric(as.character(l$frame$n))
l$frame$ncompete <- as.numeric(l$frame$ncompete)
l$frame$estimates <- as.character(l$frame$estimates)

l$frame$nsurrogate <- as.numeric(l$frame$nsurrogate)

	l$method <- "anova" #"semtree"
#	l$method <- "user"
#l$functions$node.fun <- function(x, labs, digits, varlen)
#{
#paste("#",x$frame$yval)
#paste(ifelse(x$frame$var=="<leaf>",x$frame$yval, x$frame$yval), "\n\nN=",x$frame$n,",p=",round(x$frame$dev,3))
#paste(x$frame$label,"\n\nN=",x$frame$n)
#}

# kindly borrowed from rpart
formatg <- function (x, digits = getOption("digits"), format = paste0("%.", 
    digits, "g")) 
{
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector")
    temp <- sprintf(format, x)
    if (is.matrix(x)) 
        matrix(temp, nrow = nrow(x))
    else temp
}

	
l$functions$summary<-
function (yval, dev, wt, ylevel, digits) 
{
    paste("  mean=", formatg(yval, digits), ", MSE=", formatg(dev/wt, 
        digits), sep = "")
}
#<environment: namespace:rpart>

l$functions$text<-
function (yval, dev, wt, ylevel, digits, n, use.n) 
{
	
    #if (use.n) {
        paste("#",yval, ", N=", n, sep = "")
    #}
    #else {
     #   paste(formatg(yval, digits))
    #}
}

	class(l) <- "rpart"
	#l$frame<-as.matrix(data)
	#data <- data.frame(data)#, row.names= 1:dim(data)[1])
	

	if (no.plot) {
		return(l)
	} else {
		rpart.plot::prp(l,left=F, type=type, node.fun=nodeFunSemtree, varlen=0,...)
	}
	
}

