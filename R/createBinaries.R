# NOT IMPLEMENTED!!!!!!!!!!!

####################################################
# creates a set of binary variables from binary    #
# categorical, ordered, and continuous variables   #
####################################################

createBinaries <- function(mydata=NULL, manif=NULL)
{
#num.bin <- c()
#og.var <- c()
mv.array<- mydata[,1:manif]
#uses only the covariates for this function
for(i in (manif+1):ncol(mydata))
{
	result <- c()
	# variables read in as factors are then sorted as ordered or not
	if(is.factor(mydata[,i])==T) {
		if(is.ordered(mydata[,i])==T) {
			x <- as.numeric(mydata[,i])
			result<-recodeOrdered(x,colnames(mydata)[i])
	    #num.bin <- cbind(num.bin, ncol(result$columns))
			#og.var <- cbind(og.var,rep(i,ncol(result$columns)))
		}
		else {
		  #browser()
		  x <- as.numeric(mydata[,i])
		  result<-recodeAllSubsets(x,colnames(mydata)[i])
		  #num.bin <- cbind(num.bin, ncol(result$columns))
		  #og.var <- cbind(og.var,rep(i,ncol(result$columns)))
		}
	}
	else {
	  x <- as.numeric(mydata[,i])
	  result<-recodeOrdered(x,colnames(mydata)[i])
	  #num.bin <- cbind(num.bin, ncol(result$columns))
	  #og.var <- cbind(og.var,rep(i,ncol(result$columns)))
	}
	if(nrow(result$columns)==nrow(mydata)) {
	  mv.array <- cbind(mv.array, result$columns)
	  #num.bin <- cbind(num.bin, ncol(result$columns))
	}
	# numeric variables use the ordered analysis to make contrasts
	# at points along scale of each continuous variable
	#else {
	#	names <- colnames(mv.array)
	#	names <- append(names, paste(colnames(mydata[i])))
	#	mv.array <- cbind(mv.array, mydata[i])
	#	names(mv.array) <- names
	#	num.bin <- cbind(num.bin, 0)
	#}
}	

#result <- c()
#result$mv.array <- mv.array
result <- mv.array
#result$num.bin <- num.bin
#result$og.var <-og.var
#returns appended original data file, and binaries only data
return(result)
}
