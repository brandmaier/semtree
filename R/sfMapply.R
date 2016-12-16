# cl: cluster reference
# fun: function name
sfMapply<- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE) {
	#browser()
	dots <- list(...)
	
	#sfCheck()			- only internal?!
	#checkFunction(fun)	- only internal?!
	if (snowfall::sfParallel()) {
		
		# relevant code from mapply internals
		FUN <- match.fun(FUN)
		
		m <- length(dots)
		# ---relevant part from staticClusterApply modified to be mapply
		cl <- snowfall::sfGetCluster()
		snow::checkCluster(cl)
		p <- length(cl)		
		n <- length(dots[[1]])	# check should be more thorough here!
		
		# confirm length of all dot arguments
		llen <- sapply(dots, length)
		lt <- all(llen==llen[1])
		if (!lt) {
			stop("Argument lengths do not match in sfMapply()")
		}
		
		#cat("DO parallel exec!",n," ",p,"\n")
		flush.console()
		val <- vector("list", n)
 		start <- 1
		# in each iteration, start as many jobs as CPUs are available
        while (start <= n) {
            end <- min(n, start + p - 1)
            jobs <- end - start + 1

            for (i in 1:jobs) {
            	
             	args <- vector("list", m)
            	for (j in 1:m) {
            	#args[j] <- dots[j](start + 
               # i - 1)
                args[[j]] <- dots[[j]][[start+i-1]]
                }
                if (!is.null(MoreArgs)) {
                	args <- append(args, MoreArgs)
                }
                cat("Job ",i,":", start,"->", end," jobs:",jobs,"\n")
            	flush.console()
				snow::sendCall(cl[[i]], FUN, args)
            }
            val[start:end] <- lapply(cl[1:jobs], snow::recvResult)
            start <- start + jobs
        }
	    snow::checkForRemoteErrors(val)
        # simplify to array here!

	if (!identical(SIMPLIFY, FALSE) && length(val)) 
        simplify2array(val, higher = (SIMPLIFY == "array"))
    else val

	} else {
		return(mapply(FUN = FUN, ... = ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
	}
	
	
}