setGlobal <- function(x,value) {
	#env <- as.environment("package:semtree")
	env <- .GlobalEnv
	assign(x, value, envir=env)
}

getGlobal <- function(x)
{
	#env <- as.environment("package:semtree")
	env <- .GlobalEnv
	get(x, envir=env)
}

global.node.id <- NA