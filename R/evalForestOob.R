# Evaluate likelihood of the tree on the OOB sample
evalTreeOob <- function(tree, data, fulldata) {
 oob.data <- data$oob.data
 oob.data.full <- fulldata[as.numeric(rownames(oob.data)),]
 evaluateTree(tree, oob.data.full)
}

evalForestOob <- function(forest)
{
	res <- mapply(FUN=evalTreeOob, forest$forest, forest$forest.data, forest$data)

	vals <- simplify2array(res[1,])

	mean.ll <- mean(vals)
	stderr <- sqrt(var(vals))/sqrt(length(forest$forest))

	return(list(values=vals, mean=mean.ll, std.err.mean=stderr, ci95.upper=mean.ll+stderr*1.96, ci95.lower=mean.ll-stderr*1.96 ))

}