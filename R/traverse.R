
#row <- dataset[1,]

traverse.rec <- function(row, tree)
{
	#cat(row,"\n")
if (tree$caption=="TERMINAL") return(tree$node_id);
	
value <- tryCatch({
 row[[tree$rule$name]]
},error=function(cond){ message("ERROR! Incomplete dataset!"); stop(); return(NA);})

if (is.na(value)) return(tree$node_id);

log.val <- NA

if (tree$rule$relation==">=") {
  if (is.ordered(value)) { value <- as.numeric(as.character(value)) }
	log.val <- value >= tree$rule$value
} else if (tree$rule$relation=="%in%") {
	log.val <- value %in% tree$rule$value
} else {
	stop("Comparison not supported in traverse.rec():", tree$rule)
}

if (!log.val) 
{
	return(traverse.rec(row, tree$left_child));
} else {
	return(traverse.rec(row, tree$right_child));
	
}

}

traverse <- function(tree, dataset)
{
  if (!is.null(tree$traverse.fun)) {
    return(tree$traverse.fun(dataset))
  }
  
  if (is(dataset,"data.frame")) {
    result <- rep(NA, dim(dataset)[1])
    for (i in 1:dim(dataset)[1]) {
      result[i] <- traverse.rec(row = dataset[i,],tree = tree)
    }
    return(result)
  } else {
  	return(apply(X=dataset,MARGIN=1, FUN=traverse.rec, tree))
  }
}

#traverse(tree, dataset)
