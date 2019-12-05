
#row <- dataset[1,]

traverseConditional.rec <- function(row1, row2, tree, depth=0)
{

if (tree$caption=="TERMINAL") return(tree$node_id);
	
# row info is conditional on depth
  if (depth==0) {
    row <- row2
  } else {
    row <- row1
  }
  

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
	return(traverseConditional.rec(row1, row2, tree$left_child, depth+1));
} else {
	return(traverseConditional.rec(row1, row2, tree$right_child, depth+1));
	
}

}

traverseConditional <- function(tree, dataset1, dataset2)
{
  stopifnot(dim(dataset1)==dim(dataset2))
  

    result <- rep(NA, dim(dataset1)[1])
    for (i in 1:dim(dataset1)[1]) {
      result[i] <- traverseConditional.rec(row1 = dataset1[i,], row2=dataset2[i,], tree = tree, depth=0)
    }
    return(result)

}
