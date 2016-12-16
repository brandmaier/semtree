
#row <- dataset[1,]

traverse.rec <- function(row, tree)
{
	#cat(row,"\n")
if (tree$caption=="TERMINAL") return(tree$node_id);
	

#value <- row[tree$rule$variable]
#cat("Row",paste(row),"\n")
#cat("Tree rule", tree$rule$name,"\n")
value <- tryCatch({
 row[[tree$rule$name]]
},error=function(cond){ message("ERROR! Incomplete dataset!"); stop(); return(NA);})

if (is.na(value)) return(tree$node_id);

log.val <- NA

if (tree$rule$relation==">=") {
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
	return(apply(dataset,1, FUN=traverse.rec, tree))
}

#traverse(tree, dataset)
